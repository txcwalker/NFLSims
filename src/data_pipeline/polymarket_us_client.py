"""Read-only client for Polymarket US (the CFTC-regulated exchange,
gateway.polymarket.us) -- NFL game events and their markets.

Public, unauthenticated endpoints only. Nothing here places orders or needs an
API key: the key Polymarket US issues (Ed25519, via polymarket.us/developer
after in-app identity verification) is only for trading/portfolio endpoints,
which this project deliberately does not touch yet. Rate limit on public
endpoints is 20 req/s per IP (docs.polymarket.us/api-reference/rate-limits).

Event slugs are derivable from our own schedule: nfl-{away}-{home}-{gameday},
lowercase team codes, gameday = nflverse's local (ET) `gameday` column --
verified 2026-09-26 against all 16 week-3 games. The only code that differs
from nflverse is the Rams (nflverse "LA", Polymarket "lar").

Companion plan: docs/implementation_plans/prediction_market_props_plan.md.
Consumed by: src/evaluation/prop_markets.py (-> GET /api/props/polymarket).
"""

import time
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone

import requests

GATEWAY = "https://gateway.polymarket.us"

# nflverse team code -> Polymarket US slug code, where they differ. Anything not
# listed is just lowercased. Add to this if a game ever comes back 404.
TEAM_SLUG_OVERRIDES = {"LA": "lar"}

REQUEST_TIMEOUT_S = 30
MAX_RETRIES = 3
MAX_WORKERS = 8   # 16 games / 8 workers -- well under the 20 req/s public limit


def team_slug_code(team):
    """Inputs: nflverse team code (str, e.g. 'KC', 'LA').
    Output: Polymarket US slug code (str, e.g. 'kc', 'lar')."""
    return TEAM_SLUG_OVERRIDES.get(team, str(team).lower())


def event_slug(away_team, home_team, gameday):
    """Inputs: nflverse away/home codes + gameday ('YYYY-MM-DD', schedule CSV).
    Output: the Polymarket US event slug, e.g. 'nfl-kc-mia-2026-09-27'."""
    return f"nfl-{team_slug_code(away_team)}-{team_slug_code(home_team)}-{gameday}"


def _get(session, path, params=None):
    """GET {GATEWAY}{path} with exponential backoff on 429/5xx.

    Inputs: requests.Session, path (str), optional query params (dict).
    Output: parsed JSON (dict), or None on 404 / 4xx (slug not found).
    Raises requests.HTTPError only after MAX_RETRIES on 429/5xx, so one
    flaky game can't silently look like "no markets"."""
    url = f"{GATEWAY}{path}"
    for attempt in range(MAX_RETRIES + 1):
        resp = session.get(url, params=params, timeout=REQUEST_TIMEOUT_S)
        if resp.status_code == 200:
            return resp.json()
        if resp.status_code in (429,) or resp.status_code >= 500:
            if attempt < MAX_RETRIES:
                time.sleep(2 ** attempt)
                continue
            resp.raise_for_status()
        # Polymarket US answers an unknown slug with a 404 (code 5); treat any
        # other 4xx the same way -- the caller reports it as "no event found".
        return None
    return None


def fetch_event(slug, session=None):
    """One game's full event (all ~750-880 markets, incl. player props).

    Inputs: event slug (str), optional shared requests.Session.
    Output: the `event` dict from GET /v1/events/slug/{slug}, or None if the
    slug doesn't exist. ~3.5 MB of JSON per game -- callers should parse it
    down, not store it raw."""
    session = session or requests.Session()
    data = _get(session, f"/v1/events/slug/{slug}")
    return (data or {}).get("event")


def fetch_events_for_games(games):
    """Fetch every game's event in parallel.

    Inputs: games -- iterable of dicts with game_id, away_team, home_team,
    gameday (schedule CSV rows).
    Output: (events, meta) -- events: {game_id: event dict or None};
    meta: {fetched_at (unix s), slugs: {game_id: slug}, errors: {game_id: str}}.
    A game whose fetch raised lands in errors (event None), so one bad game
    never takes down the whole week."""
    games = list(games)
    session = requests.Session()
    slugs = {g["game_id"]: event_slug(g["away_team"], g["home_team"], g["gameday"]) for g in games}
    events, errors = {}, {}

    def _one(game_id):
        try:
            return game_id, fetch_event(slugs[game_id], session), None
        except requests.RequestException as exc:  # network / exhausted retries
            return game_id, None, str(exc)

    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as pool:
        for game_id, event, err in pool.map(_one, slugs):
            events[game_id] = event
            if err:
                errors[game_id] = err
    meta = {"fetched_at": datetime.now(timezone.utc).timestamp(), "slugs": slugs, "errors": errors}
    return events, meta
