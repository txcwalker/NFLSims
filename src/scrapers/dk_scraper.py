"""Live NFL salary feed from DraftKings' public lobby/draftgroups JSON endpoints.

There is no official DraftKings developer API for salaries or contests -- this
calls the same unauthenticated, undocumented endpoints DK's own website calls
(see docs/todo/dk_contest_api.md for background/history). No auth required as
of writing, but the shape or availability could change without notice, so
every public function here fails soft (returns empty/stale data) rather than
raising -- callers (src/api/app.py's get_week_salaries() and get_rosters())
always have calculate_dfs_salary()'s synthetic estimate to fall back to per
player when a real salary isn't found.

Validated 2026-08-22 against DK's live "Main Slate" Classic contest (draft
group 151307): 638/678 (94%) of 2026 roster players matched by name+team after
normalization; the rest are deep bench/practice-squad players DK's salary pool
doesn't include at all (expected, not a matching bug).

Salaries live at the draft-group level, not the contest level -- every
contest built on the same draft group (Millionaire Maker, Double-Ups, single-
entry, etc.) draws from one identical player pool, so a single draftables
fetch per slate covers every contest on it. The lobby fetch is only needed to
discover which draft group IDs exist and which contests hang off each one.
"""

import difflib
import json
import os
import re
import time
from typing import Any, Dict, List, Optional, Tuple

import requests

LOBBY_URL = "https://www.draftkings.com/lobby/getcontests?sport=NFL"
DRAFTABLES_URL = "https://api.draftkings.com/draftgroups/v1/draftgroups/{draft_group_id}/draftables"
CONTEST_DETAIL_URL = "https://api.draftkings.com/contests/v1/contests/{contest_id}?format=json"
# Empirically (2026-09-15): DK's edge/WAF now 403s any request carrying a
# custom User-Agent -- "Mozilla/5.0" and a full modern Chrome UA string both
# got blocked 100% of repeated, interleaved tests against the live API,
# while an unmodified `requests` call (default `python-requests/x.y` UA, no
# header override) succeeded 100% of the time. So: send no custom headers at
# all rather than a value that looks deliberately browser-like.
HEADERS: Dict[str, str] = {}
REQUEST_TIMEOUT_SECONDS = 8
CACHE_TTL_SECONDS = 30 * 60

# Auto-accept threshold for a fuzzy name match (difflib ratio, 0-1). A wrong
# fuzzy match silently assigns the wrong player's salary -- much worse than a
# miss, which just falls back to the visible synthetic estimate -- so this is
# set high and deliberately not user-configurable without touching the code.
FUZZY_MATCH_THRESHOLD = 0.90

BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
ALIAS_FILE_PATH = os.path.join(BASE_DIR, "data", "dna", "dk_name_aliases.json")

_SUFFIXES = {"jr", "sr", "ii", "iii", "iv", "v"}

# In-process cache, split in two because the two DK calls have different
# scopes: the lobby (contests + slate list) is one fetch that covers every
# slate, while draftables (salaries) is one fetch per slate and only needed
# for slates someone actually asked for. Both share the same 30-minute TTL.
# Neither is ever cleared to empty on a failed refresh -- a stale salary is
# far more useful than none.
_lobby_cache: Dict[str, Any] = {
    "fetched_at": None,
    "contests_by_dg": {},          # {draft_group_id: [contest dict, ...]}
    "slates": [],                  # see get_dk_slates()
    "default_draft_group_id": None,
}
_slate_caches: Dict[int, Dict[str, Any]] = {}  # draft_group_id -> {fetched_at, players, defense, player_ids, defense_ids, teams}
_showdown_slate_caches: Dict[int, Dict[str, Any]] = {}  # draft_group_id -> showdown pool (see _refresh_showdown_slate)

_alias_cache: Optional[Dict[str, Dict[str, Any]]] = None

# DK Showdown ("Showdown Captain Mode") only -- deliberately excludes Snake
# Showdown, In-Game Showdown, and Madden Showdown, which are different roster
# rules / not real NFL player pools.
SHOWDOWN_GAME_TYPES = {"Showdown Captain Mode"}

# DK's roster-slot ids in a Showdown Captain Mode draftables feed: every player
# appears twice, once per slot. 511 = Captain (salary already ×1.5, 1.5× points),
# 512 = FLEX (base salary). Confirmed 2026-09-09 against draft group 153072
# (SF@LAR): e.g. Puka Nacua 16800 (511) / 11200 (512), ratio exactly 1.5.
DK_SHOWDOWN_CPT_SLOT = 511
DK_SHOWDOWN_FLEX_SLOT = 512

# DK city/relocation abbreviations that differ from the internal roster's.
# Only LAR is live today; the rest are defensive (DK has historically used
# JAX/LV/WAS the same as us, but a feed flip to JAC/LVR/WSH would silently
# break the team match otherwise).
DK_TEAM_ALIASES = {"LAR": "LA", "JAC": "JAX", "LVR": "LV", "WSH": "WAS"}


def _normalize_team(abbrev: Optional[str]) -> str:
    return DK_TEAM_ALIASES.get(abbrev or "", abbrev or "")


def normalize_player_name(name: str) -> str:
    """Case/punctuation/suffix-insensitive key for matching a DK displayName
    against our internal roster traits name (e.g. DK's "Amon-Ra St. Brown" vs
    our "Amon-Ra St Brown"; DK's "James Cook III" vs our "James Cook")."""
    name = name.replace(".", "")
    name = re.sub(r"\s+", " ", name).strip().lower()
    parts = name.split(" ")
    if parts and parts[-1] in _SUFFIXES:
        parts = parts[:-1]
    return " ".join(parts)


def _load_aliases() -> Dict[str, Dict[str, Any]]:
    """Persisted internal-name -> DK-name overrides, built up automatically
    by resolve_dk_salary()'s fuzzy-match step so a given naming mismatch
    (nickname, missing hyphen, etc.) only ever needs to be resolved once --
    every later week reads the alias straight back instead of re-running
    difflib. Missing/corrupt file both just mean 'no aliases yet'."""
    global _alias_cache
    if _alias_cache is not None:
        return _alias_cache
    if os.path.exists(ALIAS_FILE_PATH):
        try:
            with open(ALIAS_FILE_PATH, "r") as f:
                _alias_cache = json.load(f)
        except (json.JSONDecodeError, OSError):
            _alias_cache = {}
    else:
        _alias_cache = {}
    return _alias_cache


def _save_alias(internal_key: str, dk_name_norm: str, confidence: float) -> None:
    aliases = _load_aliases()
    aliases[internal_key] = {
        "dk_name": dk_name_norm,
        "confidence": round(confidence, 4),
        "matched_on": time.strftime("%Y-%m-%d"),
    }
    try:
        os.makedirs(os.path.dirname(ALIAS_FILE_PATH), exist_ok=True)
        with open(ALIAS_FILE_PATH, "w") as f:
            json.dump(aliases, f, indent=2, sort_keys=True)
    except OSError as e:
        print(f"DK scraper: failed to persist name alias for {internal_key}: {e}")


def resolve_dk_salary(name: str, team: str, dk: Dict[str, Any]) -> Tuple[Optional[int], Optional[int]]:
    """Resolves a real DK salary + draftableId for one internal roster player,
    trying progressively looser matches so a DK naming quirk doesn't silently
    fall back to the synthetic salary estimate when a real one is available:

      1. Exact match on normalize_player_name(name) + team (the common case,
         ~94% of a roster per this module's 2026-08-22 validation).
      2. A previously-confirmed alias for this exact (name, team) pair, from
         data/dna/dk_name_aliases.json -- built by step 3 below, so the fuzzy
         match only ever has to run once per mismatched player.
      3. A fuzzy match (difflib ratio) against every DK player on the same
         team, auto-accepted only at FUZZY_MATCH_THRESHOLD (0.90) or above
         and immediately persisted to the alias file. Scoped to team, not
         the whole slate, to keep the candidate pool small (~10-20 names)
         and avoid cross-team false positives.

    Returns (salary, draftable_id) -- both None if nothing cleared the bar,
    same as a straight dict-lookup miss, so callers fall back to
    calculate_dfs_salary() exactly as before.
    """
    name_norm = normalize_player_name(name)
    key = (name_norm, team)
    if key in dk["players"]:
        return dk["players"][key], dk["player_ids"].get(key)

    internal_key = f"{name_norm}|{team}"
    alias = _load_aliases().get(internal_key)
    if alias:
        alias_key = (alias["dk_name"], team)
        if alias_key in dk["players"]:
            return dk["players"][alias_key], dk["player_ids"].get(alias_key)

    team_dk_names = [n for (n, t) in dk["players"].keys() if t == team]
    if not team_dk_names:
        return None, None
    best = difflib.get_close_matches(name_norm, team_dk_names, n=1, cutoff=FUZZY_MATCH_THRESHOLD)
    if not best:
        return None, None

    dk_name = best[0]
    ratio = difflib.SequenceMatcher(None, name_norm, dk_name).ratio()
    _save_alias(internal_key, dk_name, ratio)
    print(f"DK scraper: fuzzy-matched '{name}' ({team}) -> DK's '{dk_name}' (ratio={ratio:.3f}), saved as alias.")
    fuzzy_key = (dk_name, team)
    return dk["players"].get(fuzzy_key), dk["player_ids"].get(fuzzy_key)


def _resolve_showdown_entry(
    name: str, team: str, players: Dict[Tuple[str, str], Dict[str, Any]]
) -> Optional[Tuple[Tuple[str, str], Dict[str, Any]]]:
    """Same 3-tier matching as resolve_dk_salary (exact normalized name+team,
    then a persisted data/dna/dk_name_aliases.json alias, then a one-time
    fuzzy match saved as a new alias) but against a Showdown slate's
    {(name_norm, team): {salary, cpt_salary, flex_id, cpt_id, ...}} dict.
    Kept separate from resolve_dk_salary (which returns a (salary, id) tuple)
    since callers here need the whole entry -- captain price and both ids,
    not just one salary. Shares the same alias file/key format as the Classic
    path, so a mismatch already resolved there (e.g. internal "Josh Palmer"
    vs DK's "Joshua Palmer") is picked up here for free, with no separate
    fuzzy match needed."""
    name_norm = normalize_player_name(name)
    key = (name_norm, team)
    if key in players:
        return key, players[key]

    internal_key = f"{name_norm}|{team}"
    alias = _load_aliases().get(internal_key)
    if alias:
        alias_key = (alias["dk_name"], team)
        if alias_key in players:
            return alias_key, players[alias_key]

    team_dk_names = [n for (n, t) in players.keys() if t == team]
    if not team_dk_names:
        return None
    best = difflib.get_close_matches(name_norm, team_dk_names, n=1, cutoff=FUZZY_MATCH_THRESHOLD)
    if not best:
        return None
    dk_name = best[0]
    ratio = difflib.SequenceMatcher(None, name_norm, dk_name).ratio()
    _save_alias(internal_key, dk_name, ratio)
    print(f"DK scraper: fuzzy-matched (showdown) '{name}' ({team}) -> DK's '{dk_name}' (ratio={ratio:.3f}), saved as alias.")
    fuzzy_key = (dk_name, team)
    return fuzzy_key, players.get(fuzzy_key)


def _apply_internal_names(players: Dict[Tuple[str, str], Dict[str, Any]], teams: set, year: int = 2026) -> None:
    """Renames each DK Showdown player entry's "name" in place to match the
    internal roster's own display name whenever the two resolve to the same
    person. Without this, DK's raw displayName (e.g. "Joshua Palmer") can
    silently defeat a caller's exact-normalized-name join against the
    internal roster's name (e.g. "Josh Palmer") even when a Classic-side
    alias for that exact player already exists -- confirmed 2026-09-17 as
    the actual cause of a real player showing no live Showdown salary in the
    optimizer despite DK pricing them correctly."""
    for team in teams:
        roster_path = os.path.join(BASE_DIR, "data", "current_rosters", f"{team}_traits_{year}.json")
        if not os.path.exists(roster_path):
            continue
        try:
            with open(roster_path, "r") as f:
                roster_data = json.load(f)
        except (json.JSONDecodeError, OSError):
            continue
        matched_keys = set()
        for name in roster_data.get("traits", {}):
            match = _resolve_showdown_entry(name, team, players)
            if match:
                dk_key, dk_rec = match
                if dk_key not in matched_keys:
                    dk_rec["name"] = name
                    matched_keys.add(dk_key)


def _find_main_slate_draft_group_id(contests: List[Dict[str, Any]]) -> Optional[int]:
    """The NFL 'Main Slate' Classic draft group -- DK's flagship Sunday
    contests (Millionaire, etc). Identified as the Classic (non-Showdown,
    non-Snake, non-preseason) draft group appearing in the most contests,
    since side-slates (single-game Showdown, 3-player Snake) are a small
    fraction of the Main Slate's contest count."""
    classic = [
        c for c in contests
        if c.get("gameType") == "Classic" and "Preseason" not in c.get("n", "")
    ]
    if not classic:
        return None
    counts: Dict[int, int] = {}
    for c in classic:
        dg = c.get("dg")
        if dg is not None:
            counts[dg] = counts.get(dg, 0) + 1
    if not counts:
        return None
    return max(counts, key=counts.get)


MAIN_SLATE_PIN_PATH = os.path.join(BASE_DIR, "data", "dk_main_slate_pins.json")


def _load_main_slate_pins() -> Dict[str, Any]:
    if not os.path.exists(MAIN_SLATE_PIN_PATH):
        return {}
    try:
        with open(MAIN_SLATE_PIN_PATH, "r") as f:
            return json.load(f)
    except Exception:
        return {}


def _save_main_slate_pins(pins: Dict[str, Any]) -> None:
    os.makedirs(os.path.dirname(MAIN_SLATE_PIN_PATH), exist_ok=True)
    tmp = MAIN_SLATE_PIN_PATH + ".tmp"
    with open(tmp, "w") as f:
        json.dump(pins, f, indent=2)
    os.replace(tmp, MAIN_SLATE_PIN_PATH)


def get_main_slate_pin(year: int, week: int) -> Optional[Dict[str, Any]]:
    """Read-only lookup of the current pin record ({draft_group_id,
    contest_count, pinned_at}) for one (year, week), or None if never pinned.
    For callers (e.g. the /api/dk/slates endpoint) that need to display info
    about a pinned slate without triggering a live refresh themselves."""
    pins = _load_main_slate_pins()
    return (pins.get(str(year)) or {}).get(str(week))


def pin_main_slate_draft_group_id(year: int, week: int, draft_group_id: int, contest_count: int = 10**9) -> None:
    """Explicit manual/admin pin, for backfilling a (year, week) whose real
    main slate already locked and dropped out of the live lobby before this
    sticky-pin mechanism existed to catch it automatically (e.g. one-time
    fixup via a shell one-liner). `contest_count` defaults to effectively
    infinite so this manual pin can't be silently outvoted later by a
    same-week live "default" candidate with a merely large contest count --
    pass a real number instead if that matters for a given case."""
    pins = _load_main_slate_pins()
    pins.setdefault(str(year), {})[str(week)] = {
        "draft_group_id": draft_group_id,
        "contest_count": contest_count,
        "pinned_at": time.time(),
        "manual": True,
    }
    _save_main_slate_pins(pins)


def resolve_main_slate_draft_group_id(year: int, week: int, force_refresh: bool = False) -> Optional[int]:
    """Sticky per-(year, week) pin for the real NFL 'Main Slate' Classic
    draft group, so it stays correct for the rest of the week even after DK's
    live lobby moves on.

    _find_main_slate_draft_group_id()'s "most CURRENTLY OPEN contests"
    heuristic silently breaks once the real main slate's games lock: DK drops
    locked contests from the live lobby entirely, so that draft group's open-
    contest count falls to (near) zero, and whatever small leftover classic
    contest DK still has open (e.g. a Monday-night-only slate) trivially wins
    the vote instead -- confirmed live 2026-09-14, where draft group 153109
    (a 2-team Monday-night leftover) out-"defaulted" 151307 (the real,
    722-player week-1 main slate) purely because 151307's contests had locked
    and dropped out of the lobby. DK's draftables endpoint keeps serving a
    locked slate's full real salary/id data for a long while after that
    (confirmed same day), so there's no need to give up on 151307 just
    because the lobby stopped listing its contests.

    Fix: remember whichever classic draft group has ever had the most
    contests for this (year, week) -- across repeated calls, not just the
    current live snapshot -- and keep using that one even after it drops out
    of the live "default" pick. A pin only ever grows (replaced by a bigger
    candidate), never shrinks, so a genuinely bigger/updated slate later in
    the week can still take over, but a lock-induced shrink can't steal it
    back. A (year, week) with no pin yet falls back to the live heuristic and
    gets pinned by that same call, so normal weeks are pinned automatically
    the first time anyone loads the app while the real main slate is live --
    no manual step needed in the common case.
    """
    _maybe_refresh_lobby(force_refresh)
    pins = _load_main_slate_pins()
    ykey, wkey = str(year), str(week)
    pinned = (pins.get(ykey) or {}).get(wkey)

    live_default = _lobby_cache["default_draft_group_id"]
    live_count = 0
    if live_default is not None:
        for s in _lobby_cache.get("slates", []):
            if s["draft_group_id"] == live_default:
                live_count = s["contest_count"]
                break

    if pinned is None or (live_default is not None and live_count > pinned.get("contest_count", 0)):
        if live_default is not None:
            pins.setdefault(ykey, {})[wkey] = {
                "draft_group_id": live_default,
                "contest_count": live_count,
                "pinned_at": time.time(),
            }
            _save_main_slate_pins(pins)
            return live_default
        return pinned["draft_group_id"] if pinned else None

    return pinned["draft_group_id"]


def _fetch_json(url: str) -> Optional[Dict[str, Any]]:
    try:
        resp = requests.get(url, headers=HEADERS, timeout=REQUEST_TIMEOUT_SECONDS)
        resp.raise_for_status()
        return resp.json()
    except Exception as e:
        print(f"DK scraper: request to {url} failed: {e}")
        return None


def _extract_contests(contests: List[Dict[str, Any]], draft_group_id: int) -> List[Dict[str, Any]]:
    """Every live contest in the Main Slate draft group, with entry fee /
    payout / size fields pulled straight out of the same lobby listing
    already fetched to identify the draft group -- no extra request needed.
    Field mapping confirmed 2026-08-22 by cross-referencing DK's own contest
    names (e.g. "NFL GIANT $5 Double Up" -> a=5.0, "NFL MASSIVE $25 Double
    Up" -> a=25.0) against the raw lobby JSON, since DK ships no field-name
    documentation for this undocumented endpoint:
      a    = entry fee (dollars)
      po   = total prize pool (dollars)
      m    = max total entries (contest size cap)
      nt   = current number of entries
      mec  = max entries allowed per user
      pd   = short payout description (e.g. {"Cash": "$3,500,000"})
    Does NOT include the full rank-by-rank payout table (1st/2nd/.../min-cash
    $ amounts) -- that requires a separate per-contest call, not built yet.
    """
    out = []
    for c in contests:
        if c.get("dg") != draft_group_id:
            continue
        out.append({
            "contest_id": c.get("id"),
            "name": c.get("n"),
            "entry_fee": c.get("a"),
            "prize_pool": c.get("po"),
            "max_entries": c.get("m"),
            "current_entries": c.get("nt"),
            "max_entries_per_user": c.get("mec"),
            "payout_summary": c.get("pd"),
            "start_time": c.get("sdstring"),
            "guaranteed": c.get("attr", {}).get("IsGuaranteed") == "true",
        })
    out.sort(key=lambda c: c["current_entries"] or 0, reverse=True)
    return out


def _label_slate(game_type: Optional[str], dg_contests: List[Dict[str, Any]], is_default: bool) -> str:
    """Best-effort human label for a draft group DK doesn't itself name.
    Only the Main Slate branch is validated (2026-08-22, draft group
    151307) -- Showdown/Snake/other-slate labels are an educated guess from
    gameType + earliest contest start time, unvalidated until a live week
    actually has one of those slates to check against."""
    if is_default:
        return "Main Slate (Classic)"
    times = [c.get("sdstring") for c in dg_contests if c.get("sdstring")]
    suffix = f" - {min(times)}" if times else ""
    label = game_type or "Slate"
    return f"{label}{suffix}"


def _build_slates(contests: List[Dict[str, Any]], dg_ids: set, default_dg: Optional[int]) -> List[Dict[str, Any]]:
    slates = []
    for dg in dg_ids:
        dg_contests = [c for c in contests if c.get("dg") == dg]
        if not dg_contests:
            continue
        is_default = dg == default_dg
        slates.append({
            "draft_group_id": dg,
            "game_type": dg_contests[0].get("gameType"),
            "label": _label_slate(dg_contests[0].get("gameType"), dg_contests, is_default),
            "contest_count": len(dg_contests),
            "total_entries": sum(c.get("nt") or 0 for c in dg_contests),
            "is_default": is_default,
        })
    slates.sort(key=lambda s: (not s["is_default"], -s["contest_count"]))
    return slates


def _refresh_lobby() -> None:
    """Fetches the lobby once and populates every slate's contest list plus
    the discoverable slate menu -- this is the only call needed to answer
    'what slates exist right now' or 'what contests are on slate X', since
    contests carry no salary data of their own (see module docstring)."""
    lobby = _fetch_json(LOBBY_URL)
    if lobby is None:
        return
    contests = lobby.get("Contests", [])
    default_dg = _find_main_slate_draft_group_id(contests)
    dg_ids = {c.get("dg") for c in contests if c.get("dg") is not None}
    contests_by_dg = {dg: _extract_contests(contests, dg) for dg in dg_ids}
    slates = _build_slates(contests, dg_ids, default_dg)

    _lobby_cache.update({
        "fetched_at": time.time(),
        "contests_by_dg": contests_by_dg,
        "slates": slates,
        "default_draft_group_id": default_dg,
    })
    print(f"DK scraper: refreshed lobby -- {len(slates)} slate(s) found, "
          f"default draft group {default_dg}.")


def _maybe_refresh_lobby(force_refresh: bool) -> None:
    stale = _lobby_cache["fetched_at"] is None or (time.time() - _lobby_cache["fetched_at"] > CACHE_TTL_SECONDS)
    if force_refresh or stale:
        _refresh_lobby()


def _refresh_slate(draft_group_id: int) -> None:
    draftables_resp = _fetch_json(DRAFTABLES_URL.format(draft_group_id=draft_group_id))
    if draftables_resp is None:
        return
    draftables = draftables_resp.get("draftables", [])

    players: Dict[Tuple[str, str], int] = {}
    player_pos: Dict[Tuple[str, str], str] = {}
    defense: Dict[str, int] = {}
    player_ids: Dict[Tuple[str, str], int] = {}
    defense_ids: Dict[str, int] = {}
    defense_names: Dict[str, str] = {}
    teams: set = set()

    for d in draftables:
        team = d.get("teamAbbreviation")
        salary = d.get("salary")
        pos = d.get("position")
        draftable_id = d.get("draftableId")
        if not team or salary is None:
            continue
        teams.add(team)
        if pos == "DST":
            defense[team] = salary
            if draftable_id is not None:
                defense_ids[team] = draftable_id
            # DK's real displayName for a defense (e.g. "Buccaneers"), not the
            # generic "Defense" label the sim engine uses internally -- needed
            # to write a lineup-upload CSV cell DK's own upload will accept
            # (see src/api/app.py's get_week_dk_names()).
            display_name = d.get("displayName", "")
            if display_name:
                defense_names[team] = display_name
        else:
            name = d.get("displayName", "")
            if name:
                key = (normalize_player_name(name), team)
                players[key] = salary
                # `pos` was already read above to route DST vs. everyone else
                # -- keeping it here too (added 2026-09-22) so callers get a
                # real position instead of having to re-derive it elsewhere.
                # Found via the DFS ownership model's ~670-player-per-week
                # ~ main-slate salary archive silently carrying pos="" for
                # every non-DST player since week 1 -- get_dk_salaries()'s
                # `players` dict had nowhere to put it before this.
                if pos:
                    player_pos[key] = pos
                if draftable_id is not None:
                    player_ids[key] = draftable_id

    if not players:
        print(f"DK scraper: draft group {draft_group_id} returned no usable player salaries.")
        return

    _slate_caches[draft_group_id] = {
        "fetched_at": time.time(),
        "players": players,
        "player_pos": player_pos,
        "defense": defense,
        "player_ids": player_ids,
        "defense_ids": defense_ids,
        "defense_names": defense_names,
        "teams": teams,
    }
    print(f"DK scraper: refreshed salaries for draft group {draft_group_id} "
          f"({len(players)} players, {len(defense)} defenses, {len(teams)} teams).")


def get_dk_slates(force_refresh: bool = False) -> Dict[str, Any]:
    """Every distinct slate DK currently has live -- Main Slate (Classic) is
    always present when there's any live NFL contest at all; other entries
    (Showdown, Snake, split Sunday slates) come and go depending on the day
    of week / bye weeks / how far into the week it is. One lobby fetch, no
    per-slate draftables calls, so this is cheap to call just to populate a
    picker before the user has chosen anything.

        {
          "is_live": bool,
          "fetched_at": float | None,
          "default_draft_group_id": int | None,   # the Main Slate, when found
          "slates": [{draft_group_id, game_type, label, contest_count,
                      total_entries, is_default}, ...],
        }

    Never raises -- same fail-soft contract as the rest of this module.
    """
    _maybe_refresh_lobby(force_refresh)
    return {
        "is_live": _lobby_cache["fetched_at"] is not None,
        "fetched_at": _lobby_cache["fetched_at"],
        "default_draft_group_id": _lobby_cache["default_draft_group_id"],
        "slates": _lobby_cache["slates"],
    }


def get_dk_salaries(draft_group_id: Optional[int] = None, force_refresh: bool = False) -> Dict[str, Any]:
    """Returns the salary feed for one slate -- the Main Slate by default,
    or whichever draft_group_id the caller picked from get_dk_slates():

        {
          "draft_group_id": int | None,
          "fetched_at": float | None,   # unix timestamp of last successful fetch
          "is_live": bool,              # False if we've never fetched successfully
          "players": {(normalized_name, team_abbrev): salary},
          "player_pos": {(normalized_name, team_abbrev): position},  # added 2026-09-22
          "defense": {team_abbrev: salary},
          "player_ids": {(normalized_name, team_abbrev): draftableId},
          "defense_ids": {team_abbrev: draftableId},
          "defense_names": {team_abbrev: displayName},   # DK's real name, e.g. "Buccaneers"
          "main_slate_teams": {team_abbrev, ...},
        }

    draftableId is DK's own per-slate player identifier -- the same numeric
    ID DK's own downloadable salary CSV shows as "Name (ID)" and the same ID
    DK's lineup-upload CSV template expects in each roster-slot cell. Needed
    for exporting a lineup that can actually be uploaded to DK, not just a
    human-readable summary.

    Never raises. On any network/parsing failure, returns the last successful
    fetch for that slate (or all-empty on first-ever failure) so callers can
    unconditionally fall back to calculate_dfs_salary() per player without
    special-casing.
    """
    _maybe_refresh_lobby(force_refresh)
    dg = draft_group_id if draft_group_id is not None else _lobby_cache["default_draft_group_id"]
    empty = {"draft_group_id": dg, "fetched_at": None, "is_live": False, "players": {}, "player_pos": {},
             "defense": {}, "player_ids": {}, "defense_ids": {}, "defense_names": {}, "main_slate_teams": set()}
    if dg is None:
        return empty

    entry = _slate_caches.get(dg)
    stale = entry is None or (time.time() - entry["fetched_at"] > CACHE_TTL_SECONDS)
    if force_refresh or stale:
        _refresh_slate(dg)
        entry = _slate_caches.get(dg)
    if entry is None:
        return empty

    return {
        "draft_group_id": dg,
        "fetched_at": entry["fetched_at"],
        "is_live": True,
        "players": entry["players"],
        "player_pos": entry.get("player_pos", {}),
        "defense": entry["defense"],
        "player_ids": entry["player_ids"],
        "defense_ids": entry["defense_ids"],
        "defense_names": entry.get("defense_names", {}),
        "main_slate_teams": entry["teams"],
    }


_PRELOCK_CACHE: Dict[Tuple[int, int], Dict[str, Any]] = {}  # (year, week) -> get_dk_salaries()-shaped dict, or None


def load_prelock_salary_snapshot(year: int, week: int) -> Optional[Dict[str, Any]]:
    """Same shape as get_dk_salaries() (so resolve_dk_salary() takes it as a
    drop-in substitute), built from
    data/dfs_ownership/<year>/week_<NN>/main_slate/salaries_prelock.csv
    instead of a live fetch.

    Why this needs to exist at all (2026-09-16): a closed/settled week's
    draft_group_id is NOT a stable historical record -- DK reuses/repoints
    the numeric id over time, so a live draftables fetch by that id can
    return 200 OK with a WRONG, unrelated player pool instead of failing
    (confirmed on week 1's own pin, 151307: this module's docstring already
    notes it was DK's live Main Slate back on 2026-08-22 during preseason
    dev; by 2026-09-16 the same id returned a 24-team mix including
    Thursday-week-2 BUF/DET, not week 1's real 32-team field). is_live-style
    "did the fetch fail" checks (see src/api/app.py's _overlay_live_salaries)
    can't catch this failure mode since the fetch itself succeeds -- only a
    frozen, known-good pre-lock snapshot can. Callers should therefore
    prefer this over get_dk_salaries() whenever a snapshot exists for the
    requested week, live or not.

    Cached in-process by (year, week) with no TTL -- a settled week's
    snapshot is immutable once written (see scripts/dfs_ownership/
    snapshot_slate_salaries.py); re-snapshotting a week (rare, only if
    re-run before lock) invalidates by mtime like the other caches here.

    Returns None if no snapshot file exists for this week (current/future
    week, or one never snapshotted) -- callers should fall back to
    get_dk_salaries() in that case.
    """
    import csv

    key = (int(year), int(week))
    path = os.path.join(BASE_DIR, "data", "dfs_ownership", str(int(year)), f"week_{int(week):02d}",
                        "main_slate", "salaries_prelock.csv")
    if not os.path.exists(path):
        _PRELOCK_CACHE.pop(key, None)
        return None

    mtime = os.path.getmtime(path)
    cached = _PRELOCK_CACHE.get(key)
    if cached is not None and cached.get("_mtime") == mtime:
        return cached

    players: Dict[Tuple[str, str], int] = {}
    player_pos: Dict[Tuple[str, str], str] = {}
    player_ids: Dict[Tuple[str, str], int] = {}
    defense: Dict[str, int] = {}
    defense_ids: Dict[str, int] = {}
    defense_names: Dict[str, str] = {}
    teams: set = set()
    with open(path, "r", encoding="utf-8-sig", newline="") as f:
        for row in csv.DictReader(f):
            team = (row.get("team") or "").strip().upper()
            if not team:
                continue
            teams.add(team)
            try:
                salary = int(float(row["salary"]))
                dk_id = int(row["dk_id"]) if row.get("dk_id") else None
            except (KeyError, ValueError):
                continue
            pos = (row.get("pos") or "").strip().upper()
            if pos == "DST":
                defense[team] = salary
                if dk_id is not None:
                    defense_ids[team] = dk_id
                raw_name = (row.get("name") or "").strip()
                if raw_name:
                    defense_names[team] = raw_name
            else:
                name_norm = normalize_player_name(row.get("name") or "")
                if not name_norm:
                    continue
                players[(name_norm, team)] = salary
                if pos:
                    player_pos[(name_norm, team)] = pos
                if dk_id is not None:
                    player_ids[(name_norm, team)] = dk_id

    result = {
        "draft_group_id": None, "fetched_at": mtime, "is_live": True,
        "players": players, "player_pos": player_pos, "defense": defense,
        "player_ids": player_ids, "defense_ids": defense_ids,
        "defense_names": defense_names,
        "main_slate_teams": teams, "_mtime": mtime,
    }
    _PRELOCK_CACHE[key] = result
    return result


def _refresh_showdown_slate(draft_group_id: int) -> None:
    """Fetches one Showdown Captain Mode draft group's draftables and folds the
    two-rows-per-player feed into one record per player carrying BOTH the base
    (FLEX) salary and the captain salary/id.

    Cached shape (`_showdown_slate_caches[dg]`):
        {
          "fetched_at": float,
          "teams": {abbrev, abbrev},                 # the two teams in the game
          "players": {(normalized_name, team): {"name", "team", "pos",
                       "salary" (base/FLEX), "cpt_salary", "flex_id", "cpt_id"}},
          "defense": {team: {same shape, pos="DST"}},
        }
    Kickers (pos "K") are kept in `players` -- DK prices them on Showdown even
    though the sim engine doesn't project them, so a caller can at least show
    the salary.
    """
    resp = _fetch_json(DRAFTABLES_URL.format(draft_group_id=draft_group_id))
    if resp is None:
        return
    draftables = resp.get("draftables", [])

    by_player: Dict[Tuple[str, str], Dict[str, Any]] = {}
    for d in draftables:
        team = _normalize_team(d.get("teamAbbreviation"))
        salary = d.get("salary")
        pos = d.get("position")
        draftable_id = d.get("draftableId")
        slot = d.get("rosterSlotId")
        name = d.get("displayName", "")
        if not team or salary is None or not name:
            continue
        key = ("__dst__", team) if pos == "DST" else (normalize_player_name(name), team)
        rec = by_player.setdefault(key, {"name": name, "team": team, "pos": pos})
        if slot == DK_SHOWDOWN_CPT_SLOT:
            rec["cpt_salary"] = salary
            rec["cpt_id"] = draftable_id
        else:  # FLEX slot, or an unexpected slot id -> treat as the base row
            rec["flex_salary"] = salary
            rec["flex_id"] = draftable_id

    players: Dict[Tuple[str, str], Dict[str, Any]] = {}
    defense: Dict[str, Dict[str, Any]] = {}
    teams: set = set()
    for key, rec in by_player.items():
        base = rec.get("flex_salary")
        if base is None and rec.get("cpt_salary") is not None:
            base = int(round(rec["cpt_salary"] / 1.5))
        if base is None:
            continue
        teams.add(rec["team"])
        entry = {
            "name": rec["name"], "team": rec["team"], "pos": rec["pos"],
            "salary": base,
            "cpt_salary": rec.get("cpt_salary", int(round(base * 1.5))),
            "flex_id": rec.get("flex_id"),
            "cpt_id": rec.get("cpt_id"),
        }
        if rec["pos"] == "DST":
            defense[rec["team"]] = entry
        else:
            players[key] = entry

    if not players:
        print(f"DK scraper: showdown draft group {draft_group_id} returned no usable salaries.")
        return

    _apply_internal_names(players, teams)

    _showdown_slate_caches[draft_group_id] = {
        "fetched_at": time.time(),
        "teams": teams,
        "players": players,
        "defense": defense,
    }
    print(f"DK scraper: refreshed Showdown salaries for draft group {draft_group_id} "
          f"({len(players)} players, {len(defense)} DST, teams {sorted(teams)}).")


def _showdown_slate_entry(draft_group_id: int, force_refresh: bool) -> Optional[Dict[str, Any]]:
    entry = _showdown_slate_caches.get(draft_group_id)
    stale = entry is None or (time.time() - entry["fetched_at"] > CACHE_TTL_SECONDS)
    if force_refresh or stale:
        _refresh_showdown_slate(draft_group_id)
        entry = _showdown_slate_caches.get(draft_group_id)
    return entry


def get_dk_showdown_slates(force_refresh: bool = False) -> Dict[str, Any]:
    """Every live Showdown Captain Mode slate with its two teams resolved.

    One draftables fetch per showdown slate (cached 30 min) -- needed because
    DK's lobby listing labels a showdown slate only by start time, never by
    matchup. Typically 1-8 showdown slates live at once.

        {
          "is_live": bool,
          "slates": [{"draft_group_id", "label", "teams": [a, b],
                      "contest_count", "player_count"}, ...],
        }
    Fail-soft: never raises; a slate whose draftables fetch failed comes back
    with teams=[] and player_count=0 rather than being dropped.
    """
    _maybe_refresh_lobby(force_refresh)
    out = []
    for s in _lobby_cache["slates"]:
        if s.get("game_type") not in SHOWDOWN_GAME_TYPES:
            continue
        dg = s["draft_group_id"]
        entry = _showdown_slate_entry(dg, force_refresh)
        out.append({
            "draft_group_id": dg,
            "label": s.get("label"),
            "teams": sorted(entry["teams"]) if entry else [],
            "contest_count": s.get("contest_count"),
            "player_count": len(entry["players"]) if entry else 0,
        })
    return {"is_live": _lobby_cache["fetched_at"] is not None, "slates": out}


def get_dk_showdown_salaries(
    draft_group_id: Optional[int] = None,
    away_team: Optional[str] = None,
    home_team: Optional[str] = None,
    force_refresh: bool = False,
) -> Dict[str, Any]:
    """Salary pool for one Showdown slate -- by explicit `draft_group_id`, or
    discovered by matching a `{away_team, home_team}` pair against every live
    showdown slate's resolved teams (internal abbreviations, e.g. "LA" not
    "LAR").

        {
          "found": bool,
          "draft_group_id": int | None,
          "fetched_at": float | None,
          "teams": [a, b],
          "players": [{"name","team","pos","salary","cpt_salary",
                       "flex_id","cpt_id"}, ...],   # includes K; excludes DST
          "defense": [{...same shape, pos="DST"...}, ...],
        }
    Fail-soft: `found=False` with empty lists on any miss/failure.
    """
    _maybe_refresh_lobby(force_refresh)
    empty = {"found": False, "draft_group_id": draft_group_id, "fetched_at": None,
             "teams": [], "players": [], "defense": []}

    dg = draft_group_id
    if dg is None:
        if not (away_team and home_team):
            return empty
        want = {away_team, home_team}
        for s in _lobby_cache["slates"]:
            if s.get("game_type") not in SHOWDOWN_GAME_TYPES:
                continue
            entry = _showdown_slate_entry(s["draft_group_id"], force_refresh)
            if entry and want.issubset(entry["teams"]):
                dg = s["draft_group_id"]
                break
        if dg is None:
            return empty

    entry = _showdown_slate_entry(dg, force_refresh)
    if entry is None:
        return {**empty, "draft_group_id": dg}
    return {
        "found": True,
        "draft_group_id": dg,
        "fetched_at": entry["fetched_at"],
        "teams": sorted(entry["teams"]),
        "players": sorted(entry["players"].values(), key=lambda p: -p["salary"]),
        "defense": list(entry["defense"].values()),
    }


def get_dk_contests(draft_group_id: Optional[int] = None, force_refresh: bool = False) -> Dict[str, Any]:
    """Returns every live contest on one slate -- the Main Slate by default --
    with entry fee / prize pool / size / current-entries -- see
    _extract_contests()'s docstring for the exact field mapping. Shares the
    lobby cache with get_dk_slates(), so calling this after get_dk_slates()
    in the same refresh window is free.

        {
          "draft_group_id": int | None,
          "fetched_at": float | None,
          "is_live": bool,
          "contests": [{contest_id, name, entry_fee, prize_pool, max_entries,
                        current_entries, max_entries_per_user, payout_summary,
                        start_time, guaranteed}, ...],
        }

    Never raises -- same fail-soft contract as get_dk_salaries().
    """
    _maybe_refresh_lobby(force_refresh)
    dg = draft_group_id if draft_group_id is not None else _lobby_cache["default_draft_group_id"]
    return {
        "draft_group_id": dg,
        "fetched_at": _lobby_cache["fetched_at"],
        "is_live": _lobby_cache["fetched_at"] is not None,
        "contests": _lobby_cache["contests_by_dg"].get(dg, []) if dg is not None else [],
    }


def _parse_cash_amount(s: str) -> float:
    """'$1,000,000.00' -> 1000000.0. Non-cash payout descriptions (tickets,
    swag, etc.) don't parse as a dollar amount -- returns 0.0 for those
    rather than raising, since a contest can mix cash and non-cash tiers."""
    try:
        return float(s.replace("$", "").replace(",", ""))
    except (ValueError, AttributeError):
        return 0.0


def get_dk_contest_payout(contest_id: int) -> Dict[str, Any]:
    """Fetches the REAL rank-by-rank payout table for one specific contest --
    the exact dollar amount paid at every finishing position, not the
    contest-type-shaped percentage guess _get_default_payout_structure() in
    src/api/app.py falls back to. Unlike get_dk_salaries()/get_dk_contests(),
    this is a live per-request call with no cache: a contest's own entry
    count changes constantly as people enter, and the payout call is only
    made when a user actually picks a specific contest in the Optimizer, not
    on every page load.

    Endpoint discovered 2026-08-22 by testing against a live Millionaire
    Maker contest ($5 entry, 34 payout tiers from $1,000,000 at 1st down to
    $8 at the last paying rank) -- matches the exact shape DK's own contest
    rules popup renders, confirmed field-by-field.

        {
          "contest_id": int,
          "entries": int | None,          # current entries
          "max_entries": int | None,
          "entry_fee": float | None,
          "tiers": [{"rank_start": int, "rank_end": int, "payout": float}, ...],
          "error": str | None,            # set (tiers == []) on any failure
        }

    Fails soft like the rest of this module: never raises, "error" is set
    and "tiers" is empty on any network/parsing failure so callers can
    unconditionally fall back to _get_default_payout_structure().
    """
    data = _fetch_json(CONTEST_DETAIL_URL.format(contest_id=contest_id))
    if data is None:
        return {"contest_id": contest_id, "entries": None, "max_entries": None,
                "entry_fee": None, "tiers": [], "error": "request failed"}

    cd = data.get("contestDetail")
    if not cd or data.get("errorStatus"):
        return {"contest_id": contest_id, "entries": None, "max_entries": None,
                "entry_fee": None, "tiers": [], "error": str(data.get("errorStatus") or "no contestDetail in response")}

    tiers = []
    for tier in cd.get("payoutSummary", []):
        cash_desc = tier.get("tierPayoutDescriptions", {}).get("Cash")
        if cash_desc is None:
            continue  # non-cash tier (tickets/swag) -- not usable for EV math
        tiers.append({
            "rank_start": tier.get("minPosition"),
            "rank_end": tier.get("maxPosition"),
            "payout": _parse_cash_amount(cash_desc),
        })

    return {
        "contest_id": contest_id,
        "entries": cd.get("entries"),
        "max_entries": cd.get("maximumEntries"),
        "entry_fee": cd.get("entryFee"),
        "tiers": tiers,
        "error": None if tiers else "no cash payout tiers found",
    }
