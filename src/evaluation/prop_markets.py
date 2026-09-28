"""Prediction-market player props (Polymarket US, first venue) vs. our sims.

Draft 2026-09-26 -- the live, read-only "what does the market say vs. what do
our sims say" view behind GET /api/props/polymarket and the Prop Markets page
(frontend/src/pages/PropMarkets.jsx). NOT the graded evaluation yet: grading
needs PRE-KICKOFF prices, which requires the capture ledger + kickoff backfill
described in docs/implementation_plans/prediction_market_props_plan.md. Until that lands this
module only compares against whatever is on the book right now, and it only
computes edges for games that haven't kicked off.

How a Polymarket US player prop maps onto a sim:
  Every player prop is a ladder of binary "Will X record N+ <stat>?" markets
  (slug suffix `-gte{N}`), YES = stat >= N. Our week sim parquet
  (data/interim/dfs_week_{W}_players.parquet) holds one row per player per
  iteration, so P_sim(YES) = share of that game's iterations where the
  player's stat >= N. A player missing from an iteration counts as 0.

Prices and fees (docs.polymarket.us/fees, verified 2026-09-26):
  bid/ask are the YES book (bestBidQuote/bestAskQuote). Buying YES costs the
  ask; taking NO costs 1 - bid (short YES). Taker fee per contract =
  feeCoefficient * p * (1 - p) (0.0695 today), symmetric in p, so the same fee
  applies to either side. Edges below are expected value per $1-payout
  contract AFTER the taker fee, assuming our sim probability is right -- a
  statement about disagreement, not proof of value (the sim is ungraded
  against markets so far).
"""

import re
import time
from collections import Counter, defaultdict

import numpy as np
import pandas as pd

from src.evaluation.player_proj_eval import norm_name

# sportsMarketType -> (sim stat key, UI label). Keys that aren't raw parquet
# columns (scrimYds, anyTD, pprESPN) are derived in add_derived_stats().
PRICED_STATS = {
    "football_player_passing_yards": ("pYds", "Pass yds"),
    "football_player_passing_touchdowns": ("pTD", "Pass TD"),
    "football_player_passing_attempts": ("pAtt", "Pass att"),
    "football_player_passing_completions": ("pCmp", "Pass comp"),
    "football_player_interceptions_thrown": ("int", "INT thrown"),
    "football_player_rushing_yards": ("rYds", "Rush yds"),
    "football_player_rushing_attempts": ("rAtt", "Rush att"),
    "football_player_receiving_yards": ("recYds", "Rec yds"),
    "football_player_receptions": ("rec", "Receptions"),
    "football_player_scrimmage_yards": ("scrimYds", "Scrimmage yds"),
    # Market rules: "touchdowns (excluding passing touchdowns)".
    "football_player_touchdowns": ("anyTD", "TDs (rush+rec)"),
    # Market rules: ESPN standard PPR, fractional.
    "football_player_fantasy_points_ppr": ("pprESPN", "Fantasy pts (PPR)"),
}

# Player-prop types Polymarket lists that our sim output can't price yet, with
# the reason (shown in the UI's coverage panel).
UNPRICED_STATS = {
    "football_player_first_touchdown": "sim output doesn't record TD order",
    "football_player_team_first_touchdown": "sim output doesn't record TD order",
    "football_player_longest_reception": "sim output doesn't keep per-play reception lengths",
    "football_player_most_passing_yards": "head-to-head market -- priceable from iteration-aligned QB rows, not built yet",
}

SIM_COLUMNS = ["Player", "Team", "Pos", "iteration", "pAtt", "pCmp", "pYds", "pTD", "int",
               "rAtt", "rYds", "rTD", "rec", "recYds", "recTD", "fumbles_lost"]

_GTE_RE = re.compile(r"-gte(\d+(?:[.p]\d+)?)$")

_EMPTY_SIM_FIELDS = {"sim_player": None, "sim_pos": None, "matched_by": None, "sim_p": None, "sim_mean": None,
                     "diff": None, "flag": None, "yes_ev": None, "no_ev": None, "best_side": None, "best_ev": None}

# |sim - market| this large on a liquid prop is far more often a sim INPUT
# problem (role/usage/injury, or a bug) than a real edge. First draft saw
# e.g. sims with ~0 usage for a player the market has as a starter, and a
# player whose every simmed catch was -10 yds (2026-09-26).
LARGE_GAP = 0.30


def sanity_flag(sim_p, mid):
    """Inputs: sim P(YES), market mid. Output: None, 'sim_extreme' (sim says
    0% or 100% while the market is between 5% and 95%), or 'large_gap'
    (|sim - mid| >= LARGE_GAP). Flagged rows are shown but kept out of the
    default edge ranking."""
    if sim_p in (0.0, 1.0) and 0.05 < mid < 0.95:
        return "sim_extreme"
    if abs(sim_p - mid) >= LARGE_GAP:
        return "large_gap"
    return None


def add_derived_stats(df):
    """Adds the composite stats the markets settle on.

    Inputs: players frame with SIM_COLUMNS (from the week sim parquet).
    Output: same frame (copy) plus scrimYds, anyTD, pprESPN.
    pprESPN = ESPN standard PPR: 0.04/pass yd, 4/pass TD, -2/INT, 0.1/rush or
    rec yd, 6/rush or rec TD, 1/reception, -2/fumble LOST. The sim has no
    2-point conversions (engine gap, AGENTS.md §0), so those are missing --
    a small, known downward bias on this one market type."""
    out = df.copy()
    f = lambda c: out[c].fillna(0) if c in out.columns else 0
    out["scrimYds"] = f("rYds") + f("recYds")
    out["anyTD"] = f("rTD") + f("recTD")
    out["pprESPN"] = (0.04 * f("pYds") + 4 * f("pTD") - 2 * f("int") + 0.1 * f("rYds") + 6 * f("rTD")
                      + f("rec") + 0.1 * f("recYds") + 6 * f("recTD") - 2 * f("fumbles_lost"))
    return out


def parse_threshold(market):
    """YES threshold N of a 'N+' ladder market.

    Inputs: one Polymarket US market dict.
    Output: float N, or None if it can't be read.
    Source order: the slug suffix (`...-gte125`, the machine-readable form),
    then metadata.lineLabel ('125+' or 'O15')."""
    m = _GTE_RE.search(market.get("slug") or "")
    if m:
        return float(m.group(1).replace("p", "."))
    label = str((market.get("metadata") or {}).get("lineLabel") or "")
    m = re.search(r"(\d+(?:\.\d+)?)", label)
    return float(m.group(1)) if m else None


def _quote(market, key):
    q = market.get(key)
    try:
        return float(q["value"]) if isinstance(q, dict) and q.get("value") is not None else None
    except (TypeError, ValueError):
        return None


def taker_fee(price, coef):
    """Polymarket US taker fee per contract: coef * p * (1 - p). Inputs: price
    in (0,1), coef (float). Output: $ per contract (float)."""
    return coef * price * (1.0 - price)


def side_edges(sim_p, bid, ask, coef):
    """Expected value per contract of taking each side at the current book.

    Inputs: sim_p (our P(YES)), bid/ask (YES book, may be None), coef (fee).
    Output: (yes_ev, no_ev) in $ per $1-payout contract, None where that side
    has no quote. yes_ev = sim_p - ask - fee(ask); no_ev = (1 - sim_p) -
    (1 - bid) - fee(bid) = bid - sim_p - fee(bid)."""
    yes_ev = None if ask is None else sim_p - ask - taker_fee(ask, coef)
    no_ev = None if bid is None else bid - sim_p - taker_fee(bid, coef)
    return yes_ev, no_ev


def game_phase(event, now=None):
    """'pre' before kickoff, 'final' once the event is closed, else 'live'."""
    now = now if now is not None else time.time()
    start = pd.Timestamp(event.get("startTime")).timestamp() if event.get("startTime") else None
    if event.get("closed"):
        return "final"
    if start is not None and now < start:
        return "pre"
    return "live"


def _team_code_map(event, away_team, home_team):
    """Polymarket teamId -> our (nflverse) team code for this event.
    Matches on abbreviation, then fills the remaining team by elimination."""
    ours = {away_team, home_team}
    alias = {"LAR": "LA"}
    out = {}
    for t in event.get("teams") or []:
        code = alias.get(str(t.get("abbreviation", "")).upper(), str(t.get("abbreviation", "")).upper())
        if code in ours:
            out[t["id"]] = code
    left = ours - set(out.values())
    unmatched = [t["id"] for t in (event.get("teams") or []) if t["id"] not in out]
    if len(left) == 1 and len(unmatched) == 1:
        out[unmatched[0]] = left.pop()
    return out


def normalize_player_props(event, game):
    """Flattens one event's player-prop markets into rows.

    Inputs: event (Polymarket US event dict), game (schedule row dict:
    game_id, week, away_team, home_team).
    Output: (rows, skipped) -- rows: list of dicts, one per priced-type ladder
    rung; skipped: Counter of unpriced player-prop types seen."""
    team_map = _team_code_map(event, game["away_team"], game["home_team"])
    phase = game_phase(event)
    rows, skipped = [], Counter()
    for m in event.get("markets") or []:
        mtype = m.get("sportsMarketType") or ""
        if not mtype.startswith("football_player_"):
            continue
        if mtype not in PRICED_STATS:
            skipped[mtype] += 1
            continue
        meta = m.get("metadata") or {}
        stat, label = PRICED_STATS[mtype]
        bid, ask = _quote(m, "bestBidQuote"), _quote(m, "bestAskQuote")
        try:
            coef = float(m.get("feeCoefficient"))
        except (TypeError, ValueError):
            coef = None
        rows.append({
            "game_id": game["game_id"], "week": int(game["week"]),
            "matchup": f"{game['away_team']} @ {game['home_team']}",
            "kickoff": event.get("startTime"), "phase": phase,
            "team": team_map.get(meta.get("teamId")),
            "pm_player": meta.get("playerName"), "pm_player_id": meta.get("playerId"),
            # (question/status left out on purpose: ~30% of the payload, and
            # the UI rebuilds the "N+ stat" label from threshold + stat_label.)
            "stat": stat, "stat_label": label, "threshold": parse_threshold(m),
            "slug": m.get("slug"),
            "bid": bid, "ask": ask,
            "mid": (bid + ask) / 2 if bid is not None and ask is not None else None,
            "spread": (ask - bid) if bid is not None and ask is not None else None,
            "fee_coef": coef,
        })
    return rows, skipped


def _sim_index(players, n_iter_by_team):
    """(team, norm_name) -> {'name', 'pos', 'n', stat: sorted np.array} for
    every sim player, plus a (team, last name, first initial) fallback index.
    Inputs: players frame (derived stats added), n_iter_by_team {team: int}."""
    stats = sorted({s for s, _ in PRICED_STATS.values()})
    idx, fallback = {}, defaultdict(list)
    players = players[players["Player"] != "Defense"]
    for (team, name), grp in players.groupby(["Team", "Player"], sort=False):
        n = int(n_iter_by_team.get(team, 0))
        if not n:
            continue
        # Sum per iteration in case a player ever appears twice in one run.
        per_iter = grp.groupby("iteration")[stats].sum()
        entry = {"name": name, "pos": grp["Pos"].iloc[0], "n": n, "present": len(per_iter)}
        for s in stats:
            entry[s] = np.sort(per_iter[s].to_numpy(dtype=float))
        key = norm_name(name)
        idx[(team, key)] = entry
        parts = str(name).replace(".", "").split()
        if len(parts) >= 2:
            fallback[(team, parts[-1].lower() if parts[-1].lower() not in ("jr", "sr", "ii", "iii", "iv", "v") else parts[-2].lower(),
                      parts[0][0].lower())].append(entry)
    return idx, fallback


def _match(idx, fallback, team, pm_name):
    """Our sim entry for a Polymarket player, or (None, None).
    Exact normalized name within team first, then unique last-name +
    first-initial within team."""
    if team is None or not pm_name:
        return None, None
    e = idx.get((team, norm_name(pm_name)))
    if e is not None:
        return e, "name"
    parts = str(pm_name).replace(".", "").split()
    if len(parts) >= 2:
        last = parts[-1].lower() if parts[-1].lower() not in ("jr", "sr", "ii", "iii", "iv", "v") else parts[-2].lower()
        cands = fallback.get((team, last, parts[0][0].lower()), [])
        if len(cands) == 1:
            return cands[0], "last+initial"
    return None, None


def price_rows(rows, players, n_iter_by_team):
    """Adds sim probability + edges to normalized rows (in place).

    Inputs: rows (normalize_player_props output), players (sim frame with
    SIM_COLUMNS), n_iter_by_team ({team: iterations simmed for its game}).
    Output: the same list; each row gains sim_player, sim_pos, matched_by,
    sim_p, sim_mean, diff (sim_p - mid), yes_ev, no_ev, best_side, best_ev.
    P(stat >= N) = (#iterations with stat >= N) / n; iterations where the
    player has no row count as 0 (so they add to the count only when N <= 0)."""
    idx, fallback = _sim_index(add_derived_stats(players), n_iter_by_team)
    for r in rows:
        e, how = _match(idx, fallback, r["team"], r["pm_player"])
        r.update(_EMPTY_SIM_FIELDS)
        if e is None or r["threshold"] is None:
            continue
        vals, n, k = e[r["stat"]], e["n"], r["threshold"]
        hits = len(vals) - np.searchsorted(vals, k, side="left")
        if k <= 0:
            hits += n - e["present"]
        p = float(hits) / n
        r.update({"sim_player": e["name"], "sim_pos": e["pos"], "matched_by": how,
                  "sim_p": p, "sim_mean": float(vals.sum()) / n})
        if r["mid"] is not None:
            r["diff"] = p - r["mid"]
            r["flag"] = sanity_flag(p, r["mid"])
        # Edges only before kickoff -- a live/final book isn't a pre-game price.
        if r["phase"] == "pre" and r["fee_coef"] is not None:
            yes_ev, no_ev = side_edges(p, r["bid"], r["ask"], r["fee_coef"])
            r["yes_ev"], r["no_ev"] = yes_ev, no_ev
            cands = [(v, s) for v, s in ((yes_ev, "YES"), (no_ev, "NO")) if v is not None]
            if cands:
                r["best_ev"], r["best_side"] = max(cands)
    return rows


def summarize(rows, skipped):
    """Coverage + disagreement snapshot for the page header.

    Inputs: priced rows, skipped Counter (unpriced types).
    Output: dict -- by_stat list ({stat_label, markets, priced, two_sided,
    median_spread, mean_diff, mean_abs_diff}), unmatched players
    [{team, pm_player, markets}], unpriced [{market_type, count, reason}].
    mean_diff > 0 = our sims price YES higher than the market mid on average.
    This is disagreement, not accuracy: accuracy needs graded pre-kickoff prices."""
    by = defaultdict(list)
    for r in rows:
        by[r["stat_label"]].append(r)
    by_stat = []
    for label, rs in sorted(by.items(), key=lambda kv: -len(kv[1])):
        diffs = [r["diff"] for r in rs if r["diff"] is not None]
        spreads = [r["spread"] for r in rs if r["spread"] is not None]
        by_stat.append({
            "stat_label": label, "markets": len(rs),
            "priced": sum(r["sim_p"] is not None for r in rs),
            "flagged": sum(r.get("flag") is not None for r in rs),
            "two_sided": len(spreads),
            "median_spread": float(np.median(spreads)) if spreads else None,
            "mean_diff": float(np.mean(diffs)) if diffs else None,
            "mean_abs_diff": float(np.mean(np.abs(diffs))) if diffs else None,
        })
    unmatched = Counter((r["team"], r["pm_player"]) for r in rows if r["sim_p"] is None and r["pm_player"])
    return {
        "by_stat": by_stat,
        "unmatched": [{"team": t, "pm_player": p, "markets": n} for (t, p), n in unmatched.most_common()],
        "unpriced": [{"market_type": t, "count": n, "reason": UNPRICED_STATS.get(t, "not mapped yet")}
                     for t, n in skipped.most_common()],
    }


def build_week_props(week_games, events, players, n_iter_by_team):
    """Whole-week payload.

    Inputs: week_games (schedule rows as dicts), events ({game_id: event or
    None}, from polymarket_us_client.fetch_events_for_games), players (week
    sim frame or None), n_iter_by_team.
    Output: {rows, summary, games: [{game_id, matchup, found, phase,
    markets}]}. With no sim parquet, rows still carry market prices, sim
    fields stay None."""
    all_rows, skipped, games = [], Counter(), []
    for g in week_games:
        ev = events.get(g["game_id"])
        entry = {"game_id": g["game_id"], "matchup": f"{g['away_team']} @ {g['home_team']}",
                 "found": ev is not None, "phase": game_phase(ev) if ev else None, "markets": 0}
        if ev:
            rows, sk = normalize_player_props(ev, g)
            all_rows += rows
            skipped += sk
            entry["markets"] = len(rows)
        games.append(entry)
    if players is not None and len(players):
        price_rows(all_rows, players, n_iter_by_team)
    else:
        for r in all_rows:
            r.update(_EMPTY_SIM_FIELDS)
    return {"rows": all_rows, "summary": summarize(all_rows, skipped), "games": games}
