"""Grade the Underdog board against real results (Pickems > Underdog Grading).

For every leg in the ledger whose game is final, settle it against the real
stat line (nflverse actuals, same loader the Player Projections page uses) and
record, at BOTH the opening and the closing multiplier:
  * the single-leg return  (m if it hit, else 0, minus 1),
  * our probability vs the market's de-margined probability,
so we can ask the questions the page exists to answer:
  - Are our probabilities better calibrated than the market's? (Brier score)
  - Do legs we flag +EV actually profit, and is that edge there at the OPEN
    but gone by the CLOSE (the "opening lines are more beatable" theory)?

Definitions
  open   = multiplier at the first capture BEFORE kickoff.
  close  = last pre-kickoff multiplier we saw. Trusted only when the leg was
           re-pasted within close_max_age_h of kickoff (ledger last_seen) --
           otherwise it is a stale open, not a close, and is excluded from
           close statistics.
  void   = push (stat lands exactly on the line) or no stat line for a
           matched player in a final game. Voids are listed, never counted as
           wins or losses. CAVEAT on the second kind: nflverse only lists players
           who recorded a stat, so "no stat line" is either inactive (Underdog
           voids the leg -- correct to exclude) or played-and-recorded-nothing
           (the leg really loses/wins). We cannot tell which; for non-thin
           players it is overwhelmingly inactive. The count is reported so a
           surprising number is visible.
  Our probability is the CURRENT week sim (the last pre-kickoff sim for games
  that kicked off -- the sim writers lock kicked-off games), not a snapshot
  frozen at paste time.

Entry point: grade_results(weeks=None) -> dict; summarize(rows) -> dict.
"""
from __future__ import annotations

import datetime as dt
import os
import time
from typing import Dict, List, Optional
from zoneinfo import ZoneInfo

import numpy as np
import pandas as pd

from src.evaluation.player_actuals import load_player_actuals
from src.evaluation.player_proj_eval import match_to_actuals, norm_name, roster_id_map
from src.pickems.grader import (BASE_DIR, IGNORED_MARKETS, MARKET_STATS, TEAM_ALIAS, _match,
                                load_sim_index, market_totals, p_higher)
from src.pickems.ledger import leg_histories

CLOSE_MAX_AGE_H = 6.0
MIN_USAGE = 3.0
ET = ZoneInfo("America/New_York")
EV_BUCKETS = [("< 0%", -1e9, 0.0), ("0 to 10%", 0.0, 10.0), ("10 to 25%", 10.0, 25.0), ("25%+", 25.0, 1e9)]


def schedule_games(week: int, year: int = 2026, base_dir: str = BASE_DIR,
                   sched: Optional[pd.DataFrame] = None) -> Dict[frozenset, dict]:
    """Games of a week keyed by frozenset({away, home}).

    Inputs: week, year; sched (optional pre-loaded schedule frame, for tests).
    Output: {frozenset: {'game_id', 'kickoff' (epoch secs), 'away', 'home'}}.
    Purpose: attach a real kickoff instant (schedule gametime is Eastern) and
        game_id to each Underdog leg, which only carries "Sun 12:00PM CDT".
    """
    if sched is None:
        sched = pd.read_csv(os.path.join(base_dir, "data", "external", f"schedule_{year}.csv"))
    out = {}
    for r in sched[sched["week"] == week].itertuples():
        k = dt.datetime.strptime(f"{r.gameday} {r.gametime}", "%Y-%m-%d %H:%M").replace(tzinfo=ET).timestamp()
        out[frozenset({r.away_team, r.home_team})] = {
            "game_id": r.game_id, "kickoff": k, "away": r.away_team, "home": r.home_team}
    return out


def settle(side: str, line: float, value: float) -> str:
    """'hit' | 'miss' | 'push' for a Higher/Lower leg given the real stat value."""
    if value == line:
        return "push"
    over = value > line
    return "hit" if over == (side == "Higher") else "miss"


def actual_value(row, market: str) -> Optional[float]:
    """Real value of a market from an actuals row (same stat names as the sim), or None."""
    cols = MARKET_STATS.get(market.strip().lower())
    if not cols:
        return None
    return float(sum((row["rTD"] + row["recTD"]) if c == "tds" else row[c] for c in cols))


def _devig(m_this: float, m_other: Optional[float]) -> Optional[float]:
    """De-margined probability of THIS side from both sides' multipliers (None if one-sided)."""
    if m_other is None:
        return None
    a, b = 1 / m_this, 1 / m_other
    return a / (a + b)


def _close_info(hist: List[tuple], last_seen: float, kickoff: float):
    """Open/close multipliers and close age for one leg.

    Inputs: hist [(captured_at, multiplier)] oldest first; last_seen; kickoff (epoch secs).
    Output: (open_m, close_m, close_age_h) or None when every capture is post-kickoff.
        close_age_h = hours between the last pre-kickoff sighting and kickoff.
    """
    pre = [(t, m) for t, m in hist if t < kickoff]
    if not pre:
        return None
    seen_pre = last_seen if last_seen < kickoff else (kickoff if len(pre) == len(hist) else pre[-1][0])
    return pre[0][1], pre[-1][1], max(0.0, (kickoff - max(seen_pre, pre[-1][0])) / 3600.0)


def _mult_at(histories: dict, key: tuple, which: str, kickoff: float) -> Optional[float]:
    """Open or close multiplier of the OTHER side of a prop (key = team, player, market, line, side)."""
    h = histories.get(key)
    if not h:
        return None
    info = _close_info(h["history"], h["last_seen"], kickoff)
    if not info:
        return None
    return info[0] if which == "open" else info[1]


def grade_results(weeks: Optional[List[int]] = None, year: int = 2026, base_dir: str = BASE_DIR,
                  close_max_age_h: float = CLOSE_MAX_AGE_H, min_usage: float = MIN_USAGE,
                  actuals: Optional[pd.DataFrame] = None, sched_by_week: Optional[Dict[int, pd.DataFrame]] = None,
                  index_by_week: Optional[dict] = None, roster_ids: Optional[dict] = None) -> Dict:
    """Settle every final-game leg on the ledger and summarize.

    Inputs: weeks (None = every week in the ledger); close_max_age_h (a close
        counts only if re-pasted this many hours or fewer before kickoff);
        min_usage (thin-player cutoff for the SUMMARY; rows keep everyone);
        actuals / sched_by_week / index_by_week / roster_ids -- optional
        pre-loaded inputs (tests); defaults read the real files.
    Output: {'rows': graded legs, 'summary': summarize(...), 'pending': legs
        waiting on a final game, 'void': pushes, 'no_stat_line': [...],
        'unmatched': legs with no sim player, 'late': legs first captured
        after kickoff, 'weeks': weeks graded, 'close_max_age_h', 'min_usage'}.
    Purpose: the data behind the grading page.
    """
    if weeks is None:
        weeks = sorted({int(r["week"]) for r in _all_ledger_weeks(year, base_dir)})
    act_all = load_player_actuals(year, base_dir, fetch_if_missing=False) if actuals is None else actuals
    ids = roster_id_map(year, base_dir) if roster_ids is None else roster_ids

    rows, void, no_line, unmatched = [], [], [], []
    pending = late = 0
    for week in weeks:
        hists = leg_histories(week, year, base_dir)
        if not hists:
            continue
        sched = schedule_games(week, year, base_dir, None if sched_by_week is None else sched_by_week.get(week))
        index = (index_by_week or {}).get(week) if index_by_week is not None else load_sim_index(week, base_dir)
        if index is None:
            pending += len(hists)
            continue
        by_key = {(h["team"], h["player"], h["market"], h["line"], h["side"]): h for h in hists}
        final_games = set(act_all["game_id"]) if len(act_all) else set()

        # First pass: attach game / sim entry / our probability; collect player-games for one actuals match.
        cand = []
        for h in hists:
            mk = h["market"].strip().lower()
            if mk in IGNORED_MARKETS or mk not in MARKET_STATS or h["side"] not in ("Higher", "Lower"):
                continue
            game = sched.get(frozenset({TEAM_ALIAS.get(h["team"], h["team"]), TEAM_ALIAS.get(h["opp"], h["opp"])}))
            if game is None:
                continue
            entry, _ = _match(index, h["team"], h["player"])
            if entry is None:
                unmatched.append({"player": h["player"], "team": h["team"], "market": h["market"], "week": week})
                continue
            cand.append((h, game, entry))

        summ = pd.DataFrame({
            "game_id": [g["game_id"] for _, g, _ in cand], "Team": [e["team"] for _, _, e in cand],
            "Player": [e["name"] for _, _, e in cand],
            "player_id": [ids.get((e["team"], e["name"])) for _, _, e in cand]})
        act_rows, _ = match_to_actuals(summ, act_all[act_all["game_id"].isin(summ["game_id"].unique())]) \
            if len(summ) and len(act_all) else ([None] * len(summ), None)

        for (h, game, entry), arow in zip(cand, act_rows):
            info = _close_info(h["history"], h["last_seen"], game["kickoff"])
            if info is None:
                late += 1
                continue
            if game["game_id"] not in final_games:
                pending += 1
                continue
            if arow is None:
                no_line.append({"player": h["player"], "team": h["team"], "market": h["market"], "week": week,
                                "game_id": game["game_id"], "usage": entry["usage"]})
                continue
            value = actual_value(arow, h["market"])
            outcome = settle(h["side"], h["line"], value)
            if outcome == "push":
                void.append({"player": h["player"], "market": h["market"], "line": h["line"], "week": week})
                continue
            hit = outcome == "hit"
            open_m, close_m, age = info
            other = "Lower" if h["side"] == "Higher" else "Higher"
            okey = (h["team"], h["player"], h["market"], h["line"], other)
            o_open = _mult_at(by_key, okey, "open", game["kickoff"])
            o_close = _mult_at(by_key, okey, "close", game["kickoff"])
            p_hi = p_higher(market_totals(entry, h["market"]), h["line"])
            sim_p = p_hi if h["side"] == "Higher" else 1.0 - p_hi
            close_ok = age <= close_max_age_h
            rows.append({
                "week": week, "game_id": game["game_id"], "player": h["player"], "team": h["team"], "opp": h["opp"],
                "pos": entry["pos"], "usage": entry["usage"], "market": h["market"], "line": h["line"], "side": h["side"],
                "actual": value, "hit": hit, "sim_prob": sim_p,
                "open_multiplier": open_m, "close_multiplier": close_m if close_ok else None, "close_age_h": age,
                "market_prob_open": _devig(open_m, o_open), "market_prob_close": _devig(close_m, o_close) if close_ok else None,
                "ev_open": (sim_p * open_m - 1) * 100, "ev_close": (sim_p * close_m - 1) * 100 if close_ok else None,
                "ret_open": open_m * hit - 1, "ret_close": (close_m * hit - 1) if close_ok else None,
            })
    out = {"rows": rows, "pending": pending, "void": void, "no_stat_line": no_line, "unmatched": unmatched,
           "late": late, "weeks": weeks, "close_max_age_h": close_max_age_h, "min_usage": min_usage}
    out["summary"] = summarize(rows, min_usage)
    return out


def _all_ledger_weeks(year: int, base_dir: str):
    from src.pickems.ledger import _read_rows, ledger_path
    return _read_rows(ledger_path(year, base_dir))


def _roi_block(df: pd.DataFrame, ret_col: str, mean_p_col: str = "sim_prob") -> dict:
    """n, hit rate, mean sim prob, mean return per $1 and its standard error for a leg subset."""
    d = df[df[ret_col].notna()]
    n = len(d)
    if n == 0:
        return {"n": 0, "hit_rate": None, "mean_sim": None, "roi": None, "se": None}
    r = d[ret_col].to_numpy(dtype=float)
    return {"n": n, "hit_rate": float(d["hit"].mean()), "mean_sim": float(d[mean_p_col].mean()),
            "roi": float(r.mean()), "se": float(r.std(ddof=1) / np.sqrt(n)) if n > 1 else None}


def _brier(df: pd.DataFrame, p_col: str) -> Optional[float]:
    d = df[df[p_col].notna()]
    return float(((d[p_col] - d["hit"].astype(float)) ** 2).mean()) if len(d) else None


def summarize(rows: List[dict], min_usage: float = MIN_USAGE) -> dict:
    """Aggregate graded legs (thin players below min_usage excluded).

    Inputs: rows (grade_results rows); min_usage.
    Output: dict with
      overall      -- n, hit rate, mean sim prob, flat-stake return on EVERY leg at open / close
      plus_ev      -- legs we flagged +EV, at the open and at the close (n, hit, roi, se)
      ev_buckets   -- same by EV band at the open (does a bigger flagged edge pay more?)
      brier        -- Brier score of OUR probability vs the market's, on legs with both sides posted
                      (lower = better; same legs for each comparison)
      by_market, by_side, calibration (sim prob bins vs actual hit rate)
    Purpose: n is small until several weeks are graded -- read se before trusting any roi.
    """
    df = pd.DataFrame(rows)
    if df.empty:
        return {"n": 0}
    df = df[df["usage"] >= min_usage].copy()
    if df.empty:
        return {"n": 0}
    two_open = df[df["market_prob_open"].notna()]
    two_close = df[df["market_prob_close"].notna()]
    out = {
        "n": len(df),
        "overall": {"open": _roi_block(df, "ret_open"), "close": _roi_block(df, "ret_close")},
        "plus_ev": {"open": _roi_block(df[df["ev_open"] > 0], "ret_open"),
                    "close": _roi_block(df[df["ev_close"].fillna(-1) > 0], "ret_close")},
        "ev_buckets": [{"label": lbl, **_roi_block(df[(df["ev_open"] >= lo) & (df["ev_open"] < hi)], "ret_open")}
                       for lbl, lo, hi in EV_BUCKETS],
        "brier": {
            "open": {"n": len(two_open), "sim": _brier(two_open, "sim_prob"), "market": _brier(two_open, "market_prob_open")},
            "close": {"n": len(two_close), "sim": _brier(two_close, "sim_prob"), "market": _brier(two_close, "market_prob_close")},
        },
        "by_market": {}, "by_side": {}, "calibration": [],
    }
    for key, col in (("by_market", "market"), ("by_side", "side")):
        for name, g in df.groupby(col):
            two = g[g["market_prob_open"].notna()]
            out[key][name] = {"n": len(g), "hit_rate": float(g["hit"].mean()), "mean_sim": float(g["sim_prob"].mean()),
                              "plus_ev_open": _roi_block(g[g["ev_open"] > 0], "ret_open"),
                              "brier_sim": _brier(two, "sim_prob"), "brier_market": _brier(two, "market_prob_open")}
    edges = np.linspace(0, 1, 11)
    df["bin"] = pd.cut(df["sim_prob"], edges, include_lowest=True)
    for b, g in df.groupby("bin", observed=True):
        out["calibration"].append({"lo": float(b.left), "hi": float(b.right), "n": len(g),
                                   "mean_sim": float(g["sim_prob"].mean()), "hit_rate": float(g["hit"].mean())})
    return out
