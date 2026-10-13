"""Grade Underdog Pick'em legs against our sims (Pickems > Underdog Lines).

Underdog multipliers ARE the odds: a leg paying m x implies a win probability
of 1/m (before Underdog's margin). A leg is +EV exactly when our sim
probability p satisfies p * m > 1, i.e. p > 1/m -- that 1/m is the "minimum
multiplier" threshold the page reports. The prop's own line (e.g. 0.5 TDs, or
91.5 yards) only defines the event being priced; it is NOT a probability cutoff.

The sim index keeps each player's per-iteration stat arrays ALIGNED by sim
iteration, so any line can be priced (slider) and, later, same-game legs can be
combined by counting iterations where they all hit (correlation for free).

Entry points: load_sim_index(week) -> index, grade_legs(legs, index) -> dict.
"""
from __future__ import annotations

import os
from collections import defaultdict
from typing import Dict, List, Optional, Tuple

import numpy as np
import pandas as pd

from src.evaluation.player_proj_eval import norm_name
from src.pickems.underdog_parser import PickemLeg

BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SUFFIXES = ("jr", "sr", "ii", "iii", "iv", "v")
# Underdog team code -> our sim/roster code (only the ones that differ).
TEAM_ALIAS = {"LAR": "LA", "JAC": "JAX", "WSH": "WAS"}
LARGE_GAP = 0.25  # |sim - market| above this is flagged for a sanity check

# Underdog market name (lowercased) -> sim stat columns summed per iteration.
# "tds" is rTD + recTD (built at index time). Names are best guesses until a real
# paste of each market is seen; an unmapped name is reported in `unsupported`
# with its exact text, never silently dropped.
MARKET_STATS: Dict[str, List[str]] = {
    "rush + rec tds": ["tds"],
    "passing yards": ["pYds"], "pass yards": ["pYds"],
    "passing tds": ["pTD"], "pass tds": ["pTD"], "passing touchdowns": ["pTD"],
    "pass attempts": ["pAtt"], "passing attempts": ["pAtt"],
    "pass completions": ["pCmp"], "completions": ["pCmp"],
    "interceptions": ["int"],
    "rushing yards": ["rYds"], "rush yards": ["rYds"],
    "rush attempts": ["rAtt"], "rushing attempts": ["rAtt"],
    "receiving yards": ["recYds"], "rec yards": ["recYds"],
    "receptions": ["rec"], "targets": ["targets"],
    "rush + rec yards": ["rYds", "recYds"], "rushing + receiving yards": ["rYds", "recYds"],
}
# Roulette-wheel markets deliberately not graded (Cam's call, 2026-10-03).
IGNORED_MARKETS = {"first td scorer", "last td scorer"}
STORED_STATS = ["pYds", "pTD", "pAtt", "pCmp", "int", "rYds", "rAtt", "rec", "recYds", "targets", "tds"]

_INDEX_CACHE: Dict[int, Tuple[float, tuple]] = {}


def load_sim_index(week: int, base_dir: str = BASE_DIR):
    """Per-player, iteration-aligned stat arrays from the week's sim parquet.

    Inputs: week (int); base_dir (repo root).
    Output: (idx, fallback) or None when no parquet exists.
        idx[(team, norm_name)] = {'name','team','pos','n','usage','game_id','stats':{stat: int16[n]}}
        where stats[s][k] is the player's value in sim iteration k (0 when the
        player had no row that iteration). usage = mean rush attempts + targets
        per game (pass attempts for QBs) -- the "thin player" measure.
        fallback[(team, last, first_initial)] = [entries] for fuzzy matching.
    Purpose: one pass over the 4M-row parquet, cached on its mtime, so grading
        a board afterwards is instant. ~75 MB of int16 per cached week.
    """
    path = os.path.join(base_dir, "data", "interim", f"dfs_week_{week}_players.parquet")
    if not os.path.exists(path):
        return None
    mtime = os.path.getmtime(path)
    cached = _INDEX_CACHE.get(week)
    if cached and cached[0] == mtime:
        return cached[1]

    cols = list(dict.fromkeys(["game_id", "Team", "Player", "Pos", "iteration", "rTD", "recTD", "targets"] +
                              [c for c in STORED_STATS if c != "tds"]))
    pl = pd.read_parquet(path, columns=cols)
    pl = pl[pl["Player"] != "Defense"]
    pl["tds"] = pl["rTD"] + pl["recTD"]
    n_by_game = pl.groupby("game_id")["iteration"].nunique()

    idx, fallback = {}, defaultdict(list)
    for (game_id, team, name), grp in pl.groupby(["game_id", "Team", "Player"], sort=False):
        n = int(n_by_game[game_id])
        it = grp["iteration"].to_numpy(dtype=int)
        stats = {}
        for s in STORED_STATS:
            arr = np.zeros(n, dtype=np.int16)
            np.add.at(arr, it, grp[s].to_numpy(dtype=np.int16))  # sums a duplicate row in one iteration
            stats[s] = arr
        pos = grp["Pos"].iloc[0]
        usage = float(stats["pAtt"].sum() / n) if pos == "QB" else \
            float((grp["rAtt"].sum() + grp["targets"].sum()) / n)
        entry = {"name": name, "team": team, "pos": pos, "n": n, "usage": usage, "stats": stats,
                 "game_id": game_id}
        idx[(team, norm_name(name))] = entry
        parts = str(name).replace(".", "").split()
        if len(parts) >= 2:
            last = parts[-1].lower() if parts[-1].lower() not in SUFFIXES else parts[-2].lower()
            fallback[(team, last, parts[0][0].lower())].append(entry)
    out = (idx, dict(fallback))
    _INDEX_CACHE[week] = (mtime, out)
    return out


def _match(index, team: str, name: str):
    """Sim entry for an Underdog player: exact normalized name within team,
    else unique last-name + first-initial within team. Returns (entry, how)."""
    idx, fallback = index
    team = TEAM_ALIAS.get(team, team)
    e = idx.get((team, norm_name(name)))
    if e is not None:
        return e, "name"
    parts = str(name).replace(".", "").split()
    if len(parts) >= 2:
        last = parts[-1].lower() if parts[-1].lower() not in SUFFIXES else parts[-2].lower()
        cands = fallback.get((team, last, parts[0][0].lower()), [])
        if len(cands) == 1:
            return cands[0], "last+initial"
    return None, None


def market_totals(entry: dict, market: str) -> Optional[np.ndarray]:
    """Per-iteration value of a market for a player (sum of its stat columns),
    or None when the market name is not mapped."""
    cols = MARKET_STATS.get(market.strip().lower())
    if not cols:
        return None
    return sum(entry["stats"][c].astype(np.int32) for c in cols)


def p_higher(totals: np.ndarray, line: float) -> float:
    """P(value > line) over the sim iterations. At line 0.5 on TDs = P(>= 1 TD)."""
    return float((totals > line).mean())


def line_ladder(entry: dict, market: str, line: float, span: int = 12) -> Optional[Dict]:
    """Our probabilities and break-even multipliers for one player's market at nearby lines.

    Inputs: entry (sim index entry from _match); market (Underdog market name);
        line (the line on the board); span (how many lines to show each side).
    Output: {'mean','median','lines':[{line, p_higher, p_lower, min_mult_higher, min_mult_lower}]}
        or None for an unmapped market. Lines step by 1.0 from the board line (Underdog
        lines are x.5), floored at 0.
    Purpose: lets the page answer "if Underdog moved this line up/down, what multiplier
        would still make Higher/Lower +EV?" -- min_mult = 1 / probability, same rule as
        grade_legs. Lower uses P(value < line); on whole-number lines the push mass is
        in neither side.
    """
    totals = market_totals(entry, market)
    if totals is None:
        return None
    lines = []
    for k in range(-span, span + 1):
        ln = line + k
        if ln < 0:
            continue
        p_hi, p_lo = float((totals > ln).mean()), float((totals < ln).mean())
        lines.append({"line": ln, "p_higher": p_hi, "p_lower": p_lo,
                      "min_mult_higher": (1 / p_hi) if p_hi > 0 else None,
                      "min_mult_lower": (1 / p_lo) if p_lo > 0 else None})
    return {"mean": float(totals.mean()), "median": float(np.median(totals)), "lines": lines}


def combo_probability(legs: List[dict], index) -> Optional[Dict]:
    """Probability that every leg of a slip hits, using the sims' iteration alignment.

    Inputs: legs -- dicts with team, player, market, line, side ('Higher'|'Lower'); index.
    Output: {'joint', 'independent', 'legs':[{p, matched}], 'games'} or None if any leg can't be
        matched/priced (the caller reports which). joint = legs in the SAME game are counted
        together per sim iteration (captures correlation, e.g. a TD and the pass attempts that
        led to it); different games are independent, so their joint probs multiply.
        independent = plain product of single-leg probabilities, for comparison.
    Purpose: entry EV for a multi-leg pick'em slip. A Lower on a whole-number line counts a push
        as a miss (Underdog would void/reprice it) -- conservative.
    """
    by_game: Dict[object, list] = defaultdict(list)
    singles = []
    for l in legs:
        entry, _ = _match(index, l["team"], l["player"])
        totals = market_totals(entry, l["market"]) if entry is not None else None
        if totals is None:
            return None
        hit = (totals > l["line"]) if l["side"] == "Higher" else (totals < l["line"])
        singles.append(float(hit.mean()))
        by_game[entry["game_id"]].append(hit)
    joint = 1.0
    for hits in by_game.values():
        joint *= float(np.logical_and.reduce(hits).mean())
    return {"joint": joint, "independent": float(np.prod(singles)), "singles": singles,
            "games": len(by_game)}


def grade_legs(legs: List[PickemLeg], index) -> Dict:
    """Price every leg against the sims.

    Inputs: legs (PickemLeg list -- the accumulated board); index (load_sim_index).
    Output: {'rows': [...], 'unmatched': [...], 'unsupported': {market: n}}.
        Each row = the leg's fields plus:
          pos, usage         player's position + sim opportunities/game (thin filter)
          implied_prob       1/multiplier (includes Underdog's margin)
          market_prob        two-sided de-margined probability, or None when only
                             one side is offered
          sim_mean           our mean for the market's stat (the "sim line")
          sim_prob           our probability for THIS side
          edge_factor        sim_prob * multiplier (>1 = +EV for a single leg)
          ev_pct             (edge_factor - 1) * 100
          min_multiplier     1 / sim_prob -- take the leg only at or above this
          matched_by         'name' | 'last+initial'
          flag               'large_gap' when sim and market differ by > 25 pts
    Purpose: the product -- a multiplier-aware, rankable list of legs.
    """
    # Both sides of the same (player, market, line) give the de-margined market prob.
    sides: Dict[tuple, Dict[str, float]] = defaultdict(dict)
    for l in legs:
        sides[(l.team, l.player, l.market, l.line)][l.side] = l.multiplier

    rows, unmatched, unsupported = [], [], defaultdict(int)
    seen_unmatched = set()
    for l in legs:
        mk = l.market.strip().lower()
        if mk in IGNORED_MARKETS:
            continue
        if mk not in MARKET_STATS or l.side not in ("Higher", "Lower"):
            unsupported[l.market] += 1
            continue
        entry, how = _match(index, l.team, l.player)
        if entry is None:
            if (l.team, l.player) not in seen_unmatched:
                seen_unmatched.add((l.team, l.player))
                unmatched.append({"player": l.player, "team": l.team})
            continue
        totals = market_totals(entry, l.market)
        p_hi = p_higher(totals, l.line)
        sim_p = p_hi if l.side == "Higher" else 1.0 - p_hi
        pair = sides[(l.team, l.player, l.market, l.line)]
        market_p = None
        if "Higher" in pair and "Lower" in pair:
            ih, il = 1 / pair["Higher"], 1 / pair["Lower"]
            hi_dm = ih / (ih + il)
            market_p = hi_dm if l.side == "Higher" else 1 - hi_dm
        edge = sim_p * l.multiplier
        row = l.to_dict()
        row.update({
            "sim_player": entry["name"], "matched_by": how, "pos": entry["pos"], "usage": entry["usage"],
            "sim_mean": float(totals.mean()),
            "implied_prob": 1 / l.multiplier, "market_prob": market_p,
            "sim_prob": sim_p, "edge_factor": edge, "ev_pct": (edge - 1) * 100,
            "min_multiplier": (1 / sim_p) if sim_p > 0 else None,
            "flag": "large_gap" if market_p is not None and abs(sim_p - market_p) > LARGE_GAP else None,
        })
        rows.append(row)
    rows.sort(key=lambda r: r["ev_pct"], reverse=True)
    return {"rows": rows, "unmatched": unmatched, "unsupported": dict(unsupported)}
