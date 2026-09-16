"""Shared loaders for calibrate_ownership_model.py and train_ownership_model.py
-- both need the same "reconstruct what the model would have seen pre-lock"
inputs (salary snapshot + that week's cached sim projections + Vegas implied
team totals), so this is the one place that logic lives. Not run directly.
"""
from __future__ import annotations

import json
import os

import pandas as pd

BASE = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
ARCHIVE = os.path.join(BASE, "data", "dfs_ownership")
SCHEDULE = os.path.join(BASE, "data", "external", "schedule_2026.csv")
INTERIM = os.path.join(BASE, "data", "interim")


def load_sim_projections(week: int) -> dict:
    """{game_id: {(normalized_name, team): sim_projection_dict}} from the
    cached weekly sim results (data/interim/week_N_sim_results.json) -- the
    same projections that were live around lock time for that week. Requires
    normalize_player_name (from src.scrapers.dk_scraper) be applied by the
    caller when building the lookup key; done here so both scripts key
    identically."""
    from src.scrapers.dk_scraper import normalize_player_name

    path = os.path.join(INTERIM, f"week_{week}_sim_results.json")
    if not os.path.exists(path):
        return {}
    d = json.load(open(path))
    out = {}
    for gid, g in d.get("games", {}).items():
        by_key = {}
        for p in g.get("projections", []):
            by_key[(normalize_player_name(p["name"]), p["team"])] = p
        out[gid] = by_key
    return out


def vegas_implied(week: int) -> dict:
    """{team: implied_total} for every team playing that week, from the
    schedule's home-favored spread_line (matches
    snapshot_slate_salaries.py's _vegas_for_game)."""
    sched = pd.read_csv(SCHEDULE)
    wk = sched[(sched["week"] == week) & (sched["game_type"] == "REG")]
    out = {}
    for _, r in wk.iterrows():
        if pd.isna(r.get("total_line")) or pd.isna(r.get("spread_line")):
            continue
        total, spread = float(r["total_line"]), float(r["spread_line"])
        out[r["home_team"]] = round((total + spread) / 2.0, 2)
        out[r["away_team"]] = round((total - spread) / 2.0, 2)
    return out


def vegas_spread(week: int) -> dict:
    """{team: signed spread from that team's own perspective, positive =
    favored} -- e.g. a team favored by 3 gets +3.0, their opponent -3.0.
    Companion to vegas_implied(); same schedule source."""
    sched = pd.read_csv(SCHEDULE)
    wk = sched[(sched["week"] == week) & (sched["game_type"] == "REG")]
    out = {}
    for _, r in wk.iterrows():
        if pd.isna(r.get("spread_line")):
            continue
        spread = float(r["spread_line"])  # home-favored convention
        out[r["home_team"]] = spread
        out[r["away_team"]] = -spread
    return out


def cash_consensus_frac(week: int) -> dict:
    """{(normalized_name, team): fraction of this week's cash-consensus
    lineups (see app.py's _generate_cash_consensus_lineups, cached at the
    top level of week_N_sim_results.json as `cash_consensus_lineups`, ~10
    cash-optimal classic builds) a player appears in, 0-1. Classic-slate
    concept only -- showdown has no equivalent structure in the cache, use
    optimal_cpt_pct/optimal_flex_pct (already in each projection) instead."""
    from src.scrapers.dk_scraper import normalize_player_name

    path = os.path.join(INTERIM, f"week_{week}_sim_results.json")
    if not os.path.exists(path):
        return {}
    d = json.load(open(path))
    lineups = d.get("cash_consensus_lineups") or []
    if not lineups:
        return {}
    counts: dict = {}
    for lu in lineups:
        for slot in lu.get("slots", []):
            key = (normalize_player_name(slot["name"]), slot["team"])
            counts[key] = counts.get(key, 0) + 1
    n = len(lineups)
    return {k: v / n for k, v in counts.items()}


def n_games_on_slate(week: int) -> int:
    """Count of distinct REG games in the schedule for this week -- a proxy
    for classic main-slate size (a bye-heavy week spreads ownership over
    fewer games than a full 16-game week). Showdown is always exactly 1
    game, so this feature is only meaningful for the classic model."""
    sched = pd.read_csv(SCHEDULE)
    return int(len(sched[(sched["week"] == week) & (sched["game_type"] == "REG")]))


def slate_folders(year: int, week: int | None) -> list[str]:
    import glob

    week_glob = f"week_{week:02d}" if week else "week_*"
    folders = sorted(glob.glob(os.path.join(ARCHIVE, str(year), week_glob, "*")))
    return [f for f in folders if os.path.isdir(f) and os.path.basename(f) != "_processed"]


def load_manifest(folder: str) -> dict | None:
    path = os.path.join(folder, "manifest.json")
    return json.load(open(path)) if os.path.exists(path) else None
