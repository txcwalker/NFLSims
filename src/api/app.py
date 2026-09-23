"""FastAPI service bridging the React frontends and the simulation engine.

Exposes endpoints to compile team rosters/settings, apply in-memory scenario
overlays (target/carry/pressure/pace overrides with touchdown & carry
redistribution), dispatch vectorized Monte Carlo simulations via
ProcessPoolExecutor, and post-process results into DraftKings/FanDuel scoring
(averages, floors, ceilings, salaries) plus Vegas cover / over-under
frequencies.

Run via uvicorn (see start_backend_api.bat and AGENTS.md §3 for the port map).
Full design rationale (endpoints, multiprocessing notes): see app.md.
"""

import os
import re
import sys
import json
import glob
import math
import time
import zlib
import itertools
import functools
import hashlib
import threading
import numpy as np
import pandas as pd
import pulp
from typing import Dict, List, Any, Literal, Optional, Tuple, Union
from fastapi import FastAPI, HTTPException, Query, Body, BackgroundTasks
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel

# Try loading environment variables from .env
try:
    from dotenv import load_dotenv
    load_dotenv()
except ImportError:
    pass

from src.nfl_sim.batch import BatchSimulator, StatAggregator
from src.nfl_sim.scoring import calculate_fantasy_points
from src.nfl_sim.field_simulator import build_field_sample, score_field_at_iteration, load_archetype_params
from src.scrapers.dk_scraper import (
    get_dk_salaries, get_dk_contests, get_dk_contest_payout, get_dk_slates, resolve_dk_salary,
    get_dk_showdown_slates, get_dk_showdown_salaries, resolve_main_slate_draft_group_id,
    get_main_slate_pin, load_prelock_salary_snapshot,
)
from src.ownership.heuristic import _ownership_soft_cap, _compute_ownership, _compute_showdown_ownership
from src.ownership.model_inference import predict_classic_ownership, predict_showdown_ownership
from src.api import optimizer_store
from src.api import workspace_store
from src.api import paper_store
from src.api import sim_replay_store
from src.api import account_store
from src.api import cash_pool_store
from src.api.lineup_stats import (
    get_default_payout_structure as _get_default_payout_structure,
    compute_lineup_field_stats_batch as _compute_lineup_field_stats_batch,
)
from src.data_pipeline.current_week_v_0_1_0 import get_current_week
from src.data_pipeline.vegas_lines_refresh import refresh_vegas_lines

# Positional (chess-style) evaluator — lazily constructed singleton so the heavy
# WP/EP model loads + KEP curve build happen once, not per request.
_POSITIONAL_EVALUATOR = None

def get_positional_evaluator():
    """Returns the process-wide PositionalEvaluator, building it on first use."""
    global _POSITIONAL_EVALUATOR
    if _POSITIONAL_EVALUATOR is None:
        from src.nfl_sim.nfl_positional_evaluator import PositionalEvaluator
        _POSITIONAL_EVALUATOR = PositionalEvaluator()
    return _POSITIONAL_EVALUATOR

# Default impartial matchup for roster-agnostic slider evaluations. The positional
# evaluator measures situational value, not roster talent (see AGENTS.md), so the
# slider tool defaults to a fixed neutral matchup unless teams are supplied.
DEFAULT_OFF_TEAM = "KC"
DEFAULT_DEF_TEAM = "BUF"

def weighted_quantile(values, quantiles, sample_weight=None):
    values = np.array(values)
    quantiles = np.array(quantiles)
    if sample_weight is None:
        sample_weight = np.ones(len(values))
    sample_weight = np.array(sample_weight)
    
    sorter = np.argsort(values)
    values = values[sorter]
    sample_weight = sample_weight[sorter]
    
    weighted_quantiles = np.cumsum(sample_weight) - 0.5 * sample_weight
    weighted_quantiles /= np.sum(sample_weight)
    return np.interp(quantiles, weighted_quantiles, values)

app = FastAPI(title="NFLSims Week-to-Week Simulator API", version="0.2.0")

# Enable CORS for both React frontends (DFS site: 5173, strategy site: 5174)
allowed_origins = os.environ.get(
    "ALLOWED_ORIGINS",
    "http://localhost:5173,http://127.0.0.1:5173,http://localhost:5174,http://127.0.0.1:5174"
).split(",")

app.add_middleware(
    CORSMiddleware,
    allow_origins=allowed_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

@app.middleware("http")
async def no_store_cache_headers(request, call_next):
    """Every /api/* response reflects data that changes underneath it (DK
    salaries, live sim caches) on a timescale the browser's own HTTP cache
    has no way to know about. Without this, a GET to an unchanged URL (e.g.
    switching slates and back) can silently replay a stale response instead
    of re-hitting the server -- caught live during DK integration testing,
    where a browser reload kept re-displaying a salary from several backend
    restarts ago purely because the URL hadn't changed."""
    response = await call_next(request)
    response.headers["Cache-Control"] = "no-store"
    return response

# -------------------------------------------------------------------------
# PATH RESOLUTION & DATA LOADERS
# -------------------------------------------------------------------------
BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# scripts/roster_management and scripts/simulation_runners are flat modules
# (no __init__.py -- same convention those scripts already use for their own
# peer imports), added here so POST /api/dfs/roster_status below can call
# apply_team_week_overrides()/build_team_week_rows()/resim_games() in-process
# instead of shelling out -- this codebase already removed a subprocess-based
# solver from a request path because it "occasionally hung, dominating the
# endpoint" (see the CBC ILP solver notes), so a live-triggered roster
# recompute + resim follows the same in-process-import precedent instead.
for _scripts_dir in (os.path.join(BASE_DIR, "scripts", "roster_management"),
                     os.path.join(BASE_DIR, "scripts", "simulation_runners")):
    if _scripts_dir not in sys.path:
        sys.path.insert(0, _scripts_dir)
from apply_team_week_overrides_v_0_1_0 import apply_team_week_overrides  # noqa: E402
from roster_feed_v_0_1_0 import read_rows as roster_read_rows, write_rows as roster_write_rows, match_key as roster_match_key  # noqa: E402
from resim_games_2026 import resim_games  # noqa: E402
from sim_run_status import read_run_marker as read_sim_run_marker, run_marker as sim_run_marker  # noqa: E402
from build_week_overrides_v_0_1_0 import build_team_week_rows  # noqa: E402
from dfs_status_ledger import load_ledger as load_dfs_ledger, save_ledger as save_dfs_ledger, record_toggle as record_dfs_toggle  # noqa: E402

# DFS site is scoped to 2026 Weeks 1-2 for now (see /api/weeks below) -- those
# are the only weeks with real DraftKings salaries live (get_dk_salaries()) and
# the only ones currently relevant to a pre-season dev build.
SCHEDULE_CSV_PATH = os.path.join(BASE_DIR, "data", "external", "schedule_2026.csv")
TEAM_COACHES_PATH = os.path.join(BASE_DIR, "data", "dna", "team_to_coach_2026.json")
COACH_DNA_PATH = os.path.join(BASE_DIR, "data", "dna", "coach_dna.json")
GAMES_CACHE_PATH = os.path.join(BASE_DIR, "data", "interim", "sim_results_2026_games.parquet")
PLAYERS_CACHE_PATH = os.path.join(BASE_DIR, "data", "interim", "sim_results_2026_players.parquet")

# Pre-load cached simulation data into memory and reload if files are updated on disk
ALL_GAMES_CACHED = None
ALL_PLAYERS_CACHED = None
LAST_LOADED_TIME_GAMES = 0.0
LAST_LOADED_TIME_PLAYERS = 0.0
WEEK_PROJECTIONS_CACHE = {}
# cache_key -> the _dfs_week_input_mtime() token in effect when that entry was
# computed, so a stale entry (rerun sim / injury update since) is detected
# and recomputed instead of served forever for the life of the process.
WEEK_PROJECTIONS_CACHE_FRESHNESS: Dict[tuple, float] = {}
# Per-(week,year,draft_group_id) lock so concurrent /api/week_projections callers
# wait for the first computation instead of each kicking off their own optimal-
# lineup solve. Same stampede failure mode as WEEK_SIM_RESULTS_CACHE below (see
# WORKLOG 2026-09-09/2026-09-13) -- even the exact MILP solve
# (solve_optimal_lineup_milp) used for OPTIMAL_LINEUP_SAMPLE_ITERATIONS below
# costs real time per call, so a handful of concurrent cold hits (the frontend
# fires one request per draft_group_id variant on load) would otherwise each
# kick off their own redundant sample.
_WEEK_PROJECTIONS_LOCKS: Dict[tuple, "threading.Lock"] = {}
_week_projections_locks_guard = threading.Lock()
# Live-request fallback sample size for optimal_pct, used only when
# scripts/simulation_runners/compute_optimal_pct_2026.py hasn't been run yet
# for this week (that script solves every iteration offline instead of
# sampling -- see its use below). 50 keeps a cold solve to a few seconds with
# the exact MILP solver.
OPTIMAL_LINEUP_SAMPLE_ITERATIONS = 50

# game_id -> sub-dataframe lookups, rebuilt whenever the parquet caches reload.
# run_simulation()'s cache-hit path used to filter the full ~11M-row players
# cache with a linear `== game_id` scan on every single call (~1s each, the
# dominant non-optimizer cost per /api/week_sim_results profiling) — these
# dicts turn that into an O(1) lookup instead.
GAMES_BY_GAME_ID = {}
PLAYERS_BY_GAME_ID = {}

# In-memory + disk cache for /api/week_sim_results — see that endpoint for details.
WEEK_SIM_RESULTS_CACHE = {}
# Same staleness guard as WEEK_PROJECTIONS_CACHE_FRESHNESS above.
WEEK_SIM_RESULTS_CACHE_FRESHNESS: Dict[tuple, float] = {}
# Per-(week,year) lock so concurrent /api/week_sim_results callers wait for the
# first computation instead of each kicking off their own full 16-game re-sim.
# This is a genuinely expensive endpoint (minutes when starters don't match the
# season parquet) and the frontend + its retries hammer it on every page load;
# without this, a handful of concurrent cold hits saturate every CPU and none
# ever finish. See WORKLOG 2026-09-09.
_WEEK_SIM_LOCKS: Dict[tuple, "threading.Lock"] = {}
_week_sim_locks_guard = threading.Lock()

# DFS-specific per-week simulation cache (2026-09-04) — separate from
# ALL_GAMES_CACHED/ALL_PLAYERS_CACHED, which hold the SEASON-LONG sim (every
# player at their healthy, "if everyone's available" usage). A week's real
# availability (IR/PUP/suspensions/etc., see data/overrides/2026/week_NN/)
# is compiled into its own roster tree (data/current_rosters/dfs/) by
# apply_team_week_overrides_v_0_1_0.py and simulated by
# scripts/simulation_runners/run_week_sim_2026.py into
# data/interim/dfs_week_{N}_players.parquet — get_week_projections() below
# prefers that file when it exists, so DFS projections reflect the week's
# actual injury news instead of a slice of the season-long average.
DFS_WEEK_PLAYERS_CACHE: Dict[int, Any] = {}     # week -> (mtime, DataFrame)
# week -> (players_mtime, games_mtime, {game_id: (games_df_slice, players_df_slice)})
# Lets run_simulation() serve a whole week straight from the DFS-week parquet
# (which was simulated from data/current_rosters/dfs/, so its starters match
# the week's real availability by construction) instead of live-re-simming
# every game whose starters differ from the season-long "everyone healthy"
# parquet. See _compute_week_sim_results / SimulationRequest.use_dfs_week.
DFS_WEEK_BY_GAME_ID: Dict[int, Tuple[float, float, Dict[str, Tuple[Any, Any]]]] = {}


def _get_dfs_week_players(week: int):
    """Loads (and mtime-based-reloads) data/interim/dfs_week_{week}_players.parquet
    if it exists. Returns None if there is no DFS-specific sim for this week
    yet — callers should fall back to slicing the season-long cache."""
    path = os.path.join(BASE_DIR, "data", "interim", f"dfs_week_{week}_players.parquet")
    if not os.path.exists(path):
        return None
    mtime = os.path.getmtime(path)
    cached = DFS_WEEK_PLAYERS_CACHE.get(week)
    if cached is not None and cached[0] == mtime:
        return cached[1]
    df = pd.read_parquet(path)
    DFS_WEEK_PLAYERS_CACHE[week] = (mtime, df)
    return df


def _build_week_trial_scores(week: Optional[int], teams: set) -> Tuple[Dict[tuple, np.ndarray], int, str]:
    """Per-player, iteration-aligned dk_score arrays for ONE week's slate --
    the "real sim draws" the classic optimizer grades lineups (and the field)
    against.

    Inputs:
        week  (int|None) -- the slate's week (OptimizeRequest.week).
        teams (set[str]) -- teams present in the request's player pool; rows
              for other teams are skipped.
    Outputs (to /api/optimize's grading block):
        trial_map  {(name, team, pos): np.ndarray[n_iter]} -- arr[k] is that
                   player's dk_score in slate-wide sim iteration k. DST rows are
                   also keyed as ("{team} DST", team, "DST") and
                   ("Defense", team, "DST"), matching the frontend's payloads.
        n_iter     (int) -- iterations in the source (10,000 or 1,000 today;
                   read from the data, never assumed).
        source     (str) -- 'dfs_week_{N}' | 'season_parquet_week_{N}' | 'none'.

    Purpose: replaces an older inline build that grouped the SEASON-LONG
    parquet by (Player, Team, Pos) with no week filter -- concatenating a
    player's 17 games x 1000 iterations and then indexing 0-999 into that
    17,000-row array, so "iteration k" was really a mix of early iterations
    against different opponents (found 2026-09-23). Preference order mirrors
    get_week_projections(): the week's DFS-specific sim (real injury news)
    first, else the season-long parquet restricted to THIS week's game_ids,
    else nothing (callers fall back to synthetic draws rather than
    wrong-week ones).

    Tricky bit: arrays are filled by the iteration column itself
    (arr[iterations] = scores), not by sorted row order, so array index ==
    iteration id even if a file ever skipped or reordered iterations -- that
    is what lets a Game Distribution box-select (which sends raw iteration
    ids) index straight into these arrays.
    """
    if week is None:
        return {}, 0, 'none'
    df = _get_dfs_week_players(week)
    source = f'dfs_week_{week}'
    if df is None or df.empty:
        source = f'season_parquet_week_{week}'
        df = None
        if ALL_PLAYERS_CACHED is not None and os.path.exists(SCHEDULE_CSV_PATH):
            sched = pd.read_csv(SCHEDULE_CSV_PATH)
            week_ids = sched[(sched["week"] == week) & (sched["game_type"] == "REG")]["game_id"].unique()
            df = ALL_PLAYERS_CACHED[ALL_PLAYERS_CACHED["game_id"].isin(week_ids)]
    if df is None or df.empty:
        return {}, 0, 'none'

    df = df[df["Team"].isin(teams)]
    if df.empty:
        return {}, 0, 'none'
    n_iter = int(df["iteration"].max()) + 1
    trial_map: Dict[tuple, np.ndarray] = {}
    for (player_name, team_name, pos_name), group in df.groupby(["Player", "Team", "Pos"]):
        arr = np.zeros(n_iter, dtype=float)
        arr[group["iteration"].values.astype(int)] = group["dk_score"].values
        if pos_name == "DST":
            trial_map[(f"{team_name} DST", team_name, pos_name)] = arr
            trial_map[("Defense", team_name, pos_name)] = arr
        else:
            trial_map[(player_name, team_name, pos_name)] = arr
    return trial_map, n_iter, source


def _dfs_week_input_mtime(week: int, year: int) -> float:
    """Max mtime across this week's DFS-specific sim parquet + every current
    roster file, for staleness-checking an IN-MEMORY response cache (as
    opposed to _get_dfs_week_players' own mtime check, which only protects
    its own DataFrame cache -- a downstream response cache keyed by
    (week, year, ...) that never re-consults that DataFrame after its first
    populate would otherwise keep serving pre-rerun numbers, e.g. an injury
    update, for the life of the server process. Same signal
    _compute_week_sim_results' disk-JSON path already checks; this extends
    the same check to the in-memory cache-hit path, which previously skipped
    it entirely (found 2026-09-16 -- a rerun after marking players out
    correctly regenerated the parquet but the site kept serving the old
    numbers until the process was restarted)."""
    paths = [
        os.path.join(BASE_DIR, "data", "interim", f"dfs_week_{week}_players.parquet"),
        os.path.join(BASE_DIR, "data", "interim", f"dfs_week_{week}_games.parquet"),
        # compute_optimal_pct_2026.py's output -- feeds optimal_pct into this
        # same response (see get_week_projections), so a batch rerun of that
        # script must invalidate this cache too, not just the sim parquets.
        os.path.join(BASE_DIR, "data", "interim", f"week_{week}_optimal_pct.json"),
    ]
    roster_glob = os.path.join(BASE_DIR, "data", "current_rosters", "**", f"*_traits_{year}.json")
    mtimes = [os.path.getmtime(p) for p in paths if os.path.exists(p)]
    mtimes += [os.path.getmtime(p) for p in glob.glob(roster_glob, recursive=True)]
    return max(mtimes, default=0.0)


def _get_dfs_week_by_game_id(week: int) -> Optional[Dict[str, Tuple[Any, Any]]]:
    """{game_id: (games_df_slice, players_df_slice)} for a week's DFS-specific
    sim, or None if either parquet is missing. mtime-reloaded like the others.
    Same column schema as GAMES_BY_GAME_ID / PLAYERS_BY_GAME_ID, so a slice
    drops straight into run_simulation()'s cache-hit path."""
    p_path = os.path.join(BASE_DIR, "data", "interim", f"dfs_week_{week}_players.parquet")
    g_path = os.path.join(BASE_DIR, "data", "interim", f"dfs_week_{week}_games.parquet")
    if not (os.path.exists(p_path) and os.path.exists(g_path)):
        return None
    p_mtime, g_mtime = os.path.getmtime(p_path), os.path.getmtime(g_path)
    cached = DFS_WEEK_BY_GAME_ID.get(week)
    if cached is not None and cached[0] == p_mtime and cached[1] == g_mtime:
        return cached[2]
    players = pd.read_parquet(p_path)
    games = pd.read_parquet(g_path)
    g_by_id = {gid: df for gid, df in games.groupby("game_id")}
    p_by_id = {gid: df for gid, df in players.groupby("game_id")}
    by_game = {gid: (g_by_id[gid], p_by_id.get(gid, pd.DataFrame())) for gid in g_by_id}
    DFS_WEEK_BY_GAME_ID[week] = (p_mtime, g_mtime, by_game)
    return by_game

# In-memory cache of built field samples (see src/nfl_sim/field_simulator.py),
# keyed by (week, draft_group_id). Built once inside get_week_sim_results()
# (expensive -- K archetype-weighted lineups), then reused by both
# /api/field_sample (inspection) and /api/optimize (scoring against it) so
# repeated small optimizer edits don't pay the field-construction cost again
# -- see the implementation plan's "build/score cache split".
FIELD_SAMPLE_CACHE: Dict[Tuple[int, Optional[int]], Dict[str, Any]] = {}

# Synthetic field size for both optimizers' EV/ITM/Top% math -- as large as
# benchmarked-sustainable, then percentiles against it get extrapolated to
# the real `total_entries` (Cam: "up the sample... until it is unsustainable,
# then base off of percentile" -- comparing against an actual 50k+-entry
# field isn't tractable per-request, but the ranking math only needs the
# field's *distribution* to be well-estimated, not its literal size).
# Benchmarked on this machine: field CONSTRUCTION (one-time per week, then
# cached in FIELD_SAMPLE_CACHE) scales ~2.6ms/lineup for the archetype
# builder (K=5,000 -> ~13s once); RANKING (via
# _compute_lineup_field_stats_batch's double-argsort, repeats every
# optimize call across all generated lineups at once) took ~6s for 150
# lineups at K=5,000, ~12.5s at K=10,000, and catastrophically degraded
# (~6.5 MIN) at K=20,000 -- almost certainly a memory-pressure cliff from
# the combined (n_field + n_lineups, n_sims) sort buffer, not a clean
# asymptotic curve. 5,000 is chosen as comfortably on the sustainable side
# of that cliff while still being 5x the old K=1,000's tail resolution.
FIELD_SAMPLE_K = 5000

# Full-response cache for /api/simulate, keyed on the exact request parameters
# (see build_simulate_cache_key). The expensive part of this endpoint isn't the
# Monte Carlo simulation itself (that's already served from ALL_GAMES_CACHED/
# ALL_PLAYERS_CACHED when available) — it's the per-iteration DFS optimal-lineup
# solve that runs unconditionally afterward, once per simulated iteration
# (up to 10,000 times). That reran identically every time a user just switched
# back to a game with unchanged settings. This cache makes repeat requests with
# identical inputs instant; any actual change to inputs produces a new key and
# computes fresh. Cleared whenever the underlying simulation cache reloads.
SIMULATE_RESPONSE_CACHE = {}


def build_simulate_cache_key(req: "SimulationRequest") -> str:
    """Canonical, order-independent key for the full simulate request — used to
    cache the complete computed response (see SIMULATE_RESPONSE_CACHE)."""
    team_overrides_sorted = {
        team: req.team_overrides[team].dict() for team in sorted(req.team_overrides.keys())
    }
    player_overrides_sorted = sorted(
        (po.dict() for po in req.player_overrides),
        key=lambda d: (d.get("team", ""), d.get("name", ""))
    )
    payload = {
        "away_team": req.away_team,
        "home_team": req.home_team,
        "year": req.year,
        "iterations": req.iterations,
        "spread_override": req.spread_override,
        "total_override": req.total_override,
        "apply_weighting": req.apply_weighting,
        "team_overrides": team_overrides_sorted,
        "player_overrides": player_overrides_sorted,
        "use_dfs_week": req.use_dfs_week,
    }
    return json.dumps(payload, sort_keys=True, default=str)

def reload_cache_if_changed():
    global ALL_GAMES_CACHED, ALL_PLAYERS_CACHED, LAST_LOADED_TIME_GAMES, LAST_LOADED_TIME_PLAYERS, WEEK_PROJECTIONS_CACHE, WEEK_SIM_RESULTS_CACHE, GAMES_BY_GAME_ID, PLAYERS_BY_GAME_ID
    reloaded = False
    
    if os.path.exists(GAMES_CACHE_PATH):
        mtime = os.path.getmtime(GAMES_CACHE_PATH)
        if ALL_GAMES_CACHED is None or mtime > LAST_LOADED_TIME_GAMES:
            try:
                print(f"Reloading games cache from disk (mtime={mtime})...")
                ALL_GAMES_CACHED = pd.read_parquet(GAMES_CACHE_PATH)
                LAST_LOADED_TIME_GAMES = mtime
                reloaded = True
            except Exception as e:
                print(f"Error reloading games cache: {e}")
                
    if os.path.exists(PLAYERS_CACHE_PATH):
        mtime = os.path.getmtime(PLAYERS_CACHE_PATH)
        if ALL_PLAYERS_CACHED is None or mtime > LAST_LOADED_TIME_PLAYERS:
            try:
                print(f"Reloading players cache from disk (mtime={mtime})...")
                ALL_PLAYERS_CACHED = pd.read_parquet(PLAYERS_CACHE_PATH)
                LAST_LOADED_TIME_PLAYERS = mtime
                reloaded = True
            except Exception as e:
                print(f"Error reloading players cache: {e}")

    if reloaded:
        WEEK_PROJECTIONS_CACHE.clear()
        WEEK_PROJECTIONS_CACHE_FRESHNESS.clear()
        SIMULATE_RESPONSE_CACHE.clear()
        WEEK_SIM_RESULTS_CACHE.clear()
        WEEK_SIM_RESULTS_CACHE_FRESHNESS.clear()
        if ALL_GAMES_CACHED is not None:
            GAMES_BY_GAME_ID = {gid: df for gid, df in ALL_GAMES_CACHED.groupby("game_id")}
        if ALL_PLAYERS_CACHED is not None:
            PLAYERS_BY_GAME_ID = {gid: df for gid, df in ALL_PLAYERS_CACHED.groupby("game_id")}
        print("Cleared week projections, simulate response, and week sim results caches due to data reload.")

# Initial load on startup
reload_cache_if_changed()

# -------------------------------------------------------------------------
# 2026 SEASON REPORTS (frontend_analysis Season2026 page)
# -------------------------------------------------------------------------
# Read-only pass-through of the report files scripts/simulation_runners/
# generate_*_2026.py and run_full_season_sim_2026.py already produce --
# no new computation here, same mtime-gated reload pattern as
# reload_cache_if_changed() above, kept in its own cache/globals so a 2025
# cache reload never has to know about 2026 report files or vice versa.
SEASON2026_DIR = os.path.join(BASE_DIR, "docs", "reports")
SEASON2026_PATHS = {
    "standings": os.path.join(SEASON2026_DIR, "season_summaries_2026.csv"),
    "team_stats": os.path.join(SEASON2026_DIR, "team_stats_2026.csv"),
    "leaders": os.path.join(SEASON2026_DIR, "season_leaders_2026.json"),
    "matchups": os.path.join(SEASON2026_DIR, "matchup_win_probabilities_2026.json"),
    "teams": os.path.join(SEASON2026_DIR, "2026", "teams_data.json"),
}
SEASON2026_CACHE: Dict[str, Any] = {}
SEASON2026_MTIMES: Dict[str, float] = {}


def reload_season2026_cache_if_changed():
    for key, path in SEASON2026_PATHS.items():
        if not os.path.exists(path):
            continue
        mtime = os.path.getmtime(path)
        if key in SEASON2026_MTIMES and mtime <= SEASON2026_MTIMES[key]:
            continue
        try:
            if path.endswith(".csv"):
                SEASON2026_CACHE[key] = pd.read_csv(path).to_dict("records")
            else:
                with open(path, "r", encoding="utf-8") as f:
                    SEASON2026_CACHE[key] = json.load(f)
            SEASON2026_MTIMES[key] = mtime
            print(f"Loaded 2026 season report '{key}' from {path} (mtime={mtime})")
        except Exception as e:
            print(f"Error loading 2026 season report '{key}': {e}")


reload_season2026_cache_if_changed()


@app.get("/api/season2026/standings")
def get_season2026_standings():
    reload_season2026_cache_if_changed()
    if "standings" not in SEASON2026_CACHE:
        raise HTTPException(status_code=404, detail="season_summaries_2026.csv not found -- run run_full_season_sim_2026.py first.")
    return SEASON2026_CACHE["standings"]


@app.get("/api/season2026/team-stats")
def get_season2026_team_stats():
    reload_season2026_cache_if_changed()
    if "team_stats" not in SEASON2026_CACHE:
        raise HTTPException(status_code=404, detail="team_stats_2026.csv not found -- run generate_team_stats_2026.py first.")
    return SEASON2026_CACHE["team_stats"]


@app.get("/api/season2026/leaders")
def get_season2026_leaders():
    reload_season2026_cache_if_changed()
    if "leaders" not in SEASON2026_CACHE:
        raise HTTPException(status_code=404, detail="season_leaders_2026.json not found -- run generate_season_leaders_2026.py first.")
    return SEASON2026_CACHE["leaders"]


@app.get("/api/season2026/matchups")
def get_season2026_matchups():
    reload_season2026_cache_if_changed()
    if "matchups" not in SEASON2026_CACHE:
        raise HTTPException(status_code=404, detail="matchup_win_probabilities_2026.json not found -- run generate_matchup_win_probabilities_2026.py first.")
    return SEASON2026_CACHE["matchups"]


@app.get("/api/season2026/teams")
def get_season2026_teams():
    reload_season2026_cache_if_changed()
    if "teams" not in SEASON2026_CACHE:
        raise HTTPException(status_code=404, detail="teams_data.json not found -- run run_full_season_sim_2026.py first.")
    return SEASON2026_CACHE["teams"]


# -------------------------------------------------------------------------
# 2026 CURRENT SEASON (frontend_analysis "Current Season" page) -- real,
# actual stats to date, as opposed to SEASON2026_* above (fully hypothetical
# / additive-projected "Rest of Season" reports). Same read-only pass-
# through + mtime-gated reload pattern, pointed at the report files
# scripts/simulation_runners/build_actual_season_stats_2026.py writes.
# -------------------------------------------------------------------------
SEASON2026_CURRENT_DIR = os.path.join(BASE_DIR, "docs", "reports", "season_actuals_2026")
SEASON2026_CURRENT_PATHS = {
    "standings": os.path.join(SEASON2026_CURRENT_DIR, "standings.csv"),
    "team_stats": os.path.join(SEASON2026_CURRENT_DIR, "team_stats.csv"),
    "leaders": os.path.join(SEASON2026_CURRENT_DIR, "leaders.json"),
    "teams": os.path.join(SEASON2026_CURRENT_DIR, "teams_data.json"),
}
SEASON2026_CURRENT_CACHE: Dict[str, Any] = {}
SEASON2026_CURRENT_MTIMES: Dict[str, float] = {}


def reload_season2026_current_cache_if_changed():
    for key, path in SEASON2026_CURRENT_PATHS.items():
        if not os.path.exists(path):
            continue
        mtime = os.path.getmtime(path)
        if key in SEASON2026_CURRENT_MTIMES and mtime <= SEASON2026_CURRENT_MTIMES[key]:
            continue
        try:
            if path.endswith(".csv"):
                SEASON2026_CURRENT_CACHE[key] = pd.read_csv(path).to_dict("records")
            else:
                with open(path, "r", encoding="utf-8") as f:
                    SEASON2026_CURRENT_CACHE[key] = json.load(f)
            SEASON2026_CURRENT_MTIMES[key] = mtime
            print(f"Loaded Current Season report '{key}' from {path} (mtime={mtime})")
        except Exception as e:
            print(f"Error loading Current Season report '{key}': {e}")


reload_season2026_current_cache_if_changed()


@app.get("/api/season2026/current/standings")
def get_season2026_current_standings():
    reload_season2026_current_cache_if_changed()
    if "standings" not in SEASON2026_CURRENT_CACHE:
        raise HTTPException(status_code=404, detail="standings.csv not found -- run build_actual_season_stats_2026.py first.")
    return SEASON2026_CURRENT_CACHE["standings"]


@app.get("/api/season2026/current/team-stats")
def get_season2026_current_team_stats():
    reload_season2026_current_cache_if_changed()
    if "team_stats" not in SEASON2026_CURRENT_CACHE:
        raise HTTPException(status_code=404, detail="team_stats.csv not found -- run build_actual_season_stats_2026.py first.")
    return SEASON2026_CURRENT_CACHE["team_stats"]


@app.get("/api/season2026/current/leaders")
def get_season2026_current_leaders():
    reload_season2026_current_cache_if_changed()
    if "leaders" not in SEASON2026_CURRENT_CACHE:
        raise HTTPException(status_code=404, detail="leaders.json not found -- run build_actual_season_stats_2026.py first.")
    return SEASON2026_CURRENT_CACHE["leaders"]


@app.get("/api/season2026/current/teams")
def get_season2026_current_teams():
    reload_season2026_current_cache_if_changed()
    if "teams" not in SEASON2026_CURRENT_CACHE:
        raise HTTPException(status_code=404, detail="teams_data.json not found -- run build_actual_season_stats_2026.py first.")
    return SEASON2026_CURRENT_CACHE["teams"]


@app.get("/api/season2026/current/matchups")
def get_season2026_current_matchups():
    """Current week's matchup(s), simmed via the same per-week DFS roster
    tree the DFS site uses (run_week_sim_2026.py's dfs_week_{week}_games
    parquet) -- NOT the season-long file, since the current week's real
    injury/availability news isn't reflected in the season-long "everyone
    healthy" cache the way it is in the DFS-specific weekly sim."""
    week = get_current_week(2026)
    games_path = os.path.join(BASE_DIR, "data", "interim", f"dfs_week_{week}_games.parquet")
    if not os.path.exists(games_path):
        raise HTTPException(
            status_code=404,
            detail=(f"No DFS week sim found for week {week} -- run "
                    f"apply_team_week_overrides_v_0_1_0.py {week} then run_week_sim_2026.py {week} first."),
        )
    games_df = pd.read_parquet(games_path)
    sched = pd.read_csv(SCHEDULE_CSV_PATH)
    sched = sched[sched["game_type"] == "REG"][["game_id", "week", "spread_line", "total_line", "away_moneyline", "home_moneyline"]]

    win_pct = games_df.groupby("game_id").apply(
        lambda g: pd.Series({
            "away_team": g["away_team"].iloc[0],
            "home_team": g["home_team"].iloc[0],
            "away_win_pct": (g["away_score"] > g["home_score"]).mean() * 100,
            "home_win_pct": (g["home_score"] > g["away_score"]).mean() * 100,
            "tie_pct": (g["away_score"] == g["home_score"]).mean() * 100,
            "avg_away_score": g["away_score"].mean(),
            "avg_home_score": g["home_score"].mean(),
        }),
        include_groups=False,
    ).reset_index()

    merged = win_pct.merge(sched, on="game_id", how="left").round(2)
    for col in ("away_moneyline", "home_moneyline"):
        merged[col] = merged[col].astype("Int64")
    # NaN (e.g. no posted line yet) isn't valid JSON -- convert to null.
    merged = merged.astype(object).where(merged.notna(), None)
    return {"weeks": {str(week): merged.to_dict("records")}}


@functools.lru_cache(maxsize=None)
def load_json(path: str) -> Dict[str, Any]:
    """Memoized — these DNA/roster JSON files are static for a server's
    lifetime (a restart is already required to pick up data changes, same as
    BatchSimulator's own per-instance _json_cache). get_rosters() was calling
    this ~7 times per game with zero caching (~2.3s/call from re-parsing the
    same league-wide DNA files from disk), which dominated /api/week_sim_results'
    per-game cost. Callers must not mutate the returned dict in place — the
    same cached object is returned on every call (see get_rosters()'s
    dict(...) copy before deleting backup-QB keys, for example)."""
    if os.path.exists(path):
        with open(path, "r") as f:
            return json.load(f)
    return {}

# -------------------------------------------------------------------------
# PYDANTIC SCHEMAS
# -------------------------------------------------------------------------
class TeamOverride(BaseModel):
    plays_per_game: float
    def_pressure_rate: float
    proe: float

class PlayerOverride(BaseModel):
    name: str
    team: str
    target_share: float  # Percentage (e.g. 20.5)
    carry_share: float   # Percentage (e.g. 45.0)
    catch_rate: float    # Percentage (e.g. 72.0)
    rush_td_share: Optional[float] = 0.0
    rec_td_share: Optional[float] = 0.0
    ownership_proj: Optional[float] = None  # None = no override; caller (e.g. _compute_one_game) fills it in
    pos: Optional[str] = "WR/TE"
    salary: Optional[int] = 3000


class SimulationRequest(BaseModel):
    away_team: str
    home_team: str
    year: int = 2026
    iterations: int = 10000
    spread_override: Optional[float] = None
    total_override: Optional[float] = None
    apply_weighting: Optional[bool] = True
    team_overrides: Dict[str, TeamOverride]  # Keyed by team name
    player_overrides: List[PlayerOverride]
    # Set by /api/week_sim_results: the caller built team_overrides/player_overrides
    # from get_rosters()'s *rounded* percentages, which will almost never exactly
    # equal the unrounded internal roster values within the override-detection
    # tolerance below — so trust the caller instead of float-comparing and let the
    # request take the fast cache-hit path unconditionally.
    use_cached_defaults: Optional[bool] = False
    # How many iterations the DFS optimal-lineup branch-and-bound solve
    # samples from (see the "solve_iterations" subsampling below). Defaults to
    # 1,000 for direct user-facing "Run Engine" requests (Cam's call,
    # 2026-09-17: fast enough for an interactive single-game rerun — ~20-25s
    # on a real Showdown-sized pool, ~15-25ms/iteration on real, naturally
    # skewed DK scores — while still high enough to capture a slate's real
    # variance, unlike the old default of 50). /api/week_sim_results sets
    # this to the FULL 1,000 iterations available (no sub-sampling at all)
    # since it's a once-per-week batch precompute, not a live request — a
    # few extra minutes there is fine per Cam, in exchange for the optimal-
    # lineup rates baked into the weekly sim being exact rather than sampled.
    optimizer_sample_cap: Optional[int] = 1000
    # When set, serve this game straight from data/interim/dfs_week_{N}_*.parquet
    # (simulated from data/current_rosters/dfs/, so its starters already reflect
    # the week's real availability) instead of the season-long parquet + a
    # starter-mismatch bypass that would otherwise live-re-sim. Set by
    # _compute_week_sim_results; ignored if the DFS-week parquet is absent.
    use_dfs_week: Optional[int] = None

class OptimizerPlayer(BaseModel):
    name: str
    team: str
    pos: str  # QB, RB, WR, TE, DST
    salary: int
    projection: float          # Always P50/median — used for simulation & display
    gpp_projection: Optional[float] = None  # Blended ceiling value — used only by ILP objective
    locked: bool = False
    excluded: bool = False
    ownership_pct: Optional[float] = None
    dk_pcts_all: Optional[List[float]] = None  # 101-element array [p0..p100]
    dk_id: Optional[int] = None  # DK's per-slate draftableId, for a real lineup-upload CSV export
    dk_name: Optional[str] = None  # DST only: DK's real display name (e.g. "Buccaneers") for a lineup-upload CSV -- `name` stays the generic "Defense" the sim engine keys on (see get_week_dk_names())

class PayoutTier(BaseModel):
    rank_start: int
    rank_end: int
    payout: float

class ManualLineup(BaseModel):
    """A hand-built classic lineup to score instead of solving for one --
    the classic-optimizer analogue of ManualShowdownLineup. Names are matched
    against the pool case/punctuation-insensitively; append '|TEAM' to
    disambiguate a shared name. Slots are the player's own declared role
    (not re-derived), so a RB you put at FLEX is scored/labelled as FLEX."""
    qb: str
    rb: List[str]    # exactly 2
    wr: List[str]    # exactly 3
    te: str
    flex: str        # RB, WR, or TE
    dst: str
    label: Optional[str] = None


class OptimizeRequest(BaseModel):
    players: List[OptimizerPlayer]
    n_lineups: int = 20
    salary_cap: int = 50000
    contest_type: str = 'top_heavy'  # cash, flat, top_heavy, extreme_top_heavy
    contest_size: int = 11000
    min_unique_players: int = 2
    include_dst_in_unique: bool = False
    max_exposure: float = 0.40
    payout_structure: Optional[List[PayoutTier]] = None
    entry_fee: float = 18.0
    total_entries: int = 11000
    paying_positions: int = 2200
    # Max entries per user for the target contest ("Nmax") -- not sourced
    # from anywhere in the frontend yet (no UI field sets it today), so this
    # stays None in practice until that's built. Accepted here so the
    # ownership model's segmentation features (2026-09-22) can use it the
    # moment a caller does supply it, without another API contract change.
    max_entries: Optional[float] = None
    week: Optional[int] = None  # looks up the cached archetype field sample (FIELD_SAMPLE_CACHE) built by get_week_sim_results(); falls back to a uniform-random field if omitted or not yet built
    # When set, skip lineup generation entirely and just run these hand-built
    # lineups through the same field sim + EV/ITM/Top%/portfolio scoring --
    # the classic-optimizer "Lineup Lab" (see ManualShowdownLineup for the
    # showdown version, added first).
    manual_lineups: Optional[List[ManualLineup]] = None
    # Conditional re-scoring: the raw 0-999 iteration ids a GameDistribution
    # box-select on ONE game resolved to (see game_distribution.raw.iteration
    # from GET /api/game_distribution). Every game on the slate shares one
    # full-slate iteration index, so this conditions the WHOLE field + our
    # lineups on that one game landing in the selected range. Same mechanism
    # as ShowdownOptimizeRequest.iteration_filter.
    iteration_filter: Optional[List[int]] = None
    # Approximate target mix for which position fills FLEX across the
    # generated set, e.g. {"RB": 30, "WR": 40, "TE": 10, "ANY": 20} -- raw
    # slider values, any non-negative scale, normalized into weights server-
    # side (see optimize_lineups). "ANY" leaves that share of lineups fully
    # unconstrained (today's default: solver picks FLEX purely on score).
    # None/empty/all-zero -> unconstrained for every lineup, unchanged from
    # before this existed.
    flex_position_weights: Optional[Dict[str, float]] = None
    # Per-position exposure caps, e.g. {"QB": 0.5, "RB": 0.4, "WR": 0.3, "TE": 0.4, "DST": 0.6}
    # (fractions 0-1). Takes over from the single `max_exposure` scalar above
    # when provided (classic optimizer's Settings sliders always send this);
    # `max_exposure` stays as the fallback for any caller that doesn't (and
    # as the value used when this is omitted entirely).
    max_exposure_by_pos: Optional[Dict[str, float]] = None
    # Where each lineup's ILP score draw comes from (2026-09-23):
    #   'sim'      -- one REAL slate-wide sim iteration per lineup: every
    #                 player's deviation from his own sim mean in that same
    #                 iteration, so booms, duds and stack correlation come
    #                 straight from the game engine.
    #   'gaussian' -- the original correlated-normal draw (IQR-sized sigma,
    #                 hand-set _build_correlation_matrix). DEFAULT: the
    #                 2026-09-23 week-3 A/B (scripts/eda/compare_optimizer_
    #                 draw_sources.py) had 'sim' at ~100-106% portfolio EV vs.
    #                 ~177-187% for 'gaussian', well outside rep-to-rep noise.
    # 'sim' silently degrades to 'gaussian' when the week has no sim data;
    # the response's portfolio.draw_source says which one actually ran.
    draw_source: Literal['sim', 'gaussian'] = 'gaussian'

# -------------------------------------------------------------------------
# ENDPOINTS
# -------------------------------------------------------------------------

@app.get("/")
def read_root():
    """Returns API status, version, and link to docs and health check."""
    return {
        "status": "online",
        "service": "NFLSims Week-to-Week Simulator API",
        "version": "0.2.0",
        "documentation": {
            "swagger_ui": "/docs",
            "redoc": "/redoc"
        },
        "diagnostics": {
            "health_check": "/health"
        }
    }

@app.get("/health")
@app.get("/api/health")
def health_check():
    """Verifies that parquet data and schedule files are loaded in RAM and disk."""
    db_loaded = (ALL_GAMES_CACHED is not None) and (ALL_PLAYERS_CACHED is not None)
    return {
        "status": "healthy" if db_loaded else "degraded",
        "cache_loaded": db_loaded,
        "schedule_exists": os.path.exists(SCHEDULE_CSV_PATH),
        "games_parquet_exists": os.path.exists(GAMES_CACHE_PATH),
        "players_parquet_exists": os.path.exists(PLAYERS_CACHE_PATH)
    }

@app.get("/api/weeks")
def get_weeks():
    """Returns available weeks -- hardcoded to Weeks 1-3. The DFS dev site is
    scoped to weeks with real DraftKings salaries live/snapshotted (see
    SCHEDULE_CSV_PATH above); week 3's main slate + Vegas are live as of
    2026-09-22 (sim projections/ownership not run yet -- that's separate).
    Revisit/extend each week as it goes live."""
    return {"weeks": [1, 2, 3]}

@app.get("/api/dk/slates")
def get_dk_slates_endpoint(week: Optional[int] = None, year: int = 2026, force_refresh: bool = False):
    """Every DraftKings slate currently live (Main Slate always first when
    present; Showdown/Snake/split-Sunday slates appear alongside it as DK
    adds them) -- see dk_scraper.get_dk_slates(). Powers a slate picker so
    the user can choose which draft group's salaries/contests to work with
    instead of being locked to whichever slate the auto-detect heuristic
    picks.

    When `week` is given, `default_draft_group_id` (and the matching entry's
    `is_default`/label in `slates`) is overridden with the sticky per-week
    main-slate pin (see dk_scraper.resolve_main_slate_draft_group_id)
    instead of DK's raw live "most open contests" pick, which flips to a
    small leftover slate once the real main slate's contests lock. Omit
    `week` to get the unpinned live view (e.g. a slate picker not yet tied
    to a specific week)."""
    result = get_dk_slates(force_refresh=force_refresh)
    if week is None:
        return result
    pinned_dg = resolve_main_slate_draft_group_id(year, week, force_refresh=force_refresh)
    if pinned_dg is None:
        return result

    result = dict(result)
    result["default_draft_group_id"] = pinned_dg
    slates = result.get("slates", [])
    found_pinned = any(s["draft_group_id"] == pinned_dg for s in slates)
    new_slates = []
    for s in slates:
        is_pinned = s["draft_group_id"] == pinned_dg
        if s.get("is_default") and not is_pinned:
            # This slate lost the pin (its contests locked and dropped out of
            # the live lobby's count, or a bigger classic slate now exists) --
            # it's still real and pickable, just no longer "the" main slate,
            # so it needs a label that isn't also "Main Slate (Classic)".
            s = {**s, "label": f"{s.get('game_type') or 'Classic'} (locked/secondary)"}
        new_slates.append({**s, "is_default": is_pinned})
    if not found_pinned:
        # The pinned draft group has no currently-open contests at all, so it
        # dropped out of the live lobby fetch entirely -- synthesize an entry
        # from the pin record so it still shows up as selectable/default.
        pin_record = get_main_slate_pin(year, week) or {}
        # A manual/backfilled pin (see dk_scraper.pin_main_slate_draft_group_id)
        # stores an effectively-infinite contest_count so it can't be outvoted
        # -- real, but meaningless to a human, so don't surface it verbatim.
        raw_count = pin_record.get("contest_count", 0)
        new_slates.insert(0, {
            "draft_group_id": pinned_dg,
            "game_type": "Classic",
            "label": "Main Slate (Classic)",
            "contest_count": raw_count if raw_count < 10**6 else None,
            "total_entries": None,
            "is_default": True,
        })
    result["slates"] = new_slates
    return result

@app.get("/api/dk/salaries")
def get_dk_salaries_endpoint(draft_group_id: Optional[int] = None, force_refresh: bool = False):
    """Debug/visibility endpoint for the live DraftKings salary feed (see
    src/scrapers/dk_scraper.py). Returns metadata plus counts rather than the
    full player dict, which isn't JSON-tuple-key-friendly."""
    dk = get_dk_salaries(draft_group_id=draft_group_id, force_refresh=force_refresh)
    return {
        "is_live": dk["is_live"],
        "draft_group_id": dk["draft_group_id"],
        "fetched_at": dk["fetched_at"],
        "player_count": len(dk["players"]),
        "defense_count": len(dk["defense"]),
        "main_slate_teams": sorted(dk["main_slate_teams"]),
    }

@app.get("/api/dk/showdown_slates")
def get_dk_showdown_slates_endpoint(force_refresh: bool = False):
    """Every live DK Showdown Captain Mode slate with its two teams resolved
    to internal abbreviations (see dk_scraper.get_dk_showdown_slates()).
    Powers the Showdown optimizer's "which DK slate is this game" lookup."""
    return get_dk_showdown_slates(force_refresh=force_refresh)


@app.get("/api/dk/showdown_salaries")
def get_dk_showdown_salaries_endpoint(
    draft_group_id: Optional[int] = None,
    away: Optional[str] = None,
    home: Optional[str] = None,
    force_refresh: bool = False,
):
    """DK Showdown salary pool for one game -- by explicit draft_group_id, or
    found by matching the away/home team pair against every live showdown
    slate. Each player carries the base (FLEX) salary, the captain salary,
    and both DK draftableIds (for a real upload CSV). Includes kickers;
    defenses are in a separate `defense` list. See
    dk_scraper.get_dk_showdown_salaries()."""
    return get_dk_showdown_salaries(
        draft_group_id=draft_group_id, away_team=away, home_team=home, force_refresh=force_refresh
    )


@app.get("/api/dk/contests")
def get_dk_contests_endpoint(draft_group_id: Optional[int] = None, force_refresh: bool = False):
    """Live DraftKings contest list for one slate (Main Slate by default) --
    entry fee, prize pool, size, and current entries per contest (see
    dk_scraper.get_dk_contests() for the exact field mapping). Powers the
    Optimizer's contest picker so entry fee/payout settings can be
    auto-populated instead of hand-typed."""
    dk = get_dk_contests(draft_group_id=draft_group_id, force_refresh=force_refresh)
    return dk

@app.get("/api/dk/contest_payout")
def get_dk_contest_payout_endpoint(contest_id: int):
    """Real rank-by-rank $ payout table for one specific live DK contest
    (see dk_scraper.get_dk_contest_payout()). No caching here -- this is
    only called when a user actively selects a contest in the Optimizer, so
    hitting DK live each time is both rare and gives the freshest entries
    count."""
    return get_dk_contest_payout(contest_id)

def _resolve_main_slate_dg_for(week_games_df, year: int, draft_group_id: Optional[int]) -> Optional[int]:
    """draft_group_id if the caller picked one explicitly; otherwise the
    sticky per-(year, week) main-slate pin (see
    dk_scraper.resolve_main_slate_draft_group_id) rather than leaving it None
    and letting get_dk_salaries() fall back to DK's live "default," which
    silently flips to a small leftover slate once the real main slate's
    contests lock and drop out of the lobby."""
    if draft_group_id is not None:
        return draft_group_id
    if week_games_df.empty:
        return None
    week = int(week_games_df["week"].iloc[0])
    return resolve_main_slate_draft_group_id(year, week)


def _resolve_dk_pool(week_games_df, year: int, draft_group_id: Optional[int]) -> Dict[str, Any]:
    """Which salary source to trust for this week: a pre-lock snapshot
    (dk_scraper.load_prelock_salary_snapshot) when one exists, a live
    draftables fetch (get_dk_salaries) otherwise. The snapshot wins whenever
    present -- not just as a fallback -- because a live fetch by
    draft_group_id can't be trusted for a week whose slate has already
    closed: DK reuses/repoints old ids, so the fetch can return 200 OK with
    an entirely wrong, unrelated player pool instead of failing (see
    load_prelock_salary_snapshot's docstring for the confirmed 2026-09-16
    case on week 1's own pin). A live fetch is only actually correct for the
    current, still-open week, which is exactly the case with no snapshot
    written yet."""
    if week_games_df.empty:
        return get_dk_salaries(draft_group_id=draft_group_id)
    week = int(week_games_df["week"].iloc[0])
    snapshot = load_prelock_salary_snapshot(year, week)
    if snapshot is not None:
        return snapshot
    return get_dk_salaries(draft_group_id=_resolve_main_slate_dg_for(week_games_df, year, draft_group_id))


def get_week_salaries(week_games_df, year=2026, draft_group_id: Optional[int] = None) -> Dict[Tuple[str, str], Optional[int]]:
    """Real DraftKings salaries only -- None (not a fabricated placeholder)
    for a defense or player DK's live board doesn't currently price, e.g. a
    team whose game hasn't been posted to the slate yet. Callers must treat
    None as "can't be priced," not "missing data to estimate.\""""
    salaries: Dict[Tuple[str, str], Optional[int]] = {}
    teams = set(week_games_df["away_team"].unique()).union(set(week_games_df["home_team"].unique()))
    dk = _resolve_dk_pool(week_games_df, year, draft_group_id)

    for team in teams:
        salaries[("Defense", team)] = dk["defense"].get(team)

        roster_path = os.path.join(BASE_DIR, "data", "current_rosters", f"{team}_traits_{year}.json")
        if os.path.exists(roster_path):
            roster_data = load_json(roster_path)
            traits = roster_data.get("traits", {})
            for name in traits:
                dk_salary, _ = resolve_dk_salary(name, team, dk)
                salaries[(name, team)] = dk_salary
    return salaries

def get_week_dk_ids(week_games_df, year=2026, draft_group_id: Optional[int] = None) -> Dict[Tuple[str, str], Optional[int]]:
    """DK's per-slate draftableId for each player/defense on the selected
    slate -- keyed the same way as get_week_salaries() so callers can zip
    them together. Unlike salary, there's no synthetic fallback for a player
    DK doesn't list: None means "can't be put in a real DK-upload lineup,"
    not "missing data to estimate." See dk_scraper.get_dk_salaries()'s
    docstring for what draftableId is used for."""
    ids: Dict[Tuple[str, str], Optional[int]] = {}
    teams = set(week_games_df["away_team"].unique()).union(set(week_games_df["home_team"].unique()))
    dk = _resolve_dk_pool(week_games_df, year, draft_group_id)

    for team in teams:
        ids[("Defense", team)] = dk["defense_ids"].get(team)
        roster_path = os.path.join(BASE_DIR, "data", "current_rosters", f"{team}_traits_{year}.json")
        if os.path.exists(roster_path):
            roster_data = load_json(roster_path)
            for name in roster_data.get("traits", {}):
                _, dk_id = resolve_dk_salary(name, team, dk)
                ids[(name, team)] = dk_id
    return ids

def get_week_dk_names(week_games_df, year=2026, draft_group_id: Optional[int] = None) -> Dict[str, str]:
    """DK's real per-team defense display name (e.g. "Buccaneers"), keyed by
    team abbreviation only -- offense players don't need this, their `name`
    is already correct. Used solely to label the DST slot in a real
    lineup-upload CSV export; every other DST-identifying lookup in this file
    (salaries, ids, ownership, correlation) intentionally keeps keying on the
    sim engine's generic "Defense" name (see get_week_salaries's
    defense_salary_by_team comment for why), so this is looked up separately
    rather than replacing that name outright."""
    names: Dict[str, str] = {}
    teams = set(week_games_df["away_team"].unique()).union(set(week_games_df["home_team"].unique()))
    dk = _resolve_dk_pool(week_games_df, year, draft_group_id)
    for team in teams:
        nm = dk.get("defense_names", {}).get(team)
        if nm:
            names[team] = nm
    return names

def _overlay_live_salaries(players_list: List[Dict[str, Any]], week: int, year: int, draft_group_id: Optional[int]) -> None:
    """DK salaries move continuously; the sim/percentile stats they're
    bundled with in a week_projections response do not, which is exactly
    why that response gets cached in memory and snapshotted to disk (see
    get_week_projections()). Without this, a cache/snapshot hit would keep
    serving whatever salary happened to be live at the moment it was written
    (or None, for a player DK hadn't priced yet) -- salary is cheap to
    recompute (dict lookups only, no percentile/rank math), so
    it's refreshed in place on every read regardless of which of the three
    response sources (memory cache, disk snapshot, fresh live solve) served
    the rest of the payload.

    Skipped entirely when DK has never successfully served this slate this
    process lifetime AND no pre-lock snapshot exists for the week (see
    _resolve_dk_pool -- a snapshot, when present, is always trusted over a
    live fetch, so this guard only needs to protect the no-snapshot case).
    Without this guard, a backend restart wipes the in-memory salary cache
    and every later read of a JSON-snapshotted past week with no snapshot
    file either gets its real, baked-in salaries silently overwritten with
    None across the whole player pool (surfaces as "half my roster
    disappears" in the optimizer -- the response snapshot itself is
    untouched, only the served response was clobbered)."""
    if not os.path.exists(SCHEDULE_CSV_PATH):
        return
    sched_df = pd.read_csv(SCHEDULE_CSV_PATH)
    week_games_df = sched_df[(sched_df["week"] == week) & (sched_df["game_type"] == "REG")]
    if week_games_df.empty:
        return
    has_snapshot = not week_games_df.empty and load_prelock_salary_snapshot(year, int(week_games_df["week"].iloc[0])) is not None
    dg = _resolve_main_slate_dg_for(week_games_df, year, draft_group_id)
    if not has_snapshot and not get_dk_salaries(draft_group_id=dg)["is_live"]:
        return
    salaries = get_week_salaries(week_games_df, year, draft_group_id)
    dk_ids = get_week_dk_ids(week_games_df, year, draft_group_id)
    dk_names = get_week_dk_names(week_games_df, year, draft_group_id)
    for p in players_list:
        key = (p.get("name"), p.get("team"))
        if key in salaries:
            p["salary"] = salaries[key]
        if key in dk_ids:
            p["dk_id"] = dk_ids[key]
        if p.get("pos") == "DST" and p.get("team") in dk_names:
            p["dk_name"] = dk_names[p["team"]]

@app.get("/api/week_projections")
def get_week_projections(week: int = 1, year: int = 2026, draft_group_id: Optional[int] = None):
    """Aggregates all player simulations for the given week from the pre-loaded parquet cache."""
    global ALL_PLAYERS_CACHED
    reload_cache_if_changed()

    if ALL_PLAYERS_CACHED is None:
        raise HTTPException(
            status_code=503,
            detail="Pre-loaded simulation cache not available. Check server startup logs."
        )

    cache_key = (week, year, draft_group_id)
    fresh_token = _dfs_week_input_mtime(week, year)
    if cache_key in WEEK_PROJECTIONS_CACHE and WEEK_PROJECTIONS_CACHE_FRESHNESS.get(cache_key) == fresh_token:
        cached = WEEK_PROJECTIONS_CACHE[cache_key]
        _overlay_live_salaries(cached.get("players", []), week, year, draft_group_id)
        return cached

    # Serialise the expensive path: one caller computes (JSON-disk-cache load or
    # a full live optimal-lineup solve), the rest wait and then read the cache
    # it populated, instead of each independently kicking off the same brute-
    # force search (see the lock declaration above for why that's dangerous).
    with _week_projections_locks_guard:
        compute_lock = _WEEK_PROJECTIONS_LOCKS.setdefault(cache_key, threading.Lock())
    with compute_lock:
        if cache_key in WEEK_PROJECTIONS_CACHE and WEEK_PROJECTIONS_CACHE_FRESHNESS.get(cache_key) == fresh_token:
            cached = WEEK_PROJECTIONS_CACHE[cache_key]
            _overlay_live_salaries(cached.get("players", []), week, year, draft_group_id)
            return cached
        return _compute_week_projections(week, year, draft_group_id, cache_key, fresh_token)


def _compute_week_projections(week: int, year: int, draft_group_id: Optional[int], cache_key: tuple, fresh_token: float = 0.0):
    # Check if a precomputed JSON cache exists on disk -- only safe to use
    # when the requested slate is the Main Slate this snapshot was baked
    # against (no slate specified, or explicitly the current default -- the
    # frontend always sends an explicit id once it has loaded the slate
    # list, even for the default slate, so comparing only to None here would
    # silently force every request onto the much slower live-compute path
    # below as soon as slate selection is wired up). The snapshot's salaries
    # are refreshed below regardless -- see _overlay_live_salaries().
    json_cache_path = os.path.join(BASE_DIR, "data", "interim", f"week_{week}_full_projections.json")
    is_default_slate = draft_group_id is None or draft_group_id == get_dk_slates().get("default_draft_group_id")
    if is_default_slate and os.path.exists(json_cache_path):
        try:
            print(f"Loading precomputed projections for week {week} from JSON cache...")
            with open(json_cache_path, "r") as f:
                res = json.load(f)
            _overlay_live_salaries(res.get("players", []), week, year, draft_group_id)
            WEEK_PROJECTIONS_CACHE[cache_key] = res
            WEEK_PROJECTIONS_CACHE_FRESHNESS[cache_key] = fresh_token
            return res
        except Exception as e:
            print(f"Error loading projections JSON cache: {e}")

    # Get all game IDs for the specified week from the schedule
    if not os.path.exists(SCHEDULE_CSV_PATH):
        raise HTTPException(status_code=404, detail="Schedule CSV file not found.")
        
    sched_df = pd.read_csv(SCHEDULE_CSV_PATH)
    week_games_df = sched_df[(sched_df["week"] == week) & (sched_df["game_type"] == "REG")]
    
    if week_games_df.empty:
        return {"week": week, "players": [], "games": []}
        
    week_game_ids = week_games_df["game_id"].unique().tolist()

    # Prefer a dedicated DFS-week simulation (reflects this week's real
    # injuries/roster moves) over slicing the season-long "everyone healthy"
    # cache by game_id. Falls back to the season slice when no DFS-specific
    # sim has been run for this week yet (see run_week_sim_2026.py).
    dfs_wp = _get_dfs_week_players(week)
    if dfs_wp is not None:
        wp = dfs_wp.copy()
    else:
        wp = ALL_PLAYERS_CACHED[ALL_PLAYERS_CACHED["game_id"].isin(week_game_ids)].copy()
    if wp.empty:
        return {"week": week, "players": [], "games": []}
        
    # Generate salaries lookup
    salaries = get_week_salaries(week_games_df, year, draft_group_id)
    dk_ids = get_week_dk_ids(week_games_df, year, draft_group_id)
    dk_names = get_week_dk_names(week_games_df, year, draft_group_id)

    # Group and aggregate averages
    summary = wp.groupby(["Player", "Team", "Pos"]).agg({
        "rAtt": "mean", "rYds": "mean", "rTD": "mean",
        "targets": "mean", "rec": "mean", "recYds": "mean", "recTD": "mean",
        "pAtt": "mean", "pCmp": "mean", "pYds": "mean", "pTD": "mean",
        "int": "mean", "fumbles": "mean",
        "dk_score": "mean", "fd_score": "mean"
    }).reset_index()
    
    # Group and aggregate percentiles
    dk_quantiles = wp.groupby(["Player", "Team", "Pos"])["dk_score"].quantile([0.05, 0.25, 0.50, 0.75, 0.95]).unstack().reset_index()
    dk_quantiles.columns = ["Player", "Team", "Pos", "dk_p5", "dk_p25", "dk_p50", "dk_p75", "dk_p95"]
    
    fd_quantiles = wp.groupby(["Player", "Team", "Pos"])["fd_score"].quantile([0.05, 0.25, 0.50, 0.75, 0.95]).unstack().reset_index()
    fd_quantiles.columns = ["Player", "Team", "Pos", "fd_p5", "fd_p25", "fd_p50", "fd_p75", "fd_p95"]
    
    merged = summary.merge(dk_quantiles, on=["Player", "Team", "Pos"]).merge(fd_quantiles, on=["Player", "Team", "Pos"])

    # Calculate full 101 percentiles (0 to 100) for slider support
    dk_pcts_lookup = {}
    fd_pcts_lookup = {}
    pcts = np.linspace(0, 100, 101)
    for (player_name, team_name, pos_name), group in wp.groupby(["Player", "Team", "Pos"]):
        dk_pcts_lookup[(player_name, team_name, pos_name)] = np.percentile(group["dk_score"].values, pcts).round(2).tolist()
        fd_pcts_lookup[(player_name, team_name, pos_name)] = np.percentile(group["fd_score"].values, pcts).round(2).tolist()

    # Vectorized calculation of Top 12 and Top 1 probabilities across all simulation iterations
    formats = ["4_ppr", "4_half", "4_std", "6_ppr", "6_half", "6_std"]
    wp_scoring = wp[["Player", "Team", "Pos", "iteration", "pYds", "pTD", "int", "rYds", "rTD", "rec", "recYds", "recTD", "fumbles", "dk_score", "fd_score"]].copy()
    
    dst_mask = wp_scoring["Pos"] == "DST"
    agg_dict = {}
    
    for fmt in formats:
        is_6pt_td = fmt.startswith("6")
        ppr_val = 1.0 if "ppr" in fmt else (0.5 if "half" in fmt else 0.0)
        pass_td_val = 6.0 if is_6pt_td else 4.0
        
        # Season-long scoring formula
        scores = (
            wp_scoring["pYds"] * 0.04 +
            wp_scoring["pTD"] * pass_td_val +
            wp_scoring["int"] * -2.0 +
            wp_scoring["rYds"] * 0.1 +
            wp_scoring["rTD"] * 6.0 +
            wp_scoring["rec"] * ppr_val +
            wp_scoring["recYds"] * 0.1 +
            wp_scoring["recTD"] * 6.0 +
            wp_scoring["fumbles"] * -1.0
        )
        
        dst_scores = wp_scoring["dk_score"] if "ppr" in fmt else wp_scoring["fd_score"]
        wp_scoring[f"score_{fmt}"] = np.where(dst_mask, dst_scores, scores)
        
        # Rank within Pos group and iteration
        wp_scoring[f"rank_{fmt}"] = wp_scoring.groupby(["Pos", "iteration"])[f"score_{fmt}"].rank(ascending=False, method="min")
        
        # Top 12 and Top 1 finish markers
        wp_scoring[f"top12_{fmt}"] = (wp_scoring[f"rank_{fmt}"] <= 12).astype(int)
        wp_scoring[f"top1_{fmt}"] = (wp_scoring[f"rank_{fmt}"] == 1).astype(int)
        
        agg_dict[f"top12_{fmt}"] = "mean"
        agg_dict[f"top1_{fmt}"] = "mean"
        
    prob_summary = wp_scoring.groupby(["Player", "Team", "Pos"]).agg(agg_dict).reset_index()
    
    # Save the aggregated rank probabilities to a CSV file in data/interim/
    try:
        rank_output_path = os.path.join(BASE_DIR, "data", "interim", f"week_{week}_rank_probabilities.csv")
        prob_summary.to_csv(rank_output_path, index=False)
    except Exception as e:
        print(f"Error saving rank probabilities CSV: {e}")
        
    # Merge with the main player DataFrame
    merged = merged.merge(prob_summary, on=["Player", "Team", "Pos"])
    
    # Map game information (opponent, game_id, slate information) -- is_main
    # must agree with dk["main_slate_teams"] (same source /api/games uses)
    # rather than a weekday/time guess: a game can be a normal Sunday-
    # afternoon slot yet still not be on DK's live Main Slate yet (salaries
    # not posted), and a synthetic-formula salary silently standing in for
    # that team's players is exactly the "fake DK price" this endpoint must
    # not produce. Falls back to the weekday/time heuristic only when the
    # live feed itself isn't available. Routed through _resolve_dk_pool (not
    # a direct get_dk_salaries(draft_group_id=...) call) for the same reason
    # as run_simulation()'s salary lookup -- even a caller-supplied, once-
    # correct draft_group_id can't be trusted for a past week's own pin once
    # DK repoints it (see load_prelock_salary_snapshot's docstring).
    dk_for_slate = _resolve_dk_pool(week_games_df, year, draft_group_id)
    dk_live_slate = dk_for_slate["is_live"] and dk_for_slate["main_slate_teams"]
    team_info_lookup = {}
    for _, row in week_games_df.iterrows():
        away = row["away_team"]
        home = row["home_team"]
        game_id = row["game_id"]

        if dk_live_slate:
            is_main = away in dk_for_slate["main_slate_teams"] and home in dk_for_slate["main_slate_teams"]
        else:
            weekday = str(row.get("weekday", "Sunday"))
            gametime = str(row.get("gametime", "13:00"))
            is_main = True
            if weekday in ["Thursday", "Monday", "Friday", "Saturday"]:
                is_main = False
            elif weekday == "Sunday" and gametime >= "20:00":
                is_main = False

        team_info_lookup[away] = {"opponent": f"@{home}", "game_id": game_id, "is_main": is_main}
        team_info_lookup[home] = {"opponent": f"vs {away}", "game_id": game_id, "is_main": is_main}
        
    # Calculate DFS Optimal, Boom, and Value rates trial-by-trial for traditional
    # slate -- scoped to players with a real DK salary only: a player DK hasn't
    # priced can't actually be in a real lineup, so they must not enter the
    # optimal-lineup solve below (a None or fabricated salary there would
    # either crash the solve or bias it with a fake cheap/expensive price).
    priced_keys = [pk for pk in salaries if salaries[pk] is not None]
    optimal_counts = {p: 0 for p in priced_keys}
    boom_counts = {p: 0 for p in priced_keys}
    value_counts = {p: 0 for p in priced_keys}

    unique_iterations = wp["iteration"].unique()
    num_iterations = len(unique_iterations) if len(unique_iterations) > 0 else 1

    player_keys = priced_keys
    player_salaries = np.array([salaries[pk] for pk in player_keys])
    
    # Get clean positions mapping
    player_positions = {}
    for (p, t, pos), _ in wp.groupby(["Player", "Team", "Pos"]):
        player_positions[(p, t)] = pos

    player_positions_list = [player_positions.get(pk, "WR") for pk in player_keys]
    clean_positions = [pos.replace("1", "").replace("2", "").replace("3", "").replace("4", "").replace("5", "").replace("6", "") for pos in player_positions_list]
    
    # High-performance arrays extract to map score iterations
    wp_players = wp["Player"].values
    wp_teams = wp["Team"].values
    wp_iterations = wp["iteration"].values
    wp_dk_scores = wp["dk_score"].values
    
    iter_scores = {it: {} for it in unique_iterations}
    for i in range(len(wp_players)):
        p = wp_players[i]
        t = wp_teams[i]
        it = wp_iterations[i]
        score = wp_dk_scores[i]
        iter_scores[it][(p, t)] = score
        
        # Calculate Boom % (score >= 30) and Value % (score >= 3x salary) --
        # value % is undefined (not zero) without a real salary, so it's
        # simply skipped rather than measured against a fabricated price.
        sal = salaries.get((p, t))
        if score >= 30.0:
            boom_counts[(p, t)] = boom_counts.get((p, t), 0) + 1
        if sal is not None and score >= 3.0 * (sal / 1000.0):
            value_counts[(p, t)] = value_counts.get((p, t), 0) + 1
            
    # optimal_pct wants "how often is this player in the TRUE optimal lineup,
    # across every one of the week's sim iterations" -- prefer a batch-
    # precomputed file (scripts/simulation_runners/compute_optimal_pct_2026.py),
    # which solves genuinely every iteration via the exact MILP solver
    # (solve_optimal_lineup_milp) offline, over doing a small live sample here.
    # See WORKLOG 2026-09-15 -- the former live-only approach sampled just
    # OPTIMAL_LINEUP_SAMPLE_ITERATIONS iterations (still true today for a week
    # the batch script hasn't been run for yet), which is what produced the
    # lumpy "only ever 50% or 100%" percentages Cam flagged.
    precomputed_path = os.path.join(BASE_DIR, "data", "interim", f"week_{week}_optimal_pct.json")
    if os.path.exists(precomputed_path):
        with open(precomputed_path, "r") as f:
            precomputed = json.load(f)
        num_solve_iterations = precomputed.get("iterations") or 1
        for row in precomputed.get("counts", []):
            optimal_counts[(row["name"], row["team"])] = row["count"]
    else:
        from src.nfl_sim.optimizer import solve_optimal_lineup_milp
        # Sub-sample iterations to bound worst-case cost for a week that
        # hasn't had the batch pass run yet. solve_optimal_lineup_milp is a
        # real, exact MILP solve (~0.05-0.2s here vs. the old branch-and-
        # bound's ~1.5s/call) but still too slow to run across all ~10,000
        # iterations synchronously on a request; the lock above ensures that
        # cost is only ever paid once per (week,year,slate).
        solve_iterations = unique_iterations
        if len(unique_iterations) > OPTIMAL_LINEUP_SAMPLE_ITERATIONS:
            step = len(unique_iterations) // OPTIMAL_LINEUP_SAMPLE_ITERATIONS
            solve_iterations = unique_iterations[::step][:OPTIMAL_LINEUP_SAMPLE_ITERATIONS]

        num_solve_iterations = len(solve_iterations) if len(solve_iterations) > 0 else 1

        for it in solve_iterations:
            scores_arr = np.array([iter_scores[it].get(pk, 0.0) for pk in player_keys])
            opt_lineup = solve_optimal_lineup_milp(player_keys, player_salaries, clean_positions, scores_arr)
            for pk in opt_lineup:
                optimal_counts[pk] = optimal_counts.get(pk, 0) + 1

    players_list = []
    for _, row in merged.iterrows():
        name = row["Player"]
        team = row["Team"]
        pos = row["Pos"]
        
        t_info = team_info_lookup.get(team, {"opponent": "BYE", "game_id": "", "is_main": False})
        salary = salaries.get((name, team))
        dk_id = dk_ids.get((name, team))

        # Build rank probabilities lookup for each format
        rank_probs = {}
        for fmt in formats:
            rank_probs[f"top12_{fmt}"] = round(float(row[f"top12_{fmt}"]) * 100, 1)
            rank_probs[f"top1_{fmt}"] = round(float(row[f"top1_{fmt}"]) * 100, 1)
            
        players_list.append({
            "name": name,
            "team": team,
            "pos": pos,
            "opponent": t_info["opponent"],
            "game_id": t_info["game_id"],
            "is_main": t_info["is_main"],
            "salary": salary,
            "dk_id": dk_id,
            "dk_name": dk_names.get(team) if pos == "DST" else None,
            "rAtt": round(row["rAtt"], 2),
            "rYds": round(row["rYds"], 1),
            "rTD": round(row["rTD"], 2),
            "targets": round(row["targets"], 2),
            "rec": round(row["rec"], 2),
            "recYds": round(row["recYds"], 1),
            "recTD": round(row["recTD"], 2),
            "pAtt": round(row["pAtt"], 2),
            "pCmp": round(row["pCmp"], 2),
            "pYds": round(row["pYds"], 1),
            "pTD": round(row["pTD"], 2),
            "int": round(row["int"], 2),
            "fumbles": round(row["fumbles"], 2),
            "dk_score": round(row["dk_score"], 2),
            "fd_score": round(row["fd_score"], 2),
            "dk_p5": round(row["dk_p5"], 2),
            "dk_p25": round(row["dk_p25"], 2),
            "dk_p50": round(row["dk_p50"], 2),
            "dk_p75": round(row["dk_p75"], 2),
            "dk_p95": round(row["dk_p95"], 2),
            "fd_p5": round(row["fd_p5"], 2),
            "fd_p25": round(row["fd_p25"], 2),
            "fd_p50": round(row["fd_p50"], 2),
            "fd_p75": round(row["fd_p75"], 2),
            "fd_p95": round(row["fd_p95"], 2),
            "dk_pcts_all": dk_pcts_lookup.get((name, team, pos), [row["dk_score"]]*101),
            "fd_pcts_all": fd_pcts_lookup.get((name, team, pos), [row["fd_score"]]*101),
            "rank_probs": rank_probs,
            "optimal_pct": round((optimal_counts.get((name, team), 0) / num_solve_iterations) * 100.0, 2),
            "boom_pct": round((boom_counts.get((name, team), 0) / num_iterations) * 100.0, 2),
            "value_pct": round((value_counts.get((name, team), 0) / num_iterations) * 100.0, 2)
        })
        
    games_list = []
    for _, row in week_games_df.iterrows():
        games_list.append({
            "game_id": row["game_id"],
            "away_team": row["away_team"],
            "home_team": row["home_team"],
            "dk_main": team_info_lookup[row["away_team"]]["is_main"]
        })
        
    result = {
        "week": week,
        "players": players_list,
        "games": games_list
    }
    
    # Save results to a JSON cache to avoid future solver latency
    try:
        json_cache_path = os.path.join(BASE_DIR, "data", "interim", f"week_{week}_full_projections.json")
        os.makedirs(os.path.dirname(json_cache_path), exist_ok=True)
        with open(json_cache_path, "w") as f:
            json.dump(result, f)
        print(f"Saved projections for week {week} to JSON cache.")
    except Exception as e:
        print(f"Error saving projections JSON cache: {e}")

    WEEK_PROJECTIONS_CACHE[cache_key] = result
    WEEK_PROJECTIONS_CACHE_FRESHNESS[cache_key] = fresh_token
    return result

# Parquet mtime -> sims-per-game, so /api/sim_status stays cheap (read only
# the parquet footer, once per new run).
_SIM_ITERATIONS_BY_MTIME: Dict[tuple, int] = {}


@app.get("/api/sim_status")
def get_sim_status(week: int, year: int = 2026):
    """Cheap poll target (2026-09-23) so the Game Explorer / Showdown pages can
    auto-refresh when a new sim run lands, instead of serving whatever they
    loaded at page open until a manual reload.

    Inputs: week (int), year (int).
    Output: {
      week, version      -- _dfs_week_input_mtime: changes whenever the DFS
                            parquet, a roster file, or optimal_pct changes
                            (the same token week_sim_results' cache keys on)
      sims_updated_at    -- mtime of dfs_week_{week}_games.parquet (null if none)
      iterations         -- sims per game in that parquet (footer row count / week's games)
      running            -- sim_run_status marker dict while run_week_sim_2026 /
                            resim_games is writing a new run, else null
      results_ready      -- week_sim_results already computed for `version`
                            (false = the next /api/week_sim_results call rebuilds,
                            which takes minutes at 10K sims)
    }
    Reads no parquet data -- file mtimes, the parquet footer, and in-memory
    cache state only."""
    g_path = os.path.join(BASE_DIR, "data", "interim", f"dfs_week_{week}_games.parquet")
    sims_updated_at, iterations = None, None
    if os.path.exists(g_path):
        sims_updated_at = os.path.getmtime(g_path)
        key = (week, sims_updated_at)
        if key not in _SIM_ITERATIONS_BY_MTIME:
            try:
                import pyarrow.parquet as pq
                rows = pq.read_metadata(g_path).num_rows
                n_games = 0
                if os.path.exists(SCHEDULE_CSV_PATH):
                    sched = pd.read_csv(SCHEDULE_CSV_PATH, usecols=["season", "week", "game_type"])
                    n_games = int(((sched["season"] == year) & (sched["week"] == week)
                                   & (sched["game_type"] == "REG")).sum())
                _SIM_ITERATIONS_BY_MTIME[key] = int(round(rows / n_games)) if n_games else None
            except Exception as e:  # noqa: BLE001 -- status is best-effort, never fail the poll
                print(f"sim_status: couldn't read {g_path} footer: {e}")
                _SIM_ITERATIONS_BY_MTIME[key] = None
        iterations = _SIM_ITERATIONS_BY_MTIME[key]
    version = _dfs_week_input_mtime(week, year)
    cache_key = (week, year)
    return {
        "week": week,
        "version": version,
        "sims_updated_at": sims_updated_at,
        "iterations": iterations,
        "running": read_sim_run_marker(week, BASE_DIR),
        "results_ready": bool(cache_key in WEEK_SIM_RESULTS_CACHE
                              and WEEK_SIM_RESULTS_CACHE_FRESHNESS.get(cache_key) == version),
    }


@app.get("/api/week_sim_results")
def get_week_sim_results(week: int = 1, year: int = 2026):
    """Prepopulates every game in the week from the cached parquet sim data
    via run_simulation()'s own baseline cache-hit path, so the DFS site can
    show results immediately on load instead of requiring a per-game
    'Run Engine' click. Reuses get_rosters() for baseline overrides and
    run_simulation() for the actual computation — no new sim logic here."""
    reload_cache_if_changed()

    cache_key = (week, year)
    fresh_token = _dfs_week_input_mtime(week, year)
    if cache_key in WEEK_SIM_RESULTS_CACHE and WEEK_SIM_RESULTS_CACHE_FRESHNESS.get(cache_key) == fresh_token:
        return WEEK_SIM_RESULTS_CACHE[cache_key]

    # Serialise the expensive path: one caller computes, the rest wait and then
    # read the cache it populated.
    with _week_sim_locks_guard:
        compute_lock = _WEEK_SIM_LOCKS.setdefault(cache_key, threading.Lock())
    with compute_lock:
        if cache_key in WEEK_SIM_RESULTS_CACHE and WEEK_SIM_RESULTS_CACHE_FRESHNESS.get(cache_key) == fresh_token:
            return WEEK_SIM_RESULTS_CACHE[cache_key]
        return _compute_week_sim_results(week, year, cache_key, fresh_token)


def _compute_week_sim_results(week: int, year: int, cache_key: tuple, fresh_token: float = 0.0):
    json_cache_path = os.path.join(BASE_DIR, "data", "interim", f"week_{week}_sim_results.json")
    if not os.path.exists(SCHEDULE_CSV_PATH):
        raise HTTPException(status_code=404, detail="Schedule CSV file not found.")
    sched_df = pd.read_csv(SCHEDULE_CSV_PATH)
    week_games_df = sched_df[(sched_df["week"] == week) & (sched_df["game_type"] == "REG")]
    expected_game_ids = set(week_games_df["game_id"].astype(str))
    if os.path.exists(json_cache_path):
        # Stale-cache guard: this file used to be trusted unconditionally
        # once written, so a roster/starter edit after it was generated
        # (e.g. a depth-chart change) never showed up here even though
        # run_simulation()'s own starter-mismatch check would have caught
        # it on a fresh compute -- this is exactly what caused the DFS
        # site and the analytics site to disagree on 2026-08-22 (see
        # WORKLOG.md). Treat it as stale if any input it was built from
        # (the games/players parquet, or that year's roster files) is
        # newer than the cache file itself.
        json_mtime = os.path.getmtime(json_cache_path)
        roster_glob = os.path.join(BASE_DIR, "data", "current_rosters", "**", f"*_traits_{year}.json")
        newest_roster_mtime = max(
            (os.path.getmtime(p) for p in glob.glob(roster_glob, recursive=True)), default=0.0
        )
        # The DFS-week parquet (data/interim/dfs_week_{N}_*.parquet) is now this
        # endpoint's real data source (see _compute_one_game's use_dfs_week) --
        # a re-run of run_week_sim_2026.py must invalidate this cache.
        dfs_week_mtime = max(
            (os.path.getmtime(os.path.join(BASE_DIR, "data", "interim", f"dfs_week_{week}_{kind}.parquet"))
             for kind in ("players", "games")
             if os.path.exists(os.path.join(BASE_DIR, "data", "interim", f"dfs_week_{week}_{kind}.parquet"))),
            default=0.0,
        )
        newest_input_mtime = max(LAST_LOADED_TIME_GAMES, LAST_LOADED_TIME_PLAYERS, newest_roster_mtime, dfs_week_mtime)
        if json_mtime >= newest_input_mtime:
            try:
                with open(json_cache_path, "r") as f:
                    res = json.load(f)
                # A per-game exception during the original compute (e.g. a
                # transient race writing dk_main_slate_pins.json) silently
                # drops that one game_id from `res["games"]` -- see the
                # per-game try/except below. The mtime check above can't
                # catch that (the file is genuinely newer than its inputs,
                # just incomplete), so a missing game_id would otherwise be
                # served forever until something else invalidates the cache.
                # Confirmed live 2026-09-17: week_2_sim_results.json was
                # missing 2026_02_CLE_TB this way. Treat an incomplete cache
                # as stale instead.
                cached_game_ids = set(res.get("games", {}).keys())
                missing = expected_game_ids - cached_game_ids
                if not missing:
                    WEEK_SIM_RESULTS_CACHE[cache_key] = res
                    WEEK_SIM_RESULTS_CACHE_FRESHNESS[cache_key] = fresh_token
                    return res
                print(f"week_{week}_sim_results.json cache is missing game(s) {sorted(missing)} -- recomputing.")
            except Exception as e:
                print(f"Error loading week sim results JSON cache: {e}")
        else:
            print(f"week_{week}_sim_results.json cache is stale (older than parquet/roster inputs) -- recomputing.")

    def _compute_one_game(away: str, home: str, game_id: str):
        roster_data = get_rosters(away, home, year)

        team_overrides = {}
        player_overrides = []
        for team in [away, home]:
            settings = roster_data[team]["team_settings"]
            team_overrides[team] = TeamOverride(
                plays_per_game=settings["plays_per_game"],
                def_pressure_rate=settings["def_pressure_rate"],
                proe=settings["proe"],
            )
            for p in roster_data[team]["roster"]:
                player_overrides.append(PlayerOverride(
                    name=p["name"],
                    team=team,
                    target_share=p["target_share"],
                    carry_share=p["carry_share"],
                    catch_rate=p["catch_rate"],
                    rush_td_share=p.get("rush_td_share", 0.0),
                    rec_td_share=p.get("rec_td_share", 0.0),
                    # Always None here regardless of get_rosters()'s own
                    # "ownership_proj" field (that one's a flat display
                    # default for the separate single-game Simulator page,
                    # not a real per-player value) -- the actual slate-wide
                    # deterministic ownership gets filled in below, in
                    # get_week_sim_results(), where the full player pool
                    # for the softmax is available.
                    ownership_proj=None,
                    pos=p["pos"],
                    salary=p["salary"],
                ))

        # Baseline values match each team's cached defaults exactly, so
        # run_simulation() takes its existing fast cache-hit path. use_dfs_week
        # steers it to this week's DFS parquet (real availability baked in) so
        # it doesn't live-re-sim every game with an injury/depth-chart move.
        req = SimulationRequest(
            away_team=away,
            home_team=home,
            year=year,
            iterations=1000,
            apply_weighting=False,
            team_overrides=team_overrides,
            player_overrides=player_overrides,
            use_cached_defaults=True,
            # Full 1,000-iteration solve, no sub-sampling -- Cam's call
            # (2026-09-17): this bulk prepopulation runs once per week (then
            # cached), so a few extra minutes across the whole slate is a
            # fine trade for the showdown optimal-captain/FLEX rates
            # (optimal_cpt_pct/optimal_flex_pct) baked into it being exact
            # rather than sampled from just 2 iterations. Measured ~20-25s
            # per game on a real Showdown-sized pool -- see
            # _compute_showdown_optimal_rates' docstring for the per-
            # iteration cost this scales from.
            optimizer_sample_cap=1000,
            use_dfs_week=week,
        )
        sim_res = run_simulation(req)
        return {
            "game_id": game_id,
            "away_team": away,
            "home_team": home,
            "summary": sim_res["summary"],
            "projections": sim_res["projections"],
            # binned distribution only -- drop the per-iteration `raw` arrays
            # (16 games x 1000 iters would bloat this cached payload)
            "game_distribution": ({k: v for k, v in sim_res["game_distribution"].items() if k != "raw"}
                                  if sim_res.get("game_distribution") else None),
        }

    # Each game's cache-hit computation is CPU-bound, GIL-bound pure-Python
    # work (the branch-and-bound optimizer solve) — a thread pool was tried
    # here and measured *slower* than plain serial (GIL contention/context-
    # switching overhead with no real concurrency to offset it), so this is
    # a straightforward loop.
    games_results = {}
    for _, row in week_games_df.iterrows():
        away = str(row["away_team"])
        home = str(row["home_team"])
        game_id = str(row["game_id"])
        try:
            games_results[game_id] = _compute_one_game(away, home, game_id)
        except Exception as e:
            print(f"Error prepopulating sim results for {game_id} ({away}@{home}): {e}")

    # Ownership is inherently relative to the WHOLE slate's player pool (a
    # softmax over ~300 players), not just the two teams in one game, so it
    # can't be computed inside _compute_one_game() above -- one pass here,
    # across every game's players combined, gets the right relative
    # scoring. Deterministic (seeded by week) since this feeds contest-
    # EV/portfolio metrics that need to be comparable run-to-run, not a
    # fresh random draw on every page load -- next week naturally gets a
    # different seed.
    all_players_flat = [p for g in games_results.values() for p in g["projections"]]

    # Vegas implied team total (spread_line here is home-team-favored-by;
    # positive spread_line correlates positively with home margin in this
    # schedule data -- verified against 10 seasons of actual results).
    implied_total_by_team: Dict[str, float] = {}
    for _, row in week_games_df.iterrows():
        spread = row.get("spread_line")
        total = row.get("total_line")
        if pd.isna(spread) or pd.isna(total):
            continue
        implied_total_by_team[str(row["home_team"])] = (float(total) + float(spread)) / 2.0
        implied_total_by_team[str(row["away_team"])] = (float(total) - float(spread)) / 2.0

    # Cash-lineup consensus: this slate's own top cash-optimal builds (see
    # _generate_cash_consensus_lineups' docstring for why this is a useful
    # ownership signal). Uses the same players_list shape _solve_lineup_ilp
    # expects -- only priced players can be in a real lineup at all.
    priced_for_ilp = [p for p in all_players_flat if p.get('salary') is not None]
    # Shared shape for both the cash-consensus generator and the field
    # simulator below -- dk_pcts_all is only used by the latter (pet-player
    # misread targeting needs the P25-P95 spread), harmless extra key for
    # the former.
    priced_shaped = [
        {'name': p['name'], 'team': p['team'], 'pos': p['pos'], 'salary': p['salary'],
         'projection': p['dk_points'], 'dk_pcts_all': p.get('dk_pcts_all')}
        for p in priced_for_ilp
    ]
    cash_counts, n_cash_generated, cash_consensus_lineups = _generate_cash_consensus_lineups(
        priced_shaped,
        salary_cap=50000,
        pos_max_exposure={'DST': 0.5},  # no single DST monopolizes every build -- see docstring
    )

    ownership_input = [
        {
            'name': p['name'], 'team': p['team'],
            'pos': p['pos'], 'salary': p['salary'], 'projection': p['dk_points'],
            'ownership_pct': p.get('ownership_proj'),
            'implied_total': implied_total_by_team.get(p['team']),
            'cash_consensus_frac': (cash_counts.get((p['name'], p['team']), 0) / n_cash_generated) if n_cash_generated else 0.0,
        }
        for p in all_players_flat
    ]
    # Ownership PRIOR (salary/Vegas/cash-consensus-blended) -- seeds field
    # construction below (leverage fading, additive-ownership steering; see
    # field_simulator.py's module docstring for the chicken-and-egg
    # rationale) AND, via field_sample['ownership_pct'] a few lines down,
    # ends up as the actual displayed/exported ownership_proj for every
    # player on the week_projections response -- despite what the "no
    # longer the displayed number" phrasing above used to say, tracing
    # p['ownership_proj'] = field_sample['ownership_pct'].get(...) shows the
    # field's own realized rates come directly off whichever prior seeded
    # its archetype composition. So this IS worth the trained model, not
    # just the heuristic.
    predict_classic_ownership(ownership_input, week=week, seed=week)
    prior_own = {
        (p['name'], p['team']): own_p['ownership_pct']
        for p, own_p in zip(all_players_flat, ownership_input)
        if own_p['ownership_pct'] is not None
    }

    # Build the actual tournament field sample -- archetype-composed (sharp
    # / fake_sharp / casual / toilet, see field_simulator.py and
    # docs/implementation_plans/field_simulation_implementation_plan.md).
    # Cached (keyed by week; this endpoint only ever operates on the
    # default/main slate today, same as _compute_one_game()) so
    # /api/optimize and /api/field_sample reuse this build instead of
    # repeating it on every request -- the "build/score cache split" the
    # plan calls out as the actual fix for optimizer-edit latency.
    field_sample = build_field_sample(
        priced_shaped, prior_own, salary_cap=50000, K=FIELD_SAMPLE_K, seed=week,
    )
    FIELD_SAMPLE_CACHE[(week, None)] = {**field_sample, 'built_at': time.time(), 'week': week}

    # Final, DISPLAYED ownership is the field sample's own observed per-player
    # frequency, then soft-capped: the archetype builder over-rosters the top
    # value plays (chalk WR/RB come out 60-80%), unrealistic for a large NFL
    # field -- see _apply_ownership_cap / _ownership_soft_cap.
    for p in all_players_flat:
        p['ownership_proj'] = field_sample['ownership_pct'].get((p['name'], p['team']))
    _apply_ownership_cap(all_players_flat, own_key='ownership_proj', proj_key='dk_points')
    for p in all_players_flat:
        fo = p.get('ownership_proj')
        p['ownership_leverage'] = round(p['dk_points'] / fo, 2) if fo else 0.0

    result = {"week": week, "games": games_results, "cash_consensus_lineups": cash_consensus_lineups}

    try:
        os.makedirs(os.path.dirname(json_cache_path), exist_ok=True)
        with open(json_cache_path, "w") as f:
            json.dump(result, f)
        print(f"Saved week {week} sim results to JSON cache.")
    except Exception as e:
        print(f"Error saving week sim results JSON cache: {e}")

    WEEK_SIM_RESULTS_CACHE[cache_key] = result
    WEEK_SIM_RESULTS_CACHE_FRESHNESS[cache_key] = fresh_token
    return result

def _priced_pool_for_week(week: int, year: int) -> List[Dict[str, Any]]:
    """Rebuilds the priced player pool (the shape _generate_cash_consensus_lineups
    expects) from get_week_sim_results' cached per-game projections. A short,
    deliberate re-derivation of the same ~6 lines get_week_sim_results uses to
    build its own priced_shaped -- that function can't call itself, and this
    only runs on the exclusion path (rare, user-triggered), so it's cheaper to
    duplicate than to restructure the heavy pipeline function around it."""
    week_data = get_week_sim_results(week=week, year=year)
    all_players_flat = [p for g in week_data.get("games", {}).values() for p in g.get("projections", [])]
    return [
        {'name': p['name'], 'team': p['team'], 'pos': p['pos'], 'salary': p['salary'],
         'projection': p['dk_points'], 'dk_pcts_all': p.get('dk_pcts_all')}
        for p in all_players_flat if p.get('salary') is not None
    ]


def _cash_lineups_with_pool(
    week: int, year: int, excluded: List[Dict[str, str]], locked: List[Dict[str, str]]
) -> Dict[str, Any]:
    """Re-solves this slate's top cash-optimal builds with `excluded` players
    hard-removed from the pool and `locked` players forced into every build
    (see _generate_cash_consensus_lineups' locked_keys). Cheap: the week's
    sims/projections are already cached (see _priced_pool_for_week), so this
    only re-runs the ILP consensus loop (~10 solves), not the sims."""
    pool = _priced_pool_for_week(week, year)
    excluded_keys = {(e.get('name'), e.get('team')) for e in (excluded or [])}
    locked_keys = {(e.get('name'), e.get('team')) for e in (locked or [])}
    filtered_pool = [p for p in pool if (p['name'], p['team']) not in excluded_keys]
    _, n_generated, lineups = _generate_cash_consensus_lineups(
        filtered_pool, salary_cap=50000, pos_max_exposure={'DST': 0.5}, locked_keys=locked_keys,
    )
    return {
        "week": week, "lineups": lineups, "n_generated": n_generated,
        "excluded": excluded or [], "locked": locked or [],
    }


@app.get("/api/week_cash_lineups")
def get_week_cash_lineups(week: int = 1, year: int = 2026):
    """This slate's own top-10 cash-optimal (pure median, zero-variance)
    lineups for the Cash Lineups page -- the same builds
    _generate_cash_consensus_lineups() already produces as an ownership
    signal inside get_week_sim_results(), just surfaced directly instead of
    only feeding the softmax.

    Applies this week's saved player-pool overrides (cash_pool_store), if
    any, so a previously hand-excluded/locked player stays that way on every
    plain reload, not just right after clicking Rerun. With no overrides
    saved, this is a cheap thin wrapper reusing get_week_sim_results' cache;
    with overrides, it re-solves the consensus builds (see
    _cash_lineups_with_pool)."""
    pool = cash_pool_store.read_pool(year, week)
    if not pool["excluded"] and not pool["locked"]:
        week_data = get_week_sim_results(week=week, year=year)
        return {
            "week": week, "lineups": week_data.get("cash_consensus_lineups", []),
            "excluded": [], "locked": [],
        }
    return _cash_lineups_with_pool(week, year, pool["excluded"], pool["locked"])


@app.post("/api/week_cash_lineups")
def regenerate_week_cash_lineups(
    week: int = 1, year: int = 2026,
    excluded: List[Dict[str, str]] = Body(default=[]),
    locked: List[Dict[str, str]] = Body(default=[]),
):
    """Button-triggered rerun for the Cash Lineups page: re-solves the
    consensus builds with `excluded` players removed and `locked` players
    forced in, and persists both lists (cash_pool_store) so they're still
    applied the next time this week is loaded via the GET above -- these
    calls don't need to be re-made every visit."""
    cash_pool_store.write_pool(year, week, excluded, locked)
    return _cash_lineups_with_pool(week, year, excluded, locked)

@app.get("/api/field_sample")
def get_field_sample(week: int = 1, year: int = 2026, sample_lineups: int = 20):
    """Inspection endpoint for the archetype-composed tournament field
    sample built inside get_week_sim_results() (see field_simulator.py and
    docs/implementation_plans/field_simulation_implementation_plan.md) --
    lets us look at what the field actually looks like without needing the
    full Optimizer UI. Triggers a week_sim_results compute (which populates
    FIELD_SAMPLE_CACHE as a side effect) if this week hasn't been built yet
    this process lifetime; otherwise this is a cheap cache read.

    `sample_lineups` caps how many of the built field's actual lineups are
    returned in full (the field can be 1000+ lineups; returning all of them
    on every request is wasteful when you usually just want to eyeball a
    handful plus the aggregate composition/ownership).
    """
    cache_key = (week, None)
    if cache_key not in FIELD_SAMPLE_CACHE:
        get_week_sim_results(week=week, year=year)  # populates FIELD_SAMPLE_CACHE as a side effect
    field = FIELD_SAMPLE_CACHE.get(cache_key)
    if field is None:
        raise HTTPException(status_code=404, detail=f"No field sample available for week {week}.")

    lineups = field.get('lineups', [])
    return {
        "week": week,
        "built_at": field.get('built_at'),
        "k_built": field.get('k_built'),
        "archetype_counts": field.get('archetype_counts'),
        "ownership_pct": {f"{name}|{team}": pct for (name, team), pct in field.get('ownership_pct', {}).items()},
        "sample_lineups": [
            [{"name": p["name"], "team": p["team"], "pos": p["pos"], "salary": p["salary"], "projection": p["projection"]} for p in lu]
            for lu in lineups[:sample_lineups]
        ],
    }

@app.get("/api/games")
def get_games(week: int = 1, draft_group_id: Optional[int] = None):
    """Returns schedule, Vegas lines, dynamic team records, and DFS slate tags for a week."""
    if not os.path.exists(SCHEDULE_CSV_PATH):
        raise HTTPException(status_code=404, detail="Schedule CSV file not found.")

    df = pd.read_csv(SCHEDULE_CSV_PATH)
    # Routed through _resolve_dk_pool (prelock snapshot first) rather than a
    # direct get_dk_salaries(draft_group_id=...) call -- see run_simulation's
    # salary lookup fix for why even a caller-supplied, once-correct
    # draft_group_id can't be trusted for a past week once DK repoints it.
    dk = _resolve_dk_pool(df[(df["week"] == week) & (df["game_type"] == "REG")], 2026, draft_group_id)

    # Calculate records for all teams prior to the selected week
    records = {} # Team name -> [wins, losses, ties]
    past_games = df[(df["week"] < week) & (df["game_type"] == "REG")]
    
    all_teams = set(df["away_team"].unique()).union(set(df["home_team"].unique()))
    for team in all_teams:
        records[team] = [0, 0, 0]
        
    for _, row in past_games.iterrows():
        away = row["away_team"]
        home = row["home_team"]
        a_score = row.get("away_score")
        h_score = row.get("home_score")
        
        if pd.notna(a_score) and pd.notna(h_score):
            if a_score > h_score:
                records[away][0] += 1
                records[home][1] += 1
            elif h_score > a_score:
                records[home][0] += 1
                records[away][1] += 1
            else:
                records[away][2] += 1
                records[home][2] += 1
                
    week_df = df[(df["week"] == week) & (df["game_type"] == "REG")]
    
    games = []
    for _, row in week_df.iterrows():
        # Parse Vegas spread from home team perspective
        home_spread = row.get("spread_line", 0.0)
        total_line = row.get("total_line", 45.0)
        away_ml = row.get("away_moneyline")
        home_ml = row.get("home_moneyline")
        
        away_team = str(row["away_team"])
        home_team = str(row["home_team"])
        weekday = str(row.get("weekday", "Sunday"))
        gametime = str(row.get("gametime", "13:00"))

        # Slate tagging: prefer the real DraftKings Main Slate roster
        # (dk["main_slate_teams"], from get_dk_salaries()) when live -- it's
        # ground truth for which games DK actually bundles into its flagship
        # contests. Falls back to a weekday/time heuristic (Thu/Fri/Sat/Mon
        # and Sun games at/after 20:00 are night games, not on Main) when the
        # live feed hasn't been fetched successfully yet.
        if dk["is_live"] and dk["main_slate_teams"]:
            is_main = away_team in dk["main_slate_teams"] and home_team in dk["main_slate_teams"]
        else:
            is_main = True
            if weekday in ["Thursday", "Monday", "Friday", "Saturday"]:
                is_main = False
            elif weekday == "Sunday" and gametime >= "20:00":
                is_main = False

        away_rec = records.get(away_team, [0, 0, 0])
        home_rec = records.get(home_team, [0, 0, 0])
        
        away_rec_str = f"{away_rec[0]}-{away_rec[1]}" + (f"-{away_rec[2]}" if away_rec[2] > 0 else "")
        home_rec_str = f"{home_rec[0]}-{home_rec[1]}" + (f"-{home_rec[2]}" if home_rec[2] > 0 else "")
            
        games.append({
            "game_id": str(row["game_id"]),
            "away_team": away_team,
            "home_team": home_team,
            "away_record": away_rec_str,
            "home_record": home_rec_str,
            "gameday": str(row["gameday"]),
            "gametime": gametime,
            "weekday": weekday,
            "spread_line": float(home_spread),
            "total_line": float(total_line),
            "away_moneyline": None if pd.isna(away_ml) else int(away_ml),
            "home_moneyline": None if pd.isna(home_ml) else int(home_ml),
            "dk_main": is_main,
            "fd_main": is_main
        })
        
    return {"week": week, "games": games}


@app.post("/api/refresh_vegas_lines")
def post_refresh_vegas_lines(year: int = 2026):
    """Button-triggered refresh: re-pulls the nflverse schedule feed (spread,
    total, moneylines, odds) via nfl_data_py and overwrites
    data/external/schedule_{year}.csv in place. Every endpoint that reads
    Vegas lines (get_games above, run_simulation, etc.) reads that CSV fresh
    on each request, so the new lines apply immediately -- no cache to bust,
    no server restart needed. Note: pre-simulated game/player caches
    (data/interim/sim_results_2026_*.parquet) were baked with whatever lines
    were live at sim time and are NOT retroactively updated by this -- only a
    fresh /api/simulate call picks up the new baseline.
    """
    try:
        return refresh_vegas_lines(year)
    except Exception as e:
        raise HTTPException(status_code=502, detail=f"Vegas line refresh failed: {e}")


# -------------------------------------------------------------------------
# OPTIMIZER PERSISTENCE (DFS Optimizer weekly working state -- see
# src/api/optimizer_store.py and docs/implementation_plans/optimizer_persistence_plan.md)
# -------------------------------------------------------------------------
@app.get("/api/optimizer/state")
def get_optimizer_state(week: int = Query(..., ge=1, le=22), season: int = 2026):
    """The DFS Optimizer's saved working state for a week (settings + manual
    overlay + slate). Empty object when nothing has been saved yet."""
    return optimizer_store.read_state(season, week)


@app.put("/api/optimizer/state")
def put_optimizer_state(
    state: Dict[str, Any] = Body(...),
    week: int = Query(..., ge=1, le=22),
    season: int = 2026,
):
    """Persist the Optimizer's working state for a week (debounced autosave from
    the frontend). The body is stored as-is."""
    return optimizer_store.write_state(season, week, state)


@app.get("/api/optimizer/builds")
def list_optimizer_builds(week: int = Query(..., ge=1, le=22), season: int = 2026):
    """Summaries of every saved Optimize run for a week, newest first."""
    return optimizer_store.list_builds(season, week)


@app.get("/api/optimizer/builds/{build_id}")
def get_optimizer_build(build_id: str, week: int = Query(..., ge=1, le=22), season: int = 2026):
    b = optimizer_store.read_build(season, week, build_id)
    if b is None:
        raise HTTPException(status_code=404, detail="build not found")
    return b


@app.post("/api/optimizer/builds")
def create_optimizer_build(
    build: Dict[str, Any] = Body(...),
    week: int = Query(..., ge=1, le=22),
    season: int = 2026,
):
    """Save one Optimize run (lineups + the frozen inputs that produced them)."""
    return optimizer_store.write_build(season, week, build)


def _live_build_contest(settings: Dict[str, Any]) -> Dict[str, Any]:
    """`settings["contest"]` (the DK Contest Picker's selection), or {} if
    it's stale. The picker sets `contest` and the plain `entryFee` field
    together (see Optimizer.jsx's ContestPicker.applyContest), but only
    `entryFee` has any other edit path after that -- so once the two
    disagree, `contest`'s name/prize_pool/payout table no longer describes
    the fee this build actually carries, and trusting it silently attaches
    the wrong contest to the lineups (bit two High Stakes batches
    2026-09-16, both stuck on a stale Screen Pass selection from an earlier
    run). Treating that disagreement as "no contest attached" instead falls
    back to the same label-based contest_name/plain-entryFee path a build
    that never had one goes through."""
    contest = settings.get("contest") or {}
    manual_fee = settings.get("entryFee")
    if contest and manual_fee is not None and contest.get("entry_fee") is not None \
            and float(contest["entry_fee"]) != float(manual_fee):
        return {}
    return contest


def _bulk_register_paper_entries(build: Dict[str, Any], account_id: str) -> int:
    """When a Build gets tagged to a bankroll account, register each of its
    lineups as a paper_entries.json row (see paper_store.py) so
    scripts/dfs_ownership/score_paper_entries.py can settle them exactly like
    any individually-flagged paper entry -- no changes needed to that script.
    Carries prize_pool/paying_positions/contest_type through on each entry so
    a later payout estimate (get_default_payout_structure) has what it needs
    without re-reading the build file. Classic-only for now (Builds are a
    classic-optimizer concept; Showdown has its own per-lineup 📝 flag flow
    straight into paper_store, which this doesn't touch)."""
    settings = build.get("settings") or {}
    contest = _live_build_contest(settings)
    contest_name = contest.get("name") or build.get("label") or build.get("build_id")
    entry_fee = contest.get("entry_fee", settings.get("entryFee")) or 0
    max_entries = contest.get("field_size") or settings.get("contestSize") or 1
    week = build.get("week")
    season = build.get("season", 2026)

    n = 0
    for lineup in build.get("lineups") or []:
        entry = {
            "slate_format": "classic",
            "source": "optimize",
            "label": build.get("label"),
            "contest_name": contest_name,
            "entry_fee": entry_fee,
            "max_entries": max_entries,
            "players": lineup.get("players", []),
            "model": lineup,
            "build_id": build.get("build_id"),
            "account_id": account_id,
            "prize_pool": contest.get("prize_pool"),
            "paying_positions": contest.get("paying_positions"),
            "contest_type": settings.get("contestType"),
            # DK's own real rank-by-rank payout table, when the attached
            # contest carries one (dk_scraper.get_dk_contest_payout) -- lets
            # score_paper_entries.py's _estimated_payout skip the generic
            # curve approximation entirely. None on a contest attached only
            # via prize_pool/paying_positions (the /api/dk/contests summary
            # shape, not the per-contest payout endpoint).
            "payout_tiers": contest.get("payout_tiers"),
        }
        paper_store.add_entry(season, week, "main_slate", entry)
        n += 1
    return n


@app.patch("/api/optimizer/builds/{build_id}")
def patch_optimizer_build(
    build_id: str,
    patch: Dict[str, Any] = Body(...),
    week: int = Query(..., ge=1, le=22),
    season: int = 2026,
):
    """Update a build's label / pinned / submitted / submission / account_id
    fields. Setting account_id to a NEW value (first tag, or a change from
    whatever it was) bulk-registers every one of the build's lineups as a
    paper entry under that account -- see _bulk_register_paper_entries."""
    prior = optimizer_store.read_build(season, week, build_id)
    prior_account_id = prior.get("account_id") if prior else None

    b = optimizer_store.patch_build(season, week, build_id, patch)
    if b is None:
        raise HTTPException(status_code=404, detail="build not found")

    new_account_id = patch.get("account_id")
    if new_account_id and new_account_id != prior_account_id:
        _bulk_register_paper_entries(b, new_account_id)

    return b


@app.delete("/api/optimizer/builds/{build_id}")
def delete_optimizer_build(build_id: str, week: int = Query(..., ge=1, le=22), season: int = 2026):
    return {"deleted": optimizer_store.delete_build(season, week, build_id)}


@app.post("/api/optimizer/prune")
def prune_optimizer_builds(week: int = Query(..., ge=1, le=22), season: int = 2026):
    """Delete a week's throwaway autosave builds (not pinned / labeled / submitted)."""
    return {"removed": optimizer_store.prune_builds(season, week)}


# -------------------------------------------------------------------------
# BANKROLL ACCOUNTS -- named paper/real buckets a Build (or, for Showdown,
# a directly-flagged paper entry) tags itself into via account_id, so
# /api/bankroll can roll up cost/winnings per account. See account_store.py.
# -------------------------------------------------------------------------
@app.get("/api/accounts")
def list_accounts():
    return {"accounts": account_store.list_accounts()}


@app.post("/api/accounts")
def create_account(payload: Dict[str, Any] = Body(...)):
    try:
        return account_store.create_account(
            payload.get("label", ""), payload.get("kind", ""),
            payload.get("starting_bankroll", 0.0))
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@app.patch("/api/accounts/{account_id}")
def patch_account(account_id: str, patch: Dict[str, Any] = Body(...)):
    a = account_store.patch_account(account_id, patch)
    if a is None:
        raise HTTPException(status_code=404, detail="account not found")
    return a


@app.delete("/api/accounts/{account_id}")
def delete_account(account_id: str):
    return {"deleted": account_store.delete_account(account_id)}


@app.get("/api/bankroll")
def get_bankroll():
    """Per-account roll-up across every week: cost (entry_fee x lineups,
    charged the moment a build is tagged to an account -- see
    _bulk_register_paper_entries) vs. settled real winnings (from
    scripts/dfs_ownership/score_paper_entries.py's estimated_payout, joined
    by build_id/account_id -- see that script's _estimated_payout for what
    "estimated" means: a real backtested score against a generic payout
    curve, not DK's own curve, which isn't observable). An entry with no
    settled row yet counts toward `pending`, not `net_pnl` -- money
    committed but not yet won or lost. Classic-only for now, same scope as
    the Builds system this reads (see _bulk_register_paper_entries)."""
    accounts = account_store.list_accounts()
    by_id = {a["account_id"]: {
        **a,
        "n_builds": 0, "n_lineups_total": 0,
        "cost_total": 0.0, "cost_settled": 0.0, "cost_pending": 0.0,
        "winnings_settled": 0.0, "n_lineups_settled": 0, "n_lineups_pending": 0,
        "weeks": {},
    } for a in accounts}

    build_paths = sorted(glob.glob(os.path.join(BASE_DIR, "data", "optimizer", "*", "week_*", "builds", "*.json")))
    build_by_id: Dict[str, Dict[str, Any]] = {}
    for path in build_paths:
        try:
            with open(path, "r") as f:
                b = json.load(f)
        except (json.JSONDecodeError, OSError):
            continue
        acc = by_id.get(b.get("account_id"))
        if acc is None:
            continue
        n_lineups = len(b.get("lineups") or [])
        entry_fee = _live_build_contest(b.get("settings") or {}).get("entry_fee")
        if entry_fee is None:
            entry_fee = (b.get("settings") or {}).get("entryFee") or 0
        cost = n_lineups * entry_fee
        week = b.get("week")
        acc["n_builds"] += 1
        acc["n_lineups_total"] += n_lineups
        acc["cost_total"] += cost
        wk = acc["weeks"].setdefault(week, {"week": week, "n_lineups": 0, "cost": 0.0,
                                            "winnings_settled": 0.0, "n_settled": 0})
        wk["n_lineups"] += n_lineups
        wk["cost"] += cost
        build_by_id[b.get("build_id")] = {"account_id": b.get("account_id"), "week": week, "entry_fee": entry_fee}

    # Paper entries with no build_id -- never came from an Optimizer Build,
    # e.g. a real DK entry pulled in from a settled contest's standings CSV
    # (see the Bankroll "Real" account's import flow). Each is its own
    # one-lineup cost line, and (since there's no build_id to key on) joined
    # to its settled result below by entry_id instead. Builds are counted
    # per distinct contest here (not per lineup) to match the "one row per
    # submitted batch" meaning n_builds has everywhere else.
    entry_info: Dict[str, Dict[str, Any]] = {}
    entries_paths = sorted(glob.glob(os.path.join(BASE_DIR, "data", "dfs_ownership", "*", "week_*", "*", "paper_entries.json")))
    seen_contests: set = set()
    for path in entries_paths:
        try:
            with open(path, "r") as f:
                doc = json.load(f)
        except (json.JSONDecodeError, OSError):
            continue
        for e in doc.get("entries", []):
            if e.get("build_id"):
                continue  # already counted via its Build file above
            acc = by_id.get(e.get("account_id"))
            if acc is None:
                continue
            entry_fee = e.get("entry_fee") or 0
            week = e.get("week")
            acc["n_lineups_total"] += 1
            acc["cost_total"] += entry_fee
            contest_key = (e.get("account_id"), week, e.get("contest_name"))
            if contest_key not in seen_contests:
                seen_contests.add(contest_key)
                acc["n_builds"] += 1
            wk = acc["weeks"].setdefault(week, {"week": week, "n_lineups": 0, "cost": 0.0,
                                                "winnings_settled": 0.0, "n_settled": 0})
            wk["n_lineups"] += 1
            wk["cost"] += entry_fee
            entry_info[e.get("entry_id")] = {"account_id": e.get("account_id"), "week": week, "entry_fee": entry_fee}

    paper_results_path = os.path.join(BASE_DIR, "data", "dfs_ownership", "_processed", "paper_results.parquet")
    if os.path.exists(paper_results_path):
        results_df = pd.read_parquet(paper_results_path)
        for _, row in results_df.iterrows():
            # A parquet round-trip can leave a missing build_id as something
            # still truthy in Python (e.g. a float NaN), so this looks it up
            # rather than branching on `if build_id` -- a real DK entry (no
            # build_id at all) falls through to the entry_id-keyed lookup
            # either way.
            info = build_by_id.get(row.get("build_id"))
            if info is None:
                info = entry_info.get(row.get("entry_id"))
            if info is None:
                continue  # not a Bankroll-tracked entry (older/individually-flagged, or unknown account)
            acc = by_id.get(info["account_id"])
            if acc is None:
                continue
            payout = row.get("estimated_payout")
            settled = payout is not None and not pd.isna(payout)
            if not settled:
                continue
            payout = float(payout)
            acc["winnings_settled"] += payout
            acc["cost_settled"] += info["entry_fee"]
            acc["n_lineups_settled"] += 1
            wk = acc["weeks"].get(info["week"])
            if wk is not None:
                wk["winnings_settled"] += payout
                wk["n_settled"] += 1

    out = []
    for acc in by_id.values():
        acc["cost_pending"] = round(acc["cost_total"] - acc["cost_settled"], 2)
        acc["n_lineups_pending"] = acc["n_lineups_total"] - acc["n_lineups_settled"]
        net_pnl = acc["winnings_settled"] - acc["cost_settled"]
        acc["net_pnl_settled"] = round(net_pnl, 2)
        acc["roi_settled_pct"] = round(100.0 * net_pnl / acc["cost_settled"], 1) if acc["cost_settled"] else None
        acc["current_bankroll"] = round(acc["starting_bankroll"] + net_pnl, 2)
        acc["cost_total"] = round(acc["cost_total"], 2)
        acc["cost_settled"] = round(acc["cost_settled"], 2)
        acc["winnings_settled"] = round(acc["winnings_settled"], 2)
        acc["weeks"] = sorted(
            [{**w, "cost": round(w["cost"], 2), "winnings_settled": round(w["winnings_settled"], 2)}
             for w in acc["weeks"].values()],
            key=lambda w: w["week"])
        out.append(acc)
    return {"accounts": out}


@app.get("/api/bankroll/{account_id}/entries")
def get_bankroll_account_entries(account_id: str):
    """Drill-down for one account: every paper entry tagged to it (across
    every week/slate), grouped by the Build that produced it -- contest,
    lineup composition, predicted vs. settled-real result, and the notes/
    late_swap annotation (see paper_store.update_entry). Reads every
    paper_entries.json in the archive rather than joining through the Build
    files (unlike /api/bankroll's roll-up) since an entry already carries
    everything needed (players/model/contest_name) and this is a per-entry,
    not per-build-cost, view."""
    entries_paths = sorted(glob.glob(os.path.join(BASE_DIR, "data", "dfs_ownership", "*", "week_*", "*", "paper_entries.json")))

    results_by_entry: Dict[str, Dict[str, Any]] = {}
    paper_results_path = os.path.join(BASE_DIR, "data", "dfs_ownership", "_processed", "paper_results.parquet")
    if os.path.exists(paper_results_path):
        results_df = pd.read_parquet(paper_results_path).replace({np.nan: None})
        for _, row in results_df.iterrows():
            eid = row.get("entry_id")
            if eid:
                results_by_entry[eid] = row.to_dict()

    builds: Dict[str, Dict[str, Any]] = {}
    for path in entries_paths:
        try:
            with open(path, "r") as f:
                doc = json.load(f)
        except (json.JSONDecodeError, OSError):
            continue
        for e in doc.get("entries", []):
            if e.get("account_id") != account_id:
                continue
            # No build_id (a real DK entry, not from an Optimizer Build) --
            # group by contest instead of collapsing every such entry into
            # one indiscriminate bucket, which used to silently mix e.g.
            # First Down and Screen Pass entries under the same "untagged"
            # row.
            build_id = e.get("build_id") or f"entry-contest:{e.get('contest_name')}"
            b = builds.setdefault(build_id, {
                "build_id": build_id, "label": e.get("label"), "week": e.get("week"),
                "contest_name": e.get("contest_name"), "entry_fee": e.get("entry_fee"),
                "slate_id": e.get("slate_id"), "entries": [],
            })
            result = results_by_entry.get(e.get("entry_id"))
            model = e.get("model") or {}
            b["entries"].append({
                "entry_id": e.get("entry_id"),
                "created_at": e.get("created_at"),
                "players": e.get("players", []),
                "predicted_score": model.get("projected_score"),
                "actual_score": result.get("actual_score") if result else None,
                "actual_rank": result.get("actual_rank") if result else None,
                "field_size": result.get("field_size") if result else None,
                "beat_field_pct": result.get("beat_field_pct") if result else None,
                "finish_percentile": result.get("finish_percentile") if result else None,
                "estimated_payout": result.get("estimated_payout") if result else None,
                "settled": result is not None,
                "notes": e.get("notes"),
                "late_swap": bool(e.get("late_swap")),
            })

    out = sorted(builds.values(), key=lambda b: (b["week"] or 0, b["label"] or ""))
    return {"account_id": account_id, "builds": out}


@app.post("/api/bankroll/{account_id}/clear")
def clear_bankroll_account(account_id: str):
    """Resets one account to empty, across every week: un-tags every Build
    currently pointed at it (account_id -> None -- the lineups themselves
    are NOT deleted, still visible/manageable from the Optimizer's own
    Builds panel, just no longer counted on the Bankroll page) and deletes
    every paper_entries.json row tagged to it (those exist only as a
    bankroll-tracking artifact of the tagging -- see
    _bulk_register_paper_entries -- so once untagged they serve no purpose).
    For rebuilding a contaminated account from scratch (e.g. lineups run
    against a since-fixed bad salary feed) without losing the old lineups
    entirely or hand-clicking through every row."""
    n_builds = 0
    build_paths = glob.glob(os.path.join(BASE_DIR, "data", "optimizer", "*", "week_*", "builds", "*.json"))
    for path in build_paths:
        try:
            with open(path, "r") as f:
                b = json.load(f)
        except (json.JSONDecodeError, OSError):
            continue
        if b.get("account_id") != account_id:
            continue
        season_str = os.path.basename(os.path.dirname(os.path.dirname(os.path.dirname(path))))
        week_str = os.path.basename(os.path.dirname(os.path.dirname(path)))
        try:
            season, week = int(season_str), int(week_str.replace("week_", ""))
        except ValueError:
            continue
        optimizer_store.patch_build(season, week, b.get("build_id"), {"account_id": None})
        n_builds += 1

    n_entries = paper_store.clear_account_entries(account_id)
    return {"account_id": account_id, "builds_untagged": n_builds, "entries_removed": n_entries}


@app.patch("/api/paper/entries/{entry_id}")
def patch_paper_entry(entry_id: str, slate_id: str, patch: Dict[str, Any] = Body(...),
                       week: int = Query(..., ge=1, le=22), year: int = 2026):
    """Annotate one paper entry -- notes / late_swap only, see
    paper_store.update_entry. Used by the Bankroll account-detail view."""
    e = paper_store.update_entry(year, week, slate_id, entry_id, patch)
    if e is None:
        raise HTTPException(status_code=404, detail="entry not found")
    return e


# -------------------------------------------------------------------------
# WORKSPACE SAVE SLOTS (3 switchable, autosaved workspace snapshots per
# slate -- see src/api/workspace_store.py). Shared by the classic and
# showdown optimizer pages so a pool build / lineup set / Game-Read scenario
# survives a page or tab switch, and up to 3 different takes on the same
# slate can be kept side by side (like save files). Additive to -- not a
# replacement for -- optimizer_store's per-week state/builds above.
# -------------------------------------------------------------------------
@app.get("/api/workspace/slots")
def get_workspace_slots(slate_key: str, week: int = Query(..., ge=1, le=22), season: int = 2026):
    """Lightweight metadata for all 3 slots + which one is active -- labels
    and timestamps only, no lineup payloads, so a slot-switcher UI can render
    without fetching all three full blobs."""
    return workspace_store.list_slots(season, week, slate_key)


@app.get("/api/workspace/slot")
def get_workspace_slot(slate_key: str, slot: int = Query(..., ge=1, le=3),
                        week: int = Query(..., ge=1, le=22), season: int = 2026):
    """The full saved blob for one slot (or an empty shell if never saved)."""
    return workspace_store.read_slot(season, week, slate_key, slot)


@app.put("/api/workspace/slot")
def put_workspace_slot(slate_key: str, slot: int = Query(..., ge=1, le=3),
                        week: int = Query(..., ge=1, le=22), season: int = 2026,
                        body: Dict[str, Any] = Body(...)):
    """Autosave target: persists ``body.data`` into one slot. ``body.label``
    is optional -- omit it on a routine autosave tick so a name Cam typed
    doesn't get clobbered back to "Slot N"."""
    return workspace_store.write_slot(season, week, slate_key, slot, body.get("data"), body.get("label"))


@app.post("/api/workspace/active")
def set_workspace_active(payload: Dict[str, Any] = Body(...)):
    """Switch which slot is "current" for a slate -- the slot the page loads
    from and autosaves into until switched again."""
    try:
        return workspace_store.set_active(int(payload["season"]), int(payload["week"]),
                                          str(payload["slate_key"]), int(payload["slot"]))
    except (KeyError, ValueError) as e:
        raise HTTPException(status_code=400, detail=str(e))


@app.patch("/api/workspace/slot")
def rename_workspace_slot(slate_key: str, slot: int = Query(..., ge=1, le=3),
                           week: int = Query(..., ge=1, le=22), season: int = 2026,
                           payload: Dict[str, Any] = Body(...)):
    """Rename a slot (e.g. "Slot 1" -> "Chalk build") without touching its data."""
    return workspace_store.rename_slot(season, week, slate_key, slot, payload.get("label", ""))


@app.delete("/api/workspace/slot")
def clear_workspace_slot(slate_key: str, slot: int = Query(..., ge=1, le=3),
                          week: int = Query(..., ge=1, le=22), season: int = 2026):
    """"Start fresh": wipe a slot's saved data (keeps its label)."""
    return workspace_store.clear_slot(season, week, slate_key, slot)


# -------------------------------------------------------------------------
# PAPER TRADING (Phase 3) -- flag a lineup as "I'm actually entering this";
# scripts/dfs_ownership/score_paper_entries.py settles it later against a
# dropped-in standings CSV. See src/api/paper_store.py. `slate_id` here is
# the ownership-archive folder name (`showdown_<AWAY>_<HOME>` / `main_slate`),
# NOT the workspace-slot `slate_key` above (game_id-based) -- paper entries
# live alongside that slate's salaries/standings, not the UI session state.
# -------------------------------------------------------------------------
@app.get("/api/paper/entries")
def list_paper_entries(slate_id: str, week: int = Query(..., ge=1, le=22), year: int = 2026):
    return {"entries": paper_store.list_entries(year, week, slate_id)}


@app.post("/api/paper/entries")
def create_paper_entry(slate_id: str, entry: Dict[str, Any] = Body(...),
                        week: int = Query(..., ge=1, le=22), year: int = 2026):
    return paper_store.add_entry(year, week, slate_id, entry)


@app.delete("/api/paper/entries/{entry_id}")
def remove_paper_entry(entry_id: str, slate_id: str,
                        week: int = Query(..., ge=1, le=22), year: int = 2026):
    return {"deleted": paper_store.delete_entry(year, week, slate_id, entry_id)}


@app.post("/api/paper/grade")
def grade_paper_entries(week: Optional[int] = Query(None, ge=1, le=22), year: Optional[int] = None):
    """UI-triggered equivalent of running scripts/dfs_ownership/score_paper_entries.py
    by hand -- rescans every paper_entries.json for a dropped-in standings CSV
    and rewrites _processed/paper_results.parquet. Cheap and idempotent (no
    CSV yet just means nothing new settles), so the Bankroll/Evaluation pages
    call this on load; the "Grade Now" button is the same call on demand."""
    from scripts.dfs_ownership.score_paper_entries import run_grading
    return run_grading(year, week)


def _read_parquet_rows(rel_path: str, slate_id: Optional[str] = None) -> list:
    """One parquet -> JSON-safe list of row dicts, optionally filtered to one
    slate. NaN (common in these evaluation tables -- e.g. a paper entry with
    no settled contest yet) is replaced with None first: Python's json module
    happily emits a bare `NaN` token, which is not valid JSON and several
    strict parsers (including some browsers' fetch().json()) reject."""
    path = os.path.join(BASE_DIR, "data", "dfs_ownership", "_processed", rel_path)
    if not os.path.exists(path):
        return []
    df = pd.read_parquet(path)
    if slate_id and "slate_id" in df.columns:
        df = df[df["slate_id"] == slate_id]
    return df.replace({np.nan: None}).to_dict("records")


@app.get("/api/eval/field")
def get_field_eval(slate_id: Optional[str] = None):
    """Field analysis for settled contests -- see scripts/dfs_ownership/eval_field.py
    (winner, percentile score cutoffs, hindsight-optimal lineup, top-finisher
    ownership profile). Offline-built; empty until that script has run."""
    return {"rows": _read_parquet_rows("field_eval.parquet", slate_id)}


@app.get("/api/eval/paper")
def get_paper_eval(slate_id: Optional[str] = None):
    """Settled paper-trade results -- see scripts/dfs_ownership/score_paper_entries.py
    (predicted vs actual score/ownership/rank for lineups you flagged as
    "actually entering"). Offline-built; empty until that script has run."""
    return {"rows": _read_parquet_rows("paper_results.parquet", slate_id)}


@app.get("/api/eval/sim_replay")
def get_sim_replay_eval(slate_id: Optional[str] = None):
    """Post-lock sim replay -- see scripts/dfs_ownership/sim_replay_field.py
    (real field lineups from a settled contest, rescored under OUR own
    week's simulations instead of the real result, for every entry flagged
    "actually entering"). Answers "how would this lineup have done against
    the real field if our model were reality", not "how did it actually do"
    (that's /api/eval/paper). Offline-built; empty until that script has run."""
    return {"rows": _read_parquet_rows("sim_replay.parquet", slate_id)}


# -------------------------------------------------------------------------
# SIM REPLAYS (2026-09-15) -- "how did MY submitted lineups do against what
# our sim projected for them." NOT the same thing as /api/eval/sim_replay
# above (that one rescores an entire real field with our sim, and needs a
# paper_entries.json flag beforehand). This is simpler and needs neither:
# it reads your own rows straight out of the archived standings CSVs
# (matched by EntryName, see src/api/sim_replay_store.py), which already
# carry both your real lineup and its real scored Points, then compares
# each entry against our sim's own projected distribution for that exact
# lineup. Configure your DK username(s) in data/dfs_ownership/config.json.
# -------------------------------------------------------------------------
@app.get("/api/sim_replay/contests")
def list_sim_replay_contests(year: Optional[int] = None, week: Optional[int] = None):
    """Every contest with at least one of your entries. See
    sim_replay_store.list_my_contests -- cached by file mtime."""
    return {"contests": sim_replay_store.list_my_contests(year, week)}


@app.get("/api/sim_replay/entries")
def get_sim_replay_entries(year: int, week: int, slate_id: str, contest_name: str):
    """Your entries in one contest: real lineup + actual points/rank plus
    our sim's projected score distribution for that exact lineup."""
    return sim_replay_store.get_contest_entries(year, week, slate_id, contest_name)


@app.get("/api/sim_replay/field_stats")
def get_sim_replay_field_stats(
    year: int, week: int, slate_id: str, contest_name: str,
    contest_type: str = "top_heavy", paying_positions: Optional[int] = None, top_pct: float = 1.0,
):
    """Solver-style Sim ROI/Cash Rate/Ceiling/Floor/Top1%/histogram for your
    entries plus the real top `top_pct`% of the field -- rescores every real
    entrant's real roster with our sim and ranks them against each other (the
    field this contest actually had, not a synthetic one). Capped at
    sim_replay_store.MAX_FIELD_SIZE_FOR_RESCORE entries for now."""
    return sim_replay_store.get_contest_field_stats(
        year, week, slate_id, contest_name, contest_type, paying_positions, top_pct)


@app.get("/api/rosters")
def get_rosters(away: str, home: str, year: int = 2026, week: Optional[int] = None,
                 draft_group_id: Optional[int] = None):
    """Serves team rosters, base DNA, and live DraftKings (or synthetic
    fallback) salaries for a matchup. Pass `week` to read the DFS-week
    traits tree (data/current_rosters/dfs/, gameday-adjusted usage) instead
    of the season-long one -- see POST /api/dfs/roster_status."""
    away = away.strip().upper()
    home = home.strip().upper()
    reload_cache_if_changed()
    team_coaches = load_json(TEAM_COACHES_PATH)
    coach_dna_atlas = load_json(COACH_DNA_PATH)

    # Find game_id (and this matchup's week, for salary resolution below)
    # from schedule to query cached sims
    game_id = None
    match = None
    if os.path.exists(SCHEDULE_CSV_PATH):
        try:
            sched_df = pd.read_csv(SCHEDULE_CSV_PATH)
            match = sched_df[((sched_df["away_team"] == away) & (sched_df["home_team"] == home)) |
                             ((sched_df["away_team"] == home) & (sched_df["home_team"] == away))]
            if not match.empty:
                game_id = match.iloc[0]["game_id"]
        except Exception as e:
            print(f"Error finding game_id for rosters: {e}")

    # Routed through _resolve_dk_pool (prelock snapshot first) rather than a
    # direct get_dk_salaries(draft_group_id=...) call -- see run_simulation's
    # salary lookup fix for why even a caller-supplied, once-correct
    # draft_group_id can't be trusted for a past week once DK repoints it.
    dk = _resolve_dk_pool(match if match is not None else pd.DataFrame(), year, draft_group_id)

    result = {}
    for team in [away, home]:
        dfs_week_path = os.path.join(BASE_DIR, "data", "current_rosters", "dfs", f"{team}_traits_{year}.json")
        season_path = os.path.join(BASE_DIR, "data", "current_rosters", f"{team}_traits_{year}.json")
        use_dfs_week = week is not None and os.path.exists(dfs_week_path)
        roster_path = dfs_week_path if use_dfs_week else season_path
        if not os.path.exists(roster_path):
            raise HTTPException(status_code=404, detail=f"Roster traits not found for team {team}.")

        roster_data = load_json(roster_path)
        # Shallow copy — load_json() is memoized (see its docstring), so this
        # must not mutate the shared cached dict via the del below.
        traits = dict(roster_data.get("traits", {}))

        # Keep only the starting QB and discard backups to prevent UI clutter and
        # split stats -- skipped in DFS-week mode (week= passed) so the gameday
        # injury-toggle UI can see every QB to pick a starter from. See
        # POST /api/dfs/roster_status and game_engine.py's _get_starter_static,
        # which already prefers a `starter_override` flag over this same
        # total_attempts heuristic at sim time.
        if not use_dfs_week:
            qbs = [p for p, t in traits.items() if t.get("pos") == "QB"]
            if len(qbs) > 1:
                qb_dna = load_json(os.path.join(BASE_DIR, "data", "dna", "qb_dna.json"))
                starter_qb = max(qbs, key=lambda p: qb_dna.get(p, {}).get("total_attempts", 0))
                for qb in qbs:
                    if qb != starter_qb:
                        del traits[qb]
        
        # Load coach baseline details
        coach_name = team_coaches.get(team, "Unknown")
        coach_proe = coach_dna_atlas.get(coach_name, {}).get("proe", 0.0)

        # Load trench baseline pressure rate from trench_dna.json for year 2024
        pressure_rate = 0.30
        try:
            trench_dna_path = os.path.join(BASE_DIR, "data", "dna", "trench_dna.json")
            if os.path.exists(trench_dna_path):
                trench_dna_data = load_json(trench_dna_path)
                pressure_rate = trench_dna_data.get("2024", {}).get(team, {}).get("def_pressure_rate", 0.30)
        except Exception as e:
            print(f"Error loading pressure rate from trench_dna.json: {e}")
        
        # Load plays per game from the cached simulations (the sims) if available.
        # O(1) lookup via PLAYERS_BY_GAME_ID instead of scanning the full
        # ~11M-row players cache with a linear == comparison (this was the
        # dominant cost in get_rosters(), same anti-pattern as run_simulation's
        # cache-hit path — see PLAYERS_BY_GAME_ID's definition for details).
        team_plays = 63.5
        if game_id and game_id in PLAYERS_BY_GAME_ID:
            try:
                game_players = PLAYERS_BY_GAME_ID[game_id]
                gp = game_players[game_players["Team"] == team]
                if not gp.empty:
                    qb_patt = gp[gp["Pos"] == "QB"].groupby("Player")["pAtt"].mean().sum()
                    if pd.isna(qb_patt): qb_patt = 0.0
                    r_att_sum = gp.groupby("Player")["rAtt"].mean().sum()
                    if pd.isna(r_att_sum): r_att_sum = 0.0
                    sacks = gp[gp["Pos"] == "QB"].groupby("Player")["sacks_taken"].mean().sum() if "sacks_taken" in gp.columns else 0.0
                    if pd.isna(sacks): sacks = 0.0
                    team_plays = round(float(qb_patt + r_att_sum + sacks), 1)
                    if team_plays <= 0:
                        team_plays = 63.5
            except Exception as e:
                print(f"Error calculating team plays from cached sims: {e}")
        
        # Build team info
        team_info = {
            "coach": coach_name,
            "plays_per_game": team_plays,
            "def_pressure_rate": pressure_rate,
            "proe": coach_proe
        }
        
        # Load skill DNA catch rates
        rb_dna = load_json(os.path.join(BASE_DIR, "data", "dna", "rb_dna.json"))
        wr_dna = load_json(os.path.join(BASE_DIR, "data", "dna", "wr_dna.json"))
        te_dna = load_json(os.path.join(BASE_DIR, "data", "dna", "te_dna.json"))
        skill_dna = {}
        skill_dna.update(rb_dna)
        skill_dna.update(wr_dna)
        skill_dna.update(te_dna)
        if not skill_dna:
            skill_dna = load_json(os.path.join(BASE_DIR, "data", "dna", "skill_dna.json"))
        qb_dna = load_json(os.path.join(BASE_DIR, "data", "dna", "qb_dna.json"))
        
        players_list = []
        for name, p_traits in traits.items():
            pos = p_traits.get("pos", "WR/TE")
            target_share = p_traits.get("target_share", 0.0)
            carry_share = p_traits.get("carry_share", 0.0)
            
            # Fetch CPOE for QB salary scaling
            cpoe = 0.0
            if pos == "QB":
                cpoe = qb_dna.get(name, {}).get("cpoe", 0.0)
                
            catch_rate = skill_dna.get(name, {}).get("catch_rate", 0.65)
            
            # Generate baseline TD shares proportional to workloads
            rush_td_share = 0.0
            rec_td_share = 0.0
            
            if pos == "QB":
                # QBs can score rushing TDs baseline proportional to their carry share
                rush_td_share = carry_share
            elif pos == "RB":
                rush_td_share = carry_share
                rec_td_share = target_share
            elif pos in ["WR", "TE", "WR/TE"]:
                rec_td_share = target_share
            
            salary, _ = resolve_dk_salary(name, team, dk)

            players_list.append({
                "name": name,
                "pos": pos,
                "status": p_traits.get("status", "active"),
                "starter_override": bool(p_traits.get("starter_override", False)),
                "target_share": round(target_share * 100, 1),
                "carry_share": round(carry_share * 100, 1),
                "catch_rate": round(catch_rate * 100, 1),
                "td_share": round((rush_td_share + rec_td_share) * 100, 1),
                "rush_td_share": round(rush_td_share * 100, 1),
                "rec_td_share": round(rec_td_share * 100, 1),
                "salary": salary,
                "ownership_proj": 12.5
            })
            
        # NORMALIZE shares to sum to 100% (target_share, carry_share, rush_td_share, rec_td_share)
        non_dst_players = [p for p in players_list if p["pos"] != "DST"]
        
        tgt_sum = sum(p["target_share"] for p in non_dst_players if p["pos"] != "QB")
        rush_sum = sum(p["carry_share"] for p in non_dst_players)
        rush_td_sum = sum(p["rush_td_share"] for p in non_dst_players)
        rec_td_sum = sum(p["rec_td_share"] for p in non_dst_players)

        for p in players_list:
            if p["pos"] != "DST":
                if tgt_sum > 0 and p["pos"] != "QB":
                    p["target_share"] = round((p["target_share"] / tgt_sum) * 100, 1)
                if rush_sum > 0:
                    p["carry_share"] = round((p["carry_share"] / rush_sum) * 100, 1)
                if rush_td_sum > 0:
                    p["rush_td_share"] = round((p["rush_td_share"] / rush_td_sum) * 100, 1)
                if rec_td_sum > 0:
                    p["rec_td_share"] = round((p["rec_td_share"] / rec_td_sum) * 100, 1)
                
                # Keep legacy td_share as sum of both
                p["td_share"] = round(p["rush_td_share"] + p["rec_td_share"], 1)

        result[team] = {
            "team_settings": team_info,
            "roster": sorted(players_list, key=lambda x: x["salary"] if x["salary"] is not None else -1, reverse=True)
        }

    return result

# -------------------------------------------------------------------------
# DFS GAMEDAY INJURY / STARTER TOGGLE (2026-09-19)
#
# Lets the Simulator page mark a player active/inactive, or flip which QB is
# starting, for one specific week/game -- without Cam hand-editing a CSV or
# running scripts from a terminal. Scope is DFS-week-only: writes go to
# data/overrides/2026/week_NN/{TEAM}.csv's dfs_status/starter_override
# columns (see week_roster_v_0_1_0.py), never season_long/{TEAM}.csv, so a
# gameday toggle can never be confused with a real season-long injury move.
#
# Flow: POST persists the toggle + recomputes that team's week sheet
# (synchronous, fast -- CSV I/O only), then kicks off a BackgroundTasks job
# that recompiles the DFS traits JSON and re-simulates just the affected
# game (10,000 iterations, ~2 min per Cam's preference -- no speed/fidelity
# tradeoff wanted). The frontend polls the GET job-status endpoint. In-memory
# job dict is fine here: single-user local tool, and a job lost to a server
# restart is self-evident (poll returns "idle", Cam just retries) -- but
# that assumes start_backend_api.bat's uvicorn --reload doesn't itself
# restart mid-job from the CSV/JSON/parquet writes below, hence
# --reload-dir src there.
# -------------------------------------------------------------------------
DFS_ROSTER_TOGGLE_JOBS: Dict[str, dict] = {}


class RosterStatusChange(BaseModel):
    player_name: str
    action: Literal["set_active", "set_inactive", "set_starter"]


class RosterStatusRequest(BaseModel):
    game_id: str
    # Keyed by team abbreviation -- both away and home may carry changes in
    # one request (e.g. a full inactives list touching both sides of a game)
    # so they compile and resim together as ONE job instead of two racing,
    # 409-colliding POSTs for the same game_id.
    changes: Dict[str, List[RosterStatusChange]]


def _week_sheet_path(week: int, team: str) -> str:
    return os.path.join(BASE_DIR, "data", "overrides", "2026", f"week_{week:02d}", f"{team}.csv")


def _apply_roster_toggle_changes(week: int, team: str, changes: List[RosterStatusChange]) -> list:
    """Inputs: week (int, the DFS week being toggled), team (str abbr),
    changes (list of RosterStatusChange from the Game Explorer UI).
    Output: resolve_week_rows' report list for `week` (share redistribution).

    Purpose: persist the toggles and recompute that team's week sheet FRESH
    from season_long/{team}.csv -- not from the already-redistributed
    week-sheet numbers, so toggling a player back to Active is a clean undo
    instead of compounding a previous redistribution bump (confirmed with
    Cam 2026-09-19: season_long is the sole baseline for a toggle-triggered
    recompute).

    2026-09-22 -- sticky toggles: set_active/set_inactive are recorded in
    the persistent ledger (scripts/roster_management/dfs_status_ledger.py)
    at `week`, so they carry forward to later weeks and survive sheet
    rebuilds. set_active on a player sitting on a reserve slot (IR/PUP/...)
    now overrides that slot ("force_active"); cut/left_team/retired players
    stay out. set_starter stays week-only, as before. Any LATER week sheet
    that already exists for this team is rebuilt too, so it picks up the
    new sticky status instead of holding a stale one."""
    season_path = os.path.join(BASE_DIR, "data", "overrides", "2026", "season_long", f"{team}.csv")
    if not os.path.exists(season_path):
        raise HTTPException(status_code=404, detail=f"No season-long override sheet for {team}.")
    season_cols, season_rows = roster_read_rows(season_path)
    season_by_key = {roster_match_key(r["player_name"]): r for r in season_rows if r.get("player_name")}
    out_cols = list(season_cols) + [c for c in ("dfs_status", "starter_override") if c not in season_cols]

    # 1. Active/inactive -> sticky ledger (the source of truth for these).
    ledger = load_dfs_ledger(2026, BASE_DIR)
    for change in changes:
        if change.action in ("set_active", "set_inactive"):
            record_dfs_toggle(ledger, team, change.player_name, week,
                              "active" if change.action == "set_active" else "out",
                              season_by_key.get(roster_match_key(change.player_name)))
    save_dfs_ledger(ledger, 2026, BASE_DIR)

    # 2. Rebuild this week's sheet: season_long base + existing week-only
    #    columns + ledger statuses, then resolve redistribution.
    week_path = _week_sheet_path(week, team)
    new_rows, report = build_team_week_rows(
        season_rows, week, existing_week_path=week_path if os.path.exists(week_path) else None,
        ledger=ledger, team=team)

    # 3. Starter flips are week-only. At most one starter per team -- clear
    #    every other player's flag first (mirrors set_qb_starter_overrides'
    #    clear-then-set convention). resolve_week_rows never touches this
    #    column, so applying it after the resolve is equivalent.
    starter_keys = [roster_match_key(c.player_name) for c in changes if c.action == "set_starter"]
    if starter_keys:
        for r in new_rows:
            if r.get("player_name"):
                r["starter_override"] = "TRUE" if roster_match_key(r["player_name"]) == starter_keys[-1] else ""

    os.makedirs(os.path.dirname(week_path), exist_ok=True)
    roster_write_rows(week_path, out_cols, new_rows)

    # 4. Propagate the sticky status into any later week sheet already built.
    for later in range(week + 1, 19):
        later_path = _week_sheet_path(later, team)
        if os.path.exists(later_path):
            later_rows, _ = build_team_week_rows(season_rows, later, existing_week_path=later_path,
                                                 ledger=ledger, team=team)
            roster_write_rows(later_path, out_cols, later_rows)
    return report


def _run_roster_toggle_job(week: int, teams: List[str], away: str, home: str, game_id: str):
    try:
        # The whole job runs under the sim-run marker (2026-09-23), not just
        # resim_games' own: the roster compile below bumps the week's version
        # token first, and without the marker the site's sim-status poll would
        # start a multi-minute week_sim_results rebuild on half-updated inputs.
        with sim_run_marker(week, iterations=10000, games=[f"{away}@{home}"], base_dir=BASE_DIR):
            for team in teams:
                apply_team_week_overrides(week, team)
            resim_games(week, [(away, home)], iterations=10000)
        DFS_ROSTER_TOGGLE_JOBS[game_id] = {
            "status": "done",
            "finished_at": time.time(),
        }
    except Exception as e:  # noqa: BLE001 -- surface any failure to the poller, don't crash the background task silently
        DFS_ROSTER_TOGGLE_JOBS[game_id] = {"status": "error", "message": str(e)}


@app.post("/api/dfs/roster_status")
def post_roster_status(req: RosterStatusRequest, background_tasks: BackgroundTasks):
    """Toggle one or more players active/inactive/starter for `req.game_id`
    (on either or both teams -- see RosterStatusRequest), then re-simulate
    just that game in the background as ONE job. Poll
    GET /api/dfs/roster_status/job?game_id=... for completion."""
    if not os.path.exists(SCHEDULE_CSV_PATH):
        raise HTTPException(status_code=404, detail="Schedule not found.")
    sched_df = pd.read_csv(SCHEDULE_CSV_PATH)
    match = sched_df[sched_df["game_id"] == req.game_id]
    if match.empty:
        raise HTTPException(status_code=404, detail=f"No schedule entry for game_id {req.game_id}.")
    row = match.iloc[0]
    week, away, home = int(row["week"]), row["away_team"], row["home_team"]

    teams = [t.strip().upper() for t in req.changes.keys()]
    for team in teams:
        if team not in (away, home):
            raise HTTPException(status_code=400, detail=f"{team} is not in game {req.game_id} ({away} @ {home}).")
    if not teams:
        raise HTTPException(status_code=400, detail="No changes provided.")

    existing = DFS_ROSTER_TOGGLE_JOBS.get(req.game_id)
    if existing and existing.get("status") == "running":
        raise HTTPException(status_code=409, detail="A resim is already running for this game.")

    for team, changes in req.changes.items():
        _apply_roster_toggle_changes(week, team.strip().upper(), changes)

    DFS_ROSTER_TOGGLE_JOBS[req.game_id] = {"status": "running", "week": week, "teams": teams, "started_at": time.time()}
    background_tasks.add_task(_run_roster_toggle_job, week, teams, away, home, req.game_id)
    return {"game_id": req.game_id, "status": "running"}


@app.get("/api/dfs/roster_status/job")
def get_roster_status_job(game_id: str):
    return DFS_ROSTER_TOGGLE_JOBS.get(game_id, {"status": "idle"})


# -------------------------------------------------------------------------
# POSITIONAL EVALUATOR (Chess-style KEP / EP)
# -------------------------------------------------------------------------
@app.get("/api/positional-evaluator")
def positional_evaluator(
    down: int = Query(1, ge=1, le=4),
    distance: int = Query(10, ge=1, le=99),
    yardline_100: int = Query(75, ge=1, le=99),
    clock: int = Query(1800, ge=0, le=3600, description="Game seconds remaining"),
    score_differential: int = Query(0, description="Offense score - defense score"),
    posteam_timeouts: int = Query(3, ge=0, le=3),
    defteam_timeouts: int = Query(3, ge=0, le=3),
    off_team: str = Query(DEFAULT_OFF_TEAM),
    def_team: str = Query(DEFAULT_DEF_TEAM),
    n_sims: int = Query(1000, ge=100, le=5000),
):
    """
    Evaluates a single live game state and ranks first-play tactical concepts.

    Returns ep (situational Expected Points), kep (Kickoff-Equivalent Points), and
    a per-concept (Run/Screen/Short/Medium/Deep) KEP delta from a drive-long rollout.
    """
    game_state = {
        "down": down,
        "ydstogo": distance,
        "yardline_100": yardline_100,
        "game_seconds_remaining": clock,
        "score_differential": score_differential,
        "posteam_timeouts_remaining": posteam_timeouts,
        "defteam_timeouts_remaining": defteam_timeouts,
    }
    try:
        evaluator = get_positional_evaluator()
        res = evaluator.evaluate(
            game_state,
            off_team=off_team.strip().upper(),
            def_team=def_team.strip().upper(),
            n_sims=n_sims,
        )
    except FileNotFoundError as e:
        # Roster/DNA file missing for a requested team.
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Positional evaluation failed: {e}")

    return {
        "state": game_state,
        "off_team": off_team.strip().upper(),
        "def_team": def_team.strip().upper(),
        "ep": round(res["ep_start"], 3),
        "kep": round(res["kep_start"], 3),
        "efsd": round(res["efsd_start"], 3),
        "n_sims": res["n_sims"],
        "drive_end_rate": round(res["drive_end_rate"], 3),
        "concepts": [
            {
                "concept": c,
                "mean_kep": None if res["concepts"][c]["mean_kep"] is None else round(res["concepts"][c]["mean_kep"], 3),
                "delta_kep": None if res["concepts"][c]["delta_kep"] is None else round(res["concepts"][c]["delta_kep"], 3),
                "mean_efsd": None if res["concepts"][c]["mean_efsd"] is None else round(res["concepts"][c]["mean_efsd"], 3),
                "delta_efsd": None if res["concepts"][c]["delta_efsd"] is None else round(res["concepts"][c]["delta_efsd"], 3),
                "n": res["concepts"][c]["n"],
            }
            for c in ["Run", "Screen", "Short", "Medium", "Deep"]
        ],
        "excluded": res["excluded"],
    }


@app.get("/api/games/{game_id}/positional-eval")
def game_positional_eval(game_id: str):
    """
    Returns the sequence of per-play KEP/EFSD/EP evaluations for a live or recent game.

    Fetches the ESPN play-by-play stream, parses every scrimmage play into a game
    state, and computes KEP (clock-aware positional security), EFSD (expected final
    score differential), and EP (situational field value) for each. This is a
    lightweight per-play lookup — NOT a per-play drive simulation — so it stays
    fast across a full game.
    """
    from src.live.main import get_game_pbp
    from src.live.espn_adapter import parse_plays_to_states

    pbp = get_game_pbp(game_id)
    if not pbp:
        raise HTTPException(status_code=502, detail=f"Could not fetch play-by-play for game {game_id} from ESPN.")

    home_abbr, away_abbr = _extract_team_abbrs(pbp)
    states = parse_plays_to_states(_flatten_pbp_plays(pbp), home_abbr=home_abbr, away_abbr=away_abbr, team_id_map=_extract_team_id_map(pbp))

    evaluator = get_positional_evaluator()
    evals = []
    for s in states:
        wp_state = {
            "score_differential": s["score_differential"],
            "game_seconds_remaining": s["game_seconds_remaining"],
            "down": s["down"],
            "ydstogo": s["ydstogo"],
            "yardline_100": s["yardline_100"],
            "posteam_timeouts_remaining": 3,  # ESPN pbp does not expose timeouts; assume full
            "defteam_timeouts_remaining": 3,
            "receive_2h_ko": 0.0,
        }
        kep = evaluator.kep.kep_from_state(wp_state)
        efsd = evaluator.efsd_model.predict_efsd(wp_state)
        goal_to_go = 1 if s["ydstogo"] >= s["yardline_100"] else 0
        ep = evaluator.ep_model.predict_expected_points(
            yardline_100=int(s["yardline_100"]),
            down=s["down"],
            ydstogo=s["ydstogo"],
            goal_to_go=goal_to_go,
        )
        evals.append({
            "play_id": s["play_id"],
            "qtr": s["qtr"],
            "clock": s["clock"],
            "game_seconds_remaining": s["game_seconds_remaining"],
            "off": s["off"],
            "def": s["def"],
            "down": s["down"],
            "ydstogo": s["ydstogo"],
            "yardline_100": s["yardline_100"],
            "score_differential": s["score_differential"],
            "kep": round(kep, 3),
            "efsd": round(efsd, 3),
            "ep": round(ep, 3),
            "text": s["text"],
        })

    return {
        "game_id": game_id,
        "home_team": home_abbr,
        "away_team": away_abbr,
        "n_plays": len(evals),
        "evaluations": evals,
    }


def _yardline_display(yardline_100, posteam):
    if yardline_100 is None:
        return "Own 25"
    y100 = int(float(yardline_100))
    return f"Opp {y100}" if y100 <= 50 else f"Own {100 - y100}"


def _leverage_label(home_wp, quarter):
    spread = abs(home_wp - 50.0)
    if quarter is not None and quarter >= 4 and spread < 15:
        return "Critical"
    if (quarter is not None and quarter >= 4) or spread < 15:
        return "High"
    if quarter == 3:
        return "Medium"
    return "Low"


def _extract_team_abbrs(pbp):
    """Pulls home/away team abbreviations from an ESPN summary payload's header."""
    home_abbr, away_abbr = "", ""
    try:
        competitions = (pbp.get("header", {}) or {}).get("competitions", []) or []
        if competitions:
            for c in competitions[0].get("competitors", []) or []:
                side = c.get("homeAway")
                abbr = (c.get("team", {}) or {}).get("abbreviation", "")
                if side == "home":
                    home_abbr = abbr
                elif side == "away":
                    away_abbr = abbr
    except Exception as e:
        print(f"Error extracting team abbreviations: {e}")
    return home_abbr, away_abbr


def _extract_team_id_map(pbp):
    """{ESPN numeric team id: abbreviation} from an ESPN summary payload's
    header -- see espn_adapter.build_team_id_map / resolve_posteam_id for
    why this is needed (no-snap plays like Timeout/FG-good omit the
    play-level `team` field, but carry a reliable `start.team.id`)."""
    from src.live.espn_adapter import build_team_id_map
    try:
        competitions = (pbp.get("header", {}) or {}).get("competitions", []) or []
        competitors = competitions[0].get("competitors", []) if competitions else []
        return build_team_id_map(competitors)
    except Exception as e:
        print(f"Error extracting team id map: {e}")
        return {}


def _flatten_pbp_plays(pbp):
    """
    Flattens an ESPN summary payload's previous + current drives into one play
    list, deduplicated by play id.

    On a truly live game, ESPN can momentarily list the drive-boundary play in
    both "previous" (the drive that just ended) and "current" (before the feed
    has rolled the pointer to the new drive) -- confirmed 2026-08-20 against a
    real in-progress game. Dedup here so every consumer downstream (positional
    eval, play-by-play, stats, fourth-downs) gets a clean, unique play list.
    """
    drives = pbp.get("drives", {}) or {}
    plays = []
    seen_ids = set()
    for d in drives.get("previous", []) or []:
        plays.extend(d.get("plays", []) or [])
    current = drives.get("current", {}) or {}
    if current.get("plays"):
        plays.extend(current.get("plays", []) or [])

    deduped = []
    for p in plays:
        pid = p.get("id")
        if pid in seen_ids:
            continue
        seen_ids.add(pid)
        deduped.append(p)
    return deduped


@app.get("/api/games/{game_id}/play-by-play")
def game_play_by_play(game_id: str):
    """
    Returns per-play win probability for a live or recent game, driving the
    Game Center tab's WP graph and play log.

    Reuses the same ESPN pbp fetch + play-state parser as the positional-eval
    endpoint above, then runs each state through the shared WP model (the
    same model /api/live-games and the 4th-down bot use) to get home/away WP.
    """
    from src.live.main import get_game_pbp
    from src.live.espn_adapter import parse_plays_to_states
    from src.live.decision_engine import predict_win_probability

    pbp = get_game_pbp(game_id)
    if not pbp:
        raise HTTPException(status_code=502, detail=f"Could not fetch play-by-play for game {game_id} from ESPN.")

    home_abbr, away_abbr = _extract_team_abbrs(pbp)
    states = parse_plays_to_states(_flatten_pbp_plays(pbp), home_abbr=home_abbr, away_abbr=away_abbr, team_id_map=_extract_team_id_map(pbp))

    result = []
    for s in states:
        wp_state = {
            "down": s["down"],
            "ydstogo": s["ydstogo"],
            "yardline_100": s["yardline_100"],
            "game_seconds_remaining": s["game_seconds_remaining"],
            "score_differential": s["score_differential"],
            "posteam_timeouts_remaining": 3,  # ESPN pbp does not expose timeouts; assume full
            "defteam_timeouts_remaining": 3,
        }
        posteam_wp = predict_win_probability(wp_state) * 100.0
        if s["off"] == home_abbr:
            home_wp, away_wp = posteam_wp, 100.0 - posteam_wp
        else:
            away_wp, home_wp = posteam_wp, 100.0 - posteam_wp

        result.append({
            "play_id": s["play_id"],
            "qtr": s["qtr"],
            "time": s["clock"],
            "game_seconds_remaining": s["game_seconds_remaining"],
            "desc": s["text"],
            "possession": s["off"],
            "home_wp": round(home_wp, 1),
            "away_wp": round(away_wp, 1),
        })

    _clamp_final_play_wp(result, pbp)
    return result


def _clamp_final_play_wp(result: List[Dict[str, Any]], pbp: Dict[str, Any]) -> None:
    """Overwrites the LAST play's home_wp/away_wp to 100/0 (winner) or
    50/50 (tie) when the game is actually final. The WP model's own
    end-of-game special case (win_probability_v_0_1_0/inference.py) is
    gated on game_seconds_remaining <= 0, but ESPN's per-play `clock` is
    the clock at the START of that play, not after it resolves -- so the
    final play almost never has exactly 0 seconds left and the raw
    (uncapped) model probability is what would otherwise display, e.g.
    95.6% instead of 100% for a team that already won. Mutates `result`
    in place; a no-op if the game isn't final or scores are missing."""
    if not result:
        return
    try:
        competitions = (pbp.get("header", {}) or {}).get("competitions", []) or []
        if not competitions:
            return
        status = (competitions[0].get("status", {}) or {}).get("type", {}) or {}
        if not status.get("completed"):
            return
        home_score = away_score = None
        for c in competitions[0].get("competitors", []) or []:
            side = c.get("homeAway")
            score = c.get("score")
            if side == "home":
                home_score = score
            elif side == "away":
                away_score = score
        home_score, away_score = int(home_score), int(away_score)
    except (TypeError, ValueError, KeyError):
        return

    if home_score > away_score:
        result[-1]["home_wp"], result[-1]["away_wp"] = 100.0, 0.0
    elif away_score > home_score:
        result[-1]["home_wp"], result[-1]["away_wp"] = 0.0, 100.0
    else:
        result[-1]["home_wp"], result[-1]["away_wp"] = 50.0, 50.0


@app.get("/api/games/{game_id}/stats")
def game_stats(game_id: str):
    """
    Returns team-level box score stats for the Game Center tab's stat table.

    First downs, total/pass/rush yards, and turnovers come straight from
    ESPN's boxscore (already fetched for the play-by-play endpoints above).
    EPA/play is NOT part of ESPN's boxscore -- there's no real per-play EPA
    source wired into this app -- so it's approximated here as the average
    per-play change in our own field-position EP model (positional_ep_v_0_1_0),
    attributed to whichever team ran each play. This is not the same figure
    nflfastR calls EPA; it's a same-app-primitives stand-in until a real
    play-level EPA source is wired in.
    """
    from src.live.main import get_game_pbp
    from src.live.espn_adapter import parse_plays_to_states

    pbp = get_game_pbp(game_id)
    if not pbp:
        raise HTTPException(status_code=502, detail=f"Could not fetch stats for game {game_id} from ESPN.")

    boxscore = pbp.get("boxscore", {}) or {}
    teams = boxscore.get("teams", []) or []
    if len(teams) < 2:
        raise HTTPException(status_code=404, detail=f"No boxscore stats available yet for game {game_id}.")

    def _stat(stats_list, name, default="0"):
        for s in stats_list:
            if s.get("name") == name:
                return s.get("displayValue", default)
        return default

    home_abbr, away_abbr = _extract_team_abbrs(pbp)
    states = parse_plays_to_states(_flatten_pbp_plays(pbp), home_abbr=home_abbr, away_abbr=away_abbr, team_id_map=_extract_team_id_map(pbp))

    evaluator = get_positional_evaluator()
    epa_sums = {home_abbr: 0.0, away_abbr: 0.0}
    epa_counts = {home_abbr: 0, away_abbr: 0}
    prev_ep, prev_off = None, None
    for s in states:
        goal_to_go = 1 if s["ydstogo"] >= s["yardline_100"] else 0
        ep = evaluator.ep_model.predict_expected_points(
            yardline_100=int(s["yardline_100"]), down=s["down"], ydstogo=s["ydstogo"], goal_to_go=goal_to_go,
        )
        if prev_ep is not None:
            # EP is always from the current play's offense perspective; flip the
            # prior play's "end" value if possession changed hands between plays.
            end_ep_prev_off = ep if s["off"] == prev_off else -ep
            if prev_off in epa_sums:
                epa_sums[prev_off] += end_ep_prev_off - prev_ep
                epa_counts[prev_off] += 1
        prev_ep, prev_off = ep, s["off"]

    def _epa_play(abbr):
        n = epa_counts.get(abbr, 0)
        return round(epa_sums[abbr] / n, 2) if n else 0.0

    def _team_row(team_bx, abbr):
        stats_list = team_bx.get("statistics", []) or []
        return {
            "team": abbr,
            "first_downs": _stat(stats_list, "firstDowns"),
            "total_yds": _stat(stats_list, "totalYards"),
            "pass_yds": _stat(stats_list, "netPassingYards"),
            "rush_yds": _stat(stats_list, "rushingYards"),
            "turnovers": _stat(stats_list, "turnovers"),
            "epa_play": _epa_play(abbr),
        }

    home_bx = next((t for t in teams if t.get("team", {}).get("abbreviation") == home_abbr), teams[0])
    away_bx = next((t for t in teams if t.get("team", {}).get("abbreviation") == away_abbr), teams[1])

    return {
        "home": _team_row(home_bx, home_abbr),
        "away": _team_row(away_bx, away_abbr),
    }


@app.get("/api/games/{game_id}/fourth-downs")
def game_fourth_downs(game_id: str):
    """
    Returns Go/Punt/FG win-probability analysis for every 4th down in a live
    or recent game, driving the Game Center tab's 4th Down Decisions view.

    Reuses the exact same parser (parse_plays_to_fd_rows) and decision engine
    (evaluate_fourth_down) as the live bot in src/live/main.py -- this is a
    read-only view of that same production pipeline, not a new model.
    """
    from src.live.main import get_game_pbp
    from src.live.espn_adapter import parse_plays_to_fd_rows
    from src.live.decision_engine import evaluate_fourth_down

    pbp = get_game_pbp(game_id)
    if not pbp:
        raise HTTPException(status_code=502, detail=f"Could not fetch play-by-play for game {game_id} from ESPN.")

    home_abbr, away_abbr = _extract_team_abbrs(pbp)
    team_map = {"home": home_abbr, "away": away_abbr}
    fd_rows = parse_plays_to_fd_rows(game_id, _flatten_pbp_plays(pbp), team_map, team_id_map=_extract_team_id_map(pbp))

    result = []
    for row in fd_rows:
        try:
            sim = evaluate_fourth_down(row)
        except Exception as e:
            print(f"Skipping 4th down eval for play {row['play_id']}: {e}")
            continue

        home_score = row["off_score"] if row["off"] == home_abbr else row["def_score"]
        away_score = row["off_score"] if row["off"] == away_abbr else row["def_score"]

        result.append({
            "play_id": row["play_id"],
            "desc": f"4th & {row['ydstogo']} at {_yardline_display(row['yardline_100'], row['off'])} | Q{row['qtr']} {row['clock']}",
            "actual": row["text"],
            "possession": row["off"],
            "home_score": home_score,
            "away_score": away_score,
            "recharts_data": [
                {"name": "GO", "wp": round((sim.get("go_for_it_ev") or 0) * 100, 1),
                 "success_rate": round((sim.get("fd_prob") or 0) * 100, 1), "label": "Go For It"},
                {"name": "PUNT", "wp": round((sim.get("punt_wp") or 0) * 100, 1),
                 "success_rate": 100.0, "label": "Punt"},
                {"name": "FG", "wp": round((sim.get("field_goal_ev") or 0) * 100, 1),
                 "success_rate": round((sim.get("fg_prob") or 0) * 100, 1), "label": "Field Goal"},
            ],
        })

    return result


@app.get("/api/games/{game_id}/player-stats")
def game_player_stats(game_id: str):
    """
    Returns individual player box score stats (passing/rushing/receiving) for
    a live or recent game, split by team, for the Game Center tab's
    individual stats table.

    Straight from ESPN's boxscore.players -- no computation, just filtered
    down to the categories relevant here and reshaped to {labels, rows}.
    """
    from src.live.main import get_game_pbp

    pbp = get_game_pbp(game_id)
    if not pbp:
        raise HTTPException(status_code=502, detail=f"Could not fetch player stats for game {game_id} from ESPN.")

    boxscore = pbp.get("boxscore", {}) or {}
    players = boxscore.get("players", []) or []
    if len(players) < 2:
        raise HTTPException(status_code=404, detail=f"No player stats available yet for game {game_id}.")

    home_abbr, away_abbr = _extract_team_abbrs(pbp)
    keep_categories = ("passing", "rushing", "receiving")

    def _team_players(block):
        abbr = block.get("team", {}).get("abbreviation", "")
        categories = {}
        for cat in block.get("statistics", []) or []:
            name = cat.get("name")
            if name not in keep_categories:
                continue
            rows = []
            for a in cat.get("athletes", []) or []:
                athlete = a.get("athlete", {}) or {}
                rows.append({
                    "name": athlete.get("displayName", ""),
                    "jersey": athlete.get("jersey", ""),
                    "stats": a.get("stats", []),
                })
            categories[name] = {"labels": cat.get("labels", []), "rows": rows}
        return {"team": abbr, "categories": categories}

    home_block = next((t for t in players if t.get("team", {}).get("abbreviation") == home_abbr), players[0])
    away_block = next((t for t in players if t.get("team", {}).get("abbreviation") == away_abbr), players[1])

    return {
        "home": _team_players(home_block),
        "away": _team_players(away_block),
    }


def _fetch_games_for_date(date_str, target_ct):
    """
    Fetches and shapes one date's ESPN scoreboard into the live-games schema.

    Inputs: date_str (YYYYMMDD or None for ESPN's default/today), target_ct
        (the Central-time date those games should fall on -- used to filter
        the scoreboard's rolling multi-day window down to just that day).
    Returns: None if the ESPN fetch itself failed, otherwise a list (possibly
        empty if that date had no games).
    """
    from src.live.main import fetch_scoreboard, get_game_pbp
    from src.live.espn_adapter import parse_plays_to_states
    from src.live.decision_engine import predict_win_probability
    from datetime import datetime as _dt
    from zoneinfo import ZoneInfo

    sb = fetch_scoreboard(dates=date_str)
    if not sb:
        return None

    games = []
    for ev in sb.get("events", []) or []:
        comp = (ev.get("competitions") or [{}])[0]
        status_state = ((comp.get("status") or {}).get("type") or {}).get("state", "").lower()

        # Scoreboard spans a rolling window (past + future days), not just
        # the requested day -- only surface games actually happening on
        # target_ct (Central time, matching the rest of the live-bot's
        # scheduling), unless one is already in progress (keep showing it
        # even if it crossed midnight UTC).
        ev_date_str = ev.get("date") or ""
        try:
            ev_date_utc = _dt.fromisoformat(ev_date_str.replace("Z", "+00:00"))
            try:
                ev_date_ct = ev_date_utc.astimezone(ZoneInfo("America/Chicago")).date()
            except Exception:
                ev_date_ct = ev_date_utc.date()
        except Exception:
            ev_date_ct = None
        if status_state != "in" and ev_date_ct != target_ct:
            continue

        home_abbr = away_abbr = ""
        home_score = away_score = 0
        for c in comp.get("competitors", []) or []:
            side = c.get("homeAway")
            abbr = (c.get("team") or {}).get("abbreviation", "")
            score = int(c.get("score") or 0)
            if side == "home":
                home_abbr, home_score = abbr, score
            elif side == "away":
                away_abbr, away_score = abbr, score

        quarter = None
        time_remaining = "Pregame"
        possession = None
        down = 1
        distance = 10
        yardline_100 = None
        home_wp = away_wp = 50.0

        if status_state == "in":
            quarter = int((comp.get("status", {}).get("period")) or 0) or None
            time_remaining = ((comp.get("status") or {}).get("displayClock")) or "?:??"
            pbp = get_game_pbp(str(ev.get("id")))
            if pbp:
                drives = pbp.get("drives", {}) or {}
                plays = []
                for d in drives.get("previous", []) or []:
                    plays.extend(d.get("plays", []) or [])
                current = drives.get("current", {}) or {}
                if current.get("plays"):
                    plays.extend(current.get("plays", []) or [])
                from src.live.espn_adapter import build_team_id_map
                team_id_map = build_team_id_map(comp.get("competitors", []) or [])
                states = parse_plays_to_states(plays, home_abbr=home_abbr, away_abbr=away_abbr, team_id_map=team_id_map)
                if states:
                    last = states[-1]
                    possession = last["off"]
                    down = last["down"]
                    distance = last["ydstogo"]
                    yardline_100 = last["yardline_100"]
                    try:
                        wp_state = {
                            "score_differential": last["score_differential"],
                            "game_seconds_remaining": last["game_seconds_remaining"],
                            "down": last["down"],
                            "ydstogo": last["ydstogo"],
                            "yardline_100": last["yardline_100"],
                            "posteam_timeouts_remaining": 3,
                            "defteam_timeouts_remaining": 3,
                        }
                        posteam_wp = predict_win_probability(wp_state) * 100.0
                        if possession == home_abbr:
                            home_wp, away_wp = posteam_wp, 100.0 - posteam_wp
                        else:
                            away_wp, home_wp = posteam_wp, 100.0 - posteam_wp
                    except Exception as e:
                        print(f"WP calc failed for {ev.get('id')}: {e}")
        elif status_state == "post":
            time_remaining = "Final"
            # A completed game's outcome is deterministic -- show 100% for
            # the actual winner (50/50 on a tie) instead of leaving the
            # pre-game default 50/50, which the "in" branch above never
            # overwrites for a game that's already final by the time this
            # endpoint is hit (e.g. the Home page's "Most Recent Games"
            # card after the fact).
            if home_score > away_score:
                home_wp, away_wp = 100.0, 0.0
            elif away_score > home_score:
                home_wp, away_wp = 0.0, 100.0
            else:
                home_wp, away_wp = 50.0, 50.0

        games.append({
            "game_id": str(ev.get("id")),
            "away_team": away_abbr,
            "home_team": home_abbr,
            "away_score": away_score,
            "home_score": home_score,
            "quarter": quarter,
            "time_remaining": time_remaining,
            "possession": possession,
            "down": down,
            "distance": distance,
            "yardline": _yardline_display(yardline_100, possession),
            "away_wp": round(away_wp, 1),
            "home_wp": round(home_wp, 1),
            "leverage": _leverage_label(home_wp, quarter),
        })

    return games


@app.get("/api/live-games")
def live_games(date: Optional[str] = None):
    """
    Returns real ESPN games for the homepage's "Live Matchup Feeds" section.

    Defaults to today. Pass ?date=YYYYMMDD (e.g. 20260814) to review a past
    day's slate instead -- lets us troubleshoot the bot's output against a
    completed day's games without waiting for the next live window.

    Pregame ("scheduled") games get sane placeholder situational fields
    (1st & 10, own 25, 50/50 WP) rather than nulls, so the frontend needs no
    changes to render them cleanly alongside in-progress games.

    No mock-data fallback: when ?date is omitted and today has no games
    (offseason day, bye gap between preseason slates, etc.), walks backward
    up to 21 days for the most recent date that actually had games and
    returns that day's slate instead, with is_fallback/fallback_date set on
    every entry so the frontend can label them honestly rather than passing
    stale games off as live. An explicit ?date with no games just returns []
    -- only the default "today" call auto-falls-back.
    """
    from datetime import datetime as _dt, timedelta

    if date:
        try:
            target_ct = _dt.strptime(date, "%Y%m%d").date()
        except ValueError:
            raise HTTPException(status_code=400, detail="date must be in YYYYMMDD format.")
    else:
        try:
            from zoneinfo import ZoneInfo
            target_ct = _dt.now(ZoneInfo("America/Chicago")).date()
        except Exception:
            target_ct = _dt.utcnow().date()

    games = _fetch_games_for_date(date, target_ct)
    if games is None:
        raise HTTPException(status_code=502, detail="Could not fetch scoreboard from ESPN.")

    if games or date:
        return games

    for days_back in range(1, 22):
        search_date = target_ct - timedelta(days=days_back)
        search_str = search_date.strftime("%Y%m%d")
        fallback_games = _fetch_games_for_date(search_str, search_date)
        if fallback_games:
            for g in fallback_games:
                g["is_fallback"] = True
                g["fallback_date"] = search_str
            return fallback_games

    return []


@app.get("/api/live-bot-feed")
def live_bot_feed(limit: int = 100):
    """
    Returns the live 4th-down bot's full evaluation history (posted and
    skipped alike) from src/live/state_store.py's evaluated_plays table —
    the review/testing surface for the bot's social-post decisions and
    underlying model outputs, independent of whether DRY_RUN posting
    actually reached Bluesky/Mastodon.
    """
    from src.live.state_store import get_recent_evaluated_plays

    rows = get_recent_evaluated_plays(limit=min(max(limit, 1), 500))
    return {"count": len(rows), "plays": rows}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# HISTORICAL TESTING LAB — Week 1 2025 endpoints
# -------------------------------------------------------------------------
_WEEK1_MODULE = None

def _get_week1():
    """Lazy-import the week1 module so nfl_data_py download happens on first request."""
    global _WEEK1_MODULE
    if _WEEK1_MODULE is None:
        from src.nfl_sim.week1_2025 import get_game_list, get_game_plays
        _WEEK1_MODULE = (get_game_list, get_game_plays)
    return _WEEK1_MODULE


@app.get("/api/historical/week1-2025")
def historical_week1_games():
    """Returns the 16 Week 1 2025 game records (metadata only, no play data)."""
    try:
        get_game_list, _ = _get_week1()
        return {"games": get_game_list()}
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to load schedule: {e}")


@app.get("/api/historical/plays/{game_id}")
def historical_game_plays(game_id: str):
    """
    Returns all scrimmage plays for a Week 1 2025 game with pre-computed home-KEP values.

    home_kep is positive when the home team has the positional advantage and negative
    when the away team leads — never flips sign based on possession (chess evaluation frame).
    """
    try:
        get_game_list, get_game_plays = _get_week1()
        evaluator = get_positional_evaluator()
        plays = get_game_plays(game_id, evaluator)
        if not plays:
            raise HTTPException(status_code=404, detail=f"No plays found for {game_id}")
        return {
            "game_id":   game_id,
            "home_team": plays[0]["home_team"],
            "away_team": plays[0]["away_team"],
            "n_plays":   len(plays),
            "plays":     plays,
        }
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to load plays: {e}")


@app.get("/api/historical/suggest-lines")
def historical_suggest_lines(
    down:               int   = Query(1, ge=1, le=4),
    distance:           int   = Query(10, ge=1, le=99),
    yardline_100:       int   = Query(75, ge=1, le=99),
    clock:              int   = Query(1800, ge=0, le=3600),
    score_differential: float = Query(0.0),
    posteam_timeouts:   int   = Query(3, ge=0, le=3),
    defteam_timeouts:   int   = Query(3, ge=0, le=3),
    off_team:           str   = Query(DEFAULT_OFF_TEAM),
    def_team:           str   = Query(DEFAULT_DEF_TEAM),
    n_sims:             int   = Query(150, ge=50, le=1000),
    depth:              int   = Query(2, ge=1, le=3),
    n_lines:            int   = Query(3, ge=1, le=5),
    metric:             str   = Query("kep", regex="^(kep|efsd)$"),
):
    """
    Returns n_lines suggested play sequences of the given depth (principal variations).
    Each line shows the best-concept call at each step and the expected trajectory
    (KEP or EFSD, selected by the metric parameter).

    metric: "kep" (default, Kickoff-Equivalent Points) or "efsd" (Expected Final Score Differential).
    """
    game_state = {
        "down": down, "ydstogo": distance, "yardline_100": yardline_100,
        "game_seconds_remaining": float(clock),
        "score_differential": float(score_differential),
        "posteam_timeouts_remaining": float(posteam_timeouts),
        "defteam_timeouts_remaining": float(defteam_timeouts),
        "receive_2h_ko": 0.0,
    }
    try:
        evaluator = get_positional_evaluator()
        result = evaluator.suggest_lines(
            game_state,
            off_team=off_team.strip().upper(),
            def_team=def_team.strip().upper(),
            n_sims=n_sims, depth=depth, n_lines=n_lines,
            metric=metric.lower(),
        )
        return result
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"suggest_lines failed: {e}")


# -------------------------------------------------------------------------
# MULTIPROCESSING INITIALIZER
# -------------------------------------------------------------------------
def worker_init(proe_overrides: Dict[str, float]):
    """Runs inside spawned child processes to inject coach PROE overrides."""
    from src.nfl_sim import proe_overlay_v_0_1_0
    proe_overlay_v_0_1_0._HISTORICAL_PROE.update(proe_overrides)

def _salary_sort_key(po: "PlayerOverride") -> int:
    """Sort key for picking a roster's highest-salary (presumed starter)
    PlayerOverride. A player with no live DK salary is either off the Main
    Slate or -- more often at this point in the preseason -- has already
    been cut and our roster traits files just haven't caught up yet (see
    data/dna/preseason_overrides_2026_notes.md); either way they were never
    going to be the real starter, so they sort lowest instead of raising
    when compared against a real salary via plain max(key=lambda x: x.salary).
    Self-resolving once rosters catch up before the season starts -- not a
    fix to the roster data itself, just a guard so a stale/cut entry can't
    crash starter detection."""
    return po.salary if po.salary is not None else -1


def _build_game_distribution(game_df: pd.DataFrame, away_team: str, home_team: str,
                              ref_total: Optional[float], ref_spread: Optional[float]) -> dict:
    """Per-iteration game-outcome distribution for the Simulator's "Score
    Distribution" panel (total-points histogram, score-differential histogram,
    a sparse joint grid for the heatmap) AND for conditional lineup re-scoring
    (GameDistribution.jsx's box-select -> ShowdownOptimizeRequest /
    OptimizeRequest `iteration_filter`, which maps a selected total/margin box
    back to the `raw.iteration` ids here). `margin` = away - home (positive =
    away ahead); the UI flips it for display.

    Self-contained off `game_df` (needs away_score/home_score/total, iteration,
    optional weight columns -- the same per-iteration game-level frame
    GAMES_BY_GAME_ID / _get_dfs_week_by_game_id slices carry) so it can run
    WITHOUT the full player-projection simulation pipeline -- shared by
    run_simulation() (which already has game_df in hand) and the lightweight
    GET /api/game_distribution (just this, for a UI that only needs the read
    on the game, not projections).
    """
    away_scores = game_df["away_score"].values
    home_scores = game_df["home_score"].values
    totals = game_df["total"].values
    away_diff = away_scores - home_scores
    weights = game_df["weight"].values if "weight" in game_df.columns else np.ones(len(game_df))

    _iters = game_df["iteration"].values if "iteration" in game_df.columns else np.arange(len(game_df))
    _weighted = bool("weight" in game_df.columns and not np.allclose(weights, 1.0))
    _w = weights if _weighted else np.ones(len(totals))
    _wsum = _w.sum() or 1.0

    # Fixed ranges with catch-all end buckets (Cam, 2026-09-22): past ~4
    # scores the tail is noise -- a 52-17 and a 41-14 are functionally the
    # same game -- and auto-ranging to the single most extreme sim squashed
    # the useful middle of every chart. Geometry stays uniform {lo, step, nb}
    # so the frontend maps value -> pixel the same way; the first/last bin
    # simply absorbs everything beyond it (`under_label`/`over_label`).
    #   total : "<10" | 10-11 | 12-13 | ... | 78-79 | "80+"   (2-pt bins)
    #   margin: "<=-28" | -27 | ... | +27 | ">=+28"           (1-pt bins, so
    #           the 3/7/10 key numbers show up as spikes instead of being
    #           blurred inside 4-6-pt bins)
    _t_lo, _t_step, _t_nb = 8.0, 2.0, 37          # bin 0 = [8,10) = "<10", bin 36 = [80,82) = "80+"
    _t_idx = np.clip(np.floor((totals - _t_lo) / _t_step).astype(int), 0, _t_nb - 1)
    _t_p = np.bincount(_t_idx, weights=_w, minlength=_t_nb) / _wsum

    MARGIN_CAP = 28
    _m_lo, _m_step, _m_nb = -MARGIN_CAP - 0.5, 1.0, 2 * MARGIN_CAP + 1   # bins centred on integers
    _m_idx = np.clip(np.rint(away_diff).astype(int), -MARGIN_CAP, MARGIN_CAP) + MARGIN_CAP
    _m_p = np.bincount(_m_idx, weights=_w, minlength=_m_nb) / _wsum

    # Joint heatmap uses COARSER 5x5-pt cells (Cam, 2026-09-22): the 1-pt x
    # 2-pt 1D bins spread ~1,000 sims over ~2,100 cells (mostly 0-3 sims
    # each -- unreadable confetti). 5 is the step that lines up with the same
    # catch-alls AND keeps 0 centred: total "<10" | 10-14 | ... | 75-79 |
    # "80+"; margin "<=-28" | ... | -7..-3 | -2..+2 | +3..+7 | ... | ">=+28".
    # Mirrored in GameDistribution.jsx's normalizeDist -- keep in sync.
    _jt_lo, _jt_step, _jt_nb = 5.0, 5.0, 16      # [5,10) = "<10", [80,85) = "80+"
    _jt_idx = np.clip(np.floor((totals - _jt_lo) / _jt_step).astype(int), 0, _jt_nb - 1)
    _jm_lo, _jm_step, _jm_nb = -MARGIN_CAP - 4.5, 5.0, 13   # [-32.5,-27.5) = "<=-28", [27.5,32.5) = ">=+28"
    _jm_idx = np.clip(np.floor((np.clip(np.rint(away_diff), -MARGIN_CAP, MARGIN_CAP) - _jm_lo) / _jm_step).astype(int),
                      0, _jm_nb - 1)
    _joint = np.zeros((_jm_nb, _jt_nb))
    np.add.at(_joint, (_jm_idx, _jt_idx), _w)
    _joint /= _wsum
    _cells = [[int(mi), int(ti), round(float(_joint[mi, ti]), 5)]
              for mi, ti in zip(*np.nonzero(_joint))]

    return {
        "n": int(len(game_df)),
        # Bin-layout version: 2 = catch-all ends + 5x5 joint (2026-09-22).
        # GameDistribution.jsx's normalizeDist rebins anything older from `raw`.
        "layout": 2,
        "away_team": away_team,
        "home_team": home_team,
        "ref_total": round(float(ref_total), 1) if ref_total is not None else None,
        "ref_spread_home": round(float(ref_spread), 1) if ref_spread is not None else None,
        "mean_total": round(float(np.average(totals, weights=_w)), 1),
        "mean_margin_away": round(float(np.average(away_diff, weights=_w)), 1),
        "total": {"lo": _t_lo, "step": _t_step, "nb": _t_nb, "p": [round(float(x), 5) for x in _t_p],
                  "under_label": "<10", "over_label": "80+"},
        "margin": {"lo": _m_lo, "step": _m_step, "nb": _m_nb, "p": [round(float(x), 5) for x in _m_p],
                   "under_label": f"≤−{MARGIN_CAP}", "over_label": f"≥+{MARGIN_CAP}"},
        "joint": {"t_lo": _jt_lo, "t_step": _jt_step, "t_nb": _jt_nb,
                  "m_lo": _jm_lo, "m_step": _jm_step, "m_nb": _jm_nb, "cells": _cells,
                  "clamped": True},   # edge bins are open-ended -- see GameDistribution.jsx's selection
        "raw": {
            "iteration": [int(x) for x in _iters],
            "total": [int(round(x)) for x in totals],
            "margin": [int(round(x)) for x in away_diff],
            "weight": [round(float(x), 4) for x in _w] if _weighted else None,
        },
    }


@app.get("/api/game_distribution")
def get_game_distribution(away_team: str, home_team: str, week: Optional[int] = None, year: int = 2026):
    """Lightweight standalone game-outcome read: just _build_game_distribution
    off the cached per-iteration game data, no player-projection simulation.
    For embedding the Score Distribution panel somewhere that only has a
    game_id in hand (e.g. the showdown optimizer) without paying for a full
    /api/simulate call -- always returns the exact `raw` iteration arrays
    (unlike week_sim_results' binned-only game_distribution), so a box-select
    there gets real iteration ids for conditional lineup re-scoring immediately,
    no separate "run a fresh sim" step needed.

    Prefers the DFS-week parquet (data/interim/dfs_week_{N}_games.parquet) --
    reflects this week's real availability -- falling back to the season-long
    "everyone healthy" parquet (GAMES_BY_GAME_ID) when week is omitted or not
    yet DFS-simmed.
    """
    away_team, home_team = away_team.strip().upper(), home_team.strip().upper()
    game_id, ref_spread, ref_total = None, None, None
    if os.path.exists(SCHEDULE_CSV_PATH):
        sched_df = pd.read_csv(SCHEDULE_CSV_PATH)
        match = sched_df[(sched_df["away_team"] == away_team) & (sched_df["home_team"] == home_team)
                          & (sched_df["season"] == year)]
        if not match.empty:
            game_id = match.iloc[0]["game_id"]
            ref_spread = float(match.iloc[0]["spread_line"])
            ref_total = float(match.iloc[0]["total_line"])
    if game_id is None:
        raise HTTPException(status_code=404, detail=f'No {year} schedule entry for {away_team} @ {home_team}.')

    game_df = None
    if week is not None:
        dfs_by_game = _get_dfs_week_by_game_id(week)
        if dfs_by_game and game_id in dfs_by_game:
            game_df = dfs_by_game[game_id][0]  # (games_df_slice, players_df_slice)
    if game_df is None:
        game_df = GAMES_BY_GAME_ID.get(game_id)
    if game_df is None or game_df.empty:
        raise HTTPException(status_code=404, detail=f'No simulated game data for {away_team} @ {home_team} yet.')

    return _build_game_distribution(game_df, away_team, home_team, ref_total, ref_spread)


@app.post("/api/simulate")
def run_simulation(req: SimulationRequest):
    """Executes parallelized game simulations applying dynamic overlays."""
    req.away_team = req.away_team.strip().upper()
    req.home_team = req.home_team.strip().upper()
    reload_cache_if_changed()

    # Full-response cache: if this exact request (same teams, lines, overrides,
    # iterations) has already been computed, return it instantly instead of
    # re-running the (expensive, see below) post-processing. Any real change to
    # inputs produces a different key and computes fresh.
    cache_key = build_simulate_cache_key(req)
    if cache_key in SIMULATE_RESPONSE_CACHE:
        print(f"Simulate response cache hit for {req.away_team}@{req.home_team}.")
        return SIMULATE_RESPONSE_CACHE[cache_key]

    # 1. Initialize simulator to load baseline DNA/traits
    sim = BatchSimulator(team_off=req.away_team, team_def=req.home_team, year=req.year)

    # Check whether any player workload override (target/carry share, catch
    # rate) or team override (pace, pressure, PROE, etc.) actually differs from
    # this team's baseline roster/DNA values. Previously only custom Vegas
    # lines and a starter-QB/RB mismatch forced a fresh live simulation —
    # adjusting a workload slider silently had no effect when the cache hit,
    # since the cached data was generated with baseline shares and this
    # endpoint never re-checked them. Fixed here: any real override now forces
    # a live recompute so the slider actually does something.
    has_workload_overrides = False
    if req.use_cached_defaults:
        # Caller (e.g. /api/week_sim_results) built overrides from get_rosters()'s
        # rounded percentages, which won't reliably float-compare equal to the
        # unrounded internal roster values below — trust the flag instead.
        pass
    else:
        for po in req.player_overrides:
            base = sim.rosters.get(po.team, {}).get(po.name)
            if base is None:
                continue
            if (
                abs(po.target_share / 100.0 - base.get("target_share", 0.0)) > 1e-4
                or abs(po.carry_share / 100.0 - base.get("carry_share", 0.0)) > 1e-4
                or abs(po.catch_rate / 100.0 - base.get("catch_rate", base.get("catch_rate", 0.0))) > 1e-4
            ):
                has_workload_overrides = True
                break
        if not has_workload_overrides:
            for team, settings in req.team_overrides.items():
                coach_name = sim.team_coaches.get(team)
                coach_dna = sim.dna["coach"].get(coach_name, {}) if coach_name else {}
                trench = sim.dna["trench"].get(str(req.year), {}).get(team, {})
                if (
                    abs(settings.proe - coach_dna.get("proe", settings.proe)) > 1e-4
                    or abs(settings.def_pressure_rate - trench.get("def_pressure_rate", settings.def_pressure_rate)) > 1e-4
                ):
                    has_workload_overrides = True
                    break

    # 2. Check if pre-simulated cache contains this game
    game_df = None
    player_df = None
    
    # Determine the game_id from schedule if possible
    game_id = None
    baseline_spread = 0.0
    baseline_total = 45.0
    if os.path.exists(SCHEDULE_CSV_PATH):
        try:
            sched_df = pd.read_csv(SCHEDULE_CSV_PATH)
            match = sched_df[(sched_df["away_team"] == req.away_team) & 
                             (sched_df["home_team"] == req.home_team) & 
                             (sched_df["season"] == req.year)]
            if not match.empty:
                game_id = match.iloc[0]["game_id"]
                baseline_spread = float(match.iloc[0]["spread_line"])
                baseline_total = float(match.iloc[0]["total_line"])
        except Exception as e:
            print(f"Error checking schedule for game_id: {e}")

    # Check if lines are overridden (differing from schedule baselines)
    has_custom_lines = False
    if req.apply_weighting:
        if req.spread_override is not None and abs(req.spread_override - baseline_spread) > 1e-4:
            has_custom_lines = True
        if req.total_override is not None and abs(req.total_override - baseline_total) > 1e-4:
            has_custom_lines = True

    ref_spread = req.spread_override if req.spread_override is not None else baseline_spread
    ref_total = req.total_override if req.total_override is not None else baseline_total

    # Check if we should bypass cache (due to custom lines, starter mismatch,
    # or a genuine workload/team-setting override — see has_workload_overrides above)
    bypass_cache = has_custom_lines or has_workload_overrides

    # DFS-week fast path: serve straight from data/interim/dfs_week_{N}_*.parquet.
    # That sim already used this week's real availability (data/current_rosters/dfs/),
    # so it's authoritative by construction — skip the season-parquet
    # starter-mismatch check entirely (it would flag every injury/depth-chart
    # move and force a live re-sim, which is the whole cost we're avoiding).
    if req.use_dfs_week is not None and game_id and not has_custom_lines:
        dfs_by_game = _get_dfs_week_by_game_id(req.use_dfs_week)
        if dfs_by_game and game_id in dfs_by_game:
            g_slice, p_slice = dfs_by_game[game_id]
            game_df = g_slice.copy()
            player_df = p_slice.copy()
            bypass_cache = False
            print(f"Serving {game_id} from DFS-week {req.use_dfs_week} parquet (no live sim).")

    if game_df is None and not bypass_cache and game_id and ALL_PLAYERS_CACHED is not None:
        try:
            # O(1) lookup instead of scanning the full ~11M-row players cache
            cache_game_players = PLAYERS_BY_GAME_ID.get(game_id)
            if cache_game_players is not None and not cache_game_players.empty:
                for team in [req.away_team, req.home_team]:
                    team_qbs = cache_game_players[(cache_game_players["Team"] == team) & (cache_game_players["Pos"] == "QB")]
                    if not team_qbs.empty:
                        cache_starter = team_qbs.groupby("Player")["pAtt"].mean().idxmax()
                        req_team_qbs = [po for po in req.player_overrides if po.team == team and po.pos == "QB"]
                        if req_team_qbs:
                            req_starter = max(req_team_qbs, key=_salary_sort_key).name
                            def normalize_name(n):
                                return "".join(c for c in n.lower() if c.isalnum())
                            if normalize_name(cache_starter) != normalize_name(req_starter):
                                print(f"Starter QB mismatch for {team}: cache={cache_starter}, request={req_starter}. Bypassing cache.")
                                bypass_cache = True
                                break
                    team_rbs = cache_game_players[(cache_game_players["Team"] == team) & (cache_game_players["Pos"] == "RB")]
                    if not team_rbs.empty:
                        cache_starter_rb = team_rbs.groupby("Player")["rAtt"].mean().idxmax()
                        req_team_rbs = [po for po in req.player_overrides if po.team == team and po.pos == "RB"]
                        if req_team_rbs:
                            req_starter_rb = max(req_team_rbs, key=_salary_sort_key).name
                            def normalize_name(n):
                                return "".join(c for c in n.lower() if c.isalnum())
                            if normalize_name(cache_starter_rb) != normalize_name(req_starter_rb):
                                print(f"Starter RB mismatch for {team}: cache={cache_starter_rb}, request={req_starter_rb}. Bypassing cache.")
                                bypass_cache = True
                                break
        except Exception as e:
            print(f"Error validating cache starters: {e}")

    # Try loading from cache (skipped when the DFS-week fast path above already
    # populated game_df/player_df)
    if game_df is None and not bypass_cache and game_id and ALL_GAMES_CACHED is not None and ALL_PLAYERS_CACHED is not None:
        try:
            print(f"Attempting to load simulation data for {game_id} from memory cache...")
            if game_id in GAMES_BY_GAME_ID:
                # O(1) lookup instead of scanning the full games/players caches
                game_df = GAMES_BY_GAME_ID[game_id].copy()
                player_df = PLAYERS_BY_GAME_ID.get(game_id, pd.DataFrame()).copy()
                print(f"Successfully loaded simulation data for {game_id} from memory cache!")
        except Exception as e:
            print(f"Error querying memory cache: {e}. Falling back to live simulation.")
            game_df = None
            player_df = None

    if game_df is None or player_df is None:
        print(f"No cache hit for game_id: {game_id}. Running live simulations...")
        # 3. Apply team settings overrides in memory
        proe_overrides = {}
        for team, settings in req.team_overrides.items():
            coach_name = sim.team_coaches.get(team)
            if coach_name:
                proe_overrides[coach_name] = settings.proe

            # Pressure overrides
            if team in sim.dna["trench"].get(str(req.year), {}):
                sim.dna["trench"][str(req.year)][team]["def_pressure_rate"] = settings.def_pressure_rate
                sim.dna["trench"][str(req.year)][team]["sack_rate_allowed"] = settings.def_pressure_rate
                
        # 4. Apply player workload overrides in memory
        for po in req.player_overrides:
            team = po.team
            name = po.name
            
            if name in sim.rosters[team]:
                sim.rosters[team][name]["target_share"] = po.target_share / 100.0
                sim.rosters[team][name]["carry_share"] = po.carry_share / 100.0
                
            if name in sim.dna["skill"]:
                sim.dna["skill"][name]["catch_rate"] = po.catch_rate / 100.0

        # 4B. Dynamically override simulation starters based on active slate roster salaries
        for team in [req.away_team, req.home_team]:
            team_qbs = [po for po in req.player_overrides if po.team == team and po.pos == "QB"]
            if team_qbs:
                starter_qb = max(team_qbs, key=_salary_sort_key).name
                # Set starter QB's status to active and others on the roster to inactive
                for p_name, p_traits in sim.rosters[team].items():
                    if p_traits.get("pos") == "QB":
                        p_traits["status"] = "active" if p_name == starter_qb else "inactive"
                
            team_rbs = [po for po in req.player_overrides if po.team == team and po.pos == "RB"]
            if team_rbs:
                starter_rb = max(team_rbs, key=_salary_sort_key).name
                # Set starter RB's status to active and others on the roster to inactive
                for p_name, p_traits in sim.rosters[team].items():
                    if p_traits.get("pos") == "RB":
                        # We don't want to make all other RBs inactive since they might take carries, 
                        # but we can ensure they are marked appropriately. The engine selects starter 
                        # by carry share or total targets.
                        pass

        # 5. Set up multiprocessing initializer for PROE overrides
        import concurrent.futures
        max_workers = max(1, os.cpu_count() - 1)
        
        # Temporarily set the global executor with our custom initializer
        from src.nfl_sim import batch
        batch._GLOBAL_EXECUTOR = concurrent.futures.ProcessPoolExecutor(
            max_workers=max_workers,
            initializer=worker_init,
            initargs=(proe_overrides,)
        )

        try:
            # Run the batch simulation
            game_df, player_df = sim.run_batch(iterations=req.iterations)
        finally:
            # Shut down the temporary executor to release resources
            if getattr(batch, '_GLOBAL_EXECUTOR', None) is not None:
                batch._GLOBAL_EXECUTOR.shutdown()
                batch._GLOBAL_EXECUTOR = None

    # Unify live-sim and cache-sim column schemas (rename game_id -> iteration, add game_id, away_team, home_team, div_game)
    if game_df is not None and "iteration" not in game_df.columns:
        game_df = game_df.rename(columns={'game_id': 'iteration'})
        game_df['game_id'] = game_id
        game_df['away_team'] = req.away_team
        game_df['home_team'] = req.home_team
        game_df['div_game'] = 0
        
    if player_df is not None and "iteration" not in player_df.columns:
        player_df = player_df.rename(columns={'game_id': 'iteration'})
        player_df['game_id'] = game_id

    # -------------------------------------------------------------------------
    # DYNAMIC OVERLAY POST-PROCESSING (TD Share, Tempo/Pace)
    # -------------------------------------------------------------------------
    
    # Compute actual average simulated plays per team
    avg_plays = game_df["total_plays"].mean() / 2.0  # Apportioned plays per team
    if avg_plays <= 0 or np.isnan(avg_plays):
        avg_plays = 63.5
        
    # 5. Extract plays-per-game pace factors
    away_target_plays = req.team_overrides[req.away_team].plays_per_game if (req.team_overrides and req.away_team in req.team_overrides) else avg_plays
    home_target_plays = req.team_overrides[req.home_team].plays_per_game if (req.team_overrides and req.home_team in req.team_overrides) else avg_plays
    
    pace_factor_away = away_target_plays / avg_plays
    pace_factor_home = home_target_plays / avg_plays
    
    # Create player mappings for custom Rushing and Receiving TD shares
    rush_td_shares = {po.name: po.rush_td_share / 100.0 for po in req.player_overrides if po.rush_td_share is not None}
    rec_td_shares = {po.name: po.rec_td_share / 100.0 for po in req.player_overrides if po.rec_td_share is not None}

    # Vectorized pace and touchdown overlay processing
    # 1. Assign pace factors based on player team
    player_df["pace_factor"] = np.where(player_df["Team"] == req.away_team, pace_factor_away, pace_factor_home)
    
    # 2. Scale stats by pace factor
    vol_cols = ["pAtt", "pCmp", "pYds", "int", "rAtt", "rYds", "rTD", "recTD", "targets", "rec", "recYds", "fumbles", "sacks_taken"]
    for col in vol_cols:
        player_df[col] = player_df[col] * player_df["pace_factor"]
        
    player_df["is_qb"] = player_df["Pos"] == "QB"
    
    # 3. Handle Rushing TDs (QBs and skill players are eligible)
    team_game_r_tds = player_df.groupby(["iteration", "Team"])["rTD"].transform("sum")
    player_df["user_rush_td_share"] = player_df["Player"].map(rush_td_shares).fillna(0.0)
    team_user_rush_td_share_sum = player_df.groupby(["iteration", "Team"])["user_rush_td_share"].transform("sum")
    
    player_df["rush_workload"] = player_df["rAtt"]
    team_rush_workload_sum = player_df.groupby(["iteration", "Team"])["rush_workload"].transform("sum")
    
    player_df["final_rush_td_share"] = np.where(
        team_user_rush_td_share_sum > 0,
        player_df["user_rush_td_share"] / team_user_rush_td_share_sum,
        np.where(team_rush_workload_sum > 0, player_df["rush_workload"] / team_rush_workload_sum, 0.0)
    )
    # Reallocate rushing TDs
    player_df["rTD"] = team_game_r_tds * player_df["final_rush_td_share"]

    # 4. Handle Receiving TDs (Only non-QBs are eligible)
    player_df["rec_td_eligible"] = ~player_df["is_qb"]
    player_df["sim_rec_td"] = np.where(player_df["rec_td_eligible"], player_df["recTD"], 0.0)
    team_game_rec_tds_baseline = player_df.groupby(["iteration", "Team"])["sim_rec_td"].transform("sum")
    
    player_df["user_rec_td_share"] = np.where(player_df["rec_td_eligible"], player_df["Player"].map(rec_td_shares).fillna(0.0), 0.0)
    team_user_rec_td_share_sum = player_df.groupby(["iteration", "Team"])["user_rec_td_share"].transform("sum")
    
    player_df["rec_workload"] = np.where(player_df["rec_td_eligible"], player_df["targets"], 0.0)
    team_rec_workload_sum = player_df.groupby(["iteration", "Team"])["rec_workload"].transform("sum")
    
    player_df["final_rec_td_share"] = np.where(
        team_user_rec_td_share_sum > 0,
        player_df["user_rec_td_share"] / team_user_rec_td_share_sum,
        np.where(team_rec_workload_sum > 0, player_df["rec_workload"] / team_rec_workload_sum, 0.0)
    )
    # Reallocate receiving TDs
    player_df["recTD"] = team_game_rec_tds_baseline * player_df["final_rec_td_share"]
    
    player_df["touches"] = player_df["rAtt"] + player_df["rec"]
    
    # 8. Allocate passing touchdowns to QBs (sum of receiving TDs of all skill players on their team for that game)
    team_game_rec_tds = player_df.groupby(["iteration", "Team"])["recTD"].transform("sum")
    
    # Get total simulated passing attempts per team per game
    team_game_p_att = player_df.groupby(["iteration", "Team"])["pAtt"].transform("sum")
    
    # Allocate passing touchdowns to QBs proportional to their passing attempts (starter vs. backups)
    player_df["pTD"] = np.where(
        player_df["is_qb"] & (team_game_p_att > 0),
        team_game_rec_tds * (player_df["pAtt"] / team_game_p_att),
        np.where(player_df["is_qb"], 0.0, player_df["pTD"])
    )
    
    # 9. Vectorized calculation of DraftKings and FanDuel scores
    # DST's dk_score/fd_score already reflect its own sacks/INTs/fumble-
    # recoveries/defensive-TDs/points-allowed formula from game_engine.py
    # (see its "if pos == 'DST'" scoring block) -- saved off here so the
    # passing/rushing/receiving formula below (needed to reflect this
    # request's TD-share/pace overlays for skill players, applied just
    # above) doesn't overwrite it with a flat 0, since none of those
    # offensive categories apply to a Defense. Same fix as get_week_projections()'s
    # dst_mask for the identical reason.
    dst_mask = player_df["Pos"] == "DST"
    original_dst_dk_score = player_df["dk_score"].copy()
    original_dst_fd_score = player_df["fd_score"].copy()

    dk_p_yds = player_df["pYds"] * 0.04
    dk_p_td = player_df["pTD"] * 4
    dk_int = player_df["int"] * 1
    dk_p_bonus = np.where(player_df["pYds"] >= 300, 3.0, 0.0)
    dk_r_yds = player_df["rYds"] * 0.1
    dk_r_td = player_df["rTD"] * 6
    dk_r_bonus = np.where(player_df["rYds"] >= 100, 3.0, 0.0)
    dk_rec = player_df["rec"] * 1.0
    dk_rec_yds = player_df["recYds"] * 0.1
    dk_rec_td = player_df["recTD"] * 6
    dk_rec_bonus = np.where(player_df["recYds"] >= 100, 3.0, 0.0)
    dk_fumbles = player_df["fumbles"] * 1.0
    
    computed_dk_score = (dk_p_yds + dk_p_td - dk_int + dk_p_bonus +
                             dk_r_yds + dk_r_td + dk_r_bonus +
                             dk_rec + dk_rec_yds + dk_rec_td + dk_rec_bonus -
                             dk_fumbles).round(2)
    player_df["dk_score"] = np.where(dst_mask, original_dst_dk_score, computed_dk_score)
    
    fd_p_yds = player_df["pYds"] * 0.04
    fd_p_td = player_df["pTD"] * 4
    fd_int = player_df["int"] * 2
    fd_r_yds = player_df["rYds"] * 0.1
    fd_r_td = player_df["rTD"] * 6
    fd_rec = player_df["rec"] * 0.5
    fd_rec_yds = player_df["recYds"] * 0.1
    fd_rec_td = player_df["recTD"] * 6
    fd_fumbles = player_df["fumbles"] * 2.0
    
    computed_fd_score = (fd_p_yds + fd_p_td - fd_int +
                             fd_r_yds + fd_r_td +
                             fd_rec + fd_rec_yds + fd_rec_td -
                             fd_fumbles).round(2)
    player_df["fd_score"] = np.where(dst_mask, original_dst_fd_score, computed_fd_score)
    
    # Look up the baseline lines to check if user has adjusted them
    baseline_spread = 0.0
    baseline_total = 45.0
    if game_id is not None:
        try:
            sched_df = pd.read_csv(SCHEDULE_CSV_PATH)
            match = sched_df[sched_df["game_id"] == game_id]
            if not match.empty:
                baseline_spread = float(match.iloc[0]["spread_line"])
                baseline_total = float(match.iloc[0]["total_line"])
        except Exception as e:
            print(f"Error reading baseline lines: {e}")

    ref_spread = req.spread_override if req.spread_override is not None else baseline_spread
    ref_total = req.total_override if req.total_override is not None else baseline_total

    # Apply importance weights only if user has adjusted the lines from baseline
    has_custom_lines = False
    if req.apply_weighting:
        if req.spread_override is not None and abs(req.spread_override - baseline_spread) > 1e-4:
            has_custom_lines = True
        if req.total_override is not None and abs(req.total_override - baseline_total) > 1e-4:
            has_custom_lines = True

    if has_custom_lines:
        print(f"Calculating Importance Weights matching line: Spread {ref_spread}, Total {ref_total}")
        # ref_spread from schedule is home spread (home - away).
        # game_df["spread"] is away_score - home_score. So target for away is -ref_spread.
        margin_diff = game_df["spread"] - (-ref_spread)
        total_diff = game_df["total"] - ref_total
        
        sigma_spread = 6.0
        sigma_total = 10.0
        
        game_df["weight"] = np.exp(- (margin_diff**2) / (2 * sigma_spread**2) - (total_diff**2) / (2 * sigma_total**2))
        sum_weights = game_df["weight"].sum()
        if sum_weights > 0:
            game_df["weight"] = game_df["weight"] * (len(game_df) / sum_weights)
        else:
            game_df["weight"] = 1.0
    else:
        game_df["weight"] = 1.0

    weight_map = dict(zip(game_df["iteration"], game_df["weight"]))
    player_df["weight"] = player_df["iteration"].map(weight_map).fillna(1.0)
    updated_player_df = player_df

    # 6. Aggregate player results
    agg_player_df = StatAggregator.aggregate_player_stats(updated_player_df)

    # Build a lookup for salaries to include in the final response -- real DK
    # salary only (None when the player isn't live on the Main Slate), same
    # resolution as get_week_salaries()/get_rosters(), which this endpoint's
    # response is merged with client-side.
    #
    # Bug fixed 2026-09-16: this used to call get_dk_salaries() with no
    # draft_group_id, i.e. always DK's current live default slate -- correct
    # for a direct ad-hoc /api/simulate call (no week context at all), but
    # wrong for the /api/week_sim_results prepopulation path (req.use_dfs_week
    # set), which was resolving EVERY week's salaries against whatever slate
    # happens to be live right now. That's how Week 2's real prices (Chase
    # Brown, D.J. Moore, etc.) ended up in the Week 1 Optimizer's player pool
    # (allSimResults, which wins the salary merge over the correctly-resolved
    # /api/week_projections) even after _resolve_dk_pool was fixed elsewhere --
    # this call site never went through it. Now routes through the same
    # _resolve_dk_pool (prelock snapshot first, live fetch only as a fallback)
    # whenever a week is known.
    if req.use_dfs_week is not None:
        dk = _resolve_dk_pool(pd.DataFrame({"week": [req.use_dfs_week]}), req.year, None)
    else:
        dk = get_dk_salaries()
    salaries = {}
    for team in [req.away_team, req.home_team]:
        for name in sim.rosters[team]:
            dk_salary, _ = resolve_dk_salary(name, team, dk)
            salaries[name] = dk_salary

    # DST isn't in sim.rosters (it's synthesized by game_engine.py, not part
    # of the traits-file roster) and resolve_dk_salary() only matches against
    # dk["players"] (real skill-player names), so it can never resolve a
    # defense's price -- and both teams' defense rows share the literal
    # Player name "Defense" besides, so a plain salaries[name] lookup
    # couldn't tell them apart even if it could. Resolved separately here,
    # keyed by team, from the same dk["defense"] source get_week_salaries()
    # uses for /api/week_projections.
    defense_salary_by_team = {
        team: dk["defense"].get(team) for team in [req.away_team, req.home_team]
    }

    player_ownership = {po.name: po.ownership_proj for po in req.player_overrides if po.ownership_proj is not None}

    # Ground-truth position (from the traits file, via get_rosters() ->
    # PlayerOverride.pos) for the pos_clean fallback below -- the sim
    # engine's own "Slot" label (e.g. Harold Fannin, Tyler Warren, Greg
    # Dulcich all getting "WR1") is assigned by target-share/workload rank
    # among pass-catchers, not real position, so a true rookie/new TE who
    # leads a team's receiving snaps can get slotted as a "WR" even though
    # traits.json has always had them correctly as "TE". DST has no
    # PlayerOverride entry (see _compute_one_game), so it's absent here and
    # falls through to the Slot-derived guess, which is fine for DST.
    true_pos_by_name = {po.name: po.pos for po in req.player_overrides}
    
    # 5.5 Compute 101-value percentiles (0 to 100) for all player metrics
    pct_keys = [
        "dk_score", "fd_score", "rAtt", "rYds", "rTD", 
        "targets", "rec", "recYds", "recTD", 
        "pAtt", "pCmp", "pYds", "pTD", "int", "fumbles"
    ]
    pcts = np.linspace(0, 100, 101)
    player_percentiles = {}
    for player_name, group in updated_player_df.groupby("Player"):
        if has_custom_lines:
            w = group["weight"].values
            player_percentiles[player_name] = {
                col: [round(x, 2) for x in weighted_quantile(group[col].values, pcts / 100.0, sample_weight=w).tolist()]
                for col in pct_keys if col in group.columns
            }
        else:
            player_percentiles[player_name] = {
                col: np.percentile(group[col].values, pcts).round(2).tolist()
                for col in pct_keys if col in group.columns
            }

    # Calculate DFS Optimal, Boom, and Value rates trial-by-trial for simulated
    # game (Showdown) -- scoped to real-DK-priced players only, same reasoning
    # as the Classic-slate block in get_week_projections(). Boom/Value stay on
    # the Classic `salaries` resolved above (they're Classic-DK-value
    # concepts); the optimal-lineup solve just below needs its own
    # Showdown-specific pricing -- see the comment there.
    priced_names = [p for p in salaries if salaries[p] is not None]
    boom_counts = {p: 0 for p in priced_names}
    value_counts = {p: 0 for p in priced_names}

    unique_iterations = updated_player_df["iteration"].unique()
    num_iterations = len(unique_iterations) if len(unique_iterations) > 0 else 1

    # The embedded showdown-optimal-lineup solve below needs THIS game's real
    # Showdown salaries specifically, not the Classic Main Slate prices
    # `salaries` above resolves -- a Thursday/Monday game's own real slate is
    # a separate Showdown-only DK draft group, so every one of its players
    # prices None on the Classic feed, silently zeroing optimal_cpt_pct/
    # optimal_flex_pct for the whole game no matter how high
    # optimizer_sample_cap is set (confirmed live 2026-09-17 on DET@BUF, a
    # Thursday game -- still 0% for everyone after raising the cap to 1,000,
    # because the real bug was upstream of the solve, not the sample size).
    # Falls back to the Classic `salaries` dict when no live Showdown slate
    # is found for this matchup yet, rather than leaving the solve entirely
    # unpriced.
    showdown_dk = get_dk_showdown_salaries(away_team=req.away_team, home_team=req.home_team)
    if showdown_dk.get('found') and showdown_dk.get('players'):
        showdown_salaries = {p['name']: p['salary'] for p in showdown_dk['players'] if p.get('pos') != 'K'}
    else:
        showdown_salaries = salaries

    player_names = [p for p in showdown_salaries if showdown_salaries[p] is not None]
    optimal_counts = {p: 0 for p in player_names}
    optimal_cpt_counts = {p: 0 for p in player_names}
    optimal_flex_counts = {p: 0 for p in player_names}
    player_salaries = np.array([showdown_salaries[p] for p in player_names])

    # High-performance arrays extract to map score iterations
    up_players = updated_player_df["Player"].values
    up_iterations = updated_player_df["iteration"].values
    up_dk_scores = updated_player_df["dk_score"].values
    
    iter_scores = {it: {} for it in unique_iterations}
    for i in range(len(up_players)):
        p = up_players[i]
        it = up_iterations[i]
        score = up_dk_scores[i]
        iter_scores[it][p] = score
        
        # Calculate Boom % (score >= 30) and Value % (score >= 3x salary) --
        # value % is skipped, not zeroed, without a real salary to measure against.
        sal = salaries.get(p)
        if score >= 30.0:
            boom_counts[p] = boom_counts.get(p, 0) + 1
        if sal is not None and score >= 3.0 * (sal / 1000.0):
            value_counts[p] = value_counts.get(p, 0) + 1
            
    from src.nfl_sim.optimizer import solve_showdown_iteration
    # Sub-sample iterations if they exceed the cap to reduce CPU stress and
    # latency (same pattern as get_week_projections' optimal-lineup sampling,
    # now solve_optimal_lineup_milp there) -- this per-iteration branch-and-
    # bound solve, not the Monte Carlo sim itself, is what made the first
    # /api/simulate hit for a game take up to minutes). req.optimizer_sample_cap
    # defaults to 1,000 for direct user requests (see SimulationRequest's own
    # docstring); /api/week_sim_results sets it to the full 1,000 for its
    # once-per-week bulk prepopulation (no sub-sampling there at all).
    sample_cap = max(1, req.optimizer_sample_cap or 1000)
    solve_iterations = unique_iterations
    if len(unique_iterations) > sample_cap:
        step = len(unique_iterations) // sample_cap
        solve_iterations = unique_iterations[::step][:sample_cap]
    num_solve_iterations = len(solve_iterations) if len(solve_iterations) > 0 else 1

    for it in solve_iterations:
        scores_arr = np.array([iter_scores[it].get(p, 0.0) for p in player_names])
        opt_lineup = solve_showdown_iteration(player_names, player_salaries, scores_arr)
        if opt_lineup:
            cpt = opt_lineup[0]
            optimal_cpt_counts[cpt] = optimal_cpt_counts.get(cpt, 0) + 1
            for flex_player in opt_lineup[1:]:
                optimal_flex_counts[flex_player] = optimal_flex_counts.get(flex_player, 0) + 1
            for player_in_lineup in opt_lineup:
                optimal_counts[player_in_lineup] = optimal_counts.get(player_in_lineup, 0) + 1

    final_projections = []
    for idx, row in agg_player_df.iterrows():
        name = row["Player"]
        slot_pos = row["Slot"]
        team = row["Team"]
        
        # Prefer the traits-file ground-truth position when we have one --
        # see true_pos_by_name's comment above for why the Slot-derived
        # guess below can mislabel a real TE as "WR". Only DST has no
        # override entry, so it always falls through to the Slot guess.
        true_pos = true_pos_by_name.get(name)
        if true_pos in ("QB", "RB", "WR", "TE"):
            pos_clean = true_pos
        else:
            # Clean slot position (e.g. WR1 -> WR, TE2 -> TE, RB1 -> RB) to match frontend filters
            pos_clean = slot_pos
            for base_pos in ["QB", "RB", "WR", "TE", "DST"]:
                if slot_pos.startswith(base_pos):
                    pos_clean = base_pos
                    break
                
        salary = defense_salary_by_team.get(team) if pos_clean == "DST" else salaries.get(name)

        dk_pts = row["dk_score_avg"]
        fd_pts = row["fd_score_avg"]

        # None here (no per-request override) -- get_week_sim_results()
        # fills in the real slate-wide deterministic ownership afterward,
        # across every game's players at once (the softmax needs the whole
        # pool, not just this one game's two teams, to be comparable).
        own_val = player_ownership.get(name)

        p_pcts = player_percentiles.get(name, {})

        final_projections.append({
            "name": name,
            "pos": pos_clean,
            "team": team,
            "salary": salary,
            "dk_points": dk_pts,
            "fd_points": fd_pts,
            "dk_value": round(dk_pts / (salary / 1000.0), 2) if salary else 0.0,
            "fd_value": round(fd_pts / (salary / 1000.0), 2) if salary else 0.0,
            "ownership_proj": own_val,
            "ownership_leverage": round(dk_pts / own_val, 2) if own_val else 0.0,
            "optimal_pct": round((optimal_counts.get(name, 0) / num_solve_iterations) * 100.0, 2),
            "optimal_cpt_pct": round((optimal_cpt_counts.get(name, 0) / num_solve_iterations) * 100.0, 2),
            "optimal_flex_pct": round((optimal_flex_counts.get(name, 0) / num_solve_iterations) * 100.0, 2),
            "boom_pct": round((boom_counts.get(name, 0) / num_iterations) * 100.0, 2),
            "value_pct": round((value_counts.get(name, 0) / num_iterations) * 100.0, 2),
            "floor_dk_points": round(dk_pts * 0.45, 1),
            "ceiling_dk_points": round(dk_pts * 1.65, 1),
            "floor_fd_points": round(fd_pts * 0.45, 1),
            "ceiling_fd_points": round(fd_pts * 1.65, 1),
            "rAtt": row["rAtt_avg"],
            "rYds": row["rYds_avg"],
            "rTD": row["rTD_avg"],
            "targets": row["targets_avg"],
            "rec": row["rec_avg"],
            "recYds": row["recYds_avg"],
            "recTD": row["recTD_avg"],
            "pAtt": row["pAtt_avg"],
            "pCmp": row["pCmp_avg"],
            "pYds": row["pYds_avg"],
            "pTD": row["pTD_avg"],
            "int": row["int_avg"],
            "fumbles": row["fumbles_avg"],
            "sacks_taken": row.get("sacks_taken_avg", 0.0),
            # Percentiles arrays (101 values)
            "dk_pcts_all": p_pcts.get("dk_score", [dk_pts]*101),
            "fd_pcts_all": p_pcts.get("fd_score", [fd_pts]*101),
            "rAtt_pcts_all": p_pcts.get("rAtt", [row["rAtt_avg"]]*101),
            "rYds_pcts_all": p_pcts.get("rYds", [row["rYds_avg"]]*101),
            "rTD_pcts_all": p_pcts.get("rTD", [row["rTD_avg"]]*101),
            "targets_pcts_all": p_pcts.get("targets", [row["targets_avg"]]*101),
            "rec_pcts_all": p_pcts.get("rec", [row["rec_avg"]]*101),
            "recYds_pcts_all": p_pcts.get("recYds", [row["recYds_avg"]]*101),
            "recTD_pcts_all": p_pcts.get("recTD", [row["recTD_avg"]]*101),
            "pAtt_pcts_all": p_pcts.get("pAtt", [row["pAtt_avg"]]*101),
            "pCmp_pcts_all": p_pcts.get("pCmp", [row["pCmp_avg"]]*101),
            "pYds_pcts_all": p_pcts.get("pYds", [row["pYds_avg"]]*101),
            "pTD_pcts_all": p_pcts.get("pTD", [row["pTD_avg"]]*101),
            "int_pcts_all": p_pcts.get("int", [row["int_avg"]]*101),
            "fumbles_pcts_all": p_pcts.get("fumbles", [row["fumbles_avg"]]*101)
        })

        
    # 7. Compile game outcomes & probability metrics
    # off_score/def_score renamed to away_score/home_score (2026-07-21) --
    # see AGENTS.md's off_score/def_score fragile-area note.
    away_scores = game_df["away_score"].values
    home_scores = game_df["home_score"].values
    totals = game_df["total"].values
    away_diff = away_scores - home_scores
    weights = game_df["weight"].values if "weight" in game_df.columns else np.ones(len(game_df))
    sum_w = weights.sum()
    
    avg_away = np.average(away_scores, weights=weights) if sum_w > 0 else away_scores.mean()
    avg_home = np.average(home_scores, weights=weights) if sum_w > 0 else home_scores.mean()
    
    # Vegas comparison values (use overrides if provided)
    ref_spread = req.spread_override if req.spread_override is not None else baseline_spread
    ref_total = req.total_override if req.total_override is not None else baseline_total
    
    # ref_spread from schedule is home spread. Away spread is -ref_spread.
    # Away covers if away_diff > -ref_spread.
    away_covers = np.average(away_diff > -ref_spread, weights=weights) if sum_w > 0 else (away_diff > -ref_spread).mean()
    over_hits = np.average(totals > ref_total, weights=weights) if sum_w > 0 else (totals > ref_total).mean()
    
    win_away = np.average(away_diff > 0, weights=weights) if sum_w > 0 else (away_diff > 0).mean()
    win_home = 1.0 - win_away
    
    # 8. Compile score density bins for UI chart
    # Create 15 total-score ranges to show score likelihood distribution
    bin_min = min(totals)
    bin_max = max(totals)
    bins = np.linspace(bin_min, bin_max, 16)
    counts, _ = np.histogram(totals, bins=bins, weights=weights)
    
    density_chart = []
    for i in range(15):
        density_chart.append({
            "range": f"{int(bins[i])}-{int(bins[i+1])}",
            "probability": round((counts[i] / sum_w) * 100, 1) if sum_w > 0 else 0.0
        })

    # 8b. Game-outcome distribution for the Simulator panel + conditional
    #     lineup re-scoring (see _build_game_distribution).
    game_distribution = _build_game_distribution(game_df, req.away_team, req.home_team, ref_total, ref_spread)

    # Aggregate team summary stats from player projections
    away_players = [p for p in final_projections if p["team"] == req.away_team]
    home_players = [p for p in final_projections if p["team"] == req.home_team]

    away_pass_yds = sum(p["pYds"] for p in away_players if p["pos"] == "QB")
    home_pass_yds = sum(p["pYds"] for p in home_players if p["pos"] == "QB")
    away_rush_yds = sum(p["rYds"] for p in away_players)
    home_rush_yds = sum(p["rYds"] for p in home_players)

    away_pass_att = sum(p["pAtt"] for p in away_players if p["pos"] == "QB")
    home_pass_att = sum(p["pAtt"] for p in home_players if p["pos"] == "QB")
    away_sacks = sum(p["sacks_taken"] for p in away_players if p["pos"] == "QB")
    home_sacks = sum(p["sacks_taken"] for p in home_players if p["pos"] == "QB")
    away_rush_att = sum(p["rAtt"] for p in away_players)
    home_rush_att = sum(p["rAtt"] for p in home_players)

    away_plays = away_pass_att + away_sacks + away_rush_att
    home_plays = home_pass_att + home_sacks + home_rush_att

    # Offensive TDs = Rushing + Receiving TDs
    away_projected_tds = sum(p["rTD"] + p["recTD"] for p in away_players)
    home_projected_tds = sum(p["rTD"] + p["recTD"] for p in home_players)

    # Turnovers = Interceptions + Fumbles
    away_turnovers = sum(p["int"] for p in away_players if p["pos"] == "QB") + sum(p["fumbles"] for p in away_players)
    home_turnovers = sum(p["int"] for p in home_players if p["pos"] == "QB") + sum(p["fumbles"] for p in home_players)
        
    response = {
        "summary": {
            "away_avg_score": round(avg_away, 1),
            "home_avg_score": round(avg_home, 1),
            "win_probability_away": round(win_away * 100, 1),
            "win_probability_home": round(win_home * 100, 1),
            "away_cover_rate": round(away_covers * 100, 1),
            "home_cover_rate": round((1.0 - away_covers) * 100, 1),
            "over_probability": round(over_hits * 100, 1),
            "under_probability": round((1.0 - over_hits) * 100, 1),
            "away_plays": round(away_plays, 1),
            "home_plays": round(home_plays, 1),
            "away_pass_yds": round(away_pass_yds, 1),
            "home_pass_yds": round(home_pass_yds, 1),
            "away_rush_yds": round(away_rush_yds, 1),
            "home_rush_yds": round(home_rush_yds, 1),
            "away_projected_tds": round(away_projected_tds, 1),
            "home_projected_tds": round(home_projected_tds, 1),
            "away_turnovers": round(away_turnovers, 1),
            "home_turnovers": round(home_turnovers, 1)
        },
        "projections": sorted(final_projections, key=lambda x: x["dk_points"], reverse=True),
        "score_density": density_chart,
        "game_distribution": game_distribution,
    }
    SIMULATE_RESPONSE_CACHE[cache_key] = response
    return response

# -------------------------------------------------------------------------
# DFS OPTIMIZER ENDPOINT
# -------------------------------------------------------------------------

def _nearest_positive_definite(A: np.ndarray) -> np.ndarray:
    """Find the nearest positive definite matrix to A using Higham's algorithm."""
    B = (A + A.T) / 2
    _, s, Vt = np.linalg.svd(B)
    H = Vt.T @ np.diag(s) @ Vt
    A2 = (B + H) / 2
    A3 = (A2 + A2.T) / 2
    # Ensure positive definiteness by adding small diagonal
    k = 1
    while True:
        try:
            np.linalg.cholesky(A3)
            return A3
        except np.linalg.LinAlgError:
            min_eig = np.min(np.real(np.linalg.eigvals(A3)))
            A3 += (-min_eig * k**2 + np.finfo(float).eps) * np.eye(A.shape[0])
            k += 1


def _build_correlation_matrix(players: list) -> np.ndarray:
    """Build structural correlation matrix based on team/position relationships."""
    n = len(players)
    rho = np.eye(n)

    for i in range(n):
        for j in range(i + 1, n):
            pi, pj = players[i], players[j]
            r = 0.0
            same_team = (pi['team'] == pj['team'])

            if same_team:
                pi_pos, pj_pos = pi['pos'], pj['pos']
                if 'QB' in [pi_pos, pj_pos]:
                    other_pos = pj_pos if pi_pos == 'QB' else pi_pos
                    if other_pos == 'WR':
                        r = 0.60
                    elif other_pos == 'TE':
                        r = 0.45
                    elif other_pos == 'RB':
                        r = 0.15
                    elif other_pos == 'DST':
                        r = 0.05
                elif {pi_pos, pj_pos} == {'WR', 'WR'} or {pi_pos, pj_pos} == {'WR', 'TE'}:
                    r = 0.28
                elif {pi_pos, pj_pos} == {'RB', 'WR'} or {pi_pos, pj_pos} == {'RB', 'TE'}:
                    r = 0.10
            else:
                # Opposing team: check if one is DST vs the other's offense
                if pi['pos'] == 'DST' and pj['pos'] != 'DST':
                    r = -0.38
                elif pj['pos'] == 'DST' and pi['pos'] != 'DST':
                    r = -0.38
                elif pi['pos'] == 'QB' and pj['pos'] == 'QB':
                    r = 0.12  # Same-game QBs mildly positive (high-scoring game lifts both)
                else:
                    r = 0.02  # Mild positive for same-game players

            rho[i, j] = r
            rho[j, i] = r

    return rho


def _apply_ownership_cap(players: list, own_key: str, proj_key: str) -> None:
    """In-place soft-cap of `own_key` on every priced player (see _ownership_soft_cap)."""
    priced = [p for p in players
              if p.get('salary') and p.get(own_key) is not None and p.get(proj_key) is not None]
    if len(priced) < 10:
        return
    vals = [p[proj_key] / (max(p['salary'], 1) / 1000.0) for p in priced]
    v_med, v_p90 = float(np.median(vals)), float(np.percentile(vals, 90))
    for p, val in zip(priced, vals):
        p[own_key] = _ownership_soft_cap(p[own_key], val, v_med, v_p90)


def _solve_lineup_ilp(
    players: list,
    draw_scores: np.ndarray,
    salary_cap: int,
    prior_lineups: list,
    min_unique: int,
    include_dst_unique: bool,
    max_exposure: Union[float, Dict[str, float]],
    n_total: int,
    locked_indices: set,
    excluded_indices: set,
    flex_position: Optional[str] = None,
) -> Optional[list]:
    """Solve a single ILP lineup given a draw of scores.

    flex_position ('RB'|'WR'|'TE'|None) pins WHICH position fills the FLEX
    slot, by forcing that position's count one above its mandatory minimum
    (RB==3, WR==4, or TE==2) -- with QB/DST fixed at 1 and total at 9, RB+WR+TE
    is always exactly 7, so pinning one position's count to minimum+1 forces
    the other two to their bare minimums, which is exactly "this position
    occupies FLEX, the rest are the required starters". None leaves the
    solver free to pick FLEX purely on score, today's default behavior.

    max_exposure is either a single fraction applied to every position (the
    original behavior -- still what cash consensus / showdown pass) or a
    {pos: fraction} dict (the classic optimizer's per-position Settings
    sliders) so e.g. a chalky QB can be capped tighter than a deep WR pool
    without also choking off legitimate RB/WR rotation. A position missing
    from the dict is left uncapped (1.0) rather than silently inheriting some
    other position's number."""
    n = len(players)

    prob = pulp.LpProblem('DFS_Lineup', pulp.LpMaximize)
    x = [pulp.LpVariable(f'x_{i}', cat='Binary') for i in range(n)]

    # Objective: maximize drawn scores
    prob += pulp.lpSum(draw_scores[i] * x[i] for i in range(n))

    # Salary cap
    prob += pulp.lpSum(players[i]['salary'] * x[i] for i in range(n)) <= salary_cap

    # Position constraints
    qb_idx = [i for i, p in enumerate(players) if p['pos'] == 'QB']
    rb_idx = [i for i, p in enumerate(players) if p['pos'] == 'RB']
    wr_idx = [i for i, p in enumerate(players) if p['pos'] == 'WR']
    te_idx = [i for i, p in enumerate(players) if p['pos'] == 'TE']
    dst_idx = [i for i, p in enumerate(players) if p['pos'] == 'DST']
    flex_idx = rb_idx + wr_idx + te_idx  # FLEX eligible

    if not qb_idx or len(rb_idx) < 2 or len(wr_idx) < 3 or not te_idx or not dst_idx:
        return None

    prob += pulp.lpSum(x[i] for i in qb_idx) == 1
    prob += pulp.lpSum(x[i] for i in rb_idx) >= 2
    prob += pulp.lpSum(x[i] for i in wr_idx) >= 3
    prob += pulp.lpSum(x[i] for i in te_idx) >= 1
    prob += pulp.lpSum(x[i] for i in dst_idx) == 1
    prob += pulp.lpSum(x[i] for i in flex_idx) >= 6  # 2 RB + 3 WR + 1 TE minimum in flex pool = at least 6
    prob += pulp.lpSum(x[i] for i in range(n)) == 9  # exactly 9 players

    # Pin FLEX to a specific position (see flex_position docstring above)
    if flex_position == 'RB':
        prob += pulp.lpSum(x[i] for i in rb_idx) == 3
    elif flex_position == 'WR':
        prob += pulp.lpSum(x[i] for i in wr_idx) == 4
    elif flex_position == 'TE':
        prob += pulp.lpSum(x[i] for i in te_idx) == 2

    # Lock constraints
    for i in locked_indices:
        if i < n:
            prob += x[i] == 1

    # Exclude constraints
    for i in excluded_indices:
        if i < n:
            prob += x[i] == 0

    # Max exposure constraint (skip locked players)
    if n_total > 1:
        for i in range(n):
            if i in locked_indices:
                continue
            appearances = sum(1 for lu in prior_lineups if i in lu['indices'])
            pos_exposure = max_exposure.get(players[i]['pos'], 1.0) if isinstance(max_exposure, dict) else max_exposure
            max_apps = max(1, int(np.ceil(pos_exposure * n_total)))
            if appearances >= max_apps:
                prob += x[i] == 0

    # Min unique constraint against prior lineups
    for lu in prior_lineups:
        unique_indices = lu['indices']
        if not include_dst_unique:
            unique_indices = [i for i in unique_indices if players[i]['pos'] != 'DST']
        max_shared = 9 - min_unique
        prob += pulp.lpSum(x[i] for i in unique_indices) <= max_shared

    # Solve (silent mode)
    prob.solve(pulp.PULP_CBC_CMD(msg=0, timeLimit=5))

    if prob.status != 1:  # Not optimal
        return None

    selected = [i for i in range(n) if pulp.value(x[i]) and pulp.value(x[i]) > 0.5]
    if len(selected) != 9:
        return None

    return selected


def _generate_cash_consensus_lineups(
    players: list, salary_cap: int, n_lineups: int = 10, min_unique: int = 2,
    pos_max_exposure: Optional[Dict[str, float]] = None,
    locked_keys: Optional[set] = None,
) -> Tuple[Dict[Tuple[str, str], int], int, list]:
    """Approximates the public's cash-game consensus by generating this
    slate's own top-N cash-optimal lineups (pure median projection, zero
    variance -- exactly the objective /api/optimize itself uses for
    contest_type='cash') and counting how many of them each player lands
    in.

    Real-world DFS behavior this is modeling: the optimal cash lineup is
    largely "solved" across the industry by Thursday most weeks --
    normally one build with a couple of 2-for-2 or 3-for-3 swaps at a
    position or two, occasionally 3-5 genuinely distinct builds -- so
    players who recur across most of a slate's own top cash-optimal builds
    are a strong, cheap proxy for who the GPP field will also chalk, on
    top of (not instead of) the salary/value/Vegas signals below. The
    lineups themselves are also served directly by /api/week_cash_lineups
    for the Cash Lineups page.

    min_unique=2 (not 1) is deliberate here -- real cash consensus moves in
    2-for-2 or 3-for-3 swaps at a position or two, not single-player edits,
    so a min_unique of 1 was letting the solver make swaps looser than what
    actually happens industry-wide.

    pos_max_exposure ({pos: max_fraction}, e.g. {'DST': 0.5}) caps how often
    any ONE player at that position can appear across the N builds, forcing
    rotation to the next-best options once hit. This exists for exactly one
    reason: an early-preseason projection can run hot on a single player
    (e.g. one DST looking gaudy before sims settle down closer to the
    season) and otherwise monopolize every build, which is a temporary
    projection-noise artifact, not a real signal worth learning from.
    Positions not listed are deliberately left uncapped -- the players who
    recur through nearly every build ARE the real signal this whole
    function exists to surface; capping them too would defeat the point.

    locked_keys ({(name, team), ...}) force-includes those players in every
    generated build (x[i] == 1 in the ILP -- see _solve_lineup_ilp) instead
    of just leaving them free to be picked. Locked players are exempt from
    the pos_max_exposure capping below, same as _solve_lineup_ilp already
    exempts them from its own max_exposure constraint -- a lock is a
    deliberate override, not a rotation candidate.

    Returns (appearance_counts keyed by (name, team), the actual number of
    lineups generated -- may be less than n_lineups if the pool runs out
    of distinct valid builds -- and the assembled lineups themselves, each
    {'slots': [...], 'total_salary': int, 'projected_score': float}).
    """
    if len(players) < 9:
        return {}, 0, []

    pos_max_exposure = pos_max_exposure or {}
    locked_indices = {
        i for i, p in enumerate(players) if (p['name'], p['team']) in (locked_keys or set())
    }
    draw_scores = np.array([p['projection'] for p in players])
    prior_lineups: list = []
    appearance_counts: Dict[Tuple[str, str], int] = {}
    lineups: list = []
    max_attempts = n_lineups * 3
    attempts = 0

    while len(prior_lineups) < n_lineups and attempts < max_attempts:
        attempts += 1

        # Hard-exclude anyone who's already hit their position's exposure
        # cap, so the solver is forced onto the next-best option at that
        # position for the remaining builds.
        capped_out = set()
        for i, p in enumerate(players):
            if i in locked_indices:
                continue
            cap = pos_max_exposure.get(p['pos'])
            if cap is None:
                continue
            max_allowed = max(1, int(np.ceil(cap * n_lineups)))
            key = (p['name'], p['team'])
            if appearance_counts.get(key, 0) >= max_allowed:
                capped_out.add(i)

        selected_indices = _solve_lineup_ilp(
            players, draw_scores, salary_cap, prior_lineups,
            min_unique, True, 1.0, n_lineups, locked_indices, capped_out
        )
        if selected_indices is None:
            break  # exhausted the pool's distinct near-optimal builds
        prior_lineups.append({'indices': selected_indices})
        for i in selected_indices:
            key = (players[i]['name'], players[i]['team'])
            appearance_counts[key] = appearance_counts.get(key, 0) + 1
        slots = _assign_slots(selected_indices, players)
        lineups.append({
            'slots': slots,
            'total_salary': sum(players[i]['salary'] for i in selected_indices),
            'projected_score': round(sum(players[i]['projection'] for i in selected_indices), 2),
        })

    return appearance_counts, len(prior_lineups), lineups


def _assign_slots(selected_indices: list, players: list) -> list:
    """Assign players to canonical DK slots: QB, RB, RB, WR, WR, WR, TE, FLEX, DST."""
    selected = [(i, players[i]) for i in selected_indices]

    qbs = [(i, p) for i, p in selected if p['pos'] == 'QB']
    rbs = [(i, p) for i, p in selected if p['pos'] == 'RB']
    wrs = [(i, p) for i, p in selected if p['pos'] == 'WR']
    tes = [(i, p) for i, p in selected if p['pos'] == 'TE']
    dsts = [(i, p) for i, p in selected if p['pos'] == 'DST']

    slots = []
    used = set()

    # QB (1)
    if qbs:
        i, p = qbs[0]; slots.append({**p, 'slot': 'QB', 'player_idx': i}); used.add(i)

    # RB (2)
    rb_count = 0
    for i, p in rbs:
        if rb_count < 2 and i not in used:
            slots.append({**p, 'slot': 'RB', 'player_idx': i}); used.add(i); rb_count += 1

    # WR (3)
    wr_count = 0
    for i, p in wrs:
        if wr_count < 3 and i not in used:
            slots.append({**p, 'slot': 'WR', 'player_idx': i}); used.add(i); wr_count += 1

    # TE (1)
    for i, p in tes:
        if i not in used:
            slots.append({**p, 'slot': 'TE', 'player_idx': i}); used.add(i); break

    # FLEX: first remaining eligible player not yet used
    flex_eligible = rbs + wrs + tes
    for i, p in flex_eligible:
        if i not in used:
            slots.append({**p, 'slot': 'FLEX', 'player_idx': i}); used.add(i); break

    # DST (1)
    if dsts:
        i, p = dsts[0]; slots.append({**p, 'slot': 'DST', 'player_idx': i}); used.add(i)

    return slots


def _compute_lineup_stats(
    lineup_slots: list,
    field_scores: np.ndarray,
    payout_structure: list,
    entry_fee: float,
    total_entries: int,
    paying_positions: int,
    n_field_sims: int = 10000,
) -> dict:
    """Compute ITM%, Top1%, Top0.1%, EV%, and lineup percentile/volatility stats via Monte Carlo vs precomputed simulated field.

    Design decisions:
    - Our lineup is scored by sampling each player's dk_pcts_all distribution (empirical CDF).
    - Field lineups are precomputed to save massive execution time.
    - All player projections here are the MEDIAN (P50) values.
    """
    import random
    rng = np.random.default_rng()

    def sample_score_n(p: dict, n: int) -> np.ndarray:
        """Vectorised version for our lineup (n draws per player)."""
        pcts = p.get('dk_pcts_all')
        proj = p.get('projection', 10.0)
        if pcts and len(pcts) == 101:
            pct_arr = np.array(pcts, dtype=float)
            idxs = rng.integers(0, 101, size=n)
            return pct_arr[idxs]
        else:
            std = max(0.5, proj * 0.35)
            return np.maximum(0.0, rng.normal(proj, std, n))

    # ── Simulate our lineup scores (N draws) ──────────────────────────────────
    lineup_score_draws = np.zeros(n_field_sims)
    for p in lineup_slots:
        lineup_score_draws += sample_score_n(p, n_field_sims)

    # ── Compute percentile cutoffs ────────────────────────────────────────────
    paying_pct = paying_positions / max(total_entries, 1)
    cutoff_itm  = float(np.percentile(field_scores, (1 - paying_pct) * 100))
    cutoff_top1  = float(np.percentile(field_scores, 99))
    cutoff_top01 = float(np.percentile(field_scores, 99.9))

    itm_pct  = float(np.mean(lineup_score_draws > cutoff_itm)  * 100)
    top1_pct  = float(np.mean(lineup_score_draws > cutoff_top1)  * 100)
    top01_pct = float(np.mean(lineup_score_draws > cutoff_top01) * 100)

    # ── EV calculation ────────────────────────────────────────────────────────
    prize_pool = entry_fee * total_entries * 0.85  # ~15% rake
    ev = 0.0
    if payout_structure:
        for tier in payout_structure:
            r_start = tier['rank_start'] if isinstance(tier, dict) else tier.rank_start
            r_end   = tier['rank_end']   if isinstance(tier, dict) else tier.rank_end
            payout  = tier['payout']     if isinstance(tier, dict) else tier.payout
            p_rank  = (r_end - r_start + 1) / max(total_entries, 1)
            top001_thresh = max(1, int(total_entries * 0.001))
            top1_thresh   = max(1, int(total_entries * 0.01))
            if r_end <= top001_thresh:
                p_in_tier = top01_pct / 100.0 * (r_end - r_start + 1) / top001_thresh
            elif r_end <= top1_thresh:
                p_in_tier = top1_pct / 100.0 * (r_end - r_start + 1) / top1_thresh
            else:
                p_in_tier = itm_pct / 100.0 * p_rank / max(paying_pct, 1e-9)
            ev += p_in_tier * payout
    else:
        ev = itm_pct / 100.0 * (prize_pool / max(paying_positions, 1))

    ev_pct = ((ev / max(entry_fee, 1)) - 1) * 100

    # ── Lineup percentile and volatility metrics ──────────────────────────────
    lineup_p50 = float(np.percentile(lineup_score_draws, 50))
    lineup_p75 = float(np.percentile(lineup_score_draws, 75))
    lineup_p95 = float(np.percentile(lineup_score_draws, 95))
    lineup_std = float(np.std(lineup_score_draws))

    return {
        'itm_pct':    round(itm_pct, 2),
        'top1_pct':   round(top1_pct, 2),
        'top01_pct':  round(top01_pct, 2),
        'ev_pct':     round(ev_pct, 2),
        'lineup_p50': round(lineup_p50, 2),
        'lineup_p75': round(lineup_p75, 2),
        'lineup_p95': round(lineup_p95, 2),
        'lineup_std': round(lineup_std, 2),
    }


@app.post('/api/optimize')
async def optimize_lineups(req: OptimizeRequest):
    """Generate N optimal DFS lineups using ILP + stochastic correlated draws."""

    # Filter excluded players. For the Lineup Lab, keep everyone -- an
    # excluded flag is a generation constraint, not "this player can't
    # score" -- so a hand lineup can reference anyone (same rationale as
    # showdown's optimize_showdown).
    manual = req.manual_lineups or None
    active_players = [p.dict() for p in req.players] if manual else [p.dict() for p in req.players if not p.excluded]
    all_players_dict = [p.dict() for p in req.players]  # keep all for field sims

    if len(active_players) < 9:
        raise HTTPException(status_code=400, detail='Not enough active players to build a lineup (need at least 9)')

    # Validate position availability
    pos_counts = {}
    for p in active_players:
        pos_counts[p['pos']] = pos_counts.get(p['pos'], 0) + 1

    required = {'QB': 1, 'RB': 2, 'WR': 3, 'TE': 1, 'DST': 1}
    for pos, min_count in required.items():
        if pos_counts.get(pos, 0) < min_count:
            raise HTTPException(
                status_code=400,
                detail=f'Not enough {pos} players (need at least {min_count}, have {pos_counts.get(pos, 0)})'
            )

    # Fill in ownership for any player missing one (predict_classic_ownership
    # -- and the heuristic it falls back to -- both skip players that already
    # have a value, whether that's the Player Pool's computed per-week number
    # or a manual override typed into its Own% column). Seeded from the
    # pool's own content -- name, team, salary, projection -- so the exact
    # same optimize request always reproduces the exact same ownership, and
    # therefore the exact same EV/portfolio metrics below; a genuinely
    # different pool (edited projections, a new week) naturally gets a
    # different seed.
    ownership_seed_str = "|".join(
        f"{p['name']}:{p['team']}:{p['salary']}:{p['projection']}"
        for p in sorted(active_players, key=lambda p: (p['name'], p['team']))
    )
    ownership_seed = int(hashlib.md5(ownership_seed_str.encode()).hexdigest(), 16) % (2**32)
    # Cash-consensus signal for the trained model (same technique as
    # get_week_sim_results' bulk prepopulation, see _generate_cash_consensus_lineups) --
    # computed fresh off THIS request's pool so it reflects any live
    # projection edits, not a stale weekly precompute.
    cash_counts, n_cash_generated, _ = _generate_cash_consensus_lineups(
        active_players, salary_cap=req.salary_cap, pos_max_exposure={'DST': 0.5},
    )
    cash_consensus = {k: v / n_cash_generated for k, v in cash_counts.items()} if n_cash_generated else {}
    # Contest-aware segmentation (2026-09-22) -- entry_fee/total_entries are
    # real values the Settings panel already sends for the field-sim/payout
    # math below; reusing them here costs nothing and lets the ownership
    # model see the actual contest this optimize run targets. max_entries
    # has no frontend field yet (nothing in this app tracks "Nmax" today),
    # so it's always None here until that's built -- predict_classic_ownership
    # degrades gracefully when any contest key is missing.
    active_players = predict_classic_ownership(
        active_players, week=req.week, cash_consensus=cash_consensus, seed=ownership_seed,
        contest={"field_size": req.total_entries, "entry_fee": req.entry_fee, "max_entries": req.max_entries})

    # Build correlation matrix and covariance
    n = len(active_players)
    # projection = P50/median — used for simulation evaluation and displayed score
    projections = np.array([p['projection'] for p in active_players])
    # ilp_scores = gpp_projection if provided (blended ceiling), else fall back to median
    # This is the objective the ILP maximises; it is NOT used for evaluation/EV.
    ilp_scores = np.array([
        p.get('gpp_projection') if p.get('gpp_projection') is not None else p['projection']
        for p in active_players
    ])

    # Estimate std devs from percentile data or projection (using median baseline)
    std_devs = []
    for p in active_players:
        pcts = p.get('dk_pcts_all')
        if pcts and len(pcts) == 101:
            p25 = pcts[25]
            p75 = pcts[75]
            sigma = max(0.5, (p75 - p25) / 1.35)
        else:
            sigma = max(0.5, p['projection'] * 0.35)
        std_devs.append(sigma)
    std_devs = np.array(std_devs)

    rho = _build_correlation_matrix(active_players)
    D = np.diag(std_devs)
    cov = D @ rho @ D
    cov = _nearest_positive_definite(cov)

    try:
        L = np.linalg.cholesky(cov)
    except np.linalg.LinAlgError:
        L = np.diag(std_devs)  # fallback to independent draws

    # Distribution width multiplier by contest type
    WIDTH = {'cash': 0.0, 'flat': 0.6, 'top_heavy': 1.0, 'extreme_top_heavy': 1.4}
    width_mult = WIDTH.get(req.contest_type, 1.0)

    # For cash games, use deterministic approach (single lineup)
    if req.contest_type == 'cash':
        req_n = 1
    else:
        req_n = min(req.n_lineups, 1000)

    # Iteration-aligned real sim scores for THIS week's slate (see
    # _build_week_trial_scores for why this no longer reads the season-long
    # parquet unfiltered). n_sim_iter comes from the data -- 10,000 for weeks
    # simmed at full size, 1,000 for older ones -- never a hardcoded 1000.
    trial_scores_map: Dict[tuple, np.ndarray] = {}
    n_sim_iter, trial_source = 0, 'none'
    try:
        trial_scores_map, n_sim_iter, trial_source = _build_week_trial_scores(
            req.week, {p['team'] for p in active_players})
    except Exception as e:
        print(f"Error loading trial aligned scores: {e}")
    # Synthetic fallback arrays (players with no sim rows) use the same length
    # so every player is indexed by the same iteration ids.
    n_index = n_sim_iter if n_sim_iter > 0 else 1000

    valid_filter = sorted({int(i) for i in req.iteration_filter if 0 <= int(i) < n_index}) if req.iteration_filter else None

    # ── 'sim' draw source: per-player deviations from real sim iterations ──
    # sim_dev[k, i] = player i's (projection-scaled) sim score in slate-wide
    # iteration k minus his own (scaled) sim mean. A lineup's ILP objective
    # is then ilp_scores + width_mult * sim_dev[k] -- the exact shape of the
    # gaussian path (ilp_scores + width_mult * L @ z), with the made-up
    # correlated normal swapped for one real simulated week. Centering on
    # the MEAN keeps the perturbation zero-mean like the gaussian's, so the
    # ceiling tilt still comes only from gpp_projection (no double count).
    # Scaling matches grading's get_player_trial_scores: an edited
    # projection rescales that player's whole sim distribution.
    # Players with no sim rows (custom adds) get an independent normal draw
    # sized by their std_dev, per lineup.
    sim_dev: Optional[np.ndarray] = None
    sim_missing = np.zeros(n, dtype=bool)
    iter_order = np.arange(0)
    draw_source = req.draw_source if (req.draw_source == 'sim' and n_sim_iter > 0) else 'gaussian'
    if draw_source == 'sim':
        sim_dev = np.zeros((n_index, n), dtype=np.float32)
        for i, p in enumerate(active_players):
            arr = trial_scores_map.get((p['name'], p['team'], p['pos']))
            if arr is None:
                sim_missing[i] = True
                continue
            med = float(np.percentile(arr, 50))
            proj = p.get('projection', 10.0)
            scaled = arr * (proj / med) if (med > 1.0 and abs(proj - med) > 0.1) else arr
            sim_dev[:, i] = scaled - scaled.mean()
        # One distinct real iteration per lineup attempt where possible (a
        # permutation, cycled), restricted to the Game Distribution
        # box-select when one is set -- so a filtered optimize BUILDS for
        # that scenario, not just grades against it.
        iter_pool = np.array(valid_filter) if valid_filter else np.arange(n_index)
        iter_order = np.random.default_rng().permutation(iter_pool)

    # Lock/exclude index mapping
    locked_indices = {i for i, p in enumerate(active_players) if p.get('locked', False)}
    excluded_indices = set()  # already filtered out

    # Build payout structure
    prize_pool = req.entry_fee * req.total_entries * 0.85
    if req.payout_structure:
        payout_structure = [t.dict() for t in req.payout_structure]
    else:
        payout_structure = _get_default_payout_structure(
            req.contest_type, prize_pool, req.paying_positions, req.total_entries
        )

    rng = np.random.default_rng()
    generated_lineups = []
    prior_lineups = []  # track indices for uniqueness/exposure

    if manual:
        # ── Lineup Lab: score hand-built lineups, no ILP generation ──────
        def _key(s):
            return re.sub(r'[^a-z0-9]', '', str(s).lower())
        by_name: Dict[str, int] = {}
        for i, p in enumerate(active_players):
            by_name.setdefault(_key(p['name']), i)
            by_name[f"{_key(p['name'])}|{str(p['team']).upper()}"] = i

        def _resolve(tok: str) -> int:
            tok = str(tok).strip()
            if '|' in tok:
                nm, tm = tok.split('|', 1)
                hit = by_name.get(f"{_key(nm)}|{tm.strip().upper()}")
                if hit is not None:
                    return hit
                tok = nm
            return by_name.get(_key(tok), -1)

        for li, ml in enumerate(manual):
            rb, wr = list(ml.rb or []), list(ml.wr or [])
            if len(rb) != 2 or len(wr) != 3:
                raise HTTPException(status_code=400,
                                    detail=f'Lineup {li + 1} needs exactly 2 RB and 3 WR (got {len(rb)} RB, {len(wr)} WR).')
            picks = [('QB', ml.qb), ('RB', rb[0]), ('RB', rb[1]), ('WR', wr[0]), ('WR', wr[1]),
                     ('WR', wr[2]), ('TE', ml.te), ('FLEX', ml.flex), ('DST', ml.dst)]
            resolved = [(slot, _resolve(tok), tok) for slot, tok in picks]
            missing = [tok for _, idx, tok in resolved if idx < 0]
            if missing:
                raise HTTPException(status_code=400,
                                    detail=f'Lineup {li + 1}: player(s) not in pool: {", ".join(missing)}')
            idxs = [idx for _, idx, _ in resolved]
            if len(set(idxs)) != 9:
                raise HTTPException(status_code=400,
                                    detail=f'Lineup {li + 1}: a player is used twice.')
            # Each fixed slot must actually hold that position; FLEX just
            # needs to be flex-eligible (a mis-slotted player is a silent
            # scoring bug otherwise -- classic scoring has no CPT-style
            # multiplier to catch it visually like showdown does).
            for slot, idx, tok in resolved:
                actual = active_players[idx]['pos']
                ok = actual in ('RB', 'WR', 'TE') if slot == 'FLEX' else actual == slot
                if not ok:
                    raise HTTPException(status_code=400,
                                        detail=f"Lineup {li + 1}: {tok} is {actual}, not eligible for {slot}.")
            slots = [{**active_players[idx], 'slot': slot, 'player_idx': idx} for slot, idx, _ in resolved]
            total_salary = sum(s['salary'] for s in slots)
            median_score = sum(s['projection'] for s in slots)
            prior_lineups.append({'indices': idxs})
            generated_lineups.append({
                'slots': slots, 'indices': idxs, 'total_salary': total_salary,
                'projected_score': round(median_score, 2),
                'label': ml.label or f'Lineup {li + 1}',
                'over_salary_cap': total_salary > req.salary_cap,
            })
    else:
        max_attempts = req_n * 5  # allow extra attempts for failed ILP solves
        attempts = 0

        # FLEX position mix (Settings sliders, see OptimizeRequest.flex_position_weights):
        # normalize the raw slider values into a probability distribution over
        # ['RB', 'WR', 'TE', None] (None = 'ANY', unconstrained) once up front,
        # then draw a target from it per lineup below. This makes the REALIZED
        # mix across the req_n lineups approximate the slider ratios -- exact
        # per-lineup guarantees aren't meaningful here since a given score draw
        # + pool + locks can make a specific target infeasible (handled the
        # same way an infeasible score draw already is: solve returns None,
        # the attempt is skipped, the next draw tries again).
        flex_choices: Optional[List[Optional[str]]] = None
        flex_probs: Optional[np.ndarray] = None
        raw_weights = req.flex_position_weights or {}
        weight_total = sum(max(0.0, raw_weights.get(k, 0.0)) for k in ('RB', 'WR', 'TE', 'ANY'))
        if weight_total > 0:
            flex_choices = ['RB', 'WR', 'TE', None]
            flex_probs = np.array([
                max(0.0, raw_weights.get('RB', 0.0)),
                max(0.0, raw_weights.get('WR', 0.0)),
                max(0.0, raw_weights.get('TE', 0.0)),
                max(0.0, raw_weights.get('ANY', 0.0)),
            ]) / weight_total

        # Per-position exposure caps (Settings -> Max Exposure by Position)
        # take over from the single max_exposure scalar when provided -- see
        # OptimizeRequest.max_exposure_by_pos and _solve_lineup_ilp's docstring.
        exposure_arg = req.max_exposure_by_pos if req.max_exposure_by_pos else req.max_exposure

        while len(generated_lineups) < req_n and attempts < max_attempts:
            attempts += 1

            # Draw correlated scores for ILP — uses ilp_scores (gpp-blended ceiling)
            # NOT projections (P50), so the ILP picks players with real ceiling.
            if width_mult > 0 and draw_source == 'sim':
                k = int(iter_order[(attempts - 1) % len(iter_order)])
                perturbation = sim_dev[k].astype(float)
                if sim_missing.any():
                    perturbation[sim_missing] = std_devs[sim_missing] * rng.standard_normal(int(sim_missing.sum()))
                draw_scores = np.maximum(0, ilp_scores + width_mult * perturbation)
            elif width_mult > 0:
                z = rng.standard_normal(n)
                perturbation = L @ z
                draw_scores = np.maximum(0, ilp_scores + width_mult * perturbation)
            else:
                draw_scores = ilp_scores.copy()

            flex_position = rng.choice(flex_choices, p=flex_probs) if flex_choices else None

            selected_indices = _solve_lineup_ilp(
                active_players, draw_scores,
                req.salary_cap, prior_lineups,
                req.min_unique_players, req.include_dst_in_unique,
                exposure_arg, req_n,
                locked_indices, excluded_indices,
                flex_position=flex_position,
            )

            if selected_indices is None:
                continue

            slots = _assign_slots(selected_indices, active_players)

            total_salary = sum(p['salary'] for p in slots)
            median_score = sum(p['projection'] for p in slots)

            prior_lineups.append({'indices': selected_indices})
            generated_lineups.append({
                'slots': slots,
                'indices': selected_indices,
                'total_salary': total_salary,
                'projected_score': round(median_score, 2)
            })

    if not generated_lineups:
        raise HTTPException(
            status_code=500,
            detail='Could not generate any valid lineups. Check player pool and constraints.'
        )

    # ── Precompute simulated field scores once for the entire request ─────────
    n_stat_sims = 10000

    # trial_scores_map / n_index / valid_filter are built above, before
    # lineup generation -- the 'sim' draw_source builds from the same arrays.
    rng = np.random.default_rng()
    # Sample aligned iteration indices (n_stat_sims draws, with replacement,
    # from 0..n_index-1). Every game on the slate was simulated together per
    # iteration (one full-slate scenario per index), so restricting which
    # indices get drawn from -- see OptimizeRequest.iteration_filter, fed by
    # GameDistribution.jsx's box-select on ONE game -- conditions the whole
    # slate's field AND our lineups on that game landing in the selected
    # total/margin range, same "cheap" mechanism as the showdown optimizer's
    # iteration_filter. The filter's upper bound is n_index, not 1000: a
    # 10,000-iteration week sends ids up to 9999, which the old `< 1000` check
    # silently dropped (~90% of a box-select thrown away).
    if valid_filter:
        aligned_indices = rng.choice(np.array(valid_filter), size=n_stat_sims, replace=True)
        iteration_filter_frac = round(len(valid_filter) / float(n_index), 4)
    else:
        aligned_indices = rng.integers(0, n_index, size=n_stat_sims)
        iteration_filter_frac = None

    # Both branches below get called once per (field-lineup, slot) pair --
    # with FIELD_SAMPLE_K in the thousands and ~9 slots per lineup, that's
    # tens of thousands of calls per request, many for the SAME player. Cache
    # each player's median (cheap but was being recomputed every call) and,
    # for the no-real-data fallback, the whole synthetic 1000-length trial
    # array (was instead constructing a brand-new np.random.default_rng
    # PER ITERATION PER CALL -- 10,000 RNG constructions each time a player
    # with no parquet trials got scored, which is what actually made a
    # large field sample unaffordable, not the field size itself).
    _median_cache: Dict[tuple, float] = {}
    _fallback_cache: Dict[tuple, np.ndarray] = {}

    def get_player_trial_scores(p: dict, indices: np.ndarray) -> np.ndarray:
        key = (p['name'], p['team'], p['pos'])
        if key in trial_scores_map:
            arr = trial_scores_map[key]
            safe_idxs = np.clip(indices, 0, len(arr) - 1)
            # If the user edited the projection, scale the trials proportionally
            original_median = _median_cache.get(key)
            if original_median is None:
                original_median = float(np.percentile(arr, 50))
                _median_cache[key] = original_median
            proj = p.get('projection', 10.0)
            if original_median > 1.0 and abs(proj - original_median) > 0.1:
                scale = proj / original_median
                return arr[safe_idxs] * scale
            return arr[safe_idxs]
        else:
            # Fallback: no real parquet trials for this player -- synthesize
            # a deterministic n_index-length trial array once (same convention
            # as trial_scores_map, indexed by iteration id) instead of a fresh
            # RNG per iteration per call.
            synth = _fallback_cache.get(key)
            if synth is None:
                proj = p.get('projection', 10.0)
                std = max(0.5, proj * 0.35)
                base_seed = hash(p['name']) % 10000
                # One seeded generator for the whole array (not one per
                # iteration) -- at 10,000 iterations the per-index RNG
                # construction was 10x the old 1000-length cost per player.
                synth = np.maximum(
                    0.0, np.random.default_rng(base_seed).normal(proj, std, n_index))
                _fallback_cache[key] = synth
            safe_idxs = np.clip(indices, 0, len(synth) - 1)
            return synth[safe_idxs]

    # ── Field: score EVERY field-sample lineup across ALL aligned iterations ──
    # (not one randomly-picked opponent per iteration, and not a cutoff blended
    # across every environment) so our lineup can be ranked against the actual
    # simulated field -- archetype-composed (sharp / fake_sharp / casual /
    # toilet, see field_simulator.py and
    # docs/implementation_plans/field_simulation_implementation_plan.md) --
    # in the SAME game-environment draw, iteration by iteration. See
    # _compute_lineup_field_stats for how this gets turned into ITM/Top1%/EV%.
    field_lineups_cached = None
    if req.week is not None:
        cached_field = FIELD_SAMPLE_CACHE.get((req.week, None))
        if cached_field and cached_field.get('lineups'):
            field_lineups_cached = cached_field['lineups']
        else:
            # Self-heal: FIELD_SAMPLE_CACHE is normally built as a side effect
            # of GET /api/week_sim_results -> get_week_sim_results(), but that
            # endpoint has its own disk-backed cache (data/interim/week_N_sim_
            # results.json) which, when fresh, returns early WITHOUT ever
            # reaching the code that builds the field sample -- so on a plain
            # backend restart, FIELD_SAMPLE_CACHE can stay empty indefinitely
            # even though that endpoint keeps responding instantly. Previously
            # this silently degraded /api/optimize to the crude uniform-random
            # fallback field below (no value/ownership weighting at all),
            # which is what was producing wildly inflated EV%/ITM% -- verified
            # live: that fallback field's mean score was roughly HALF of a
            # normal generated lineup's. Build the real field directly from
            # this request's own pool instead of depending on that other
            # endpoint's cache having taken its slow path.
            try:
                prior_own = {
                    (p['name'], p['team']): p['ownership_pct']
                    for p in active_players if p.get('ownership_pct') is not None
                }
                field_sample = build_field_sample(
                    active_players, prior_own, salary_cap=req.salary_cap, K=FIELD_SAMPLE_K, seed=req.week,
                )
                if field_sample.get('lineups'):
                    FIELD_SAMPLE_CACHE[(req.week, None)] = {
                        **field_sample, 'built_at': time.time(), 'week': req.week,
                    }
                    field_lineups_cached = field_sample['lineups']
            except Exception as e:
                print(f"Error self-building field sample for week {req.week}: {e}")

    if field_lineups_cached:
        n_field = len(field_lineups_cached)
        field_matrix = np.zeros((n_field, n_stat_sims))
        for fi, lu in enumerate(field_lineups_cached):
            for p in lu:
                field_matrix[fi] += get_player_trial_scores(p, aligned_indices)
    else:
        # Fallback: no week given, or that week's field sample hasn't been
        # built yet this process lifetime -- build a modest uniform-random
        # field ourselves so this endpoint never hard-fails just because
        # week was omitted.
        by_pos: dict = {'QB': [], 'RB': [], 'WR': [], 'TE': [], 'DST': []}
        for p in active_players:
            pos = p.get('pos', '')
            if pos in by_pos:
                by_pos[pos].append(p)

        import random

        def _pick_one(pool: list) -> Optional[dict]:
            if not pool:
                return None
            return random.choice(pool)

        def build_field_lineup_fallback() -> Optional[list]:
            used = set()
            lineup = []
            remaining = req.salary_cap

            def pick_pos(pos: str, count: int) -> bool:
                nonlocal remaining
                for _ in range(count):
                    pool = [
                        p for p in by_pos.get(pos, [])
                        if p['name'] not in used and p.get('salary', 0) <= remaining
                    ]
                    if not pool:
                        pool = [p for p in by_pos.get(pos, []) if p['name'] not in used]
                    if not pool:
                        return False
                    player = _pick_one(pool)
                    if player is None:
                        return False
                    lineup.append(player)
                    used.add(player['name'])
                    remaining -= player.get('salary', 0)
                return True

            required = [('QB', 1), ('RB', 2), ('WR', 3), ('TE', 1), ('DST', 1)]
            for pos, cnt in required:
                if not pick_pos(pos, cnt):
                    return None

            flex_pool = [
                p for pos in ('RB', 'WR', 'TE')
                for p in by_pos.get(pos, [])
                if p['name'] not in used
            ]
            flex = _pick_one(flex_pool)
            if flex is None:
                return None
            lineup.append(flex)
            return lineup

        N_FIELD_FALLBACK = 300
        fallback_lineups = [lu for lu in (build_field_lineup_fallback() for _ in range(N_FIELD_FALLBACK)) if lu is not None]
        if fallback_lineups:
            field_matrix = np.zeros((len(fallback_lineups), n_stat_sims))
            for fi, lu in enumerate(fallback_lineups):
                for p in lu:
                    field_matrix[fi] += get_player_trial_scores(p, aligned_indices)
        else:
            avg_proj = float(np.mean([p.get('projection', 10.0) for p in active_players]))
            field_matrix = np.full((1, n_stat_sims), avg_proj * 9)

    # Evaluate each lineup using aligned simulations. All lineups' draws are
    # ranked against the field in ONE batched pass (see
    # _compute_lineup_field_stats_batch) rather than one comparison per
    # lineup -- required to make a field this large (FIELD_SAMPLE_K) affordable.
    all_lineup_draws = []
    for lu in generated_lineups:
        lineup_score_draws = np.zeros(n_stat_sims)
        for p in lu['slots']:
            lineup_score_draws += get_player_trial_scores(p, aligned_indices)
        all_lineup_draws.append(lineup_score_draws)

    all_stats = _compute_lineup_field_stats_batch(
        all_lineup_draws, field_matrix, payout_structure,
        req.entry_fee, req.total_entries, req.paying_positions
    )

    lineup_results = []
    for lu, stats in zip(generated_lineups, all_stats):
        lineup_results.append({
            **stats,
            'players': [{
                'name': p['name'],
                'pos': p['pos'],
                'team': p['team'],
                'salary': p['salary'],
                'projection': p['projection'],
                'slot': p['slot'],
                'ownership_pct': p.get('ownership_pct', 0.0),
                'ownership_source': p.get('ownership_source'),  # "model" | "heuristic" -- which one produced this number
                'dk_pcts_all': p.get('dk_pcts_all'),
                'dk_id': p.get('dk_id'),
                'dk_name': p.get('dk_name'),
            } for p in lu['slots']],
            'total_salary': lu['total_salary'],
            'projected_score': lu['projected_score'],
            'label': lu.get('label'),
            'over_salary_cap': lu.get('over_salary_cap', lu['total_salary'] > req.salary_cap),
        })

    # Compute portfolio metrics
    n_gen = len(generated_lineups)

    # Pairwise similarity
    similarities = []
    for i in range(n_gen):
        for j in range(i + 1, n_gen):
            shared = len(set(generated_lineups[i]['indices']) & set(generated_lineups[j]['indices']))
            similarities.append(shared / 9.0)
    avg_correlation = float(np.mean(similarities)) if similarities else 0.0

    # ELC (weight = 1/n for uniform)
    w = 1.0 / n_gen
    elc = 1.0 / (n_gen * w**2)  # = n_gen when all unique
    # Adjust by actual diversity: penalize for shared players
    diversity_factor = 1.0 - avg_correlation
    elc = round(n_gen * diversity_factor, 1)

    # Portfolio EV
    portfolio_ev = float(np.mean([lu['ev_pct'] for lu in lineup_results]))

    # Coverage score: fraction of players used at least once across all lineups
    all_used = set()
    for lu in generated_lineups:
        all_used.update(lu['indices'])
    coverage_score = round(len(all_used) / max(n, 1), 3)

    # Portfolio score per lineup: marginal contribution
    # Simplified: (lineup EV - avg EV) + (1 - similarity to rest of book)
    for i, lu_result in enumerate(lineup_results):
        if n_gen > 1:
            other_similarities = []
            for j in range(n_gen):
                if j != i:
                    shared = len(set(generated_lineups[i]['indices']) & set(generated_lineups[j]['indices']))
                    other_similarities.append(shared / 9.0)
            avg_sim_to_others = float(np.mean(other_similarities))
            portfolio_score = round((lu_result['ev_pct'] - portfolio_ev) * 0.7 + (1 - avg_sim_to_others) * 3.0, 2)
        else:
            portfolio_score = 0.0
        lu_result['portfolio_score'] = portfolio_score

    # Realized FLEX position mix -- what actually came out of the generated
    # set, as opposed to the requested flex_position_weights target. Lets the
    # Settings sliders show "here's what you asked for vs. what you got"
    # (a target can go unmet when the pool/salary cap/locks make it
    # infeasible for some share of lineups -- see flex_position's docstring).
    flex_mix_counts = {'RB': 0, 'WR': 0, 'TE': 0}
    for lu in generated_lineups:
        flex_slot = next((s for s in lu['slots'] if s['slot'] == 'FLEX'), None)
        if flex_slot and flex_slot['pos'] in flex_mix_counts:
            flex_mix_counts[flex_slot['pos']] += 1
    flex_mix_pct = (
        {k: round(100.0 * v / n_gen, 1) for k, v in flex_mix_counts.items()} if n_gen else None
    )

    return {
        'mode': 'manual' if manual else 'optimize',
        'lineups': lineup_results,
        'portfolio': {
            'total_ev_pct': round(portfolio_ev, 2),
            'effective_lineup_count': elc,
            'avg_correlation': round(avg_correlation, 3),
            'coverage_score': coverage_score,
            'n_generated': n_gen,
            'n_requested': req_n,
            'iteration_filter_frac': iteration_filter_frac,
            'iteration_filter_n': len(valid_filter) if valid_filter else None,
            'flex_mix_pct': flex_mix_pct,
            # Which sim the EV/ITM/Top% grading read, and how many iterations
            # it had -- same idea as showdown's field_source.
            'trial_source': trial_source,
            'n_sim_iterations': n_sim_iter,
            'draw_source': draw_source,
        }
    }


# -------------------------------------------------------------------------
# SHOWDOWN (SINGLE-GAME) OPTIMIZER ENDPOINT
# -------------------------------------------------------------------------
#
# Structure mirrors /api/optimize as closely as possible so the two tools
# stay legible side-by-side, with three deliberate differences driven by
# the showdown format and by what this tool is *for*:
#
#   1. Roster: 1 CPT (1.5x points AND 1.5x salary) + 5 FLEX, any position
#      (DK lets a DST captain; kickers would belong here too but the sim
#      engine doesn't produce them yet -- known gap, noted in the response).
#
#   2. Ownership is a first-class objective term, not just a post-hoc stat.
#      Showdown fields are small and top-heavy, and the single biggest edge
#      is captaining someone the field isn't. The ILP objective subtracts
#      `leverage_lambda * projected_ownership` from each player's drawn
#      score, so raising lambda trades raw ceiling for a lower total-owned
#      lineup continuously (penalty-only knob -- see the Sept 2026 build
#      decision; a hard cap can come later).
#
#   3. Ownership model is showdown-specific: CPT ownership concentrates on
#      ceiling/name plays far more than FLEX ownership does, so the two are
#      modelled separately (_compute_showdown_ownership) rather than reusing
#      the 9-slot classic softmax.
#
# The field-sim EV/ITM/Top% machinery is shared with the classic path
# (_compute_lineup_field_stats_batch) -- the only new field-side code is a
# showdown lineup constructor (_build_showdown_field) and iteration-aligned
# scoring off this game's slice of the season parquet so a shared game
# environment lifts our lineup and the field together (same
# correlation-preserving trick /api/optimize uses).

class ShowdownOptimizerPlayer(BaseModel):
    name: str
    team: str
    pos: str                                    # QB, RB, WR, TE, DST
    salary: int                                 # base FLEX salary; CPT costs 1.5x
    projection: float                           # P50/median at FLEX scoring; CPT scores 1.5x
    gpp_projection: Optional[float] = None       # blended ceiling -- ILP objective only
    locked: bool = False                        # force into every lineup (CPT or FLEX)
    locked_cpt: bool = False                    # force in specifically as the captain
    cpt_eligible: bool = True                   # False -> never considered as captain (still FLEX-eligible)
    excluded: bool = False
    ownership_pct: Optional[float] = None        # FLEX ownership % override (None -> modelled)
    cpt_ownership_pct: Optional[float] = None    # CPT ownership % override (None -> modelled)
    optimal_cpt_pct: Optional[float] = None      # sim's optimal-captain rate (0-100), feeds the ownership model
    optimal_flex_pct: Optional[float] = None     # sim's optimal-flex rate (0-100), feeds the ownership model
    implied_total: Optional[float] = None        # team's Vegas/sim implied points, feeds the ownership model
    dk_pcts_all: Optional[List[float]] = None    # 101-element [p0..p100] FLEX-scoring dist
    dk_id: Optional[int] = None                 # DK draftableId for the FLEX slot (CSV export)
    dk_cpt_id: Optional[int] = None             # DK draftableId for the CPT slot (CSV export)


class ShowdownPrepPlayer(BaseModel):
    name: str
    team: str
    pos: str
    salary: Optional[int] = None
    projection: Optional[float] = None
    optimal_cpt_pct: Optional[float] = None
    optimal_flex_pct: Optional[float] = None
    ownership_pct: Optional[float] = None        # a hand override, left untouched
    cpt_ownership_pct: Optional[float] = None


class ShowdownPrepRequest(BaseModel):
    players: List[ShowdownPrepPlayer]
    game_id: Optional[str] = None
    away_team: Optional[str] = None
    home_team: Optional[str] = None
    # A Game-Read box-select's iteration subset (same convention as
    # ShowdownOptimizeRequest.iteration_filter) -- when given,
    # _compute_showdown_optimal_rates() scopes its solve to just these
    # iterations, so re-picking a scenario yields real optimal-lineup rates
    # for that conditioned subset instead of the whole season.
    iteration_filter: Optional[List[int]] = None
    # Contest-aware segmentation (2026-09-22) -- unlike /api/optimize and
    # /api/showdown_optimize, this endpoint runs BEFORE a contest is chosen
    # (it pre-populates the pool right after picking a game), so the
    # frontend has nothing to send here yet -- these stay None until that
    # flow changes. Accepted now so the backend is ready the moment it does,
    # without yet another API contract change.
    entry_fee: Optional[float] = None
    field_size: Optional[float] = None
    max_entries: Optional[float] = None


class ManualShowdownLineup(BaseModel):
    """A hand-built showdown lineup to score instead of solving for one.
    `cpt` / `flex` are player names (matched against the pool, case- and
    punctuation-insensitive; append '|TEAM' to disambiguate a shared name)."""
    cpt: str
    flex: List[str]
    label: Optional[str] = None


class ShowdownOptimizeRequest(BaseModel):
    players: List[ShowdownOptimizerPlayer]
    game_id: Optional[str] = None               # locates this game's parquet slice for aligned field scoring
    week: Optional[int] = None
    n_lineups: int = 20
    # When set, skip lineup generation entirely and just run these hand-built
    # lineups through the same field sim + EV/ITM/Top%/1st% scoring. The
    # "Lineup Lab" -- paste a lineup, see how it does.
    manual_lineups: Optional[List[ManualShowdownLineup]] = None
    salary_cap: int = 50000
    contest_type: str = 'top_heavy'             # cash, flat, top_heavy, extreme_top_heavy
    min_unique_players: int = 2                 # min differing players between any two of our lineups (of 6)
    max_exposure: float = 0.5                   # cap on any player's total appearance rate
    cpt_max_exposure: float = 0.4              # tighter cap on any player's *captain* rate
    leverage_lambda: float = 0.0               # ownership penalty weight in the ILP objective
    entry_fee: float = 5.0
    total_entries: int = 50000
    paying_positions: int = 12000
    # See OptimizeRequest.max_entries -- same "not sourced from the frontend
    # yet, accepted so the ownership model can use it once it is" story.
    max_entries: Optional[float] = None
    payout_structure: Optional[List[PayoutTier]] = None
    # Conditional lineup re-scoring ("cheap" mode of the game-distribution
    # box-select): the raw 0-999 iteration ids GameDistribution.jsx's
    # drag-selection resolved to (see game_distribution.raw.iteration). When
    # set, both our lineup(s) AND the synthetic field are scored ONLY on
    # iterations from this set -- i.e. "given the game lands in this total /
    # margin box, how does this lineup do" -- by restricting where the aligned
    # trial draws come from, not by re-solving anything. Works with both
    # generated and manual_lineups.
    iteration_filter: Optional[List[int]] = None


def _solve_showdown_fast(
    players: list,
    draw_scores: np.ndarray,
    own_flex: np.ndarray,
    own_cpt: np.ndarray,
    leverage_lambda: float,
    salary_cap: int,
    prior_lineups: list,
    min_unique: int,
    max_exposure: float,
    cpt_max_exposure: float,
    n_total: int,
    locked_indices: set,
    locked_cpt_indices: set,
    excluded_indices: set,
    cpt_ineligible_indices: set = frozenset(),
) -> Optional[dict]:
    """Pure-Python showdown solve (1 CPT + 5 FLEX). Replaces the per-draw CBC
    ILP: for a 12-26 player single-game pool the ILP's subprocess spawn +
    solve was 50ms-5s each and occasionally hung, dominating the endpoint.
    Per captain, an exact branch-and-bound over the 5 FLEX slots with a
    suffix-sum upper-bound prune (the same shape as
    solve_showdown_iteration) -- sub-millisecond for a pool this size, and
    it's the true optimum for the drawn objective, not an approximation.

    Objective per player: FLEX = draw - lambda*own_flex ; CPT = 1.5*draw -
    lambda*own_cpt (values may be negative when the leverage penalty bites).
    Honours lock / lock-as-captain / exclude, per-player and per-captain
    exposure caps vs `prior_lineups`, and min-unique (a player counts once
    whether CPT or FLEX). Returns {'cpt': idx, 'flex': [idx x5]} or None.
    """
    n = len(players)
    sal = np.array([p['salary'] for p in players], dtype=float)
    # A tiny salary term in the objective so that, all else near-equal, the
    # solver spends the cap (real showdown lineups do) -- ~0.5 value per $1k,
    # far below a real projection gap, just a tie-breaker toward studs. Keeps
    # the branch-and-bound's value-based pruning strong (a hard min-salary
    # leaf reject would gut the prune and blow up to full enumeration).
    flex_val = draw_scores - leverage_lambda * own_flex + 0.0005 * sal
    cpt_val = 1.5 * draw_scores - leverage_lambda * own_cpt + 0.00075 * sal

    over_total, over_cpt = set(), set()
    if n_total > 1:
        max_apps = max(1, int(np.ceil(max_exposure * n_total)))
        max_cpt_apps = max(1, int(np.ceil(cpt_max_exposure * n_total)))
        for i in range(n):
            if i in locked_indices or i in locked_cpt_indices:
                continue
            t = sum(1 for lu in prior_lineups if i == lu['cpt'] or i in lu['flex'])
            c = sum(1 for lu in prior_lineups if i == lu['cpt'])
            if t >= max_apps:
                over_total.add(i)
            if c >= max_cpt_apps:
                over_cpt.add(i)

    prior_members = [{lu['cpt'], *lu['flex']} for lu in prior_lineups]
    max_shared = 6 - min_unique
    forced_flex = locked_indices - locked_cpt_indices

    if locked_cpt_indices:
        cpt_choices = [next(iter(locked_cpt_indices))]
    else:
        cpt_choices = [i for i in range(n)
                       if i not in excluded_indices and i not in over_total and i not in over_cpt
                       and i not in cpt_ineligible_indices
                       and sal[i] * 1.5 <= salary_cap]

    def _search():
        best = {'cpt': None, 'flex': None, 'val': -1e18}
        best_any = {'cpt': None, 'flex': None, 'val': -1e18}
        for cpt in cpt_choices:
            rem_budget = salary_cap - sal[cpt] * 1.5
            forced = [i for i in forced_flex if i != cpt]
            if any(i in excluded_indices for i in forced):
                continue
            forced_sal = sum(sal[i] for i in forced)
            if len(forced) > 5 or forced_sal > rem_budget:
                continue

            cands = [i for i in range(n)
                     if i != cpt and i not in excluded_indices and i not in over_total
                     and i not in forced and sal[i] <= rem_budget]
            cands.sort(key=lambda i: -flex_val[i])
            nf = len(cands)
            need = 5 - len(forced)
            if nf < need:
                continue

            suffix = [0.0] * (nf + 1)  # upper-bound prune, valid with negatives
            for i in range(nf - 1, -1, -1):
                suffix[i] = suffix[i + 1] + flex_val[cands[i]]

            forced_val = sum(flex_val[i] for i in forced)
            # Pure value-optimal 5-flex for this captain -- NO min-unique
            # check in the loop (that would kill the prune and blow up to
            # full enumeration); checked once, after, on the finished lineup.
            local = {'val': -1e18, 'set': None}

            def dfs(idx, count, cur_sal, cur_val, chosen):
                if count == need:
                    total = cur_val + forced_val
                    if total > local['val']:
                        local['val'] = total
                        local['set'] = list(chosen)
                    return
                if idx >= nf or count + (nf - idx) < need:
                    return
                if cur_val + suffix[idx] + forced_val <= local['val']:
                    return
                ci = cands[idx]
                if cur_sal + sal[ci] <= rem_budget:
                    chosen.append(ci)
                    dfs(idx + 1, count + 1, cur_sal + sal[ci], cur_val + flex_val[ci], chosen)
                    chosen.pop()
                dfs(idx + 1, count, cur_sal, cur_val, chosen)

            dfs(0, 0, forced_sal, 0.0, [])
            if local['set'] is None:
                continue
            val = cpt_val[cpt] + local['val']
            members = {cpt, *forced, *local['set']}
            distinct = not any(len(members & pm) > max_shared for pm in prior_members)
            cand = {'cpt': cpt, 'flex': sorted([*forced, *local['set']]), 'val': val}
            if distinct:
                if val > best['val']:
                    best = cand
            elif best['cpt'] is None and val > best_any['val']:
                best_any = cand
        return best if best['cpt'] is not None else best_any

    best = _search()
    if best is None or best['cpt'] is None:
        return None
    return {'cpt': best['cpt'], 'flex': best['flex']}


def _solve_showdown_ilp(
    players: list,
    draw_scores: np.ndarray,
    own_flex: np.ndarray,
    own_cpt: np.ndarray,
    leverage_lambda: float,
    salary_cap: int,
    prior_lineups: list,
    min_unique: int,
    max_exposure: float,
    cpt_max_exposure: float,
    n_total: int,
    locked_indices: set,
    locked_cpt_indices: set,
    excluded_indices: set,
) -> Optional[dict]:
    """Solve one showdown lineup (1 CPT + 5 FLEX) for a given score draw.

    Objective per player:
        FLEX: draw_i            - leverage_lambda * own_flex_i
        CPT : 1.5 * draw_i      - leverage_lambda * own_cpt_i
    Returns {'cpt': idx, 'flex': [idx x5]} or None if infeasible.
    """
    n = len(players)
    prob = pulp.LpProblem('Showdown_Lineup', pulp.LpMaximize)
    c = [pulp.LpVariable(f'c_{i}', cat='Binary') for i in range(n)]  # captain
    f = [pulp.LpVariable(f'f_{i}', cat='Binary') for i in range(n)]  # flex

    prob += pulp.lpSum(
        (1.5 * draw_scores[i] - leverage_lambda * own_cpt[i]) * c[i]
        + (draw_scores[i] - leverage_lambda * own_flex[i]) * f[i]
        for i in range(n)
    )

    prob += pulp.lpSum(c) == 1
    prob += pulp.lpSum(f) == 5
    for i in range(n):
        prob += c[i] + f[i] <= 1                     # a player fills at most one slot
    prob += pulp.lpSum(1.5 * players[i]['salary'] * c[i] + players[i]['salary'] * f[i]
                       for i in range(n)) <= salary_cap

    for i in excluded_indices:
        if i < n:
            prob += c[i] == 0
            prob += f[i] == 0
    for i in locked_indices:
        if i < n and i not in locked_cpt_indices:
            prob += c[i] + f[i] == 1
    for i in locked_cpt_indices:
        if i < n:
            prob += c[i] == 1

    # Exposure caps (skip locked players). Appearances counted from prior lineups.
    if n_total > 1:
        max_apps = max(1, int(np.ceil(max_exposure * n_total)))
        max_cpt_apps = max(1, int(np.ceil(cpt_max_exposure * n_total)))
        for i in range(n):
            if i in locked_indices or i in locked_cpt_indices:
                continue
            total_apps = sum(1 for lu in prior_lineups if i == lu['cpt'] or i in lu['flex'])
            cpt_apps = sum(1 for lu in prior_lineups if i == lu['cpt'])
            if total_apps >= max_apps:
                prob += c[i] + f[i] == 0
            if cpt_apps >= max_cpt_apps:
                prob += c[i] == 0

    # Min-unique vs each prior lineup: a player counts once whether CPT or FLEX.
    max_shared = 6 - min_unique
    for lu in prior_lineups:
        members = [lu['cpt']] + list(lu['flex'])
        prob += pulp.lpSum(c[i] + f[i] for i in members) <= max_shared

    try:
        prob.solve(pulp.PULP_CBC_CMD(msg=0, timeLimit=5))
    except pulp.PulpSolverError:
        # CBC can transiently fail to spawn its subprocess under heavy machine
        # load -- treat as "no solution this draw" so one bad draw drops a
        # lineup attempt instead of 500ing the whole request.
        return None
    if prob.status != 1:
        return None
    cpt_sel = [i for i in range(n) if pulp.value(c[i]) and pulp.value(c[i]) > 0.5]
    flex_sel = [i for i in range(n) if pulp.value(f[i]) and pulp.value(f[i]) > 0.5]
    if len(cpt_sel) != 1 or len(flex_sel) != 5:
        return None
    return {'cpt': cpt_sel[0], 'flex': sorted(flex_sel)}


def _assign_showdown_slots(sol: dict, players: list) -> list:
    """CPT first, then FLEX by projection desc. Each slot dict carries the
    slot-adjusted salary/projection so downstream math and display don't
    have to remember the 1.5x rule."""
    out = []
    cp = players[sol['cpt']]
    out.append({**cp, 'slot': 'CPT', 'player_idx': sol['cpt'],
                'slot_salary': int(round(cp['salary'] * 1.5)),
                'slot_projection': round(cp['projection'] * 1.5, 2)})
    for i in sorted(sol['flex'], key=lambda j: -players[j]['projection']):
        p = players[i]
        out.append({**p, 'slot': 'FLEX', 'player_idx': i,
                    'slot_salary': int(p['salary']),
                    'slot_projection': round(p['projection'], 2)})
    return out


def _synthesize_kicker_scores(game_id: str, team: str) -> Optional[np.ndarray]:
    """Back out a team kicker's DK fantasy line per sim iteration from the
    cached game + player parquet, since the engine models no kickers.

    Per iteration i:
        team_tds  = sum of that team's players' (rTD + recTD + def_td) at i
        team_pts  = away_score[i] or home_score[i] for `team`
        xp_made   = team_tds            (assume all PATs kicked & made)
        fg_made   = max(0, round((team_pts - 7*team_tds) / 3))
        dk_score  = 3.5*fg_made + 1.0*xp_made      (3.5 ~ blended FG value
                    incl. the DK 40-49 (+1) / 50+ (+2) distance bonuses)

    Returns a per-iteration np.ndarray aligned to the parquet's iteration
    order, or None if the game isn't in cache.
    """
    game_df = GAMES_BY_GAME_ID.get(game_id)
    players_df = PLAYERS_BY_GAME_ID.get(game_id)
    if game_df is None or players_df is None or game_df.empty:
        return None
    g = game_df.sort_values('iteration')
    if str(g.iloc[0]['away_team']) == team:
        team_pts = g['away_score'].values.astype(float)
    elif str(g.iloc[0]['home_team']) == team:
        team_pts = g['home_score'].values.astype(float)
    else:
        return None

    tp = players_df[players_df['Team'] == team]
    td_cols = [c for c in ('rTD', 'recTD', 'def_td') if c in tp.columns]
    if not td_cols:
        return None
    tds_by_iter = tp.groupby('iteration')[td_cols].sum().sum(axis=1)
    tds = tds_by_iter.reindex(g['iteration'].values, fill_value=0.0).values.astype(float)

    n = min(len(team_pts), len(tds))
    team_pts, tds = team_pts[:n], tds[:n]
    fg_made = np.maximum(0.0, np.round((team_pts - 7.0 * tds) / 3.0))
    return 3.5 * fg_made + 1.0 * tds


# solve_showdown_iteration (src/nfl_sim/optimizer.py) is an exact branch-and-
# bound solve, not the MILP solve_optimal_lineup_milp uses for Classic --
# ~15-25ms/iteration against a real ~30-40-player Showdown pool (measured
# 2026-09-17; a naive synthetic/uniform-score benchmark is ~10x slower since
# it defeats the score-based pruning real, skewed DK scores get for free).
# 300 iterations keeps a live /showdown_prep recompute (including a fresh
# Game-Read scenario pick) in the same few-second range as the page's other
# "Load DK Salaries"-style waits, without the multi-minute cost a full
# 1,000-10,000-iteration solve would take synchronously on a request.
SHOWDOWN_OPTIMAL_SAMPLE_CAP = 300


def _compute_showdown_optimal_rates(
    game_id: Optional[str], week: Optional[int],
    player_keys: List[Tuple[str, str]], salary_by_key: Dict[Tuple[str, str], Optional[int]],
    iteration_filter: Optional[List[int]] = None,
) -> Dict[Tuple[str, str], Dict[str, float]]:
    """True per-iteration Showdown-optimal captain/FLEX rates for one game:
    solve_showdown_iteration against a sample of that game's REAL simulated
    iterations -- not the 2-iteration guess /api/week_sim_results' bulk
    week-prepopulation settles for (optimizer_sample_cap=2, chosen there to
    keep a whole-week prepopulation fast; see that request's own comment).
    Prefers the DFS-week-specific sim (real current-week injuries/roster),
    same preference run_simulation() applies, falling back to the season-long
    cache. `iteration_filter`, when given (a Game-Read box-select), scopes
    the solve to just that iteration subset, so re-picking a scenario
    produces real rates conditioned on it rather than the whole season.

    Kickers are out of scope -- the engine models no kicker plays at all (see
    _synthesize_kicker_scores' docstring), so there is no real per-iteration
    kicker score series to solve against; callers should exclude pos == 'K'
    from `player_keys`.
    """
    if not game_id or not player_keys:
        return {}
    wp = _get_dfs_week_players(week) if week else None
    if wp is None or wp.empty:
        wp = ALL_PLAYERS_CACHED
    if wp is None or wp.empty:
        return {}
    g = wp[wp["game_id"] == game_id]
    if iteration_filter:
        allowed = {int(i) for i in iteration_filter}
        g = g[g["iteration"].isin(allowed)]
    if g.empty:
        return {}

    unique_iterations = g["iteration"].unique()
    if len(unique_iterations) > SHOWDOWN_OPTIMAL_SAMPLE_CAP:
        step = len(unique_iterations) // SHOWDOWN_OPTIMAL_SAMPLE_CAP
        unique_iterations = unique_iterations[::step][:SHOWDOWN_OPTIMAL_SAMPLE_CAP]
    if len(unique_iterations) == 0:
        return {}

    from src.nfl_sim.optimizer import solve_showdown_iteration
    salaries_arr = np.array([salary_by_key.get(pk) or 0 for pk in player_keys])
    sub = g[g["iteration"].isin(unique_iterations)]
    pivot = sub.pivot_table(index="iteration", columns=["Player", "Team"],
                             values="dk_score", fill_value=0.0)

    cpt_counts: Dict[Tuple[str, str], int] = {}
    flex_counts: Dict[Tuple[str, str], int] = {}
    solved = 0
    for it in unique_iterations:
        if it not in pivot.index:
            continue
        row = pivot.loc[it]
        scores = np.array([row[pk] if pk in row.index else 0.0 for pk in player_keys])
        lineup = solve_showdown_iteration(player_keys, salaries_arr, scores)
        if not lineup:
            continue
        solved += 1
        cpt_counts[lineup[0]] = cpt_counts.get(lineup[0], 0) + 1
        for flex_pk in lineup[1:]:
            flex_counts[flex_pk] = flex_counts.get(flex_pk, 0) + 1

    if solved == 0:
        return {}
    return {
        pk: {
            "optimal_cpt_pct": round(100.0 * cpt_counts.get(pk, 0) / solved, 2),
            "optimal_flex_pct": round(100.0 * flex_counts.get(pk, 0) / solved, 2),
        }
        for pk in player_keys
    }


def _build_showdown_field(
    players: list, salary_cap: int, n_field: int, seed: Optional[int] = None
) -> list:
    """Ownership-weighted synthetic showdown field: `n_field` lineups, each
    {'cpt': idx, 'flex': [idx x5]}. CPT drawn from cpt_ownership_pct, FLEX
    from ownership_pct, rejection-sampled against the salary cap (cap check
    relaxed after a few misses so a tight pool still fills)."""
    rng = np.random.default_rng(seed)
    n = len(players)
    idxs = np.arange(n)
    cpt_w = np.array([max(0.01, p.get('cpt_ownership_pct') or 0.5) for p in players], dtype=float)
    cpt_w = cpt_w / cpt_w.sum()
    flex_w = np.array([max(0.01, p.get('ownership_pct') or 0.5) for p in players], dtype=float)
    sal = np.array([p['salary'] for p in players], dtype=float)

    field = []
    attempts = 0
    while len(field) < n_field and attempts < n_field * 20:
        attempts += 1
        cpt = int(rng.choice(idxs, p=cpt_w))
        remaining = [j for j in idxs if j != cpt]
        w = flex_w[remaining] / flex_w[remaining].sum()
        flex = rng.choice(remaining, size=5, replace=False, p=w).tolist()
        total = sal[cpt] * 1.5 + sal[flex].sum()
        if total > salary_cap and attempts % 4 != 0:  # mostly enforce, occasionally allow
            continue
        field.append({'cpt': cpt, 'flex': sorted(flex)})
    return field


@app.post('/api/showdown_prep')
def showdown_prep(req: ShowdownPrepRequest):
    """Pre-compute the three things the showdown pool needs that the raw sim
    doesn't give it, so the Player Pool table can show them before any
    optimize run (same idea as the classic optimizer pre-populating
    `ownership_proj` from the weekly sim):

      1. Real per-iteration Showdown-optimal captain/FLEX rates, but ONLY
         when req.iteration_filter (a Game-Read scenario) is set --
         _compute_showdown_optimal_rates re-solves against just that
         iteration subset and overwrites the client's optimal_cpt_pct/
         optimal_flex_pct with it. With no scenario, the client's own values
         are trusted as-is: they already come from a real, near-exhaustive
         solve (optimizer_sample_cap=1000, no sub-sampling) baked into the
         weekly sim or a "Run Engine" rerun, so there's nothing to gain by
         paying for the same unconditioned solve again on every pool load.
      2. Modelled FLEX + CPT ownership for every player
         (_compute_showdown_ownership -- value core + the real optimal-
         CPT/FLEX rates from step 1 as a chalk proxy + a Vegas nudge).
         Players that already carry a hand override keep it.
      3. A synthetic per-iteration line for each kicker (pos 'K') off the
         game's team score + TD count (_synthesize_kicker_scores), returned
         as projection / ceiling / a 101-pt dk_pcts_all so the kicker slots
         into the pool and the optimizer's field sim like any other player.

    Response: {"players": [{name, team, pos, ownership_pct, cpt_ownership_pct,
               optimal_cpt_pct, optimal_flex_pct, projection?, ceiling?,
               dk_pcts_all?}], "kicker_source": str}
    """
    game_id = req.game_id
    if not game_id and req.away_team and req.home_team:
        for gid, gdf in GAMES_BY_GAME_ID.items():
            if gdf.empty:
                continue
            r0 = gdf.iloc[0]
            if {str(r0['away_team']), str(r0['home_team'])} == {req.away_team, req.home_team}:
                game_id = gid
                break

    # ShowdownPrepRequest carries no week field -- game_id is
    # "{year}_{week:02d}_{away}_{home}" (e.g. "2026_01_DEN_KC"), so pull it
    # from there for the trained ownership model's Vegas/slate-size lookups.
    week = None
    if game_id:
        try:
            week = int(game_id.split('_')[1])
        except (IndexError, ValueError):
            pass

    # Per-team implied points from the sim's own mean score (Vegas proxy).
    implied_by_team: Dict[str, float] = {}
    gdf = GAMES_BY_GAME_ID.get(game_id) if game_id else None
    if gdf is not None and not gdf.empty:
        r0 = gdf.iloc[0]
        implied_by_team[str(r0['away_team'])] = float(gdf['away_score'].mean())
        implied_by_team[str(r0['home_team'])] = float(gdf['home_score'].mean())

    pool = []
    kicker_source = 'none'
    for p in req.players:
        d = p.dict()
        d['implied_total'] = implied_by_team.get(p.team)
        if p.pos == 'K':
            karr = _synthesize_kicker_scores(game_id, p.team) if game_id else None
            if karr is not None and len(karr) >= 100:
                kicker_source = 'game_script'
                pcts = np.percentile(karr, np.linspace(0, 100, 101)).round(2).tolist()
                d['projection'] = round(float(np.percentile(karr, 50)), 1)
                d['_ceiling'] = round(float(np.percentile(karr, 95)), 1)
                d['_dk_pcts_all'] = pcts
            elif not d.get('projection'):
                d['projection'] = 8.0  # last-resort flat default
        pool.append(d)

    # Only re-solve here when a Game-Read scenario is actually active. The
    # UNCONDITIONED optimal_cpt_pct/optimal_flex_pct the client sent already
    # come from a real, near-exhaustive solve baked into the weekly sim
    # (see _compute_one_game's optimizer_sample_cap=1000, a full solve with
    # no sub-sampling) or a "Run Engine" single-game rerun (same field's
    # default, 1,000) -- Cam's call 2026-09-17: precompute it properly once
    # rather than re-solving the same unconditioned answer on every pool
    # load. A scenario pick, though, can't be precomputed (it's chosen at
    # browse time), so that case still needs a fresh, scenario-scoped solve.
    if req.iteration_filter:
        player_keys = [(d['name'], d['team']) for d in pool if d['pos'] != 'K']
        salary_by_key = {(d['name'], d['team']): d.get('salary') for d in pool}
        optimal_rates = _compute_showdown_optimal_rates(
            game_id, week, player_keys, salary_by_key, iteration_filter=req.iteration_filter
        )
        for d in pool:
            rates = optimal_rates.get((d['name'], d['team']))
            if rates:
                d['optimal_cpt_pct'] = rates['optimal_cpt_pct']
                d['optimal_flex_pct'] = rates['optimal_flex_pct']
            elif d['pos'] != 'K':
                # No real rate resolved (e.g. this game has no cached sim
                # data at all) -- 0 rather than the client's stale guess, so
                # a UI showing "0%" here is an honest "couldn't compute",
                # not a leftover sample from a different (unconditioned)
                # scenario.
                d['optimal_cpt_pct'] = 0.0
                d['optimal_flex_pct'] = 0.0

    # zlib.crc32, not Python's builtin hash() -- hash() is randomized per
    # process (PYTHONHASHSEED), so the "deterministic" seed used to only
    # hold within one server run, not across restarts.
    predict_showdown_ownership(
        pool, week=week, seed=(zlib.crc32((game_id or 'sd').encode()) & 0xffffffff),
        contest={"field_size": req.field_size, "entry_fee": req.entry_fee, "max_entries": req.max_entries})

    out = []
    for d in pool:
        row = {
            'name': d['name'], 'team': d['team'], 'pos': d['pos'],
            'ownership_pct': d.get('ownership_pct'),
            'cpt_ownership_pct': d.get('cpt_ownership_pct'),
            'optimal_cpt_pct': d.get('optimal_cpt_pct'),
            'optimal_flex_pct': d.get('optimal_flex_pct'),
            'ownership_source': d.get('ownership_source'),  # "model" | "heuristic"
        }
        if d['pos'] == 'K':
            row['projection'] = d.get('projection')
            row['ceiling'] = d.get('_ceiling')
            row['dk_pcts_all'] = d.get('_dk_pcts_all')
        out.append(row)
    return {'players': out, 'game_id': game_id, 'kicker_source': kicker_source}


_SHOWDOWN_OPT_LOCK = threading.Lock()


@app.post('/api/optimize_showdown')
def _optimize_showdown_endpoint(req: ShowdownOptimizeRequest):
    """Serialise showdown-optimize requests. The body is CPU-bound pure
    Python (~1s); without this, a burst of duplicate/retry requests (e.g.
    a page reload while one is in flight) all run at once in the anyio
    threadpool, GIL-thrash each other to a near-standstill, and pile up.
    One at a time: the loser waits a beat, then computes its own (fast)."""
    with _SHOWDOWN_OPT_LOCK:
        return optimize_showdown(req)


def optimize_showdown(req: ShowdownOptimizeRequest):
    """Generate N showdown lineups: pure-Python showdown solve + correlated
    stochastic draws + leverage-lambda ownership penalty, then full field-sim
    EV/ITM/Top%/portfolio metrics (shared with /api/optimize)."""

    manual = req.manual_lineups or None
    # For the Lineup Lab, keep every player in the pool (an excluded flag is a
    # generation constraint, not a "this player can't score" statement) so a
    # hand lineup can reference anyone.
    active = [p.dict() for p in req.players] if manual else [p.dict() for p in req.players if not p.excluded]
    if len(active) < 6:
        raise HTTPException(status_code=400, detail='Need at least 6 players in the pool for a showdown lineup.')

    # Deterministic ownership seed from pool content (same pool -> same
    # numbers -> comparable EV), same rationale as /api/optimize.
    seed_str = "|".join(
        f"{p['name']}:{p['team']}:{p['salary']}:{p['projection']}"
        for p in sorted(active, key=lambda p: (p['name'], p['team']))
    )
    own_seed = int(hashlib.md5(seed_str.encode()).hexdigest(), 16) % (2**32)
    # Contest-aware segmentation (2026-09-22) -- see the identical comment on
    # /api/optimize's predict_classic_ownership call; entry_fee/total_entries
    # are real values already sent by this endpoint's caller, max_entries
    # isn't tracked by the frontend yet so stays None until that's built.
    active = predict_showdown_ownership(
        active, week=req.week, seed=own_seed,
        contest={"field_size": req.total_entries, "entry_fee": req.entry_fee, "max_entries": req.max_entries})

    n = len(active)
    projections = np.array([p['projection'] for p in active], dtype=float)
    ilp_scores = np.array([
        p['gpp_projection'] if p.get('gpp_projection') is not None else p['projection']
        for p in active
    ], dtype=float)
    own_flex = np.array([p.get('ownership_pct') or 0.5 for p in active], dtype=float)
    own_cpt = np.array([p.get('cpt_ownership_pct') or 0.5 for p in active], dtype=float)

    # σ from percentile spread when available, else 35% of projection.
    std_devs = []
    for p in active:
        pcts = p.get('dk_pcts_all')
        if pcts and len(pcts) == 101:
            std_devs.append(max(0.5, (pcts[75] - pcts[25]) / 1.35))
        else:
            std_devs.append(max(0.5, p['projection'] * 0.35))
    std_devs = np.array(std_devs)

    rho = _build_correlation_matrix(active)
    D = np.diag(std_devs)
    cov = _nearest_positive_definite(D @ rho @ D)
    try:
        L = np.linalg.cholesky(cov)
    except np.linalg.LinAlgError:
        L = np.diag(std_devs)

    WIDTH = {'cash': 0.0, 'flat': 0.6, 'top_heavy': 1.0, 'extreme_top_heavy': 1.4}
    width_mult = WIDTH.get(req.contest_type, 1.0)
    req_n = 1 if req.contest_type == 'cash' else min(req.n_lineups, 1000)

    locked_indices = {i for i, p in enumerate(active) if p.get('locked') or p.get('locked_cpt')}
    locked_cpt_indices = {i for i, p in enumerate(active) if p.get('locked_cpt')}
    if len(locked_cpt_indices) > 1:
        raise HTTPException(status_code=400, detail='Only one player can be locked as captain.')
    # cpt_eligible defaults True; locking a player as captain overrides an
    # accidental cpt_eligible=False on that same player rather than fighting it.
    cpt_ineligible_indices = {i for i, p in enumerate(active)
                               if p.get('cpt_eligible') is False and i not in locked_cpt_indices}
    if not manual and len(cpt_ineligible_indices) >= n:
        raise HTTPException(status_code=400, detail='At least one player must be eligible for the captain pool.')

    # Normalise the leverage penalty so `leverage_lambda` is scale-free and
    # intuitive regardless of the slate's point/ownership magnitudes. `own_scale`
    # = pool-average points per 1% of ownership, so with lambda=1 a
    # league-average-owned player is docked roughly a league-average player's
    # worth of projection (very strong); lambda ~0.15-0.4 is a lean, ~1+ is
    # a hard fade. The frontend slider defaults to 0.25 and tops out ~1.5.
    mean_flex_own = float(np.mean([o for o in own_flex if o > 0])) if np.any(own_flex > 0) else 1.0
    own_scale = float(np.mean(ilp_scores)) / max(mean_flex_own, 1e-6)
    eff_lambda = req.leverage_lambda * own_scale

    def _pack(sol):
        slots = _assign_showdown_slots(sol, active)
        return {
            'sol': sol,
            'slots': slots,
            'total_salary': sum(s['slot_salary'] for s in slots),
            'projected_score': round(sum(s['slot_projection'] for s in slots), 2),
            'total_ownership': round(
                (active[sol['cpt']].get('cpt_ownership_pct') or 0.0)
                + sum((active[i].get('ownership_pct') or 0.0) for i in sol['flex']), 1),
        }

    rng = np.random.default_rng()
    generated = []

    if manual:
        # ── Lineup Lab: score hand-built lineups, no generation ──────────
        def _key(s):
            return re.sub(r'[^a-z0-9]', '', str(s).lower())
        by_name: Dict[str, int] = {}
        for i, p in enumerate(active):
            by_name.setdefault(_key(p['name']), i)
            by_name[f"{_key(p['name'])}|{str(p['team']).upper()}"] = i

        def _resolve(tok: str) -> int:
            tok = str(tok).strip()
            if '|' in tok:
                nm, tm = tok.split('|', 1)
                hit = by_name.get(f"{_key(nm)}|{tm.strip().upper()}")
                if hit is not None:
                    return hit
                tok = nm
            return by_name.get(_key(tok), -1)

        for li, ml in enumerate(manual):
            flex_names = list(ml.flex or [])
            if len(flex_names) != 5:
                raise HTTPException(status_code=400,
                                    detail=f'Lineup {li + 1} needs exactly 5 FLEX (got {len(flex_names)}).')
            ci = _resolve(ml.cpt)
            fis = [_resolve(x) for x in flex_names]
            missing = ([ml.cpt] if ci < 0 else []) + [flex_names[k] for k, v in enumerate(fis) if v < 0]
            if missing:
                raise HTTPException(status_code=400,
                                    detail=f'Lineup {li + 1}: player(s) not in pool: {", ".join(missing)}')
            picks = [ci] + fis
            if len(set(picks)) != 6:
                raise HTTPException(status_code=400,
                                    detail=f'Lineup {li + 1}: a player is used twice (CPT and FLEX must be 6 different players).')
            g = _pack({'cpt': ci, 'flex': sorted(fis)})
            g['label'] = ml.label or f'Lineup {li + 1}'
            generated.append(g)
    else:
        prior = []
        attempts = 0
        while len(generated) < req_n and attempts < req_n * 4:
            attempts += 1
            if width_mult > 0:
                z = rng.standard_normal(n)
                draw = np.maximum(0.0, ilp_scores + width_mult * (L @ z))
            else:
                draw = ilp_scores.copy()
            sol = _solve_showdown_fast(
                active, draw, own_flex, own_cpt, eff_lambda,
                req.salary_cap, prior, req.min_unique_players,
                req.max_exposure, req.cpt_max_exposure, req_n,
                locked_indices, locked_cpt_indices, set(),
                cpt_ineligible_indices,
            )
            if sol is None:
                continue
            prior.append(sol)
            generated.append(_pack(sol))

    if not generated:
        raise HTTPException(status_code=500, detail='Could not generate any valid showdown lineups. Check the pool and constraints.')

    # ── Iteration-aligned trial scores from this game's parquet slice ─────
    # Prefer the DFS-week parquet (data/interim/dfs_week_{N}_players.parquet) --
    # it's simulated from data/current_rosters/dfs/, so its starters and usage
    # reflect the week's real injury/availability news. Fall back to the
    # season-long "everyone healthy" parquet (PLAYERS_BY_GAME_ID) only when the
    # week isn't given or hasn't been DFS-simmed yet. `field_source` is surfaced
    # in the response so the UI can show which sim the EV/Top%/1st% ran against.
    n_stat_sims = 10000
    trial_map: Dict[tuple, np.ndarray] = {}
    field_source = 'independent_draws'
    game_df = None
    if req.game_id:
        if req.week is not None:
            dfs_by_game = _get_dfs_week_by_game_id(req.week)
            if dfs_by_game and req.game_id in dfs_by_game:
                game_df = dfs_by_game[req.game_id][1]  # (games_slice, players_slice)
                field_source = f'dfs_week_{req.week}'
        if game_df is None:
            game_df = PLAYERS_BY_GAME_ID.get(req.game_id)
            if game_df is not None:
                field_source = 'season_parquet'
    if game_df is not None:
        try:
            for (pl, tm, ps), grp in game_df.groupby(['Player', 'Team', 'Pos']):
                arr = grp.sort_values('iteration')['dk_score'].values
                trial_map[(pl, tm, ps)] = arr
                trial_map[(tm, ps)] = arr  # 2-team game -> (team,pos) is unique enough
        except Exception as e:
            print(f'showdown: trial map build failed: {e}')

    # Kickers aren't in the sim -- synthesise their per-iteration line from the
    # game's team score + TD count (see _synthesize_kicker_scores).
    if req.game_id:
        for p in active:
            if p['pos'] == 'K' and (p['name'], p['team'], 'K') not in trial_map:
                karr = _synthesize_kicker_scores(req.game_id, p['team'])
                if karr is not None and len(karr) >= 100:
                    trial_map[(p['name'], p['team'], 'K')] = karr
                    trial_map[(p['team'], 'K')] = karr

    # Conditional re-scoring: draw ONLY from the box-selected iterations (see
    # ShowdownOptimizeRequest.iteration_filter) instead of uniformly from all
    # 1000 -- both our lineup(s) and the field below read off the same
    # `aligned` indices, so this reweights the joint game-environment draw for
    # everyone at once rather than needing a separate solve. Sampling uniformly
    # WITH replacement from the filtered set is the correct conditional
    # resample: each of the base 1000 iterations was equally likely a priori,
    # so restricting to a subset and drawing uniformly from it reproduces
    # exactly the "given the game lands in this box" distribution.
    # Iteration count comes from the game's own sim (10,000 or 1,000), not a
    # hardcoded 1000 -- the old bound drew only from the first 1,000 of a
    # 10,000-iteration week and silently dropped box-select ids >= 1000.
    n_index = int(game_df['iteration'].max()) + 1 if game_df is not None and not game_df.empty else 1000
    valid_filter = sorted({int(i) for i in req.iteration_filter if 0 <= int(i) < n_index}) if req.iteration_filter else None
    if valid_filter:
        aligned = rng.choice(np.array(valid_filter), size=n_stat_sims, replace=True)
        iteration_filter_frac = round(len(valid_filter) / float(n_index), 4)
    else:
        aligned = rng.integers(0, n_index, size=n_stat_sims)
        iteration_filter_frac = None

    def player_draws(p: dict, indices: np.ndarray) -> np.ndarray:
        # Explicit None checks -- trial_map values are numpy arrays, so
        # `a.get(...) or a.get(...)` raises "truth value ambiguous".
        arr = trial_map.get((p['name'], p['team'], p['pos']))
        if arr is None:
            arr = trial_map.get((p['team'], p['pos']))
        if arr is not None and len(arr) >= 100:
            safe = np.clip(indices, 0, len(arr) - 1)
            med = float(np.percentile(arr, 50))
            proj = p.get('projection', 10.0)
            if med > 1.0 and abs(proj - med) > 0.1:
                return arr[safe] * (proj / med)
            return arr[safe]
        g = np.random.default_rng(int(hashlib.md5(p['name'].encode()).hexdigest(), 16) % (2**32))
        pcts = p.get('dk_pcts_all')
        if pcts and len(pcts) == 101:
            # Independent draw from the player's own percentile curve. (Was
            # pcts[clip(indices, 0, 100)] -- iteration ids are 0..n_index-1,
            # not percentiles, so ~90-99% of draws clipped to p100, the max.)
            return np.array(pcts, dtype=float)[g.integers(0, 101, len(indices))]
        proj = p.get('projection', 10.0)
        return np.maximum(0.0, g.normal(proj, max(0.5, proj * 0.35), len(indices)))

    draw_cache = {i: player_draws(active[i], aligned) for i in range(n)}

    # ── Field: build once, score at aligned iterations ───────────────────
    # `field_vecs`       — EVERY field lineup's score in EVERY aligned iteration
    #                      (n_field x n_sims), used as `field_matrix` below to
    #                      rank our lineup against the actual simulated field
    #                      in that same iteration's game-environment, not a
    #                      single random opponent or a marginal cutoff blended
    #                      across every environment (see _compute_lineup_field_stats_batch).
    # `field_max_scores` — the single best build in the field at each iteration;
    #                      the bar our lineup must clear to "finish first" (see
    #                      `first_pct` below). Denominator is the FIELD_SAMPLE_K-lineup
    #                      synthetic field of distinct builds, not the raw contest
    #                      entry count — it answers "was this the best possible
    #                      build given how the game played out".
    field = _build_showdown_field(active, req.salary_cap, n_field=FIELD_SAMPLE_K, seed=own_seed)
    field_max_scores = None
    if field:
        # Every field lineup's full score vector across the aligned iterations.
        field_vecs = np.empty((len(field), n_stat_sims), dtype=float)
        for fi, lu in enumerate(field):
            field_vecs[fi] = 1.5 * draw_cache[lu['cpt']] + sum(draw_cache[j] for j in lu['flex'])
        field_max_scores = field_vecs.max(axis=0)
        field_matrix = field_vecs
    else:
        field_matrix = np.full((1, n_stat_sims), float(np.mean(projections)) * 6.0)

    # ── Payout structure ────────────────────────────────────────────────
    prize_pool = req.entry_fee * req.total_entries * 0.85
    if req.payout_structure:
        payout_structure = [t.dict() for t in req.payout_structure]
    else:
        payout_structure = _get_default_payout_structure(
            req.contest_type, prize_pool, req.paying_positions, req.total_entries)

    # ── Per-lineup stats ────────────────────────────────────────────────
    # All lineups ranked against the field in ONE batched pass (see
    # _compute_lineup_field_stats_batch) -- required to make a field this
    # large (FIELD_SAMPLE_K) affordable.
    all_lineup_draws = [
        1.5 * draw_cache[lu['sol']['cpt']] + sum(draw_cache[i] for i in lu['sol']['flex'])
        for lu in generated
    ]
    all_stats = _compute_lineup_field_stats_batch(
        all_lineup_draws, field_matrix, payout_structure,
        req.entry_fee, req.total_entries, req.paying_positions
    )

    results = []
    for lu, lineup_draws, stats in zip(generated, all_lineup_draws, all_stats):
        sol = lu['sol']
        cpt_own = active[sol['cpt']].get('cpt_ownership_pct') or 0.0
        flex_owns = [active[i].get('ownership_pct') or 0.0 for i in sol['flex']]
        # Rough duplication estimate: field_size * P(field builds this exact lineup).
        # ownership_pct is already a per-player marginal share of the
        # UNORDERED 5-FLEX group (pool sums to ~500% = 5 slots x 100%, see
        # _compute_showdown_ownership's docstring), not a per-labeled-slot
        # rate -- so multiplying the 5 marginals together already estimates
        # the unordered combination directly. A prior version of this line
        # also multiplied by 120 (5!) on the theory that ordering needed
        # correcting for; there's no ordering here to correct, so that
        # factor was simply inflating every estimate ~120x.
        p_exact = (cpt_own / 100.0)
        for o in flex_owns:
            p_exact *= (o / 100.0)
        dupe_est = round(req.total_entries * p_exact, 2)
        # "Finished first": iterations where this lineup outscored the best build
        # in the synthetic field (>= so a tie for the top counts). Reported both
        # as a rate and a raw count out of n_stat_sims.
        if field_max_scores is not None:
            first_count = int(np.count_nonzero(lineup_draws >= field_max_scores))
            first_pct = round(first_count / n_stat_sims * 100, 3)
        else:
            first_count, first_pct = 0, 0.0
        results.append({
            **stats,
            'first_pct': first_pct,
            'first_count': first_count,
            'n_sims': n_stat_sims,
            'players': [{
                'name': s['name'], 'pos': s['pos'], 'team': s['team'],
                'slot': s['slot'], 'salary': s['slot_salary'], 'base_salary': s['salary'],
                'projection': s['slot_projection'],
                'ownership_pct': (s.get('cpt_ownership_pct') if s['slot'] == 'CPT' else s.get('ownership_pct')),
                'ownership_source': s.get('ownership_source'),  # "model" | "heuristic" -- which one produced this number
                'dk_id': (s.get('dk_cpt_id') if s['slot'] == 'CPT' else s.get('dk_id')),
            } for s in lu['slots']],
            'total_salary': lu['total_salary'],
            'projected_score': lu['projected_score'],
            'total_ownership': lu['total_ownership'],
            'cpt_name': active[sol['cpt']]['name'],
            'dupe_est': dupe_est,
            'label': lu.get('label'),
            'over_salary_cap': lu['total_salary'] > req.salary_cap,
        })

    # ── Portfolio metrics ───────────────────────────────────────────────
    ng = len(generated)

    def members(g):
        return {g['sol']['cpt']} | set(g['sol']['flex'])

    sims = []
    for i in range(ng):
        for j in range(i + 1, ng):
            sims.append(len(members(generated[i]) & members(generated[j])) / 6.0)
    avg_corr = float(np.mean(sims)) if sims else 0.0
    elc = round(ng * (1.0 - avg_corr), 1)

    used = set()
    cpt_used = set()
    for g in generated:
        used |= members(g)
        cpt_used.add(g['sol']['cpt'])
    coverage = round(len(used) / max(n, 1), 3)

    portfolio_ev = float(np.mean([r['ev_pct'] for r in results]))
    for i, r in enumerate(results):
        if ng > 1:
            others = [len(members(generated[i]) & members(generated[j])) / 6.0
                      for j in range(ng) if j != i]
            r['portfolio_score'] = round((r['ev_pct'] - portfolio_ev) * 0.7
                                         + (1 - float(np.mean(others))) * 3.0, 2)
        else:
            r['portfolio_score'] = 0.0

    top1_vals = [r['top1_pct'] for r in results]
    top01_vals = [r['top01_pct'] for r in results]
    first_vals = [r['first_pct'] for r in results]

    has_kicker = any(p['pos'] == 'K' for p in active)
    notes = ([] if has_kicker else
             ['Kickers are not in the pool (sim engine does not model them yet) — add them manually on DK.'])
    if field_source == 'season_parquet':
        notes.append('EV / Top% / 1st% ran against the season-long sim (no DFS-week sim for '
                     f'week {req.week} yet) — starters may not match this week\'s availability.')
    elif field_source == 'independent_draws':
        notes.append('No sim parquet for this game — EV / Top% / 1st% used independent player draws '
                     '(no game-environment correlation). Treat them as rough.')

    return {
        'mode': 'manual' if manual else 'optimize',
        'lineups': results,
        'portfolio': {
            'total_ev_pct': round(portfolio_ev, 2),
            'effective_lineup_count': elc,
            'avg_correlation': round(avg_corr, 3),
            'coverage_score': coverage,
            'avg_total_ownership': round(float(np.mean([g['total_ownership'] for g in generated])), 1),
            'unique_captains': len(cpt_used),
            'n_generated': ng,
            'n_requested': req_n,
            'avg_top1_pct': round(float(np.mean(top1_vals)), 2) if top1_vals else 0.0,
            'best_top1_pct': round(float(np.max(top1_vals)), 2) if top1_vals else 0.0,
            'avg_top01_pct': round(float(np.mean(top01_vals)), 2) if top01_vals else 0.0,
            'best_top01_pct': round(float(np.max(top01_vals)), 2) if top01_vals else 0.0,
            'avg_first_pct': round(float(np.mean(first_vals)), 3) if first_vals else 0.0,
            'best_first_pct': round(float(np.max(first_vals)), 3) if first_vals else 0.0,
            'field_source': field_source,
            'n_sims': n_stat_sims,
            'iteration_filter_frac': iteration_filter_frac,
            'iteration_filter_n': len(valid_filter) if valid_filter else None,
        },
        'notes': notes,
    }


if __name__ == "__main__":
    import uvicorn
    uvicorn.run("app:app", host="127.0.0.1", port=8003, reload=True)

