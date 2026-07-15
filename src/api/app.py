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
import json
import math
import itertools
import functools
import numpy as np
import pandas as pd
import pulp
from typing import Dict, List, Any, Optional, Tuple
from fastapi import FastAPI, HTTPException, Query
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

# -------------------------------------------------------------------------
# PATH RESOLUTION & DATA LOADERS
# -------------------------------------------------------------------------
BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SCHEDULE_CSV_PATH = os.path.join(BASE_DIR, "data", "external", "schedule_2025.csv")
TEAM_COACHES_PATH = os.path.join(BASE_DIR, "data", "dna", "team_to_coach_2025.json")
COORDINATOR_ATLAS_PATH = os.path.join(BASE_DIR, "data", "dna", "coordinator_atlas.json")
GAMES_CACHE_PATH = os.path.join(BASE_DIR, "data", "interim", "sim_results_2025_games.parquet")
PLAYERS_CACHE_PATH = os.path.join(BASE_DIR, "data", "interim", "sim_results_2025_players.parquet")

# Pre-load cached simulation data into memory and reload if files are updated on disk
ALL_GAMES_CACHED = None
ALL_PLAYERS_CACHED = None
LAST_LOADED_TIME_GAMES = 0.0
LAST_LOADED_TIME_PLAYERS = 0.0
WEEK_PROJECTIONS_CACHE = {}

# game_id -> sub-dataframe lookups, rebuilt whenever the parquet caches reload.
# run_simulation()'s cache-hit path used to filter the full ~11M-row players
# cache with a linear `== game_id` scan on every single call (~1s each, the
# dominant non-optimizer cost per /api/week_sim_results profiling) — these
# dicts turn that into an O(1) lookup instead.
GAMES_BY_GAME_ID = {}
PLAYERS_BY_GAME_ID = {}

# In-memory + disk cache for /api/week_sim_results — see that endpoint for details.
WEEK_SIM_RESULTS_CACHE = {}

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
        SIMULATE_RESPONSE_CACHE.clear()
        WEEK_SIM_RESULTS_CACHE.clear()
        if ALL_GAMES_CACHED is not None:
            GAMES_BY_GAME_ID = {gid: df for gid, df in ALL_GAMES_CACHED.groupby("game_id")}
        if ALL_PLAYERS_CACHED is not None:
            PLAYERS_BY_GAME_ID = {gid: df for gid, df in ALL_PLAYERS_CACHED.groupby("game_id")}
        print("Cleared week projections, simulate response, and week sim results caches due to data reload.")

# Initial load on startup
reload_cache_if_changed()


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
# DYNAMIC DFS SALARY & BASELINE GENERATORS
# -------------------------------------------------------------------------
def calculate_dfs_salary(pos: str, target_share: float, carry_share: float, cpoe: float = 0.0) -> int:
    """Generates realistic DFS salaries based on baseline player workloads."""
    base = 3000  # DFS Minimum
    if pos == "QB":
        # QBs range from $5,300 to $8,500 based on passing efficiency (CPOE)
        cpoe_modifier = max(0.0, min(1.0, (cpoe + 0.05) / 0.10))
        return int(base + 2300 + cpoe_modifier * 3200)
    elif pos == "RB":
        # RBs range from $4,000 to $8,500 based on rushing volume
        return int(base + carry_share * 11000)
    elif pos in ["WR", "TE", "WR/TE"]:
        # WRs/TEs range from $3,500 to $9,000 based on target share
        return int(base + target_share * 20000)
    return base

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
    ownership_proj: Optional[float] = 12.5
    pos: Optional[str] = "WR/TE"
    salary: Optional[int] = 3000


class SimulationRequest(BaseModel):
    away_team: str
    home_team: str
    year: int = 2025
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
    # 50 for direct user-facing "Run Engine" requests. /api/week_sim_results
    # sets this much lower — it's populating a baseline display value across
    # every game in a week, not a single deliberate user-requested run, and
    # this solve is by far the dominant per-request cost (pure-Python
    # branch-and-bound, ~80ms/iteration regardless of iteration count).
    optimizer_sample_cap: Optional[int] = 50

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

class PayoutTier(BaseModel):
    rank_start: int
    rank_end: int
    payout: float

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
    """Returns available weeks from the schedule."""
    if not os.path.exists(SCHEDULE_CSV_PATH):
        # Fallback if file missing
        return {"weeks": list(range(1, 19))}
    df = pd.read_csv(SCHEDULE_CSV_PATH)
    weeks = sorted(df["week"].unique().tolist())
    return {"weeks": weeks}

def get_week_salaries(week_games_df, year=2025) -> Dict[Tuple[str, str], int]:
    salaries = {}
    teams = set(week_games_df["away_team"].unique()).union(set(week_games_df["home_team"].unique()))
    qb_dna = load_json(os.path.join(BASE_DIR, "data", "dna", "qb_dna.json"))
    
    for team in teams:
        # Add defense
        salaries[("Defense", team)] = 3000
        
        roster_path = os.path.join(BASE_DIR, "data", "current_rosters", f"{team}_traits_{year}.json")
        if os.path.exists(roster_path):
            roster_data = load_json(roster_path)
            traits = roster_data.get("traits", {})
            for name, p_traits in traits.items():
                pos = p_traits.get("pos", "WR/TE")
                target_share = p_traits.get("target_share", 0.0)
                carry_share = p_traits.get("carry_share", 0.0)
                cpoe = 0.0
                if pos == "QB":
                    cpoe = qb_dna.get(name, {}).get("cpoe", 0.0)
                salaries[(name, team)] = calculate_dfs_salary(pos, target_share, carry_share, cpoe)
    return salaries

@app.get("/api/week_projections")
def get_week_projections(week: int = 1, year: int = 2025):
    """Aggregates all player simulations for the given week from the pre-loaded parquet cache."""
    global ALL_PLAYERS_CACHED
    reload_cache_if_changed()
    
    if ALL_PLAYERS_CACHED is None:
        raise HTTPException(
            status_code=503, 
            detail="Pre-loaded simulation cache not available. Check server startup logs."
        )
        
    cache_key = (week, year)
    if cache_key in WEEK_PROJECTIONS_CACHE:
        return WEEK_PROJECTIONS_CACHE[cache_key]

    # Check if a precomputed JSON cache exists on disk
    json_cache_path = os.path.join(BASE_DIR, "data", "interim", f"week_{week}_full_projections.json")
    if os.path.exists(json_cache_path):
        try:
            import json
            print(f"Loading precomputed projections for week {week} from JSON cache...")
            with open(json_cache_path, "r") as f:
                res = json.load(f)
            WEEK_PROJECTIONS_CACHE[cache_key] = res
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
    
    # Filter the cached player data for games of interest
    wp = ALL_PLAYERS_CACHED[ALL_PLAYERS_CACHED["game_id"].isin(week_game_ids)].copy()
    if wp.empty:
        return {"week": week, "players": [], "games": []}
        
    # Generate salaries lookup
    salaries = get_week_salaries(week_games_df, year)
    
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
    
    # Map game information (opponent, game_id, slate information)
    team_info_lookup = {}
    for _, row in week_games_df.iterrows():
        away = row["away_team"]
        home = row["home_team"]
        game_id = row["game_id"]
        
        weekday = str(row.get("weekday", "Sunday"))
        gametime = str(row.get("gametime", "13:00"))
        is_main = True
        if weekday in ["Thursday", "Monday", "Friday", "Saturday"]:
            is_main = False
        elif weekday == "Sunday" and gametime >= "20:00":
            is_main = False
            
        team_info_lookup[away] = {"opponent": f"@{home}", "game_id": game_id, "is_main": is_main}
        team_info_lookup[home] = {"opponent": f"vs {away}", "game_id": game_id, "is_main": is_main}
        
    # Calculate DFS Optimal, Boom, and Value rates trial-by-trial for traditional slate
    optimal_counts = {p: 0 for p in salaries.keys()}
    boom_counts = {p: 0 for p in salaries.keys()}
    value_counts = {p: 0 for p in salaries.keys()}
    
    unique_iterations = wp["iteration"].unique()
    num_iterations = len(unique_iterations) if len(unique_iterations) > 0 else 1
    
    player_keys = list(salaries.keys())
    player_salaries = np.array([salaries[pk] for pk in player_keys])
    
    # Get clean positions and teams mapping
    player_positions = {}
    for (p, t, pos), _ in wp.groupby(["Player", "Team", "Pos"]):
        player_positions[(p, t)] = pos
        
    player_positions_list = [player_positions.get(pk, "WR") for pk in player_keys]
    player_teams_list = [pk[1] for pk in player_keys]
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
        
        # Calculate Boom % (score >= 30) and Value % (score >= 3x salary)
        sal = salaries.get((p, t), 3000)
        if score >= 30.0:
            boom_counts[(p, t)] = boom_counts.get((p, t), 0) + 1
        if score >= 3.0 * (sal / 1000.0):
            value_counts[(p, t)] = value_counts.get((p, t), 0) + 1
            
    from src.nfl_sim.optimizer import solve_traditional_iteration
    # Sub-sample iterations if they exceed 50 to reduce CPU stress and latency
    solve_iterations = unique_iterations
    if len(unique_iterations) > 50:
        step = len(unique_iterations) // 50
        solve_iterations = unique_iterations[::step][:50]
        
    num_solve_iterations = len(solve_iterations) if len(solve_iterations) > 0 else 1
    
    for it in solve_iterations:
        scores_arr = np.array([iter_scores[it].get(pk, 0.0) for pk in player_keys])
        opt_lineup = solve_traditional_iteration(player_keys, player_salaries, clean_positions, player_teams_list, scores_arr)
        for pk in opt_lineup:
            optimal_counts[pk] = optimal_counts.get(pk, 0) + 1

    players_list = []
    for _, row in merged.iterrows():
        name = row["Player"]
        team = row["Team"]
        pos = row["Pos"]
        
        t_info = team_info_lookup.get(team, {"opponent": "BYE", "game_id": "", "is_main": False})
        salary = salaries.get((name, team), 3000)
        
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
        import json
        json_cache_path = os.path.join(BASE_DIR, "data", "interim", f"week_{week}_full_projections.json")
        os.makedirs(os.path.dirname(json_cache_path), exist_ok=True)
        with open(json_cache_path, "w") as f:
            json.dump(result, f)
        print(f"Saved projections for week {week} to JSON cache.")
    except Exception as e:
        print(f"Error saving projections JSON cache: {e}")

    WEEK_PROJECTIONS_CACHE[cache_key] = result
    return result

@app.get("/api/week_sim_results")
def get_week_sim_results(week: int = 1, year: int = 2025):
    """Prepopulates every game in the week from the cached parquet sim data
    via run_simulation()'s own baseline cache-hit path, so the DFS site can
    show results immediately on load instead of requiring a per-game
    'Run Engine' click. Reuses get_rosters() for baseline overrides and
    run_simulation() for the actual computation — no new sim logic here."""
    reload_cache_if_changed()

    cache_key = (week, year)
    if cache_key in WEEK_SIM_RESULTS_CACHE:
        return WEEK_SIM_RESULTS_CACHE[cache_key]

    json_cache_path = os.path.join(BASE_DIR, "data", "interim", f"week_{week}_sim_results.json")
    if os.path.exists(json_cache_path):
        try:
            with open(json_cache_path, "r") as f:
                res = json.load(f)
            WEEK_SIM_RESULTS_CACHE[cache_key] = res
            return res
        except Exception as e:
            print(f"Error loading week sim results JSON cache: {e}")

    if not os.path.exists(SCHEDULE_CSV_PATH):
        raise HTTPException(status_code=404, detail="Schedule CSV file not found.")

    sched_df = pd.read_csv(SCHEDULE_CSV_PATH)
    week_games_df = sched_df[(sched_df["week"] == week) & (sched_df["game_type"] == "REG")]

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
                    ownership_proj=p.get("ownership_proj", 12.5),
                    pos=p["pos"],
                    salary=p["salary"],
                ))

        # Baseline values match each team's cached defaults exactly, so
        # run_simulation() takes its existing fast cache-hit path.
        req = SimulationRequest(
            away_team=away,
            home_team=home,
            year=year,
            iterations=1000,
            apply_weighting=False,
            team_overrides=team_overrides,
            player_overrides=player_overrides,
            use_cached_defaults=True,
            optimizer_sample_cap=8,
        )
        sim_res = run_simulation(req)
        return {
            "game_id": game_id,
            "away_team": away,
            "home_team": home,
            "summary": sim_res["summary"],
            "projections": sim_res["projections"],
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

    result = {"week": week, "games": games_results}

    try:
        os.makedirs(os.path.dirname(json_cache_path), exist_ok=True)
        with open(json_cache_path, "w") as f:
            json.dump(result, f)
        print(f"Saved week {week} sim results to JSON cache.")
    except Exception as e:
        print(f"Error saving week sim results JSON cache: {e}")

    WEEK_SIM_RESULTS_CACHE[cache_key] = result
    return result

@app.get("/api/games")
def get_games(week: int = 1):
    """Returns schedule, Vegas lines, dynamic team records, and DFS slate tags for a week."""
    if not os.path.exists(SCHEDULE_CSV_PATH):
        raise HTTPException(status_code=404, detail="Schedule CSV file not found.")
    
    df = pd.read_csv(SCHEDULE_CSV_PATH)
    
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
        
        # Simple dynamic slate tagging:
        # Thursday Night, Sunday Night (20:20), Monday Night (20:15) are NOT on Main slate
        weekday = str(row.get("weekday", "Sunday"))
        gametime = str(row.get("gametime", "13:00"))
        
        is_main = True
        if weekday in ["Thursday", "Monday", "Friday", "Saturday"]:
            is_main = False
        elif weekday == "Sunday" and gametime >= "20:00":
            is_main = False
            
        away_team = str(row["away_team"])
        home_team = str(row["home_team"])
        
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
            "dk_main": is_main,
            "fd_main": is_main
        })
        
    return {"week": week, "games": games}

@app.get("/api/rosters")
def get_rosters(away: str, home: str, year: int = 2025):
    """Serves team rosters, base DNA, and generated salaries for a matchup."""
    away = away.strip().upper()
    home = home.strip().upper()
    reload_cache_if_changed()
    team_coaches = load_json(TEAM_COACHES_PATH)
    coord_atlas = load_json(COORDINATOR_ATLAS_PATH)
    
    # Find game_id for this matchup from schedule to query cached sims
    game_id = None
    if os.path.exists(SCHEDULE_CSV_PATH):
        try:
            sched_df = pd.read_csv(SCHEDULE_CSV_PATH)
            match = sched_df[((sched_df["away_team"] == away) & (sched_df["home_team"] == home)) |
                             ((sched_df["away_team"] == home) & (sched_df["home_team"] == away))]
            if not match.empty:
                game_id = match.iloc[0]["game_id"]
        except Exception as e:
            print(f"Error finding game_id for rosters: {e}")

    result = {}
    for team in [away, home]:
        roster_path = os.path.join(BASE_DIR, "data", "current_rosters", f"{team}_traits_{year}.json")
        if not os.path.exists(roster_path):
            raise HTTPException(status_code=404, detail=f"Roster traits not found for team {team}.")
            
        roster_data = load_json(roster_path)
        # Shallow copy — load_json() is memoized (see its docstring), so this
        # must not mutate the shared cached dict via the del below.
        traits = dict(roster_data.get("traits", {}))

        # Keep only the starting QB and discard backups to prevent UI clutter and split stats
        qbs = [p for p, t in traits.items() if t.get("pos") == "QB"]
        if len(qbs) > 1:
            qb_dna = load_json(os.path.join(BASE_DIR, "data", "dna", "qb_dna.json"))
            starter_qb = max(qbs, key=lambda p: qb_dna.get(p, {}).get("total_attempts", 0))
            for qb in qbs:
                if qb != starter_qb:
                    del traits[qb]
        
        # Load coach baseline details
        coach_name = team_coaches.get(team, "Unknown")
        coach_proe = coord_atlas.get("off_proe", {}).get(coach_name, 0.0)

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
            
            salary = calculate_dfs_salary(pos, target_share, carry_share, cpoe)
            
            players_list.append({
                "name": name,
                "pos": pos,
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
            "roster": sorted(players_list, key=lambda x: x["salary"], reverse=True)
        }
        
    return result

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
    Returns the sequence of per-play KEP/EP evaluations for a live or recent game.

    Fetches the ESPN play-by-play stream, parses every scrimmage play into a game
    state, and computes KEP (clock-aware positional security) and EP (situational
    field value) for each. This is a lightweight per-play lookup — NOT a per-play
    drive simulation — so it stays fast across a full game.
    """
    from src.live.main import get_game_pbp
    from src.live.espn_adapter import parse_plays_to_states

    pbp = get_game_pbp(game_id)
    if not pbp:
        raise HTTPException(status_code=502, detail=f"Could not fetch play-by-play for game {game_id} from ESPN.")

    # Extract team abbreviations from the pbp header competitors.
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
        print(f"Error extracting team abbreviations for {game_id}: {e}")

    # Flatten previous + current drives into a single play list (mirrors live daemon).
    drives = pbp.get("drives", {}) or {}
    plays = []
    for d in drives.get("previous", []) or []:
        plays.extend(d.get("plays", []) or [])
    current = drives.get("current", {}) or {}
    if current.get("plays"):
        plays.extend(current.get("plays", []) or [])

    states = parse_plays_to_states(plays, home_abbr=home_abbr, away_abbr=away_abbr)

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

    if not bypass_cache and game_id and ALL_PLAYERS_CACHED is not None:
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
                            req_starter = max(req_team_qbs, key=lambda x: x.salary).name
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
                            req_starter_rb = max(req_team_rbs, key=lambda x: x.salary).name
                            def normalize_name(n):
                                return "".join(c for c in n.lower() if c.isalnum())
                            if normalize_name(cache_starter_rb) != normalize_name(req_starter_rb):
                                print(f"Starter RB mismatch for {team}: cache={cache_starter_rb}, request={req_starter_rb}. Bypassing cache.")
                                bypass_cache = True
                                break
        except Exception as e:
            print(f"Error validating cache starters: {e}")

    # Try loading from cache
    if not bypass_cache and game_id and ALL_GAMES_CACHED is not None and ALL_PLAYERS_CACHED is not None:
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
                starter_qb = max(team_qbs, key=lambda x: x.salary).name
                # Set starter QB's status to active and others on the roster to inactive
                for p_name, p_traits in sim.rosters[team].items():
                    if p_traits.get("pos") == "QB":
                        p_traits["status"] = "active" if p_name == starter_qb else "inactive"
                
            team_rbs = [po for po in req.player_overrides if po.team == team and po.pos == "RB"]
            if team_rbs:
                starter_rb = max(team_rbs, key=lambda x: x.salary).name
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
    
    player_df["dk_score"] = (dk_p_yds + dk_p_td - dk_int + dk_p_bonus + 
                             dk_r_yds + dk_r_td + dk_r_bonus + 
                             dk_rec + dk_rec_yds + dk_rec_td + dk_rec_bonus - 
                             dk_fumbles).round(2)
    
    fd_p_yds = player_df["pYds"] * 0.04
    fd_p_td = player_df["pTD"] * 4
    fd_int = player_df["int"] * 2
    fd_r_yds = player_df["rYds"] * 0.1
    fd_r_td = player_df["rTD"] * 6
    fd_rec = player_df["rec"] * 0.5
    fd_rec_yds = player_df["recYds"] * 0.1
    fd_rec_td = player_df["recTD"] * 6
    fd_fumbles = player_df["fumbles"] * 2.0
    
    player_df["fd_score"] = (fd_p_yds + fd_p_td - fd_int + 
                             fd_r_yds + fd_r_td + 
                             fd_rec + fd_rec_yds + fd_rec_td - 
                             fd_fumbles).round(2)
    
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
    
    # Build a lookup for salaries to include in the final response
    salaries = {}
    for team in [req.away_team, req.home_team]:
        for name, p_traits in sim.rosters[team].items():
            pos = p_traits.get("pos", "WR/TE")
            target_share = p_traits.get("target_share", 0.0)
            carry_share = p_traits.get("carry_share", 0.0)
            cpoe = 0.0
            if pos == "QB":
                cpoe = sim.dna["qb"].get(name, {}).get("cpoe", 0.0)
            salaries[name] = calculate_dfs_salary(pos, target_share, carry_share, cpoe)
    
    player_ownership = {po.name: po.ownership_proj for po in req.player_overrides if po.ownership_proj is not None}
    
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

    # Calculate DFS Optimal, Boom, and Value rates trial-by-trial for simulated game (Showdown)
    optimal_counts = {p: 0 for p in salaries.keys()}
    optimal_cpt_counts = {p: 0 for p in salaries.keys()}
    optimal_flex_counts = {p: 0 for p in salaries.keys()}
    boom_counts = {p: 0 for p in salaries.keys()}
    value_counts = {p: 0 for p in salaries.keys()}
    
    unique_iterations = updated_player_df["iteration"].unique()
    num_iterations = len(unique_iterations) if len(unique_iterations) > 0 else 1
    
    player_names = list(salaries.keys())
    player_salaries = np.array([salaries[p] for p in player_names])
    
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
        
        # Calculate Boom % (score >= 30) and Value % (score >= 3x salary)
        sal = salaries.get(p, 3000)
        if score >= 30.0:
            boom_counts[p] = boom_counts.get(p, 0) + 1
        if score >= 3.0 * (sal / 1000.0):
            value_counts[p] = value_counts.get(p, 0) + 1
            
    from src.nfl_sim.optimizer import solve_showdown_iteration
    # Sub-sample iterations if they exceed the cap to reduce CPU stress and
    # latency (same pattern as get_week_projections' solve_traditional_iteration
    # loop — this per-iteration branch-and-bound solve, not the Monte Carlo sim
    # itself, is what made the first /api/simulate hit for a game take up to
    # minutes). req.optimizer_sample_cap defaults to 50 for direct user
    # requests; /api/week_sim_results sets it much lower for bulk prepopulation.
    sample_cap = max(1, req.optimizer_sample_cap or 50)
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
        
        # Clean slot position (e.g. WR1 -> WR, TE2 -> TE, RB1 -> RB) to match frontend filters
        pos_clean = slot_pos
        for base_pos in ["QB", "RB", "WR", "TE", "DST"]:
            if slot_pos.startswith(base_pos):
                pos_clean = base_pos
                break
                
        salary = salaries.get(name, 3000)
        
        dk_pts = row["dk_score_avg"]
        fd_pts = row["fd_score_avg"]
        
        own_val = player_ownership.get(name, 12.5)
        own_val = own_val if own_val is not None else 12.5
        
        p_pcts = player_percentiles.get(name, {})
        
        final_projections.append({
            "name": name,
            "pos": pos_clean,
            "team": team,
            "salary": salary,
            "dk_points": dk_pts,
            "fd_points": fd_pts,
            "dk_value": round(dk_pts / (salary / 1000.0), 2) if salary > 0 else 0.0,
            "fd_value": round(fd_pts / (salary / 1000.0), 2) if salary > 0 else 0.0,
            "ownership_proj": own_val,
            "ownership_leverage": round(dk_pts / own_val, 2) if own_val > 0 else 0.0,
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
    away_scores = game_df["off_score"].values
    home_scores = game_df["def_score"].values
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
        "score_density": density_chart
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


def _compute_ownership(players: list) -> list:
    """Compute synthetic V1 ownership using two-factor softmax + log-normal noise."""
    POS_WEIGHTS = {'QB': 0.85, 'RB': 1.30, 'WR': 1.00, 'TE': 0.90, 'DST': 0.65}

    for p in players:
        pw = POS_WEIGHTS.get(p['pos'], 1.0)
        sal_k = max(p['salary'], 1) / 1000.0
        p['_value_score'] = (p['projection'] / sal_k) * pw

    # Chalk boost: top 10% by value score get 1.4x
    scores = [p['_value_score'] for p in players]
    threshold = np.percentile(scores, 90) if len(scores) >= 10 else max(scores)
    for p in players:
        if p['_value_score'] >= threshold:
            p['_value_score'] *= 1.4

    # Softmax with temperature T=1.5, scaled to sum=900
    T = 1.5
    vals = np.array([p['_value_score'] for p in players], dtype=float)
    vals = vals - vals.max()  # numerical stability
    exp_vals = np.exp(vals / T)
    softmax = exp_vals / (exp_vals.sum() + 1e-9)
    raw_ownership = softmax * 900.0

    # Log-normal noise
    rng = np.random.default_rng()
    for i, p in enumerate(players):
        base = raw_ownership[i]
        if base > 25:
            sigma = 0.25
        elif base > 8:
            sigma = 0.35
        else:
            sigma = 0.45
        noise = float(np.exp(rng.normal(0, sigma)))
        p['ownership_pct'] = max(0.5, round(base * noise, 1))
        del p['_value_score']

    return players


def _solve_lineup_ilp(
    players: list,
    draw_scores: np.ndarray,
    salary_cap: int,
    prior_lineups: list,
    min_unique: int,
    include_dst_unique: bool,
    max_exposure: float,
    n_total: int,
    locked_indices: set,
    excluded_indices: set
) -> Optional[list]:
    """Solve a single ILP lineup given a draw of scores."""
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
            max_apps = max(1, int(np.ceil(max_exposure * n_total)))
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


def _get_default_payout_structure(contest_type: str, prize_pool: float, paying_positions: int, total_entries: int) -> list:
    """Generate a default payout structure based on contest type."""
    if contest_type == 'extreme_top_heavy':
        # Bat Flip style: top 3 = ~42% of pool
        tiers = [
            (1, 1, 0.286), (2, 2, 0.086), (3, 3, 0.057), (4, 4, 0.029),
            (5, 5, 0.014), (6, 7, 0.0075), (8, 10, 0.0045), (11, 25, 0.003),
            (26, 50, 0.0015), (51, 100, 0.001)
        ]
    elif contest_type == 'top_heavy':
        # Rally Cap style
        tiers = [
            (1, 1, 0.200), (2, 2, 0.093), (3, 3, 0.047), (4, 4, 0.027),
            (5, 5, 0.013), (6, 6, 0.0093), (7, 7, 0.0067), (8, 8, 0.0053),
            (9, 10, 0.004), (11, 15, 0.0027), (16, 20, 0.002), (21, 50, 0.0013),
            (51, 100, 0.001)
        ]
    elif contest_type == 'flat':
        # Home Plate style
        tiers = [
            (1, 1, 0.107), (2, 2, 0.071), (3, 3, 0.057), (4, 4, 0.043),
            (5, 5, 0.029), (6, 6, 0.021), (7, 8, 0.017), (9, 11, 0.014),
            (12, 15, 0.011), (16, 20, 0.010), (21, 30, 0.0086), (31, 55, 0.0071),
            (56, 96, 0.005)
        ]
    elif contest_type == 'cash':
        # 50/50 style: top 50% wins 1.9x
        cutoff = max(1, int(total_entries * 0.50))
        return [{'rank_start': 1, 'rank_end': cutoff, 'payout': prize_pool * 1.9 / cutoff}]
    else:
        tiers = [(1, 1, 0.200), (2, 10, 0.050), (11, 50, 0.010), (51, paying_positions, 0.002)]

    structure = []
    for r_start, r_end, pct in tiers:
        if r_start > paying_positions:
            break
        r_end = min(r_end, paying_positions)
        count = r_end - r_start + 1
        total_pct = pct * count
        per_entry = (prize_pool * pct) if count == 1 else (prize_pool * total_pct / count)
        structure.append({'rank_start': r_start, 'rank_end': r_end, 'payout': round(per_entry, 2)})

    # Fill remaining paying positions with min payout (entry fee recovery)
    if structure:
        last_covered = structure[-1]['rank_end']
        if last_covered < paying_positions:
            min_payout = prize_pool * 0.0008
            structure.append({'rank_start': last_covered + 1, 'rank_end': paying_positions, 'payout': round(min_payout, 2)})

    return structure


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


def _compute_lineup_stats_direct(
    lineup_score_draws: np.ndarray,
    field_scores: np.ndarray,
    payout_structure: list,
    entry_fee: float,
    total_entries: int,
    paying_positions: int,
) -> dict:
    """Compute ITM%, Top1%, Top0.1%, EV%, and lineup percentile/volatility stats using already computed lineup draws."""
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

    # Filter excluded players
    active_players = [p.dict() for p in req.players if not p.excluded]
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

    # Compute ownership if not provided
    for p in active_players:
        if p.get('ownership_pct') is None:
            pass  # will compute below
    active_players = _compute_ownership(active_players)

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

    max_attempts = req_n * 5  # allow extra attempts for failed ILP solves
    attempts = 0

    while len(generated_lineups) < req_n and attempts < max_attempts:
        attempts += 1

        # Draw correlated scores for ILP — uses ilp_scores (gpp-blended ceiling)
        # NOT projections (P50), so the ILP picks players with real ceiling.
        if width_mult > 0:
            z = rng.standard_normal(n)
            perturbation = L @ z
            draw_scores = np.maximum(0, ilp_scores + width_mult * perturbation)
        else:
            draw_scores = ilp_scores.copy()

        selected_indices = _solve_lineup_ilp(
            active_players, draw_scores,
            req.salary_cap, prior_lineups,
            req.min_unique_players, req.include_dst_in_unique,
            req.max_exposure, req_n,
            locked_indices, excluded_indices
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

    by_pos: dict = {'QB': [], 'RB': [], 'WR': [], 'TE': [], 'DST': []}
    for p in active_players:
        pos = p.get('pos', '')
        if pos in by_pos:
            by_pos[pos].append(p)

    import random

    def _pick_one(pool: list) -> Optional[dict]:
        """Uniform random choice pick from pool (since custom ownership is unavailable)."""
        if not pool:
            return None
        return random.choice(pool)

    def build_field_lineup() -> Optional[list]:
        """Construct one valid DK-format lineup: 1QB/2RB/3WR/1TE/1FLEX/1DST.
        Uses ownership-weighted selection and salary-cap awareness.
        """
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
                    # Salary cap too tight — relax constraint for field sims
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

        # FLEX: any RB/WR/TE not yet used
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

    # Load cache data for week reg matchup players to index trial alignment
    # We want a map: (name, team, pos) -> 1000 trial scores (unsorted)
    # If not in cache or cached version doesn't have 1000 trials, we fall back to normal approximation.
    trial_scores_map = {}
    try:
        if ALL_PLAYERS_CACHED is not None:
            # Get game IDs for this week's active players
            active_game_ids = set()
            for p in active_players:
                # We need to find this player in ALL_PLAYERS_CACHED
                # Let's filter players df for the players in our request
                pass
            
            # Group by player/team/pos and select dk_score values per iteration
            # Note: Parquet contains 1000 iterations (usually 0 to 999) per player per game.
            # Let's pivot/group by to retrieve iteration-aligned values
            # To be efficient, we only slice the df for relevant teams/players
            all_teams = list({p['team'] for p in active_players})
            subset_df = ALL_PLAYERS_CACHED[ALL_PLAYERS_CACHED["Team"].isin(all_teams)]
            for (player_name, team_name, pos_name), group in subset_df.groupby(["Player", "Team", "Pos"]):
                # Sort by iteration to make sure it's aligned (0-999)
                sorted_group = group.sort_values("iteration")
                # Format key name: if pos is DST, standard lookup uses '[Team] DST' or 'Defense' depending on payload.
                # Standard payload name from frontend for defense is 'Defense' but standard pos is 'DST'
                p_name_key = f"{team_name} DST" if pos_name == "DST" else player_name
                trial_scores_map[(p_name_key, team_name, pos_name)] = sorted_group["dk_score"].values
                # Also store under generic "Defense" in case frontend references name: "Defense"
                if pos_name == "DST":
                    trial_scores_map[("Defense", team_name, pos_name)] = sorted_group["dk_score"].values
    except Exception as e:
        print(f"Error loading trial aligned scores: {e}")

    rng = np.random.default_rng()
    # Sample aligned iteration index (10000 times from 0-999)
    # For players who are missing or custom-added, we can simulate their values
    # but still use the same iteration index as a seed to allow correlation or simulate
    # based on projection.
    aligned_indices = rng.integers(0, 1000, size=n_stat_sims)

    def get_player_trial_scores(p: dict, indices: np.ndarray) -> np.ndarray:
        key = (p['name'], p['team'], p['pos'])
        if key in trial_scores_map and len(trial_scores_map[key]) >= 1000:
            arr = trial_scores_map[key]
            # Clip indices just in case
            safe_idxs = np.clip(indices, 0, len(arr) - 1)
            # If the user edited the projection, scale the trials proportionally
            original_median = np.percentile(arr, 50)
            proj = p.get('projection', 10.0)
            if original_median > 1.0 and abs(proj - original_median) > 0.1:
                scale = proj / original_median
                return arr[safe_idxs] * scale
            return arr[safe_idxs]
        else:
            # Fallback normal draw using a deterministic function of the iteration index to stay aligned
            proj = p.get('projection', 10.0)
            std = max(0.5, proj * 0.35)
            # Generate pseudo-random normal based on index + hash seed
            scores = np.zeros(len(indices))
            for i, idx in enumerate(indices):
                gen = np.random.default_rng(idx + hash(p['name']) % 10000)
                scores[i] = max(0.0, gen.normal(proj, std))
            return scores

    field_scores = np.zeros(n_stat_sims)
    n_failures = 0
    
    # Pre-draw field lineups and sample them
    for sim_idx in range(n_stat_sims):
        lu = build_field_lineup()
        iter_idx = aligned_indices[sim_idx]
        if lu is not None:
            score_sum = 0.0
            for p in lu:
                # Sample the score for this specific player at this specific iteration
                key = (p['name'], p['team'], p['pos'])
                if key in trial_scores_map and len(trial_scores_map[key]) > iter_idx:
                    score_sum += trial_scores_map[key][iter_idx]
                else:
                    proj = p.get('projection', 10.0)
                    std = max(0.5, proj * 0.35)
                    # Draw pseudo-randomly aligned to the iteration index
                    gen = np.random.default_rng(iter_idx + hash(p['name']) % 10000)
                    score_sum += max(0.0, gen.normal(proj, std))
            field_scores[sim_idx] = score_sum
        else:
            n_failures += 1
            # Fallback: estimate from average projection of pool
            avg_proj = float(np.mean([p.get('projection', 10.0) for p in active_players]))
            gen = np.random.default_rng(iter_idx)
            field_scores[sim_idx] = max(0.0, float(gen.normal(avg_proj * 9, avg_proj * 9 * 0.15)))

    # Evaluate each lineup using aligned simulations
    lineup_results = []

    for lu in generated_lineups:
        # Calculate lineup score draws aligned with the simulated trials
        lineup_score_draws = np.zeros(n_stat_sims)
        for p in lu['slots']:
            lineup_score_draws += get_player_trial_scores(p, aligned_indices)

        # Pass precomputed aligned draws into _compute_lineup_stats or run inline
        # Let's adapt _compute_lineup_stats signature to accept lineup_score_draws directly
        # to ensure perfect correlation alignment!
        stats = _compute_lineup_stats_direct(
            lineup_score_draws, field_scores, payout_structure,
            req.entry_fee, req.total_entries, req.paying_positions
        )
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
                'dk_pcts_all': p.get('dk_pcts_all')
            } for p in lu['slots']],
            'total_salary': lu['total_salary'],
            'projected_score': lu['projected_score']
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

    return {
        'lineups': lineup_results,
        'portfolio': {
            'total_ev_pct': round(portfolio_ev, 2),
            'effective_lineup_count': elc,
            'avg_correlation': round(avg_correlation, 3),
            'coverage_score': coverage_score,
            'n_generated': n_gen,
            'n_requested': req_n
        }
    }


if __name__ == "__main__":
    import uvicorn
    uvicorn.run("app:app", host="127.0.0.1", port=8003, reload=True)

