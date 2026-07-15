"""
week1_2025.py
=============
# Status: live | v1.0.0 | 2026-06-22

Week 1 2025 NFL play-by-play data loader for the Historical Testing Lab.

Downloads the nfl_data_py PBP dataset, filters to scrimmage plays with complete
game-state information, and provides pre-computed home-team-reference KEP values
for the chess evaluator timeline chart.

Public functions:
    get_game_list()                   -> list[dict]  - 16 Week 1 2025 games
    get_game_plays(game_id, evaluator) -> list[dict]  - play states + home_kep per play
"""

import warnings
import numpy as np
warnings.filterwarnings("ignore")

try:
    import nfl_data_py as nfl
    _NFL_DATA_AVAILABLE = True
except ImportError:
    _NFL_DATA_AVAILABLE = False

# Cached at module level so repeated calls within the same process don't re-download.
_PBP_CACHE = None
_SCHEDULE_CACHE = None

SCRIMMAGE_TYPES = {"pass", "run", "qb_kneel", "qb_spike"}

REQUIRED_COLS = [
    "game_id", "play_id", "qtr", "game_seconds_remaining",
    "posteam", "home_team", "away_team",
    "score_differential", "posteam_score", "defteam_score",
    "down", "ydstogo", "yardline_100",
    "posteam_timeouts_remaining", "defteam_timeouts_remaining",
    "play_type", "desc",
]


def _load_week1_pbp():
    """Download + cache 2025 Week 1 PBP. Returns a list of row dicts (scrimmage plays only)."""
    global _PBP_CACHE
    if _PBP_CACHE is not None:
        return _PBP_CACHE

    if not _NFL_DATA_AVAILABLE:
        raise RuntimeError("nfl_data_py is not installed. Run: pip install nfl_data_py")

    pbp = nfl.import_pbp_data([2025])
    w1 = pbp[pbp["week"] == 1].copy()

    # Filter to scrimmage plays with complete state data.
    scrimmage = w1[
        w1["play_type"].isin(SCRIMMAGE_TYPES) &
        w1["down"].notna() &
        w1["ydstogo"].notna() &
        w1["yardline_100"].notna() &
        w1["game_seconds_remaining"].notna() &
        w1["score_differential"].notna() &
        w1["posteam"].notna()
    ].copy()

    rows = []
    for _, row in scrimmage[REQUIRED_COLS].iterrows():
        rows.append({
            "game_id":                    str(row["game_id"]),
            "play_id":                    int(row["play_id"]),
            "qtr":                        int(row["qtr"]),
            "game_seconds_remaining":     float(row["game_seconds_remaining"]),
            "posteam":                    str(row["posteam"]),
            "home_team":                  str(row["home_team"]),
            "away_team":                  str(row["away_team"]),
            "score_differential":         float(row["score_differential"]),
            "posteam_score":              int(row["posteam_score"]) if not (isinstance(row["posteam_score"], float) and np.isnan(row["posteam_score"])) else 0,
            "defteam_score":              int(row["defteam_score"]) if not (isinstance(row["defteam_score"], float) and np.isnan(row["defteam_score"])) else 0,
            "down":                       int(row["down"]),
            "ydstogo":                    float(row["ydstogo"]),
            "yardline_100":               float(row["yardline_100"]),
            "posteam_timeouts_remaining": float(row.get("posteam_timeouts_remaining", 3) or 3),
            "defteam_timeouts_remaining": float(row.get("defteam_timeouts_remaining", 3) or 3),
            "play_type":                  str(row["play_type"]),
            "desc":                       str(row.get("desc", "")),
        })

    _PBP_CACHE = rows
    return _PBP_CACHE


def _load_schedule():
    """Download + cache 2025 Week 1 schedule."""
    global _SCHEDULE_CACHE
    if _SCHEDULE_CACHE is not None:
        return _SCHEDULE_CACHE

    if not _NFL_DATA_AVAILABLE:
        raise RuntimeError("nfl_data_py is not installed.")

    sched = nfl.import_schedules([2025])
    w1 = sched[sched["week"] == 1]

    games = []
    for _, row in w1.iterrows():
        games.append({
            "game_id":    str(row["game_id"]),
            "home_team":  str(row["home_team"]),
            "away_team":  str(row["away_team"]),
            "gameday":    str(row["gameday"]),
            "home_score": int(row["home_score"]) if not (isinstance(row["home_score"], float) and np.isnan(row["home_score"])) else None,
            "away_score": int(row["away_score"]) if not (isinstance(row["away_score"], float) and np.isnan(row["away_score"])) else None,
        })

    _SCHEDULE_CACHE = sorted(games, key=lambda g: g["gameday"])
    return _SCHEDULE_CACHE


def get_game_list():
    """
    Returns the 16 Week 1 2025 game records sorted by game date.

    Output: list of dicts with keys: game_id, home_team, away_team, gameday, home_score, away_score
    """
    return _load_schedule()


def get_game_plays(game_id: str, evaluator):
    """
    Returns all scrimmage plays for game_id with KEP and EFSD computed in home-team reference.

    Inputs:
        game_id   : nfl_data_py game identifier (e.g. '2025_01_KC_LAC')
        evaluator : PositionalEvaluator instance (provides KEP converter and EFSD model)

    Output: list of dicts per play:
        play_id, qtr, game_seconds_remaining, posteam, home_team, away_team,
        down, ydstogo, yardline_100, score_differential,
        posteam_timeouts_remaining, defteam_timeouts_remaining,
        play_type, desc,
        kep_off,   <- KEP from offense (posteam) perspective
        home_kep,  <- KEP from home team perspective (positive = home winning)
        efsd_off,  <- EFSD (expected final score diff) from offense perspective
        home_efsd, <- EFSD from home team perspective (positive = home winning)
        ep         <- Expected Points for this state
    """
    plays = _load_week1_pbp()
    game_plays = [p for p in plays if p["game_id"] == game_id]

    results = []
    for play in game_plays:
        game_state = {
            "score_differential":         play["score_differential"],
            "game_seconds_remaining":     play["game_seconds_remaining"],
            "down":                       play["down"],
            "ydstogo":                    play["ydstogo"],
            "yardline_100":               play["yardline_100"],
            "posteam_timeouts_remaining": play["posteam_timeouts_remaining"],
            "defteam_timeouts_remaining": play["defteam_timeouts_remaining"],
            "receive_2h_ko":              0.0,
        }

        kep_off = float(evaluator.kep.kep_from_state(game_state))

        # Home-team reference: positive = home team is winning positionally.
        home_kep = kep_off if play["posteam"] == play["home_team"] else -kep_off

        # EFSD: expected final score differential (offense perspective, then home reference)
        efsd_off = float(evaluator.efsd_model.predict_efsd(game_state))
        home_efsd = efsd_off if play["posteam"] == play["home_team"] else -efsd_off

        goal_to_go = 1 if play["ydstogo"] >= play["yardline_100"] else 0
        try:
            ep = float(evaluator.ep_model.predict_expected_points(
                yardline_100=int(play["yardline_100"]),
                down=int(play["down"]),
                ydstogo=int(play["ydstogo"]),
                goal_to_go=goal_to_go,
            ))
        except Exception:
            ep = None

        results.append({
            **play,
            "kep_off":  round(kep_off, 3),
            "home_kep": round(home_kep, 3),
            "efsd_off": round(efsd_off, 3),
            "home_efsd": round(home_efsd, 3),
            "ep":       round(ep, 3) if ep is not None else None,
        })

    return results
