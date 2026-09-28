"""Real per-player, per-game 2026 stat lines for the Player Projections evaluation.

Source (2026-09-25): nflverse's weekly player-stats release,
  https://github.com/nflverse/nflverse-data/releases/download/stats_player/stats_player_week_{year}.csv
nfl_data_py's import_weekly_data() still 404s for 2026 -- that library is
deprecated and points at the retired `player_stats` release; the newer
`stats_player` release IS published and updated during the season. Its
game_id matches ours (2026_03_ATL_GB) and player_id is the GSIS id our
roster traits files already carry, so the join is exact.

Cache: data/eval/{year}/player_actuals_week.parquet -- regeneratable (just
re-download), rebuilt by refresh_player_actuals(). Pulled by hand after games
finish (same manual pattern as the Vegas-lines refresh), or from the
Evaluation tab's refresh button.

Columns are renamed to the SIM's stat names (pYds, rTD, recYds, ...) so the
evaluator compares like with like. dk_score is computed with the sim's own
src/nfl_sim/scoring.calculate_fantasy_points -- note that function subtracts
1 per fumble (all fumbles, as the sim counts them), where real DK only
subtracts fumbles LOST; we match the sim so the comparison is apples to
apples. 2-pt conversions / return TDs are excluded on both sides (the engine
doesn't model PATs or return TDs).
"""
import os

import pandas as pd

from src.evaluation.line_history import BASE_DIR
from src.nfl_sim.scoring import calculate_fantasy_points

SOURCE_URL = ("https://github.com/nflverse/nflverse-data/releases/download/"
              "stats_player/stats_player_week_{year}.csv")
TEAM_ABBR_FIX = {"LAR": "LA"}   # nflverse sometimes uses LAR; the schedule/sims use LA

# nflverse column -> sim column
STAT_MAP = {
    "attempts": "pAtt", "completions": "pCmp", "passing_yards": "pYds", "passing_tds": "pTD",
    "passing_interceptions": "int", "sacks_suffered": "sacks_taken",
    "carries": "rAtt", "rushing_yards": "rYds", "rushing_tds": "rTD",
    "targets": "targets", "receptions": "rec", "receiving_yards": "recYds", "receiving_tds": "recTD",
}
FUMBLE_COLS = ["sack_fumbles", "rushing_fumbles", "receiving_fumbles"]


def cache_path(year, base_dir=BASE_DIR):
    """Inputs: year, base_dir. Output: str path of the cached actuals parquet."""
    return os.path.join(base_dir, "data", "eval", str(year), "player_actuals_week.parquet")


def normalize_actuals(raw):
    """Reshape a raw nflverse stats_player_week frame into sim-shaped rows.

    Inputs: raw DataFrame (nflverse columns).
    Output: DataFrame, one row per (game_id, player_id): game_id, week, team,
      player_id, player_name, position, the sim stat columns, fumbles, dk_score.
    REG season only. Missing stat cells -> 0 (nflverse leaves blanks for
    "didn't record one").
    """
    df = raw[raw["season_type"] == "REG"].copy() if "season_type" in raw else raw.copy()
    out = pd.DataFrame({
        "game_id": df["game_id"], "week": df["week"].astype(int),
        "team": df["team"].replace(TEAM_ABBR_FIX), "player_id": df["player_id"],
        "player_name": df.get("player_display_name", df.get("player_name")),
        "position": df["position"],
    })
    for src, dst in STAT_MAP.items():
        out[dst] = pd.to_numeric(df[src], errors="coerce").fillna(0) if src in df else 0
    out["fumbles"] = sum(pd.to_numeric(df[c], errors="coerce").fillna(0) for c in FUMBLE_COLS if c in df)
    out["dk_score"] = [calculate_fantasy_points(r, "DK") for r in out.to_dict(orient="records")]
    return out.reset_index(drop=True)


def refresh_player_actuals(year=2026, base_dir=BASE_DIR):
    """Download the season's weekly player stats and overwrite the cache.
    Inputs: year, base_dir. Output: dict {rows, weeks, games, updated_at}.
    Written atomically (tmp + replace) so a concurrent API read never sees a
    half-written file."""
    raw = pd.read_csv(SOURCE_URL.format(year=year), low_memory=False)
    df = normalize_actuals(raw)
    p = cache_path(year, base_dir)
    os.makedirs(os.path.dirname(p), exist_ok=True)
    df.to_parquet(p + ".tmp", index=False)
    os.replace(p + ".tmp", p)
    return {"rows": int(len(df)), "weeks": sorted(int(w) for w in df["week"].unique()),
            "games": int(df["game_id"].nunique()), "updated_at": pd.Timestamp.now("UTC").isoformat()}


def load_player_actuals(year=2026, base_dir=BASE_DIR, fetch_if_missing=True):
    """Inputs: year, base_dir, fetch_if_missing (download on first use).
    Output: cached actuals DataFrame (empty if missing and not fetched / fetch failed)."""
    p = cache_path(year, base_dir)
    if not os.path.exists(p) and fetch_if_missing:
        try:
            refresh_player_actuals(year, base_dir)
        except Exception as e:  # noqa: BLE001 -- offline etc.: evaluate nothing rather than crash the API
            print(f"player actuals fetch failed: {e}")
    return pd.read_parquet(p) if os.path.exists(p) else pd.DataFrame()


if __name__ == "__main__":
    import sys
    print(refresh_player_actuals(int(sys.argv[1]) if len(sys.argv) > 1 else 2026))
