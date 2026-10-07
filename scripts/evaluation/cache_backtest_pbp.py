"""Downloads + trims nfl_data_py play-by-play for the window-blend backtest and
caches each season to data/interim/backtest_pbp_{year}.parquet (gitignored).
Inputs: seasons on argv (default 2023 2024 2025) from nflverse via nfl_data_py.
Outputs: one trimmed parquet per season, regular season, pass/run plays only.
Purpose: the backtest re-reads these many times; the live pull is slow."""
import os
import sys

import nfl_data_py as nfl

COLS = [
    "season", "season_type", "week", "game_id", "play_type", "posteam", "defteam",
    "passer_player_id", "receiver_player_id", "rusher_player_id",
    "complete_pass", "air_yards", "yards_after_catch", "xyac_mean_yardage", "cpoe",
    "pass_attempt", "sack", "qb_scramble", "yards_gained", "qb_hit", "yardline_100",
]

if __name__ == "__main__":
    years = [int(y) for y in sys.argv[1:]] or [2023, 2024, 2025]
    for y in years:
        out = os.path.join("data", "interim", f"backtest_pbp_{y}.parquet")
        if os.path.exists(out):
            print(y, "cached")
            continue
        df = nfl.import_pbp_data([y], columns=COLS, include_participation=False)
        df = df[(df["season_type"] == "REG") & df["play_type"].isin(["pass", "run"])]
        df.to_parquet(out)
        print(y, len(df), "rows ->", out)
