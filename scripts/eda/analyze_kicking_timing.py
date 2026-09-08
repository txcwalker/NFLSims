"""
Real-data EDA for the live-play clock runoff used on kickoffs, punts, and
field goals in game_engine.py, plus the kickoff touchback rate. Requested by
Cam to verify (not just cite from memory) the constants at those three call
sites -- none of them previously had a comment explaining where the number
came from.

Timing methodology matches scripts/eda/analyze_clock_pace_grid.py exactly:
game_clock_elapsed for a row = the previous row's game_seconds_remaining
minus this row's, i.e. the time consumed by the PREVIOUS play. To measure
"how long does a kickoff/punt/FG take", filter on `prev_play_type` and read
this row's game_clock_elapsed.

Run from repo root: python scripts/eda/analyze_kicking_timing.py
"""

import os
import pandas as pd
import numpy as np
import nfl_data_py as nfl

SEASONS = list(range(2021, 2026))
OUTPUT_DIR = "docs/eda_outputs/kicking_timing"


def get_stats(series):
    if len(series) == 0:
        return {'N': 0, 'Mean': np.nan, 'Median': np.nan, 'P25': np.nan, 'P75': np.nan}
    return {
        'N': len(series),
        'Mean': float(series.mean()),
        'Median': float(series.median()),
        'P25': float(series.quantile(0.25)),
        'P75': float(series.quantile(0.75)),
    }


def main():
    print(f"Pulling real PBP for seasons {SEASONS}...")
    df_raw = nfl.import_pbp_data(SEASONS, downcast=True)
    df = df_raw[df_raw['season_type'] == 'REG'].copy()
    print(f"Filtered to Regular Season: {len(df)} rows.")

    # -------------------------------------------------------------
    # 1. Kickoff touchback rate, by season (checking the "new kickoff rule"
    #    hypothesis directly rather than assuming it).
    # -------------------------------------------------------------
    ko_all = df[df['play_type'] == 'kickoff'].copy()
    tb_by_season = ko_all.groupby('season')['touchback'].agg(['mean', 'count'])
    print("\nKickoff touchback rate by season:")
    print(tb_by_season)

    # -------------------------------------------------------------
    # 2. Live-play timing for kickoffs/punts/FGs, same "previous play"
    #    delta methodology as analyze_clock_pace_grid.py.
    # -------------------------------------------------------------
    df = df.sort_values(by=['game_id', 'play_id']).reset_index(drop=True)
    df['prev_game_id'] = df['game_id'].shift(1)
    df['prev_game_half'] = df['game_half'].shift(1)
    df['prev_game_seconds'] = df['game_seconds_remaining'].shift(1)
    df['game_clock_elapsed'] = np.where(
        (df['game_id'] == df['prev_game_id']) & (df['game_half'] == df['prev_game_half']),
        df['prev_game_seconds'] - df['game_seconds_remaining'],
        np.nan
    )
    df['prev_play_type'] = df['play_type'].shift(1)
    df['prev_touchback'] = df['touchback'].shift(1).fillna(0)
    df['prev_punt_fair_catch'] = df['punt_fair_catch'].shift(1).fillna(0)
    df['prev_field_goal_result'] = df['field_goal_result'].shift(1)

    # Bound elapsed time to a sane live-play window -- excludes half-boundary
    # artifacts and the rare data glitch, same [0, 60] bound the pace grid uses.
    valid = df[(df['game_clock_elapsed'].notna()) & (df['game_clock_elapsed'] >= 0) & (df['game_clock_elapsed'] <= 60)].copy()

    results = {}

    ko_rows = valid[valid['prev_play_type'] == 'kickoff']
    results['kickoff_touchback'] = get_stats(ko_rows[ko_rows['prev_touchback'] == 1]['game_clock_elapsed'])
    results['kickoff_return'] = get_stats(ko_rows[ko_rows['prev_touchback'] == 0]['game_clock_elapsed'])

    punt_rows = valid[valid['prev_play_type'] == 'punt']
    results['punt_touchback'] = get_stats(punt_rows[punt_rows['prev_touchback'] == 1]['game_clock_elapsed'])
    results['punt_fair_catch'] = get_stats(punt_rows[(punt_rows['prev_touchback'] == 0) & (punt_rows['prev_punt_fair_catch'] == 1)]['game_clock_elapsed'])
    results['punt_return'] = get_stats(punt_rows[(punt_rows['prev_touchback'] == 0) & (punt_rows['prev_punt_fair_catch'] == 0)]['game_clock_elapsed'])

    fg_rows = valid[valid['prev_play_type'] == 'field_goal']
    results['fg_made'] = get_stats(fg_rows[fg_rows['prev_field_goal_result'] == 'made']['game_clock_elapsed'])
    results['fg_missed'] = get_stats(fg_rows[fg_rows['prev_field_goal_result'] == 'missed']['game_clock_elapsed'])
    results['fg_blocked'] = get_stats(fg_rows[fg_rows['prev_field_goal_result'] == 'blocked']['game_clock_elapsed'])

    results_df = pd.DataFrame(results).T
    print("\nLive-play timing (seconds), 2021-2025 real PBP:")
    print(results_df)

    os.makedirs(OUTPUT_DIR, exist_ok=True)
    results_df.to_csv(os.path.join(OUTPUT_DIR, "kicking_timing.csv"))
    tb_by_season.to_csv(os.path.join(OUTPUT_DIR, "kickoff_touchback_by_season.csv"))

    with open(os.path.join(OUTPUT_DIR, "README.md"), 'w', encoding='utf-8') as f:
        f.write("# Kicking Timing + Touchback Rate EDA (2021-2025 Regular Season)\n\n")
        f.write("Verifies the previously-uncited constants in `game_engine.py` for kickoff/punt/FG live-play ")
        f.write("clock runoff, and the kickoff touchback rate. Methodology: same `game_clock_elapsed` (previous-play ")
        f.write("game-seconds-remaining delta) approach as `scripts/eda/analyze_clock_pace_grid.py`.\n\n")
        f.write("## Kickoff touchback rate by season\n\n")
        f.write("Confirms the 2025 kickoff-rule change directly rather than assuming it:\n\n")
        f.write("| Season | Touchback Rate | N |\n| :--- | ---: | ---: |\n")
        for season, row in tb_by_season.iterrows():
            f.write(f"| {int(season)} | {row['mean']:.4f} | {int(row['count']):,} |\n")
        f.write("\n## Live-play timing (seconds)\n\n")
        f.write("| Category | N | Mean | Median | P25 | P75 |\n| :--- | ---: | ---: | ---: | ---: | ---: |\n")
        for name, row in results_df.iterrows():
            if row['N'] == 0:
                f.write(f"| {name} | 0 | - | - | - | - |\n")
            else:
                f.write(f"| {name} | {int(row['N']):,} | {row['Mean']:.1f}s | {row['Median']:.1f}s | {row['P25']:.1f}s | {row['P75']:.1f}s |\n")

    print(f"\nSaved to {OUTPUT_DIR}/")


if __name__ == '__main__':
    main()
