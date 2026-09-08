"""Quick empirical calibration harness for game_engine.py's pace-nudge
constant (_sample_pace_runoff's "+2s global calibration nudge"). Measures
combined offensive_snaps/game across a diverse sample of real 2026 matchups
at moderate N, for fast iteration -- NOT a replacement for a full official
audit, just a cheap way to zero in on a candidate value before committing to
one. Real target: 123.82 combined offensive snaps/game (docs/audit/
v_0_2_0_audit/historical_eda_metrics.json).

Usage: python test_pace_nudge_calibration.py
"""
import sys
import os
sys.path.append(os.getcwd())
import pandas as pd
from src.nfl_sim.batch import BatchSimulator

MATCHUPS = [
    ('KC', 'BUF'), ('SF', 'DAL'), ('DET', 'GB'), ('PHI', 'WAS'),
    ('CIN', 'BAL'), ('LAC', 'DEN'), ('MIA', 'NYJ'), ('MIN', 'CHI'),
    ('SEA', 'ARI'), ('HOU', 'IND'), ('LV', 'DEN'), ('NO', 'ATL'),
    ('CLE', 'PIT'), ('TB', 'CAR'), ('NYG', 'NE'), ('LA', 'TEN'),
]
TARGET_COMBINED = 123.82


def main():
    plays = []
    for away, home in MATCHUPS:
        b = BatchSimulator(away, home, year=2026)
        game_df, _ = b.run_batch(iterations=500, vectorized=True)
        plays.append(game_df['total_plays'].mean())

    avg = sum(plays) / len(plays)
    print(f"Sample avg combined plays/game: {avg:.2f}  (target: {TARGET_COMBINED}, gap: {avg - TARGET_COMBINED:+.2f})")


if __name__ == "__main__":
    main()
