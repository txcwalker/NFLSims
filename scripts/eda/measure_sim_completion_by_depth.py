"""Phase 0 of the completion-rate calibration ("A2"): measure what the SIM
currently produces for completion rate by target depth, to lay next to the real
2021-2025 baseline from analyze_completion_by_depth.py.

Runs a diverse real slate (2026 schedule, first N weeks) through the vectorized
engine with the `track_cmp_by_depth` hook on, sums the per-bucket (attempt,
completion) counts across every matchup, and prints sim vs real.

Usage:
    venv\\Scripts\\python.exe scripts/eda/measure_sim_completion_by_depth.py [weeks] [iters]
    (defaults: weeks=2, iters=400  -> ~32 matchups x 400 games)
"""
import os
import sys
import numpy as np
import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.nfl_sim.batch import BatchSimulator  # noqa: E402

SIM_YEAR = 2026
BUCKET_LABELS = ["<=0", "0-5", "5-10", "10-15", "15-20", "20-30", "30+"]
# Real 2021-2025, REAL TARGETS ONLY (receiver_player_id not null -- matches what
# the sim's completion model actually sees; excludes throwaways/batted balls).
# From analyze_completion_by_depth.py -- keep in sync.
REAL_CMP = [81.4, 76.4, 65.9, 58.8, 52.8, 39.2, 30.1]
REAL_ATT_PCT = [21.5, 30.8, 18.1, 11.5, 7.7, 6.3, 4.1]


def main():
    weeks = int(sys.argv[1]) if len(sys.argv) > 1 else 2
    iters = int(sys.argv[2]) if len(sys.argv) > 2 else 400

    sched = pd.read_csv(f"data/external/schedule_{SIM_YEAR}.csv")
    games = sched[(sched["game_type"] == "REG") & (sched["week"] <= weeks)]
    print(f"{len(games)} matchups (weeks 1-{weeks}) x {iters} iterations\n")

    tot_att = np.zeros(len(BUCKET_LABELS), dtype=np.int64)
    tot_cmp = np.zeros(len(BUCKET_LABELS), dtype=np.int64)

    for _, g in games.iterrows():
        away, home = g["away_team"], g["home_team"]
        batch = BatchSimulator(away, home, year=SIM_YEAR)
        batch.run_batch(iterations=iters, vectorized=True, track_cmp_by_depth=True)
        att, cmp = batch.last_cmp_by_depth
        tot_att += att
        tot_cmp += cmp

    sim_cmp = np.where(tot_att > 0, 100 * tot_cmp / tot_att, np.nan)
    sim_att_pct = 100 * tot_att / tot_att.sum()

    print(f"\n{'bucket':>8} | {'sim att%':>8} {'real att%':>9} | {'sim cmp%':>8} {'real cmp%':>9} {'delta':>7}")
    print("-" * 62)
    for i, lab in enumerate(BUCKET_LABELS):
        d = sim_cmp[i] - REAL_CMP[i]
        print(f"{lab:>8} | {sim_att_pct[i]:8.1f} {REAL_ATT_PCT[i]:9.1f} | "
              f"{sim_cmp[i]:8.1f} {REAL_CMP[i]:9.1f} {d:+7.1f}")

    sim_overall = 100 * tot_cmp.sum() / tot_att.sum()
    # real overall completion, attempt-weighted by the SIM's own bucket mix and
    # by the real mix, so we can see how much of any gap is shape vs mix
    real_on_sim_mix = np.average(REAL_CMP, weights=tot_att)
    print("-" * 62)
    print(f"sim overall completion:            {sim_overall:.2f}%")
    print(f"real curve @ sim's depth mix:      {real_on_sim_mix:.2f}%")
    print(f"real overall (2021-2025):          64.87%   (target 64.40%)")


if __name__ == "__main__":
    main()
