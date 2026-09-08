"""
Real-data check on QB scramble yardage, requested by Cam after noticing
game_engine.py's Normal(5, 4) scramble-yardage model felt low ("scrambles go
for >5 yards quite a bit").

Cam's proposed approach was a heuristic (QB rush plays, excluding 1-2 yard
gains as presumed designed sneaks/keepers) because he wasn't sure the data
distinguishes real scrambles from designed QB runs. It does: nfl_data_py's
PBP has a purpose-built `qb_scramble` column (already used earlier this
session for the general fumble-rate EDA), so this uses that directly rather
than the heuristic -- more reliable than approximating it.

Run from repo root: python scripts/eda/analyze_scramble_yardage.py
"""

import os
import numpy as np
import nfl_data_py as nfl

SEASONS = list(range(2020, 2026))
OUTPUT_DIR = "docs/eda_outputs/qb_scramble_yardage"


def main():
    print(f"Pulling real PBP for seasons {SEASONS}...")
    df_raw = nfl.import_pbp_data(SEASONS, downcast=True)
    df = df_raw[df_raw['season_type'] == 'REG'].copy()

    scrambles = df[df['qb_scramble'] == 1]['yards_gained'].dropna()
    n = len(scrambles)
    mean = float(scrambles.mean())
    std = float(scrambles.std())
    median = float(scrambles.median())
    p25, p75 = float(scrambles.quantile(0.25)), float(scrambles.quantile(0.75))
    min_v, max_v = float(scrambles.min()), float(scrambles.max())

    print(f"n scrambles: {n}")
    print(f"mean={mean:.3f} std={std:.3f} median={median:.1f} P25={p25:.1f} P75={p75:.1f} min={min_v:.1f} max={max_v:.1f}")
    print(f"\nCurrent game_engine.py model: Normal(5, 4), clipped at floor -2")
    print(f"Real data suggests: Normal({mean:.1f}, {std:.1f}) -- mean is ~2 yards higher, std ~1.5x wider")

    os.makedirs(OUTPUT_DIR, exist_ok=True)
    with open(os.path.join(OUTPUT_DIR, "README.md"), 'w', encoding='utf-8') as f:
        f.write("# QB Scramble Yardage EDA (2020-2025 Regular Season)\n\n")
        f.write("Verifies `game_engine.py`'s scramble-yardage model (`Normal(5, 4)`, clipped at floor -2) ")
        f.write("against real data. Uses `nfl_data_py`'s `qb_scramble` PBP column directly -- a real scramble ")
        f.write("is already distinguished from a designed QB run/sneak in the source data, so no heuristic ")
        f.write("(e.g. excluding 1-2 yard QB runs) is needed.\n\n")
        f.write(f"| Metric | Value |\n| :--- | ---: |\n")
        f.write(f"| N | {n:,} |\n")
        f.write(f"| Mean | {mean:.2f} |\n")
        f.write(f"| Std Dev | {std:.2f} |\n")
        f.write(f"| Median | {median:.1f} |\n")
        f.write(f"| P25 | {p25:.1f} |\n")
        f.write(f"| P75 | {p75:.1f} |\n")
        f.write(f"| Min | {min_v:.1f} |\n")
        f.write(f"| Max | {max_v:.1f} |\n\n")
        f.write(f"**Current model:** `Normal(5, 4)`, clipped at floor -2.\n\n")
        f.write(f"**Real data:** mean {mean:.2f} (vs. modeled 5 -- undershooting by ~{mean-5:.1f} yards), ")
        f.write(f"std {std:.2f} (vs. modeled 4). Distribution is right-skewed (mean > median), which a Normal ")
        f.write("doesn't capture, but Cam's call is a Normal approximation is fine for now -- just needs the ")
        f.write("real mean/std, not a distribution-shape change.\n")

    print(f"\nSaved to {OUTPUT_DIR}/")


if __name__ == '__main__':
    main()
