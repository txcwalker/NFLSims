"""One-off seed for data/dna/trench_overrides_2026.csv's raw metric fields
(both offense and defense sides), populated ahead of any real 2026 PBP.

Real 2026 trench data doesn't exist yet, and there's no public source for a
from-scratch build of it -- so instead of inventing numbers, this assigns
each 2026 team a REAL 2023-2025 team-season's whole row of raw metrics (all
fields for that side together, not randomized field-by-field, so no team
ends up with a combination no real unit has ever actually produced) drawn
from a historical donor pool, chosen so the resulting composite roughly
matches Cam's hand-built 2026 consensus ranking for that side. See
WORKLOG.md's 2026-08-12 entry for the full sourcing history.

Donor assignment is per-target-rank with a random window (not a strict 1:1
percentile match) and without replacement -- two different 2026 teams never
get the identical historical donor row for the same side. Seeded
(np.random.seed(2026)) for reproducibility.

Usage: python seed_trench_overrides_from_history_v_0_1_0.py <year>
  (then run apply_trench_overrides_v_0_1_0.py <year> to sync into trench_dna.json)
"""
import sys
import os
import csv
import numpy as np
import pandas as pd

DNA_DIR = "data/dna"
HISTORY_SEASONS = [2023, 2024, 2025]
PASS_WEIGHT = 0.60
RUN_WEIGHT = 0.40

RUN_BLOCK_OFF_METRICS = {"ybc_per_att": 1, "rush_pct_over_expected": 1, "stuff_rate": -1, "avg_time_to_los": -1}
PASS_BLOCK_OFF_METRICS = {"pressured_pct_allowed": -1, "hurry_rate_allowed": -1, "hit_rate_allowed": -1}
RUN_DEF_METRICS = {"stuff_rate_forced": 1, "aly_allowed": -1, "ybc_allowed_per_att": -1, "rush_pct_over_expected_allowed": -1}
PASS_DEF_METRICS = {"pressure_rate_forced": 1, "hurry_rate_forced": 1, "hit_rate_forced": 1}

# Cam's 2026 consensus O-line rank, 1=best to 32=worst. Average of PFF
# (https://www.pff.com/news/nfl-offensive-line-rankings-2026) and FTN Fantasy
# (https://ftnfantasy.com/nfl/2026-offensive-line-rankings) for every team;
# for the 16 teams Cam has since sourced an individual Brandon Thorn rank for
# (DEN/PHI/TB/BUF/LAC/NO/CIN/DET/PIT/BAL/LV/GB/NYJ/HOU/CAR/WAS, 2026-08-12),
# Thorn's number is averaged in as a third source instead of just two.
# Washington is the one exception: PFF's raw 32 is excluded from its average
# (Cam's original call, 2026-08-12) rather than blended in, since Thorn's
# number was specifically sourced to correct that one outlier -- WAS uses
# FTN+Thorn only. Ties broken arbitrarily (order found stable enough not to
# matter -- Cam confirmed the ends matter most, the middle moves around a lot
# across sources regardless).
OFFENSE_TARGET_RANK_ORDER = [
    "DEN", "PHI", "TB", "IND", "BUF", "CHI", "LAC", "ATL", "SF", "LA",
    "NO", "KC", "SEA", "DET", "NE", "MIN", "PIT", "NYG", "DAL", "NYJ",
    "BAL", "CIN", "ARI", "WAS", "LV", "CAR", "MIA", "HOU", "GB", "JAX",
    "TEN", "CLE",
]

# Cam's 2026 D-line/front-seven rank, 1=best to 32=worst. Single source --
# Sharp Football Analysis's staff-voted 2026 Front Seven Rankings
# (https://www.sharpfootballanalysis.com/analysis/best-nfl-front-seven-rankings/),
# accepted as-is 2026-08-12 ("this looks like a completely reasonable list").
# Unlike the O-line order above, this is NOT an average of multiple sources --
# PFF/FTN don't publish an equivalent all-32 D-line unit ranking, and
# Brandon Thorn's D-line piece is paywalled with only fragments surfaced
# (Texans/Rams/Broncos top 3 in a separate "Playoffs" sub-ranking, not
# necessarily his main list) -- revise this if more sourcing comes in, same
# as the O-line order was revised as Thorn values arrived piecemeal.
DEFENSE_TARGET_RANK_ORDER = [
    "HOU", "PHI", "LA", "DEN", "SEA", "CLE", "PIT", "SF",
    "DET", "BAL", "NE", "KC", "GB", "TB", "JAX", "NYJ",
    "DAL", "NYG", "CAR", "MIN", "LAC", "TEN", "BUF", "WAS",
    "IND", "CIN", "LV", "NO", "ARI", "CHI", "ATL", "MIA",
]


def zscore_within_season(df, metrics):
    z_cols = []
    for metric, sign in metrics.items():
        z_col = f"_z_{metric}"
        df[z_col] = df.groupby("season")[metric].transform(lambda x: (x - x.mean()) / x.std(ddof=0)) * sign
        z_cols.append(z_col)
    return df[z_cols].mean(axis=1)


def build_donor_pool(run_path, run_metrics, run_z_col, pass_path, pass_metrics, pass_z_col):
    run_df = pd.read_csv(run_path)
    pass_df = pd.read_csv(pass_path)

    run_df = run_df[run_df["season"].isin(HISTORY_SEASONS)].copy()
    pass_df = pass_df[pass_df["season"].isin(HISTORY_SEASONS)].copy()

    run_df[run_z_col] = zscore_within_season(run_df, run_metrics)
    pass_df[pass_z_col] = zscore_within_season(pass_df, pass_metrics)

    pool = pd.merge(
        run_df[["team", "season"] + list(run_metrics) + [run_z_col]],
        pass_df[["team", "season"] + list(pass_metrics) + [pass_z_col]],
        on=["team", "season"], how="inner",
    )
    pool["combined_z"] = pool[pass_z_col] * PASS_WEIGHT + pool[run_z_col] * RUN_WEIGHT
    return pool.sort_values("combined_z", ascending=False).reset_index(drop=True)


def assign_donors(pool, target_rank_order, window=4):
    np.random.seed(2026)
    n_pool = len(pool)
    n_targets = len(target_rank_order)
    available = set(range(n_pool))
    assignment = {}

    for rank, team in enumerate(target_rank_order):
        ideal_idx = round(rank / (n_targets - 1) * (n_pool - 1))
        w = window
        while True:
            candidates = [i for i in range(max(0, ideal_idx - w), min(n_pool, ideal_idx + w + 1)) if i in available]
            if candidates:
                break
            w += 2  # expand window if the local neighborhood is already fully claimed
        pick = int(np.random.choice(candidates))
        available.discard(pick)
        assignment[team] = pool.iloc[pick]

    return assignment


def seed(year):
    csv_path = os.path.join(DNA_DIR, f"trench_overrides_{year}.csv")
    if not os.path.exists(csv_path):
        raise SystemExit(f"{csv_path} not found -- run export_trench_overrides_v_0_1_0.py {year} first.")

    print(f"Building offense donor pool from {HISTORY_SEASONS}...")
    off_pool = build_donor_pool(
        "docs/eda_outputs/run_block_metrics_team_season.csv", RUN_BLOCK_OFF_METRICS, "run_block_off_z",
        "docs/eda_outputs/pass_block_offense_team_season.csv", PASS_BLOCK_OFF_METRICS, "pass_block_off_z",
    )
    print(f"  {len(off_pool)} real team-seasons in offense pool.")
    off_assignment = assign_donors(off_pool, OFFENSE_TARGET_RANK_ORDER)
    off_fields = list(RUN_BLOCK_OFF_METRICS) + list(PASS_BLOCK_OFF_METRICS)

    print(f"Building defense donor pool from {HISTORY_SEASONS}...")
    def_pool = build_donor_pool(
        "docs/eda_outputs/run_defense_metrics_team_season.csv", RUN_DEF_METRICS, "run_def_z",
        "docs/eda_outputs/pass_block_defense_team_season.csv", PASS_DEF_METRICS, "pass_def_z",
    )
    print(f"  {len(def_pool)} real team-seasons in defense pool.")
    def_assignment = assign_donors(def_pool, DEFENSE_TARGET_RANK_ORDER)
    def_fields = list(RUN_DEF_METRICS) + list(PASS_DEF_METRICS)

    rows = list(csv.DictReader(open(csv_path, newline="")))
    fieldnames = list(rows[0].keys())
    for row in rows:
        off_donor = off_assignment.get(row["team"])
        if off_donor is not None:
            for field in off_fields:
                row[field] = round(float(off_donor[field]), 4)
        def_donor = def_assignment.get(row["team"])
        if def_donor is not None:
            for field in def_fields:
                row[field] = round(float(def_donor[field]), 4)

    with open(csv_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f"Wrote offense + defense donor values for all teams to {csv_path}.")
    print(f"Run apply_trench_overrides_v_0_1_0.py {year} next to sync into trench_dna.json.")

    print("\nOffense donor assignment (team -> donor team/season, combined_z):")
    for team in OFFENSE_TARGET_RANK_ORDER:
        d = off_assignment[team]
        print(f"  {team:4s} <- {d['team']} {int(d['season'])}  (combined_z={d['combined_z']:.3f})")

    print("\nDefense donor assignment (team -> donor team/season, combined_z):")
    for team in DEFENSE_TARGET_RANK_ORDER:
        d = def_assignment[team]
        print(f"  {team:4s} <- {d['team']} {int(d['season'])}  (combined_z={d['combined_z']:.3f})")


def main():
    if len(sys.argv) != 2:
        print("Usage: python seed_trench_overrides_from_history_v_0_1_0.py <year>")
        sys.exit(1)
    seed(int(sys.argv[1]))


if __name__ == "__main__":
    main()
