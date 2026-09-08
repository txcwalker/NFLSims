"""
build_trench_dna_composites.py
================================
Extends data/dna/trench_dna.json with run-blocking (offense) and run-defense
raw metrics plus two composite z-scores (run_block_off_z, run_def_z), per
season/team. This is the data-layer half of the trench matchup gate work --
see the plan doc for full context.

trench_dna.json is currently 100% pass-game data (sack_rate_allowed,
def_pressure_rate, etc.) with a _metadata.deprecation_notice already stating
it supersedes trench_tiers_2025.json's 1-5 tier grades "for Air Yards
modeling with continuous metrics" -- this extends that same intent to the
run game rather than inventing a parallel file.

Composite construction matches analyze_trench_matchup_outcomes.py exactly:
z-score each metric WITHIN season (removes era drift), sign-flipped so a
higher composite always means "better," then averaged. pct_stacked_box_forced
is deliberately excluded from the run-defense composite -- it's a context
stat (how often the defense saw a stacked box), not a performance grade,
consistent with how the offense side excludes its own box-count field.

This only ADDS keys (dict.update, never replaces a dict wholesale) --
existing pass-game fields and _metadata are left completely untouched. If a
season/team pair doesn't exist yet at all (e.g. extending to a season with
no prior pass-game data), the entry is created rather than skipped.

Run from repo root: python scripts/eda/build_trench_dna_composites.py
"""

import json
import pandas as pd

DNA_PATH = "data/dna/trench_dna.json"

RUN_BLOCK_OFF_METRICS = {"ybc_per_att": 1, "rush_pct_over_expected": 1, "stuff_rate": -1, "avg_time_to_los": -1}
RUN_DEF_METRICS = {"stuff_rate_forced": 1, "aly_allowed": -1, "ybc_allowed_per_att": -1, "rush_pct_over_expected_allowed": -1}


def build_composite_table(path, metrics, out_col):
    """Returns [team, season] + raw metric columns + the composite z column."""
    df = pd.read_csv(path)
    z_cols = []
    for metric, sign in metrics.items():
        z_col = f"_z_{metric}"
        df[z_col] = df.groupby("season")[metric].transform(
            lambda x: (x - x.mean()) / x.std(ddof=0)
        ) * sign
        z_cols.append(z_col)
    df[out_col] = df[z_cols].mean(axis=1)
    keep = ["team", "season"] + list(metrics.keys()) + [out_col]
    return df[keep]


def main():
    run_off = build_composite_table(
        "docs/eda_outputs/run_block_metrics_team_season.csv", RUN_BLOCK_OFF_METRICS, "run_block_off_z")
    run_def = build_composite_table(
        "docs/eda_outputs/run_defense_metrics_team_season.csv", RUN_DEF_METRICS, "run_def_z")

    with open(DNA_PATH, "r") as f:
        dna = json.load(f)

    updated_count = 0
    new_entries = []

    for table in (run_off, run_def):
        for _, row in table.iterrows():
            season_key = str(int(row["season"]))
            team = row["team"]
            new_fields = row.drop(["team", "season"]).to_dict()
            if season_key not in dna:
                dna[season_key] = {}
            if team not in dna[season_key]:
                dna[season_key][team] = {}
                new_entries.append((season_key, team))
            dna[season_key][team].update(new_fields)
            updated_count += 1

    with open(DNA_PATH, "w") as f:
        json.dump(dna, f, indent=4)

    print(f"Updated {updated_count} (season, team) entries in {DNA_PATH}")
    if new_entries:
        print(f"{len(new_entries)} were brand-new season/team dicts this file didn't have before "
              f"(e.g. a season with no prior pass-game data) -- those entries have ONLY the "
              f"run-block/run-defense fields just written, no pass-game fields. Every consumer "
              f"already reads via .get(key, fallback), so this is safe. New: {new_entries[:10]}")

    sample = dna["2024"]["ARI"]
    print("\nSample after update (2024 ARI):")
    print(json.dumps(sample, indent=2))


if __name__ == "__main__":
    main()
