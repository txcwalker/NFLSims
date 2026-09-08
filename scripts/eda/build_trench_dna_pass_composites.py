"""
build_trench_dna_pass_composites.py
=====================================
Extends data/dna/trench_dna.json with pass-blocking (offense) and pass-rush
(defense) composite z-scores (pass_block_off_z, pass_def_z), per season/team.
Pass-side counterpart to build_trench_dna_composites.py (run side) -- same
pattern, mirrored exactly. Data layer for the Gate 2b sack-correction gate;
see the plan doc for full context.

Composite construction matches the run-side script: z-score each metric
WITHIN season (removes era drift), sign-flipped so a higher composite always
means "better" for that side, then averaged.
  - Offense (pass-blocking): pressured_pct_allowed, hurry_rate_allowed,
    hit_rate_allowed -- all lower is better (fewer pressures/hurries/hits
    allowed), so all three get sign=-1.
  - Defense (pass-rush): pressure_rate_forced, hurry_rate_forced,
    hit_rate_forced -- all higher is better (more pressure generated), so
    all three keep sign=+1.

trench_dna.json already has sack_rate_allowed (offense) and
def_pressure_rate/def_sack_rate (defense) from before this project -- this
adds the newer, richer metric set alongside them, not a replacement.

This only ADDS keys (dict.update, never replaces a dict wholesale), same
safety property as the run-side script.

Run from repo root: python scripts/eda/build_trench_dna_pass_composites.py
"""

import json
import pandas as pd

DNA_PATH = "data/dna/trench_dna.json"

PASS_BLOCK_OFF_METRICS = {"pressured_pct_allowed": -1, "hurry_rate_allowed": -1, "hit_rate_allowed": -1}
PASS_DEF_METRICS = {"pressure_rate_forced": 1, "hurry_rate_forced": 1, "hit_rate_forced": 1}


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
    pass_off = build_composite_table(
        "docs/eda_outputs/pass_block_offense_team_season.csv", PASS_BLOCK_OFF_METRICS, "pass_block_off_z")
    pass_def = build_composite_table(
        "docs/eda_outputs/pass_block_defense_team_season.csv", PASS_DEF_METRICS, "pass_def_z")

    with open(DNA_PATH, "r") as f:
        dna = json.load(f)

    updated_count = 0
    new_entries = []

    for table in (pass_off, pass_def):
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
        print(f"{len(new_entries)} were brand-new season/team dicts -- New: {new_entries[:10]}")

    sample = dna["2024"]["ARI"]
    print("\nSample after update (2024 ARI):")
    print(json.dumps(sample, indent=2))


if __name__ == "__main__":
    main()
