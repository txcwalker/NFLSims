"""Exports every team's current data/dna/trench_dna.json[<year>] raw trench
metrics into a single flat CSV for hand-editing -- data/dna/trench_overrides_<year>.csv.

Covers the raw metrics that feed the engine's run/pass matchup composites
(run_block_off_z, run_def_z, pass_block_off_z, pass_def_z -- see
scripts/eda/build_trench_dna_composites.py and
build_trench_dna_pass_composites.py for the exact z-scoring formula each
composite is built from) -- NOT the composites themselves. Editing the raw
metrics and re-running apply_trench_overrides_v_0_1_0.py recomputes the
composites from the (possibly-overridden) raw values, the same relationship
export/apply_preseason_overrides_v_0_1_0.py has to current_rosters' fields.

Re-running this OVERWRITES the CSV with whatever's currently in trench_dna.json
-- only run it before you've started hand-editing, or after you've applied+
committed your edits and want a fresh baseline.

Usage: python export_trench_overrides_v_0_1_0.py <year>
"""
import sys
import os
import csv
import json

DNA_PATH = "data/dna/trench_dna.json"
DNA_DIR = "data/dna"

RUN_OFF_FIELDS = ["ybc_per_att", "rush_pct_over_expected", "stuff_rate", "avg_time_to_los"]
RUN_DEF_FIELDS = ["stuff_rate_forced", "aly_allowed", "ybc_allowed_per_att", "rush_pct_over_expected_allowed"]
PASS_OFF_FIELDS = ["pressured_pct_allowed", "hurry_rate_allowed", "hit_rate_allowed"]
PASS_DEF_FIELDS = ["pressure_rate_forced", "hurry_rate_forced", "hit_rate_forced"]
ALL_FIELDS = RUN_OFF_FIELDS + PASS_OFF_FIELDS + RUN_DEF_FIELDS + PASS_DEF_FIELDS

TEAMS = [
    'BUF', 'MIA', 'NE', 'NYJ', 'BAL', 'CIN', 'CLE', 'PIT', 'HOU', 'IND', 'JAX', 'TEN',
    'DEN', 'KC', 'LV', 'LAC', 'DAL', 'NYG', 'PHI', 'WAS', 'CHI', 'DET', 'GB', 'MIN',
    'ATL', 'CAR', 'NO', 'TB', 'ARI', 'LA', 'SF', 'SEA',
]


def export(year):
    dna = json.load(open(DNA_PATH))
    season = dna.get(str(year), {})

    rows = []
    for team in TEAMS:
        t = season.get(team, {})
        row = {"team": team}
        for field in ALL_FIELDS:
            row[field] = t.get(field, "")
        rows.append(row)

    out_path = os.path.join(DNA_DIR, f"trench_overrides_{year}.csv")
    with open(out_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["team"] + ALL_FIELDS)
        writer.writeheader()
        writer.writerows(rows)

    print(f"Wrote {len(rows)} team rows to {out_path}.")


def main():
    if len(sys.argv) != 2:
        print("Usage: python export_trench_overrides_v_0_1_0.py <year>")
        sys.exit(1)
    export(int(sys.argv[1]))


if __name__ == "__main__":
    main()
