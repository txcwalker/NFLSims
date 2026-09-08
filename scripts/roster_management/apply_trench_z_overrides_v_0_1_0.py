"""Applies hand-set composite trench z-score overrides from
data/dna/trench_z_overrides_<year>.csv directly into
data/dna/trench_dna.json[<year>][<team>][<field>].

This is a DIRECT override of the composite z-scores (run_block_off_z /
pass_block_off_z / run_def_z / pass_def_z) -- distinct from
apply_trench_overrides_v_0_1_0.py, which edits the RAW metrics and recomputes
the composites. Use this when you want to move a team's projected line
strength by judgment (injury, scheme, a year that doesn't reflect the roster)
rather than by editing individual PFF-style metrics.

The engine reads these composites straight from trench_dna.json at game
construction (game_engine.py ~L335), so applying here is all that's needed --
no downstream recompute. The tail-widening `_trench_spread` transform in
game_engine.py is applied ON TOP of whatever this leaves.

Idempotent: re-running with the same CSV is a no-op. Safe to re-run after any
trench_dna.json rebuild (re-applies the judgment calls a rebuild would wipe).

Usage: venv\\Scripts\\python.exe scripts/roster_management/apply_trench_z_overrides_v_0_1_0.py <year>
"""
import csv
import json
import os
import sys

DNA_PATH = "data/dna/trench_dna.json"
VALID_FIELDS = {"run_block_off_z", "pass_block_off_z", "run_def_z", "pass_def_z"}


def main():
    if len(sys.argv) != 2:
        print(__doc__)
        sys.exit(1)
    year = sys.argv[1]
    csv_path = f"data/dna/trench_z_overrides_{year}.csv"
    if not os.path.exists(csv_path):
        raise SystemExit(f"{csv_path} not found")

    dna = json.load(open(DNA_PATH))
    season = dna.get(year)
    if season is None:
        raise SystemExit(f"trench_dna.json has no '{year}' season")

    changes, skipped = [], []
    with open(csv_path, newline="") as f:
        for row in csv.DictReader(f):
            team, field = row["team"].strip(), row["field"].strip()
            if field not in VALID_FIELDS:
                skipped.append((team, field, "not a composite z field"))
                continue
            if team not in season:
                skipped.append((team, field, "team not in season"))
                continue
            new = float(row["value"])
            old = season[team].get(field)
            if old is None or abs(old - new) > 1e-9:
                season[team][field] = new
                changes.append((team, field, old, new))

    with open(DNA_PATH, "w") as f:
        json.dump(dna, f, indent=1)

    if changes:
        print(f"Applied {len(changes)} trench z override(s) for {year}:")
        for t, fld, o, n in changes:
            print(f"  {t} {fld}: {o} -> {n}")
    else:
        print("No changes -- trench_dna.json already matches the CSV.")
    for t, fld, why in skipped:
        print(f"  SKIPPED {t} {fld}: {why}")


if __name__ == "__main__":
    main()
