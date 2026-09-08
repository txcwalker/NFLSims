"""Exports every veteran's current preseason_projection into a single flat
CSV for hand-editing -- data/dna/preseason_overrides_2026.csv.

Only exports the fields that actually get blended week-to-week
(rolling_stats_v_0_1_0.PLAYER_RATE_FIELDS + PLAYER_NGS_FIELDS), not the
static career-DNA fields (pressure_rate, ngs_aggressiveness_index, etc.)
that current_rosters/*_traits_2026.json also carries but the weekly refresh
never touches. Curve-based rookies (no preseason_projection block, still
routed through data/dna/rookie_projections_2026.json's usage/efficiency
curves) are excluded -- different schema, not a single flat number.
Rookies "promoted" to the flat/no-ramp track (have a preseason_projection
block, removed from rookie_projections_2026.json -- e.g. a backup expected
to play a real, un-ramped role if called on) are included and treated
identically to veterans.

Re-running this OVERWRITES the CSV with whatever's currently in the roster
files -- only run it before you've started hand-editing the CSV, or after
you've applied+committed your edits via apply_preseason_overrides_v_0_1_0.py
and want a fresh baseline (e.g. after a full roster rebuild).

Usage: python export_preseason_overrides_v_0_1_0.py <year>
"""
import sys
import os
import csv
import glob
import json

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import PLAYER_RATE_FIELDS, PLAYER_NGS_FIELDS

ROSTERS_DIR = "data/current_rosters"
DNA_DIR = "data/dna"
TUNABLE_FIELDS = PLAYER_RATE_FIELDS + PLAYER_NGS_FIELDS
POS_ORDER = {"QB": 0, "RB": 1, "WR": 2, "TE": 3}


def export(year):
    rows = []
    for path in sorted(glob.glob(os.path.join(ROSTERS_DIR, f"*_traits_{year}.json"))):
        data = json.load(open(path))
        team = data["team"]
        for name, traits in data["traits"].items():
            if traits.get("rookie") and "preseason_projection" not in traits:
                continue  # curve-based rookie, not promoted -- see rookie_projections_2026.json
            pp = traits.get("preseason_projection", {})
            row = {"player_name": name, "team": team, "pos": traits["pos"]}
            for field in TUNABLE_FIELDS:
                row[field] = pp.get(field, "")
            rows.append(row)

    rows.sort(key=lambda r: (r["team"], POS_ORDER.get(r["pos"], 4), r["player_name"]))

    out_path = os.path.join(DNA_DIR, f"preseason_overrides_{year}.csv")
    with open(out_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["player_name", "team", "pos"] + TUNABLE_FIELDS)
        writer.writeheader()
        writer.writerows(rows)

    print(f"Wrote {len(rows)} veteran rows to {out_path}.")


def main():
    if len(sys.argv) != 2:
        print("Usage: python export_preseason_overrides_v_0_1_0.py <year>")
        sys.exit(1)
    export(int(sys.argv[1]))


if __name__ == "__main__":
    main()
