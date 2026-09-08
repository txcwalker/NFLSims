"""Exports every player's `backup_to`/`if_starting` handcuff designation
into a single flat CSV for hand-editing -- data/dna/handcuff_overrides_{year}.csv.

Unrelated to preseason_overrides_{year}.csv/export_preseason_overrides_v_0_1_0.py
-- that file describes a player's own committee-role numbers; this one only
covers the small set of players who need a *different* set of numbers for
the specific scenario where the teammate they're designated to replace
(`backup_to`) is out. See resolve_handcuff_overrides() in game_engine.py for
how/when this gets applied during a sim. Most players have no row here at
all -- that's expected, not missing data (a passing-down back, for example,
needs no entry; his own carry_share already gets proportionally boosted for
free by the engine's active-pool renormalization once the starter is out).

Re-running this OVERWRITES the CSV with whatever's currently in the roster
files -- only run it before hand-editing the CSV, or after applying+
committing edits via apply_handcuff_overrides_v_0_1_0.py and wanting a
fresh baseline.

Usage: python export_handcuff_overrides_v_0_1_0.py <year>
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
            if_starting = traits.get("if_starting")
            backup_to = traits.get("backup_to")
            if not if_starting or not backup_to:
                continue
            row = {"player_name": name, "team": team, "pos": traits["pos"], "backup_to": backup_to}
            for field in TUNABLE_FIELDS:
                row[field] = if_starting.get(field, "")
            rows.append(row)

    rows.sort(key=lambda r: (r["team"], POS_ORDER.get(r["pos"], 4), r["player_name"]))

    out_path = os.path.join(DNA_DIR, f"handcuff_overrides_{year}.csv")
    with open(out_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["player_name", "team", "pos", "backup_to"] + TUNABLE_FIELDS)
        writer.writeheader()
        writer.writerows(rows)

    print(f"Wrote {len(rows)} handcuff-designation row(s) to {out_path}.")


def main():
    if len(sys.argv) != 2:
        print("Usage: python export_handcuff_overrides_v_0_1_0.py <year>")
        sys.exit(1)
    export(int(sys.argv[1]))


if __name__ == "__main__":
    main()
