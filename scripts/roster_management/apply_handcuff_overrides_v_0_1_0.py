"""Applies hand-edited rows from data/dna/handcuff_overrides_{year}.csv back
into data/current_rosters/{TEAM}_traits_{year}.json -- writes each row's
`backup_to` and non-blank field values into the player's `if_starting`
block, read by resolve_handcuff_overrides() in game_engine.py whenever
`backup_to`'s status is inactive (see that function's docstring for the
full mechanism -- a genuine successor's numbers, not the auto-renormalized
committee-role share every other backup already gets for free).

A row needs both a player match and a non-blank `backup_to` to do anything
-- rows with `backup_to` blank are skipped (nothing to designate). Only the
fields actually filled in on a row become part of `if_starting`; blank
cells are simply omitted from that player's block, same blank-means-no-
override convention apply_preseason_overrides_v_0_1_0.py uses.

Safe to re-run: idempotent, only reports/writes what actually changed. Run
export_handcuff_overrides_v_0_1_0.py first if the CSV doesn't exist yet.

Usage: python apply_handcuff_overrides_v_0_1_0.py <year>
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


def load_overrides(year):
    path = os.path.join(DNA_DIR, f"handcuff_overrides_{year}.csv")
    if not os.path.exists(path):
        raise SystemExit(f"{path} not found -- run export_handcuff_overrides_v_0_1_0.py {year} first.")
    with open(path, newline="") as f:
        return list(csv.DictReader(f))


def apply(year):
    rows = load_overrides(year)

    rosters = {}
    for path in glob.glob(os.path.join(ROSTERS_DIR, f"*_traits_{year}.json")):
        data = json.load(open(path))
        rosters[data["team"]] = (path, data)

    changes = []
    skipped = []

    for row in rows:
        name, team = row["player_name"], row["team"]
        backup_to = (row.get("backup_to") or "").strip()
        if not backup_to:
            skipped.append((name, team, "no backup_to given -- nothing to designate"))
            continue
        if team not in rosters:
            skipped.append((name, team, "team file not found"))
            continue
        _, data = rosters[team]
        traits = data["traits"].get(name)
        if traits is None:
            skipped.append((name, team, "player not found in roster file"))
            continue
        if backup_to not in data["traits"]:
            skipped.append((name, team, f"backup_to player {backup_to!r} not found on {team}"))
            continue

        if_starting = {}
        for field in TUNABLE_FIELDS:
            raw = row.get(field, "")
            if raw is None or str(raw).strip() == "":
                continue
            try:
                if_starting[field] = float(raw)
            except ValueError:
                skipped.append((name, team, f"unparseable value for {field}: {raw!r}"))
                continue
        if not if_starting:
            skipped.append((name, team, "backup_to given but no field values filled in"))
            continue

        old_backup_to = traits.get("backup_to")
        old_if_starting = traits.get("if_starting")
        if old_backup_to == backup_to and old_if_starting == if_starting:
            continue  # unchanged

        traits["backup_to"] = backup_to
        traits["if_starting"] = if_starting
        changes.append((team, name, backup_to, if_starting))

    for path, data in rosters.values():
        with open(path, "w") as f:
            json.dump(data, f, indent=4)

    if changes:
        print(f"Applied {len(changes)} handcuff designation(s):")
        for team, name, backup_to, if_starting in changes:
            fields = ", ".join(f"{k}={v}" for k, v in sorted(if_starting.items()))
            print(f"  {team} {name} -> backs up {backup_to}: {fields}")
    else:
        print("No changes -- CSV matches what's already in the roster files.")

    if skipped:
        print(f"\n{len(skipped)} row(s) skipped:")
        for name, team, reason in skipped:
            print(f"  {name} ({team}): {reason}")


def main():
    if len(sys.argv) != 2:
        print("Usage: python apply_handcuff_overrides_v_0_1_0.py <year>")
        sys.exit(1)
    apply(int(sys.argv[1]))


if __name__ == "__main__":
    main()
