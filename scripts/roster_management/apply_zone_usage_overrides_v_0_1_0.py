"""Applies Cam's hand-edited data/dna/zone_usage_overrides_{year}.csv back
into data/current_rosters/{TEAM}_traits_{year}.json's
splits.redzone/goalline.target_share/carry_share.

This is the missing wiring step export_zone_usage_overrides_v_0_1_0.py's
docstring flagged as not yet built: that script only produced the
hand-editable starting document, it never fed the sim. game_engine.py's
zone-based lookup (self.precomputed_skill_target_share/carry_share, see
game_engine.py:545-554) already prefers splits[zone][field] over the flat
field whenever that key is present -- so once this script writes into
splits.redzone/goalline, the very next sim run picks the new values up with
no other code changes needed. Same write target as
build_redzone_goalline_shares_v_0_1_0.py (that script's source is raw
2023-2025 PBP with empirical-Bayes shrinkage; this script's source is Cam's
own hand-tuned CSV -- whichever ran most recently wins, since both write the
same fields).

CSV columns applied: rz_target_share -> splits.redzone.target_share,
rz_carry_share -> splits.redzone.carry_share, five_target_share ->
splits.goalline.target_share, five_carry_share -> splits.goalline.carry_share.
Matched on (player_name, team) directly against the roster file's traits
dict keys -- same name key export_zone_usage_overrides_v_0_1_0.py wrote,
sourced from preseason_overrides_{year}.csv, so no id bridge needed here.

Blank cells are skipped (no override, leaves whatever's currently in that
splits field alone) -- matches apply_preseason_overrides_v_0_1_0.py's
convention. Only writes splits.redzone/goalline -- does not touch
splits.primary or any flat/top-level field.

Safe to re-run: idempotent, only reports/writes fields that actually changed.

Usage: python apply_zone_usage_overrides_v_0_1_0.py <year>
"""
import sys
import os
import csv
import glob
import json

ROSTERS_DIR = "data/current_rosters"
DNA_DIR = "data/dna"

# CSV column -> (zone, roster field)
FIELD_MAP = {
    "rz_target_share":   ("redzone",  "target_share"),
    "rz_carry_share":    ("redzone",  "carry_share"),
    "five_target_share": ("goalline", "target_share"),
    "five_carry_share":  ("goalline", "carry_share"),
}


def load_overrides(year):
    path = os.path.join(DNA_DIR, f"zone_usage_overrides_{year}.csv")
    if not os.path.exists(path):
        raise SystemExit(f"{path} not found -- run export_zone_usage_overrides_v_0_1_0.py first.")
    with open(path, newline="") as f:
        return list(csv.DictReader(f))


def apply(year):
    rows = load_overrides(year)

    rosters = {}
    for path in glob.glob(os.path.join(ROSTERS_DIR, f"*_traits_{year}.json")):
        data = json.load(open(path))
        rosters[data["team"]] = (path, data)

    changes = []
    unmatched = []

    for row in rows:
        name, team = row["player_name"], row["team"]
        if team not in rosters:
            unmatched.append((name, team, "team file not found"))
            continue
        _, data = rosters[team]
        traits = data["traits"].get(name)
        if traits is None:
            unmatched.append((name, team, "player not found in roster file"))
            continue

        splits = traits.setdefault("splits", {})
        for csv_col, (zone, field) in FIELD_MAP.items():
            raw = row.get(csv_col, "")
            if raw is None or str(raw).strip() == "":
                continue
            try:
                value = float(raw)
            except ValueError:
                unmatched.append((name, team, f"unparseable value for {csv_col}: {raw!r}"))
                continue
            zdict = splits.setdefault(zone, {})
            old = zdict.get(field)
            if old is None or abs(old - value) >= 1e-9:
                zdict[field] = value
                changes.append((team, name, f"splits.{zone}.{field}", old, value))

    for path, data in rosters.values():
        with open(path, "w") as f:
            json.dump(data, f, indent=4)

    if changes:
        print(f"Applied {len(changes)} field change(s) across {len(set((c[0], c[1]) for c in changes))} players.")
    else:
        print("No changes -- CSV matches what's already in the roster files.")

    if unmatched:
        print(f"\n{len(unmatched)} row/field issue(s) skipped:")
        for name, team, reason in unmatched:
            print(f"  {name} ({team}): {reason}")


def main():
    if len(sys.argv) != 2:
        print("Usage: python apply_zone_usage_overrides_v_0_1_0.py <year>")
        sys.exit(1)
    apply(int(sys.argv[1]))


if __name__ == "__main__":
    main()
