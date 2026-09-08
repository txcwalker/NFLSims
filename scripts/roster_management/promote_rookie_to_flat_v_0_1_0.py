"""Promotes selected rookies from the curve-based ramp track
(data/dna/rookie_projections_{year}.json) to the flat/no-ramp track -- a
real `preseason_projection` block in
data/current_rosters/{TEAM}_traits_{year}.json, the same track every
veteran and every rookie QB already uses (QB rookies get this by design --
a QB1 promotion is a real-evidence status flip, not a gradual ramp, see
refresh_weekly_dna_v_0_1_0.py). This script extends that same flat
treatment to specific named non-QB rookies on request -- e.g. a round-1
skill-position pick Cam expects to start and produce at a real level from
week 1, not ramp into over the first month.

Seeds each promoted field from the rookie's existing curve data --
usage_curve/efficiency_curve's `late_value` (the eventual steady-state
number, used as the flat season-long number since there's no more ramp)
plus static_projection's already-flat fields -- rather than inventing new
numbers. These are still placeholder-quality, same as everything else in
rookie_projections_{year}.json -- promotion's purpose is to turn the number
into a directly hand-editable CSV row (see
export_preseason_overrides_v_0_1_0.py), not to make it more accurate.
Fields the rookie's curve/static_projection doesn't cover keep whatever
generic position-baseline default build_2026_rosters_v_0_1_0.py already
gave them.

Removes each promoted player from rookie_projections_{year}.json --
required for export_preseason_overrides_v_0_1_0.py to pick them up (it
skips any rookie that still has no preseason_projection block).

After running, re-run export_preseason_overrides_v_0_1_0.py to pick up the
newly-promoted rows in preseason_overrides_{year}.csv.

Usage: python promote_rookie_to_flat_v_0_1_0.py <year> <player name> [<player name> ...]
  e.g. python promote_rookie_to_flat_v_0_1_0.py 2026 "Jeremiyah Love" "Carnell Tate"
"""
import sys
import os
import json
import glob

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import PLAYER_RATE_FIELDS, PLAYER_NGS_FIELDS, ZONE_SPLIT_FIELDS, ZONES

DNA_DIR = "data/dna"
ROSTERS_DIR = "data/current_rosters"
TUNABLE_FIELDS = PLAYER_RATE_FIELDS + PLAYER_NGS_FIELDS


def flatten_curve_and_static(rp):
    """Curve late_value + static_projection -> one flat {field: value} dict,
    restricted to fields this pipeline actually tunes."""
    flat = dict(rp.get("static_projection", {}))
    for field, curve in rp.get("usage_curve", {}).items():
        flat[field] = curve["late_value"]
    for field, curve in rp.get("efficiency_curve", {}).items():
        flat[field] = curve["late_value"]
    return {f: round(float(v), 4) for f, v in flat.items() if f in TUNABLE_FIELDS}


def promote(year, names):
    rp_path = os.path.join(DNA_DIR, f"rookie_projections_{year}.json")
    rookie_projections = json.load(open(rp_path))

    roster_files = {}
    for path in glob.glob(os.path.join(ROSTERS_DIR, f"*_traits_{year}.json")):
        data = json.load(open(path))
        roster_files[data["team"]] = (path, data)

    promoted, skipped = [], []
    for name in names:
        rp = rookie_projections.get(name)
        if rp is None:
            skipped.append((name, "not found in rookie_projections file (already promoted, or a name mismatch)"))
            continue
        team = rp["team"]
        if team not in roster_files:
            skipped.append((name, f"no roster file found for team {team}"))
            continue
        _, data = roster_files[team]
        traits = data["traits"].get(name)
        if traits is None:
            skipped.append((name, f"not found in {team}_traits_{year}.json"))
            continue
        if "preseason_projection" in traits:
            skipped.append((name, "already has a preseason_projection block -- already promoted"))
            continue

        flat = flatten_curve_and_static(rp)
        for field, value in flat.items():
            traits[field] = value

        traits["preseason_projection"] = {f: traits[f] for f in TUNABLE_FIELDS if f in traits}
        traits["preseason_projection"]["splits"] = {
            zone: {f: traits[f] for f in ZONE_SPLIT_FIELDS if f in traits} for zone in ZONES
        }

        del rookie_projections[name]
        promoted.append((name, team, rp["pos"], flat))

    for path, data in roster_files.values():
        with open(path, "w") as f:
            json.dump(data, f, indent=4)
    with open(rp_path, "w") as f:
        json.dump(rookie_projections, f, indent=4)

    if promoted:
        print(f"Promoted {len(promoted)} rookie(s) to the flat track:")
        for name, team, pos, flat in promoted:
            fields = ", ".join(f"{k}={v}" for k, v in sorted(flat.items()))
            print(f"  {name} ({pos}, {team}): {fields}")
    if skipped:
        print(f"\n{len(skipped)} skipped:")
        for name, reason in skipped:
            print(f"  {name}: {reason}")


def main():
    if len(sys.argv) < 3:
        print("Usage: python promote_rookie_to_flat_v_0_1_0.py <year> <player name> [<player name> ...]")
        sys.exit(1)
    promote(int(sys.argv[1]), sys.argv[2:])


if __name__ == "__main__":
    main()
