"""Build a full set of week-by-week roster trees for the SEASON sim
(data/current_rosters/week_NN/{TEAM}_traits_2026.json, NN = 1..18).

Why: run_full_season_sim_2026.py sims every one of the 272 matchups in its
scheduled week. A player on IR through week 8, or a mid-season QB change, has
to be reflected per matchup -- not carried at full strength all season (the
single data/current_rosters/{TEAM}_traits_2026.json) nor frozen at week 1
(the DFS data/current_rosters/dfs/ tree).

Each week NN tree =
  1. base:  the season-long current_rosters/{TEAM}_traits_2026.json
  2. injuries: season_long/{TEAM}.csv resolved for week NN
     (src.data_pipeline.week_roster_v_0_1_0.resolve_week_rows -- a reserve
     player whose return_week > NN is zeroed and their share redistributed
     pro-rata to active same-position teammates; fill-ins auto-revert once
     the starter is back). Week 1 uses the existing HAND-EDITED
     data/overrides/2026/week_01/ sheets instead of a fresh resolve.
  3. QB swaps: data/overrides/2026/qb_swaps_2026.csv -- for every row whose
     effective_week <= NN, `starter_override` is set on in_qb and cleared on
     out_qb (both kept active). game_engine._get_starter_static picks the
     override, so this flips qb_starters[team] from that week on.

Output dir per week is gitignored (large, always regeneratable). Run this
before run_full_season_sim_2026.py (regenerate_2026_reports.py chains it).

Usage: venv\\Scripts\\python.exe scripts/roster_management/build_season_week_rosters_v_0_1_0.py [year] [max_week]
"""
import copy
import csv
import glob
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from src.data_pipeline.week_roster_v_0_1_0 import resolve_week_rows, is_player_row  # noqa: E402
from roster_feed_v_0_1_0 import read_rows  # noqa: E402
from build_2026_rosters_v_0_1_0 import load_skill_dna  # noqa: E402
from apply_sheet_helpers_v_0_1_0 import apply_sheet_to_traits, POS_ORDER  # noqa: E402

GONE = ("left_team", "cut", "retired")


def load_qb_swaps(year):
    path = os.path.join("data", "overrides", str(year), f"qb_swaps_{year}.csv")
    swaps = []
    if os.path.exists(path):
        with open(path, newline="") as f:
            for r in csv.DictReader(f):
                swaps.append((r["team"].strip(), int(r["effective_week"]),
                              r["out_qb"].strip(), r["in_qb"].strip()))
    return swaps


def apply_qb_swaps(traits, team, week, swaps):
    """Set starter_override on the incoming QB for every swap active by `week`."""
    changed = []
    for s_team, eff_week, out_qb, in_qb in swaps:
        if s_team != team or week < eff_week:
            continue
        if in_qb in traits:
            traits[in_qb]["starter_override"] = True
            traits[in_qb]["status"] = "active"
        else:
            changed.append(f"!! {in_qb} not on {team} roster")
            continue
        if out_qb in traits:
            traits[out_qb].pop("starter_override", None)
        changed.append(f"{out_qb} -> {in_qb}")
    return changed


def main():
    year = int(sys.argv[1]) if len(sys.argv) > 1 else 2026
    max_week = int(sys.argv[2]) if len(sys.argv) > 2 else 18

    season_dir = os.path.join("data", "overrides", str(year), "season_long")
    out_root = os.path.join("data", "current_rosters")
    season_paths = sorted(glob.glob(os.path.join(season_dir, "*.csv")))
    skill_dna = load_skill_dna()
    swaps = load_qb_swaps(year)
    print(f"{len(swaps)} QB swap(s) loaded: " + "; ".join(f"{t} wk{w} {o}->{i}" for t, w, o, i in swaps))

    for week in range(1, max_week + 1):
        wk_out = os.path.join(out_root, f"week_{week:02d}")
        os.makedirs(wk_out, exist_ok=True)
        wk01_dir = os.path.join("data", "overrides", str(year), "week_01")

        n_teams = swap_notes = 0
        for sp in season_paths:
            team = os.path.basename(sp)[:-4]
            season_traits_path = os.path.join(out_root, f"{team}_traits_{year}.json")
            if not os.path.exists(season_traits_path):
                continue

            if week == 1 and os.path.exists(os.path.join(wk01_dir, f"{team}.csv")):
                _, rows = read_rows(os.path.join(wk01_dir, f"{team}.csv"))
            else:
                _, season_rows = read_rows(sp)
                rows, _report = resolve_week_rows(season_rows, week)

            rows = [r for r in rows if is_player_row(r)
                    and (r.get("roster_slot") or "").strip().lower() not in GONE]

            data = copy.deepcopy(json.load(open(season_traits_path)))
            data["max_week"] = week
            data["roster_kind"] = "season_week"
            traits = data["traits"]
            apply_sheet_to_traits(traits, rows, skill_dna)
            notes = apply_qb_swaps(traits, team, week, swaps)
            if notes:
                swap_notes += 1
                if week in (1, min(w for _, w, _, _ in swaps) if swaps else 99):
                    print(f"  wk{week} {team}: {', '.join(notes)}")

            data["traits"] = dict(sorted(traits.items(), key=lambda kv: POS_ORDER.get(kv[1].get("pos"), 4)))
            json.dump(data, open(os.path.join(wk_out, f"{team}_traits_{year}.json"), "w"), indent=1)
            n_teams += 1

        print(f"week {week:2}: {n_teams} teams -> {wk_out}" + (f"  ({swap_notes} with QB swap active)" if swap_notes else ""))

    print(f"\nDone. run_full_season_sim_{year}.py will use data/current_rosters/week_NN/ per matchup.")


if __name__ == "__main__":
    main()
