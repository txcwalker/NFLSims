"""Compile one week's DFS roster/usage sheets
(data/overrides/2026/week_NN/{TEAM}.csv) into their own traits.json tree,
SEPARATE from the season-long one -- so a week's injury-adjusted usage never
overwrites data/current_rosters/{TEAM}_traits_2026.json (the season-long /
rest-of-season file).

Base = a fresh copy of the CURRENT data/current_rosters/{TEAM}_traits_2026.json
(so every static career-DNA field a week sheet doesn't carry -- pressure_rate,
route_profile, top_speed_mph, etc. -- comes along for free), then each
player's 15 flat + 4 red-zone/goal-line fields are overlaid from the week
sheet via apply_sheet_helpers_v_0_1_0.apply_sheet_to_traits() (same
create-missing / zero-removed logic apply_team_season_overrides_v_0_1_0.py
uses for the season-long file).

Output: data/current_rosters/dfs/{TEAM}_traits_2026.json -- overwritten each
time this runs. Only the CURRENT week's compiled JSON is kept on disk (per
Cam, 2026-09-04): the week_NN/*.csv sheets are the historical record, the
JSON is a disposable, always-regeneratable cache -- there is no need to keep
one JSON per week. Point a DFS sim run at rosters_dir="data/current_rosters/dfs"
(NFLGameEngine / BatchSimulator both take that kwarg) to use it.

Usage:
    venv\\Scripts\\python.exe scripts/roster_management/apply_team_week_overrides_v_0_1_0.py <week>
    venv\\Scripts\\python.exe scripts/roster_management/apply_team_week_overrides_v_0_1_0.py <week> <TEAM>
"""
import os
import sys
import json
import glob
import copy

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from src.data_pipeline.week_roster_v_0_1_0 import is_player_row  # noqa: E402
from roster_feed_v_0_1_0 import read_rows  # noqa: E402
from build_2026_rosters_v_0_1_0 import load_skill_dna  # noqa: E402
from apply_sheet_helpers_v_0_1_0 import apply_sheet_to_traits, POS_ORDER  # noqa: E402

SEASON_ROSTERS_DIR = os.path.join("data", "current_rosters")
DFS_ROSTERS_DIR = os.path.join("data", "current_rosters", "dfs")
GONE = ("left_team", "cut", "retired")


def main():
    if len(sys.argv) < 2 or not sys.argv[1].isdigit():
        print(__doc__)
        sys.exit(1)
    week = int(sys.argv[1])
    only = sys.argv[2].upper() if len(sys.argv) > 2 else None

    week_dir = os.path.join("data", "overrides", "2026", f"week_{week:02d}")
    paths = sorted(glob.glob(os.path.join(week_dir, "*.csv")))
    if not paths:
        print(f"No sheets in {week_dir} -- run build_week_overrides_v_0_1_0.py {week} first.")
        sys.exit(1)
    if only:
        paths = [p for p in paths if os.path.basename(p) == f"{only}.csv"]

    os.makedirs(DFS_ROSTERS_DIR, exist_ok=True)
    skill_dna = load_skill_dna()

    grand_updated = grand_created = grand_zeroed = 0
    for p in paths:
        team = os.path.basename(p)[:-4]
        season_path = os.path.join(SEASON_ROSTERS_DIR, f"{team}_traits_2026.json")
        if not os.path.exists(season_path):
            print(f"{team}: no season-long traits file, skipped")
            continue

        _, rows = read_rows(p)
        rows = [r for r in rows if is_player_row(r)
               and (r.get("roster_slot") or "").strip().lower() not in GONE]

        data = copy.deepcopy(json.load(open(season_path)))
        data["max_week"] = week
        data["roster_kind"] = "dfs_week"
        traits = data["traits"]
        updated, created, zeroed = apply_sheet_to_traits(traits, rows, skill_dna)

        data["traits"] = dict(sorted(traits.items(), key=lambda kv: POS_ORDER.get(kv[1].get("pos"), 4)))
        out_path = os.path.join(DFS_ROSTERS_DIR, f"{team}_traits_2026.json")
        json.dump(data, open(out_path, "w"), indent=4)

        print(f"{team}: {len(updated)} updated, {len(created)} created, {len(zeroed)} zeroed -> {out_path}")
        grand_updated += len(updated)
        grand_created += len(created)
        grand_zeroed += len(zeroed)

    print(f"\nWeek {week}: {grand_updated} updated / {grand_created} created / {grand_zeroed} zeroed "
         f"across {len(paths)} team(s). Point a sim at rosters_dir='{DFS_ROSTERS_DIR}'.")


if __name__ == "__main__":
    main()
