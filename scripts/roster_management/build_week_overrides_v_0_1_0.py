"""Generate data/overrides/2026/week_NN/{TEAM}.csv from the 32 season-long
sheets (data/overrides/2026/season_long/{TEAM}.csv) for a given week.

For that week, each player whose `roster_slot` is a reserve type (ir / pup /
nfi / suspended / exempt) and whose `return_week` has NOT arrived is treated
as OUT: target_share / carry_share zeroed, and that share redistributed
pro-rata to the `active` SAME-POSITION players (equal split if none holds
any). Players back by `return_week` keep their curated shares; fill-ins
auto-revert. Team share totals are preserved. Efficiency fields are copied
through unchanged.

Same 22-column schema as the season-long sheets, so it is a drop-in
hand-editable sheet too -- but for the FIRST FEW WEEKS ONLY. Once real games
are in, the weekly refresh overwrites these with the rolling L4 average.

Usage:
    venv\\Scripts\\python.exe scripts/roster_management/build_week_overrides_v_0_1_0.py <week>
    venv\\Scripts\\python.exe scripts/roster_management/build_week_overrides_v_0_1_0.py 1 GB   # one team
"""
import os
import sys
import glob

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.week_roster_v_0_1_0 import resolve_week_rows  # noqa: E402
from roster_feed_v_0_1_0 import read_rows, write_rows  # noqa: E402

SEASON_DIR = os.path.join("data", "overrides", "2026", "season_long")


def main():
    if len(sys.argv) < 2 or not sys.argv[1].isdigit():
        print(__doc__)
        sys.exit(1)
    week = int(sys.argv[1])
    only = sys.argv[2] if len(sys.argv) > 2 else None

    out_dir = os.path.join("data", "overrides", "2026", f"week_{week:02d}")
    os.makedirs(out_dir, exist_ok=True)

    paths = sorted(glob.glob(os.path.join(SEASON_DIR, "*.csv")))
    if only:
        paths = [p for p in paths if os.path.basename(p) == f"{only}.csv"]

    grand_out = 0
    for path in paths:
        team = os.path.basename(path)[:-4]
        cols, rows = read_rows(path)

        new_rows, report = resolve_week_rows(rows, week)

        write_rows(os.path.join(out_dir, f"{team}.csv"), cols, new_rows)

        outs = [r for r in report if r["pool"] > 0]
        lost = [r for r in outs if not r["recipients"]]
        if outs:
            desc = "; ".join(f"{r['field'].split('_')[0]} {r['pos']} {r['pool']}" for r in outs)
            print(f"{team}: redistributed [{desc}]" + ("  !! LOST share" if lost else ""))
        grand_out += sum(len(r["recipients"]) for r in outs)

    print(f"\nWrote week {week} sheets to {out_dir}")


if __name__ == "__main__":
    main()
