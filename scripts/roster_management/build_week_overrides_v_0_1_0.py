"""Generate data/overrides/2026/week_NN/{TEAM}.csv from the 32 season-long
sheets (data/overrides/2026/season_long/{TEAM}.csv) for a given week.

For that week, each player whose `roster_slot` is a reserve type (ir / pup /
nfi / suspended / exempt) and whose `return_week` has NOT arrived is treated
as OUT: target_share / carry_share zeroed, and that share redistributed
pro-rata to the `active` SAME-POSITION players (equal split if none holds
any). Players back by `return_week` keep their curated shares; fill-ins
auto-revert. Team share totals are preserved. Efficiency fields are copied
through unchanged.

Same 22-column schema as the season-long sheets plus two week-only trailing
columns, `dfs_status` (active/out/force_active -- a UI-driven gameday
toggle, sticky across weeks via dfs_status_ledger.py, see
week_roster_v_0_1_0.py) and `starter_override` (TRUE/blank, QB rows
only), so it is a drop-in hand-editable sheet too -- but for the FIRST FEW
WEEKS ONLY. Once real games are in, the weekly refresh overwrites these with
the rolling L4 average.

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
from roster_feed_v_0_1_0 import read_rows, write_rows, match_key  # noqa: E402
from dfs_status_ledger import load_ledger, statuses_for_week  # noqa: E402

SEASON_DIR = os.path.join("data", "overrides", "2026", "season_long")
WEEK_STATUS_COLS = ("dfs_status", "starter_override")


def build_team_week_rows(season_rows, week, existing_week_path=None, ledger=None, team=None):
    """Resolve one team's week sheet from its season_long rows. If
    existing_week_path already has a week sheet, carries forward its
    dfs_status/starter_override values (keyed by match_key(player_name))
    onto the freshly-resolved rows first -- otherwise regenerating a week
    sheet (e.g. after a season_long edit) would silently discard any
    UI-driven gameday toggle already made this week, since those two
    columns only ever live in the week sheet. Returns (new_rows, report),
    same shape as resolve_week_rows.

    ledger/team (2026-09-22, optional): the sticky dfs_status ledger (see
    dfs_status_ledger.py) and this sheet's team abbr. When given, any
    player with an applicable ledger entry gets the ledger's dfs_status,
    overriding the existing week sheet's value -- this is what carries a
    week-3 toggle into weeks 4, 5, ... ."""
    overlay = {}
    if existing_week_path and os.path.exists(existing_week_path):
        _, existing_rows = read_rows(existing_week_path)
        overlay = {
            match_key(r["player_name"]): {
                "dfs_status": r.get("dfs_status", "active"),
                "starter_override": r.get("starter_override", ""),
            }
            for r in existing_rows if r.get("player_name")
        }
    rows = [dict(r) for r in season_rows]
    for r in rows:
        ov = overlay.get(match_key(r.get("player_name", "")), {})
        r["dfs_status"] = ov.get("dfs_status", "active")
        r["starter_override"] = ov.get("starter_override", "")
    if ledger is not None and team:
        sticky = statuses_for_week(ledger, team, week, season_rows)
        for r in rows:
            key = match_key(r.get("player_name", ""))
            if key in sticky:
                r["dfs_status"] = sticky[key]
    return resolve_week_rows(rows, week)


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

    ledger = load_ledger(2026)
    grand_out = 0
    for path in paths:
        team = os.path.basename(path)[:-4]
        cols, rows = read_rows(path)
        out_path = os.path.join(out_dir, f"{team}.csv")

        new_rows, report = build_team_week_rows(rows, week, existing_week_path=out_path,
                                                ledger=ledger, team=team)

        out_cols = list(cols) + [c for c in WEEK_STATUS_COLS if c not in cols]
        write_rows(out_path, out_cols, new_rows)

        outs = [r for r in report if r["pool"] > 0]
        lost = [r for r in outs if not r["recipients"]]
        if outs:
            desc = "; ".join(f"{r['field'].split('_')[0]} {r['pos']} {r['pool']}" for r in outs)
            print(f"{team}: redistributed [{desc}]" + ("  !! LOST share" if lost else ""))
        grand_out += sum(len(r["recipients"]) for r in outs)

    print(f"\nWrote week {week} sheets to {out_dir}")


if __name__ == "__main__":
    main()
