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
import json

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.week_roster_v_0_1_0 import resolve_week_rows, is_player_row  # noqa: E402
from roster_feed_v_0_1_0 import read_rows, write_rows, match_key  # noqa: E402
from dfs_status_ledger import load_ledger, statuses_for_week  # noqa: E402
from apply_sheet_helpers_v_0_1_0 import ZONE_COL_MAP  # noqa: E402

SEASON_DIR = os.path.join("data", "overrides", "2026", "season_long")
LIVE_ROSTERS_DIR = os.path.join("data", "current_rosters")
SHARE_COLS = ("target_share", "carry_share", "rz_target_share", "rz_carry_share", "gl_target_share", "gl_carry_share")
WEEK_STATUS_COLS = ("dfs_status", "starter_override")


def _live_share(traits, col):
    """The live traits dict's value behind one sheet share column (top level
    for target_share/carry_share, splits.{redzone,goalline} for rz_*/gl_*)."""
    if col in ZONE_COL_MAP:
        zone, field = ZONE_COL_MAP[col]
        return (traits.get("splits") or {}).get(zone, {}).get(field)
    return traits.get(col)


def fill_live_shares(rows, live_traits):
    """Inputs: sheet rows (dicts), live_traits {name: traits} from
    data/current_rosters/{TEAM}_traits_2026.json (the refresh's blend).
    Output: copies of the rows where every share column of a row that is NOT
    hand_edit=TRUE and whose player exists in the live traits is replaced by the
    live blended value.
    Purpose (window-blend rollout Phase 1b/1c): sheets carry the frozen preseason
    shares, and apply_team_week_overrides overlays any sheet share that differs
    from live -- so unmarked players silently got preseason instead of the
    blend. Filling first means (a) the sheet shows what the sim uses, (b) OUT
    redistribution (resolve_week_rows) works on live numbers, and (c) the
    overlay only carries redistribution and hand edits."""
    by_key = {match_key(n): t for n, t in live_traits.items()}
    out = []
    for r in rows:
        r = dict(r)
        t = by_key.get(match_key(r.get("player_name", ""))) if r.get("player_name") else None
        if t is not None and (r.get("hand_edit") or "").strip().upper() != "TRUE":
            for col in SHARE_COLS:
                v = _live_share(t, col)
                if v is not None:
                    r[col] = repr(float(v))
        out.append(r)
    return out


def _load_live_traits(team):
    path = os.path.join(LIVE_ROSTERS_DIR, f"{team}_traits_2026.json")
    if not os.path.exists(path):
        return None
    with open(path, encoding="utf-8") as f:
        return json.load(f)["traits"]


def build_team_week_rows(season_rows, week, existing_week_path=None, ledger=None, team=None, live_traits=None):
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
    # Live-blend fill (see fill_live_shares). Needs the team's live traits: passed in,
    # or loaded from data/current_rosters when `team` is given; with neither, the
    # sheet's own (preseason) shares are used unchanged -- the pre-2026-10-07 behavior.
    if live_traits is None and team:
        live_traits = _load_live_traits(team)
    rows = fill_live_shares(season_rows, live_traits) if live_traits else [dict(r) for r in season_rows]
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


def report_team_share_sums(week_dir):
    """Report only (Phase 1d, downgraded): the engine renormalizes target/carry shares
    per zone among active players, so team sums off 1.0 do not break sampling -- but the
    raw values are also model features, so print the teams that are far off."""
    odd = []
    for path in sorted(glob.glob(os.path.join(week_dir, "*.csv"))):
        if path.endswith("_live.csv"):
            continue
        _, rows = read_rows(path)
        rows = [r for r in rows if is_player_row(r) and (r.get("dfs_status") or "active") != "out"]
        for col in ("target_share", "carry_share"):
            tot = sum(float(r[col]) for r in rows if r.get(col) not in (None, ""))
            if not 0.9 <= tot <= 1.1:
                odd.append(f"{os.path.basename(path)[:-4]} {col}={tot:.2f}")
    print(f"Team share sums outside 0.90-1.10 (report only): {', '.join(odd) if odd else 'none'}")


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
    report_team_share_sums(out_dir)


if __name__ == "__main__":
    main()
