"""Generate docs/rosters/2026/{TEAM}.md -- a readable roster snapshot per
team, from the season-long override sheets (data/overrides/2026/season_long/
{TEAM}.csv).

Sections: Active roster (by position, ordered by projected usage), then
Injured / Reserve (with slot + estimated return week), then Practice squad,
then New to team (players added at 0% usage from the feed).

Generated, not edited -- re-run after editing the season-long sheets.

Usage: venv\\Scripts\\python.exe scripts/roster_management/build_roster_md_v_0_1_0.py
"""
import os
import sys
import glob
import datetime

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from roster_feed_v_0_1_0 import POS_ORDER, RESERVE_SLOTS, read_rows  # noqa: E402
from src.data_pipeline.week_roster_v_0_1_0 import is_player_row  # noqa: E402

SEASON_DIR = os.path.join("data", "overrides", "2026", "season_long")
OUT_DIR = os.path.join("docs", "rosters", "2026")
SLOT_LABEL = {"ir": "IR", "pup": "PUP", "nfi": "NFI",
              "suspended": "Suspended", "exempt": "Commissioner's Exempt"}


def _f(v):
    try:
        return float(v)
    except (TypeError, ValueError):
        return 0.0


def usage_str(r):
    t, c = _f(r.get("target_share")), _f(r.get("carry_share"))
    parts = []
    if t:
        parts.append(f"{t*100:.1f}% tgt")
    if c:
        parts.append(f"{c*100:.1f}% car")
    # only surface red-zone / goal-line when it diverges from the overall share
    rzt, rzc = _f(r.get("rz_target_share")), _f(r.get("rz_carry_share"))
    glt, glc = _f(r.get("gl_target_share")), _f(r.get("gl_carry_share"))
    if abs(rzt - t) > 0.02 or abs(rzc - c) > 0.02 or abs(glt - t) > 0.02 or abs(glc - c) > 0.02:
        parts.append(f"(RZ {rzt*100:.0f}/{rzc*100:.0f}, GL {glt*100:.0f}/{glc*100:.0f})")
    return ", ".join(parts) if parts else "-"


def build_one(path):
    team = os.path.basename(path)[:-4]
    _, rows = read_rows(path)
    rows = [r for r in rows if is_player_row(r)]     # drop any 'Totals' row

    active = [r for r in rows if (r["roster_slot"] or "active") == "active"]
    reserve = [r for r in rows if r["roster_slot"] in RESERVE_SLOTS]
    cut = [r for r in rows if r["roster_slot"] in ("cut", "left_team", "retired")]
    ps = [r for r in rows if r["roster_slot"] == "practice_squad"]
    new_to_team = [r for r in rows if (r.get("note") or "").strip().lower() == "new to team"]

    def by_pos(rs):
        return sorted(rs, key=lambda r: (POS_ORDER.get(r["pos"], 4),
                                         -(_f(r["target_share"]) + _f(r["carry_share"])),
                                         r["player_name"]))

    out = [f"# {team} - 2026 Roster",
           "",
           f"<!-- Generated {datetime.date.today().isoformat()} from "
           f"data/overrides/2026/season_long/{team}.csv. Do not hand-edit; "
           f"edit the sheet and re-run build_roster_md_v_0_1_0.py. -->",
           ""]

    out.append("## Active roster")
    out.append("")
    cur_pos = None
    for r in by_pos(active):
        if r["pos"] != cur_pos:
            cur_pos = r["pos"]
            out.append(f"### {cur_pos}")
        tag = "  _(new to team)_" if r in new_to_team else ""
        u = usage_str(r)
        out.append(f"- **{r['player_name']}**" + (f" - {u}" if u != "-" else "") + tag)
    out.append("")

    out.append("## Injured / Reserve")
    out.append("")
    if reserve:
        for r in by_pos(reserve):
            rw = (r.get("return_week") or "").strip()
            when = "out for the season" if rw in ("", "99") else f"est. return wk {rw}"
            note = f" - {r['note'].strip()}" if (r.get("note") or "").strip() and r["note"].strip().lower() != "new to team" else ""
            u = usage_str(r)
            was = f", was {u}" if u != "-" else ""
            out.append(f"- **{r['player_name']}** ({r['pos']}) - "
                       f"{SLOT_LABEL.get(r['roster_slot'], r['roster_slot'])}, {when}"
                       f"{was}{note}")
    else:
        out.append("_None._")
    out.append("")

    if cut:
        out.append("## Cut at final roster - reassign share, then delete from the sheet")
        out.append("")
        for r in by_pos(cut):
            out.append(f"- **{r['player_name']}** ({r['pos']}) - was {usage_str(r)}")
        out.append("")

    out.append("## Practice squad")
    out.append("")
    out.append(", ".join(f"{r['player_name']} ({r['pos']})" for r in by_pos(ps)) or "_None._")
    out.append("")

    added = [r for r in new_to_team if r["roster_slot"] not in RESERVE_SLOTS]
    if added:
        out.append("## New to team (0% usage - set a role in the sheet)")
        out.append("")
        for r in by_pos(added):
            out.append(f"- {r['player_name']} ({r['pos']}, {r['roster_slot']})")
        out.append("")

    os.makedirs(OUT_DIR, exist_ok=True)
    op = os.path.join(OUT_DIR, f"{team}.md")
    with open(op, "w", encoding="utf-8") as f:
        f.write("\n".join(out).rstrip() + "\n")
    return op


def main():
    paths = sorted(glob.glob(os.path.join(SEASON_DIR, "*.csv")))
    if not paths:
        print(f"No sheets in {SEASON_DIR} -- run export_team_season_overrides first.")
        sys.exit(1)
    for p in paths:
        build_one(p)
    print(f"Wrote {len(paths)} roster files to {OUT_DIR}")


if __name__ == "__main__":
    main()
