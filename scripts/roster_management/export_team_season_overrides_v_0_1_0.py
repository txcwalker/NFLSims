"""(Re)generate data/overrides/2026/season_long/{TEAM}.csv -- 32 per-team,
hand-editable, Excel-friendly season-long override sheets. These REPLACE
data/dna/preseason_overrides_2026.csv as the edit surface; the league-wide
CSV is regenerated FROM these by apply_team_season_overrides_v_0_1_0.py.

Base = the current preseason_overrides_2026.csv (flat fields) +
zone_usage_overrides_2026.csv (red-zone / goal-line shares), per team, then:
  - roster status from the official nfl.com pages (roster_feed_v_0_1_0, which
    reads scrape_nfl_rosters_v_0_1_0.py's cache -- run that first)
  - players nfl.com marks off the team: ROW REMOVED if they had no projected
    role, else KEPT with roster_slot=cut and a "reassign+delete" note so a
    real projected role never vanishes silently
  - nfl.com skill players missing from the sheet (trades, post-cut signings):
    ROW ADDED with all shares = 0 and career-DNA / position-default
    efficiency fields (enrich_player)
  - roster_slot + return_week columns filled from nfl.com status + slot defaults

Every existing hand-tuned value is preserved.

Columns:
  player_name, team, pos, player_id, roster_slot, return_week, note,
  target_share, carry_share, catch_rate, ypc, yac_per_rec, cpoe, sack_rate,
  scramble_rate, adot, avg_air_yards_per_att, deep_target_rate, elusiveness,
  broken_tackle_rate, avg_time_to_throw_sec, avg_separation_yds,
  rz_target_share, rz_carry_share, gl_target_share, gl_carry_share

  roster_slot   active | practice_squad | ir | pup | nfi | suspended | exempt
                | cut  (cut = nfl.com says gone but had a role -- reassign+delete)
  return_week   week the player is expected back; 99 = out for the season.
                Blank for active / practice_squad. Drives build_week_overrides
                and build_roster_md.
  rz_*          red-zone (yardline 6-20) target/carry share
  gl_*          goal-line (yardline <=5) target/carry share
  note          free text (kept, not applied downstream)

OVERWRITES all 32 sheets. Run before you have started hand-editing, or after
you have applied + committed your edits and want a fresh baseline.

Usage: venv\\Scripts\\python.exe scripts/roster_management/export_team_season_overrides_v_0_1_0.py
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from build_2026_rosters_v_0_1_0 import enrich_player, load_skill_dna  # noqa: E402
from src.data_pipeline.rolling_stats_v_0_1_0 import PLAYER_RATE_FIELDS, PLAYER_NGS_FIELDS  # noqa: E402
from roster_feed_v_0_1_0 import (  # noqa: E402
    build_feed_index, resolve_status, clean_name, match_key, read_rows, write_rows,
    DROP_SLOTS, RESERVE_SLOTS, RETURN_WEEK_SEED, POS_ORDER,
)

PRESEASON_CSV = os.path.join("data", "dna", "preseason_overrides_2026.csv")
ZONE_CSV = os.path.join("data", "dna", "zone_usage_overrides_2026.csv")
OUT_DIR = os.path.join("data", "overrides", "2026", "season_long")
TUNABLE = PLAYER_RATE_FIELDS + PLAYER_NGS_FIELDS
# red-zone (6-20) + goal-line (<=5) target/carry shares, seeded from ZONE_CSV.
RZ_FIELDS = ["rz_target_share", "rz_carry_share", "gl_target_share", "gl_carry_share"]
ZONE_CSV_SRC = {"rz_target_share": "rz_target_share", "rz_carry_share": "rz_carry_share",
                "gl_target_share": "five_target_share", "gl_carry_share": "five_carry_share"}
COLS = (["player_name", "team", "pos", "player_id", "roster_slot", "return_week", "note"]
        + TUNABLE + RZ_FIELDS)


def load_preseason_by_team():
    by_team = {}
    _, rows = read_rows(PRESEASON_CSV)
    for r in rows:
        by_team.setdefault(r["team"], {})[match_key(r["player_name"])] = r
    return by_team


def load_zone_by_team():
    by_team = {}
    if os.path.exists(ZONE_CSV):
        _, rows = read_rows(ZONE_CSV)
        for r in rows:
            by_team.setdefault(r["team"], {})[match_key(r["player_name"])] = r
    return by_team


def seed_row(base, zone, team, pos, player_id, slot, note):
    rw = RETURN_WEEK_SEED.get(slot, "") if slot in RESERVE_SLOTS else ""
    row = {c: "" for c in COLS}
    row.update({"player_name": base["player_name"] if base else None,
                "team": team, "pos": pos, "player_id": player_id or "",
                "roster_slot": slot, "return_week": rw, "note": note})
    for f in TUNABLE:
        row[f] = (base or {}).get(f, "")
    for f in RZ_FIELDS:
        row[f] = (zone or {}).get(ZONE_CSV_SRC[f], "")
    return row


def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    by_team = load_preseason_by_team()
    zone_by_team = load_zone_by_team()
    by_name, team_feed, lf_index = build_feed_index()
    skill_dna = load_skill_dna()
    teams = sorted(set(by_team) | set(team_feed))

    grand, kept_cut, no_match, fuzzy = 0, [], [], []
    for team in teams:
        rows, seen = [], set()
        zteam = zone_by_team.get(team, {})
        for pkey, prow in by_team.get(team, {}).items():
            zrow = zteam.get(pkey)
            pname = prow["player_name"]
            rec, how = resolve_status(team, pname, by_name, lf_index)
            has_role = bool(_f(prow.get("target_share")) or _f(prow.get("carry_share")))
            if how == "lastname+initial":
                fuzzy.append((team, pname, rec["name"]))

            if rec is None:
                # not on this team's nfl.com roster at all. 0-role -> stale
                # entry, drop it. Real role -> keep as active, flag for review.
                if not has_role:
                    continue
                rows.append(seed_row(prow, zrow, team, prow["pos"], "", "active",
                                     "NOT on nfl.com roster -- confirm (name mismatch?) or delete"))
                seen.add(pkey)
                no_match.append((team, pname))
                continue

            seen.add(rec["key"])                       # so the add-loop skips it too
            slot = rec["slot"]
            if slot in DROP_SLOTS:
                # nfl.com says off the team. No role -> drop silently. Real
                # role -> keep flagged so the share gets reassigned.
                if has_role:
                    rows.append(seed_row(prow, zrow, team, prow["pos"], rec["player_id"],
                                         "cut",
                                         "nfl.com: off roster -- reassign this share, then delete"))
                    seen.add(pkey)
                    kept_cut.append((team, prow["player_name"]))
                continue
            rows.append(seed_row(prow, zrow, team, prow["pos"], rec["player_id"], slot, ""))
            seen.add(pkey)

        for rec in team_feed.get(team, []):
            if rec["key"] in seen or rec["slot"] in DROP_SLOTS:
                continue
            is_rk = str(rec.get("experience", "")).strip().upper() in ("R", "0")
            enr = enrich_player(rec["clean"], rec["pos"], 0.0, 0.0, skill_dna,
                                is_rk, None, player_id=rec["player_id"] or None)
            row = {c: "" for c in COLS}
            row.update({"player_name": rec["clean"], "team": team, "pos": rec["pos"],
                        "player_id": rec["player_id"], "roster_slot": rec["slot"],
                        "return_week": RETURN_WEEK_SEED.get(rec["slot"], "")
                        if rec["slot"] in RESERVE_SLOTS else "",
                        "note": "new to team"})
            for f in TUNABLE:
                row[f] = 0.0 if f in ("target_share", "carry_share") else enr.get(f, "")
            for f in RZ_FIELDS:
                row[f] = 0.0
            rows.append(row)
            seen.add(rec["key"])

        rows.sort(key=lambda r: (POS_ORDER.get(r["pos"], 4),
                                 -( _f(r["target_share"]) + _f(r["carry_share"]) ),
                                 r["player_name"] or ""))
        # trailing Totals row -- a running check that a position group / team
        # isn't over-allocated (the sim renormalises, so it's a sanity aid only)
        tot = {c: "" for c in COLS}
        tot["player_name"] = "Totals"
        for f in TUNABLE + RZ_FIELDS:
            if f.endswith("_share"):
                tot[f] = round(sum(_f(r.get(f)) for r in rows), 4)
        rows.append(tot)
        path = os.path.join(OUT_DIR, f"{team}.csv")
        write_rows(path, COLS, rows)
        grand += len(rows)
        print(f"{team}: {len(rows)} rows -> {path}")
    print(f"\nWrote {len(teams)} sheets, {grand} rows total, to {OUT_DIR}")
    if kept_cut:
        print(f"\n{len(kept_cut)} player(s) nfl.com has off the roster but who still had a "
              f"projected role -- kept as roster_slot=cut, REASSIGN their share then delete:")
        for t, n in sorted(kept_cut):
            print(f"  {t}  {n}")
    if fuzzy:
        print(f"\n{len(fuzzy)} player(s) matched to nfl.com by last name + first initial "
              f"(sheet name kept) -- sanity check these:")
        for t, sheet, nflcom in sorted(fuzzy):
            print(f"  {t}  {sheet!r}  ==  {nflcom!r}")
    if no_match:
        print(f"\n{len(no_match)} player(s) with a projected role NOT found on their nfl.com "
              f"roster (kept active, flagged) -- likely a name mismatch or a cut; check each:")
        for t, n in sorted(no_match):
            print(f"  {t}  {n}")


def _f(v):
    try:
        return float(v)
    except (TypeError, ValueError):
        return 0.0


if __name__ == "__main__":
    main()
