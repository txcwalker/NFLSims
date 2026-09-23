"""Compile one week's DFS roster/usage sheets
(data/overrides/2026/week_NN/{TEAM}.csv) into their own traits.json tree,
SEPARATE from the season-long one -- so a week's injury-adjusted usage never
overwrites data/current_rosters/{TEAM}_traits_2026.json (the season-long /
rest-of-season file).

Base = a fresh copy of the CURRENT data/current_rosters/{TEAM}_traits_2026.json
-- which by this point already carries refresh_weekly_dna_v_0_1_0.py's
real-data blend, not just the frozen preseason projection. A week sheet is a
byte-for-byte copy of the season-long sheet EXCEPT for OUT-player share
redistribution (build_week_overrides_v_0_1_0.py's resolve_week_rows) -- so
for each EXISTING player, only the target_share/carry_share/rz_*/gl_* fields
that actually differ from the player's LIVE base value get overlaid on top
of it; every other field, including every efficiency field (catch_rate,
ypc, adot, etc. -- the week sheet never redistributes those), is left at
its live-blended value. Brand new players (not yet in the live base at all)
still get the full row applied via enrich_player, same as before -- there's
no live value to preserve for them. A player's static career-DNA fields
(pressure_rate, route_profile, top_speed_mph, etc.) always come from the
live base regardless, since a week sheet never carries them.

**Before this fix, the full 15-field row was overlaid unconditionally for
every player**, silently reverting every unaffected player's live-blended
numbers back to the plain frozen preseason baseline every time this ran --
found 2026-09-16 while wiring the DFS sims to reflect real Week 1 results.

**That fix originally diffed a week-sheet value against the STATIC
season-long sheet, not the live base** -- so a hand-edit that pinned a
share back down to the season-long default (to correct for the live value
having since drifted, e.g. a real Week 1 usage spike) was invisible to the
diff and silently never applied, leaving the stale drifted-up live number
in place. Found 2026-09-19 on Kalif Raymond/CHI (6.5% target share in both
the season-long and week 2 sheets, but his live splits.primary.target_share
had drifted to 26.9% off one real Week 1 game) -- see WORKLOG.md.

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
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from src.data_pipeline.week_roster_v_0_1_0 import is_player_row  # noqa: E402
from roster_feed_v_0_1_0 import read_rows, match_key  # noqa: E402
from build_2026_rosters_v_0_1_0 import load_skill_dna  # noqa: E402
from apply_sheet_helpers_v_0_1_0 import apply_sheet_to_traits, POS_ORDER, ZONE_COL_MAP  # noqa: E402

SEASON_ROSTERS_DIR = os.path.join("data", "current_rosters")
DFS_ROSTERS_DIR = os.path.join("data", "current_rosters", "dfs")
GONE = ("left_team", "cut", "retired")

# The only fields a week sheet can legitimately differ on vs. the season-long
# sheet -- OUT-player redistribution (see resolve_week_rows / SHARE_FIELDS in
# week_roster_v_0_1_0.py).
SHARE_COLS = ("target_share", "carry_share",
              "rz_target_share", "rz_carry_share",
              "gl_target_share", "gl_carry_share")
# roster_slot/return_week/note are season-scope bookkeeping; dfs_status/
# starter_override (added 2026-09-19) are week-only and have no season-long
# value to diff against, so they're carried unconditionally like the rest of
# this tuple rather than through the SHARE_COLS changed-vs-season-long check.
_BOOKKEEPING_COLS = ("player_name", "team", "pos", "player_id",
                      "roster_slot", "return_week", "note",
                      "dfs_status", "starter_override")
_TOL = 1e-9


def _atomic_write_json(path, data):
    """Write `data` as JSON to `path` atomically (temp file + os.replace) so a
    concurrent reader (e.g. a live /api/rosters request) can never see a
    half-written file -- matters once this runs from a request handler
    instead of only as an offline CLI script."""
    os.makedirs(os.path.dirname(path), exist_ok=True)
    fd, tmp = tempfile.mkstemp(dir=os.path.dirname(path), suffix=".tmp")
    try:
        with os.fdopen(fd, "w") as f:
            json.dump(data, f, indent=4)
        os.replace(tmp, path)
    except Exception:
        try:
            os.unlink(tmp)
        except OSError:
            pass
        raise


def _f(v):
    try:
        return float(v)
    except (TypeError, ValueError):
        return None


def _live_share_value(live_player, col):
    """The live traits dict's equivalent of a flat SHARE_COLS name -- top
    level for target_share/carry_share, nested under splits.{redzone,
    goalline} for rz_*/gl_* (same mapping apply_sheet_helpers_v_0_1_0 uses
    to write these columns, ZONE_COL_MAP)."""
    if live_player is None:
        return None
    if col in ZONE_COL_MAP:
        zone, field = ZONE_COL_MAP[col]
        return live_player.get("splits", {}).get(zone, {}).get(field)
    return live_player.get(col)


def _redistribution_only_row(week_row, live_player):
    """Strips a week-sheet row down to bookkeeping + only the SHARE_COLS that
    actually differ from `live_player`'s current LIVE value (the player's
    entry in the just-loaded data/current_rosters/{team}_traits_2026.json,
    i.e. already carrying refresh_weekly_dna_v_0_1_0.py's real-data blend),
    so apply_sheet_to_traits's per-field skip-if-absent logic leaves every
    other (live-blended) field alone instead of stomping it back to the
    frozen preseason number.

    Diffs against the live base, NOT the static season-long sheet -- see
    this module's docstring for why: comparing against season-long made a
    hand-edit that happened to match the season-long default invisible,
    silently keeping whatever stale, drifted-up live number was already
    there instead of applying the edit."""
    out = {k: week_row.get(k, "") for k in _BOOKKEEPING_COLS}
    for col in SHARE_COLS:
        wv = _f(week_row.get(col))
        lv = _live_share_value(live_player, col)
        if wv is not None and (lv is None or abs(wv - lv) >= _TOL):
            out[col] = week_row.get(col)
    return out


def apply_team_week_overrides(week, team):
    """Compile one team's week sheet (data/overrides/2026/week_NN/{team}.csv)
    into its DFS traits JSON. Callable directly (e.g. from the app.py
    roster-status endpoint) as well as via main()'s CLI loop below. Returns
    a report dict {team, updated, created, zeroed, out_path}, or None if the
    team has no season-long traits file or no week sheet yet."""
    week_dir = os.path.join("data", "overrides", "2026", f"week_{week:02d}")
    p = os.path.join(week_dir, f"{team}.csv")
    season_path = os.path.join(SEASON_ROSTERS_DIR, f"{team}_traits_2026.json")
    if not os.path.exists(p) or not os.path.exists(season_path):
        return None

    os.makedirs(DFS_ROSTERS_DIR, exist_ok=True)
    skill_dna = load_skill_dna()

    _, rows = read_rows(p)
    rows = [r for r in rows if is_player_row(r)
           and (r.get("roster_slot") or "").strip().lower() not in GONE]

    data = copy.deepcopy(json.load(open(season_path)))
    data["max_week"] = week
    data["roster_kind"] = "dfs_week"
    traits = data["traits"]

    traits_by_key = {match_key(n): n for n in traits}
    filtered_rows = [
        _redistribution_only_row(r, traits.get(traits_by_key.get(match_key(r.get("player_name", "")))))
        if match_key(r.get("player_name", "")) in traits_by_key
        else r  # brand-new player -- no live value to preserve, apply the full row
        for r in rows
    ]

    updated, created, zeroed = apply_sheet_to_traits(traits, filtered_rows, skill_dna, week)

    data["traits"] = dict(sorted(traits.items(), key=lambda kv: POS_ORDER.get(kv[1].get("pos"), 4)))
    out_path = os.path.join(DFS_ROSTERS_DIR, f"{team}_traits_2026.json")
    _atomic_write_json(out_path, data)

    return {"team": team, "updated": len(updated), "created": len(created),
            "zeroed": len(zeroed), "out_path": out_path}


def main():
    if len(sys.argv) < 2 or not sys.argv[1].isdigit():
        print(__doc__)
        sys.exit(1)
    week = int(sys.argv[1])
    only = sys.argv[2].upper() if len(sys.argv) > 2 else None

    week_dir = os.path.join("data", "overrides", "2026", f"week_{week:02d}")
    # exclude export_live_week_overrides_v_0_1_0.py's *_live.csv sanity sheets --
    # sibling files in the same folder, not hand-curated input.
    paths = sorted(p for p in glob.glob(os.path.join(week_dir, "*.csv"))
                   if not p.endswith("_live.csv"))
    if not paths:
        print(f"No sheets in {week_dir} -- run build_week_overrides_v_0_1_0.py {week} first.")
        sys.exit(1)
    teams = [os.path.basename(p)[:-4] for p in paths]
    if only:
        teams = [t for t in teams if t == only]

    grand_updated = grand_created = grand_zeroed = n_done = 0
    for team in teams:
        report = apply_team_week_overrides(week, team)
        if report is None:
            print(f"{team}: no season-long traits file, skipped")
            continue
        print(f"{team}: {report['updated']} updated, {report['created']} created, "
              f"{report['zeroed']} zeroed -> {report['out_path']}")
        grand_updated += report["updated"]
        grand_created += report["created"]
        grand_zeroed += report["zeroed"]
        n_done += 1

    print(f"\nWeek {week}: {grand_updated} updated / {grand_created} created / {grand_zeroed} zeroed "
         f"across {n_done} team(s). Point a sim at rosters_dir='{DFS_ROSTERS_DIR}'.")


if __name__ == "__main__":
    main()
