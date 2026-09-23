"""Sanity-check export: writes data/overrides/2026/week_NN/{TEAM}_live.csv --
same schema as the hand-curated week_NN/{TEAM}.csv, but every blendable field
pulled from the LIVE post-refresh data/current_rosters/{TEAM}_traits_{year}.json
(the file game_engine.py / the DFS sims actually read from) instead of the
frozen preseason baseline. Meant to sit side-by-side with {TEAM}.csv for an
easy diff -- confirms real in-season results have actually made it into what
the sims use, without touching the hand-edit input file itself.

Only written for weeks that already have a hand-curated week_NN/ folder (the
override sheets are a first-few-weeks-only mechanism -- see
build_week_overrides_v_0_1_0.py's docstring); later weeks are skipped since
there's no sheet left to sit beside.

Called automatically from refresh_weekly_dna_v_0_1_0.py at the end of each
weekly refresh (for week = completed_week + 1). Also runnable standalone:

Usage: python export_live_week_overrides_v_0_1_0.py <year> <week> [TEAM]
"""
import sys
import os
import glob
import json

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from roster_feed_v_0_1_0 import read_rows, write_rows, match_key  # noqa: E402

ROSTERS_DIR = "data/current_rosters"
OVERRIDES_DIR = os.path.join("data", "overrides", "2026")

# override-sheet column -> live traits field, either a top-level key or
# ("splits", zone, field) for the rz_/gl_ split columns.
LIVE_FIELD_MAP = {
    "target_share": "target_share",
    "carry_share": "carry_share",
    "catch_rate": "catch_rate",
    "ypc": "ypc",
    "yac_per_rec": "yac_per_rec",
    "cpoe": "cpoe",
    "sack_rate": "sack_rate",
    "scramble_rate": "scramble_rate",
    "adot": "adot",
    "avg_air_yards_per_att": "avg_air_yards_per_att",
    "deep_target_rate": "deep_target_rate",
    "elusiveness": "elusiveness",
    "broken_tackle_rate": "broken_tackle_rate",
    "avg_time_to_throw_sec": "avg_time_to_throw_sec",
    "avg_separation_yds": "avg_separation_yds",
    "rz_target_share": ("splits", "redzone", "target_share"),
    "rz_carry_share": ("splits", "redzone", "carry_share"),
    "gl_target_share": ("splits", "goalline", "target_share"),
    "gl_carry_share": ("splits", "goalline", "carry_share"),
}


def _live_value(traits, spec):
    if isinstance(spec, tuple):
        _, zone, field = spec
        return traits.get("splits", {}).get(zone, {}).get(field, "")
    return traits.get(spec, "")


def _round_or_blank(value):
    return round(value, 4) if isinstance(value, float) else value


def export_team(team, year, week_dir):
    """Returns the list of override-sheet player names with no live-traits
    match (e.g. cut since the sheet was curated), or None if there's no
    override sheet / no live roster file for this team at all."""
    override_path = os.path.join(week_dir, f"{team}.csv")
    traits_path = os.path.join(ROSTERS_DIR, f"{team}_traits_{year}.json")
    if not os.path.exists(override_path) or not os.path.exists(traits_path):
        return None

    cols, rows = read_rows(override_path)
    traits_by_key = {match_key(name): t for name, t in json.load(open(traits_path))["traits"].items()}

    out_rows, unmatched = [], []
    for row in rows:
        out = {c: row.get(c, "") for c in cols}
        traits = traits_by_key.get(match_key(row.get("player_name", "")))
        if traits is None:
            unmatched.append(row.get("player_name", ""))
        else:
            for col, spec in LIVE_FIELD_MAP.items():
                if col in cols:
                    out[col] = _round_or_blank(_live_value(traits, spec))
        out_rows.append(out)

    out_path = os.path.join(week_dir, f"{team}_live.csv")
    try:
        write_rows(out_path, cols, out_rows)
    except OSError as e:
        # A file open elsewhere (Excel, etc.) shouldn't take down the other
        # 31 teams' sanity-check export -- report and move on.
        print(f"  {team}: could not write {out_path} ({e}) -- likely open elsewhere, skipped.")
        return None
    return unmatched


def export_live_week(year, week, only=None):
    week_dir = os.path.join(OVERRIDES_DIR, f"week_{week:02d}")
    if not os.path.isdir(week_dir):
        print(f"No hand-curated {week_dir}/ -- skipping live sanity-check export for week {week}.")
        return

    teams = sorted(
        os.path.basename(p)[:-4]
        for p in glob.glob(os.path.join(week_dir, "*.csv"))
        if not os.path.basename(p).endswith("_live.csv")
    )
    if only:
        teams = [t for t in teams if t == only]

    written, flagged = 0, {}
    for team in teams:
        try:
            unmatched = export_team(team, year, week_dir)
        except Exception as e:  # noqa: BLE001 -- one bad team must not block the other 31
            print(f"  {team}: export failed ({e}), skipped.")
            continue
        if unmatched is None:
            continue
        written += 1
        if unmatched:
            flagged[team] = unmatched

    print(f"Wrote {written} live sanity-check sheet(s) to {week_dir}/*_live.csv.")
    for team, names in flagged.items():
        print(f"  {team}: no live-roster match for {names}")


def main():
    if len(sys.argv) not in (3, 4):
        print(__doc__)
        sys.exit(1)
    year, week = int(sys.argv[1]), int(sys.argv[2])
    only = sys.argv[3] if len(sys.argv) == 4 else None
    export_live_week(year, week, only)


if __name__ == "__main__":
    main()
