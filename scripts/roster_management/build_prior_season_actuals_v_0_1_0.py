"""Builds a reference CSV of each <target_year>-roster player's real
prior-season (<prior_year>) production, in the same schema as
preseason_overrides_<target_year>.csv, so Cam can eyeball last year's
actual numbers side-by-side while hand-tuning this year's preseason
overrides.

For each player in preseason_overrides_<target_year>.csv (the same
"tunable" veteran/promoted-rookie population that CSV covers -- curve-based
rookies are excluded there already, so they're excluded here too):
  - <target_year> rookies (traits["rookie"] is True on the target-year
    roster file): every field 0 -- no prior NFL season to report.
  - Real prior-year player_id with real prior-year PBP/NGS: real
    season-to-date average through week 18 (regular season only), via
    rolling_stats_for_player() -- the exact same function the live weekly
    refresh pipeline uses, so this is genuinely comparable to what the
    engine would compute in-season.
  - Real player_id but zero prior-year PBP (didn't play all year --
    injury, inactive, off a practice squad, etc.): falls back to the
    player's career DNA average (qb_dna.json/rb_dna.json/wr_dna.json/
    te_dna.json) per field, independently -- e.g. a real 4 games of
    receiving data but a season-ending injury before ever rushing still
    keeps the real receiving numbers and only falls back on rushing.
  - No player_id match at all (name mismatch between the CSV and the
    target-year roster files): left blank, printed as an unmatched-name
    warning at the end for manual review.

QB carry_share is a special case: rolling_stats_for_player's underlying
build_player_game_log() deliberately doesn't compute rushing fields for
QBs (the weekly-refresh pipeline doesn't tune QB rushing volume in-season),
but real QB carry_share is a genuinely meaningful preseason-override input
for mobile QBs -- computed here directly from PBP rather than left blank.
QB target_share is always 0 (QBs are never receivers).

Usage: python build_prior_season_actuals_v_0_1_0.py <prior_year> <target_year>
  e.g. `python build_prior_season_actuals_v_0_1_0.py 2025 2026` before
  hand-editing preseason_overrides_2026.csv.
"""
import sys
import os
import csv
import glob
import json
import nfl_data_py as nfl

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import (
    PLAYER_RATE_FIELDS, PLAYER_NGS_FIELDS,
    build_team_week_totals, rolling_stats_for_player, compute_season_to_date,
)

ROSTERS_DIR = "data/current_rosters"
DNA_DIR = "data/dna"
TUNABLE_FIELDS = PLAYER_RATE_FIELDS + PLAYER_NGS_FIELDS
POS_ORDER = {"QB": 0, "RB": 1, "WR": 2, "TE": 3}
REGULAR_SEASON_MAX_WEEK = 18

# current_rosters' top-level field names that differ from the career-DNA
# field names for the same concept (see rolling_stats_v_0_1_0.py's header
# comment -- confirmed against a real roster file).
CAREER_DNA_FIELD_MAP = {"adot": "avg_target_depth_yds", "yac_per_rec": "yac_per_reception"}


def load_overrides_roster(target_year):
    path = os.path.join(DNA_DIR, f"preseason_overrides_{target_year}.csv")
    if not os.path.exists(path):
        raise SystemExit(f"{path} not found -- run export_preseason_overrides_v_0_1_0.py {target_year} first.")
    with open(path, newline="") as f:
        return list(csv.DictReader(f))


def load_target_year_index(target_year):
    """(team, name) -> traits dict, for player_id + rookie flag."""
    idx = {}
    for path in glob.glob(os.path.join(ROSTERS_DIR, f"*_traits_{target_year}.json")):
        data = json.load(open(path))
        team = data["team"]
        for name, traits in data["traits"].items():
            idx[(team, name)] = traits
    return idx


def load_career_dna():
    dna = {}
    for fname, pos in [("qb_dna.json", "QB"), ("rb_dna.json", "RB"), ("wr_dna.json", "WR"), ("te_dna.json", "TE")]:
        dna[pos] = json.load(open(os.path.join(DNA_DIR, fname)))
    return dna


def career_fallback(pos, name, field, career_dna):
    entry = career_dna.get(pos, {}).get(name)
    if not entry:
        return None
    return entry.get(CAREER_DNA_FIELD_MAP.get(field, field))


def qb_carry_share_season(pbp_run, player_id, team_totals):
    """Real QB rushing share, computed directly since build_player_game_log
    skips rushing fields entirely for QBs (see module docstring)."""
    p_run = pbp_run[pbp_run["rusher_player_id"] == player_id]
    if not len(p_run):
        return None
    by_week = p_run.groupby("week")
    team_mode = p_run["posteam"].mode()
    team = team_mode.iat[0] if len(team_mode) else None
    carries_by_week = by_week.size()
    log = {
        wk: (n / team_totals[(team, wk)]["team_carries"]) if team_totals.get((team, wk)) else None
        for wk, n in carries_by_week.items()
    }
    return compute_season_to_date(log, REGULAR_SEASON_MAX_WEEK)


def build(prior_year, target_year):
    rows_in = load_overrides_roster(target_year)
    idx = load_target_year_index(target_year)
    career_dna = load_career_dna()

    print(f"Pulling {prior_year} PBP...")
    pbp = nfl.import_pbp_data([prior_year])
    pbp = pbp[pbp["play_type"].isin(["pass", "run"])]
    pbp = pbp[pbp["week"] <= REGULAR_SEASON_MAX_WEEK]
    pbp_run = pbp[pbp["play_type"] == "run"]
    team_totals = build_team_week_totals(pbp)

    print(f"Pulling {prior_year} NGS passing/receiving...")
    ngs_pass = nfl.import_ngs_data("passing", [prior_year])
    ngs_recv = nfl.import_ngs_data("receiving", [prior_year])
    ngs_pass = ngs_pass[ngs_pass["week"] <= REGULAR_SEASON_MAX_WEEK] if len(ngs_pass) else ngs_pass
    ngs_recv = ngs_recv[ngs_recv["week"] <= REGULAR_SEASON_MAX_WEEK] if len(ngs_recv) else ngs_recv

    out_rows = []
    unmatched = []
    rookie_count = 0
    career_fallback_count = 0
    real_count = 0

    for r in rows_in:
        name, team, pos = r["player_name"], r["team"], r["pos"]
        traits = idx.get((team, name))
        out = {"player_name": name, "team": team, "pos": pos}

        if traits is None:
            unmatched.append((name, team, pos))
            for f in TUNABLE_FIELDS:
                out[f] = ""
            out_rows.append(out)
            continue

        if traits.get("rookie"):
            rookie_count += 1
            for f in TUNABLE_FIELDS:
                out[f] = 0
            out_rows.append(out)
            continue

        player_id = traits.get("player_id")
        season = {}
        if player_id and len(pbp):
            season, _, _, _ = rolling_stats_for_player(
                pbp, player_id, pos, REGULAR_SEASON_MAX_WEEK, team_totals,
                ngs_pass_df=ngs_pass, ngs_recv_df=ngs_recv,
            )
            if pos == "QB":
                season["carry_share"] = qb_carry_share_season(pbp_run, player_id, team_totals)
                season["target_share"] = 0.0

        used_career = False
        for f in TUNABLE_FIELDS:
            val = season.get(f)
            if val is None or val != val:  # NaN != NaN -- pandas can produce these (e.g. thin xyac_mean_yardage samples)
                fb = career_fallback(pos, name, f, career_dna)
                if fb is not None:
                    val = fb
                    used_career = True
                else:
                    val = None
            out[f] = round(val, 4) if isinstance(val, (int, float)) else ""
        if used_career:
            career_fallback_count += 1
        else:
            real_count += 1
        out_rows.append(out)

    out_rows.sort(key=lambda r: (r["team"], POS_ORDER.get(r["pos"], 4), r["player_name"]))

    out_path = os.path.join(DNA_DIR, f"{prior_year}_actuals_for_{target_year}_overrides.csv")
    with open(out_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["player_name", "team", "pos"] + TUNABLE_FIELDS)
        writer.writeheader()
        writer.writerows(out_rows)

    print(f"\nWrote {len(out_rows)} rows to {out_path}.")
    print(f"  {real_count} with real {prior_year} in-season data for every populated field")
    print(f"  {career_fallback_count} used a career-DNA fallback for at least one field")
    print(f"  {rookie_count} {target_year} rookies zeroed out")
    if unmatched:
        print(f"  {len(unmatched)} unmatched (no player_id found in {target_year} roster files):")
        for n, t, p in unmatched:
            print(f"    {n} ({t}, {p})")


def main():
    if len(sys.argv) != 3:
        print("Usage: python build_prior_season_actuals_v_0_1_0.py <prior_year> <target_year>")
        sys.exit(1)
    build(int(sys.argv[1]), int(sys.argv[2]))


if __name__ == "__main__":
    main()
