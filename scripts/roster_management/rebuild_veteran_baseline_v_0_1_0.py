"""Rebuilds every currently-tunable player's flat preseason_projection
fields (target_share, carry_share, cpoe, ypc, scramble_rate, etc. --
everything preseason_overrides_{target_year}.csv covers) from real PBP/NGS
data over the most recent `source_years`, replacing the original build's
mixed sourcing (career DNA averages 2021-2025 for efficiency fields, a
stale week-1-only 2025 snapshot for usage shares -- see
build_2026_rosters_v_0_1_0.py's enrich_player()/load_2025_shares()).

Scope: only players who already have a preseason_projection block in
data/current_rosters/{TEAM}_traits_{target_year}.json -- i.e. exactly the
population preseason_overrides_{target_year}.csv already covers (returning
veterans + any rookie already promoted to the flat track, see
promote_rookie_to_flat_v_0_1_0.py). Never adds or removes a player.

Per player, per field: if real PBP/NGS data exists anywhere in the
source_years window, that replaces the field (and its preseason_projection
mirror). If not (a promoted rookie with zero NFL snaps in the window, or a
rare veteran who missed the whole window -- injury, etc.), the field is
left exactly as it was -- no fallback to career DNA, no blanking. This
intentionally does NOT touch each player's `splits` sub-object
(zone-conditioned values) -- out of scope, since
preseason_overrides_{target_year}.csv only covers the flat fields.

Multi-season averaging: PBP/NGS weeks across the source years are combined
into one timeline via a synthetic per-season week offset (season_index *
100 + real_week) fed straight into rolling_stats_v_0_1_0's existing
season-to-date primitives -- reuses that tested machinery unmodified
rather than duplicating its per-field logic for a multi-season case. This
computes an unweighted mean of weekly values across every real game in the
window (same convention the single-season pipeline already uses -- not a
pooled-totals average). NGS's week=0 "season aggregate" rows are dropped
BEFORE offsetting -- offsetting first would turn a second source year's
week-0 aggregate row into e.g. week 100, which looks like a real week to
the >0 filter build_player_ngs_game_log() already applies.

QB carry_share/target_share get the same special-case treatment
build_prior_season_actuals_v_0_1_0.py uses: target_share forced 0.0 (QBs
are never receivers), carry_share computed directly from PBP since
rolling_stats_v_0_1_0's build_player_game_log() deliberately skips rushing
fields for QBs.

After running, re-run export_preseason_overrides_v_0_1_0.py to regenerate
preseason_overrides_{target_year}.csv from the updated roster files.

Volume (Phase 2 of docs/implementation_plans/volume_weighted_dna_blend_plan.md):
every rebuilt field also gets a `preseason_projection_volume` sibling block
-- the real total sample size (attempts/targets/carries/dropbacks/
completions) behind that historical rate, summed across the whole
source_years window. Not yet consumed anywhere (dna_blender_v_0_1_0 is
still the fixed taper as of this writing) -- persisted now so Phase 3 has
real historical volume to pool against once it's wired in.

--volume-only: computes and writes ONLY `preseason_projection_volume`,
never touches the rate fields (`traits[field]`/`preseason_projection[field]`
stay exactly as they are, including anything hand-tuned via the season-long
override CSVs -- apply_team_season_overrides writes into
preseason_projection too, so a normal (non-`--volume-only`) run silently
overwrites hand-tuned values with a freshly-pulled real average, same as it
always has). data/current_rosters/*_traits_{year}.json is gitignored -- no
git safety net -- so default to --volume-only unless a real baseline
refresh (not just backfilling volume) is actually intended.

Usage: python rebuild_veteran_baseline_v_0_1_0.py <target_year> <source_year> [<source_year> ...] [--volume-only]
  e.g. python rebuild_veteran_baseline_v_0_1_0.py 2026 2024 2025 --volume-only
"""
import sys
import os
import glob
import json
import pandas as pd
import nfl_data_py as nfl

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import (
    PLAYER_RATE_FIELDS, PLAYER_NGS_FIELDS,
    build_team_week_totals, rolling_stats_for_player, compute_season_to_date,
    compute_season_to_date_volume,
)

ROSTERS_DIR = "data/current_rosters"
TUNABLE_FIELDS = PLAYER_RATE_FIELDS + PLAYER_NGS_FIELDS
REGULAR_SEASON_MAX_WEEK = 18
SEASON_OFFSET = 100  # keeps each source season's weeks distinct once combined


def load_combined_pbp(source_years):
    frames = []
    for i, year in enumerate(sorted(source_years)):
        pbp = nfl.import_pbp_data([year])
        pbp = pbp[pbp["play_type"].isin(["pass", "run"])]
        pbp = pbp[pbp["week"] <= REGULAR_SEASON_MAX_WEEK].copy()
        pbp["week"] = pbp["week"] + i * SEASON_OFFSET
        frames.append(pbp)
    return pd.concat(frames, ignore_index=True)


def load_combined_ngs(kind, source_years):
    frames = []
    for i, year in enumerate(sorted(source_years)):
        ngs = nfl.import_ngs_data(kind, [year])
        if not len(ngs):
            continue
        ngs = ngs[(ngs["week"] > 0) & (ngs["week"] <= REGULAR_SEASON_MAX_WEEK)].copy()
        ngs["week"] = ngs["week"] + i * SEASON_OFFSET
        frames.append(ngs)
    return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()


def qb_carry_share(pbp_run, player_id, team_totals, through_week):
    """Returns (rate, volume) -- volume is real total carries across the
    window, the same denominator concept build_player_game_log() exposes
    for every other field, computed here by hand since QB carry_share
    itself is already a hand-computed special case (rolling_stats_v_0_1_0's
    build_player_game_log() deliberately skips rushing shares for QBs)."""
    p_run = pbp_run[pbp_run["rusher_player_id"] == player_id]
    if not len(p_run):
        return None, 0
    by_week = p_run.groupby("week")
    team_mode = p_run["posteam"].mode()
    team = team_mode.iat[0] if len(team_mode) else None
    carries_by_week = by_week.size()
    log = {
        wk: (n / team_totals[(team, wk)]["team_carries"]) if team_totals.get((team, wk)) else None
        for wk, n in carries_by_week.items()
    }
    rate = compute_season_to_date(log, through_week)
    volume = compute_season_to_date_volume(carries_by_week.to_dict(), through_week)
    return rate, volume


def rebuild(target_year, source_years, volume_only=False):
    print(f"Pulling PBP for {source_years}...")
    pbp = load_combined_pbp(source_years)
    pbp_run = pbp[pbp["play_type"] == "run"]
    team_totals = build_team_week_totals(pbp)
    through_week = int(pbp["week"].max())

    print(f"Pulling NGS passing/receiving for {source_years}...")
    ngs_pass = load_combined_ngs("passing", source_years)
    ngs_recv = load_combined_ngs("receiving", source_years)

    roster_files = glob.glob(os.path.join(ROSTERS_DIR, f"*_traits_{target_year}.json"))

    updated_players, untouched_players, out_of_scope = 0, 0, 0
    field_updates, volume_updates = 0, 0

    for path in roster_files:
        data = json.load(open(path))
        file_changed = False
        for name, traits in data["traits"].items():
            if "preseason_projection" not in traits:
                out_of_scope += 1
                continue  # not in preseason_overrides_{target_year}.csv's population

            pos = traits["pos"]
            player_id = traits.get("player_id")
            season, season_volume = {}, {}
            if player_id and len(pbp):
                season, _, season_volume, _ = rolling_stats_for_player(
                    pbp, player_id, pos, through_week, team_totals,
                    ngs_pass_df=ngs_pass, ngs_recv_df=ngs_recv,
                )
                if pos == "QB":
                    season["carry_share"], season_volume["carry_share"] = qb_carry_share(
                        pbp_run, player_id, team_totals, through_week
                    )
                    # QBs are never real receivers -- forced constant, not a
                    # real rate with a volume behind it (see module docstring).
                    season["target_share"] = 0.0
                    season_volume["target_share"] = 0

            player_rate_changed = False
            player_volume_changed = False
            volume_block = traits.setdefault("preseason_projection_volume", {})
            for field in TUNABLE_FIELDS:
                vol = season_volume.get(field)
                if vol is not None and volume_block.get(field) != vol:
                    volume_block[field] = int(vol)
                    volume_updates += 1
                    player_volume_changed = True

                if volume_only:
                    continue
                val = season.get(field)
                if val is None or val != val:  # None or NaN -- no real data for this field, leave as-is
                    continue
                val = round(float(val), 4)
                if traits.get(field) != val:
                    traits[field] = val
                    traits["preseason_projection"][field] = val
                    player_rate_changed = True
                    field_updates += 1

            if player_rate_changed or player_volume_changed:
                updated_players += 1
                file_changed = True
            else:
                untouched_players += 1

        if file_changed:
            with open(path, "w") as f:
                json.dump(data, f, indent=4)

    print(f"\n{updated_players} player(s) had at least one field (rate or volume) updated.")
    print(f"  {field_updates} rate field value(s) changed.")
    print(f"  {volume_updates} preseason_projection_volume field(s) written/changed.")
    print(f"{untouched_players} player(s) in scope had no real data in {source_years} for any field -- left exactly as-is.")
    print(f"{out_of_scope} player(s) skipped -- no preseason_projection block (not in the CSV's population).")


def main():
    if len(sys.argv) < 3:
        print("Usage: python rebuild_veteran_baseline_v_0_1_0.py <target_year> <source_year> [<source_year> ...] [--volume-only]")
        sys.exit(1)
    volume_only = "--volume-only" in sys.argv[1:]
    years = [a for a in sys.argv[1:] if a != "--volume-only"]
    rebuild(int(years[0]), [int(y) for y in years[1:]], volume_only=volume_only)


if __name__ == "__main__":
    main()
