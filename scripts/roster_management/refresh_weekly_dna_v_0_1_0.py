"""Weekly production refresh: pulls real PBP through the most recently
completed week, blends it against each player's frozen preseason
projection (veterans) or rookie curve (rookies) and writes the result back
into data/current_rosters/{TEAM}_traits_{year}.json for the upcoming game.
A veteran's flat fields, zone-split fields (target_share/carry_share/cpoe/
catch_rate per primary/redzone/goalline), AND team defense all use
dna_blender_v_0_1_0's volume-pooled mechanism (preseason_projection_volume
for flat fields, from rebuild_veteran_baseline_v_0_1_0.py's --volume-only;
a fixed TEAM_DEFENSE_HISTORICAL_GAMES/ZONE_HISTORICAL_GAMES=17
pseudo-historical-games pool for team defense/zone splits, which don't have
a real per-field volume figure to pool against -- see
docs/implementation_plans/volume_weighted_dna_blend_plan.md). Only rookies
still use the original fixed-taper/steady-state formula (dna_blender_v_0_1_0's
module docstring explains why).

Also detects real starter changes (a backup/rookie genuinely taking over,
e.g. at QB) and flips `status` accordingly -- see game_engine.py's
_get_starter_static(), which only considers 'active'-status players and
otherwise has no way to react to an in-season change since it sorts by
static career total_attempts/total_targets. NOT run automatically (no
scheduled task set up yet, per 2026-07-22 decision) -- run by hand after
each week's games complete.

Usage: python refresh_weekly_dna_v_0_1_0.py <year> <completed_week>
  e.g. `python refresh_weekly_dna_v_0_1_0.py 2026 3` after week 3 finishes,
  writes projections for week 4 (game_number = completed_week + 1).
"""
import sys
import os
import json
import glob
import pandas as pd
import nfl_data_py as nfl

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import (
    build_team_week_totals, build_team_week_zone_totals,
    rolling_stats_for_player, rolling_stats_for_player_zones, rolling_stats_for_team,
    ZONES, ZONE_SPLIT_FIELDS, TEAM_RATE_FIELDS,
)
from src.data_pipeline.dna_blender_v_0_1_0 import blend_player_dna, blend_team_dna
from src.data_pipeline.rookie_curves_v_0_1_0 import resolve_rookie_curves
from src.data_pipeline.trench_composite_stats_v_0_1_0 import compute_trench_composites, ALL_TRENCH_FIELDS
from export_live_week_overrides_v_0_1_0 import export_live_week

ROSTERS_DIR = "data/current_rosters"
DNA_DIR = "data/dna"
STARTER_FLIP_POSITIONS = ["QB", "RB"]
STARTER_FLIP_LOOKBACK_WEEKS = 2

# Team-defense volume-pooled blend (Cam, 2026-09-17): same "unproportional
# balance" bug as the player taper (see volume_weighted_dna_blend_plan.md) --
# refresh_team_defense() below was still pooling the frozen preseason
# PFF-ranking baseline against real in-season data via the fixed taper (20%
# real-data weight at game 2 regardless of how little of the season that
# really is), which is what let one week's small-sample defensive z-scores
# swing a matchup projection as hard as a full season of PFF grading did
# (confirmed live: NYJ's pass_def_z went from a real week of data alone).
# There's no real per-play volume persisted for the historical baseline the
# way rebuild_veteran_baseline_v_0_1_0.py built for players, so this treats
# the preseason baseline as if it represents one full 17-game season and the
# real in-season data as `completed_week` games of that same season -- i.e.
# week 1's real data pools in at 1/17th weight, not a flat 20%. Applies to
# every field this function blends (the 4 composite z-scores + their raw
# inputs, plus TEAM_RATE_FIELDS).
TEAM_DEFENSE_HISTORICAL_GAMES = 17

# Same trick, applied to a veteran's zone-split fields (target_share/
# carry_share/cpoe/catch_rate per primary/redzone/goalline) -- added
# 2026-09-19 after a WR's primary-zone target_share swung ~4x off ONE real
# game under the old flat 80/20 (game-2) taper. MIN_ZONE_SAMPLES
# (rolling_stats_v_0_1_0.py) still gates a zone/field out entirely below 5
# real plays -- this only changes how much weight a real sample gets once
# it clears that gate, same as it already does for team defense above.
# Rookies stay on the old taper (see blend_one_player -- no real historical
# volume exists for them, and their curve_override already provides its own
# usage ramp).
ZONE_HISTORICAL_GAMES = 17


def load_rookie_projections(year):
    path = os.path.join(DNA_DIR, f"rookie_projections_{year}.json")
    if not os.path.exists(path):
        return {}
    return json.load(open(path))


def blend_one_player(traits, pbp, team_totals, team_zone_totals, ngs_pass_df, ngs_recv_df,
                      completed_week, game_number, rookie_projections):
    pos = traits["pos"]
    player_id = traits.get("player_id")

    if player_id and len(pbp):
        season_actual, l4_actual, season_volume, l4_volume = rolling_stats_for_player(
            pbp, player_id, pos, completed_week, team_totals,
            ngs_pass_df=ngs_pass_df, ngs_recv_df=ngs_recv_df,
        )
        zone_season, zone_l4 = rolling_stats_for_player_zones(
            pbp, player_id, pos, completed_week, team_zone_totals=team_zone_totals,
        )
    else:
        season_actual, l4_actual, season_volume, l4_volume = {}, {}, {}, {}
        zone_season = {f: {z: None for z in ZONES} for f in ZONE_SPLIT_FIELDS}
        zone_l4 = {f: {z: None for z in ZONES} for f in ZONE_SPLIT_FIELDS}

    is_rookie = traits.get("rookie") and traits.get("_name") in rookie_projections
    if is_rookie:
        rp = rookie_projections[traits["_name"]]
        projection = dict(rp.get("static_projection", {}))
        curve_override = {}
        curve_override.update(resolve_rookie_curves(rp.get("usage_curve", {}), game_number))
        curve_override.update(resolve_rookie_curves(rp.get("efficiency_curve", {}), game_number))
        # No per-zone curves for rookies yet (Phase 4 didn't build that) --
        # reuse the same flat curve-resolved value as every zone's projection
        # baseline until real zone-specific data exists to blend against.
        flat_projection_by_field = dict(projection)
        flat_projection_by_field.update(curve_override)
        zone_projection = {zone: {f: flat_projection_by_field[f] for f in ZONE_SPLIT_FIELDS if f in flat_projection_by_field} for zone in ZONES}
        # Rookies aren't in rebuild_veteran_baseline_v_0_1_0.py's population --
        # no real historical volume exists for them, and their curve_override
        # already provides its own usage ramp, which zero-volume pooling
        # would instantly override with one real game instead of smoothing --
        # explicit None routes them to dna_blender's original fixed-taper
        # mechanism, unchanged (see dna_blender_v_0_1_0.py's module docstring).
        projection_volume = None
    else:
        projection = traits.get("preseason_projection", {})
        curve_override = None
        zone_projection = projection.get("splits", {})
        projection_volume = traits.get("preseason_projection_volume")

    blended = blend_player_dna(
        projection, l4_actual, season_actual, game_number, curve_override,
        projection_volume=projection_volume, season_volume=season_volume,
    )
    for field, value in blended.items():
        if value is not None:
            traits[field] = round(value, 4) if isinstance(value, float) else value

    traits.setdefault("splits", {})
    for zone in ZONES:
        if zone == "primary":
            # "primary" = every play outside the red zone/goal line, i.e.
            # the large majority of snaps -- it should just track the flat/
            # overall blended value (`blended` above), not run its own
            # separate zone-blend. Found 2026-09-19 (Cam): a cluster of
            # depth/complementary players league-wide (Kalif Raymond,
            # Kendrick Bourne, Michael Mayer, DeMario Douglas, etc. -- 13
            # found on a league scan) had a frozen
            # preseason_projection.splits.primary anchor that had drifted
            # 2-4x above their flat preseason target_share; even the
            # corrected volume-pooled weighting (ZONE_HISTORICAL_GAMES,
            # below) mostly reproduced that bad anchor at ~94% weight since
            # the anchor itself, not real in-season data, was the problem.
            # data/overrides/2026/week_NN/{TEAM}_live.csv (this script's own
            # sanity-check export, see export_live_week_overrides_v_0_1_0.py)
            # never carries a separate primary column -- only flat
            # target_share/carry_share plus rz_*/gl_* -- confirming the flat
            # value IS the intended source of truth here. redzone/goalline
            # below are unaffected: those are genuinely distinct situational
            # splits with their own real-sample gating and Cam's hand-tuned
            # apply_zone_usage_overrides_v_0_1_0.py override on top.
            for field in ZONE_SPLIT_FIELDS:
                if field in blended and blended[field] is not None:
                    v = blended[field]
                    traits["splits"].setdefault("primary", {})[field] = round(v, 4) if isinstance(v, float) else v
            continue
        zone_proj = zone_projection.get(zone, {})
        zone_l4_actual = {f: zone_l4[f][zone] for f in ZONE_SPLIT_FIELDS if zone_l4[f].get(zone) is not None}
        zone_season_actual = {f: zone_season[f][zone] for f in ZONE_SPLIT_FIELDS if zone_season[f].get(zone) is not None}
        if is_rookie:
            zone_blended = blend_player_dna(zone_proj, zone_l4_actual, zone_season_actual, game_number)
        else:
            # Volume-pool instead of the flat taper -- see ZONE_HISTORICAL_GAMES.
            zone_projection_volume = {f: ZONE_HISTORICAL_GAMES for f in zone_season_actual}
            zone_season_volume = {f: completed_week for f in zone_season_actual}
            zone_blended = blend_player_dna(zone_proj, zone_l4_actual, zone_season_actual, game_number,
                                             projection_volume=zone_projection_volume,
                                             season_volume=zone_season_volume)
        for field, value in zone_blended.items():
            if value is not None:
                traits["splits"].setdefault(zone, {})[field] = round(value, 4) if isinstance(value, float) else value


def detect_starter_flips(team_traits, pbp, completed_week, lookback=STARTER_FLIP_LOOKBACK_WEEKS):
    """Compares real recent volume (pass attempts for QB, carries for RB)
    between same-position teammates and flips `status` when a
    currently-inactive player has clearly out-volumed the currently-active
    one -- reacting to real evidence rather than predicting a takeover
    week. RB is a narrower/lower-confidence signal than QB (committees are
    legitimate; carry_share already differentiates workload for the main
    per-play sampling -- this only affects _get_starter_static's single
    designated "starter" reference), so it's included but expected to
    matter less in practice."""
    if completed_week < 1:
        return  # preseason, no real data to react to

    for position in STARTER_FLIP_POSITIONS:
        candidates = {name: t for name, t in team_traits.items() if t["pos"] == position}
        if len(candidates) < 2:
            continue

        volumes = {}
        for name, t in candidates.items():
            pid = t.get("player_id")
            if not pid:
                volumes[name] = 0
                continue
            lo = max(1, completed_week - lookback + 1)
            window = pbp[(pbp["week"] >= lo) & (pbp["week"] <= completed_week)]
            if position == "QB":
                volumes[name] = int((window["passer_player_id"] == pid).sum())
            else:
                volumes[name] = int((window["rusher_player_id"] == pid).sum())

        if not any(volumes.values()):
            continue  # no real volume yet for anyone at this position

        top_name = max(volumes, key=volumes.get)
        currently_active = {name for name, t in candidates.items() if t.get("status", "active") == "active"}

        if top_name not in currently_active and volumes[top_name] > 0:
            print(f"  Starter flip: {position} -> {top_name} ({volumes[top_name]} plays over last {lookback} wks)")
            for name, t in candidates.items():
                t["status"] = "active" if name == top_name else "inactive"


def refresh_team_defense(team, year, pbp, completed_week, game_number, trench_dna,
                          trench_composites_season=None, trench_composites_l4=None):
    """Blends trench_dna.json[year][team]'s live rate fields (def_pressure_rate,
    def_sack_rate, sack_rate_allowed, and -- as of this version -- the 4
    composite z-scores run_block_off_z/run_def_z/pass_block_off_z/pass_def_z
    plus every raw metric feeding them) against real in-season team defense
    data, same taper/steady-state mechanism as players (see
    dna_blender_v_0_1_0's TAPER_SCHEDULE: 100/0 before game 1 down to 20/80
    before game 5, then 2/3 L4 + 1/3 season from game 6 on). Mutates
    trench_dna in place.

    trench_composites_season/l4: {team: {field: value}} from
    trench_composite_stats_v_0_1_0.compute_trench_composites(), already
    scoped to the right week windows by the caller -- merged in alongside
    rolling_stats_for_team()'s simple-rate output before blending, so both
    go through the exact same taper call.

    One-time migration: the composite fields were never part of
    preseason_projection (they were seeded straight onto the live entry from
    PFF's 2026 rankings, outside this blend pipeline entirely -- see
    build_2026_trench_shell_v_0_1_0.py's original scoping note). Backfilled
    here, from whatever the entry's current value is, the first time this
    runs for a given team -- idempotent, since every week after that
    preseason_projection already has the key and this is a no-op.

    Also persists RAW (unblended) def_sack_rate_l4/sack_rate_allowed_l4 --
    Gate 2's off_sack_rate_l4/def_sack_rate_l4 model features (see Phase 7e)
    want a genuine last-4-games rate specifically, not a taper-blended one;
    these sit alongside the blended fields rather than replacing them."""
    entry = trench_dna.get(str(year), {}).get(team)
    if entry is None:
        return  # no shell built for this team/year yet
    projection = entry.setdefault("preseason_projection", {})
    for field in ALL_TRENCH_FIELDS:
        if field not in projection and field in entry:
            projection[field] = entry[field]

    if len(pbp):
        season_actual, l4_actual = rolling_stats_for_team(pbp, team, completed_week)
    else:
        season_actual, l4_actual = {}, {}
    season_actual = dict(season_actual)
    l4_actual = dict(l4_actual)
    if trench_composites_season:
        season_actual.update(trench_composites_season.get(team, {}))
    if trench_composites_l4:
        l4_actual.update(trench_composites_l4.get(team, {}))

    # See TEAM_DEFENSE_HISTORICAL_GAMES above -- volume-pool every field this
    # function blends instead of the flat taper. completed_week is real
    # season sample size in "games" units; TEAM_DEFENSE_HISTORICAL_GAMES
    # stands in for the preseason baseline's (unpersisted) historical volume.
    blend_fields = tuple(TEAM_RATE_FIELDS) + ALL_TRENCH_FIELDS
    projection_volume = {f: TEAM_DEFENSE_HISTORICAL_GAMES for f in blend_fields}
    season_volume = {f: completed_week for f in blend_fields}
    blended = blend_team_dna(projection, l4_actual, season_actual, game_number,
                              projection_volume=projection_volume, season_volume=season_volume)
    for field, value in blended.items():
        if value is not None:
            entry[field] = round(value, 4) if isinstance(value, float) else value

    for raw_field in ("def_sack_rate", "sack_rate_allowed"):
        raw_l4 = l4_actual.get(raw_field)
        if raw_l4 is not None:
            entry[f"{raw_field}_l4"] = round(raw_l4, 4)


def refresh(year, completed_week):
    game_number = completed_week + 1
    rookie_projections = load_rookie_projections(year)

    print(f"Pulling PBP for {year} through week {completed_week}...")
    # include_participation=False: the pbp_participation_{year}.parquet file
    # nflverse publishes separately (formation/personnel/box-count columns,
    # left-joined on by nfl_data_py) reliably lags the main PBP release early
    # in a season -- when it 404s, a bug in the pinned nfl_data_py==0.3.2
    # (`except Error` references an undefined name) masks that 404 behind a
    # confusing NameError instead of the library's own "data not available"
    # message. Nothing in rolling_stats_v_0_1_0.py reads any participation
    # column (offense_formation, defenders_in_box, etc.), so skipping it costs
    # nothing we actually use.
    pbp = nfl.import_pbp_data([year], include_participation=False)
    if "play_type" not in pbp.columns:
        # No PBP at all for this season yet (e.g. pure preseason, before
        # week 1 has been played) -- nfl_data_py returns a fully empty,
        # columnless frame rather than one with 0 matching rows. Harmless
        # for completed_week=0 (game_number=1 is 100% projection weight
        # regardless, per the taper), but real weeks should never hit this.
        print(f"No PBP data available for {year} yet -- proceeding with zero in-season data.")
        pbp = pd.DataFrame(columns=["play_type", "week", "posteam", "defteam"])
    pbp = pbp[pbp["play_type"].isin(["pass", "run"])]
    pbp = pbp[pbp["week"] <= completed_week]
    team_totals = build_team_week_totals(pbp) if len(pbp) else {}
    team_zone_totals = build_team_week_zone_totals(pbp) if len(pbp) else {}

    print(f"Pulling NGS passing/receiving for {year} through week {completed_week}...")
    ngs_pass = nfl.import_ngs_data("passing", [year])
    ngs_recv = nfl.import_ngs_data("receiving", [year])
    ngs_pass = ngs_pass[ngs_pass["week"] <= completed_week] if len(ngs_pass) else ngs_pass
    ngs_recv = ngs_recv[ngs_recv["week"] <= completed_week] if len(ngs_recv) else ngs_recv

    print(f"Pulling PFR pass/rush + NGS rushing for {year} through week {completed_week} "
          f"(trench composites: run_block_off_z, run_def_z, pass_block_off_z, pass_def_z)...")
    pfr_pass = nfl.import_weekly_pfr(s_type="pass", years=[year])
    pfr_rush = nfl.import_weekly_pfr(s_type="rush", years=[year])
    ngs_rush = nfl.import_ngs_data("rushing", [year])
    pfr_pass = pfr_pass[pfr_pass["week"] <= completed_week] if len(pfr_pass) else pfr_pass
    pfr_rush = pfr_rush[pfr_rush["week"] <= completed_week] if len(pfr_rush) else pfr_rush
    ngs_rush = ngs_rush[(ngs_rush["week"] <= completed_week) & (ngs_rush["week"] > 0)] if len(ngs_rush) else ngs_rush

    season_weeks = list(range(1, completed_week + 1))
    l4_weeks = season_weeks[-4:]
    if completed_week >= 1 and len(pbp):
        trench_composites_season = compute_trench_composites(pbp, pfr_pass, pfr_rush, ngs_rush, season_weeks)
        pbp_l4 = pbp[pbp["week"].isin(l4_weeks)]
        pfr_pass_l4 = pfr_pass[pfr_pass["week"].isin(l4_weeks)] if len(pfr_pass) else pfr_pass
        pfr_rush_l4 = pfr_rush[pfr_rush["week"].isin(l4_weeks)] if len(pfr_rush) else pfr_rush
        ngs_rush_l4 = ngs_rush[ngs_rush["week"].isin(l4_weeks)] if len(ngs_rush) else ngs_rush
        trench_composites_l4 = compute_trench_composites(pbp_l4, pfr_pass_l4, pfr_rush_l4, ngs_rush_l4, l4_weeks)
    else:
        trench_composites_season, trench_composites_l4 = {}, {}

    trench_path = os.path.join(DNA_DIR, "trench_dna.json")
    trench_dna = json.load(open(trench_path))

    roster_files = glob.glob(os.path.join(ROSTERS_DIR, f"*_traits_{year}.json"))
    before_counts = {}
    for path in roster_files:
        data = json.load(open(path))
        team = data["team"]
        before_counts[team] = set(data["traits"].keys())

        for name, traits in data["traits"].items():
            traits["_name"] = name  # transient, stripped before write
            blend_one_player(traits, pbp, team_totals, team_zone_totals, ngs_pass, ngs_recv,
                              completed_week, game_number, rookie_projections)

        detect_starter_flips(data["traits"], pbp, completed_week)
        refresh_team_defense(team, year, pbp, completed_week, game_number, trench_dna,
                              trench_composites_season, trench_composites_l4)

        for traits in data["traits"].values():
            traits.pop("_name", None)

        data["max_week"] = completed_week
        with open(path, "w") as f:
            json.dump(data, f, indent=4)

    with open(trench_path, "w") as f:
        json.dump(trench_dna, f, indent=2)

    # Sanity check: refresh should never add/remove players, only update values.
    dropped = []
    for path in roster_files:
        data = json.load(open(path))
        team = data["team"]
        after = set(data["traits"].keys())
        missing = before_counts[team] - after
        if missing:
            dropped.append((team, missing))
    if dropped:
        raise SystemExit(f"REFRESH SANITY CHECK FAILED -- players disappeared: {dropped}")

    print(f"Refreshed {len(roster_files)} team files for {year}, week {completed_week} -> projecting week {game_number}.")

    export_live_week(year, game_number)


def main():
    if len(sys.argv) != 3:
        print("Usage: python refresh_weekly_dna_v_0_1_0.py <year> <completed_week>")
        sys.exit(1)
    year, completed_week = int(sys.argv[1]), int(sys.argv[2])
    refresh(year, completed_week)


if __name__ == "__main__":
    main()
