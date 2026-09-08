"""Weekly production refresh: pulls real PBP through the most recently
completed week, blends it against each player's frozen preseason
projection (veterans) or rookie curve (rookies) per dna_blender_v_0_1_0's
taper/steady-state formula, and writes the result back into
data/current_rosters/{TEAM}_traits_{year}.json for the upcoming game.

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

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import (
    build_team_week_totals, build_team_week_zone_totals,
    rolling_stats_for_player, rolling_stats_for_player_zones, rolling_stats_for_team,
    ZONES, ZONE_SPLIT_FIELDS,
)
from src.data_pipeline.dna_blender_v_0_1_0 import blend_player_dna, blend_team_dna
from src.data_pipeline.rookie_curves_v_0_1_0 import resolve_rookie_curves

ROSTERS_DIR = "data/current_rosters"
DNA_DIR = "data/dna"
STARTER_FLIP_POSITIONS = ["QB", "RB"]
STARTER_FLIP_LOOKBACK_WEEKS = 2


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
        season_actual, l4_actual = rolling_stats_for_player(
            pbp, player_id, pos, completed_week, team_totals,
            ngs_pass_df=ngs_pass_df, ngs_recv_df=ngs_recv_df,
        )
        zone_season, zone_l4 = rolling_stats_for_player_zones(
            pbp, player_id, pos, completed_week, team_zone_totals=team_zone_totals,
        )
    else:
        season_actual, l4_actual = {}, {}
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
    else:
        projection = traits.get("preseason_projection", {})
        curve_override = None
        zone_projection = projection.get("splits", {})

    blended = blend_player_dna(projection, l4_actual, season_actual, game_number, curve_override)
    for field, value in blended.items():
        if value is not None:
            traits[field] = round(value, 4) if isinstance(value, float) else value

    traits.setdefault("splits", {})
    for zone in ZONES:
        zone_proj = zone_projection.get(zone, {})
        zone_l4_actual = {f: zone_l4[f][zone] for f in ZONE_SPLIT_FIELDS if zone_l4[f].get(zone) is not None}
        zone_season_actual = {f: zone_season[f][zone] for f in ZONE_SPLIT_FIELDS if zone_season[f].get(zone) is not None}
        zone_blended = blend_player_dna(zone_proj, zone_l4_actual, zone_season_actual, game_number)
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


def refresh_team_defense(team, year, pbp, completed_week, game_number, trench_dna):
    """Blends trench_dna.json[year][team]'s live rate fields (def_pressure_rate,
    def_sack_rate, sack_rate_allowed) against real in-season team defense
    data, same taper/steady-state mechanism as players. Composite z-score
    fields (run_block_off_z etc.) are untouched -- see
    build_2026_trench_shell_v_0_1_0.py's scoping note. Mutates trench_dna
    in place.

    Also persists RAW (unblended) def_sack_rate_l4/sack_rate_allowed_l4 --
    Gate 2's off_sack_rate_l4/def_sack_rate_l4 model features (see Phase 7e)
    want a genuine last-4-games rate specifically, not a taper-blended one;
    these sit alongside the blended fields rather than replacing them."""
    entry = trench_dna.get(str(year), {}).get(team)
    if entry is None:
        return  # no shell built for this team/year yet
    projection = entry.get("preseason_projection", {})

    if len(pbp):
        season_actual, l4_actual = rolling_stats_for_team(pbp, team, completed_week)
    else:
        season_actual, l4_actual = {}, {}

    blended = blend_team_dna(projection, l4_actual, season_actual, game_number)
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
    pbp = nfl.import_pbp_data([year])
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
        refresh_team_defense(team, year, pbp, completed_week, game_number, trench_dna)

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


def main():
    if len(sys.argv) != 3:
        print("Usage: python refresh_weekly_dna_v_0_1_0.py <year> <completed_week>")
        sys.exit(1)
    year, completed_week = int(sys.argv[1]), int(sys.argv[2])
    refresh(year, completed_week)


if __name__ == "__main__":
    main()
