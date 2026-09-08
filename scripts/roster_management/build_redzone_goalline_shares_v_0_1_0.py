"""Builds real, historical redzone/goalline target_share and carry_share for
every 2026 roster player, replacing the placeholder that currently just
copies the flat season-long share into every zone (see
build_2026_rosters_v_0_1_0.py's splits construction).

Pulls 2023-2025 real PBP (same window as the trench donor-matching), computes
each player's real per-season zone share via rolling_stats_v_0_1_0's existing
zone-split machinery (the same functions the in-season weekly refresh uses),
pools the up-to-3 real season values weighted by real sample size, then
applies empirical-Bayes shrinkage toward the player's own flat season-long
share -- goalline sample sizes per player are often just a handful of plays
across 3 whole seasons, so an unshrunk pooled value would be exactly the kind
of small-sample noise found and fixed for Willis's under_pressure_cpoe
earlier this session (2026-08-12). Redzone gets lighter shrinkage than
goalline since it naturally has more volume.

Only writes splits.redzone/goalline.target_share and .carry_share -- leaves
splits.primary, every other split field, and all top-level (flat) fields
completely untouched, so this doesn't disturb any of Cam's hand-tuned
preseason_overrides_2026.csv values.

Usage: python build_redzone_goalline_shares_v_0_1_0.py <target_year>
  e.g. `python build_redzone_goalline_shares_v_0_1_0.py 2026`
"""
import sys
import os
import glob
import json
import nfl_data_py as nfl

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import (
    build_team_week_zone_totals, build_player_zone_game_log, ZONES, MIN_ZONE_SAMPLES,
)

ROSTERS_DIR = "data/current_rosters"
HISTORY_SEASONS = [2023, 2024, 2025]
REGULAR_SEASON_MAX_WEEK = 18
SHRINKAGE_K = {"redzone": 10, "goalline": 5}
FIELDS = ["target_share", "carry_share"]


def season_zone_shares(pbp_z, zone_totals, player_id, pos):
    """One season's real zone target_share/carry_share + real sample count
    per field/zone, via the same per-week log the in-season refresh builds
    (summed back into a single season-long value+count here).

    pbp_z: that season's PBP with a 'zone' column already attached (see
    build() -- computed once per season, not once per player, since neither
    it nor zone_totals varies by player and both were previously being
    rebuilt from scratch on every single call, ~500 players x 3 seasons of
    fully redundant work)."""
    log = build_player_zone_game_log(pbp_z, player_id, pos, team_zone_totals=zone_totals)

    out = {}
    for field in FIELDS:
        id_col = "receiver_player_id" if field == "target_share" else "rusher_player_id"
        play_type = "pass" if field == "target_share" else "run"
        for zone in ("redzone", "goalline"):
            gv = log[field][zone]
            weekly_vals = [v for v in gv.values() if v is not None]
            n_plays = int((pbp_z[(pbp_z["play_type"] == play_type) & (pbp_z["zone"] == zone) & (pbp_z[id_col] == player_id)]).shape[0])
            if not weekly_vals or n_plays < MIN_ZONE_SAMPLES:
                out[(field, zone)] = (None, n_plays)
                continue
            # Season value = plays-weighted mean of the per-week shares
            # already computed (each week's share already normalized by
            # that week's real team zone volume).
            season_val = sum(weekly_vals) / len(weekly_vals)
            out[(field, zone)] = (season_val, n_plays)
    return out


def build(target_year):
    print(f"Pulling {HISTORY_SEASONS} PBP...")
    pbp_by_season = {}
    zone_totals_by_season = {}
    for s in HISTORY_SEASONS:
        pbp = nfl.import_pbp_data([s])
        pbp = pbp[pbp["play_type"].isin(["pass", "run"])]
        pbp = pbp[pbp["week"] <= REGULAR_SEASON_MAX_WEEK]
        pbp = pbp.copy()
        pbp["zone"] = pbp["yardline_100"].apply(lambda yd: "goalline" if yd <= 5 else ("redzone" if yd <= 20 else "primary"))
        pbp_by_season[s] = pbp
        # Computed once per season here -- does not vary by player, previously
        # (before this fix) recomputed from scratch on every single player.
        zone_totals_by_season[s] = build_team_week_zone_totals(pbp)
        print(f"  {s}: {len(pbp)} plays loaded, zone totals built.")

    total_players = 0
    total_updated_fields = 0

    for path in sorted(glob.glob(os.path.join(ROSTERS_DIR, f"*_traits_{target_year}.json"))):
        data = json.load(open(path))
        changed = False
        for name, traits in data["traits"].items():
            if traits.get("pos") not in ("RB", "WR", "TE"):
                continue
            player_id = traits.get("player_id")
            if not player_id:
                continue
            total_players += 1

            pooled = {}  # (field, zone) -> (weighted_sum, total_n)
            for season, pbp in pbp_by_season.items():
                season_out = season_zone_shares(pbp, zone_totals_by_season[season], player_id, traits["pos"])
                for key, (val, n) in season_out.items():
                    if val is None:
                        continue
                    ws, tn = pooled.get(key, (0.0, 0))
                    pooled[key] = (ws + val * n, tn + n)

            splits = traits.setdefault("splits", {})
            for zone in ("redzone", "goalline"):
                zone_dict = splits.setdefault(zone, {})
                for field in FIELDS:
                    flat_val = traits.get(field, 0.0)
                    ws, n = pooled.get((field, zone), (0.0, 0))
                    if n == 0:
                        continue  # no real signal at all -- leave existing placeholder alone
                    pooled_share = ws / n
                    k = SHRINKAGE_K[zone]
                    shrunk = (n * pooled_share + k * flat_val) / (n + k)
                    old = zone_dict.get(field)
                    if old is None or abs(old - shrunk) > 1e-6:
                        zone_dict[field] = round(float(shrunk), 4)
                        changed = True
                        total_updated_fields += 1

        if changed:
            with open(path, "w") as f:
                json.dump(data, f, indent=4)

    print(f"Processed {total_players} RB/WR/TE across the league, updated {total_updated_fields} zone-share fields.")


def main():
    if len(sys.argv) != 2:
        print("Usage: python build_redzone_goalline_shares_v_0_1_0.py <target_year>")
        sys.exit(1)
    build(int(sys.argv[1]))


if __name__ == "__main__":
    main()
