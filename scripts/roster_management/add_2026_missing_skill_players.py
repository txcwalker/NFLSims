"""Adds skill-position players missing from the 2026 preseason projection
pipeline: (1) veterans who changed teams via free agency/trade after
data/current_rosters/{TEAM}_traits_2026.json was last built, and (2) every
2026-draft-class rookie who was still on the ramp-up curve track
(data/dna/rookie_projections_2026.json) instead of the flat preseason CSVs.

Context (2026-08-20): Cam flagged skill-position free-agent signings not
reflected anywhere in the 2026 pipeline -- Najee Harris (NYG), Stefon Diggs
(WAS), Deebo Samuel (SF), later Keenan Allen (IND) -- confirmed via
nfl_data_py that none of them appear in ANY current_rosters/*_traits_2026.json
team file (not even under their old team), and that
nfl.import_seasonal_rosters([2026]) doesn't have them under their new teams
either -- the live feed is lagging real-world 2026 free agency, so these
have to be hand-added from known career data rather than auto-detected.
Also asked to add "any skill position player drafted in 2026 not already
included" to the two flat CSVs -- turned out all 80 2026 draft picks ARE
already in current_rosters (0 missing), but 214 of them are on the separate
curve-based ramp track (rookie_projections_2026.json), which is why they
don't show up as rows in preseason_overrides_2026.csv/
zone_usage_overrides_2026.csv (both scripts that build those CSVs
deliberately skip curve-based rookies -- see
export_preseason_overrides_v_0_1_0.py's docstring). Cam confirmed (via
AskUserQuestion) he wants those 214 promoted to the flat track too.

NEW_SIGNINGS is designed to be extended over time as Cam surfaces more
signings this feed hasn't caught up to yet -- the script skips (doesn't
error on) any entry already present in its team's roster file, so it's safe
to add one new name and rerun rather than needing a separate one-off script
each time.

Two things this script does, in order:

  1. VETERAN SIGNINGS -- for each of the 3 (see NEW_SIGNINGS below), pulls
     real single-season PBP + NGS receiving stats for a specified base
     season (2025 by default; Cam explicitly chose 2024 for Najee Harris,
     since his 2025 was with a different team/limited role) using the same
     formulas build_full_name_dna.py already uses for career DNA, just
     restricted to one season instead of averaged across many. Reuses
     build_2026_rosters_v_0_1_0.enrich_player() to build the full roster
     entry so the schema/defaults exactly match every other player instead
     of a hand-rolled dict. target_share/carry_share are forced to 0 per
     Cam's explicit instruction (their share on a NEW team isn't knowable
     from old-team data) -- only the efficiency-style fields (catch_rate,
     ypc, yac_per_rec, adot, deep_target_rate, elusiveness,
     broken_tackle_rate, avg_separation_yds) come from real history.

  2. ROOKIE PROMOTION -- every current_rosters rookie with no
     preseason_projection block yet (all still using their build-time
     position-average defaults, already real "league average" numbers, just
     never frozen into a preseason_projection block) gets one added, via
     the exact same freeze-snapshot logic
     build_2026_rosters_v_0_1_0.enrich_player() already uses for veterans.
     Critically also REMOVES each promoted rookie from
     rookie_projections_2026.json -- refresh_weekly_dna_v_0_1_0.py's
     is_rookie check (`traits.get("rookie") and traits.get("_name") in
     rookie_projections`) keys off THAT file's membership, not the presence
     of a preseason_projection block, so leaving them in rookie_projections
     would silently keep routing them through the curve system all season
     regardless of the promotion.

After this script runs, re-run export_preseason_overrides_v_0_1_0.py 2026
and export_zone_usage_overrides_v_0_1_0.py to regenerate both flat CSVs
fresh from the now-updated roster files (both are safe/idempotent re-runs
per their own docstrings, and will pick up every new/promoted player as a
new row while preserving every already-hand-tuned value, since those are
already baked into the roster JSONs).

Usage: python add_2026_missing_skill_players.py
"""
import json
import os
import sys

import numpy as np
import pandas as pd
import nfl_data_py as nfl

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import PLAYER_RATE_FIELDS, PLAYER_NGS_FIELDS, ZONE_SPLIT_FIELDS, ZONES
from scripts.roster_management.build_2026_rosters_v_0_1_0 import enrich_player

DNA_DIR = "data/dna"
ROSTERS_DIR = "data/current_rosters"
ROOKIE_PROJECTIONS_PATH = os.path.join(DNA_DIR, "rookie_projections_2026.json")
TUNABLE_FIELDS = PLAYER_RATE_FIELDS + PLAYER_NGS_FIELDS

# name -> (team, pos, player_id, stat_seasons). stat_seasons are the real
# season(s) whose per-player stats become this signing's preseason_projection
# basis (efficiency fields only -- shares are forced to 0 regardless). A
# multi-season list is pooled (volume-weighted, not a plain year-by-year
# average -- see compute_pdna_for_seasons), matching the same weighting
# convention every other multi-season DNA build in this repo already uses.
NEW_SIGNINGS = {
    "Najee Harris": {"team": "NYG", "pos": "RB", "player_id": "00-0036893", "stat_seasons": [2024]},
    "Stefon Diggs": {"team": "WAS", "pos": "WR", "player_id": "00-0031588", "stat_seasons": [2025]},
    "Deebo Samuel": {"team": "SF", "pos": "WR", "player_id": "00-0035719", "stat_seasons": [2025]},
    # 2026-08-20 addition: Cam asked for both 2024 (CHI) and 2025 (LAC)
    # pooled, not just one year -- unlike the first 3 signings, both of
    # Allen's recent seasons were real, meaningful volume.
    "Keenan Allen": {"team": "IND", "pos": "WR", "player_id": "00-0030279", "stat_seasons": [2024, 2025]},
}
STAT_SEASONS = sorted({s for v in NEW_SIGNINGS.values() for s in v["stat_seasons"]})


def load_pbp_and_ngs():
    print(f"Pulling PBP {STAT_SEASONS} for the new-signing base seasons...")
    pbp = nfl.import_pbp_data(STAT_SEASONS)
    pbp_pass = pbp[pbp["play_type"] == "pass"].copy()
    pbp_run = pbp[pbp["play_type"] == "run"].copy()
    if "yards_after_catch" in pbp_pass.columns and "xyac_mean_yardage" in pbp_pass.columns:
        pbp_pass["yac_over_expected"] = pbp_pass["yards_after_catch"] - pbp_pass["xyac_mean_yardage"]
        pbp_pass["high_yac_play"] = (pbp_pass["yac_over_expected"] > 3.0).astype(int)

    print("Pulling NGS receiving...")
    ngs_recv = nfl.import_ngs_data("receiving", STAT_SEASONS)
    return pbp_pass, pbp_run, ngs_recv


def compute_pdna_for_seasons(pbp_pass, pbp_run, ngs_recv, player_id, seasons):
    """build_full_name_dna.py's per-player skill formulas, restricted to a
    specific set of seasons instead of a full 2021-2025 career. Pooling the
    raw plays across seasons (rather than averaging each season's rate then
    averaging those) naturally volume-weights the result -- a season with
    3x the targets of the other correctly counts 3x as much, same as every
    other multi-season DNA build in this repo."""
    p_pass = pbp_pass[(pbp_pass["receiver_player_id"] == player_id) & (pbp_pass["season"].isin(seasons))]
    p_run = pbp_run[(pbp_run["rusher_player_id"] == player_id) & (pbp_run["season"].isin(seasons))]

    total_targets, total_carries = len(p_pass), len(p_run)
    catch_rate = float(p_pass["complete_pass"].mean()) if total_targets else 0.0
    avg_depth = float(p_pass["air_yards"].mean()) if total_targets else 0.0
    complete_passes = p_pass[p_pass["complete_pass"] == 1]
    yac_per_rec = float(complete_passes["yards_after_catch"].mean()) if len(complete_passes) else 4.0
    ypc = float(p_run["yards_gained"].mean()) if total_carries else 4.0
    deep_target_rate = float((p_pass["air_yards"] >= 20).mean()) if total_targets else 0.0

    elusiveness, broken_tackle_rate = 0.0, 0.15
    if "yac_over_expected" in p_pass.columns:
        valid_yac = complete_passes.dropna(subset=["yards_after_catch", "xyac_mean_yardage"])
        if len(valid_yac) >= 10:
            elusiveness = float(valid_yac["yac_over_expected"].mean())
            broken_tackle_rate = float(valid_yac["high_yac_play"].mean())

    dna = {
        "catch_rate": round(catch_rate, 4),
        "avg_target_depth_yds": round(avg_depth, 3),
        "yac_per_reception": round(yac_per_rec, 3),
        "ypc": round(ypc, 3),
        "deep_target_rate": round(deep_target_rate, 4),
        "elusiveness": round(elusiveness, 4),
        "broken_tackle_rate": round(broken_tackle_rate, 4),
    }

    # NGS week==0 row is the season-aggregate row (standard nfl_data_py NGS
    # convention) -- more correct than averaging per-week rows unweighted.
    # Multi-season: weight each season's avg_separation by that season's
    # real target count here, same volume-weighting as everything else.
    ngs_rows = ngs_recv[
        (ngs_recv["player_gsis_id"] == player_id)
        & (ngs_recv["season"].isin(seasons))
        & (ngs_recv["week"] == 0)
    ]
    if len(ngs_rows):
        targets_by_season = p_pass.groupby("season").size()
        weights = ngs_rows["season"].map(targets_by_season).fillna(0)
        valid = ngs_rows["avg_separation"].notna() & (weights > 0)
        if valid.any():
            sep = np.average(ngs_rows.loc[valid, "avg_separation"], weights=weights[valid])
            dna["avg_separation_yds"] = round(float(sep), 3)

    print(f"    -> {total_targets} targets, {total_carries} carries in {seasons}: {dna}")
    return dna


def add_new_signings():
    pbp_pass, pbp_run, ngs_recv = load_pbp_and_ngs()

    for name, spec in NEW_SIGNINGS.items():
        path = os.path.join(ROSTERS_DIR, f"{spec['team']}_traits_2026.json")
        data = json.load(open(path))
        if name in data["traits"]:
            print(f"\n{name}: already present in {path}, skipping (this script is safe to rerun "
                  f"for newly-added NEW_SIGNINGS entries without redoing earlier ones).")
            continue

        print(f"\n{name} ({spec['pos']}, -> {spec['team']}, base season(s) {spec['stat_seasons']}):")
        p_dna = compute_pdna_for_seasons(pbp_pass, pbp_run, ngs_recv, spec["player_id"], spec["stat_seasons"])
        pos_key = spec["pos"].lower()
        skill_dna = {pos_key: {name: p_dna}} if spec["pos"] != "QB" else {"qb": {name: p_dna}}

        enriched = enrich_player(
            name, spec["pos"], target_share=0.0, carry_share=0.0,
            skill_dna=skill_dna, is_rookie=False, draft_capital=None,
            player_id=spec["player_id"],
        )

        data["traits"][name] = enriched
        with open(path, "w") as f:
            json.dump(data, f, indent=4)
        print(f"  Added to {path}.")


def promote_curve_rookies():
    rookie_projections = json.load(open(ROOKIE_PROJECTIONS_PATH))
    promoted_names = []

    for path in sorted(__import__("glob").glob(os.path.join(ROSTERS_DIR, "*_traits_2026.json"))):
        data = json.load(open(path))
        changed = False
        for name, traits in data["traits"].items():
            if not traits.get("rookie") or "preseason_projection" in traits:
                continue
            traits["preseason_projection"] = {
                f: traits[f] for f in TUNABLE_FIELDS if f in traits
            }
            splits = traits.get("splits", {})
            traits["preseason_projection"]["splits"] = {
                zone: {f: splits.get(zone, {})[f] for f in ZONE_SPLIT_FIELDS if f in splits.get(zone, {})}
                for zone in ZONES
            }
            changed = True
            promoted_names.append(name)
            if name in rookie_projections:
                del rookie_projections[name]
        if changed:
            with open(path, "w") as f:
                json.dump(data, f, indent=4)

    with open(ROOKIE_PROJECTIONS_PATH, "w") as f:
        json.dump(rookie_projections, f, indent=4)

    print(f"\nPromoted {len(promoted_names)} curve-based rookies to the flat/no-ramp track, "
          f"removed them from {ROOKIE_PROJECTIONS_PATH}.")


def main():
    print("=== Adding missing 2026 skill-position veterans ===")
    add_new_signings()
    print("\n=== Promoting curve-based 2026 rookies to the flat track ===")
    promote_curve_rookies()


if __name__ == "__main__":
    main()
