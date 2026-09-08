"""Builds data/dna/zone_usage_overrides_2026.csv -- the redzone (yardline_100
6-20) / inside-the-5 (yardline_100 <= 5) companion to
data/dna/preseason_overrides_2026.csv, covering target_share and carry_share
only.

This is the CSV the AGENTS.md §0 gap note calls out: "red-zone/inside-the-5
usage-and-efficiency splits are not modeled anywhere -- season-long shares
stand in near the goal line too." This script produces the starting document
for that work -- it does NOT wire zone shares into game_engine.py or any
blend pipeline; that integration decision is still open (see the notes.md
this script also expects alongside its output).

Three kinds of columns per zone (redzone "rz_", goalline "five_"):
  - rz_target_share / rz_carry_share / five_target_share / five_carry_share:
    the 2026 hand-editable projection. Prepopulated with the player's flat
    (whole-game) target_share/carry_share from preseason_overrides_2026.csv
    as a placeholder -- nothing zone-specific has been projected yet.
  - median_rz_target_share / median_rz_carry_share / median_five_target_share
    / median_five_carry_share: the median of that player's 2024 and 2025
    real shares in that zone (median of whichever of the two years actually
    has data -- a season with zero real zone plays is excluded, not treated
    as a 0% share). Sits directly right of the placeholder columns as a
    quick two-year-at-a-glance reference while hand-editing them.
  - hist_2024_*/hist_2025_*: real zone usage pulled from actual PBP for
    that season (share = player's zone targets or carries / their team's
    zone total that season -- same definition classify_zone()/
    build_player_zone_game_log() in rolling_stats_v_0_1_0.py already use for
    the live per-week rolling pipeline, so this reference data and any
    future automated zone-split pipeline stay comparable). A raw count
    column rides alongside each share so small samples (goalline volume is
    often single digits for a season) are visible, not hidden by a rounded
    percentage.

Zone definitions match rolling_stats_v_0_1_0.GOALLINE_YARDLINE/
REDZONE_YARDLINE exactly: goalline = yardline_100 <= 5, redzone = 6-20
(mutually exclusive of goalline, NOT the traditional inclusive-of-goalline
"inside the 20" stat some other sources report -- see the notes.md).

If a player played for more than one team within a season (trade), the
season share is computed against whichever team they saw the most zone
volume with that season (mode of posteam across their zone plays) -- same
approach build_player_game_log() already uses for the flat (non-zone) case.

Usage: python export_zone_usage_overrides_v_0_1_0.py
"""
import csv
import glob
import json
import os
import statistics
import sys

import nfl_data_py as nfl

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import classify_zone

DNA_DIR = "data/dna"
ROSTERS_DIR = "data/current_rosters"
PRESEASON_CSV = os.path.join(DNA_DIR, "preseason_overrides_2026.csv")
OUT_CSV = os.path.join(DNA_DIR, "zone_usage_overrides_2026.csv")
HIST_SEASONS = [2024, 2025]
POS_ORDER = {"QB": 0, "RB": 1, "WR": 2, "TE": 3}


def load_preseason_rows():
    """player_name -> {team, pos, target_share, carry_share} from the
    already-hand-tuned flat CSV -- the placeholder source for this file's
    2026 columns."""
    rows = {}
    with open(PRESEASON_CSV, newline="") as f:
        for row in csv.DictReader(f):
            rows[(row["player_name"], row["team"])] = {
                "pos": row["pos"],
                "target_share": row["target_share"],
                "carry_share": row["carry_share"],
            }
    return rows


def load_player_ids():
    """(player_name, team) -> gsis player_id, from the 2026 roster files --
    the bridge to PBP's *_player_id columns (PBP's own name columns are
    short-form, e.g. "J.Chase", not usable against preseason_overrides'
    full display names)."""
    ids = {}
    for path in sorted(glob.glob(os.path.join(ROSTERS_DIR, "*_traits_2026.json"))):
        data = json.load(open(path))
        team = data["team"]
        for name, traits in data["traits"].items():
            pid = traits.get("player_id")
            if pid:
                ids[(name, team)] = pid
    return ids


def zone_usage_for_season(season):
    """Returns:
      targets[(player_id, zone)] -> count, carries[(player_id, zone)] -> count
      team_targets[(team, zone)] -> count, team_carries[(team, zone)] -> count
      player_team[player_id] -> mode posteam across this player's zone plays
    Pulled from real PBP -- classify_zone() matches the live pipeline's
    goalline/redzone/primary split exactly."""
    print(f"  Pulling {season} PBP...")
    pbp = nfl.import_pbp_data([season])
    pbp = pbp[pbp["play_type"].isin(["pass", "run"])].copy()
    pbp["zone"] = pbp["yardline_100"].apply(classify_zone)
    pbp = pbp[pbp["zone"].isin(["redzone", "goalline"])]

    pbp_pass = pbp[pbp["play_type"] == "pass"]
    pbp_run = pbp[pbp["play_type"] == "run"]

    team_targets = pbp_pass.groupby(["posteam", "zone"]).size().to_dict()
    team_carries = pbp_run.groupby(["posteam", "zone"]).size().to_dict()

    targets = pbp_pass.dropna(subset=["receiver_player_id"]).groupby(
        ["receiver_player_id", "zone"]).size().to_dict()
    carries = pbp_run.dropna(subset=["rusher_player_id"]).groupby(
        ["rusher_player_id", "zone"]).size().to_dict()

    player_team = {}
    for pid, sub in pbp_pass.dropna(subset=["receiver_player_id"]).groupby("receiver_player_id"):
        player_team.setdefault(pid, []).extend(sub["posteam"].tolist())
    for pid, sub in pbp_run.dropna(subset=["rusher_player_id"]).groupby("rusher_player_id"):
        player_team.setdefault(pid, []).extend(sub["posteam"].tolist())
    player_team = {pid: max(set(teams), key=teams.count) for pid, teams in player_team.items()}

    return targets, carries, team_targets, team_carries, player_team


def build():
    preseason = load_preseason_rows()
    player_ids = load_player_ids()

    hist = {}
    for season in HIST_SEASONS:
        hist[season] = zone_usage_for_season(season)

    fieldnames = ["player_name", "team", "pos", "rz_target_share", "rz_carry_share",
                  "five_target_share", "five_carry_share",
                  "median_rz_target_share", "median_rz_carry_share",
                  "median_five_target_share", "median_five_carry_share"]
    for season in HIST_SEASONS:
        for zone_label in ("rz", "five"):
            fieldnames += [f"hist_{season}_{zone_label}_target_share",
                           f"hist_{season}_{zone_label}_targets",
                           f"hist_{season}_{zone_label}_carry_share",
                           f"hist_{season}_{zone_label}_carries"]

    rows = []
    for (name, team), info in preseason.items():
        row = {
            "player_name": name, "team": team, "pos": info["pos"],
            "rz_target_share": info["target_share"], "rz_carry_share": info["carry_share"],
            "five_target_share": info["target_share"], "five_carry_share": info["carry_share"],
        }
        pid = player_ids.get((name, team))
        # zone_label -> stat -> list of real (non-blank) yearly share values,
        # for the two-year median columns.
        share_series = {zl: {"target_share": [], "carry_share": []} for zl in ("rz", "five")}
        for season in HIST_SEASONS:
            targets, carries, team_targets, team_carries, player_team = hist[season]
            for zone_label, zone in (("rz", "redzone"), ("five", "goalline")):
                tgt_n = targets.get((pid, zone), 0) if pid else 0
                car_n = carries.get((pid, zone), 0) if pid else 0
                hist_team = player_team.get(pid) if pid else None
                team_tgt_total = team_targets.get((hist_team, zone), 0) if hist_team else 0
                team_car_total = team_carries.get((hist_team, zone), 0) if hist_team else 0
                tgt_share = tgt_n / team_tgt_total if team_tgt_total else None
                car_share = car_n / team_car_total if team_car_total else None
                row[f"hist_{season}_{zone_label}_target_share"] = round(tgt_share, 4) if tgt_share is not None else ""
                row[f"hist_{season}_{zone_label}_targets"] = tgt_n
                row[f"hist_{season}_{zone_label}_carry_share"] = round(car_share, 4) if car_share is not None else ""
                row[f"hist_{season}_{zone_label}_carries"] = car_n
                if tgt_share is not None:
                    share_series[zone_label]["target_share"].append(tgt_share)
                if car_share is not None:
                    share_series[zone_label]["carry_share"].append(car_share)
        for zone_label in ("rz", "five"):
            for stat in ("target_share", "carry_share"):
                values = share_series[zone_label][stat]
                row[f"median_{zone_label}_{stat}"] = round(statistics.median(values), 4) if values else ""
        rows.append(row)

    rows.sort(key=lambda r: (r["team"], POS_ORDER.get(r["pos"], 4), r["player_name"]))

    with open(OUT_CSV, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f"Wrote {len(rows)} rows to {OUT_CSV}.")


if __name__ == "__main__":
    build()
