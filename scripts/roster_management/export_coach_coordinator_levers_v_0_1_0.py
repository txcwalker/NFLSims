"""Builds data/dna/coach_coordinator_levers_2026.csv -- the hand-editable
2026 projection sheet for the four coach/coordinator "levers" Cam wants
wired into game_engine.py eventually: PROE, deep_shot_rate, no_huddle_rate,
and pace (seconds between plays). Zone-split (primary/redzone/goalline),
same convention as everywhere else in this repo (yardline_100 <= 5 =
goalline, 6-20 = redzone, mutually exclusive of goalline -- see
classify_zone()).

Same philosophy as data/dna/preseason_overrides_2026.csv: each zone x metric
cell IS the 2026 hand-editable field, pre-populated with a real career-
weighted historical value as a starting point (not a separate proj/hist pair
of columns). As the 2026 season plays out, these get refreshed with the same
taper/steady-state blend formula src/data_pipeline/dna_blender_v_0_1_0.py
already applies to player DNA -- not wired up yet, this script only builds
the starting document.

One row per TEAM, coach = that team's real 2026 offensive play-caller (same
name data/dna/team_to_coach_2026.json + coach_dna.json already resolve to --
see AGENTS.md section 0 for the full history of that resolution, including
the known first-time-2026-HC gaps).

IMPORTANT — "coach and coordinator" is really just "coach": nflfastR's PBP
only ever credits the actual HC of record (home_coach/away_coach columns),
never a separate OC/DC identity, no matter how much of the offense was
really theirs. This script reuses the one alias already agreed on
(merge_2026_coach_placeholders.py): Klint Kubiak (LV) -> Mike Macdonald's
real 2024-2025 Seahawks-OC-tenure numbers, since Macdonald's own HC entry
happens to span exactly those two seasons. For the other 4 known 2026
first-timers with zero real offensive-playcaller history (Jesse Minter/BAL,
Jeff Hafley/MIA, Joe Brady/BUF, Todd Monken/CLE), every field is left BLANK
rather than filled with a league-average placeholder -- Cam's explicit call
this session, so he can hand-fill them himself. Any other team that happens
to have zero real history under its 2026 coach's own name is also left
blank, same treatment.

Metrics (all computed on real 2015-2025 REG-season PBP, matching coach_dna.
json's window):
  proe              - Pass Rate Over Expected. mean(pass_oe) from nflfastR's
                       own expected-pass model, over the play-selection
                       universe (see below). Computed fresh here -- the
                       existing coach_dna.json "proe" field is flat
                       (whole-game), not zone-split.
  deep_shot_rate    - share of PASS plays with air_yards >= 20. Same
                       definition as coach_dna.json's existing flat field,
                       now zone-split. CAVEAT: goalline deep_shot_rate will
                       be structurally near-zero for every coach -- the
                       field is only 5 yards deep there, so a 20+ air-yard
                       throw is essentially impossible by construction. Kept
                       for definitional consistency across zones rather than
                       inventing a different goalline-specific stat.
  no_huddle_rate    - share of the play-selection universe with no_huddle==1.
  sec_per_play      - average seconds between snaps ("pace"), reusing the
                       exact running-clock methodology from
                       scripts/eda/analyze_clock_pace_grid.py (previous play
                       not incomplete/OOB/timeout/penalty, no possession
                       change, elapsed in [0,60]s, regulation quarters only)
                       -- so these numbers are directly comparable to
                       src/nfl_sim/models/clock_pace_v_0_1_0/pace_pools.json.
                       Zone is the CURRENT play's field position. Sample
                       size for this metric is smaller than n_plays (it's
                       running-clock snaps only, a subset) -- no separate N
                       column is printed for it, budget for it being noisier
                       than the other three metrics at a given n_plays.

Play-selection universe (proe/no_huddle_rate, and the denominator for
n_plays): play_type in [pass, run], qb_spike==0, qb_kneel==0,
aborted_play==0 -- identical filter to
src/nfl_sim/models/play_selection_v_0_1_0/train.py, so n_plays here is
directly comparable to that model's training population.

Usage: python export_coach_coordinator_levers_v_0_1_0.py
"""
import csv
import json
import os
import sys

import numpy as np
import pandas as pd
import nfl_data_py as nfl

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import classify_zone

DNA_DIR = "data/dna"
TEAM_TO_COACH_PATH = os.path.join(DNA_DIR, "team_to_coach_2026.json")
OUT_CSV = os.path.join(DNA_DIR, "coach_coordinator_levers_2026.csv")
SEASONS = list(range(2015, 2026))
ZONES = ["primary", "redzone", "goalline"]

ALIAS_SOURCE = "Mike Macdonald"
ALIAS_TARGET = "Klint Kubiak"
ALIAS_NOTE = (
    "ALIASED from Mike Macdonald's real 2024-2025 Seahawks-OC-tenure numbers "
    "(same alias as coach_dna.json's flat fields -- see "
    "merge_2026_coach_placeholders.py)."
)
NO_HISTORY_NOTE = (
    "NO REAL HISTORY under this name in 2015-2025 PBP -- left blank for "
    "Cam to hand-fill, per 2026-08-17 session (not auto-filled with a "
    "league-average placeholder)."
)

PBP_COLS = [
    "game_id", "play_id", "season", "season_type", "qtr", "game_half",
    "quarter_seconds_remaining", "game_seconds_remaining",
    "posteam", "home_team", "away_team", "home_coach", "away_coach",
    "play_type", "qb_spike", "qb_kneel", "aborted_play",
    "pass_attempt", "rush_attempt", "xpass", "pass_oe", "air_yards", "no_huddle",
    "yardline_100", "incomplete_pass", "out_of_bounds", "penalty", "timeout",
]


def load_team_to_coach():
    with open(TEAM_TO_COACH_PATH) as f:
        return json.load(f)


def load_pbp():
    print(f"Pulling PBP {SEASONS[0]}-{SEASONS[-1]} (this takes a few minutes)...")
    df = nfl.import_pbp_data(SEASONS, columns=PBP_COLS, downcast=True)
    df = df[df["season_type"] == "REG"].copy()
    df["off_coach"] = np.where(df["posteam"] == df["home_team"], df["home_coach"], df["away_coach"])
    df["zone"] = df["yardline_100"].apply(classify_zone)
    print(f"  {len(df):,} REG-season rows loaded.")
    return df


def build_play_selection_universe(df):
    """play_type in [pass, run], no spike/kneel/aborted -- matches
    play_selection_v_0_1_0/train.py's filter exactly."""
    u = df[df["play_type"].isin(["pass", "run"])].copy()
    u = u[(u["qb_spike"] == 0) & (u["qb_kneel"] == 0) & (u["aborted_play"] == 0)]
    return u


def compute_proe_no_huddle_n(universe):
    """Returns {(coach, zone): {'n_plays', 'proe', 'no_huddle_rate'}}."""
    out = {}
    grouped = universe.groupby(["off_coach", "zone"])
    for (coach, zone), sub in grouped:
        n = len(sub)
        proe_vals = sub["pass_oe"].dropna()
        out[(coach, zone)] = {
            "n_plays": n,
            "proe": round(float(proe_vals.mean()), 4) if len(proe_vals) else None,
            "no_huddle_rate": round(float((sub["no_huddle"] == 1).mean()), 4) if n else None,
        }
    return out


def compute_deep_shot_rate(universe):
    """Pass-only subset of the universe -> {(coach, zone): deep_shot_rate}."""
    passes = universe[universe["play_type"] == "pass"].dropna(subset=["air_yards"])
    out = {}
    for (coach, zone), sub in passes.groupby(["off_coach", "zone"]):
        if len(sub):
            out[(coach, zone)] = round(float((sub["air_yards"] >= 20).mean()), 4)
    return out


def compute_pace(df):
    """Running-clock seconds-between-snaps, reusing
    analyze_clock_pace_grid.py's exact methodology, keyed by the CURRENT
    play's off_coach/zone."""
    d = df.sort_values(["game_id", "play_id"]).reset_index(drop=True)

    d["prev_game_id"] = d["game_id"].shift(1)
    d["prev_game_half"] = d["game_half"].shift(1)
    d["prev_game_seconds"] = d["game_seconds_remaining"].shift(1)
    d["game_clock_elapsed"] = np.where(
        (d["game_id"] == d["prev_game_id"]) & (d["game_half"] == d["prev_game_half"]),
        d["prev_game_seconds"] - d["game_seconds_remaining"],
        np.nan,
    )

    d["prev_play_type"] = d["play_type"].shift(1)
    d["prev_incomplete_pass"] = d["incomplete_pass"].shift(1).fillna(0)
    d["prev_out_of_bounds"] = d["out_of_bounds"].shift(1).fillna(0)
    d["prev_penalty"] = d["penalty"].shift(1).fillna(0)
    d["prev_timeout"] = d["timeout"].shift(1).fillna(0)
    d["prev_posteam"] = d["posteam"].shift(1)

    off_types = ["pass", "run", "no_play", "qb_kneel", "qb_spike"]
    d["prev_clock_stopped"] = (
        (d["prev_incomplete_pass"] == 1)
        | (d["prev_out_of_bounds"] == 1)
        | (d["prev_timeout"] == 1)
        | (d["prev_penalty"] == 1)
    )
    d["possession_change"] = (
        (d["game_id"] == d["prev_game_id"])
        & (d["posteam"] != d["prev_posteam"])
        & d["posteam"].notna()
        & d["prev_posteam"].notna()
    )

    valid = d[
        d["game_clock_elapsed"].notna()
        & (d["game_clock_elapsed"] >= 0)
        & (d["game_clock_elapsed"] <= 60)
        & d["prev_play_type"].isin(off_types)
        & d["qtr"].isin([1, 2, 3, 4])
    ].copy()

    running_clock = valid[~valid["prev_clock_stopped"] & ~valid["possession_change"]]

    out = {}
    for (coach, zone), sub in running_clock.groupby(["off_coach", "zone"]):
        if len(sub):
            out[(coach, zone)] = round(float(sub["game_clock_elapsed"].mean()), 2)
    return out


def build():
    team_to_coach = load_team_to_coach()
    df = load_pbp()
    universe = build_play_selection_universe(df)

    proe_nh = compute_proe_no_huddle_n(universe)
    deep_shot = compute_deep_shot_rate(universe)
    pace = compute_pace(df)

    def alias_if_needed(coach):
        has_data = any((coach, z) in proe_nh for z in ZONES)
        if not has_data and coach == ALIAS_TARGET:
            return ALIAS_SOURCE, ALIAS_NOTE
        return coach, None

    fieldnames = ["team", "coach_name", "note"]
    fieldnames += [f"n_plays_{z}" for z in ZONES]
    fieldnames += [f"proe_{z}" for z in ZONES]
    fieldnames += [f"deep_shot_rate_{z}" for z in ZONES]
    fieldnames += [f"no_huddle_rate_{z}" for z in ZONES]
    fieldnames += [f"sec_per_play_{z}" for z in ZONES]

    rows = []
    for team in sorted(team_to_coach):
        coach_display = team_to_coach[team]
        lookup_coach, note = alias_if_needed(coach_display)
        has_data = any((lookup_coach, z) in proe_nh for z in ZONES)
        if not has_data:
            note = NO_HISTORY_NOTE

        row = {"team": team, "coach_name": coach_display, "note": note or ""}
        for z in ZONES:
            stats = proe_nh.get((lookup_coach, z), {})
            row[f"n_plays_{z}"] = stats.get("n_plays", "")
            row[f"proe_{z}"] = stats.get("proe", "") if stats.get("proe") is not None else ""
            row[f"no_huddle_rate_{z}"] = stats.get("no_huddle_rate", "") if stats.get("no_huddle_rate") is not None else ""
            row[f"deep_shot_rate_{z}"] = deep_shot.get((lookup_coach, z), "")
            row[f"sec_per_play_{z}"] = pace.get((lookup_coach, z), "")
        rows.append(row)

    with open(OUT_CSV, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    blank_teams = [r["team"] for r in rows if r["note"] == NO_HISTORY_NOTE]
    print(f"\nWrote {len(rows)} rows to {OUT_CSV}.")
    if blank_teams:
        print(f"{len(blank_teams)} teams with no real history, left blank: {blank_teams}")


if __name__ == "__main__":
    build()
