"""Builds real team+season-filtered proxies for the 2026 coaches whose own
name has zero offensive-playcalling history in nflfastR's PBP, replacing the
league-average placeholders merge_2026_coach_placeholders.py set as an
interim measure.

Context (2026-08-20): nflfastR's PBP only ever credits the actual HC of
record (home_coach/away_coach), never a separate OC/DC identity -- see
export_coach_coordinator_levers_v_0_1_0.py's docstring and AGENTS.md sec 0
for the full history. That script and merge_2026_coach_placeholders.py both
left 4 of the 5 known-gap 2026 coaches (Minter/BAL, Brady/BUF, Monken/CLE,
plus Hafley/MIA) as either blank or a full league-average placeholder,
pending "a season-filtered rebuild of [team]'s PBP specifically -- not done
yet." This script IS that rebuild, for the 4 where Cam supplied real
coaching-history facts (2026-08-20 session) -- Hafley/MIA is untouched,
no history supplied for him yet:

  - Mike LaFleur (ARI)  -- real 2023-2025 Rams OC under Sean McVay (also
    Jets OC 2021-2022, not used here -- Cam chose the more recent/larger
    Rams sample). Proxy built from LA/2023,2024,2025 play-by-play.
  - Jesse Minter (BAL)  -- defensive background; BAL's real 2026 offensive
    play-caller is OC Declan Doyle, who was Bears OC in 2025. Proxy built
    from CHI/2025 (single season -- thinnest sample of the five).
  - Joe Brady (BUF)     -- real 2023-2025 Bills OC (interim from Nov 2023,
    whole seasons used per the plan already flagged in
    merge_2026_coach_placeholders.py; also Panthers OC 2020-2021, not used).
    Proxy built from BUF/2023,2024,2025.
  - Todd Monken (CLE)   -- real 2023-2025 Ravens OC (also Bucs OC
    2016-2018, not used). Proxy built from BAL/2023,2024,2025.
  - Klint Kubiak (LV)   -- CORRECTS the existing alias. The prior alias
    (merge_2026_coach_placeholders.py) borrowed Mike Macdonald's Seahawks HC
    entry spanning 2024-2025, reasoning seasons_observed=2 matched Kubiak's
    OC tenure there -- but Kubiak was actually Saints OC in 2024 and only
    moved to Seattle in 2025. Proxy rebuilt from the real combination:
    NO/2024 + SEA/2025.

Two downstream files get updated, each using ITS OWN file's existing
methodology exactly (so the new rows are computed identically to every
other row already in each file, not some new ad-hoc formula):

  1. data/dna/coach_dna.json -- flat per-coach fields, same formulas as
     R/scripts/build_dna_registry_v_0_1_0.R's build_coach_dna(): pass plays
     only (play_type=="pass", qb_spike/aborted_play excluded), REG+POST (no
     season_type filter, matching that script literally), league_mean_ay
     computed fresh per season from the same pulled seasons (sufficient
     since none of these 5 proxies reach outside 2023-2025). "proe" is
     mean(pass_oe) over the play-selection universe (pass+run, no
     spike/kneel/aborted) -- same definition/scale as every other coach's
     "proe" field, which is itself a separate historical merge
     (merge_coach_proe.py) on the same nflfastR pass_oe basis.
  2. data/dna/coach_coordinator_levers_2026.csv -- zone-split (primary/
     redzone/goalline) PROE, deep_shot_rate, no_huddle_rate, sec_per_play,
     same formulas and same play-selection universe as
     export_coach_coordinator_levers_v_0_1_0.py, REG season only.

Only pulls seasons 2023-2025 (not the full 2015-2025 window) since none of
the five proxies need anything outside that range -- keeps the PBP pull to
~3 seasons instead of 11.

Safe to rerun; each run recomputes both outputs fresh from PBP rather than
patching prior output.

Usage: python build_2026_ooc_proxies.py
"""
import json
import os
import sys

import numpy as np
import pandas as pd
import nfl_data_py as nfl

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import classify_zone

DNA_DIR = "data/dna"
COACH_DNA_PATH = f"{DNA_DIR}/coach_dna.json"
LEVERS_CSV_PATH = f"{DNA_DIR}/coach_coordinator_levers_2026.csv"
SEASONS = [2023, 2024, 2025]
ZONES = ["primary", "redzone", "goalline"]
COACH_MIN_PLAYS = 100
DEEP_THRESH = 20
SCREEN_THRESH = 0

# proxy_name -> (team, [(team, season), ...], note)
PROXIES = {
    "Mike LaFleur": {
        "spans": [("LA", 2023), ("LA", 2024), ("LA", 2025)],
        "team": "ARI",
        "note": (
            "PROXY: real 2023-2025 Rams OC under Sean McVay (also Jets OC "
            "2021-2022, not used -- more recent/larger Rams sample chosen "
            "instead). nflfastR only credits the HC of record, so this is "
            "built from LA/2023-2025 play-by-play filtered to those seasons, "
            "not McVay's full multi-year HC entry. Added 2026-08-20."
        ),
    },
    "Jesse Minter": {
        "spans": [("CHI", 2025)],
        "team": "BAL",
        "note": (
            "PROXY: Minter is a defensive coach -- BAL's real 2026 offensive "
            "play-caller is OC Declan Doyle, who was Bears OC in 2025. Built "
            "from CHI/2025 play-by-play (single season, thinnest sample of "
            "the 2026 proxy coaches). Replaces the prior full league-average "
            "placeholder. Added 2026-08-20."
        ),
    },
    "Joe Brady": {
        "spans": [("BUF", 2023), ("BUF", 2024), ("BUF", 2025)],
        "team": "BUF",
        "note": (
            "PROXY: real 2023-2025 Bills OC (interim from Nov 2023; whole "
            "seasons used, not week-filtered to his actual interim start). "
            "Also Panthers OC 2020-2021, not used -- more recent Bills "
            "sample chosen instead. Built from BUF/2023-2025 play-by-play, "
            "not Sean McDermott's full 9-season HC entry. Replaces the prior "
            "full league-average placeholder. Added 2026-08-20."
        ),
    },
    "Todd Monken": {
        "spans": [("BAL", 2023), ("BAL", 2024), ("BAL", 2025)],
        "team": "CLE",
        "note": (
            "PROXY: real 2023-2025 Ravens OC (also Bucs OC 2016-2018, not "
            "used). Built from BAL/2023-2025 play-by-play, not John "
            "Harbaugh's full 11-season HC entry. Replaces the prior full "
            "league-average placeholder. Added 2026-08-20."
        ),
    },
    "Klint Kubiak": {
        "spans": [("NO", 2024), ("SEA", 2025)],
        "team": "LV",
        "note": (
            "PROXY, CORRECTED 2026-08-20: real tenure is Saints OC 2024 + "
            "Seahawks OC 2025, NOT two full seasons under Macdonald in "
            "Seattle -- the prior alias to Mike Macdonald's 2024-2025 "
            "Seahawks HC entry wrongly assumed Kubiak was in Seattle for "
            "both years. Rebuilt from the real combination: NO/2024 + "
            "SEA/2025 play-by-play."
        ),
    },
}

PBP_COLS = [
    "game_id", "play_id", "season", "season_type", "qtr", "game_half",
    "quarter_seconds_remaining", "game_seconds_remaining",
    "posteam", "home_team", "away_team", "home_coach", "away_coach",
    "play_type", "play_type_nfl", "qb_spike", "qb_kneel", "aborted_play",
    "pass_attempt", "rush_attempt", "xpass", "pass_oe", "air_yards",
    "no_huddle", "yardline_100", "incomplete_pass", "out_of_bounds",
    "penalty", "timeout", "score_differential",
]


def load_pbp():
    print(f"Pulling PBP {SEASONS[0]}-{SEASONS[-1]} (all 32 teams, needed for league baselines)...")
    df = nfl.import_pbp_data(SEASONS, columns=PBP_COLS, downcast=True)
    df["zone"] = df["yardline_100"].apply(classify_zone)
    print(f"  {len(df):,} rows loaded ({len(df[df['season_type'] == 'REG']):,} REG).")
    return df


def spans_mask(df, spans):
    m = pd.Series(False, index=df.index)
    for team, season in spans:
        m |= (df["posteam"] == team) & (df["season"] == season)
    return m


# =============================================================================
# coach_coordinator_levers_2026.csv fields -- mirrors
# export_coach_coordinator_levers_v_0_1_0.py exactly, REG season only.
# =============================================================================
def build_lever_fields(df_reg, spans):
    sub = df_reg[spans_mask(df_reg, spans)]
    universe = sub[sub["play_type"].isin(["pass", "run"])].copy()
    universe = universe[
        (universe["qb_spike"] == 0) & (universe["qb_kneel"] == 0) & (universe["aborted_play"] == 0)
    ]

    out = {}
    for zone in ZONES:
        zsub = universe[universe["zone"] == zone]
        n = len(zsub)
        proe_vals = zsub["pass_oe"].dropna()
        out[f"n_plays_{zone}"] = n if n else ""
        out[f"proe_{zone}"] = round(float(proe_vals.mean()), 4) if len(proe_vals) else ""
        out[f"no_huddle_rate_{zone}"] = round(float((zsub["no_huddle"] == 1).mean()), 4) if n else ""

        passes = zsub[(zsub["play_type"] == "pass")].dropna(subset=["air_yards"])
        out[f"deep_shot_rate_{zone}"] = round(float((passes["air_yards"] >= 20).mean()), 4) if len(passes) else ""

    # Pace: running-clock methodology, computed over the FULL reg-season
    # frame (needs neighboring plays for prev-play state) then restricted
    # to this proxy's own spans by current-play team/season.
    out.update(compute_pace_for_spans(df_reg, spans))
    return out


def compute_pace_for_spans(df_reg, spans):
    d = df_reg.sort_values(["game_id", "play_id"]).reset_index(drop=True)

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
    running_clock = running_clock[spans_mask(running_clock, spans)]

    out = {}
    for zone in ZONES:
        zsub = running_clock[running_clock["zone"] == zone]
        out[f"sec_per_play_{zone}"] = round(float(zsub["game_clock_elapsed"].mean()), 2) if len(zsub) else ""
    return out


# =============================================================================
# coach_dna.json fields -- mirrors build_coach_dna() in
# R/scripts/build_dna_registry_v_0_1_0.R exactly (pass plays only, REG+POST,
# no season_type filter -- matches that script literally).
# =============================================================================
def build_dna_fields(df_all, spans):
    pbp_pass = df_all[
        (df_all["play_type"] == "pass")
        & (df_all["qb_spike"].isna() | (df_all["qb_spike"] == 0))
        & (df_all["aborted_play"].isna() | (df_all["aborted_play"] == 0))
    ].copy()
    pbp_pass["is_deep"] = pbp_pass["air_yards"].notna() & (pbp_pass["air_yards"] >= DEEP_THRESH)
    pbp_pass["is_screen"] = pbp_pass["air_yards"].notna() & (pbp_pass["air_yards"] <= SCREEN_THRESH)
    pbp_pass["play_action"] = pbp_pass["play_type_nfl"].fillna("").str.contains("play_action", case=False).astype(int)
    pbp_pass["no_huddle_f"] = pbp_pass["no_huddle"].fillna(0).astype(int)
    pbp_pass["rpo"] = pbp_pass["play_type_nfl"].fillna("").str.contains("run_pass_option|rpo", case=False)

    league_ay = (
        pbp_pass[pbp_pass["air_yards"].notna()]
        .groupby("season")["air_yards"].mean()
        .rename("league_mean_ay")
    )

    coach_pass = pbp_pass[spans_mask(pbp_pass, spans) & pbp_pass["air_yards"].notna()].copy()
    coach_pass = coach_pass.join(league_ay, on="season")
    coach_pass["ay_vs_league"] = coach_pass["air_yards"] - coach_pass["league_mean_ay"]

    season_stats = coach_pass.groupby("season").agg(
        n_plays=("air_yards", "size"),
        mean_ay_vs_lg=("ay_vs_league", "mean"),
        deep_rate=("is_deep", "mean"),
        screen_rate=("is_screen", "mean"),
        pa_rate=("play_action", "mean"),
        nh_rate=("no_huddle_f", "mean"),
        rpo_rate=("rpo", "mean"),
    )
    season_stats = season_stats[season_stats["n_plays"] >= COACH_MIN_PLAYS]

    def wavg(col):
        w = season_stats["n_plays"]
        return round(float((season_stats[col] * w).sum() / w.sum()), 4) if w.sum() else None

    valid = coach_pass.dropna(subset=["score_differential", "air_yards"])
    conservative_score_bias = round(float(valid["score_differential"].corr(valid["air_yards"])), 4) if len(valid) > 1 else None

    # proe -- same definition as merge_coach_proe.py's off_proe field:
    # mean(pass_oe) over the play-selection universe (pass+run, no
    # spike/kneel/aborted), REG season only for consistency with how
    # pass_oe/PROE is defined everywhere else in this repo.
    df_reg = df_all[df_all["season_type"] == "REG"]
    universe = df_reg[spans_mask(df_reg, spans) & df_reg["play_type"].isin(["pass", "run"])]
    universe = universe[(universe["qb_spike"] == 0) & (universe["qb_kneel"] == 0) & (universe["aborted_play"] == 0)]
    proe_vals = universe["pass_oe"].dropna()
    proe = round(float(proe_vals.mean()), 4) if len(proe_vals) else None

    return {
        "air_yards_tendency": wavg("mean_ay_vs_lg"),
        "deep_shot_rate": wavg("deep_rate"),
        "screen_rate": wavg("screen_rate"),
        "play_action_rate": wavg("pa_rate"),
        "no_huddle_rate": wavg("nh_rate"),
        "rpo_rate": wavg("rpo_rate"),
        "conservative_score_bias": conservative_score_bias,
        "seasons_observed": int(len(season_stats)),
        "total_pass_plays": int(season_stats["n_plays"].sum()) if len(season_stats) else 0,
        "proe": proe,
    }


def update_coach_dna(df_all):
    coach_dna = json.load(open(COACH_DNA_PATH, encoding="utf-8"))

    for name, spec in PROXIES.items():
        fields = build_dna_fields(df_all, spec["spans"])
        fields["_note"] = spec["note"]
        coach_dna[name] = fields
        print(f"  coach_dna.json['{name}'] -> seasons_observed={fields['seasons_observed']}, "
              f"total_pass_plays={fields['total_pass_plays']}, proe={fields['proe']}")

    metadata = coach_dna.pop("_metadata")
    ordered = {"_metadata": metadata}
    for name in sorted(coach_dna):
        ordered[name] = coach_dna[name]

    with open(COACH_DNA_PATH, "w", encoding="utf-8", newline="") as f:
        text = json.dumps(ordered, indent=2, ensure_ascii=False)
        f.write(text.replace("\n", "\r\n"))
    print(f"Updated {COACH_DNA_PATH}.")


def update_levers_csv(df_all):
    df_reg = df_all[df_all["season_type"] == "REG"]
    rows = pd.read_csv(LEVERS_CSV_PATH, dtype=str, keep_default_na=False)

    team_to_proxy = {spec["team"]: (name, spec) for name, spec in PROXIES.items()}

    for i, row in rows.iterrows():
        team = row["team"]
        if team not in team_to_proxy:
            continue
        name, spec = team_to_proxy[team]
        fields = build_lever_fields(df_reg, spec["spans"])
        rows.at[i, "note"] = spec["note"]
        for k, v in fields.items():
            rows.at[i, k] = "" if v == "" else str(v)
        print(f"  levers CSV row '{team}' ({name}) -> n_plays_primary={fields['n_plays_primary']}")

    rows.to_csv(LEVERS_CSV_PATH, index=False)
    print(f"Updated {LEVERS_CSV_PATH}.")


def main():
    df_all = load_pbp()
    print("\nBuilding coach_dna.json proxy entries...")
    update_coach_dna(df_all)
    print("\nBuilding coach_coordinator_levers_2026.csv proxy rows...")
    update_levers_csv(df_all)


if __name__ == "__main__":
    main()
