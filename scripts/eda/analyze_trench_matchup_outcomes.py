"""
analyze_trench_matchup_outcomes.py
====================================
Does trench (O-line/D-line) matchup strength actually move game-to-game
rushing/passing output and points? Builds 4 z-scored composites (run-block
offense, run defense, pass-block offense, pass defense) from the existing
team-season metric files, differences them into per-team-game matchup scores
against that week's actual opponent, and buckets every 2018-2024 REG-season
team-game by matchup strength to see how rate stats move as the mismatch
grows.

Z-scoring is done WITHIN season (a team is only compared to that year's
league) to remove real era drift -- e.g. pressured_pct_allowed league-wide
fell from ~24.5% (2018) to ~21% (2024), so pooling raw values across seasons
would score an average 2018 pass-block unit as "below average" purely for
being from an earlier era. Once composites are era-normalized this way, the
resulting matchup differentials ARE pooled across all seasons for bucketing
-- game-to-game, not stratified by year, per request.

matchup = offense_composite_z - defense_composite_z (difference, not ratio/
product -- both of those break near zero when a composite can be negative;
see WORKLOG for the reasoning). Positive = offensive edge.

Rush and pass matchups are kept fully separate (own composites, own
outcomes) -- a team's pass-block grade shouldn't dilute its rush matchup
score or vice versa.

Explosive/negative play rates need real play-by-play (team-season aggregates
can't tell you play-to-play shape), so this pulls nfl_data_py.import_pbp_data
for 2018-2024, REG season only.

"Negative play" is deliberately NOT "incomplete pass" -- an incompletion is a
neutral/normal outcome, not the pass rush winning the rep. Negative pass
plays here = sacks or completions for <=0 yards. Negative rush plays = any
carry for <=0 yards (there's no "incomplete" equivalent on the ground).

Run from repo root: python scripts/eda/analyze_trench_matchup_outcomes.py
"""

import os
import numpy as np
import pandas as pd
import nfl_data_py as nfl

YEARS = list(range(2018, 2025))
OUT_DIR = "docs/eda_outputs/trench_matchup_outcomes"
N_BUCKETS = 5
BUCKET_LABELS = ["Q1 (worst)", "Q2", "Q3 (neutral)", "Q4", "Q5 (best)"]

# metric -> +1 if higher raw value is better for that side, -1 if lower is better
RUN_BLOCK_OFF_METRICS = {
    "ybc_per_att": 1,
    "rush_pct_over_expected": 1,
    "stuff_rate": -1,
    "avg_time_to_los": -1,
}
RUN_DEF_METRICS = {
    "stuff_rate_forced": 1,
    "aly_allowed": -1,
    "ybc_allowed_per_att": -1,
    "rush_pct_over_expected_allowed": -1,
}
PASS_BLOCK_OFF_METRICS = {
    "pressured_pct_allowed": -1,
    "hurry_rate_allowed": -1,
    "hit_rate_allowed": -1,
}
PASS_DEF_METRICS = {
    "pressure_rate_forced": 1,
    "hurry_rate_forced": 1,
    "hit_rate_forced": 1,
}

os.makedirs(OUT_DIR, exist_ok=True)


def build_composite(path, metrics, out_col):
    """Within-season z-score each metric (sign-flipped so higher=better),
    average into a single composite column. Returns [team, season, out_col]."""
    df = pd.read_csv(path)
    z_cols = []
    for metric, sign in metrics.items():
        z_col = f"_z_{metric}"
        df[z_col] = df.groupby("season")[metric].transform(
            lambda x: (x - x.mean()) / x.std(ddof=0)
        ) * sign
        z_cols.append(z_col)
    df[out_col] = df[z_cols].mean(axis=1)
    return df[["team", "season", out_col]]


def build_opponent_map():
    """team-week -> opponent + points scored by team, REG season only."""
    sched = pd.read_csv("data/external/schedules_2015_2024.csv")
    sched = sched[(sched["season"].isin(YEARS)) & (sched["game_type"] == "REG")]
    away = sched[["season", "week", "away_team", "home_team", "away_score"]].rename(
        columns={"away_team": "team", "home_team": "opponent", "away_score": "points"})
    home = sched[["season", "week", "home_team", "away_team", "home_score"]].rename(
        columns={"home_team": "team", "away_team": "opponent", "home_score": "points"})
    return pd.concat([away, home], ignore_index=True)


def build_matchups():
    run_off = build_composite(
        "docs/eda_outputs/run_block_metrics_team_season.csv", RUN_BLOCK_OFF_METRICS, "run_block_off_z")
    run_def = build_composite(
        "docs/eda_outputs/run_defense_metrics_team_season.csv", RUN_DEF_METRICS, "run_def_z")
    pass_off = build_composite(
        "docs/eda_outputs/pass_block_offense_team_season.csv", PASS_BLOCK_OFF_METRICS, "pass_block_off_z")
    pass_def = build_composite(
        "docs/eda_outputs/pass_block_defense_team_season.csv", PASS_DEF_METRICS, "pass_def_z")

    games = build_opponent_map()
    games = games.merge(run_off, on=["team", "season"], how="left")
    games = games.merge(pass_off, on=["team", "season"], how="left")
    games = games.merge(
        run_def.rename(columns={"team": "opponent"}), on=["opponent", "season"], how="left")
    games = games.merge(
        pass_def.rename(columns={"team": "opponent"}), on=["opponent", "season"], how="left")

    games["rush_matchup"] = games["run_block_off_z"] - games["run_def_z"]
    games["pass_matchup"] = games["pass_block_off_z"] - games["pass_def_z"]
    return games.dropna(subset=["rush_matchup", "pass_matchup"])


def build_outcomes():
    pbp = nfl.import_pbp_data(YEARS, downcast=True)
    pbp = pbp[pbp["season_type"] == "REG"]

    rush = pbp[pbp["rush_attempt"] == 1].copy()
    rush_agg = rush.groupby(["season", "week", "posteam"]).agg(
        rush_att=("yards_gained", "size"),
        rush_yards=("yards_gained", "sum"),
        rush_explosive_rate=("yards_gained", lambda x: (x >= 10).mean()),
        rush_negative_rate=("yards_gained", lambda x: (x <= 0).mean()),
    ).reset_index()
    rush_agg["rush_ypa"] = rush_agg["rush_yards"] / rush_agg["rush_att"]

    drop = pbp[pbp["qb_dropback"] == 1].copy()
    drop["is_negative"] = (drop["sack"] == 1) | ((drop["complete_pass"] == 1) & (drop["yards_gained"] <= 0))
    drop["is_explosive"] = (drop["complete_pass"] == 1) & (drop["yards_gained"] >= 15)
    pass_agg = drop.groupby(["season", "week", "posteam"]).agg(
        dropbacks=("yards_gained", "size"),
        pass_yards=("yards_gained", "sum"),
        pass_explosive_rate=("is_explosive", "mean"),
        pass_negative_rate=("is_negative", "mean"),
    ).reset_index()
    pass_agg["pass_ypd"] = pass_agg["pass_yards"] / pass_agg["dropbacks"]

    outcomes = rush_agg.merge(pass_agg, on=["season", "week", "posteam"], how="outer")
    outcomes = outcomes.rename(columns={"posteam": "team"})
    return outcomes


def bucket_and_report(df, matchup_col, outcome_cols, label):
    d = df.dropna(subset=[matchup_col] + outcome_cols).copy()
    d["bucket"] = pd.qcut(d[matchup_col], N_BUCKETS, labels=BUCKET_LABELS)

    agg = {"n_games": (matchup_col, "size"), f"{matchup_col}_mean": (matchup_col, "mean")}
    for col in outcome_cols:
        agg[f"{col}_median"] = (col, "median")
        agg[f"{col}_mean"] = (col, "mean")
        agg[f"{col}_var"] = (col, "var")

    report = d.groupby("bucket", observed=True).agg(**agg).reset_index()
    report.to_csv(f"{OUT_DIR}/{label}_bucket_report.csv", index=False)
    print(f"\n=== {label} ===")
    print(report.round(4).to_string(index=False))
    return report


def main():
    print("Building matchup differentials...")
    matchups = build_matchups()

    print("Pulling PBP + building outcome rate stats (2018-2024, this takes a minute)...")
    outcomes = build_outcomes()

    merged = matchups.merge(outcomes, on=["season", "week", "team"], how="inner")
    merged.to_csv(f"{OUT_DIR}/team_game_matchups_and_outcomes.csv", index=False)
    print(f"\n{len(merged)} team-games matched.")

    bucket_and_report(
        merged, "rush_matchup",
        ["rush_ypa", "rush_explosive_rate", "rush_negative_rate", "points"],
        "rush_matchup",
    )
    bucket_and_report(
        merged, "pass_matchup",
        ["pass_ypd", "pass_explosive_rate", "pass_negative_rate", "points"],
        "pass_matchup",
    )

    print(f"\nSaved reports + full table to {OUT_DIR}/")


if __name__ == "__main__":
    main()
