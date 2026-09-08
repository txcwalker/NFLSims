"""
fit_trench_multiplier_distributions.py
========================================
Head-to-head: split-normal vs. skew-normal as the shape for the trench
matchup multiplier's noise term. Fits both families to the REAL per-play
residual distribution (not team-game aggregates -- aggregates average away
most of the play-to-play spread we actually need for a per-play multiplier)
and compares them on:
  - KS statistic (pure shape-fit quality, ignores parameter count)
  - AIC (penalizes skew-normal's extra free parameter -- split-normal here
    is fit with location fixed at 0 via semi-deviation, 2 params; skew-normal
    is a free 3-param MLE fit, so AIC keeps the comparison fair)

Both are fit against the same target: yards_gained minus a shared linear
center(z) = b0 + b1*matchup_z, where matchup_z is the SAME team-game rush/
pass matchup differential used throughout this analysis (from
analyze_trench_matchup_outcomes.py). Bucket edges are the identical Q1-Q5
quintile edges already reported, applied to play-level rows via those team-
game z values -- not a fresh qcut on play-level data -- so results line up
with everything already discussed.

split-normal CDF (median-preserving construction: sign decided by a
symmetric Z, then rescaled -- see WORKLOG for derivation):
  F(x) = Phi(x/sigma_left)   for x < 0
  F(x) = Phi(x/sigma_right)  for x >= 0

Run from repo root: python scripts/eda/fit_trench_multiplier_distributions.py
"""

import os
import numpy as np
import pandas as pd
from scipy import stats
import nfl_data_py as nfl
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

YEARS = list(range(2018, 2025))
OUT_DIR = "docs/eda_outputs/trench"
N_BUCKETS = 5
BUCKET_LABELS = ["Q1 (worst)", "Q2", "Q3 (neutral)", "Q4", "Q5 (best)"]

HIST_COLOR = "#c3c2b7"
SPLIT_COLOR = "#2a78d6"
SKEW_COLOR = "#4a3aa7"

RUN_BLOCK_OFF_METRICS = {"ybc_per_att": 1, "rush_pct_over_expected": 1, "stuff_rate": -1, "avg_time_to_los": -1}
RUN_DEF_METRICS = {"stuff_rate_forced": 1, "aly_allowed": -1, "ybc_allowed_per_att": -1, "rush_pct_over_expected_allowed": -1}
PASS_BLOCK_OFF_METRICS = {"pressured_pct_allowed": -1, "hurry_rate_allowed": -1, "hit_rate_allowed": -1}
PASS_DEF_METRICS = {"pressure_rate_forced": 1, "hurry_rate_forced": 1, "hit_rate_forced": 1}

os.makedirs(OUT_DIR, exist_ok=True)


def build_composite(path, metrics, out_col):
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
    sched = pd.read_csv("data/external/schedules_2015_2024.csv")
    sched = sched[(sched["season"].isin(YEARS)) & (sched["game_type"] == "REG")]
    away = sched[["season", "week", "away_team", "home_team"]].rename(
        columns={"away_team": "team", "home_team": "opponent"})
    home = sched[["season", "week", "home_team", "away_team"]].rename(
        columns={"home_team": "team", "away_team": "opponent"})
    return pd.concat([away, home], ignore_index=True)


def build_matchups():
    run_off = build_composite("docs/eda_outputs/run_block_metrics_team_season.csv", RUN_BLOCK_OFF_METRICS, "run_block_off_z")
    run_def = build_composite("docs/eda_outputs/run_defense_metrics_team_season.csv", RUN_DEF_METRICS, "run_def_z")
    pass_off = build_composite("docs/eda_outputs/pass_block_offense_team_season.csv", PASS_BLOCK_OFF_METRICS, "pass_block_off_z")
    pass_def = build_composite("docs/eda_outputs/pass_block_defense_team_season.csv", PASS_DEF_METRICS, "pass_def_z")

    games = build_opponent_map()
    games = games.merge(run_off, on=["team", "season"], how="left")
    games = games.merge(pass_off, on=["team", "season"], how="left")
    games = games.merge(run_def.rename(columns={"team": "opponent"}), on=["opponent", "season"], how="left")
    games = games.merge(pass_def.rename(columns={"team": "opponent"}), on=["opponent", "season"], how="left")

    games["rush_matchup"] = games["run_block_off_z"] - games["run_def_z"]
    games["pass_matchup"] = games["pass_block_off_z"] - games["pass_def_z"]
    return games.dropna(subset=["rush_matchup", "pass_matchup"])[["season", "week", "team", "rush_matchup", "pass_matchup"]]


def split_normal_cdf(x, sigma_left, sigma_right):
    x = np.asarray(x, dtype=float)
    out = np.where(x < 0, stats.norm.cdf(x / sigma_left), stats.norm.cdf(x / sigma_right))
    return out


def split_normal_logpdf(x, sigma_left, sigma_right):
    x = np.asarray(x, dtype=float)
    sigma = np.where(x < 0, sigma_left, sigma_right)
    return stats.norm.logpdf(x, loc=0, scale=sigma)


def split_normal_pdf(x, sigma_left, sigma_right):
    return np.exp(split_normal_logpdf(x, sigma_left, sigma_right))


def fit_and_compare(plays, game_level_z, matchup_col, out_prefix, label):
    """plays: DataFrame with columns [matchup_col, 'yards_gained'] (one row per play).
    game_level_z: the deduplicated team-game matchup values -- bucket edges are computed
    from THIS (one weight per game), then applied to play-level rows, so a team-game
    with a lot of pass attempts doesn't pull the bucket edges toward its own plays."""
    z = plays[matchup_col].values
    y = plays["yards_gained"].values

    b1, b0 = np.polyfit(z, y, 1)
    center = b0 + b1 * z
    resid = y - center
    print(f"\n[{label}] center(z) = {b0:.4f} + {b1:.4f} * z  (n={len(y)} plays)")

    _, bin_edges = pd.qcut(game_level_z, N_BUCKETS, retbins=True, duplicates="drop")
    bin_edges[0], bin_edges[-1] = -np.inf, np.inf
    bucket = pd.cut(z, bins=bin_edges, labels=BUCKET_LABELS[:len(bin_edges) - 1])

    rows = []
    fig, axes = plt.subplots(1, N_BUCKETS, figsize=(19, 3.8), sharey=False)
    for i, blabel in enumerate(BUCKET_LABELS):
        mask = bucket == blabel
        r = resid[mask]
        if len(r) < 30:
            continue

        sigma_left = r[r < 0].std(ddof=1) if (r < 0).sum() > 5 else r.std(ddof=1)
        sigma_right = r[r >= 0].std(ddof=1) if (r >= 0).sum() > 5 else r.std(ddof=1)
        ks_split = stats.kstest(r, lambda x: split_normal_cdf(x, sigma_left, sigma_right))
        ll_split = split_normal_logpdf(r, sigma_left, sigma_right).sum()
        aic_split = 2 * 2 - 2 * ll_split

        a, loc, scale = stats.skewnorm.fit(r)
        ks_skew = stats.kstest(r, lambda x: stats.skewnorm.cdf(x, a, loc, scale))
        ll_skew = stats.skewnorm.logpdf(r, a, loc, scale).sum()
        aic_skew = 2 * 3 - 2 * ll_skew

        rows.append({
            "bucket": blabel, "n_plays": len(r),
            "sigma_left": sigma_left, "sigma_right": sigma_right,
            "ks_split": ks_split.statistic, "ks_split_p": ks_split.pvalue, "aic_split": aic_split,
            "skew_a": a, "skew_loc": loc, "skew_scale": scale,
            "ks_skew": ks_skew.statistic, "ks_skew_p": ks_skew.pvalue, "aic_skew": aic_skew,
            "ks_winner": "split" if ks_split.statistic < ks_skew.statistic else "skew",
            "aic_winner": "split" if aic_split < aic_skew else "skew",
        })

        ax = axes[i]
        xs = np.linspace(r.min(), r.max(), 300)
        ax.hist(r, bins=30, density=True, color=HIST_COLOR, edgecolor="white", linewidth=0.4, label="Empirical (real plays)")
        ax.plot(xs, split_normal_pdf(xs, sigma_left, sigma_right), color=SPLIT_COLOR, linewidth=2, label="Split-normal fit")
        ax.plot(xs, stats.skewnorm.pdf(xs, a, loc, scale), color=SKEW_COLOR, linewidth=2, linestyle="--", label="Skew-normal fit")
        ax.set_title(blabel, fontsize=10.5, color="#0b0b0b")
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(colors="#52514e", labelsize=8)
        if i == 0:
            ax.legend(frameon=False, fontsize=7.5)

    fig.suptitle(f"{label}: empirical play residuals vs. split-normal vs. skew-normal", fontsize=13, color="#0b0b0b")
    fig.tight_layout()
    fig.savefig(f"{OUT_DIR}/{out_prefix}_fit_comparison.png", dpi=150, facecolor="#fcfcfb")
    plt.close(fig)

    report = pd.DataFrame(rows)
    report.to_csv(f"{OUT_DIR}/{out_prefix}_distribution_fit_report.csv", index=False)
    print(report[["bucket", "n_plays", "ks_split", "ks_skew", "ks_winner", "aic_split", "aic_skew", "aic_winner"]].round(4).to_string(index=False))
    return report


def main():
    print("Building matchup differentials...")
    matchups = build_matchups()

    print("Pulling PBP (2018-2024, REG)...")
    pbp = nfl.import_pbp_data(YEARS, downcast=True)
    pbp = pbp[pbp["season_type"] == "REG"]

    rush = pbp[pbp["rush_attempt"] == 1][["season", "week", "posteam", "yards_gained"]].rename(columns={"posteam": "team"})
    rush = rush.merge(matchups[["season", "week", "team", "rush_matchup"]], on=["season", "week", "team"], how="inner").dropna()

    drop = pbp[pbp["qb_dropback"] == 1][["season", "week", "posteam", "yards_gained"]].rename(columns={"posteam": "team"})
    drop = drop.merge(matchups[["season", "week", "team", "pass_matchup"]], on=["season", "week", "team"], how="inner").dropna()

    rush_report = fit_and_compare(rush, matchups["rush_matchup"], "rush_matchup", "rush", "Rush")
    pass_report = fit_and_compare(drop, matchups["pass_matchup"], "pass_matchup", "pass", "Pass")

    print("\n=== Overall bucket-win tally (KS, lower stat wins) ===")
    combined = pd.concat([rush_report.assign(side="rush"), pass_report.assign(side="pass")])
    print(combined["ks_winner"].value_counts())
    print("\n=== Overall bucket-win tally (AIC, lower wins) ===")
    print(combined["aic_winner"].value_counts())

    print(f"\nSaved figures + reports to {OUT_DIR}/")


if __name__ == "__main__":
    main()
