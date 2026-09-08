"""
run_run_block_eda.py
=====================
Quick EDA for the 4 chosen run-blocking metrics (v0.4.0 trench-tier migration,
offense side): ybc_per_att, rush_pct_over_expected, stuff_rate, avg_time_to_los.
Reuses the team-season dataset already built by
analyze_run_block_metric_correlations.py (run that first if the CSV is missing).

Produces, in docs/eda_outputs/run_block/:
  - distributions.png       : histogram of each metric across all team-seasons
  - year_over_year_stability.png : each team's value in year N vs. year N+1
                                    (is this real team skill or just noise?)
  - top_bottom_teams.png    : 2018-2024 average, best/worst 8 teams per metric
  - summary_stats.csv       : mean/std/min/max/quartiles per metric
  - team_season_ranked.csv  : full team-season table, sorted by team/season

Run from repo root: python scripts/eda/run_run_block_eda.py
"""

import os
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

OUT_DIR = "docs/eda_outputs/run_block"
IN_CSV = "docs/eda_outputs/run_block_metrics_team_season.csv"

METRICS = ["ybc_per_att", "rush_pct_over_expected", "stuff_rate", "avg_time_to_los"]
LABELS = {
    "ybc_per_att": "Yards Before Contact / Att",
    "rush_pct_over_expected": "Rush % Over Expected",
    "stuff_rate": "Stuff Rate (lower = better)",
    "avg_time_to_los": "Avg Time to LOS (sec)",
}
# Single sequential hue (blue), consistent across all magnitude plots -- no rainbow.
HIST_COLOR = "#3B82C4"
GOOD_COLOR = "#3B82C4"   # best-8 bars
BAD_COLOR = "#C4783B"    # worst-8 bars (a second, distinct categorical hue -- identity: good vs. bad group)

os.makedirs(OUT_DIR, exist_ok=True)


def plot_distributions(df):
    fig, axes = plt.subplots(2, 2, figsize=(11, 8))
    for ax, m in zip(axes.flat, METRICS):
        ax.hist(df[m].dropna(), bins=20, color=HIST_COLOR, edgecolor="white", linewidth=0.5)
        ax.set_title(LABELS[m], fontsize=11)
        ax.set_ylabel("Team-seasons")
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.grid(axis="y", alpha=0.25)
    fig.suptitle("Run-block candidate metrics — distribution across 2018-2024 team-seasons", fontsize=13)
    fig.tight_layout()
    fig.savefig(f"{OUT_DIR}/distributions.png", dpi=150)
    plt.close(fig)


def plot_year_over_year_stability(df):
    """If a metric reflects real, persistent team quality (O-line continuity,
    scheme), a team's value this year should correlate with its value next year.
    If it's mostly noise, year N tells you nothing about year N+1."""
    fig, axes = plt.subplots(2, 2, figsize=(11, 8))
    for ax, m in zip(axes.flat, METRICS):
        merged = df[["team", "season", m]].merge(
            df[["team", "season", m]].assign(season=lambda x: x["season"] - 1),
            on=["team", "season"], suffixes=("_next", "_this")
        )
        x = merged[f"{m}_this"]
        y = merged[f"{m}_next"]
        r = x.corr(y)
        ax.scatter(x, y, s=18, alpha=0.6, color=HIST_COLOR, edgecolor="none")
        ax.set_title(f"{LABELS[m]}  (year N vs N+1, r={r:.2f})", fontsize=10)
        ax.set_xlabel("Year N")
        ax.set_ylabel("Year N+1")
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.grid(alpha=0.25)
    fig.suptitle("Year-over-year stability — is this team skill, or noise?", fontsize=13)
    fig.tight_layout()
    fig.savefig(f"{OUT_DIR}/year_over_year_stability.png", dpi=150)
    plt.close(fig)


def plot_top_bottom_teams(df):
    fig, axes = plt.subplots(2, 2, figsize=(12, 9))
    team_avg = df.groupby("team")[METRICS].mean().reset_index()

    for ax, m in zip(axes.flat, METRICS):
        # lower is "better" for stuff_rate and avg_time_to_los (faster/less-stuffed = better blocking)
        ascending = m in ("stuff_rate", "avg_time_to_los")
        ranked = team_avg.sort_values(m, ascending=ascending)
        best = ranked.head(8)
        worst = ranked.tail(8)
        combo = pd.concat([best, worst])
        colors = [GOOD_COLOR] * len(best) + [BAD_COLOR] * len(worst)

        ax.barh(combo["team"], combo[m], color=colors)
        ax.set_title(LABELS[m], fontsize=11)
        ax.invert_yaxis()
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.grid(axis="x", alpha=0.25)

    fig.suptitle("2018-2024 team averages — best 8 (blue) vs. worst 8 (orange) per metric", fontsize=13)
    fig.tight_layout()
    fig.savefig(f"{OUT_DIR}/top_bottom_teams.png", dpi=150)
    plt.close(fig)


def main():
    df = pd.read_csv(IN_CSV)

    summary = df[METRICS].describe().T
    summary.to_csv(f"{OUT_DIR}/summary_stats.csv")
    print("=== Summary statistics ===")
    print(summary.round(3).to_string())

    df.sort_values(["team", "season"]).to_csv(f"{OUT_DIR}/team_season_ranked.csv", index=False)

    plot_distributions(df)
    plot_year_over_year_stability(df)
    plot_top_bottom_teams(df)

    print(f"\nSaved plots + CSVs to {OUT_DIR}/")


if __name__ == "__main__":
    main()
