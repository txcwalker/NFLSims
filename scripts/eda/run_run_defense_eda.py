"""
run_run_defense_eda.py
========================
Quick EDA for run-defense (run-stopping) candidate metrics, mirroring
run_run_block_eda.py for the offense side. Reuses the team-season dataset
built by analyze_run_defense_metrics.py.

Run from repo root: python scripts/eda/run_run_defense_eda.py
"""

import os
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

OUT_DIR = "docs/eda_outputs/run_defense"
IN_CSV = "docs/eda_outputs/run_defense_metrics_team_season.csv"

METRICS = ["ybc_allowed_per_att", "rush_pct_over_expected_allowed",
           "stuff_rate_forced", "aly_allowed", "pct_stacked_box_forced"]
LABELS = {
    "ybc_allowed_per_att": "Yards Before Contact Allowed / Att",
    "rush_pct_over_expected_allowed": "Rush % Over Expected Allowed",
    "stuff_rate_forced": "Stuff Rate Forced (higher = better)",
    "aly_allowed": "Adjusted Line Yards Allowed",
    "pct_stacked_box_forced": "% Attempts Faced w/ 8+ Box",
}
HIST_COLOR = "#3B82C4"
GOOD_COLOR = "#3B82C4"
BAD_COLOR = "#C4783B"

os.makedirs(OUT_DIR, exist_ok=True)


def plot_distributions(df):
    fig, axes = plt.subplots(2, 3, figsize=(15, 8))
    for ax, m in zip(axes.flat, METRICS):
        ax.hist(df[m].dropna(), bins=20, color=HIST_COLOR, edgecolor="white", linewidth=0.5)
        ax.set_title(LABELS[m], fontsize=10)
        ax.set_ylabel("Team-seasons")
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.grid(axis="y", alpha=0.25)
    axes.flat[-1].axis("off")
    fig.suptitle("Run-defense candidate metrics — distribution, 2018-2024 team-seasons", fontsize=13)
    fig.tight_layout()
    fig.savefig(f"{OUT_DIR}/distributions.png", dpi=150)
    plt.close(fig)


def plot_year_over_year_stability(df):
    fig, axes = plt.subplots(2, 3, figsize=(15, 8))
    for ax, m in zip(axes.flat, METRICS):
        merged = df[["team", "season", m]].merge(
            df[["team", "season", m]].assign(season=lambda x: x["season"] - 1),
            on=["team", "season"], suffixes=("_next", "_this")
        )
        x, y = merged[f"{m}_this"], merged[f"{m}_next"]
        r = x.corr(y)
        ax.scatter(x, y, s=18, alpha=0.6, color=HIST_COLOR, edgecolor="none")
        ax.set_title(f"{LABELS[m]}  (r={r:.2f})", fontsize=9)
        ax.set_xlabel("Year N")
        ax.set_ylabel("Year N+1")
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.grid(alpha=0.25)
    axes.flat[-1].axis("off")
    fig.suptitle("Year-over-year stability — real team skill, or noise?", fontsize=13)
    fig.tight_layout()
    fig.savefig(f"{OUT_DIR}/year_over_year_stability.png", dpi=150)
    plt.close(fig)


def plot_top_bottom_teams(df):
    fig, axes = plt.subplots(2, 3, figsize=(16, 9))
    team_avg = df.groupby("team")[METRICS].mean().reset_index()
    # higher is better run-D for: ybc_allowed_per_att (lower better -> ascending),
    # stuff_rate_forced (higher better -> descending), aly_allowed (lower better),
    # rush_pct_over_expected_allowed (lower better), pct_stacked_box_forced (context only)
    ascending_map = {
        "ybc_allowed_per_att": True, "rush_pct_over_expected_allowed": True,
        "stuff_rate_forced": False, "aly_allowed": True, "pct_stacked_box_forced": False,
    }
    for ax, m in zip(axes.flat, METRICS):
        ranked = team_avg.sort_values(m, ascending=ascending_map[m])
        best, worst = ranked.head(8), ranked.tail(8)
        combo = pd.concat([best, worst])
        colors = [GOOD_COLOR] * len(best) + [BAD_COLOR] * len(worst)
        ax.barh(combo["team"], combo[m], color=colors)
        ax.set_title(LABELS[m], fontsize=10)
        ax.invert_yaxis()
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.grid(axis="x", alpha=0.25)
    axes.flat[-1].axis("off")
    fig.suptitle("2018-2024 team averages — best 8 (blue) vs. worst 8 (orange) run defense per metric", fontsize=13)
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
