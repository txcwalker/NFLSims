"""
plot_trench_matchup_outcomes.py
=================================
Visualizes the bucket reports produced by analyze_trench_matchup_outcomes.py
(run that first if the CSVs are missing). Three figures, each answering one
question from the trench-multiplier design discussion:

  center_shift.png : does matchup quality move the median outcome?
                      (yes, monotonically, for rush/pass yards and points)
  asymmetry.png     : does a better matchup mostly add explosive plays, or
                      mostly remove negative ones? (removes negative ones --
                      the floor moves ~3x more than the ceiling)
  variance.png      : does the spread narrow as the mismatch grows? (no --
                      it's flat/noisy, and if anything widest at the best
                      matchups, not narrowest)

Bucket colors are the validated diverging pair (blue/red, palette.md) since
Q1-Q5 is a polarity axis (worse-than-neutral vs better-than-neutral matchup),
not a plain magnitude scale -- Q3 (neutral) gets the gray midpoint.

Run from repo root: python scripts/eda/plot_trench_matchup_outcomes.py
"""

import os
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

IN_DIR = "docs/eda_outputs/trench_matchup_outcomes"
OUT_DIR = "docs/eda_outputs/trench"

# Diverging ramp, worst -> neutral -> best (equal arms, palette.md blue<->red pair)
BUCKET_COLORS = ["#e34948", "#ef9695", "#c3c2b7", "#86b6ef", "#2a78d6"]
BUCKET_LABELS = ["Q1\n(worst)", "Q2", "Q3\n(neutral)", "Q4", "Q5\n(best)"]

# Categorical pair for the two-series asymmetry chart (validated, ΔE 114)
EXPLOSIVE_COLOR = "#2a78d6"  # blue, categorical slot 1
NEGATIVE_COLOR = "#eb6834"   # orange, categorical slot 8

os.makedirs(OUT_DIR, exist_ok=True)


def _style_ax(ax):
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_color("#c3c2b7")
    ax.spines["bottom"].set_color("#c3c2b7")
    ax.tick_params(colors="#52514e")
    ax.grid(axis="y", alpha=0.3, color="#e1e0d9")
    ax.set_axisbelow(True)


def plot_center_shift(rush, pas):
    fig, axes = plt.subplots(2, 2, figsize=(11, 8.5))

    specs = [
        (axes[0, 0], rush, "rush_ypa_mean", "Rush yards / attempt"),
        (axes[0, 1], pas, "pass_ypd_mean", "Pass yards / dropback"),
        (axes[1, 0], rush, "points_mean", "Points scored (rush-matchup buckets)"),
        (axes[1, 1], pas, "points_mean", "Points scored (pass-matchup buckets)"),
    ]
    for ax, df, col, title in specs:
        bars = ax.bar(BUCKET_LABELS, df[col], color=BUCKET_COLORS, edgecolor="white", linewidth=0.5)
        for b, v in zip(bars, df[col]):
            ax.text(b.get_x() + b.get_width() / 2, v, f"{v:.2f}", ha="center", va="bottom",
                    fontsize=9, color="#0b0b0b")
        ax.set_title(title, fontsize=11, color="#0b0b0b")
        _style_ax(ax)

    fig.suptitle("Center shifts cleanly and monotonically with matchup strength", fontsize=13, color="#0b0b0b")
    fig.tight_layout()
    fig.savefig(f"{OUT_DIR}/center_shift.png", dpi=150, facecolor="#fcfcfb")
    plt.close(fig)


def plot_asymmetry(rush, pas):
    fig, axes = plt.subplots(1, 2, figsize=(11, 4.8))
    specs = [
        (axes[0], rush, "rush_explosive_rate_mean", "rush_negative_rate_mean", "Rush"),
        (axes[1], pas, "pass_explosive_rate_mean", "pass_negative_rate_mean", "Pass"),
    ]
    x = range(len(BUCKET_LABELS))
    width = 0.36
    for ax, df, exp_col, neg_col, title in specs:
        ax.bar([i - width / 2 for i in x], df[exp_col], width, label="Explosive-play rate",
               color=EXPLOSIVE_COLOR, edgecolor="white", linewidth=0.5)
        ax.bar([i + width / 2 for i in x], df[neg_col], width, label="Negative-play rate",
               color=NEGATIVE_COLOR, edgecolor="white", linewidth=0.5)
        ax.set_xticks(list(x))
        ax.set_xticklabels(BUCKET_LABELS)
        ax.set_title(f"{title}: explosive vs. negative-play rate by matchup bucket", fontsize=10.5, color="#0b0b0b")
        ax.legend(frameon=False, fontsize=9)
        _style_ax(ax)

    fig.suptitle("A better matchup mostly removes negative plays, not adds explosive ones", fontsize=13, color="#0b0b0b")
    fig.tight_layout()
    fig.savefig(f"{OUT_DIR}/asymmetry.png", dpi=150, facecolor="#fcfcfb")
    plt.close(fig)


def plot_variance(rush, pas):
    fig, axes = plt.subplots(1, 3, figsize=(13, 4.3))
    specs = [
        (axes[0], rush, "rush_ypa_var", "Rush yds/att variance"),
        (axes[1], pas, "pass_ypd_var", "Pass yds/dropback variance"),
        (axes[2], rush, "points_var", "Points variance (rush-matchup buckets)"),
    ]
    for ax, df, col, title in specs:
        bars = ax.bar(BUCKET_LABELS, df[col], color="#2a78d6", edgecolor="white", linewidth=0.5)
        for b, v in zip(bars, df[col]):
            ax.text(b.get_x() + b.get_width() / 2, v, f"{v:.1f}", ha="center", va="bottom",
                    fontsize=9, color="#0b0b0b")
        ax.set_title(title, fontsize=10.5, color="#0b0b0b")
        _style_ax(ax)

    fig.suptitle("Spread does NOT narrow with mismatch -- flat to noisy, widest at Q5", fontsize=13, color="#0b0b0b")
    fig.tight_layout()
    fig.savefig(f"{OUT_DIR}/variance.png", dpi=150, facecolor="#fcfcfb")
    plt.close(fig)


def main():
    rush = pd.read_csv(f"{IN_DIR}/rush_matchup_bucket_report.csv")
    pas = pd.read_csv(f"{IN_DIR}/pass_matchup_bucket_report.csv")

    plot_center_shift(rush, pas)
    plot_asymmetry(rush, pas)
    plot_variance(rush, pas)

    print(f"Saved 3 figures to {OUT_DIR}/")


if __name__ == "__main__":
    main()
