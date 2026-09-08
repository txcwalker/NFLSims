"""
compare_rush_gate_before_after.py
===================================
Final deliverable of the trench matchup gate work: does the new gate
mechanism actually reproduce the real EDA-measured negative/explosive-play
rate curve across the matchup-quality range?

Real 2025 team lookups all come back near z=0 today (trench_dna.json only
has composite data through the 2024 season -- a known, separately-flagged
data-freshness gap, not a bug in this mechanism), so this drives the engine
directly and injects the 5 real bucket z-values from
docs/eda_outputs/trench_matchup_outcomes/rush_matchup_bucket_report.csv
(the same Q1-Q5 quintiles used throughout the EDA) rather than relying on
a real team pair to happen to span the range.

"Before" is represented as the organic (gate-forced-to-0) rate, not a literal
resurrection of the old trench_tiers multiplier -- that mechanism only ever
scaled play MAGNITUDE by a flat per-game scalar, it had no concept of a
negative/explosive-play RATE at all, so there's no meaningful old-mechanism
number to plot for this specific metric. Organic-only is the fair "what
would happen with no matchup-aware rate mechanism" baseline.

Run from repo root: ./venv/Scripts/python.exe scripts/eda/compare_rush_gate_before_after.py
(needs the venv -- ModelRegistry loads joblib models pickled under 3.12)
"""

import os
import sys
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))
from src.nfl_sim.game_engine import NFLGameEngine

OUT_DIR = "docs/eda_outputs/trench"
BUCKET_REPORT = "docs/eda_outputs/trench_matchup_outcomes/rush_matchup_bucket_report.csv"
N_GAMES = 2000
N_STEPS = 40
BUCKET_LABELS = ["Q1 (worst)", "Q2", "Q3 (neutral)", "Q4", "Q5 (best)"]

BEFORE_COLOR = "#898781"   # muted gray -- "no rate mechanism" baseline
AFTER_COLOR = "#2a78d6"    # blue -- validated categorical slot 1
TARGET_COLOR = "#e34948"   # red -- validated categorical slot 6 (real data, ground truth)

os.makedirs(OUT_DIR, exist_ok=True)


def run_at_matchup(z_value):
    sim = NFLGameEngine("KC", "BUF", year=2025, N=N_GAMES)
    sim.rush_matchup_away = z_value

    calib = sim.rush_gate_calib
    organic_neg = calib.get("organic_neg_rate", 0.0)
    organic_exp = calib.get("organic_exp_rate", 0.0)
    neg_a, neg_b = calib.get("target_neg_intercept", 0.0), calib.get("target_neg_slope", 0.0)
    exp_a, exp_b = calib.get("target_exp_intercept", 0.0), calib.get("target_exp_slope", 0.0)

    def gate_prob(z, a, b, organic):
        target = a + b * z
        return float(np.clip((target - organic) / max(1e-6, 1.0 - organic), 0.0, 1.0))

    sim.p_neg_gate_away = gate_prob(z_value, neg_a, neg_b, organic_neg)
    sim.p_exp_gate_away = gate_prob(z_value, exp_a, exp_b, organic_exp)

    after_gains, before_gains = [], []
    for _ in range(N_STEPS):
        sim.simulate_play_step()
        mask = sim.last_play_is_run & sim.possession_is_away
        if np.any(mask):
            after_gains.extend(sim.last_play_gain[mask].tolist())

    # Organic-only ("before") pass: same matchup, gate forced off.
    sim2 = NFLGameEngine("KC", "BUF", year=2025, N=N_GAMES)
    sim2.p_neg_gate_away = sim2.p_exp_gate_away = 0.0
    for _ in range(N_STEPS):
        sim2.simulate_play_step()
        mask = sim2.last_play_is_run & sim2.possession_is_away
        if np.any(mask):
            before_gains.extend(sim2.last_play_gain[mask].tolist())

    after_gains, before_gains = np.array(after_gains), np.array(before_gains)
    return {
        "before_neg": (before_gains <= 0).mean(), "after_neg": (after_gains <= 0).mean(),
        "before_exp": (before_gains >= 10).mean(), "after_exp": (after_gains >= 10).mean(),
        "n_after": len(after_gains), "n_before": len(before_gains),
    }


def main():
    bucket_df = pd.read_csv(BUCKET_REPORT)
    z_values = bucket_df["rush_matchup_mean"].values
    target_neg = bucket_df["rush_negative_rate_mean"].values
    target_exp = bucket_df["rush_explosive_rate_mean"].values

    rows = []
    for label, z, tneg, texp in zip(BUCKET_LABELS, z_values, target_neg, target_exp):
        print(f"Running {label} (z={z:.3f})...")
        r = run_at_matchup(z)
        rows.append({"bucket": label, "z": z, "target_neg": tneg, "target_exp": texp, **r})

    report = pd.DataFrame(rows)
    report.to_csv(f"{OUT_DIR}/rush_gate_before_after_report.csv", index=False)
    print("\n" + report.round(4).to_string(index=False))

    fig, axes = plt.subplots(1, 2, figsize=(12, 5))
    for ax, before_col, after_col, target_col, title in [
        (axes[0], "before_neg", "after_neg", "target_neg", "Negative-play rate"),
        (axes[1], "before_exp", "after_exp", "target_exp", "Explosive-play rate"),
    ]:
        x = np.arange(len(BUCKET_LABELS))
        ax.plot(x, report[before_col], marker="o", color=BEFORE_COLOR, linewidth=2, label="Before (organic, no gate)")
        ax.plot(x, report[after_col], marker="o", color=AFTER_COLOR, linewidth=2, label="After (gated)")
        ax.plot(x, report[target_col], marker="o", color=TARGET_COLOR, linewidth=2, linestyle="--", label="Real EDA target")
        ax.set_xticks(x)
        ax.set_xticklabels(BUCKET_LABELS)
        ax.set_title(title, fontsize=11, color="#0b0b0b")
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.grid(axis="y", alpha=0.3, color="#e1e0d9")
        ax.legend(frameon=False, fontsize=9)

    fig.suptitle("Rush trench gate: before vs. after vs. real target, by matchup bucket", fontsize=13, color="#0b0b0b")
    fig.tight_layout()
    fig.savefig(f"{OUT_DIR}/rush_gate_before_after.png", dpi=150, facecolor="#fcfcfb")
    plt.close(fig)

    print(f"\nSaved report + plot to {OUT_DIR}/")


if __name__ == "__main__":
    main()
