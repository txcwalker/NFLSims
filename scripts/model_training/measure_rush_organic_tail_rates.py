"""
measure_rush_organic_tail_rates.py
====================================
Second half of the trench matchup gate work (data-layer half was
build_trench_dna_composites.py). Three things, in order:

1. Measure how often the LIVE deployed rush model (RushYardsModelV010, not a
   retrain), running INSIDE real simulated games with the new gate forced
   off, already produces a negative (<=0 yd) or explosive (>=10 yd) play on
   its own. This is `organic_neg_rate` / `organic_exp_rate` -- the baseline
   the new gate must NOT double-count on top of.

   NOTE: an earlier version of this script estimated the organic rate via a
   standalone Monte Carlo over synthetic uniform-random scenarios (random
   yardline/ydstogo/game_seconds/score_diff, neutral "Unknown" personnel).
   That measured 11.70% organic_neg_rate, but a post-wiring smoke test of
   real simulated games showed a 26.9% realized negative rate against a
   ~20% calibration target -- a gap far too large to be sampling noise.
   Root cause: real games don't visit down/distance/yardline/zone situations
   uniformly the way the synthetic generator did (e.g. zone composition,
   real personnel cpoe/target_share/carry_share vs. the "Unknown" fallback
   0.0), so the synthetic organic-rate estimate didn't match what real
   gameplay actually produces. Fixed by measuring organic rate from real
   simulated games with the gate temporarily zeroed, instead of synthetic
   scenarios -- requires game_engine.py's gate wiring to already exist (used
   here purely as realistic scenario-sampling infrastructure with the gate
   disabled, not a circular dependency on the gate itself).

2. Fit target_neg(z) / target_exp(z) as linear functions of matchup z from
   the real bucket rates already measured in
   docs/eda_outputs/trench_matchup_outcomes/rush_matchup_bucket_report.csv,
   then compute the calibration gap:
       p_gate(z) = (target(z) - organic_rate) / (1 - organic_rate)
   Stored as the raw ingredients (target curve coefficients + organic rate),
   not pre-collapsed into gate coefficients, so the formula stays visible
   and traceable at the point of use in game_engine.py.

3. Build matchup-blind bootstrap pools of real historical rush yardage for
   the gate's two tail branches (negative_pool: yards_gained<=0,
   explosive_pool: yards_gained>=10) from 2018-2024 REG-season PBP -- shape
   stays constant across matchup levels by design; only the gate frequency
   carries the matchup effect (matches the EDA's flat-variance finding).

Saves everything to docs/eda_outputs/trench/rush_gate_calibration.json.

Run from repo root: python scripts/model_training/measure_rush_organic_tail_rates.py
"""

import sys
import os
import json
import numpy as np
import pandas as pd
import nfl_data_py as nfl

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))
from src.nfl_sim.game_engine import NFLGameEngine

OUT_DIR = "docs/eda_outputs/trench"
BUCKET_REPORT = "docs/eda_outputs/trench_matchup_outcomes/rush_matchup_bucket_report.csv"
YEARS = list(range(2018, 2025))
ORGANIC_MATCHUPS = [("KC", "BUF"), ("PHI", "DAL"), ("SF", "SEA")]
ORGANIC_N_GAMES = 3000
ORGANIC_STEPS = 40

os.makedirs(OUT_DIR, exist_ok=True)


def measure_organic_rates():
    """Real simulated games, gate forced to 0, so the sample matches the
    actual in-game distribution of down/distance/yardline/zone/personnel
    instead of a synthetic uniform scenario generator (see module docstring
    for why the earlier synthetic-Monte-Carlo version measured the wrong
    number)."""
    all_gains = []
    for away, home in ORGANIC_MATCHUPS:
        print(f"Simulating {away} @ {home} (N={ORGANIC_N_GAMES}, gate forced off) for organic rate...")
        # year=2025 (not 2024): roster data has gaps for some 2024 team/QB
        # combos (KeyError on an unresolved "Unknown" player_stats key) --
        # 2025 is the validated-working default elsewhere in this session.
        # Doesn't affect what's being measured here (gate is forced to 0
        # regardless of which season's trench composite would've applied).
        sim = NFLGameEngine(away, home, year=2025, N=ORGANIC_N_GAMES)
        sim.p_neg_gate_away = sim.p_neg_gate_home = 0.0
        sim.p_exp_gate_away = sim.p_exp_gate_home = 0.0

        for _ in range(ORGANIC_STEPS):
            sim.simulate_play_step()
            mask = sim.last_play_is_run
            if np.any(mask):
                all_gains.extend(sim.last_play_gain[mask].tolist())

    gains = np.array(all_gains)
    organic_neg_rate = float((gains <= 0).mean())
    organic_exp_rate = float((gains >= 10).mean())
    print(f"\nCollected {len(gains)} real rush plays (gate off) across {len(ORGANIC_MATCHUPS)} matchups.")
    print(f"organic_neg_rate (<=0 yd): {organic_neg_rate*100:.2f}%")
    print(f"organic_exp_rate (>=10 yd): {organic_exp_rate*100:.2f}%")
    print(f"(For reference, the old orphaned training script targeted 8.60% TFL / "
          f"2.48% explosive(>=20) against a different, retrained model -- not directly comparable.)")
    return organic_neg_rate, organic_exp_rate


def fit_target_curves():
    df = pd.read_csv(BUCKET_REPORT)
    z = df["rush_matchup_mean"].values
    neg_b, neg_a = np.polyfit(z, df["rush_negative_rate_mean"].values, 1)
    exp_b, exp_a = np.polyfit(z, df["rush_explosive_rate_mean"].values, 1)
    print(f"\ntarget_neg(z) = {neg_a:.4f} + {neg_b:.4f}*z")
    print(f"target_exp(z) = {exp_a:.4f} + {exp_b:.4f}*z")
    return (neg_a, neg_b), (exp_a, exp_b)


def build_tail_pools():
    print("\nPulling PBP (2018-2024, REG) for tail-outcome pools...")
    pbp = nfl.import_pbp_data(YEARS, downcast=True)
    pbp = pbp[(pbp["season_type"] == "REG") & (pbp["rush_attempt"] == 1)]

    negative_pool = pbp.loc[pbp["yards_gained"] <= 0, "yards_gained"].dropna().astype(float).tolist()
    explosive_pool = pbp.loc[pbp["yards_gained"] >= 10, "yards_gained"].dropna().astype(float).tolist()
    print(f"negative_pool: n={len(negative_pool)}")
    print(f"explosive_pool: n={len(explosive_pool)}")
    return negative_pool, explosive_pool


def main():
    organic_neg_rate, organic_exp_rate = measure_organic_rates()
    (neg_a, neg_b), (exp_a, exp_b) = fit_target_curves()
    negative_pool, explosive_pool = build_tail_pools()

    # Sanity check: at the best matchup bucket (highest real z), is target_neg still >= organic_neg_rate?
    df = pd.read_csv(BUCKET_REPORT)
    z_best = df["rush_matchup_mean"].max()
    target_neg_at_best = neg_a + neg_b * z_best
    if target_neg_at_best < organic_neg_rate:
        print(f"\nWARNING: at the best observed matchup (z={z_best:.2f}), target_neg "
              f"({target_neg_at_best*100:.2f}%) is BELOW organic_neg_rate ({organic_neg_rate*100:.2f}%). "
              f"p_neg_gate will clip to 0 there -- the base model alone already overshoots "
              f"real negative-play rates at elite matchups; game_engine.py should clip, not error.")

    out = {
        "organic_neg_rate": organic_neg_rate,
        "organic_exp_rate": organic_exp_rate,
        "target_neg_intercept": float(neg_a),
        "target_neg_slope": float(neg_b),
        "target_exp_intercept": float(exp_a),
        "target_exp_slope": float(exp_b),
        "negative_pool": negative_pool,
        "explosive_pool": explosive_pool,
    }
    out_path = f"{OUT_DIR}/rush_gate_calibration.json"
    with open(out_path, "w") as f:
        json.dump(out, f)
    print(f"\nSaved calibration + pools to {out_path}")


if __name__ == "__main__":
    main()
