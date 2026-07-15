# Positional Expected Points (EP) Model — Documentation
**Version:** V.0.1.0
**Status:** ✅ COMPLETE — NOT USED IN THE LIVE SIMULATION (evaluator-only, by design)
**Location:** `src/nfl_sim/models/positional_ep_v_0_1_0/`
**Training script:** `train_positional_ep.py`

---

## 1. Purpose

**What does this model do?**
Given a field position and down/distance, predicts the expected points value of that situation — the classic "EP" concept from public football analytics (e.g. nflfastR's `ep`), independent of score, clock, or personnel.

**Why deliberately minimal features?**
Unlike every other model in this engine, this one is built with **only** `yardline_100`, `down`, `ydstogo`, `goal_to_go` — no roster/DNA, no score, no clock. That's intentional: positional EP is meant to answer "what is this field position worth in a vacuum," a stable reference value the Positional Evaluator uses as a baseline to measure everything else against. Adding score/clock context would turn it into a win-probability-flavored metric, which is a different, already-covered use case (see `win_probability_v_0_1_0.md`).

**Where it's actually used:** not loaded by `model_registry.py`, not called anywhere in `game_engine.py`. Used directly by `src/nfl_sim/nfl_positional_evaluator.py` as the baseline EP reference for the standalone Positional Evaluator tool.

---

## 2. Architecture

XGBoost regressor, `reg:squarederror` objective, `n_estimators=300`, `max_depth=4`, `learning_rate=0.05`, `subsample=0.8`, `min_child_weight=50` (aggressively regularized — a 4-feature model on this much data risks overfitting to noise without it).

**Features (4):** `yardline_100`, `down`, `ydstogo`, `goal_to_go`.

**Training data:** `data/processed/hardened_pass_training_master_v2_5.csv`.

---

## 3. Evaluation

| Metric | Validation | Test |
|:---|:---|:---|
| RMSE | 0.368 | 0.367 |
| MAE | 0.265 | 0.265 |

n_train=64,901, n_val=13,908, n_test=13,908.
