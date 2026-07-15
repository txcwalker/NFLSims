# Air Yards Model — Documentation
**Version:** V.0.3.0
**Status:** ✅ COMPLETE — PASSED RIGOROUS EVALUATION
**Location:** `src/nfl_sim/models/air_yards_v_0_1_1/` (folder name predates the `V.0.3.0` internal version — never renamed to match)
**Training script:** `train_zone_split.py`

> **Note:** Per `MODEL_DEVELOPMENT_STANDARD.md`, this single document covers all three required stages: EDA Findings, Modeling Decision, and Evaluation Results.

---

## 1. Purpose

**What does this model do?**
Given a confirmed pass play, predict the air yards (yards the ball travels in the air from the line of scrimmage to the catch point, or target point on incompletions) on that play.

**Why do we need it?**
Air yards is the foundational output of the passing game simulation. Every downstream calculation — completion probability, yards after catch, turnover risk, field position — is conditional on where the ball was thrown. An air yards model that regresses to the mean and never produces deep shots (>20 yards) produces a simulation that systematically undershoots explosive plays, flattening score distributions and creating an unrealistic game engine.

**What the 'Regression to the Mean' Problem Is:**
A standard XGBoost regression on air yards produces a model that confidently predicts values near the mean (~8 yards) and fails to reproduce the long right tail of the distribution — the 25-, 35-, and 50-yard completions that exist in every NFL game. This is not a bug in XGBoost; it is a structural property of point-estimate regression applied to a multimodal, heavy-tailed, zero-inflated target variable. The model architecture decision in Section 3 must solve this problem explicitly.

**What it does NOT do:**
- It does not determine if a play is a pass — that is the play_selection model.
- It does not compute catch probability — that is a downstream model conditioned on air yards.
- It does not compute yards after catch — a separate model conditioned on both air yards and catch probability.

**DNA Registry Dependency (what the model actually consumes, verified against `train_zone_split.py`/`game_engine.py`):**

| DNA File | Variables Consumed |
|:---|:---|
| `data/dna/qb_dna.json` | `cpoe`, `avg_time_to_throw_sec` (feeds the per-play-sampled `play_ttt`, not read directly), `avg_air_yards_per_att` |
| `data/dna/skill_dna.json` | `target_share`, `carry_share` (RB starter, not the targeted receiver), `deep_target_rate` |

`trench_dna.json` and `coach_dna.json` are **not** consumed by this model — an earlier version of this table listed them aspirationally; see `docs/sims/inputs/README.md` for the full audit of which coach/trench fields actually reach any model.

---

## 2. EDA Findings

**EDA Script:** `R/scripts/air_yards_eda_v_0_1_0.R`
**Data Source:** `nflfastR::load_pbp(2015:2024)`, pass plays only (spikes + aborted snaps excluded)
**EDA Outputs:** `docs/eda_outputs/air_yards/`
**Total plays analyzed:** 188,272 (from 202,215 raw pass plays; 13,943 / 6.9% excluded due to NA air_yards)

---

### 2.1 Overall Distribution

| Statistic | Value |
|:---|:---|
| **Mean air yards** | 8.05 yds |
| **Median air yards** | 5 yds |
| **Standard deviation** | 10.1 yds |
| **% Screen / Behind LOS (≤ 0)** | **20.6%** |
| **% Negative air yards (< 0)** | 15.4% |
| **% Short passes (1–9 yds)** | 42.1% |
| **% Intermediate (10–19 yds)** | 21.4% |
| **% Deep shots (≥ 20 yds)** | **11.8%** |

> **Key finding:** The mean (8.05) and median (5) are separated by 3 yards, confirming the distribution is **right-skewed** with a long tail. The median is the better point estimator, but neither captures the tail behavior that drives explosive plays.

---

### 2.2 Multimodality & Zero-Inflation

**Finding: The distribution is STRONGLY zero-inflated and visually multimodal.**

20.6% of all pass plays have air_yards ≤ 0 (screens and behind-LOS throws). This is not a rounding artifact — it reflects a genuine and intentional play design cluster (bubble screens, swing passes, check-downs). The histogram shows:

1. **Screen cluster:** Strong spike at air_yards ≤ 0 (screen passes, 20.6% of plays)
2. **Short-route cluster:** Primary mode at ~5 yards (42.1% of plays in 1–9 range)
3. **Valley:** Noticeable density dip between 10–14 yards
4. **Intermediate cluster:** Secondary density bump at ~15 yards
5. **Deep shot tail:** Thin but non-negligible right tail from 20–70+ yards (11.8%)

> **Architecture implication:** This is **not** a single-mode distribution. A model that learns a single conditional mean will always predict ~7-9 yards and never generate the screen cluster or the deep shot tail. Any architecture that does not explicitly account for the screen cluster and the deep tail will fail the simulation distribution test.

**Deep shot cluster begins at:** 20 yards (confirmed by the histogram valley between 16–19 yards)

---

## 3. Modeling Decision

### 3.1 Option A — Two-Stage Hurdle Model

**Architecture:** Two sequential classifiers + one regressor.
1. **Stage 1A — Screen Gate (XGBoost Classifier):** Is this a screen play? (air_yards ≤ 0) → Yes/No
2. **Stage 1B — Deep Shot Gate (XGBoost Classifier):** Conditional on non-screen, is this a deep shot? (air_yards ≥ 20) → Yes/No
3. **Stage 2 — Conditional Regressor (3 separate XGBoost):** One per regime: screen AY, standard AY (1–19), deep shot AY (20+)

**Why it fits the EDA:**
The EDA shows three distinct behavioral regimes in the distribution (screen cluster, main body, deep tail). A hurdle model makes the regime membership an explicit decision, freeing each sub-regressor to operate on a homogeneous sub-population. The screen gate has strong predictors (coach_dna screen_rate, RPO rate, yardline_100). The deep shot gate has strong predictors (QB aggressiveness index, receiver route profile, score differential late game).

**Deep Shot handling:** Explicit — a dedicated binary classifier determines deep shot probability, and a deep-specific regressor predicts exact air yards within the deep shot regime. This is the most direct solution to the regression-to-mean problem.

---

## 4. Evaluation & Validation (V.0.1.1 Double Hurdle)

**Gate Performance (Tactical Decision):**
*   **Accuracy:** 71.3% (3-way classification: Screen / Standard / Deep)
*   **Log Loss:** 0.6710
*   **PR-AUC (Deep):** **0.3893** (Target: >0.20) — *Exceptional performance identifying deep triggers.*
*   **PR-AUC (Screen):** **0.5998**

**Regressor Performance (Within-Bucket Precision):**
*   **Screen RMSE:** 2.05 yards (MAE: 1.65)
*   **Standard RMSE:** 4.12 yards (MAE: 3.35)
*   **Deep RMSE:** 6.64 yards (MAE: 5.20)

**Distribution Check (Simulation vs 2024 Empirical):**
| Bucket | Empirical | Simulated (V.0.1.1) | Delta | Status |
|:---|:---|:---|:---|:---|
| **Screen (≤0)** | 22.0% | 20.6% | −1.4% | ✅ PASSED |
| **Standard (1-19)** | 66.5% | 67.2% | +0.7% | ✅ PASSED |
| **Deep (≥20)** | 11.5% | 12.2% | +0.7% | ✅ PASSED |

---

## 5. Current Architecture — Zone Splits

To capture spatial differences in target depth (Air Yards), the Double Hurdle model (a gate classifier + `screen`/`std`/`deep` regressors) is split across three spatial zones:
1. **Goalline (`goalline`):** `yardline_100 <= 5`
2. **Red Zone (`redzone`):** `20 >= yardline_100 > 5`
3. **Primary Field (`primary`):** `yardline_100 > 20`

A separate gate classifier and regressor pipeline is trained for each zone (9 gate/regressor pairs total).

**Dynamic Player DNA splits:** CPOE, target share, and carry share are looked up per active player and active zone. If a player has a zone-specific split in `skill_dna.json`/`qb_dna.json`, that value is used; otherwise it falls back to the player's season average.

## 6. Current Feature Set (11 features)

`down`, `ydstogo`, `yardline_100`, `score_differential`, `game_seconds_remaining`, `cpoe_by_filter`, `target_share_by_filter`, `carry_share_by_filter`, `play_ttt` (per-play sampled time-to-throw — the longer the sampled throw time, the more likely a deep read has developed; see `proe_overlay_v_0_1_0.py`'s `play_ttt` mechanism), `avg_air_yards_per_att` (QB's career average depth of target, `qb_dna.json`), `deep_target_rate` (receiver's career deep-target rate, `skill_dna.json`).

`deep_target_rate` is now the single most important feature in most zone/level combinations (42% importance in `primary_std`, 22% in `primary_deep`) — it's the first feature in this model that encodes throw-depth *preference* rather than just accuracy and volume.

**Player-name join fix (real bug, not a design change):** every retrain of this model before this fix keyed the DNA lookup on full player names ("Patrick Mahomes") against nflfastR's abbreviated PBP names ("P.Mahomes") — a total mismatch that silently zeroed out `cpoe_by_filter`/`target_share_by_filter`/`carry_share_by_filter` (100% constant, 0% match rate) in every prior version, including the V.0.1.1/V.0.2.0 metrics reported above. `train_zone_split.py` now converts DNA names to PBP format via `to_short_name()` before joining (verified 98.8%/95.7% row-weighted match rates for QBs/receivers). Post-fix metrics: primary gate accuracy 65.61%→68.26%; primary/std MAE 4.095→3.778; primary/deep MAE 6.409→6.181; redzone/std MAE 3.778→3.564.
