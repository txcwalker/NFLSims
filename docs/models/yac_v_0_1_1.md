# Yards After Catch (YAC) Model — Documentation
**Version:** V.0.3.0
**Status:** ✅ COMPLETE — PASSED RIGOROUS EVALUATION
**Location:** `src/nfl_sim/models/yac_model_v_0_1_1/`
**Training script:** `train_zone_split.py` (`train.py` is an orphaned predecessor, trains a different non-zone-split architecture, superseded)

> **Note:** Per `MODEL_DEVELOPMENT_STANDARD.md`, this single document covers all three required stages: EDA Findings, Modeling Decision, and Evaluation Results.

---

## 1. Purpose

**What does this model do?**
Conditioned on a completed pass, this model predicts the yards after catch (YAC) gained by the receiver.

**Why do we need it?**
Yards after catch is a massive contributor to explosive plays in the modern NFL. An average short pass (e.g., 3 air yards) can easily turn into a 25-yard gain if a dynamic receiver like Deebo Samuel or Travis Kelce breaks a tackle. Modeling YAC realistically is essential to capture the true variance of play outcomes and prevent flat, low-scoring simulations.

---

## 2. Model Architecture & Spatial Splits (V.0.2.0)

To handle different defensive alignments and running space limitations in the scoring zones, the YAC model was upgraded to a **Spatial Split Regression Architecture**:

1. **Goalline YAC (`goalline`):** Used when `yardline_100 <= 5`. Captures congested goal-line spaces where defenders crowd the line of scrimmage.
2. **Red Zone YAC (`redzone`):** Used when `20 >= yardline_100 > 5`. Captures tighter coverage boundaries.
3. **Primary YAC (`primary`):** Used when `yardline_100 > 20`. Captures open field plays where explosive gains are highly possible.

Separate XGBoost regressors are trained for each zone:
- `primary_yac_reg.joblib`
- `redzone_yac_reg.joblib`
- `goalline_yac_reg.joblib`

If a zone model is missing or unsupported, it falls back to the season-wide `primary_yac_reg.joblib` model.

---

## 3. Features & DNA Integration

The YAC model consumes a feature vector defined by the following inputs:

| Feature Name | Description | Source |
|:---|:---|:---|
| `air_yards` | Passing distance through the air | Downstream Input |
| `yardline_100` | Distance from opponent's goal line | Game State |
| `ydstogo` | Yards to go for a first down | Game State |
| `score_differential` | Current score differential | Game State |
| `game_seconds_remaining` | Clock time remaining | Game State |
| `cpoe_by_filter` | Passer's split CPOE value | QB DNA (`qb_dna.json`) |
| `target_share_by_filter` | Receiver's split target share | Skill DNA (`skill_dna.json`) |
| `carry_share_by_filter` | Receiver's split carry share | Skill DNA (`skill_dna.json`) |
| `room_after_catch` | `yardline_100 - air_yards`, computed inline | Derived |

`room_after_catch` is the dominant feature by importance in redzone (15%) and especially goalline (70%) — the closer the catch point is to the goal line, the less open field there is to gain YAC in, and the model has learned that hard boundary.

**Player-name join fix (real bug, not a design change):** every retrain of this model before this fix keyed the DNA lookup on full player names ("Patrick Mahomes") against nflfastR's abbreviated PBP names ("P.Mahomes") — a total mismatch that silently zeroed out `cpoe_by_filter`/`target_share_by_filter`/`carry_share_by_filter` (0% match rate) in every prior version, including the metrics below from before the fix. `train_zone_split.py` now converts DNA names to PBP format via `to_short_name()` before joining, same fix as `air_yards_v_0_1_1/train_zone_split.py`. Post-fix, these three features carry real signal (5-8% importance each) instead of 0%.

---

## 4. Evaluation & Performance

The models are validated on a `GroupShuffleSplit`-by-`game_id` test split (70/15/15), live `nfl_data_py` pull, 2020-2025.

| Zone | Test MAE | Test RMSE | Baseline MAE |
|:---|:---|:---|:---|
| Primary | 4.23 yds | 6.59 yds | 4.77 yds |
| Red Zone | 2.31 yds | 3.24 yds | 3.32 yds |
| Goalline | 0.65 yds | 1.21 yds | 1.29 yds |

- **Extreme Outlier Clamping:** at inference, post-model noise is scaled to the play's own predicted YAC (`base_scale = max(0.75, yac × 1.25)`) rather than a flat constant, and the result is clamped to a minimum of `0.0` and a maximum of `yardline_100` to prevent scoring beyond the back of the end zone.
