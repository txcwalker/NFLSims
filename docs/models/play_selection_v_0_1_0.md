# Play Selection Model — Documentation
**Version:** V.0.3.0
**Status:** ✅ COMPLETE — PASSED RIGOROUS EVALUATION
**Location:** `src/nfl_sim/models/play_selection_v_0_1_0/`
**Training script:** `train.py`

> **Note:** Per `MODEL_DEVELOPMENT_STANDARD.md`, this single document covers all three required stages: EDA Findings, Modeling Decision, and Evaluation Results.

---

## 1. Purpose

**What does this model do?**
Given the current play situation (down, distance, yardline, clock, timeouts, score differential), this model predicts the probability of a possession team executing a pass play versus a run play.

**Why do we need it?**
Play calling is the baseline decision that directs the simulation down either the passing tree (Air Yards, YAC, catch success, sacks, interceptions) or the running tree (Rush Yards, fumbles). If the play selection model does not reflect real-world team tendencies, game context (e.g. passing more when trailing late), or personnel capabilities, the game engine will simulate highly unrealistic box scores.

**What it does NOT do:**
- It does not determine fourth-down decisions (PUNT, FG, or GO) — that is handled by the Fourth Down Conversion and decision models.
- It does not simulate the play outcome — it only selects the play type.

---

## 2. Model Architecture & Spatial splits (V.0.2.0)

To handle the highly contextual nature of play calling in different parts of the field and specific game situations, the play selection model is structured as a **Multi-Bucket Spatial Split Architecture**.

### 2.1 The Down-Distance Buckets (11 Situations)
Instead of a single monolithic model attempting to learn all football situations, we train 11 distinct situation-specific binary classifiers:
- **1st Down:**
  - `1_10`: First and 10
  - `1_long`: First and >10
  - `1_short`: First and <10
- **2nd Down:**
  - `2_long`: Second and >7
  - `2_med`: Second and 4 to 7
  - `2_short`: Second and <=3
- **3rd Down:**
  - `3_long`: Third and >7
  - `3_med`: Third and 4 to 7
  - `3_short`: Third and <=3
- **4th Down:**
  - `4_med_long`: Fourth and >2
  - `4_short`: Fourth and <=2

### 2.2 The Spatial Zone Splits (3 Zones)
To reflect the compressed nature of playcalling in the scoring zones, each of the 11 situation buckets is split across 3 geographical zones:
1. **Goalline (`goalline`):** When `yardline_100 <= 5`
2. **Red Zone (`redzone`):** When `20 >= yardline_100 > 5`
3. **Primary Field (`primary`):** When `yardline_100 > 20`

This yields **33 distinct submodels** (classifiers) overall. If a zone-specific model is not available for a given scenario, it seamlessly falls back to the `primary` zone equivalent.

---

## 3. Features & DNA Integration

The feature vector is exactly 9-dimensional:

| Index | Feature Name | Description | Source |
|:---|:---|:---|:---|
| 0 | `yardline_100` | Distance from opponent's goal line (1-99) | Game State |
| 1 | `game_seconds_remaining` | Time left in regulation (0-3600) | Game State |
| 2 | `score_differential` | Possession Team Score - Defending Team Score | Game State |
| 3 | `timeouts_pos` | Possession team timeouts remaining (0-3) | Game State |
| 4 | `timeouts_def` | Defending team timeouts remaining (0-3) | Game State |
| 5 | `leverage` | `score_differential × game_seconds_remaining`, computed inline | Game State |
| 6 | `cpoe_by_filter` | QB's completion percentage over expected for active split | QB DNA (`qb_dna.json`) |
| 7 | `target_share_by_filter` | Starting RB's target share for active split | Skill DNA (`skill_dna.json`) |
| 8 | `carry_share_by_filter` | Starting RB's carry share for active split | Skill DNA (`skill_dna.json`) |

**`proe_by_filter` (formerly index 9, sourced from `coach_proe_splits.json`) was removed.** It was a second, redundant PROE mechanism baked directly into the classifier — not user-adjustable, and duplicating the separate post-model PROE overlay (`proe_overlay_v_0_1_0.py`) that the frontend's PROE slider actually controls. Cam's call: keep only the overlay, since it's the one mechanism a user can actually reason about and adjust. `coach_proe_splits.json` was deleted along with the loading code in `model_registry.py`. See `docs/models/README.md` §2.13 for how the overlay works.

---

## 4. Evaluation & Performance

The 33 XGBoost classifiers are trained on live `nfl_data_py` pull (2020-2025 regular season), `GroupShuffleSplit` by `game_id` for a 70/15/15 train/val/test split. 4 of the 33 zone/bucket combos (goalline × 1_10, 1_long, 2_long, 3_long) have zero training data and fall back to a direct copy of the corresponding `primary` classifier.

Predicted pass rate tracks real pass rate closely across every bucket, e.g.:

| Bucket | Predicted Pass Rate | Real Pass Rate | Test Accuracy |
|:---|:---|:---|:---|
| `primary_1_10` | 49.6% | 49.5% | 61.8% |
| `primary_2_med` | 54.0% | 54.5% | 60.5% |
| `primary_3_med` | 86.6% | 86.7% | 87.6% |
| `primary_4_med_long` | 91.4% | 91.3% | 90.5% |

Test accuracy ranges roughly 52-93% by bucket — highest where situational context strongly implies play type (3rd/4th and long: coaches almost always pass), lowest in genuinely mixed situations (1st and 10, 2nd and medium), which is the expected and correct shape for this kind of classifier — a bucket where real coaches are unpredictable should not have an artificially confident model.
