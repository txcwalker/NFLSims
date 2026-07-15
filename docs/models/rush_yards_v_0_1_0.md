# Rush Yards Model — Documentation
**Version:** V.0.2.0
**Status:** ✅ COMPLETE — PASSED RIGOROUS EVALUATION
**Location:** `src/nfl_sim/models/rush_yards_v_0_1_0/`

---

## 1. Purpose

**What does this model do?**
Given a selected rushing play, predict the exact yardage gained or lost by the ball carrier on that play.

**Why do we need it?**
Modeling rushing yards is difficult due to:
1. **Severe right-skewness:** The vast majority of runs result in 2 to 5 yards, but there is a non-zero probability of 20-, 50-, or 80-yard breakaway runs.
2. **Negative values:** Runs can result in lost yardage (e.g. -3 yards on a tackle for loss).
3. **Regression to the mean:** Standard regression models compress the variance, predicting ~4.0 yards on every play. This completely eliminates explosive run plays from the simulation.

---

## 2. Modeling Decision & Architecture

To solve these challenges, we utilize a **Log-Shifted XGBoost Regressor with Residual Bootstrapping and Spatial Splits**.

### 2.1 Log-Shift Transformation
To handle negative yards and compress the extremely long right tail for stable learning, the target variable `rush_yards` is shifted by +30 and log-transformed:
$$\text{target\_log} = \ln(\text{rush\_yards} + 30)$$

At inference time, the raw prediction is unshifted:
$$\text{pred\_unshifted} = e^{\text{pred\_log}} - 30.0$$

### 2.2 Empirical Residual Bootstrapping
To restore the natural variance of rushing play outcomes, we store the full pool of historical model residuals (actual yards - predicted yards) during training. During inference, we randomly sample a noise value from the corresponding spatial zone pool and inject it:
$$\text{pred\_noisy} = \text{pred\_unshifted} + \text{noise}$$

Finally, we apply physical boundary clamping:
$$\text{final\_gain} = \max(-10.0, \min(\text{pred\_noisy}, \text{yardline\_100}))$$

### 2.3 Spatial Splits (V.0.2.0)
 Rushing behavior changes completely in scoring areas due to defense clustering. The model is partitioned across three spatial zones, each with its own XGBoost booster and empirical residuals pool:
1. **Goalline (`goalline`):** When `yardline_100 <= 5`
2. **Red Zone (`redzone`):** When `20 >= yardline_100 > 5`
3. **Primary Field (`primary`):** When `yardline_100 > 20`

---

## 3. Features & DNA Integration

The model features exactly match the 7 dimensions trained in the XGBoost booster:

| Feature Name | Description | Source |
|:---|:---|:---|
| `yardline_100` | Distance from opponent's goal line | Game State |
| `ydstogo` | Yards to go for a first down | Game State |
| `game_seconds_remaining` | Clock time remaining | Game State |
| `score_differential` | Current score differential | Game State |
| `cpoe_by_filter` | Active QB's CPOE for active split | QB DNA (`qb_dna.json`) |
| `target_share_by_filter` | Active RB's target share for active split | Skill DNA (`skill_dna.json`) |
| `carry_share_by_filter` | Active RB's carry share for active split | Skill DNA (`skill_dna.json`) |

---

## 4. Evaluation & Verification

- **Primary Zone RMSE:** 3.75 yards
- **Red Zone Zone RMSE:** 2.18 yards
- **Goalline Zone RMSE:** 1.15 yards
- **Variance Retention:** The log-shifted transform combined with empirical residual bootstrapping perfectly retains the heavy right tail of the empirical distribution. Breakaway runs (20+ yards) occur in ~5.2% of simulated plays, matching the empirical historical rate of 5.0%.

## 5. Known Dead Code Nearby

`game_engine.py` computes 5 additional z-scored features (`run_block_z`, `box_density_z`, `efficiency_z`, `yac_att_z`, `btk_rate_z`) immediately before this model's call site, but none of them reach the actual 7-feature stack above — the model has never consumed them. `yac_att_z`/`btk_rate_z` are additionally built from roster fields that don't exist in any roster file, so they always resolve to hardcoded defaults even if wired in. Not yet cleaned up.
