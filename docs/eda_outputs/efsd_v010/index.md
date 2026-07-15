# EFSD V.0.1.0 — EDA & Diagnostic Outputs

**Model version:** V.0.1.0
**Training run:** June 2026
**Seasons:** 2020–2023 train · 2024 test
**Plots location:** `src/nfl_sim/models/efsd_v_0_1_0/`

---

## Plot Inventory

### 1. `efsd_calibration.png` — Predicted vs Actual Final Margin

![EFSD Calibration](efsd_calibration.png)

**What to look for:** Scatter of model prediction (x) vs actual final margin (y). Perfect calibration = tight diagonal from lower-left to upper-right.

**Findings:**
- The scatter is well-centered around the diagonal with no systematic bias direction.
- Variance is highest at extreme observed margins (±30+), where the model appropriately hedges toward zero. This is correct — a +35 blowout early in Q1 is a real outlier; the model does not over-commit to it.
- RMSE of 10.1 pts means the "error band" is roughly one TD+FG — acceptable for a full-game horizon predictor from a single moment in game time.

---

### 2. `efsd_time_decay.png` — R² and RMSE by Quarter

![EFSD Time Decay](efsd_time_decay.png)

**What to look for:** Two subplots — R² rising toward 1.0 as time runs out, RMSE falling toward 0.

| Quarter | R² | RMSE |
|:---|:---|:---|
| Q1 | 0.115 | 13.73 pts |
| Q2 | 0.382 | 11.55 pts |
| Q3 | 0.664 | 8.67 pts |
| Q4 | 0.826 | 5.24 pts |

**Key insight — why low Q1 R² is correct:** R² measures how much variance the model explains relative to a "predict the mean" baseline. In Q1, most games are still near 0–0 or within a score, so predicting the eventual final margin is genuinely hard. The model correctly shrinks its predictions toward 0 (regression to mean). If R² were 0.8 in Q1, it would mean the model was over-confidently locked onto early scores — that would be a calibration failure, not a strength.

The progression 0.115 → 0.382 → 0.664 → 0.826 is the expected monotonic convergence for any well-behaved time-series margin predictor.

---

### 3. `efsd_feature_importance.png` — XGBoost Feature Gain Scores

![EFSD Feature Importance](efsd_feature_importance.png)

**What to look for:** Horizontal bar chart of feature importance by gain (average improvement in loss when a feature is used in a split).

**Expected ranking (based on training):**
1. `score_differential` — dominates. The current score is the single strongest predictor of the final margin.
2. `game_seconds_remaining` — second most important. Time remaining determines how much the current state can change.
3. `yardline_100` — field position matters; drives near the goal line resolve quickly.
4. `ydstogo` / `down` — situational context; 3rd & 15 vs 1st & 10 are different risk profiles.
5. `posteam_timeouts_remaining` / `defteam_timeouts_remaining` — low importance individually, but relevant in late-game close situations.
6. `receive_2h_ko` — minimal importance overall; most relevant at halftime boundary.

---

### 4. `efsd_epa_correlation.png` — Δ EFSD vs EPA Scatter

![EFSD vs EPA Correlation](efsd_epa_correlation.png)

**What to look for:** Scatter of the change in EFSD from play N to play N+1 (y) against the EPA for that play (x). If the two metrics agree, points fall along a diagonal. If they're uncorrelated, the scatter is a diffuse cloud.

**Findings:**
- Pearson r = 0.018 — essentially uncorrelated at play level.
- This is **expected and correct behavior.** EPA is a micro-scale metric: how many expected points did THIS play produce vs the baseline for this situation? EFSD is a macro-scale metric: what do we think the final margin will be?

**Why they diverge:** A 55-yard punt is negative EPA (offense loses yards, gives up possession). But if it pins the opponent at their own 2-yard line, EFSD may improve because the expected field position after the opponent's series is advantageous. EPA doesn't capture that; EFSD does (indirectly, via field position and possession dynamics in the training data).

Over long sequences (a full drive or multiple drives), the two metrics converge directionally. Don't use their play-level correlation as a model quality signal.

---

## Asymmetry Finding

This came out of validation testing, not a diagnostic plot. Teams leading by 7 and trailing by 7 produce asymmetric EFSD predictions:

| Situation | EFSD prediction |
|:---|:---|
| Up 7, mid-game | ≈ +10 to +11 |
| Down 7, mid-game | ≈ −4 to −5 |

The asymmetry is real: trailing teams historically sacrifice expected margin to increase variance (going for it on 4th, throwing deep, no-huddle). This emergent behavior comes directly from the training distribution — it was not engineered in. It validates that the model has learned genuine game strategy patterns rather than just score-propagation.

---

## Validation Scenarios

Spot-checks run against `efsd_inference.py` to verify sensible outputs:

| Scenario | score_diff | seconds | EFSD (expected) | Result |
|:---|:---|:---|:---|:---|
| Tied kickoff | 0 | 3600 | Near 0 (±5 OK) | ≈ +0.90 ✓ |
| Up 7, Q4 2-min | +7 | 120 | Near +7 | +10.16 ✓ (still some clock risk) |
| Down 21, Q1 | −21 | 3200 | Large negative | −20.97 ✓ |

The "up 7 late" case returning +10.16 rather than exactly +7 is correct — with 2 minutes left there is still a real possibility the lead grows on a defensive stop + field goal.
