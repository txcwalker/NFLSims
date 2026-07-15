# Fourth Down Conversion Model — Documentation
**Version:** V.0.1.0
**Status:** ✅ COMPLETE — PASSED RIGOROUS EVALUATION
**Location:** `src/nfl_sim/models/fourth_down_conversion_v_0_1_0/`

---

## 1. Purpose

**What does this model do?**
Conditioned on a team deciding to attempt a conversion on 4th down (going for it), this model predicts the probability of successfully converting and gaining a new first down or scoring a touchdown.

**Why do we need it?**
Fourth down decision-making is a cornerstone of modern football analytics. To accurately simulate coach choices (PUNT vs FIELD GOAL vs GO FOR IT), the simulation engine must first evaluate the exact probability of conversion. If the conversion model is inaccurate or lacks physical constraints, coaches will make highly unrealistic tactical decisions (e.g. going for it on 4th and 30 in their own territory in the first quarter).

---

## 2. Model Architecture & Boundary Safety

The model is built on top of a serialized scikit-learn classifier (`fd_conversion_model.joblib`) with a baseline model fallback (`lr_baseline_model.joblib`). 

To ensure absolute realism, the inference wrapper overlays strict physical boundaries and extreme scenario decays before and after model evaluation:

### 2.1 Pre-Inference Boundary Checks
1. **Immediate Success:** If `ydstogo <= 0.0`, the conversion is physically guaranteed and returns `1.0` immediately.
2. **Goal Line Limit:** Distance to go cannot exceed distance to the opponent's goal line (`ydstogo > yardline_100`). In this case, `ydstogo` is automatically capped at `yardline_100` before running inference.

### 2.2 Post-Inference Scenarios & Outliers
1. **Extreme Distances:** For extremely long fourth-down attempts (e.g. 4th and 35+), standard classifiers often over-predict success due to lack of historical training samples. The wrapper applies a strict decay, capping conversion probability at `0.02` (representing low-probability Hail Mary throws).
2. **Probability Clamping:** All predictions are clipped to physical limits `[0.0, 1.0]` to guarantee statistical stability.

---

## 3. Features & DNA Integration

The features consumed by the fourth down model are:

| Feature Name | Description | Source | Default Fallback |
|:---|:---|:---|:---|
| `ydstogo` | Yards needed to gain a first down | Game State | 10.0 |
| `yardline_100` | Yards from opponent's goal line | Game State | 50.0 |
| `score_differential` | Current score differential | Game State | 0.0 |
| `game_seconds_remaining` | Clock time remaining in regulation | Game State | 1800.0 |

---

## 4. Decision Overlay & Aggression Bias

At runtime, the probability of converting is evaluated alongside:
1. **Field Goal Success Probability:** Based on kicker DNA and kick distance.
2. **Punt Utility:** Expected field position return.
3. **Late-Game Urgency:** Under 2 minutes remaining, if a team is trailing by 8 or fewer points, the engine overrides standard decision math and forces a conversion attempt if field goal or punt options are mathematically insufficient to win.
4. **Coach Aggression Overlay:** a per-team `coach_aggression` DNA scalar (`game_engine.py:526`) is added directly onto the "GO" option's weight before the three options (PUNT/FG/GO) are renormalized — a rules-based nudge layered on top of the trained model's raw conversion probability, not part of the model itself.
