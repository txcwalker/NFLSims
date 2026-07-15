# ML Concepts Reference Guide

> A personal reference document for statistical and machine learning concepts encountered during project work. Intended as a living study resource — add new concepts as they come up.

---

## Brier Score

**What it is:** The mean squared error between a model's predicted *probability* and the actual binary outcome (0 or 1).

**Formula:**
```
Brier = (1/n) × Σ (predicted_probability − actual_outcome)²
```

**Bounds:** 0 to 1

| Value | Meaning |
|:---|:---|
| **0.00** | Perfect — predicted exactly 0% or 100% and was right every time |
| **0.25** | The "doing nothing" baseline for a perfectly balanced 50/50 problem (always predict 0.5) |
| **1.00** | Perfectly wrong every single time |

**The natural baseline for any problem** is `π × (1 − π)` where `π` is the true event rate. For a 90% pass rate bucket, the baseline Brier is `0.90 × 0.10 = 0.09`. This means a near-deterministic bucket will always have a low Brier score regardless of model quality — so you must compare the model's Brier to its own baseline, not to a fixed threshold.

**When to use it:** Any time your model outputs a probability (not just a hard class label). It penalizes confident wrong predictions more than uncertain wrong predictions, which is exactly what you want in a simulation context where the probability itself is what gets sampled.

**When it falls short:** Brier score does not tell you *where* the model is miscalibrated or which direction. A calibration plot is needed to diagnose that.

---

## ROC Curve & ROC-AUC

**What it is:** ROC stands for *Receiver Operating Characteristic*. The ROC curve plots:
- **True Positive Rate (Sensitivity)** on the Y-axis — out of all actual positives, what fraction did the model correctly identify?
- **False Positive Rate (1 − Specificity)** on the X-axis — out of all actual negatives, what fraction did the model incorrectly flag as positive?

The curve is drawn by sweeping the classification threshold from 0 to 1 and recording the TPR and FPR at each threshold.

**ROC-AUC** (Area Under the Curve) summarizes the whole curve as a single number.

| Value | Meaning |
|:---|:---|
| **0.50** | Coin flip — model has no discriminating power at all |
| **0.70** | Decent — model is meaningfully better than random |
| **0.80** | Good — strong discriminating power |
| **0.90+** | Excellent — rare in real-world noisy problems |
| **1.00** | Perfect — model ranks every positive above every negative |

**When to use it:** ROC-AUC is the right choice when your two classes are **roughly balanced** (e.g., 40/60 or 45/55 split). It treats both classes equally.

**When it misleads you:** When classes are **heavily imbalanced** (e.g., 95% negative, 5% positive). A model that always predicts "negative" can achieve a high ROC-AUC because the FPR stays low, even though it never finds the minority class. In that case, use PR-AUC instead.

---

## PR Curve & PR-AUC

**What it is:** PR stands for *Precision-Recall*. The PR curve plots:
- **Precision** on the Y-axis — of all the plays the model predicted as positive, what fraction actually were?
- **Recall (= Sensitivity = TPR)** on the X-axis — of all actual positives, what fraction did the model find?

**PR-AUC** summarizes the whole curve as a single number. Higher is always better.

**The key difference from ROC-AUC:**

| Question | Use |
|:---|:---|
| "Can the model tell positives from negatives overall?" | ROC-AUC |
| "Can the model find the rare positive without too many false alarms?" | PR-AUC |

**When to use PR-AUC:** When the **positive class is rare** and you care specifically about finding it — for example, predicting turnovers (rare event), injury predictions, or fraud detection. The baseline PR-AUC for a random classifier equals the positive class rate (e.g., 5% if 5% of plays are turnovers), making improvement easier to see than with ROC.

**When ROC is fine:** When classes are roughly balanced. For the play type selection model (~40% run / ~60% pass), ROC-AUC is the appropriate choice.

---

## Bayesian Shrinkage (Shrinkage Estimator)

**What it is:** A technique for blending a noisy small-sample estimate with a more stable prior, pulling ("shrinking") the estimate toward the prior in proportion to how little data you have.

**The general formula:**
```
estimate_blended = (n / (n + k)) × estimate_current
                 + (k / (n + k)) × prior
```

Where:
- `n` = number of observations in the current sample
- `k` = shrinkage constant — the number of prior observations the historical estimate is "worth"
- As `n → ∞`, the blend converges fully to the current estimate
- When `n = 0`, the blend equals the prior entirely

**Intuition:** You have a new head coach who has called 3 games this season. His 3-game pass rate is noisy (small sample). You don't want to ignore it, but you also don't want to throw out 10 years of NFL averages. Bayesian shrinkage says: blend them, but weight the prior heavily until enough current-year data accumulates.

**Choosing k:** The `k` value encodes your belief about how many current observations it takes to "outweigh" the prior. If you believe a coach's play-calling style is very stable year-to-year, use a large k (e.g., k=16). If you think it can change quickly, use a smaller k (e.g., k=4).

**NFLSims usage:** Applied to coach PROE (Pass Rate Over Expected) in the play type selection overlay. Prior = historical PROE from `coordinator_atlas.json`. k=8 means the prior year is worth 8 current-season games.

| Week | n_current | Weight on Current | Weight on Prior |
|:---|:---|:---|:---|
| Pre-season | 0 | 0% | 100% |
| Week 4 | 3 | 27% | 73% |
| Week 8 | 7 | 47% | 53% |
| Week 16 | 15 | 65% | 35% |

---

## Logit Space (Log-Odds Transformation)

**What it is:** A transformation that maps a probability (0 to 1) to the entire real number line (−∞ to +∞), making it safe and mathematically correct to add linear adjustments to probabilities.

**The formulas:**
```
logit(p)   = log(p / (1 - p))        # probability → log-odds
sigmoid(x) = 1 / (1 + exp(-x))       # log-odds → probability  (inverse of logit)
```

**Why you can't just add to probabilities directly:**

If a model outputs 0.90 (90% pass) and a coach has a +10pp PROE, naively adding gives 1.00 — an impossible probability that gets clipped. The same +10pp adjustment applied to a base of 0.50 gives 0.60 — a very different relative effect. The adjustment is inconsistent.

In logit space, the same additive offset has a *proportional* effect regardless of where the base probability sits, and the result is always a valid probability after applying the sigmoid.

**Example — the play type model overlay:**
```python
# Base model says 90% pass (Andy Reid, trailing 7, 2-min drill)
base_prob = 0.90

# Andy Reid PROE = +6.4pp
logit_base     = log(0.90 / 0.10) = 2.197
logit_offset   = 0.26  (converted from +6.4pp)
logit_adjusted = 2.197 + 0.26 = 2.457
p_final        = sigmoid(2.457) = 0.921  ← still a valid probability

# vs. naive addition: 0.90 + 0.064 = 0.964  ← overstated at high probabilities
```

**When to use it:** Any time you want to apply additive adjustments to a probability output — coach tendencies, situational multipliers, player modifiers, home/away effects, etc. Always transform to logit space, add, then sigmoid back.

---

## Quick Decision Guide: Which Metric to Use?

| Situation | Best Metric |
|:---|:---|
| Probability output, any balance | Brier Score + Log Loss |
| Balanced classes (~40-60 split) | ROC-AUC |
| Imbalanced classes (rare positive) | PR-AUC |
| Simulation context (probability matters) | Calibration Plot |
| All of the above, quick summary | Brier Score (it captures both) |

---

## Time-Varying R² and Regression to Mean

**The situation:** You train a regression model to predict a final outcome (e.g., final score margin) from game state features captured at different points in time. When you split your test set by time and compute R² per bucket, you see:

| Q1 (early) | Q2 | Q3 | Q4 (late) |
|:---|:---|:---|:---|
| R² = 0.115 | R² = 0.382 | R² = 0.664 | R² = 0.826 |

**The correct interpretation:** This is *not* a sign of a broken model. It is exactly what should happen.

**Why:** R² measures how much variance the model explains *relative to a "predict the mean" baseline*. In Q1, the game is early — most games are still close, and there is genuine uncertainty about the final outcome. The model correctly hedges its predictions toward zero (toward the mean), because being 7 points up in Q1 doesn't reliably predict the final margin. If R² were 0.8 in Q1, that would mean the model was over-confidently treating early scores as decisive — a calibration failure.

As time passes, the model accumulates more "information" (score differential, field position, timeouts all become more predictive as options narrow), and its predictions converge toward the true final state. By Q4, most of the variance IS explainable — a team up 14 with 2 minutes left is overwhelmingly likely to win by a margin close to 14.

**The analogy:** Think of a weather forecast. A 7-day forecast has low "R²" — it's genuinely hard to be precise that far out, and a good model will give wide confidence intervals. A same-day forecast has high "R²" — there's much less uncertainty left. Low early-period R² + high late-period R² is the sign of a *calibrated* model, not a weak one.

**The regression-to-mean effect:** When a model predicts early-period outcomes, it is rationally discounting its own features: "yes, they're up 7, but that only weakly predicts the final score this early." The result is that predictions shrink toward the mean. In score margin terms: a team up 7 in Q1 might get a predicted EFSD of only +3 or +4. That's correct — Q1 leads evaporate all the time.

**When to worry vs. not worry:**
- ✅ Low R² in early buckets, rising monotonically to late buckets → correct calibration
- ❌ R² drops or stays flat across time buckets → model is missing time as a feature or is overfit to early-game artifacts
- ❌ R² is uniformly high across all time buckets → the model has likely data-leaked final score information into the features

**Real example from this project:** The EFSD model (V.0.1.0) shows exactly the correct monotonic R² progression: 0.115 → 0.382 → 0.664 → 0.826. See [`docs/models/efsd_v_0_1_0.md`](../models/efsd_v_0_1_0.md).

---

*Last updated: June 2026*

