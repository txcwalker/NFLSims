# Rush Yards Model Design Review — Documentation
**Version:** V.0.1.0
**Status:** ✅ APPROVED & INTEGRATED

---

## 1. Executive Summary

This document reviews the architectural decisions for modeling rushing yards in the NFL game simulator. It outlines the options analyzed, the chosen methodology (Log-Shift +30 XGBoost Regressor with Empirical Residual Bootstrapping), and why this approach delivers optimal simulation variance and physical boundary safety.

---

## 2. The Challenges of Rushing Yards Modeling

Rushing yards are exceptionally difficult to model with standard machine learning approaches for three reasons:

1. **Severe Zero-Inflation and Discontinuity:** A high percentage of runs result in exactly 1, 2, or 3 yards. Very few run plays result in negative yardage, but a small percentage result in very large gains (20-99 yards).
2. **Heavy Right Tail:** Rushing outcomes have a long right tail representing explosive breakaway runs.
3. **The "Regression to the Mean" Effect:** Standard MSE (Mean Squared Error) minimization forces regression models to predict the expected value (~4.0 yards) for almost all inputs. A simulator using raw point predictions would never produce explosive runs or negative runs, destroying score variance.

---

## 3. Architecture Options Evaluated

### Option A: Standard XGBoost Regressor
- **Description:** Train a raw XGBoost Regressor to predict rushing yards directly.
- **Pros:** Extremely simple to implement.
- **Cons:** Suffers severely from regression to the mean. Rushing distribution becomes compressed between 3.5 and 4.8 yards, completely eliminating explosive plays.

### Option B: Log-Shifted Regressor with Residual Bootstrapping (Chosen)
- **Description:** Shift the target variable by +30 yards to ensure all inputs are strictly positive, apply a natural log transform, train the XGBoost booster on the log target, and unshift the predictions. At inference time, randomly sample and inject empirical residuals from a zone-specific residuals pool to restore natural distribution variance.
- **Pros:**
  - Accurately captures the long right tail of explosive plays.
  - Successfully models negative runs (tackles for loss).
  - Retains realistic player and coach-specific variance.
- **Cons:** Slightly more complex inference logic due to bootstrapping.

---

## 4. Key Design Decisions

1. **Log-Shift Formula:**
   $$\text{target\_log} = \ln(\text{rush\_yards} + 30)$$
2. **Unshift Formula:**
   $$\text{pred\_unshifted} = e^{\text{pred\_log}} - 30.0$$
3. **Empirical Noise Injection:**
   Residual pools are stored during training for each spatial zone (`goalline`, `redzone`, `primary`) and sampled at runtime:
   $$\text{pred\_noisy} = \text{pred\_unshifted} + \text{noise}$$
4. **Physical Boundaries Capping:**
   To guarantee the runner does not go past the back of the end zone or run backward more than 10 yards:
   $$\text{final\_gain} = \max(-10.0, \min(\text{pred\_noisy}, \text{yardline\_100}))$$
