# proe_overlay_v_0_1_0.py — Coach Pass Rate Over Expected (PROE) Overlay

### Why do we need it
A standard play type prediction model (XGBoost) evaluates down, distance, and field location. However, it does not account for a coaching staff's pass-heavy or run-heavy identity. This module blends and applies a coach Pass Rate Over Expected (PROE) adjustment to the base model.

### What is it doing
* **Bayesian Shrinkage Blending**: Blends a coach's historical PROE from the coordinator atlas with current-season performance using Bayesian shrinkage with a constant of `k=8` (equivalent to 8 games of prior weight).
* **Logit-Space Application**:
  * Converts the base pass probability into log-odds space (`logit`).
  * Calculates the dimensionless logit offset corresponding to the coach's PROE percentage points.
  * Adds the logit offset to the base log-odds.
  * Converts the adjusted log-odds back to a final probability using the sigmoid function, clipping results to [0.01, 0.99].
* **Historical Ingestion**: Reads team-to-coach maps and historical PROE listings at import time.
* **Pre-Play Optimization**: Precomputes logit offsets once per team to keep the simulation loop fast.

### Subject matter expertise utilized
* **Logit-Space Consistency**: Applying the PROE offset in logit space prevents mathematical overflow, ensuring that a +5.0% pass rate adjustment degrades gracefully at extreme probability levels (such as a 3rd-and-15 where the pass rate is already near 98%).
* **Bayesian Pacing**: Employs Bayesian shrinkage to blend historical profiles with active, current-season shifts, ensuring early-season noise does not cause overcorrections.

### Decisions made & metrics/reasoning used
* **Shrinkage Factor (k=8)**: Blends current-season statistics with historical baselines. In Week 0, it relies 100% on the historical prior. By Week 8, it is a 50/50 blend. By Week 16, it is approximately 67% current-season data.
* **Offensive Coordinator Focus**: Maps to the actual play-caller in the coordinator atlas, regardless of whether they hold the Head Coach or Offensive Coordinator title.

### Why this metric vs alternatives
* **Logit Transformation vs Additive Percentage Adjustments**: Adding a PROE offset directly to probabilities can exceed boundary limits (e.g. 98% + 5% = 103%). Transforming to log-odds and using sigmoid ensures mathematically sound probabilities within the [0, 1] range.

### What project goal does this accomplish
Aligns the engine's play-calling predictions with the actual strategic identity and tendencies of each team's coaching staff.
