# 🏈 NFL Return Modeling Specification (Punts, Kickoffs, Interceptions, and Fumbles)

This document specifies the design, rules, and mathematical distributions for modeling special teams and turnover returns within the NFL play-by-play simulation engine.

---

## 1. Objective
Design and implement four distinct statistical mechanisms to model returns (kickoffs, punts, defensive interception returns, and defensive fumble returns) for the play-by-play simulation engine. The model must capture the correct probability of return outcomes (Touchback, Fair Catch, Return, or Touchdown) and sample realistic return yardage that matches the physical bounds and distributions observed in historical NFL data.

---

## 2. Data Inputs & Temporal Rules
* **Kickoffs**: Use regular season play-by-play data from the **2024 and 2025 seasons only**. Note that because of strategic adjustments to the new dynamic kickoff rules, the **2025 season data** must be heavily prioritized/weighted (2025 touchback rate dropped to ~20.7% from 64.3% in 2024).
* **Punts, Interceptions, and Fumbles**: Use the **last 10 years of regular season data (2016–2025)** to ensure statistical significance for rare events (like return touchdowns).

---

## 3. Modeling Specifications by Play Type

### A. Kickoff Return Model
1. **Outcome Gate**:
   * Roll a Bernoulli trial to determine if the kickoff results in a **Touchback** vs. a **Return** (ignore fair catches). Use the weighted 2024–2025 touchback rates (heavily favoring 2025 rules).
2. **Return Yardage (Shifted Log-Normal)**:
   * If a return occurs, sample the yardage using a **Shifted Log-Normal** distribution:
     $$Y \sim \text{ShiftedLogNormal}(\mu, \sigma, \theta)$$
     Where $\theta$ is the location parameter (e.g., minimum return of $-5$ yards) to accommodate rare negative returns.
   * If the sampled return yards exceed the remaining distance to the opponent's endzone (`yardline_100`), cap the return at the goal line and award a touchdown.

### B. Punt Return Model
1. **Punt Distance & Touchback Gate (LOS-Dependent)**:
   * Punt distance and touchback probability must be modeled dynamically based on the **Line of Scrimmage (LOS) / `yardline_100`**:
     * There is a strong positive correlation ($r = 0.194$) between punt length and touchbacks, and a negative correlation ($r = -0.250$) between starting `yardline_100` and touchbacks.
     * When punting inside the opponent's territory, touchbacks are common (e.g., ~21.8% touchback rate for punts initiated from the opponent's 30–39 yard lines).
   * **Rule**: Punts must be modeled as a function of the starting yardline, where the touchback gate is resolved either through a logistic regression curve of the starting yardline or by checking if the sampled punt distance exceeds `yardline_100`.
2. **Fair Catch Gate**:
   * If the punt is not a touchback, roll a Bernoulli trial for a **Fair Catch** (historically occurs on ~27.4% of all punts). If a fair catch occurs, return yards = 0.
3. **Return Yardage (Shifted Exponential)**:
   * For active returns, sample yardage using a **Shifted Exponential** (or highly skewed Gamma) distribution to represent the high concentration of short returns (median 7.0 yards) and the long tail of explosive returns:
     $$Y \sim \text{ShiftedExponential}(\lambda, \theta)$$
     Where $\theta$ represents the location offset.
4. **Touchdown Resolution**:
   * Evaluate return touchdowns using the historical baseline (0.38% of all punts) as a point-mass check, or cap the sampled return at the goal line.

### C. Interception Return Model
1. **Touchdown Point-Mass Gate (Pick-Six)**:
   * First, roll a Bernoulli trial for an immediate **Return Touchdown** using the historical pick-six rate of **8.92%** of all interceptions.
2. **Tackle/Slide Gate**:
   * If no touchdown is scored, roll a Bernoulli trial to determine if the defender was **immediately tackled or slid down** for a **0-yard return** (this represents the massive point-mass at 0 in the empirical data).
3. **Active Return Yardage (Shifted Gamma)**:
   * For the remaining active returns, sample return yardage using a **Shifted Gamma** distribution:
     $$Y \sim \text{ShiftedGamma}(\alpha, \beta, \theta)$$
   * Cap the maximum return yards at `yardline_100 - 1` (since touchdowns were already evaluated in Step 1).

### D. Fumble Return Model
1. **Scope Restriction**:
   * Only apply return logic to **change-of-possession fumbles** (fumbles lost by the offense and recovered by the defense).
2. **Touchdown Point-Mass Gate (Scoop-and-Score)**:
   * Roll a Bernoulli trial for an immediate **Return Touchdown** using the historical scoop-and-score rate of **8.02%** of all lost fumbles.
3. **Securing the Ball Gate**:
   * If no touchdown is scored, apply a **Bernoulli gate with an 85–90% probability** of returning exactly **0 yards** (representing players falling on the ball to secure possession).
4. **Active Return Yardage (Highly Skewed Exponential)**:
   * For the remaining 10–15% of recoveries where the player advances the ball, sample yardage using a highly skewed **Exponential** distribution. Cap the maximum return yards at `yardline_100 - 1`.

---

## 4. Evaluation & Validation Targets
* **Goodness-of-Fit**: Use the Kolmogorov-Smirnov (KS) test to evaluate how closely the sampled simulation return distributions match the empirical distributions.
* **Calibration Targets**:
  * **Kickoff Return Avg**: ~24.7 yards (Combined 2024–2025)
  * **Punt Return Avg**: ~8.9 yards (Median: 7.0 yards)
  * **Interception Return Avg**: ~12.7 yards (Median: 5.0 yards, TD Rate: ~8.92%)
  * **Fumble Return Avg**: ~0.8 yards (Median: 0.0 yards, TD Rate: ~8.02%)
