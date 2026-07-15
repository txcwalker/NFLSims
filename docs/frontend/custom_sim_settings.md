# Custom Sim Settings (Vegas Line Alignment)

This document outlines the architecture, mathematical model, and implementation details for aligning player and game simulation results with user-defined custom spreads and over/under totals, while preserving the unbiased play-by-play nature of the Monte Carlo simulation.

---

## 1. Objective & Philosophy
By default, the simulation engine runs an **unbiased play-by-play model** that evaluates team DNA and matchups without being influenced by Las Vegas betting lines or public biases. 

However, users need the ability to simulate "what-if" scenarios (e.g., *What if Dallas blowout Philadelphia?* or *What if this is a high-scoring shootout?*). 

To honor these inputs while keeping the core simulation organic, we use a **Two-Tier Resampling Architecture** rather than arbitrarily overriding simulated scores.

---

## 2. Technical Architecture: Importance Sampling
Instead of forcing slow simulations to rerun on the fly or artificially modifying individual player fantasy statistics, we implement **Importance Sampling (Weighted Resampling)**. 

When a user adjusts the Custom Spread or Over/Under and clicks **Run Engine**, the backend applies a statistical weight to every simulated game run based on how close its margin and total score are to the target scenario.

### Mathematical Formulation
For each simulation run $i$ with simulated score margin $M_i = \text{Score}_{\text{away}} - \text{Score}_{\text{home}}$ and total score $T_i = \text{Score}_{\text{away}} + \text{Score}_{\text{home}}$, we compute a weight $W_i$ using a 2D Gaussian density function:

$$W_i = \exp\left( -\frac{(M_i - S_{\text{target}})^2}{2\sigma_m^2} - \frac{(T_i - O_{\text{target}})^2}{2\sigma_t^2} \right)$$

Where:
* $S_{\text{target}}$ is the user's custom spread.
* $O_{\text{target}}$ is the user's custom over/under total.
* $\sigma_m$ (Spread standard deviation) is calibrated to $6.0$.
* $\sigma_t$ (Total standard deviation) is calibrated to $10.0$.

Weights are then normalized so that they sum to the total number of simulated games:

$$\bar{W}_i = W_i \cdot \frac{N}{\sum_{j=1}^N W_j}$$

If the custom spread and over/under match the baseline Vegas market lines, importance weighting is bypassed (all $\bar{W}_i = 1.0$), ensuring the baseline simulation remains completely unbiased.

---

## 3. Weighted Aggregation Mechanics
The normalized weights $\bar{W}_i$ map directly to every game run and all player stat lines associated with that run. All aggregated results returned to the UI are computed using these weights.

### Weighted Player Statistics
For any player metric $X$ (e.g., pass yards, rushing attempts, DraftKings score):
* **Weighted Average**:
  $$\mu_w = \frac{\sum_{i=1}^N \bar{W}_i X_i}{\sum_{i=1}^N \bar{W}_i}$$
* **Weighted Standard Deviation**:
  $$\sigma_w = \sqrt{\frac{\sum_{i=1}^N \bar{W}_i (X_i - \mu_w)^2}{\sum_{i=1}^N \bar{W}_i}}$$
* **Weighted Quantiles (Medians, 5% Floor, 95% Ceiling)**:
  Player percentiles are computed by sorting the values $X$, computing the cumulative sum of their weights $\bar{W}$, and performing linear interpolation to locate the exact quantile thresholds.

---

## 4. Game-Level Outcome Adjustments
The game summary metrics adjust dynamically to the custom lines using the same weights:
* **Win Probability**: Computed as the weighted win rate of both teams across all runs.
* **Cover Rates**: Evaluates how often the weighted runs cover the custom target spread.
* **Score Density**: The histogram showing total score probability distribution uses weighted bins (`np.histogram(..., weights=weights)`), shifting the visual chart to match the user's O/U total expectation.
