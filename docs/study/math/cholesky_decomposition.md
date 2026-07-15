# Cholesky Decomposition — Technical Study Notes

## The Problem We're Solving

In our DFS optimizer, we need to generate **correlated random scores** for players. If Josh Allen is sampled at a monster game (95th percentile), Stefon Diggs should be more likely to also draw a high score — because their outcomes are positively correlated in reality.

The naive approach of sampling each player's score independently ignores these correlations and produces unrealistic game scenarios (e.g., Allen has his best game but Diggs has his worst). Cholesky decomposition gives us the mathematical tool to sample *jointly correlated* random variables correctly.

---

## Background: The Covariance Matrix

Before Cholesky, you need to understand the **covariance matrix** (Σ, read "sigma").

For N players, Σ is an N×N matrix where:
- **Diagonal entry** Σ_ii = variance of player i's score (how spread out their outcomes are)
- **Off-diagonal entry** Σ_ij = covariance between player i and player j (do they tend to go up/down together?)

Covariance and correlation are related:
$$\rho_{ij} = \frac{\Sigma_{ij}}{\sigma_i \sigma_j}$$

Where ρ (rho) is the correlation coefficient (−1 to +1) and σ_i is the standard deviation of player i.

**Example for 3 players** (QB, WR1, opposing DST):
$$\Sigma = \begin{pmatrix} \sigma_{QB}^2 & \rho_{QB,WR1} \cdot \sigma_{QB}\sigma_{WR1} & \rho_{QB,DST} \cdot \sigma_{QB}\sigma_{DST} \\ \rho_{QB,WR1} \cdot \sigma_{QB}\sigma_{WR1} & \sigma_{WR1}^2 & \rho_{WR1,DST} \cdot \sigma_{WR1}\sigma_{DST} \\ \rho_{QB,DST} \cdot \sigma_{QB}\sigma_{DST} & \rho_{WR1,DST} \cdot \sigma_{WR1}\sigma_{DST} & \sigma_{DST}^2 \end{pmatrix}$$

Where ρ_QB,WR1 = +0.6 (positive, they score together), ρ_QB,DST = −0.4 (negative, one up = other down).

---

## What Is Cholesky Decomposition?

Cholesky decomposition finds a **lower triangular matrix L** such that:
$$\Sigma = L \cdot L^T$$

**Lower triangular** means L has zeros above the diagonal:
$$L = \begin{pmatrix} l_{11} & 0 & 0 \\ l_{21} & l_{22} & 0 \\ l_{31} & l_{32} & l_{33} \end{pmatrix}$$

This is analogous to finding the "square root" of a matrix. Just as $9 = 3 \times 3$, Cholesky finds L such that $\Sigma = L \times L^T$.

### Why Does This Help?

The key insight: if **Z** is a vector of **independent** standard normal random variables (each drawn separately, zero correlation), then:

$$X = \mu + L \cdot Z$$

...gives a vector **X** of random variables that are **correlated** exactly according to Σ, with means μ.

In plain English: you sample N independent random numbers → multiply by L → get N correlated random numbers that respect your covariance structure.

---

## Step-by-Step Algorithm

**Given**: Covariance matrix Σ (N×N, symmetric, positive definite)

**Find**: Lower triangular L such that Σ = LL^T

**Algorithm** (Cholesky-Banachiewicz):

$$L_{jj} = \sqrt{\Sigma_{jj} - \sum_{k=1}^{j-1} L_{jk}^2}$$

$$L_{ij} = \frac{1}{L_{jj}} \left( \Sigma_{ij} - \sum_{k=1}^{j-1} L_{ik} L_{jk} \right) \quad \text{for } i > j$$

**In practice, you never compute this by hand** — NumPy/SciPy handle it:

```python
import numpy as np

# Build correlation matrix for N players
rho = np.eye(N)  # Start with identity (all uncorrelated)
rho[qb_idx, wr1_idx] = 0.6   # QB ↔ WR1 positive correlation
rho[wr1_idx, qb_idx] = 0.6   # Matrix must be symmetric
rho[qb_idx, dst_idx] = -0.4  # QB ↔ opposing DST negative
rho[dst_idx, qb_idx] = -0.4

# Convert correlation → covariance using player std devs
sigma = np.diag(std_devs)                # diagonal matrix of std devs
cov = sigma @ rho @ sigma                # Σ = D * ρ * D

# Cholesky decomposition
L = np.linalg.cholesky(cov)

# Sample correlated scores for one lineup iteration
z = np.random.standard_normal(N)         # Independent draws
correlated_scores = medians + L @ z      # Shift to player medians
```

---

## Visual Intuition

Imagine you're throwing darts at a 2D target:

**Independent sampling** (no correlation):
```
• • •   •
  •   •
•   •   •
  •   •
```
Dots spread uniformly in all directions — no relationship between X and Y positions.

**Positively correlated sampling** (via Cholesky):
```
        • •
      • •
    • •
  • •
• •
```
Dots tend to move together along a diagonal — if X is high, Y tends to be high too.

**Negatively correlated sampling**:
```
• •
  • •
    • •
      • •
        • •
```
Opposite diagonal — if X is high, Y tends to be low.

This is exactly the relationship between QB score and opposing DST score.

---

## Where Does Cholesky Appear in Industry?

| Industry | Application |
|---|---|
| **Finance** | Generating correlated asset return scenarios for portfolio risk modeling (Monte Carlo VaR) |
| **Options Pricing** | Multi-asset options require correlated stock price paths (basket options, rainbow options) |
| **Insurance** | Modeling correlated catastrophe losses across geographic regions |
| **Machine Learning** | Gaussian Processes (a key probabilistic model) use Cholesky for efficient likelihood computation |
| **Physics** | Generating spatially correlated noise fields in simulations |
| **DFS** | Generating correlated player score draws that respect offensive correlation structure |

The finance application is almost identical to ours: a portfolio manager running 10,000 Monte Carlo scenarios for a multi-stock portfolio needs to ensure that a "market crash" scenario pulls all correlated stocks down together, not just one at a time. Same math, same Cholesky, different domain.

---

## How We Derive Correlation Values in Our Project

This is the key part: **we don't hardcode correlations — we estimate them from our simulation output**.

Our game engine already runs thousands of iterations where each player gets a score per trial. We can compute the **empirical correlation** directly:

```python
import numpy as np

# sim_scores shape: (n_trials, n_players)
# Each row is one sim trial, each column is one player
sim_scores = load_sim_results()  # from our batch runner

# Compute empirical correlation matrix
rho_empirical = np.corrcoef(sim_scores.T)
# rho_empirical[i, j] = correlation between player i and player j across all trials
```

The sim already captures:
- QB and his receivers going off in the same trials (positive correlation)
- Defense having a good game when the opposing offense struggles (negative correlation)
- Independent games showing near-zero correlation between players

This means our correlation matrix is **data-driven from our own model**, not arbitrary constants. It's a direct output of the sim that feeds the optimizer — closing the loop between simulation and optimization.

### Fallback: Structural Correlations

If sim data isn't available (e.g., user hasn't run sims yet), we fall back to **structural correlations** based on team/position relationships:

| Relationship | Default ρ |
|---|---|
| QB ↔ WR1 (same team) | +0.65 |
| QB ↔ WR2 (same team) | +0.45 |
| QB ↔ TE (same team, pass-heavy) | +0.40 |
| QB ↔ RB (same team) | +0.15 |
| QB ↔ opposing DST | −0.40 |
| WR1 ↔ WR2 (same team) | +0.30 |
| Players on different teams, different games | ~0.0 |

These are rough but directionally correct estimates used when no sim data is loaded.

---

## Key Properties Required for Cholesky to Work

The covariance matrix Σ must be:
1. **Symmetric**: Σ_ij = Σ_ji (correlation between i and j equals correlation between j and i)
2. **Positive semi-definite**: All eigenvalues ≥ 0 (intuitively: no paradoxical correlations)

If the empirical correlation matrix from our sims is nearly-but-not-quite positive definite (due to numerical noise), we apply **nearest positive definite projection** (scipy has a function for this):

```python
from scipy.linalg import sqrtm
# Higham's algorithm for nearest PD matrix
def nearest_pd(A):
    B = (A + A.T) / 2
    _, s, V = np.linalg.svd(B)
    H = np.dot(V.T, np.dot(np.diag(s), V))
    return (B + H) / 2
```

---

## Summary

| Concept | One-Liner |
|---|---|
| Covariance Matrix | Captures how much player scores move together |
| Cholesky Decomposition | Finds the "square root" of the covariance matrix |
| Correlated Sampling | Multiply independent draws by L to get correlated draws |
| Why it matters for DFS | Realistic game scenarios where stacks go off together |
| Where correlations come from | Empirically from our sim output (preferred) or structural defaults |
