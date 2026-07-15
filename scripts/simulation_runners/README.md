# Simulation Runners

This folder contains local scripts used to interface with the core simulator engine in `src/nfl_sim/` to run matches, full seasons, and Monte Carlo batches.

## Scripts & Functions

* **`run_formal_sim_v_0_1_0.py`**:
  * Simulates a single, high-fidelity matchup (default BUF vs KC) using the full game engine and outputs detailed play-by-play log CSVs, box scores, and fourth-down decision histories.

* **`run_full_season_v_0_1_0.py`**:
  * Orchestrates full-season simulations across the entire NFL schedule week-by-week, aggregating fantasy projection logs and game summary CSVs.

* **`run_monte_carlo_v_0_1_0.py`**:
  * Executes large-scale Monte Carlo batches (e.g. 5,000 to 10,000 iterations) for a specific matchup, reporting average scores, cover rates, win probabilities, and outcome densities.
