# backtest_weights.py — Blending Evaluation Tool

### Why do we need it
To produce realistic game simulations, player DNA (e.g. carry share, target share, efficiency) must blend long-term full-season baselines with short-term (last 4 games, or L4) recency indicators. This tool validates which blending weights generate the lowest forecast error against actual game outcomes.

### What is it doing
* **Actuals Ingestion**: Pulls real-world play-by-play data from a historical game (e.g., Week 14, 2023 BUF at KC) to compute the actual fantasy scores for all active skill players.
* **Weight Gridding**: Sweeps through a grid of blending weights (e.g., 100/0, 75/25, 50/50, 25/75, 0/100 season-to-L4 ratios).
* **Grid Simulation**: Builds temporary team trait files for each weight configuration, executes a batch of game simulations, and records projected player fantasy averages.
* **Error Measurement**: Calculates the Mean Absolute Error (MAE) between simulated averages and historical fantasy scores, compiling a performance report.

### Subject matter expertise utilized
* **Historical Validation**: Backtests against specific, high-leverage matchups where playcalling and targets had real-world shifts due to depth-chart alterations.
* **Error Minimization**: Utilizes MAE to prevent large individual outlier errors (e.g. a backup QB taking surprise snaps) from disproportionately skewing the evaluation.

### Decisions made & metrics/reasoning used
* **Game Count Boundary**: Set iteration count to 50 games per grid point to maintain rapid execution speed during backtests.
* **Overlap Filtration**: Only computes error metrics for players who registered actual statistics in the real-world game and were successfully mapped in the simulation roster.

### Why this metric vs alternatives
* **Mean Absolute Error (MAE) vs RMSE**: Selected MAE over Root Mean Squared Error (RMSE) to represent the literal, average point difference a fantasy player can expect their projection to be off by, which matches standard DFS validation practices.

### What project goal does this accomplish
Guarantees that the rolling player DNA generator uses statistically validated, optimal weights to blend baseline capabilities with active recency shifts.
