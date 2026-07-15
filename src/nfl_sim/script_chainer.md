# script_chainer.py — Geographical Scenario Sampler

### Why do we need it
When models are missing or the engine encounters game states that lack predictive coverage, it needs a fallback mechanism. This class samples realistic play types, gains, and outcomes from a historical starting pool to maintain continuous play simulation.

### What is it doing
* **Pool Classification**: Ingests a historical play pool CSV (`data/starting_position_pool.csv`) and separates plays into two groups based on field geography: redzone plays (`yardline_100 <= 20`) and field plays (`yardline_100 > 20`).
* **Scenario Sampling**: Pulls a random, complete historical play record from the appropriate geographic pool based on the game's current yardline location.
* **Fallback Simulation**: Returns simulated outcomes (play type, yards gained, turnover flags, penalty flags) based on actual historical results when ML models are unavailable.

### Subject matter expertise utilized
* **Redzone Variance Gating**: Separates the starting pool into redzone and open-field segments, ensuring the engine does not sample open-field long gains when a team is backed up on the goal-line.

### Decisions made & metrics/reasoning used
* **Heuristic Defaults**: Falls back to a 55% pass rate and a default 5-yard gain if the starting pool CSV file is missing.

### Why this metric vs alternatives
* **Geographical Historical Pool vs Generic Random Walk**: Sampling from a structured pool of historical plays is superior to a generic random walk because it ensures that yardage gains and turnover distributions follow the actual boundaries and statistical characteristics of real NFL games.

### What project goal does this accomplish
Provides a fallback mechanism that keeps simulations realistic and stable even when model boundaries are exceeded.
