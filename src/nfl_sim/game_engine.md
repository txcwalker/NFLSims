# game_engine.py — Vectorized Monte Carlo Simulation Engine

### Why do we need it
To accurately project player fantasy scoring and game outcomes, we need a game engine. It models the core physics and rules of NFL games, including drives, clocks, play-calling, gains, turnovers, and scoring, running thousands of matches simultaneously.

### What is it doing
* **Vector State Manager**: Manages all game states (yardline, down, distance, time remaining, timeouts, scores, possession) using large NumPy arrays.
* **Victory Formation & Spikes**: Vectorially evaluates late-game logic (kneel downs, spikes, timeouts) to simulate realistic late-game pacing.
* **ML Model Integration**: Integrates the machine learning submodels from `ModelRegistry` to predict play-calling, gains, and turnovers.
* **Pre-Snap Penalties**: Simulates pre-snap false starts and offsides using Chaos model coefficients, shifting field locations.
* **4th Down Strategic Decisions**: Models punt, field goal, and go choices based on expected win probability metrics and coach aggressiveness.
* **Play Execution (Pass/Run)**: Runs detailed play scripts, including pass targets, scramble escapes, sacks, air yards depth, completions, interceptions, fumbles, and yards after catch.
* **Possession Switches**: Transfers possession vectorially, managing kickoffs, safeties, turnovers, and touchbacks.

### Subject matter expertise utilized
* **Time to Throw (TTT) Modeling**: Simulates realistic QB pocket timing using normal distributions around player time-to-throw statistics.
* **Redzone & Goal-Line Logic**: Adapts play type selection, air yards, and completion adjustments to field zones (`goalline`, `redzone`, `primary`).
* **Sack Yards Decay**: Uses a customized gamma distribution to simulate sack losses between 1 and 25 yards.

### Decisions made & metrics/reasoning used
* **True Vectorization**: Runs the entire play loop using boolean masking and NumPy arrays of size `N`, avoiding slow nested Python loops.
* **Recency DNA Injection**: Injects player traits and coaching profiles dynamically at run time, allowing user sliders to affect simulations instantly.

### Why this metric vs alternatives
* **Vectorized Array Operations vs Class Objects**: Simulating games using flat NumPy arrays is chosen because it runs thousands of concurrent simulations in seconds, whereas looping through thousands of independent game class objects would take minutes.

### What project goal does this accomplish
Implements the core football physics engine, simulating plays and player actions in memory to generate accurate, high-fidelity matchup projections.
