# NFL Simulation Package (`src/nfl_sim/`)

This package contains the core physics simulation engine, statistical model loader, play selector, fantasy scoring post-processors, and graph visualization tools.

## Directory Structure

```
src/nfl_sim/
├── README.md               # This documentation file
├── batch.py                # Coordinates sequential and vectorized Monte Carlo simulation runs
├── batch.md                # Documentation for batch.py
├── game_engine.py          # Vectorized physics game engine simulating plays concurrently
├── game_engine.md          # Documentation for game_engine.py
├── model_registry.py       # Registry loading machine learning submodels (YAC, Chaos, FG)
├── model_registry.md       # Documentation for model_registry.py
├── proe_overlay_v_0_1_0.py # Calculates Pass Rate Over Expected logit-space adjustments
├── proe_overlay_v_0_1_0.md # Documentation for proe_overlay_v_0_1_0.py
├── scoring.py              # Translates player stats into DraftKings/FanDuel fantasy scores
├── scoring.md              # Documentation for scoring.py
├── script_chainer.py       # Geographical play pool sampler for simulation fallbacks
├── script_chainer.md       # Documentation for script_chainer.py
├── utils.py                # Standardizes player and coach names using clean formats
├── utils.md                # Documentation for utils.py
├── visuals.py              # Visualizes fantasy averages, point spreads, and score distributions
├── visuals.md              # Documentation for visuals.py
├── docs/                   # Playbook audits and decision architecture notes
└── models/                 # Model registry directories for specific submodel configurations
```

## Description of Files

* **[`batch.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/batch.py)**:
  * Manages large Monte Carlo batches. It loads roster DNA, matches slots, and aggregates stats.

* **[`game_engine.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/game_engine.py)**:
  * Implements the core play-by-play physics game simulation using highly optimized numpy arrays.

* **[`model_registry.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/model_registry.py)**:
  * Loads and interfaces machine learning submodels (YAC, air yards, rushing yards, chaos, field goals).

* **[`proe_overlay_v_0_1_0.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/proe_overlay_v_0_1_0.py)**:
  * Applies Bayesian-blended coaching PROE values in logit space.

* **[`scoring.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/scoring.py)**:
  * Processes fantasy points under DraftKings and FanDuel scoring rules.

* **[`script_chainer.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/script_chainer.py)**:
  * Samples starting conditions and play scenarios from a historical game play pool.

* **[`utils.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/utils.py)**:
  * Standardizes player and coach names (e.g., player initial abbreviation, coach capitalization).

* **[`visuals.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/visuals.py)**:
  * Produces plots of simulated fantasy distributions, point spreads, and combined game scores.
