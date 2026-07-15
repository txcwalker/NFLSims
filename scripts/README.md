# NFL Sims — Scripts Directory

This directory contains standalone Python automation, utility, training, and execution scripts organized into thematic subdirectories.

## Directory Structure

```
scripts/
├── model_training/       # Scripts to train core model components (e.g. 4th Down, Rushing, Win Prob)
├── roster_management/    # Scripts to build, update, compile, and validate roster player DNA traits
├── model_evaluation/     # Script audits to analyze model performance, interaction variables, and timing
├── simulation_runners/   # Local runners to execute individual games, full seasons, and Monte Carlo sims
├── eda/                  # Exploratory Data Analysis scripts (e.g. scoring efficiency diagnostics)
├── data_utils/           # Maintenance scripts for directories and dataset cleaning
└── play_by_play_runner.py # Main entry point runner to execute play-by-play workflows
```

## Subdirectories Overview

### 1. [Model Training](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/scripts/model_training)
Contains training scripts for specific statistical models:
* `train_fourth_down_conversion_v_0_1_0.py`: Fits the fourth-down conversion classification model.
* `train_rush_yards_v_0_1_0.py`: Fits the rushing yards regression model.
* `train_win_probability_v_0_1_0.py`: Fits the game win probability model.

### 2. [Roster Management](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/scripts/roster_management)
Tools to ingest, align, and validate team rosters and player DNA profiles:
* `build_full_name_dna.py`: Generates complete player DNA profiles from seasonal statistics.
* `compileRosters.js`: Ingests and compiles raw team roster JSON structures.
* `update_rosters_full_name.py`: Re-maps roster entries to full-name formats.
* `validate_rosters.py`: Checks for consistency and completeness across active rosters.

### 3. [Model Evaluation](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/scripts/model_evaluation)
Auditing tools to inspect predictions and model mechanics:
* `inspect_2nd_short_v_0_1_0.py`: Tests XGBoost submodel behavior on 2nd-and-short situations.
* `inspect_interactions_v_0_1_0.py`: Analyzes the impact of interaction variables on game predictions.
* `view_game_results_v_0_1_0.py`: Parses and summarises simulated game outcomes.
* `view_game_timing_v_0_1_0.py`: Audits simulation play timing and clock-management behavior.

### 4. [Simulation Runners](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/scripts/simulation_runners)
Local interfaces to run simulations using the `src/nfl_sim/` engine:
* `run_formal_sim_v_0_1_0.py`: Executes a high-fidelity game simulation (e.g. BUF vs KC).
* `run_full_season_v_0_1_0.py`: Simulates a complete multi-week league season.
* `run_monte_carlo_v_0_1_0.py`: Runs a large Monte Carlo batch simulation of a matchup.

### 5. [EDA](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/scripts/eda)
Exploratory research tools:
* `diagnose_scoring_efficiency.py`: Diagnoses tempo, drive pacing, and scoring metrics.

### 6. [Data Utils](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/scripts/data_utils)
Housekeeping and data pipelines maintenance:
* `cleanup_data_dir.py`: Safely archives stale training data splits or raw files.
