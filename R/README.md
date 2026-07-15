# R/ — R Language Components

This directory contains all R-language code for the NFL simulation project.
The Python daemon (`src/live/`) and R components work together as one live pipeline.

## Directory Structure

```
R/
├── bots/           # Live game pipeline — fetching, simulation dispatch, posting
├── core/           # Shared helper functions sourced by other modules
├── data_ingest/    # (Minimal) data loading utilities
├── evaluation/     # (Reserved) model evaluation scripts
├── features/       # Feature engineering and cleaning utilities
├── live/           # ESPN data adapter (espn_adapter.R)
├── logs/           # Runtime log output (fourth_down/)
├── models/         # Trained model artifacts + training scripts
│   ├── common/     # Shared model loader
│   ├── field_goal/
│   ├── fourth_down/
│   └── win_probability/
├── outputs/        # (Reserved) output artifacts
├── reports/        # (Reserved) reporting scripts
├── scripts/        # One-time training and feature pipeline scripts
├── simulators/     # fourth_down/ — live decision engine
└── tests/          # (Reserved) test harnesses
```

## Live Pipeline Flow

```
src/live/main.py
  → fetches ESPN plays via espn_adapter.py / R/live/espn_adapter.R
  → passes game state to src/live/simulator_bridge.py
  → spawns R/simulators/fourth_down/run_one_sim.R
  → which calls R/simulators/fourth_down/fourth_down_decision.R
  → result JSON returned to Python → posted via src/live/post_targets.py
```

## Naming Convention

Current scripts follow `snake_case_v_0_1_0` versioned naming.
Legacy files have been moved to `legacy/R/`.
