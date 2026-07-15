# Model Registries (`src/nfl_sim/models/`)

This directory houses the versioned model folders and configuration files loaded by the `ModelRegistry` module.

## Directory Structure

```
src/nfl_sim/models/
├── air_yards_v_0_1_1/               # Sampler files for target depth selections
├── chaos_v_0_1_0/                   # XGBoost models predicting sacks, penalties, and INTs
├── fg_v_0_1_0/                      # Models predicting field goal success by yardline
├── fourth_down_conversion_v_0_1_0/  # Models predicting 4th down conversion rates
├── play_selection_v_0_1_0/          # XGBoost models for situation-based play-calling
├── rush_yards_v_0_1_0/              # Models predicting rushing gains
├── win_probability_v_0_1_0/         # Models predicting live win probabilities
└── yac_model_v_0_1_1/               # Models predicting yards after catch
```

## Model Configurations

Every submodel folder contains:
* **`inference.py`**: The inference interface utilized by the simulation engine to make predictions.
* **`metadata.json`**: Describes features, training dates, metrics, and parameters.
* **`*.joblib` or `*.json`**: The serialized statistical model files.
