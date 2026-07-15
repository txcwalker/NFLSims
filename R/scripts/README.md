# R/scripts/ — One-Time Training & Feature Pipeline Scripts

These scripts were used to build training datasets, run EDA, and train models.
They are **not runtime dependencies** — they do not need to be run again unless
retraining from scratch.

All files follow the current `snake_case_v_0_1_0` naming convention.

## Play Selection Pipeline

| File | Purpose |
|---|---|
| `play_selection_train_v_0_1_0.R` | Trains all play selection bucket models |
| `play_selection_eda_v_0_1_0.R` | EDA for play selection features |
| `play_selection_calibration_v_0_1_0.R` | Calibration analysis |
| `play_selection_eval_v_0_1_0.R` | Model evaluation |
| `play_selection_importance_v_0_1_0.R` | Feature importance plots |
| `play_selection_overall_eval_v_0_1_0.R` | Overall evaluation across all buckets |
| `play_selection_rz_eda_v_0_1_0.R` | Red zone specific EDA |
| `play_selection_test_v_0_1_0.R` | Model testing |

## DNA & Feature Building

| File | Purpose |
|---|---|
| `build_dna_registry_v_0_1_0.R` | Builds full player DNA registry |
| `augment_dna_ngs_v_0_1_0.R` | Augments DNA with NGS tracking data |
| `air_yards_eda_v_0_1_0.R` | Air yards exploratory analysis |

## Data Prep

| File | Purpose |
|---|---|
| `model_1_data_prep.R` | Prepares training data for model 1 |
| `model_1_rolling_stats.R` | Computes rolling stats for model 1 |
| `chaos_model_data_prep.R` | Prepares chaos model training data |
| `chaos_rolling_stats.R` | Rolling stats for chaos model |

> [!NOTE]
> Pre-convention scripts (`advanced_pressure_extraction.R`, `advanced_rushing_extraction.R`)
> have been moved to `legacy/R/scripts/`.
