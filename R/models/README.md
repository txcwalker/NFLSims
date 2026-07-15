# R/models/ — Trained Model Artifacts

Contains training scripts and serialized model artifacts (`.rds`) for all R-based models.
Python model artifacts live separately in `src/nfl_sim/models/`.

## Structure

```
models/
├── common/          # Shared: load_models.R
├── field_goal/      # FG success probability model
├── fourth_down/     # 4th down conversion probability model
└── win_probability/ # Win probability model
```

## Models

### `field_goal/`
| File | Description |
|---|---|
| `fg_model.rds` | Trained XGBoost field goal model |
| `fg_levels.rds` | Categorical level encodings |
| `train_fg.R` | Training script (one-time) |

### `fourth_down/`
| File | Description |
|---|---|
| `fd_model.rds` | Trained fourth down decision model |
| `fd_conversion_model.rds` | Conversion probability model |
| `fd_levels.rds` | Categorical level encodings |
| `train_fd_conversion.R` | Training script (one-time) |
| `predict_fd.R` | Inference helper |

### `win_probability/`
| File | Description |
|---|---|
| `wp_model.rds` | Trained win probability model |
| `wp_levels.rds` | Categorical level encodings |
| `train_wp.R` | Training script (one-time) |

> [!NOTE]
> Empty stub files (`load_*.R`, `simulate_fd_decision.R`, `predict_wp.R`) have been removed.
> The `wp_clutch` model and artifacts have been removed — that approach was discontinued.
> Eval output PNGs/TXTs have been removed — evaluations live in `docs/`.
