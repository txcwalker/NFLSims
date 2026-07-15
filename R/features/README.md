# R/features/ — Feature Engineering

Feature cleaning and transformation utilities used during model training.

| File | Purpose |
|---|---|
| `cleaners.R` | Cleans raw pbp data: removes ties, creates `Winner` binary label, drops invalid rows. Used by WP training pipeline. |

> [!NOTE]
> `weather_impute.R` was removed — weather features are no longer used in any model.
