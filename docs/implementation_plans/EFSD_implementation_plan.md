# EFSD Implementation Plan — Expected Final Score Differential

This plan outlines the design, training, validation, and integration of the **Expected Final Score Differential (EFSD)** metric. Unlike KEP, which inverts a Win Probability model and is subject to artificial bounds ($\pm 24$), EFSD directly predicts the expected final margin of the game (offense score minus defense score at game end) from the current situational features.

---

## Model Design & Data Preparation

### 1. Data Source and Play Selection
Data is sourced via the **`nfl_data_py`** Python wrapper (the nflfastR Python interface). We will read from `data/processed/hardened_pass_training_master_v2_5.csv` and include **all play types** (not just scrimmage runs/passes) to capture the continuous nature of the game.

> **Data Verification Step**: Before training, assert that the required columns (`game_seconds_remaining`, `posteam_score`, `defteam_score`, `home_opening_kickoff`, `home_score`, `away_score`) are present in the loaded DataFrame. These columns are expected to exist based on prior use of this dataset, but a pre-flight assertion will catch any schema drift early and fail loudly rather than silently producing wrong targets.

* **Included Play Types**: `pass`, `run`, `kickoff`, `punt`, `field_goal`, `extra_point`, `qb_kneel`, `qb_spike`.
* **Target Variable**: `final_score_differential_posteam = final_posteam_score - final_defteam_score`.
  * `final_posteam_score = home_score if posteam == home_team else away_score`
  * `final_defteam_score = away_score if posteam == home_team else home_score`
* **Feature Engineering & Cleaning**:
  * `down`: Fill nulls with `0` (represents kickoffs, extra points, and non-down situations).
  * `ydstogo`: Fill nulls with `0` (non-scrimmage situations).
  * `yardline_100`: Drop rows where null.
  * `posteam_timeouts_remaining` / `defteam_timeouts_remaining`: Fill nulls with `3`.
  * `receive_2h_ko`:
    * Map the `home_opening_kickoff` column to identify the team receiving the 2nd half kickoff: `receive_2h_ko_team = home_team if home_opening_kickoff == 1 else away_team`.
    * Set `receive_2h_ko = 1.0` if `game_seconds_remaining > 1800` (1st half) and `posteam == receive_2h_ko_team`, else `0.0`.
    * **Overtime handling**: The NFL's current overtime format (sudden death with a mandatory first possession for each team) uses a coin flip that cannot be known in advance. For OT plays (`game_seconds_remaining ≤ 0` or OT period flags), set `receive_2h_ko = 0.5` to represent the 50/50 possession uncertainty. This treats OT neutrally rather than making a false assignment.

### 2. Features (8)
* `score_differential` (current score differential of the play: `posteam_score - defteam_score`)
* `game_seconds_remaining`
* `down`
* `ydstogo`
* `yardline_100`
* `posteam_timeouts_remaining`
* `defteam_timeouts_remaining`
* `receive_2h_ko`

---

## Training & Evaluation Workflow

We will establish a rigorous train-validation-test split to prevent data leakage and evaluate generalization performance.

### 1. Data Splits
* **Training Set**: Seasons 2020–2023 (further split 85/15 into train and validation for early stopping).
* **Test Set**: Season 2024 (fully out-of-sample holdout).

### 2. Model Configuration
* **Algorithm**: XGBoost Regressor (`xgb.XGBRegressor`).
* **Hyperparameters**:
  * Objective: `reg:squarederror`
  * Learning rate: `0.05`
  * Estimators: `500`
  * Early stopping: `20` rounds
  * Max depth: `6` (to capture complex score-differential, clock, and timeout interactions)
  * Early stopping: `50` rounds
  * Tree method: `hist`

### 3. Model Performance Evaluation
We will log the following metrics on both the training and out-of-sample test splits:
* **Root Mean Squared Error (RMSE)** & **Mean Absolute Error (MAE)**.
* **$R^2$ Score**: Measure the proportion of variance in final score differentials explained by the game state.
* **Time-Bucket RMSE & $R^2$ (by Quarter)**: Evaluate model accuracy and explained variance grouped by quarters ($1, 2, 3, 4$) to analyze how performance changes as the game clock expires.
* **Sign Accuracy**: Calculate how often the sign of the predicted EFSD matches the sign of the actual final margin (measuring implicit winner prediction accuracy) overall and by quarter.
* **Correlation with EPA**: Measure the correlation ($r$) between the change in predicted EFSD ($\text{EFSD}_t - \text{EFSD}_{t-1}$) and the play's actual `epa` value from the dataset, validating the model's play-level value mapping.

### 4. Diagnostic & Calibration Plots
We will generate and save diagnostic plots to `src/nfl_sim/models/efsd_v_0_1_0/`:
1. **Calibration Curve (`efsd_calibration.png`)**:
   * Bucket predicted EFSD into 2-point bins.
   * Plot Mean Actual final margin vs. Mean Predicted EFSD to verify unbiased points calibration (along the $y = x$ diagonal).
2. **Time-Decay Analysis (`efsd_time_decay.png`)**:
   * Plot RMSE and Sign Accuracy across game seconds remaining to visualize how prediction error decreases and winner prediction accuracy increases toward $100\%$ as the game ends.
3. **EPA Correlation Scatter (`efsd_epa_correlation.png`)**:
   * Plot $\Delta\text{EFSD}$ vs. `epa` to visually confirm alignment with traditional EPA.
4. **Feature Importance (`efsd_feature_importance.png`)**:
   * Plot XGBoost gain-based feature importances for all 8 input features.
   * Provides a quick diagnostic for whether the model is over-relying on any single feature (e.g., `score_differential` dominating and drowning out clock/timeout signal).

---

## Proposed Code Changes

> **Scope note**: EFSD is **not integrated into the play-by-play simulator**. It is used exclusively by the positional evaluator and line-suggestion tooling. The `model_registry.py` singleton cache is for sim-side models only and does not apply here. `EFSDModelV010` manages its own booster load on initialization within the evaluator.

### 1. Model Component (`src/nfl_sim/models/efsd_v_0_1_0/`)

#### [NEW] [train_efsd.py](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/models/efsd_v_0_1_0/train_efsd.py)
* Loads dataset, executes data cleaning/filling, constructs the target and features, trains the model, computes evaluation metrics (RMSE, MAE, $R^2$), calculates advanced metrics (Time-Bucket, Sign Accuracy, EPA Correlation), and generates the diagnostic plots.
* Saves `efsd_model.json` (model weights) and `metadata.json` (metadata + metrics).

#### [NEW] [efsd_inference.py](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/models/efsd_v_0_1_0/efsd_inference.py)
* Class `EFSDModelV010` wraps the booster file.
* Exposes `predict_efsd(...)` for scalar evaluations and `predict_batch(...)` for fast vectorized calculations.

### 2. Simulator & Evaluator Updates (`src/nfl_sim/`)

#### [MODIFY] [nfl_positional_evaluator.py](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/nfl_positional_evaluator.py)
* Load `EFSDModelV010` in `PositionalEvaluator`.
* Implement `_drive_end_efsd(self, score_diff_off, game_sec, terminal, off_to, def_to)`:
  * Non-terminal: returns `-self.efsd_model.predict_batch(opp_kickoff_state)`.
  * Terminal: returns `score_diff_off` (exact final margin).
* Update `evaluate()` to compute and return `efsd_start` and per-concept `mean_efsd` and `delta_efsd`.
* Update `evaluate_one_step()` to support EFSD:
  * Mid-drive: `efsd_model.predict_batch(...)`.
  * Scored/Terminal: `_drive_end_efsd(...)`.
  * Turnover: `-efsd_model.predict_batch(...)` from turnover yardline.
* Update `suggest_lines(self, ..., metric="kep")`:
  * Add support for `metric="efsd"`, which ranks concepts and chains lines based on EFSD deltas instead of KEP.

### 3. API Enhancements (`src/api/app.py`)

* **Update `/api/positional-evaluator`**: Return `efsd` alongside `ep` and `kep`, and concept-specific `delta_efsd`.
* **Update `/api/games/{game_id}/positional-eval`**: Include `efsd` per play.
* **Update `/api/historical/plays/{game_id}`**: Include `efsd_off` and `home_efsd` per play.
* **Update `/api/historical/suggest-lines`**: Accept `metric: str = Query("kep")` and pass to `suggest_lines()`.

### 4. Frontend Comparison Interface (`frontend_analysis/`)

#### [MODIFY] [HistoricalLab.jsx](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/frontend_analysis/src/pages/HistoricalLab.jsx)
* **Timeline Charts**: Render KEP and EFSD as **separate stacked charts** rather than overlaid lines on a shared axis. KEP is bounded (~±24) while EFSD is unbounded (blowouts can exceed ±30), so a shared y-axis would compress one or distort the other. Two independent charts with their own y-scales prevent this and make each metric independently readable.
* **Selected Play Details**: Show KEP and EFSD side-by-side in the play detail panel.
* **Suggested Lines Panel**: Add a metric toggle `[ KEP ] [ EFSD ]` and fetch suggestions dynamically using the selected metric.

---

## Verification Plan

### Automated Tests
1. **Model Validation**:
   - Verify `train_efsd.py` prints test set metrics ($R^2$, RMSE, MAE) and generates the calibration plot.
   - Verify `efsd_inference.py` passes scalar and batch prediction sanity checks.
2. **Evaluator Unit Tests**:
   - Create tests in `tests/test_efsd_evaluator.py`:
     - Assert EFSD symmetry: changing possession flips the expected margin sign.
     - Assert correct behavior on terminal outcomes.
     - Assert `suggest_lines(metric="efsd")` runs successfully.

### Manual Verification
1. Curl suggest-lines endpoint with `metric=efsd`.
2. Open the Historical Lab UI and verify that:
   - The dual-line chart plots both KEP and EFSD.
   - The toggle successfully updates the suggested lines using EFSD.
