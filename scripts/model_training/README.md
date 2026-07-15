# Model Training Scripts

This folder contains Python scripts used to train and fit the core machine learning models in the `nfl_sim` engine.

## Scripts & Functions

* **`train_fourth_down_conversion_v_0_1_0.py`**:
  * Fits the statistical model that predicts the probability of successfully converting a fourth down attempt under various game states.
  * Outputs the model parameters used by the fourth-down decision engine.

* **`train_rush_yards_v_0_1_0.py`**:
  * Trains the rushing yards regression model, predicting run play outcomes based on player DNA traits, down, distance, and defensive matchup characteristics.

* **`train_win_probability_v_0_1_0.py`**:
  * Fits the overall game win-probability model using live game status attributes.
