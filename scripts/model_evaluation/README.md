# Model Evaluation & Diagnostic Tools

This folder contains diagnostic script audits that help verify submodel behavior, analyze feature importances, and check play-by-play mechanics.

## Scripts & Functions

* **`inspect_2nd_short_v_0_1_0.py`**:
  * Evaluates XGBoost submodel predictions specifically on 2nd-and-short plays to diagnose any unexpected pass/run skew or regression bias.

* **`inspect_interactions_v_0_1_0.py`**:
  * Assesses model metrics and behavior when interaction variables (such as momentum, leverage, and redzone factors) are active.

* **`view_game_results_v_0_1_0.py`**:
  * Summarizes simulated game statistics and outcomes to ensure overall yards/scores align with historical baselines.

* **`view_game_timing_v_0_1_0.py`**:
  * Audits clock management, tempo, and play intervals to verify timing consistency throughout simulated halves.
