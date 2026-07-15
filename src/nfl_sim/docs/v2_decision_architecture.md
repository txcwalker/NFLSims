# V2 Engine Decision Architecture (V2.8 Decoupled Identity)

This document outlines the core decision-making components of the high-fidelity V2 NFL Simulation Engine.

## 1. Play Selection (The Decoupled Brain - V2.8)
*   **Mechanism**: 11 situational XGBoost Classification Buckets (Retrained on 2015-2024 Data).
*   **Architecture**: **Decoupled Strategic Intent**. The model handles environmental physics; the engine handles strategic personality.
*   **Input Features (7)**: 
    *   `yardline_100`: Vertical field position.
    *   `game_seconds_remaining`: Overall temporal urgency.
    *   `posteam_timeouts_remaining`: Clock resource availability.
    *   `score_differential`: Current game context.
    *   `leverage_interaction`: `score_diff * seconds_remaining`.
    *   `redzone_interaction`: `distance * yardline_100`.
    *   **Temporal CPOE**: The signal-caller's blended proficiency.
*   **Selection Logic (The Overlay)**:
    1.  **Situational Baseline**: The engine queries the XGBoost bucket for a raw `pass_prob`.
    2.  **Strategic Identity**: The coordinator's `off_coach_PROE` is added manually: `final_prob = baseline_prob + (PROE / 100)`.
    3.  **Rhythm-Blind**: Previous play types have been eliminated from features to ensure situational integrity.
*   **Performance**: Verified at 62% (1st down) to 92% (3rd down) Accuracy on independent 70/15/15 test sets.

## 2. Temporal CPOE (QB Identity)
*   **Legacy (25%)**: QB's Mean CPOE from the *previous* season.
*   **Recency (75%)**: QB's Rolling 4-Game Expanding Mean CPOE from the *current* season.
*   **Logic**: For Week 1, defaults to 100% Legacy. For Weeks 2-4, blends Legacy with available season data.

## 3. Trench & Pocket Physics
*   **Mechanism**: Linear Heuristic.
*   **Variables**: 
    *   `pass_block_tier` (Offense, 1-4)
    *   `pass_rush_tier` (Defense, 1-4)
*   **Pressure Logic**: `Prob = 0.22 + (block_tier - rush_tier) * 0.05`
*   **Sack Logic**: If pressure occurs, flat **18% chance** of a Sack.

## 4. Passing: Target & Depth Selection (The Targeting Brain - V2.5)
*   **Mechanism**: V2.2 Air Yards Regressor + Weighted Top-3 Selection.
*   **The V2.5 EV Formula**: `EV = sqrt(Predicted_Air_Yards) * Catch_Probability`
*   **Decision Logic**:
    1.  **Scan**: The QB evaluates up to 5 targets (2 if under pressure).
    2.  **Scoring**: Each target receives an EV score using the formula above.
    3.  **Sticks Premium**: A **1.2x multiplier** is applied to the score if `predicted_depth >= distance_to_first`.
    4.  **Weighted Selection**: The QB identifies the Top 3 targets and selects one via a **weighted probability distribution** (`Option_Score / Sum_of_Top_3_Scores`). This simulates progression-scanning and human variance.

## 5. Passing: Resolution
*   **Catch Check**: Random roll vs. Receiver `catch_rate` trait.
*   **YAC Mechanism**: V2.2 YAC Regressor.
*   **Input Features (7)**: `yardline_100`, `down`, `distance`, `air_yards`, `avg_separation`, `secondary_tier`, `pm_idx`.

## 6. Rushing: Resolution
*   **Carrier Selection**: Weighted random choice using `carry_share` traits.
*   **Mechanism**: V2.2 Rushing Regressor.
*   **Input Features (10)**: 
    *   `yardline_100`, `distance`, `time_remaining`, `score_diff`, `rolling_ypc`, `block_z`, `yac_z`, `btk_z`, `box_z`, `eff_z`.

## 7. 4th Down Decisions (The Aggression Engine - V2.4)
*   **Mechanism**: situational XGBoost 3-Way Classifier (Punt vs. FG vs. Go).
*   **Strategic Bias**: The coach's historical `Aggression_Index` (from the Strategic Atlas) is added directly to the "Go" probability.
*   **Selection**: The engine selects the option with the highest final biased probability.
*   **Field Goal Resolution**: 
    *   **Mechanism**: V2.1 XGBoost Accuracy Model.
    *   **Inputs**: `kick_distance`, `is_dome`, `wind_speed`.
    *   **Logic**: If missed, possession switches at the spot of the kick (or 20y).

## 8. Game State & Special Teams
*   **Kickoff**: Touchback to the 25-yard line.
*   **Clock Runoff**: `28 to 38` seconds per play (Standard).
