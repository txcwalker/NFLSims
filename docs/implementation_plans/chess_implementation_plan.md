# NFLSims: Chess-Style Football Position Evaluator (V1)

This plan outlines the design and implementation of a chess-style football position evaluator. It translates raw game states and play choices into point-equivalent positional advantages (KEP and EP) by running drive-long simulations from the live state, categorizing play outcomes into granular tactical concepts, and analyzing their performance.

---

## 📐 Mathematical & Conceptual Framework

### 1. The Point-Equivalent Metrics
To evaluate positions, we will expose two primary point-equivalent values:
- **Expected Points (EP):** Purely situational value of the current drive (down, distance, yardline), independent of score and clock. Used to evaluate immediate field position value and drive expectations.
- **Kickoff Equivalent Points (KEP):** The score differential at kickoff ($t=3600$, $down=1$, $distance=10$, $yardline\_100=75$) that matches the current Win Probability ($WP$).
  $$KEP = WP_{kickoff}^{-1}(WP_{current})$$
  This acts as our clock-aware positional security metric (the "centipawn" evaluation of the game).

### 2. Granular Play Classification & Evaluation
Rather than grouping results by generic "Pass vs. Run" play calls, we will analyze the simulator's first play execution variables to classify outcomes into granular concepts:
1. **Run Drive Simulations:** Start the simulator at the current live game state and simulate the drive to completion (change of possession).
2. **Classify First Play Executed:** For each simulation run, inspect the attributes of the **first play** to classify it into a tactical concept:
   - **Run:** Standard rush play (structured to easily split into Run Left/Center/Right when run direction features are added to the simulator).
   - **Screen Pass:** Pass play where `air_yards <= 0` (or `air_yards < 3` with yards after catch).
   - **Short Pass:** Pass play where $0 < \text{air\_yards} < 10$.
   - **Medium Pass:** Pass play where $10 \le \text{air\_yards} < 20$.
   - **Deep Pass:** Pass play where $\text{air\_yards} \ge 20$.
3. **Evaluate Outcome States:** Convert the final drive-ending state (score change and elapsed time) of each simulation run into its corresponding KEP.
4. **Calculate Expected KEP by Concept:** Average the KEPs of the drive outcomes within each classified concept group.
   $$\text{Expected Value}(Concept) = \text{mean}(KEP_{\text{concept\_outcomes}})$$
5. **Assign Point Advantage Delta:** We assign each tactical concept a point-equivalent value based on how that choice performed:
   $$\Delta KEP(Concept) = \text{Expected Value}(Concept) - KEP_{start}$$
   This allows the user to see the exact value of calling a Deep Pass vs. a Screen Pass or Run in the current situation.

---

## 🛠️ Proposed Changes

### 1. Backend Models

#### [NEW] [positional_ep_inference.py](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/models/positional_ep_v_0_1_0/positional_ep_inference.py)
Create the Expected Points inference wrapper:
- Loads the trained `positional_ep_model.json`.
- Implements `predict_expected_points(yardline_100, down, ydstogo, goal_to_go)` returning a float.

#### [NEW] [train_positional_ep.py](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/models/positional_ep_v_0_1_0/train_positional_ep.py)
Create a script to train the EP model:
- Reads the play-by-play historical dataset `data/processed/hardened_pass_training_master_v2_5.csv`.
- Trains an `XGBRegressor` on features `['yardline_100', 'down', 'ydstogo', 'goal_to_go']` targeting the `ep` column.
- Serializes the model to `positional_ep_model.json`.

#### [NEW] [nfl_positional_evaluator.py](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/nfl_positional_evaluator.py)
Create the core evaluation manager:
- Computes KEP using a pre-calculated kickoff WP grid.
- Runs drive-long simulations, classifies the first play's executed concept (Screen, Short, Medium, Deep, Run), and aggregates drive outcome KEPs to determine the point deltas for each play type.

---

### 2. Backend API Service

#### [MODIFY] [app.py](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/api/app.py)
- Register `PositionalExpectedPointsModel` on startup.
- Add endpoint `GET /api/positional-evaluator`:
  - Accepts down, distance, yardline, clock, score diff, timeouts.
  - Returns `ep`, `kep`, and recommended play concepts.
- Add endpoint `GET /api/games/{game_id}/positional-eval`:
  - Fetches the active play stream of a game and returns the sequence of play-by-play KEP evaluations.

---

### 3. Frontend Pages

#### [MODIFY] [InDevelopment.jsx](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/frontend/src/pages/InDevelopment.jsx)
- Connect sliders to `/api/positional-evaluator` and render dynamic KEP, EP, and optimal moves.

#### [MODIFY] [GameSummary.jsx](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/frontend_analysis/src/pages/GameSummary.jsx)
- Fetch play evaluations from `/api/games/{game_id}/positional-eval` and render the point-equivalent evaluation bar.

---

## 🧪 Verification Plan

### Automated Verification
- Create `tests/test_positional_evaluator.py` to assert that:
  - EP increases as yardline approaches the goal line.
  - KEP equals score differential when time is remaining at kickoff.
  - Expected KEP behaves reasonably across different downs/distances and play classifications.

### Manual Verification
- Verify backend endpoint responses for `/api/positional-evaluator` and `/api/games/{game_id}/positional-eval`.
- Open the DFS/Betting platform's "In Development" preview page, drag the sliders, and check that evaluations update dynamically.
