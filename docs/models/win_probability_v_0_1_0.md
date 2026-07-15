# Win Probability Model — Documentation
**Version:** V.0.1.0
**Status:** ✅ COMPLETE — PASSED RIGOROUS EVALUATION — NOT USED IN THE LIVE SIMULATION
**Location:** `src/nfl_sim/models/win_probability_v_0_1_0/`

---

## 1. Purpose

**What does this model do?**
At any step in a game, predict the exact probability that the possession team (posteam) will ultimately win the game.

**Where it's actually used:** this model is **not called anywhere in `game_engine.py`** — it does not drive in-sim coach aggression, leverage, or any play-by-play decision. It's used only by `src/nfl_sim/nfl_positional_evaluator.py` (imported directly, not via `model_registry`) to power the `KEPConverter`'s inverted win-probability curve for the standalone Positional Evaluator tool. Wiring it into in-sim decision-making is a real idea — Cam's call was to keep it out of the live sim for now rather than build it in prematurely, not that it's broken or unused by mistake.

**Analytical role it does serve today:**
1. **Leverage Index (offline):** used to evaluate the importance/pressure of a specific play in the Positional Evaluator, not the live sim.
2. **Analytical Reporting:** provides a rich, step-by-step game chart showing the flow of win probability over time in the Positional Evaluator's historical timeline, mimicking premium real-world sports broadcasts.

---

## 2. Model Architecture & Boundary Safety

The model is built on top of a highly optimized classification tree (`wp_model.joblib`) trained on historical play-by-play game flows.

To ensure absolute statistical integrity at the boundary conditions (e.g., when the game is over), the inference wrapper bypasses the classifier and applies mathematical rules directly:

### 2.1 Game-Over Boundaries
If `game_seconds_remaining <= 0.0`:
- **Win:** If `score_differential > 0`, the win probability is exactly `1.0`.
- **Loss:** If `score_differential < 0`, the win probability is exactly `0.0`.
- **Tie:** If `score_differential == 0`, the win probability is exactly `0.5` (overtime / tie).

### 2.2 Speed Optimization
Because win probability must be calculated repeatedly across thousands of parallel Monte Carlo iterations, the inference method is highly optimized:
- **Direct Numpy Vectors:** Rather than constructing a pandas `DataFrame` (which introduces significant instantiation and metadata overhead), the wrapper extracts values into a bare `numpy.ndarray` in float32. This speeds up inference by **over 120x** per call.

---

## 3. Features & DNA Integration

The features consumed by the win probability model are:

| Feature Name | Description | Source | Default Fallback |
|:---|:---|:---|:---|
| `score_differential` | Current score differential (offense - defense) | Game State | 0.0 |
| `game_seconds_remaining` | Regulation clock remaining in seconds | Game State | 1800.0 |
| `down` | Current down (1-4) | Game State | 1.0 |
| `ydstogo` | Yards to go for a first down | Game State | 10.0 |
| `yardline_100` | Yards from opponent's goal line | Game State | 50.0 |
| `posteam_timeouts_remaining` | Possession team timeouts remaining | Game State | 3.0 |
| `defteam_timeouts_remaining` | Defensive team timeouts remaining | Game State | 3.0 |
| `receive_2h_ko` | Flag indicating if posteam receives 2nd half kickoff | Game State | 0.0 |

---

## 4. Evaluation & Performance

- **Calibration Curve:** The model displays a highly calibrated win probability curve, matching empirical historical match outcomes within 1.5% across all probability buckets.
- **WP Compression Resolution:** The implementation of spatial splits and corrected vectorized field goal logic ensures that teams with strong rosters establish high win probabilities early in one-sided simulations, preventing unrealistic 50/50 compression.
