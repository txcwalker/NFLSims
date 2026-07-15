# Chess-Style Positional Evaluator — What Was Built

**Session date:** 2026-06-21  
**Feature:** V1 of the chess-style football positional evaluator (KEP + EP)

---

## The Core Idea

In chess, every position has a centipawn score — a single number that tells you who's winning and by how much, regardless of who's currently moving. We built the football equivalent.

Two metrics power it:

**EP (Expected Points)** — pure field-position value. Given down, distance, and yardline, what's the expected point outcome of the current possession? No clock, no score, no context — just situational value. Think of it as a heat map: the closer to the goal line and the better the down, the higher the number.

**KEP (Kickoff-Equivalent Points)** — the clock-aware version. It answers: "what score differential at kickoff would give the offense exactly the same win probability they have right now?" It's EP with game context baked in. A 3rd & 10 at midfield up 7 with 2 minutes left means something very different from the same situation in the 2nd quarter.

Together they give you a chess-engine-style positional read on any moment in a game.

---

## What Was Built

### 1. EP Model (`src/nfl_sim/models/positional_ep_v_0_1_0/`)

A trained XGBoost regression model on 10+ years of NFL play-by-play data.

- **Features:** yardline, down, yards-to-go, goal-to-go flag  
- **Target:** nflfastR's pre-computed EP label  
- **Performance:** val RMSE 0.368, test RMSE 0.367  
- **Filters:** run/pass plays only, no spikes/kneels/aborted plays, EP bounded ±7.5

Files:
- [`src/nfl_sim/models/positional_ep_v_0_1_0/train_positional_ep.py`](../src/nfl_sim/models/positional_ep_v_0_1_0/train_positional_ep.py) — training script
- [`src/nfl_sim/models/positional_ep_v_0_1_0/positional_ep_inference.py`](../src/nfl_sim/models/positional_ep_v_0_1_0/positional_ep_inference.py) — inference wrapper
- [`src/nfl_sim/models/positional_ep_v_0_1_0/positional_ep_model.json`](../src/nfl_sim/models/positional_ep_v_0_1_0/positional_ep_model.json) — trained artifact (531KB)

### 2. KEP Converter (`src/nfl_sim/nfl_positional_evaluator.py` — `KEPConverter` class)

Inverts the WP model to get the score differential that matches the current WP at kickoff conditions (t=3600, 1st & 10, own 25).

Key engineering detail: the WP classifier was trained on actual game plays, which have essentially no training data for extreme score differentials at t=3600. This caused non-monotonic spikes (WP(−33)=0.577) that broke the inversion. Fixed with **isotonic regression** — a least-squares monotone projection that averages out the spikes without distorting the well-calibrated middle of the curve. Grid range restricted to ±24 points.

### 3. Drive-Rollout Harness (`PositionalEvaluator` class)

Runs N parallel simulation lanes (default 1000) from the current game state, stops each lane when its drive ends (possession change, needs_kickoff, or game_over), and captures what happened on the first play.

The critical engineering problem was **single-drive isolation in a vectorized engine**: the engine runs all N lanes simultaneously and doesn't naturally "stop" at drive end. Solution: snapshot each lane's outcome the moment its drive ends, then immediately freeze that lane (`eng.game_over[lane] = True`) so it doesn't contaminate subsequent drives.

First-play concepts are classified by route depth:
- **Run** — non-pass play
- **Screen** — air yards ≤ 0
- **Short** — air yards 1–9
- **Medium** — air yards 10–19
- **Deep** — air yards ≥ 20

For each concept, the evaluator reports `delta_kep` = (mean KEP of drives that started with that concept) − (baseline KEP of the starting state). Positive delta = that concept tends to improve your position; negative = it tends to hurt.

### 4. Game Engine Hook (`src/nfl_sim/game_engine.py`)

Added 6 new instance arrays that persist the last play's attributes across the step boundary, making them readable from outside the engine:

```
last_play_scrimmage_mask, last_play_is_pass, last_play_is_run,
last_play_air_yards, last_play_is_sack, last_play_is_scramble
```

Additive-only change — zero impact on game logic or existing simulations.

### 5. ESPN Play Parser (`src/live/espn_adapter.py`)

Added `parse_plays_to_states()` — a sibling of the existing 4th-down parser that keeps **all** scrimmage plays (downs 1–4) rather than filtering to 4th down only. Used by the play-stream endpoint to evaluate every play in a game.

---

## API Endpoints

Both endpoints are live on the backend (port 8000).

### `GET /api/positional-evaluator` — Slider Tool

Evaluate any game state on demand. Takes query params:

| Param | Default | Description |
|---|---|---|
| `down` | 1 | 1–4 |
| `distance` | 10 | yards to gain |
| `yardline_100` | 75 | yards to end zone |
| `clock` | 1800 | game seconds remaining (3600 = kickoff) |
| `score_differential` | 0 | offense score minus defense score |
| `posteam_timeouts` | 3 | offense timeouts remaining |
| `defteam_timeouts` | 3 | defense timeouts remaining |
| `off_team` | KC | offense team (for roster DNA) |
| `def_team` | BUF | defense team |
| `n_sims` | 1000 | parallel drive simulations (100–5000) |

Example response:
```json
{
  "ep": 1.092,
  "kep": 0.376,
  "n_sims": 1000,
  "drive_end_rate": 0.994,
  "concepts": [
    { "concept": "Deep",   "delta_kep":  0.412, "n": 48  },
    { "concept": "Medium", "delta_kep":  0.218, "n": 112 },
    { "concept": "Short",  "delta_kep":  0.104, "n": 287 },
    { "concept": "Screen", "delta_kep": -0.091, "n": 62  },
    { "concept": "Run",    "delta_kep": -0.203, "n": 491 }
  ]
}
```

### `GET /api/games/{game_id}/positional-eval` — Play Stream

Fetches live ESPN PBP for a game and returns EP + KEP for every scrimmage play. No simulation — fast lookup using the KEP curve and EP model directly.

```json
{
  "game_id": "401671789",
  "home_team": "KC",
  "away_team": "BUF",
  "n_plays": 142,
  "evaluations": [
    {
      "play_id": "4017...", "qtr": 1, "clock": "14:32",
      "off": "KC", "def": "BUF",
      "down": 1, "ydstogo": 10, "yardline_100": 75,
      "score_differential": 0,
      "ep": 0.811, "kep": 0.0
    },
    ...
  ]
}
```

---

## Frontend Changes

### DFS Site — Chess Slider (`frontend/src/pages/InDevelopment.jsx`)

The chess wireframe section is now live. Sliders control:
- Down (1–4 buttons)
- Distance to gain
- Field location
- Game clock (Q1–Q4 display)
- Score margin
- Offense/defense timeouts

On any slider change, a debounced fetch (400ms) hits `/api/positional-evaluator`. Results display:
- **EP** and **KEP** as large number cards
- **Concept ranking bars** sorted best-to-worst by delta_kep, with a "BEST" badge on the top concept and proportional green/red bars
- "API offline" error banner if the backend isn't running (degrades gracefully)

### Strategy Site — GameSummary Chess Tab (`frontend_analysis/src/pages/GameSummary.jsx`)

The "Chess Tactical Evaluator" tab was rebuilt. It now shows:
- **KEP timeline chart** — a Recharts line chart of KEP (cyan) and EP (orange dashed) across every evaluated play in the game. Click any point to select that play.
- **Play detail panel** — for the selected play: team, quarter/clock, down/distance, yardline, EP value, KEP value, and a plain-English interpretation ("KC holds a +2.31 KEP positional advantage — equivalent to receiving a kickoff up 2.3 pts").

The chart and the Play Log tab share `selectedPlayId` — clicking a play in either tab highlights it in both.

---

## Tests (`tests/test_positional_evaluator.py`)

18 unit tests, all passing (8.1s runtime):

| Class | Tests |
|---|---|
| `TestPositionalEPModel` | Monotone EP across yardline, range bounds ±7.5, later-down EP lower, goal-to-go flag, batch/scalar match |
| `TestKEPConverter` | Tied kickoff ≈ 0, monotone inversion, winning state positive, trailing state negative, grid boundary clipping |
| `TestPositionalEvaluator` | EP positive at opponent territory, red-zone > midfield, tied kickoff KEP ≈ 0, all 5 concepts present, finite deltas, drive-end rate in (0,1], n_sims match, leading-late higher KEP |

Run with:
```
python -m pytest tests/test_positional_evaluator.py -v
```

---

## Can You See It Live?

**Yes, everything is wired up.** You need three things running:

1. **Backend API** (port 8000):
   ```
   uvicorn src.api.app:app --host 0.0.0.0 --port 8000 --reload
   ```

2. **DFS site** (port 5173) — navigate to the chess wireframe section:
   ```
   cd frontend && npm run dev
   ```

3. **Strategy site** (port 5174) — navigate to any live game → Chess Tactical Evaluator tab:
   ```
   cd frontend_analysis && npm run dev
   ```

**What you'll see with the backend running:**
- DFS chess sliders → live EP/KEP numbers update as you drag (~400ms after you stop)
- Concept ranking bars animate to reflect the actual simulation results
- Strategy site chess tab → KEP timeline chart across the game's play sequence

**What you'll see without the backend:**
- DFS chess section: "API offline — start the backend to see live analysis" error banner
- Strategy site: mock data (8 fabricated plays for game 1, etc.) — chart and play detail work correctly, just not real plays

**One live-game note:** there are no NFL games right now, so `/api/games/{game_id}/positional-eval` will return a 502 for any real game ID until the season starts. The slider endpoint works any time since it doesn't depend on a live feed.
