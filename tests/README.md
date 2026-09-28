# Tests (`tests/`)

Python unit tests for the simulation and evaluation layers. These guard the
model-inversion and evaluator math that the frontends and live bot depend on —
run them after any change to `src/nfl_sim/` or `src/api/`.

## Directory Structure

```
tests/
├── README.md                      # This file
├── test_positional_evaluator.py   # KEP/EP evaluator suite (18 tests)
├── test_efsd_evaluator.py         # Expected Final Score Differential evaluator
├── test_live_pipeline.py          # Live game-day pipeline
├── test_game_line_eval.py         # Evaluation tab: sim vs. Vegas vs. actual grading (43 tests)
├── test_kickoff_lock.py           # Sims freeze once a game kicks off (11 tests)
└── test_player_proj_eval.py       # Evaluation tab: player projections + percentile finish (15 tests)
```

(Other test files exist beyond the ones listed here -- `ls tests/` for the full set.)

## Description of Files

* **[`test_positional_evaluator.py`](test_positional_evaluator.py)** — 18 tests
  across `PositionalEPModelV010`, `KEPConverter` (WP→KEP inversion monotonicity,
  range, kickoff-zero), and `PositionalEvaluator` (drive-end rate, concept
  finiteness, clock-aware KEP ordering). **Must stay green through any
  `game_engine.py` edit.**
* **[`test_efsd_evaluator.py`](test_efsd_evaluator.py)** — covers the EFSD
  (Expected Final Score Differential) evaluator.
* **[`test_live_pipeline.py`](test_live_pipeline.py)** — exercises the live
  game-day pipeline end to end.
* **[`test_game_line_eval.py`](test_game_line_eval.py)** — the Evaluation tab's
  Game Lines grading (`src/evaluation/`): cover/push/payout math incl.
  plus-money moneylines, vig removal, moneyline value bets, agreement-tier
  boundaries (exactly 1.0 / 2.5 pts), per-game grading, CLV sign, summaries,
  and line-ledger open/close/override resolution. Expected values are
  hand-computed in comments. Run after any change to `src/evaluation/`.
* **[`test_kickoff_lock.py`](test_kickoff_lock.py)** — runs the real
  `run_week_sim_2026` / `resim_games_2026` writers in a temp repo with a fake
  simulator: played games are carried forward untouched, `--force`
  overrides, an all-locked resim raises. Run after any change to the sim writers.
* **[`test_player_proj_eval.py`](test_player_proj_eval.py)** — the Evaluation
  tab's Player Projections: nflverse -> sim-shaped actuals (column map, LAR->LA,
  DK via the sim's own scoring), percentile finish incl. ties, projection
  quantiles, id join + name fallback, "no stat line" / "unprojected" rules,
  unplayed games excluded, coverage bounds, min-projection filter. Uses a
  synthetic 10-run sim + temp roster file (no network, no real parquet).

## Running

```bash
# Full suite
python -m pytest tests/ -v

# The load-bearing evaluator suite on its own
python -m pytest tests/test_positional_evaluator.py -v
```
