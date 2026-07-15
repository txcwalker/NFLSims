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
└── test_live_pipeline.py          # Live game-day pipeline
```

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

## Running

```bash
# Full suite
python -m pytest tests/ -v

# The load-bearing evaluator suite on its own
python -m pytest tests/test_positional_evaluator.py -v
```
