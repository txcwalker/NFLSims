# R/simulators/ — Simulation Engine

Contains the live 4th down decision engine. Called by `src/live/simulator_bridge.py`
via subprocess during live games.

## Structure

```
simulators/
└── fourth_down/
    ├── fourth_down_decision.R  # Core decision engine — evaluates all 4th down options
    └── run_one_sim.R           # Entry point spawned by Python bridge (reads JSON stdin)
```

> [!NOTE]
> Empty simulator subdirectories (`drive/`, `game/`, `play/`, `season/`, `week/`, `playoffs/`)
> and their corresponding root-level stub `.R` files have been removed.
> These were scaffolded but never implemented — the Python engine in `src/nfl_sim/`
> handles full game simulation.
