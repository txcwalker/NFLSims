# API Package (`src/api/`)

This directory houses the web service layer that connects the React web dashboard with the underlying python Monte Carlo simulation engine.

## Directory Structure

```
src/api/
├── README.md   # This documentation file
├── app.py      # Core FastAPI application endpoints and simulation dispatchers
└── app.md      # Detailed documentation for app.py
```

## Description of Files

* **[`app.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/api/app.py)**:
  * Exposes high-performance REST APIs using the FastAPI framework.
  * Handles live NFL schedule ingestion, roster compilation, baseline team and coaching traits lookup, and custom in-memory workload overrides.
  * Dynamically schedules and parallelizes Monte Carlo match simulations, processes DraftKings/FanDuel fantasy points allocations using pace/touchdown adjustments, and returns outputs to the frontend.
