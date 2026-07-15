# NFL Sims — Core Source Code (src) Directory

This is the central source code directory (`src/`) for the NFL Week-to-Week Simulator and Live 4th Down Decision Bot. The codebase is fully modularized and structured into thematic packages.

## Directory Structure

```
src/
├── api/             # FastAPI REST endpoints serving live matchup data and rosters to the React UI
├── data_pipeline/   # Data pipeline utilities for generating rolling player DNA and standings summaries
├── live/            # Active live bot daemon that polls ESPN APIs, runs simulations, and drafts social posts
└── nfl_sim/         # Core Monte Carlo physics simulation engine and predictive ML model registry
```

## Packages Overview

### 1. [API Service](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/api/README.md)
Houses the Fast API web service. It exposes endpoints that fetch live vegas lines, compile slate-specific game lists, align matchup rosters, apply in-memory sliders or custom workload overrides, and dispatch simulations to return rich, aggregated stats to the React dashboard.

### 2. [Data Pipeline Packages](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/data_pipeline/README.md)
Responsible for computing and maintaining structural inputs:
* Generates rolling player DNA matrices by blending historical metrics with recent performance weights.
* Aggregates pilot-season multi-week logs into standings standings and simulated win-record variances.
* Validates blending configurations using backtests against actual game outcomes.

### 3. [Live Bot Daemon](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/live/README.md)
An automated daemon that runs during NFL game days. It scrapes ESPN scoreboard feeds, deduplicates seen plays, invokes a Python-R simulation bridge to compute fourth-down probabilities, applies strategic posting policies, and publishes coaching-error alerts to Bluesky and Mastodon.

### 4. [NFL Simulation Engine](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/nfl_sim/README.md)
The central simulation library for player and team metrics:
* Features a high-performance, fully vectorized numpy-based play-by-play physics game engine.
* Integrates a `ModelRegistry` that dynamically loads submodels (YAC, air yards, rushing yards, chaos, field goal success).
* Applies Pass Rate Over Expected (PROE) adjustments in logit space.
* Aggregates simulated metrics into bell-curve histograms, and calculates DraftKings and FanDuel scores.
