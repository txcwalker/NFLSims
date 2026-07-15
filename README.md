# 🏈 NFLSims (NFL Exploration Platform)

Welcome to **NFLSims**, a high-fidelity, data-driven Monte Carlo simulation engine, DFS optimizer, and live analytics platform for NFL game-play and fantasy sports. 

The platform simulates NFL game physics and situational decision-making down to the play level, leveraging player & coach "DNA" profiles to generate full season standings, game-level probability distributions, and optimized DFS lineups.

---

## 🏗️ Project Architecture

The workspace is split into a modular backend source directory and two primary frontend application interfaces:

```
NFL_Exploration/
├── src/                      # Core Backend Services & Simulation Engine
│   ├── nfl_sim/              # Vectorized Monte Carlo physics game engine & ML models
│   ├── api/                  # FastAPI REST service serving stats & matchup endpoints
│   ├── data_pipeline/        # Player DNA generation and historical aggregation pipelines
│   └── live/                 # Game-day live daemon (ESPN scraper & social posting bot)
├── frontend_analysis/        # React-based Game Analysis & Next-Play Predictor platform
├── frontend/                 # React-based DFS Optimizer & Betting Edge Finder platform
├── docs/                     # Comprehensive API specs, model standards, and research guides
└── config/                   # Configuration files and hyperparameter matrices
```

---

## 🚀 Getting Started

The platform includes convenient batch scripts to run each service locally.

### Prerequisites
1. **Python 3.12**: Set up a virtual environment in the root directory:
   ```bash
   python -m venv venv
   source venv/bin/activate  # On Windows: venv\Scripts\activate
   pip install -r requirements.txt
   ```
   > The project is pinned to Python 3.12 with the exact dependency set in
   > [`requirements.txt`](requirements.txt). It was migrated off Python 3.8 in
   > July 2026; the pinned versions are a validated set (full test suite + a
   > full-season batch-audit sim confirmed on 3.12).
2. **Node.js & npm**: Installed for frontend dependencies.

### Running the Services

1. **Start the FastAPI Backend API Server**
   - Serves on port `8002` with live reload.
   - Run the script:
     ```powershell
     .\start_backend_api.bat
     ```
   
2. **Start the Game Analysis Site**
   - Runs the React interactive simulator and situation analysis platform.
   - Run the script:
     ```powershell
     .\start_analysis_site.bat
     ```

3. **Start the Fantasy & Betting Platform**
   - Runs the DFS Lineup Optimizer (DraftKings/FanDuel) and edge-detection interface.
   - Run the script:
     ```powershell
     .\start_betting_site.bat
     ```

---

## 🏈 Key Features

### 1. Vectorized Monte Carlo Physics Engine (`src/nfl_sim`)
* Vectorized play-by-play simulations run 10,000+ parallel iterations in seconds.
* Integrates submodels for:
  - Play Type Selection (Pass vs. Run based on game state & tendencies)
  - Air Yards & Yards After Catch (YAC)
  - Rush Yards & Yards After Contact
  - Defensive Chaos (pressures, sacks, turnovers)
  - Field Goal Success & 4th Down Decisions
  - Clock Physics (accurate game-clock and play-clock simulation)

### 2. Live Game Analytics (`src/live`)
* Gameday polling daemons scrape live ESPN play-by-play feeds.
* Runs instant in-game simulations to predict win probabilities and identify suboptimal coaching decisions.
* Auto-posts major analytical updates to Bluesky and Mastodon.

### 3. DFS & Lineup Optimizer (`frontend` & `src/nfl_sim`)
* Aggregates simulation outcomes into weekly player projection distributions.
* Solves DraftKings & FanDuel lineup optimization problems with custom salary, stack, and positional constraints.
* Supports CSV lineup exports for direct sportsbook uploads.

---

## 🗺️ Documentation & Roadmaps

For detailed guides and development tracking, see:
* [Project Roadmap](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/PROJECT_ROADMAP.md) - Sequence of dependency-ordered goals.
* [Goal Tracker](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/GOAL_TRACKER.md) - Complete list of finished and upcoming milestones.
* [Detailed Goals](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/DETAILED_GOALS.md) - Sub-project specifications.
* [API Contract](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/docs/api_contract.md) - FastAPI endpoint specifications.
