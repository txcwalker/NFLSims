# 🏈 NFLSims (NFL Exploration Platform)

Welcome to **NFLSims**, a high-fidelity, data-driven Monte Carlo simulation engine, DFS optimizer, and live analytics platform for NFL game-play and fantasy sports.

The platform simulates NFL game physics and situational decision-making down to the play level, leveraging player & coach "DNA" profiles (season-long + a rolling last-4-game blend, refreshed weekly during the season) to generate full season standings, game-level probability distributions, and optimized DFS lineups.

---

## 🏗️ Project Architecture

The workspace is split into a modular backend source directory, an R-based live game-day bot, and two primary frontend application interfaces:

```
NFL_Exploration/               (repo name on GitHub: NFLSims)
├── src/                       # Core Backend Services & Simulation Engine
│   ├── nfl_sim/                # Vectorized Monte Carlo physics game engine & ML models
│   ├── api/                    # FastAPI REST service(s) — see Port Architecture below
│   ├── data_pipeline/          # Player DNA generation, weekly blending, historical aggregation
│   ├── ownership/               # DFS ownership projection model
│   ├── scrapers/                # Roster/salary/standings scraping
│   └── live/                    # Game-day live daemon (ESPN scraper, win-probability tracker)
├── R/                          # 4th-down decision bot: live-feed integration + auto-post (Bluesky/Mastodon/X)
├── frontend_analysis/          # React — Game Analysis, Positional Evaluator, 4th Down Explorer
├── frontend/                   # React — DFS Lineup Optimizer, Paper Trading, Evaluation Tab
├── data/                       # Player/coach DNA, rosters, overrides, sim output caches
├── scripts/                    # Roster management, simulation runners, EDA, model training
├── tests/                      # pytest suite (bare `pytest` now scoped here — see pyproject.toml)
└── docs/                       # API specs, model standards, EDA writeups, audits, roadmaps
```

For what's currently active vs. frozen, see [AGENTS.md](AGENTS.md). For setup/workspace detail, see [DEVELOPMENT.md](DEVELOPMENT.md).

---

## 🚀 Getting Started

The platform includes convenient batch scripts to run each service locally on Windows.

### Prerequisites
1. **Python 3.12**: Set up a virtual environment in the root directory:
   ```bash
   python -m venv venv
   source venv/bin/activate  # On Windows: venv\Scripts\activate
   pip install -r requirements.txt
   pip install -e . --no-deps  # editable install; makes `src.*` importable everywhere, scopes bare `pytest` to tests/
   ```
   > The project is pinned to Python 3.12 with the exact dependency set in
   > [`requirements.txt`](requirements.txt). It was migrated off Python 3.8 in
   > July 2026; the pinned versions are a validated set (full test suite + a
   > full-season batch-audit sim confirmed on 3.12).
2. **Node.js & npm**: for both frontends (`frontend/`, `frontend_analysis/`).
3. **R 4.4+** (only if working on the live 4th-down bot in `R/`): see [R/README.md](R/README.md).

### Running the Services

| Service | Port | Start command |
|---|---|---|
| Analytics API | 8000 | `venv\Scripts\python.exe -m uvicorn src.api.app:app --port 8000` |
| DFS API | 8002 | `.\start_backend_api.bat` |
| Game Analysis frontend | 5174 | `.\start_analysis_site.bat` |
| DFS / Betting frontend | 5173 | `.\start_betting_site.bat` |

Both frontends proxy `/api` to their respective backend port via Vite config — never hardcode `localhost:800x` in frontend JS. Full endpoint list: [AGENTS.md §4](AGENTS.md).

### Running Tests
```bash
pytest tests/ -v      # or just `pytest` from the repo root — scoped via pyproject.toml
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
* Every player/team input flows through a per-week DNA blend (season-to-date + rolling last-4-game, volume-weighted) refreshed after each week's real games — see [AGENTS.md §0](AGENTS.md) for current status.

### 2. Live Game Analytics (`src/live`, `R/`)
* Gameday polling daemons scrape live ESPN play-by-play feeds.
* Runs instant in-game simulations to predict win probabilities and identify suboptimal coaching decisions.
* A separate R-based 4th-down bot auto-posts analytical updates to Bluesky, Mastodon, and X — currently dry-run only (posting secrets not enabled). See [docs/audit/2026_09_audit/fix_pass_plan.md](docs/audit/2026_09_audit/fix_pass_plan.md) for the open question of which live-bot entry point (Python vs. R) is canonical.

### 3. DFS & Lineup Optimizer (`frontend` & `src/nfl_sim`)
* Aggregates simulation outcomes into weekly player projection distributions.
* Solves DraftKings & FanDuel lineup optimization problems with custom salary, stack, and positional constraints, ranked against a simulated tournament field (not a single opponent).
* Paper-trading spine + an Evaluation tab compare predicted vs. realized lineup performance and ownership against settled real contests.
* Supports CSV lineup exports for direct sportsbook uploads.

---

## 🗺️ Documentation & Roadmaps

For detailed guides and development tracking, see:
* [Project Roadmap](PROJECT_ROADMAP.md) — sequence of dependency-ordered goals.
* [Goal Tracker](GOAL_TRACKER.md) — status of finished and upcoming milestones.
* [Detailed Goals](docs/planning/DETAILED_GOALS.md) — sub-project specifications.
* [API Contract](docs/api_contract.md) — FastAPI endpoint specifications.
* [AGENTS.md](AGENTS.md) — AI-to-AI handoff: what's active, frozen zones, fragile areas.
* [DEVELOPMENT.md](DEVELOPMENT.md) — workspace map, generated artifacts, AI onboarding order.
* [WORKLOG.md](WORKLOG.md) — reverse-chronological session history.
* [docs/audit/2026_09_audit/](docs/audit/2026_09_audit/) — the 2026-09 full-repo audit and its batched fix-pass plan.
