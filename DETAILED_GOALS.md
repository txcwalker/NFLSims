# NFLSims Project Roadmap

**Project Goal:** Produce an accurate, annually-maintainable NFL season-long simulation engine for the current NFL season. Models will be updated each offseason by rolling the training window forward by one year (e.g., a 10-year model trained on 2016–2025 becomes 2017–2026 for the 2027 cycle).

> This document is the "World Atlas." Each Sub-Project listed here maintains its own Sub-Project Roadmap at the next level down. Refer to individual sub-project roadmaps for granular task tracking. The `GOAL_TRACKER.md` is the living status sheet for all milestones across this atlas.

---

## 🌍 WORLD LEVEL: NFLSims Platform

The NFLSims platform has three major release tiers. Each tier builds directly on the previous.

| Tier | Description | Target Release |
| :--- | :--- | :--- |
| **Tier 1: Core Engine** | A production-ready, single-game and season-long simulator with accurate clock physics, play models, and chaos injection. | Sept 2026 |
| **Tier 2: Analytics Layer** | Win Probability, 4th Down Bot, Evaluator, DFS tools, and Award Odds layered on top of the Tier 1 engine. | Offseason 2026–2027 |
| **Tier 3: Live Integration** | Sportsbook comparison, prediction market feeds, live playoff odds tracker, and automated social posting. | 2027 Season |

---

## 🗺️ COUNTRY LEVEL: Sub-Projects

### Sub-Project 1: Core Simulation Engine
**Status:** ✅ Completed (Hardened and operational)
**Goal:** Produce a statistically valid single-game and season-long simulator.
**Sub-Project Roadmap:** Consolidated into core roadmap

| Component | Status | Notes |
| :--- | :--- | :--- |
| Play-by-play event loop | ✅ Built | `game_engine.py` — core loop operational |
| Clock physics & time management | ✅ Built | Clock leaks hardened, tempo and play-clock mechanics resolved |
| Play type selection model | ✅ Built | Integrated play-by-play execution model |
| Air yards model | ✅ Built | `air_yards_v4.json` is current best |
| Rush yards model | ✅ Built | `rush_yards_v4.json` is current best |
| Yards after contact (rush & receiving) | ✅ Built | `yac_v4.json` is current best |
| Kickoff & punting logic | ✅ Built | Punting, field goals, and kickoff logic integrated & hardened |
| Field goal model | ✅ Built | `fg_model.json` operational |
| Pre-snap penalties | ✅ Built | Integrated in chaos model |
| Post-snap chaos (sacks, pressures, turnovers) | ✅ Built | `chaos_model.json` operational |
| Player & coach DNA Dictionaries | ✅ Built | Traits, coach atlases, personnel atlas all exist |
| Season-long simulator (Week 1 → Week 18) | ✅ Built | Full season simulator running 10,000 parallel runs |
| Season from Week X simulator | ✅ Built | Integrated schedule standings lookup and simulation |
| Weekly simulator | ✅ Built | Implemented via FastAPI wrappers and simulator UI |

---

### Sub-Project 2: 4th Down Decision Bot
**Status:** ✅ Completed / Operational
**Goal:** A real-time decision evaluator for 4th down situations, with automatic posting to social media.
**Sub-Project Roadmap:** [fourth_down_bot_roadmap.md](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/docs/roadmaps/fourth_down_bot_roadmap.md)

| Component | Status | Notes |
| :--- | :--- | :--- |
| 4th down model | ✅ Built | `fourth_down_conversion_v_0_1_0` monotonic XGBoost classifier |
| Bot decision logic (Go/Kick/Punt) | ✅ Built | Evaluates mathematically optimal EV decision boundaries |
| Live game feed integration | ✅ Built | ESPN Core API plays endpoint integration complete in `espn_adapter.py` |
| Auto-post to Twitter/X | ❌ Pending | Requires API credentials and post formatter |
| Auto-post to Mastodon/Bluesky | ✅ Built | Posting clients and dry-run output policies fully implemented |

---

### Sub-Project 3: Evaluator & Win Probability
**Status:** ✅ Completed / Operational
**Goal:** A chess-engine style situational evaluator providing live WP and optimal play suggestion.
**Sub-Project Roadmap:** Consolidated into analysis platform

> **Note on Evaluator vs. WP:** Win Probability answers *"Who is likely to win?"* given current state. The Positional Evaluator answers *"How valuable is this specific situation?"* — more like a score for the quality of the position itself (field position, momentum, down/distance) independent of game score. Think of WP as the headline number and the Evaluator as the analytical breakdown explaining it. These work together.

| Component | Status | Notes |
| :--- | :--- | :--- |
| Win Probability model (training data) | ✅ Built | Training sets exist in `data/wp/` |
| Win Probability model (deployed) | ✅ Built | Deployed to backend and plotted in LiveWP |
| Positional Evaluator logic | ✅ Built | Situation scoring and next-play recommendations in Game Center |
| Next-play predictor (WP-based) | ✅ Built | Recommends optimal calls to maximize win probability in Game Center |
| Next-play predictor (Tendency-based) | ❌ Pending | Predict likely next play by coach/player DNA |
| Live playoff odds tracker | ✅ Built | Monte Carlo season sim standings integrated with remaining SOS |

---

### Sub-Project 4: DFS Optimizer
**Status:** ✅ Completed
**Goal:** Use simulation outputs to generate optimal DraftKings and FanDuel lineups.
**Sub-Project Roadmap:** Consolidated into fantasy site

| Component | Status | Notes |
| :--- | :--- | :--- |
| Player projection model | ✅ Built | Aggregates player distributions and fantasy projections |
| DK lineup optimizer | ✅ Built | Mathematical knapsack solver for showdowns & full slates |
| FD lineup optimizer | ✅ Built | Positional and salary constraint solver |
| DK lineup exporter | ✅ Built | CSV exporter formatted for DraftKings upload |
| FD lineup exporter | ✅ Built | CSV exporter formatted for FanDuel upload |

---

### Sub-Project 5: Sportsbook Integration
**Status:** 🔄 In Progress
**Goal:** Compare NFLSims output odds against live sportsbook and prediction market lines.
**Sub-Project Roadmap:** Sportsbook interface planning

| Component | Status | Notes |
| :--- | :--- | :--- |
| Odds API integration | ❌ Pending | Pull live lines from The Odds API or similar |
| Prediction market integration (Polymarket, Kalshi) | ❌ Pending | Pull current market prices |
| Sim vs. book comparison view | 🔄 In Progress | Mock edge finder UI built, pending live integration |
| Weekly odds report | ❌ Pending | Automated report generation |
| Season-long odds report | ❌ Pending | Division winners, Super Bowl odds |

---

### Sub-Project 6: NFL Awards & Contract Value
**Status:** ❌ Pending
**Goal:** Simulate season-long outcomes to generate award odds and evaluate player/coach contract value.
**Sub-Project Roadmap:** Planning phase

| Component | Status | Notes |
| :--- | :--- | :--- |
| NFL award odds (MVP, OPOY, DPOY, etc.) | 🔄 In Progress | Standing simulations track division/playoff metrics |
| Wins Above Replacement (WAR) metric | ❌ Pending | Custom WAR definition for NFL context needed |
| Player contract value evaluator | ❌ Pending | WAR vs. contract dollars |
| Coach contract predictor | ❌ Pending | Win % and situational decision quality |

---

## 🏙️ STATE LEVEL: Annual Maintenance Cycle

Each year after the NFL season concludes, the following maintenance tasks are required before re-launching the platform for the new season:

1. **Roll training windows** forward by 1 year across all models.
2. **Retrain** all models (play selection, air yards, rush yards, YAC, chaos, FG, WP, 4th down).
3. **Update rosters** and player DNA dictionaries for the new season.
4. **Update coach maps** (`team_to_coach`, `team_to_dc`, coordinator atlas).
5. **Validate** simulation outputs against the prior season's actual results.
6. **Publish** updated version (increment `x` in `V.x.y.z`).
