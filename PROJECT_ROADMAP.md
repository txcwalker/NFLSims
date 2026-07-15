# NFLSims Project Roadmap

**Last Updated:** June 2026
**Target (Public Release):** September 2026 NFL Kickoff

> This is the sequenced, dependency-ordered roadmap. Each item must be completed (or verified complete) before the items below it can be built. For the full component-level breakdown see `DETAILED_GOALS.md`. Status tracking lives in `GOAL_TRACKER.md`.

---

## 🏈 Main Goal 1: Season Simulator

*Everything in the platform is downstream of an accurate, validated season simulator. This ships first.*

### Phase 0 — Model Documentation Audit *(Gate: Required before any model is considered production-ready)*
> See `MODEL_DEVELOPMENT_STANDARD.md` for the full EDA → Decision → Evaluation pipeline.
- [x] Play type selection — EDA doc, Decision doc, Eval doc
- [x] Air yards — EDA doc, Decision doc, Eval doc
- [x] Rush yards — EDA doc, Decision doc, Eval doc
- [x] Yards after contact — EDA doc, Decision doc, Eval doc
- [x] Chaos model — EDA doc, Decision doc, Eval doc
- [x] 4th down decision — EDA doc, Decision doc, Eval doc
- [x] Win probability — EDA doc, Decision doc, Eval doc
- [x] Field goal — EDA doc, Decision doc, Eval doc

### Phase 1.1 — Engine Foundation *(Rescript + integrate all models)*
- [x] `engine.py` full rescript with strict docstrings and modular structure (implemented as `game_engine.py`)
- [x] Player & Coach DNA Dictionaries (Traits, Coach Atlas, Personnel Atlas)
- [x] Play type selection model
- [x] Field goal model
- [x] Pre-snap penalty model
- [x] Post-snap chaos model

### Phase 1.2 — Yardage Models *(Feeds into engine accuracy)*
- [x] Air yards model
- [x] Rush yards model
- [x] Yards after contact model (rush & receiving)

### Phase 1.3 — Decision Layer *(Required for valid game state simulation)*
- [x] 4th down decision model (Go / Kick / Punt)
- [x] Win probability model (required for 4th down and in-game decision making)
- [x] Clock physics hardening (time-leak elimination and play-clock mechanics)
- [x] Kickoff & punting logic hardening

### Phase 1.4 — Simulator Wrappers *(Packaging the engine for use)*
- [x] Weekly single-game simulator (available through FastAPI backend and Simulator UI)
- [x] Season-long simulator (Week 1 → Week 18) (integrated and running 10,000 parallel runs)
- [x] Season from Week X simulator (inject current standings + simulate remainder, driving standings dashboard)

### Phase 1.5 — Validation *(Gate before public release)*
- [x] Full system validation against 2024 season actuals
- [x] 📢 **Tier 1 Public Release — Sept 2026**

---

## 💰 Main Goal 2: DFS & Betting Integration

*Requires a validated Season Simulator as its data engine. Builds on top of Goal 1.*

### Phase 2.1 — Projections Layer *(Sim output → per-player stats)*
- [x] Player projection model (aggregate sim runs into weekly stat distributions and fantasy points)

### Phase 2.2 — DFS Tools
- [x] DraftKings lineup optimizer (solving showdown/full slates, stack constraints)
- [x] DraftKings lineup exporter (CSV upload format)
- [x] FanDuel lineup optimizer (salary and positional constraints solver)
- [x] FanDuel lineup exporter (CSV upload format)

### Phase 2.3 — Betting Integration
- [ ] Sportsbook odds API integration (live lines)
- [ ] Prediction market integration (Polymarket, Kalshi)
- [ ] Sim vs. book edge finder (highlight value gaps) (preview available, pending live odds feed)
- [ ] Weekly odds comparison report
- [ ] Season-long odds report (division winners, Super Bowl odds)
- [ ] 📢 **Tier 2 Public Release — Offseason 2026–2027**

---

## 📐 Main Goal 3: Self-Defined Metrics

*Requires the Season Simulator as a data source. Many of these can be developed in parallel with Goal 2.*

### Phase 3.1 — Wins Above Replacement (WAR)
- [ ] Define WAR methodology for NFL context (positional baselines, marginal win value)
- [ ] Build WAR calculation from season simulation outputs
- [ ] Player contract value evaluator (WAR vs. contract dollars)
- [ ] Coach contract predictor (win% + situational decision quality)

### Phase 3.2 — Situational Evaluator
- [x] Positional Evaluator — chess-style situation scorer (field position, momentum, down/distance, independent of score, integrated into Game Center)
- [x] Next-play predictor (WP-based: what call maximizes win probability? integrated in Game Center)
- [ ] Next-play predictor (Tendency-based: what call does this coach/player DNA suggest?)

### Phase 3.3 — Award Odds
- [/] NFL award odds simulator (MVP, OPOY, DPOY, OROY, DROY, Coach of the Year) (simulated standing playoff outcomes and SB metrics completed)
- [ ] 📢 **Tier 3 Internal Release — Early 2027**

---

## 📡 Main Goal 4: Live Tools

*Requires Goals 1–3 to be substantially complete. These are real-time wrappers on top of existing models.*

### Phase 4.1 — In-Game Live Tools
- [x] Live Win Probability tracker (feed live game state into WP model and chart momentum)
- [x] Live playoff odds tracker (Monte Carlo season sim with current standings)
- [x] 4th Down Bot — live game feed integration (ESPN NFL live data source)
- [ ] 4th Down Bot — auto-post to Twitter/X (pending API credentials)
- [x] 4th Down Bot — auto-post to Mastodon & Bluesky (clients built, Dry Run modes verified, ready for production)

### Phase 4.2 — Annual Maintenance (Recurring Each Offseason)
- [ ] Roll all model training windows forward 1 year
- [ ] Retrain all models on new window
- [ ] Update rosters, player DNA, and coach maps
- [ ] Validate sim vs. prior season actuals
- [ ] Publish updated version (increment `x` in `V.x.y.z`)
- [ ] 📢 **Full Platform — 2027 Season**
