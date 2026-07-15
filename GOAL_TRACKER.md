# NFLSims Goal Tracker

This is a living document tracking all project goals, statuses, and target dates. No explanations here — just the tracker. All goals map to the Sub-Projects in `PROJECT_ROADMAP.md`.

---

## 📐 Model Development Standard

| Goal / Milestone | Status | Target Date |
| :--- | :--- | :--- |
| Establish `MODEL_DEVELOPMENT_STANDARD.md` | ✅ Completed | May 2026 |
| Retroactive EDA doc — Play type selection | ✅ Completed | May 2026 |
| Retroactive EDA doc — Air yards | ✅ Completed | May 2026 |
| Retroactive EDA doc — Rush yards | ✅ Completed | May 2026 |
| Retroactive EDA doc — Yards after contact | ✅ Completed | May 2026 |
| Retroactive EDA doc — Chaos model | ✅ Completed | May 2026 |
| Retroactive EDA doc — 4th down decision | ✅ Completed | May 2026 |
| Retroactive EDA doc — Win probability | ✅ Completed | May 2026 |
| Retroactive EDA doc — Field goal | ✅ Completed | May 2026 |

---

## 🔧 Infrastructure & Organization

| Goal / Milestone | Status | Target Date |
| :--- | :--- | :--- |
| Phase 1: Directory cleanup & subfolder organization | ✅ Completed | May 2026 |
| Phase 2: Apply `v_0_1_0` versioning to all active scripts | ✅ Completed | May 2026 |
| Phase 2: Create `.md` documentation templates for all active scripts | ✅ Completed | May 2026 |
| Phase 3: Fill out `.md` documentation content (Why/What/SME) | ✅ Completed | May 2026 |
| Phase 3: Add strict Input/Output/Purpose docstrings to all `v_0_1_0` functions | 🔄 In Progress | June 2026 |
| Create `PROJECT_ROADMAP.md` | ✅ Completed | May 2026 |
| Create per-sub-project roadmaps (`docs/roadmaps/`) | ✅ Completed | June 2026 |
| Annual maintenance cycle documented | 🔄 In Progress | Aug 2026 |

---

## 🏈 Tier 1: Core Simulation Engine (Target: Sept 2026)

| Goal / Milestone                                    | Status         | Target Date   |
| :-------------------------------------------------- | :------------- | :------------ |
| Play-by-play event loop                             | ✅ Completed    | —             |
| Clock physics & time-leak hardening                 | ✅ Completed    | June 2026     |
| Play type selection model (final version locked)    | ✅ Completed    | June 2026     |
| Air yards model (final version locked)              | ✅ Completed    | —             |
| Rush yards model (final version locked)             | ✅ Completed    | —             |
| Yards after contact model (final version locked)    | ✅ Completed    | —             |
| Field goal model                                    | ✅ Completed    | —             |
| Pre-snap penalty model                              | ✅ Completed    | —             |
| Post-snap chaos model (sacks, turnovers, pressures) | ✅ Completed    | —             |
| Kickoff & punting logic (hardened)                  | ✅ Completed    | June 2026     |
| Player & coach DNA dictionaries                     | ✅ Completed    | —             |
| Season-long simulator (Week 1 → Week 18)            | ✅ Completed    | —             |
| Weekly single-game simulator wrapper                | ✅ Completed    | July 2026     |
| Season from Week X simulator                        | ✅ Completed    | July 2026     |
| Full system validation vs. 2024 season actuals      | ✅ Completed    | Aug 2026      |
| **Tier 1 Production Release**                       | ✅ Completed    | **Sept 2026** |

---

## 📊 Tier 2: Analytics & DFS Layer (Target: Offseason 2026–2027)

| Goal / Milestone | Status | Target Date |
| :--- | :--- | :--- |
| Win Probability model (deployed) | ✅ Completed | May 2026 |
| Win Probability live tracker | ✅ Completed | July 2026 |
| Positional Evaluator (chess-style situation scoring) | ✅ Completed | Aug 2026 |
| Next-play predictor (WP-based) | ✅ Completed | Aug 2026 |
| Next-play predictor (Coach/Player tendency-based) | ❌ Pending | Aug 2026 |
| 4th Down Bot — decision logic finalized | ✅ Completed | — |
| 4th Down Bot — live game feed integration | ✅ Completed | May 2026 |
| 4th Down Bot — auto-post (Twitter/X, Mastodon, Bluesky) | 🔄 In Progress | June 2026 |
| Live playoff odds tracker (Monte Carlo standings) | ✅ Completed | Oct 2026 |
| DFS player projection model | ✅ Completed | Jan 2027 |
| DraftKings lineup optimizer & exporter | ✅ Completed | Feb 2027 |
| FanDuel lineup optimizer & exporter | ✅ Completed | Feb 2027 |
| NFL award odds (MVP, OPOY, DPOY, etc.) | 🔄 In Progress | Dec 2026 |
| Wins Above Replacement (WAR) metric designed | ❌ Pending | Jan 2027 |
| Player contract value evaluator | ❌ Pending | Feb 2027 |
| Coach contract predictor | ❌ Pending | Feb 2027 |

---

## 📡 Tier 3: Live Integrations & Betting (Target: 2027 Season)

| Goal / Milestone | Status | Target Date |
| :--- | :--- | :--- |
| Odds API integration (live sportsbook lines) | ❌ Pending | May 2027 |
| Prediction market integration (Polymarket, Kalshi) | ❌ Pending | May 2027 |
| Sim vs. book comparison view (edge finder) | 🔄 In Progress | June 2027 |
| Weekly odds comparison report | ❌ Pending | Aug 2027 |
| Season-long odds report (division, Super Bowl) | ❌ Pending | Aug 2027 |
| **Tier 3 Production Release** | ❌ Pending | **Sept 2027** |

---

## 🔁 Annual Maintenance Cycle (Recurring)

| Goal / Milestone | Status | Target Date |
| :--- | :--- | :--- |
| Roll training windows forward 1 year | ❌ Pending | Feb 2027 (after Super Bowl) |
| Retrain all models on new window | ❌ Pending | Mar 2027 |
| Fit and update return model parameters (kickoffs, punts, interceptions, fumbles) | ❌ Pending | Mar 2027 |
| Update rosters & player DNA dictionaries | ❌ Pending | Apr 2027 |
| Update coach maps | ❌ Pending | Apr 2027 |
| Validate sim vs. prior season actuals | ❌ Pending | May 2027 |
| Publish updated version (increment `x`) | ❌ Pending | June 2027 |

