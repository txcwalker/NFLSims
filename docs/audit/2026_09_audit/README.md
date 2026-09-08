# Full-Repo Audit — 2026-09

**Baseline commit:** `c2287e0` (checkpoint: 7-week batch)
**Requested by:** Cam, 2026-09-08
**Mode:** report-only. No code changes during the audit. Findings triaged and
fixed as a separate pass afterward, before the next push.

## What Cam asked for (8 focus areas)

1. **Clean & efficient code** — no redundant processes / files / functions.
2. **Accuracy & bugs.**
3. **Storage** — how it's done now, can it be better; the project is large and
   growing.
4. **Live / publishing** — how close is the analytics site to being publicly
   visitable (e.g. `camanalytics.com` for friends & family); what are the
   options.
5. **Documentation** — up to date, accurate, organized.
6. **Roadmap adherence** — are we following it, updating it where needed; same
   for WORKLOG / notes / the AI-handoff files.
7. **Best practices** — flag anything that reads as poor coding practice.
8. **Drift** — have we strayed from the original goals; are we still set up to
   grow.

## Phase plan

Each phase writes `phase_N_<name>.md` in this folder and checks in with Cam
before the next. Depth is deliberately uneven — the sim engine is the product.

| Phase | Scope | Depth | Cam's areas |
|---|---|---|---|
| **0** | Inventory & baseline — file tree, LOC, active/frozen/generated zones, dependency manifests, `.gitignore`/`.gitattributes`, root-doc census | survey | 5, 6, 8 |
| **1** | Sim core — `game_engine.py`, `batch.py`, `nfl_sim/models/`, `field_simulator.py`: logic bugs, silent-failure paths, RNG & vectorization hazards, redundant compute, dead code | deep | 1, 2, 7 |
| **2** | Play-selection & DNA — base pass-rate calibration (the ~2–4pp shortfall Cam flagged), PROE stack, game-script response, dead coach levers, DNA blend, override-sheet plumbing, `scripts/roster_management/` redundancy (~55 scripts) | deep | 1, 2 |
| **3** | Data pipeline & **storage** — `scripts/simulation_runners/`, `src/data_pipeline/`, cache-staleness logic, `regenerate_2026_reports` orchestration; storage architecture (JSON DNA vs parquet vs CSV, the `current_rosters` tree, source-of-truth vs regenerable, size trajectory, is a real datastore warranted) | deep | 1, 3 |
| **4** | API & frontends & **publishing** — `src/api/`, both React apps: contract drift, error handling; plus a concrete deploy-readiness + hosting-options assessment for the analytics site | flag + deep on publishing | 4, 7 |
| **5** | R bots & live pipeline — `R/`, `src/live/`: correctness, best-practice flags | flag-only | 2, 7 |
| **6** | Docs, roadmap, tests, organization — doc-rot, roadmap/worklog/AGENTS/DEVELOPMENT accuracy, test-coverage gaps, filename conventions (incl. the `_v_0_1_0`-in-filename question vs Cam's global standard), dead files, root-`.md` sprawl | medium | 5, 6, 7 |
| **7** | Synthesis — every finding ranked severity × effort, fix-now vs defer, and an explicit drift narrative (original `PROJECT_ROADMAP` intent vs where the project actually is) | — | 8 |

## Severity scale (used in every phase)

- **S1 blocker** — wrong results, data loss risk, or blocks the publish goal.
- **S2 significant** — bug with a real accuracy/UX impact, or meaningful tech debt.
- **S3 minor** — cleanup, redundancy, style, doc-rot.
- **S4 note** — worth knowing, no action needed now.

## Status

- [x] Phase 0 — see `phase_0_inventory.md`
- [x] Phase 1 — see `phase_1_sim_core.md`
- [ ] Phase 2
- [ ] Phase 3
- [ ] Phase 4
- [ ] Phase 5
- [ ] Phase 6
- [ ] Phase 7
