# AGENTS.md — NFLSims AI Handoff Contract
<!-- Token-efficient AI-to-AI handoff. Updated: 2026-07-10 (clock_physics_v020 session) -->

---

## 0. Current Priorities (active as of last session)

- **`clock_physics_v020` audit is the active thread** — see [docs/audit/clock_physics_v020/README.md](docs/audit/clock_physics_v020/README.md) for the full round-by-round history (9 rounds so far). Do not start a fresh investigation without reading this doc first; it has already ruled out several plausible-sounding hypotheses (see §11.5).
- **YAC is fixed (Round 9).** Was −14.4% overall (worst on deep/screens); now −1.6% overall, essentially exact in primary/deep/screen/standard, and goalline improved 5x (+241%→+49.9%, small residual on a tiny-volume, tiny-absolute-yardage zone). Two root causes, both fixed: (1) the model was trained with `reg:absoluteerror` (median-targeting) instead of `reg:squarederror` (mean-targeting), which systematically undershoots the mean on YAC's right-skewed distribution; (2) a post-model noise-injection step used a flat minimum scale (6.0) that, combined with floor-clipping at zero, mechanically inflated the mean wherever the real YAC is small (goalline worst, redzone next, primary fine) — fixed by scaling the noise to the play's own predicted YAC. See §11.7.
- **Snap-count overshoot: squeeze-play mechanic removed (Round 10), partial fix.** Squeeze-play (the discrete "rush the snap to guarantee an extra down before the two-minute warning" mechanic) is gone entirely — replaced with a plain `_sample_pace_runoff` call, since the empirical pace pools already encode real teams' hurry-up pace near the warning via fine-grained time-window buckets (no separate mechanic needed for realistic speedup, and no artificial extra-play guarantee needed either). Measured, isolated effect: offensive snaps +7.2%→+5.4%, total snaps +6.6%→+4.7% (~2.3-2.9 fewer combined plays/game) — Cam's instinct that this was "a couple of plays a game" was right. **~6-7 combined plays/game still unexplained.** See §11.8.
- **Air yards retrained (Round 11) — architecture fixed, but barely moved the numbers.** Unlike YAC, the orphaned script already used mean-targeting (`reg:squarederror`), so there was no known objective-function bug to fix. Pass yards/game and snap count both stayed flat (−7.8%→−8.1%, +5.4%→+5.2%). Main value: a real, working, git-visible zone-split training script now exists where there was none. Goalline air_yards remains a real residual (−42%, small volume ~4.5% of attempts) — a fallback-donor bug (defaulting to `primary` instead of the adjacent `redzone` zone) was found and fixed but didn't meaningfully move the number; not chased further given its small volume share. See §11.9.
- **Full 18-week 2025 season dataset regenerated (Round 16), now the default data source for the frontend tools.** `data/interim/sim_results_2025_{games,players}.parquet` — all 272 REG-season matchups at N=1000, current (post-all-fixes) engine. `src/api/app.py` already reads these by default (`/api/week_projections` already defaults to `week=1`); nothing needed there. Frontend UI wiring (actually consuming these endpoints in HistoricalLab/GameSummary/DFS site) is a deferred follow-up — this round was data generation only. **Footgun to know about:** `scripts/simulation_runners/run_week_1_only_2025.py` unconditionally overwrites the *entire* parquet cache with just Week 1's 16 games — that's what made the cache stale before this round. Don't run it expecting it to "add" week 1; it replaces everything. Use `run_full_season_sim_2025.py` to (re)generate the full cache.
- **Quarter-transition state corruption: found and fixed (Round 13).** Cam spotted repeated identical down/distance/yardline lines in a readable play-by-play (different players/gains, same situation) and asked for a real audit rather than a clock-physics explanation. Confirmed genuine bug: the Q2→Q3 kickoff logic was overwriting a just-resolved play's real yardline/down/distance with the hardcoded second-half-kickoff state, in the same step, discarding real game progress on roughly half of all simulated games (every game hits this once, whenever the Q2-ending play is a real snap rather than a kickoff/kneel). Fixed; verified via a new permanent regression test, `scripts/audit_play_continuity.py` (95→1 violations across 1000 games, the 1 a diagnostic-only artifact). Season-level snap counts barely moved (a wash — see §11.10) — this does not explain the still-open overshoot, but was a real correctness bug independent of it.
- **Sacks: fixed (Round 15).** Net sacks/game now 2.410 vs. real 2.41 (+0.2%, essentially exact). Two changes: throwaway-diversion rate cut 0.20→0.06 (kept `scramble_rate` untouched — real per-QB data, not an arbitrary constant), plus `SACK_PROB_CALIBRATION_MULT = 1.24` on gate 2's raw output (same pattern as gate 4's existing `* 0.80`), tuned empirically via `scripts/eda/test_sacks_diversion_hypothesis.py`. Bonus: snap-count overshoot improved slightly as a side effect (offensive +5.2%→+4.2%, total +4.6%→+3.8%). Expected trade-off, not a new bug: pass yards/game widened −8.1%→−10.2% (sacks now correctly subtract real negative yardage instead of some being zero-yard throwaways) — ties to the already-open air-yards gap (Round 11). The L4 duplicate-feature bug (Round 12) is deliberately bundled into the v0.4.0 trench-tier retrain rather than fixed now — it needs real per-week trench data that doesn't exist in the current pipeline (`trench_dna.json` is season-level only), the same kind of data-build as the tier-matchup work. See §11.11-11.12.
- **New, smaller residual: air_yards is now the largest remaining piece of the pass-yards gap** (−4.4% to −7.3% depending on zone/depth), untouched by the Round 9 YAC work. `air_yards_v_0_1_1` has the identical orphaned-training-script problem YAC had — the same fix pattern (live pull + DNA joins, mean-targeting objective, grouped split) would likely apply directly if tackled next.
- **Bigger-picture finding from Round 9: `src/nfl_sim/models/` has zero files tracked in git, ever** (`git ls-files src/nfl_sim/models/` returns nothing) — same for `data/` (gitignored outright). This explains why the "orphaned V.0.2.0 retrain script" from Round 8 was never findable in git history — nothing under either directory was ever expected to be there. Worth Cam's explicit call on whether this is the intended posture (rely on OneDrive sync, not git, for models/data) — right now a `git clean` or a lost local copy takes the entire trained-model layer with it.
- **Deferred to v0.4.0** (do not start without Cam's go-ahead): sacks recalibration — direction changed 2026-07-13, see §11.4 (migrate off `trench_tiers_2025.json`'s 1-5 tier grades onto `trench_dna.json`'s raw per-team metrics instead, not a trench-tier matchup add-on as originally planned), injury stoppages, other administrative clock stoppages (replay review, etc.).
- **Deferred, parameters already agreed:** throwaway mechanism rework. Target: ~5% of all dropbacks should be throwaways, split 75% pressure-related / 25% clean-pocket. Current mechanism only fires as a 20% escape from would-be-sacks — too narrow, needs a clean-pocket path too. See §11.6.
- **Must land before season launch (Sept 2026), not started:** extra-point/2-point-conversion modeling. Every touchdown currently awards a flat +7 unconditionally — there is no PAT or 2-point-attempt logic anywhere in the engine (`docs/models/README.md` §2.14). Acknowledged gap, deliberately not being worked on right now.

---

## 1. Repo Layout (active code only)

```
src/
  api/app.py                          ← FastAPI — analytics backend (port 8000)
  live/espn_adapter.py                ← ESPN scraper + play-state parser
  nfl_sim/
    game_engine.py                    ← Monte Carlo play engine (vectorized, N games in parallel). MOST ACTIVE FILE — see §11.
    model_registry.py                 ← Singleton loading all trained models; game_engine.py calls self.registry.*
    batch.py                          ← BatchSimulator wraps NFLGameEngine + StatAggregator for N-iteration batch runs
    nfl_positional_evaluator.py       ← KEP/EP evaluator; evaluate_one_step (one-play KEP delta) + suggest_lines (PV chains)
    week1_2025.py                     ← nfl_data_py loader for Week 1 2025 schedule + PBP; computes home_kep per play
    models/
      win_probability/                ← WP XGBoost model + calibration artifacts
      clock_pace_v_0_1_0/              ← NEW (clock_physics_v020): empirical bootstrap pools for snap-to-snap runoff, keyed by (quarter/time-window x score-margin tier). pace_pools.json is generated, not hand-edited — rebuild via scripts/eda/analyze_clock_pace_grid.py.
      positional_ep_v_0_1_0/
        train_positional_ep.py        ← EP model training script
        positional_ep_inference.py    ← EP inference wrapper
        positional_ep_model.json      ← Trained XGBoost artifact (531 KB)
        metadata.json                 ← Version, features, training metrics

scripts/
  simulation_runners/run_weeks_1_to_4_2025.py  ← Full batch audit driver. Writes docs/audit/v_0_2_0_audit/sim_post_cal_metrics.json + docs/boxscores/week_N/. THE way to measure any game_engine.py change at scale.
  eda/run_historical_eda.py                    ← Real 2021-2025 comparison metrics → historical_eda_metrics.json (the "ground truth" the sim is compared against)
  eda/analyze_clock_pace_grid.py                ← Rebuilds src/nfl_sim/models/clock_pace_v_0_1_0/pace_pools.json from real pbp data
  print_play_by_play_v020.py                    ← Non-interactive single-game (N=1) play-by-play dump for manual clock/mechanic verification. Different from (and supersedes for clock work) the legacy scripts/play_by_play_runner.py, which drives the old sequential engine and is interactive.

docs/
  audit/clock_physics_v020/            ← Active audit doc (7 rounds). Read before touching clock/pace/completion-rate code.
  audit/v_0_2_0_audit/                 ← Generated comparison JSONs (sim_post_cal_metrics.json, historical_eda_metrics.json) + weeks 1-4 game/standings/leader summaries. Regenerated by the scripts above, not hand-edited.
  boxscores/week_1..week_18/           ← Generated per-game boxscores. NOT git-tracked (confirmed — 0 files under version control). Local-only output.

frontend_analysis/                    ← Analytics/strategy site (port 5174)
  vite.config.js                      ← Proxy: /api → http://127.0.0.1:8000
  src/
    api.js                            ← API_BASE='/api'; safeFetch + all mock data
    pagesConfig.js                    ← Page registry; add new pages here + App.jsx
    pages/
      GameSummary.jsx                 ← Main game view: WP graph, 4th down, chess tab
      HistoricalLab.jsx               ← Testing lab: Week 1 2025 chess evaluator (remove before launch)

frontend/                             ← DFS site (port 5173; API on 8002)
  vite.config.js                      ← Proxy: /api → http://127.0.0.1:8002

tests/
  test_positional_evaluator.py        ← 18 unit tests (KEPConverter, EP model, evaluator). Must stay green through any game_engine.py edit.
```

---

## 2. Frozen / Legacy Zones

- `legacy/` — do not touch; kept for archival reference only. Its data + model binaries (2.35GB) are gitignored; only code is committed.
- `R/` (root) — **ACTIVE, not deprecated.** This is the live game-day 4th-down bot: `src/live/main.py` shells out to `Rscript R/simulators/fourth_down/run_one_sim.R`, and both GitHub Actions workflows (`nfl_live.yml`, `nfl_manuel.yml`) run R directly. Python is authoritative for the *simulation engine* (`src/nfl_sim/`), but the *live 4th-down bot* remains R. Only the R under `legacy/` is retired.
- `frontend/src/pages/InDevelopment.jsx` — chess code removed; no chess page in pagesConfig.js. Do not re-add chess to the DFS site.
- `scripts/play_by_play_runner.py` — drives the OLD `legacy.game_engine_sequential.SequentialNFLGameEngine`, not the current vectorized engine. Interactive (blocks on stdin). Do not use for verifying current engine behavior — use `scripts/print_play_by_play_v020.py` instead.
- `MODEL_DEVELOPMENT_STANDARD.md` is referenced by other docs (roadmap, tracker) as the model-audit gate but **does not exist in the repo**. Don't assume it's there.

---

## 3. Port Architecture

| Service | Port | Start command |
|---|---|---|
| Analytics API | 8000 | `python -m uvicorn src.api.app:app --host 0.0.0.0 --port 8000` |
| DFS API | 8002 | `.\start_backend_api.bat` |
| Analytics frontend | 5174 | `cd frontend_analysis && npm run dev` |
| DFS frontend | 5173 | `cd frontend && npm run dev` |

Both frontends proxy `/api` to their respective backends via vite config. Never hardcode `localhost:8000` in frontend JS — use relative `/api/` paths.

---

## 4. Key API Endpoints (port 8000)

| Endpoint | Purpose |
|---|---|
| `GET /api/positional-evaluator` | Slider/manual tool — takes down, distance, yardline_100, clock, score_differential, timeouts, n_sims; returns EP, KEP, ranked concept delta_kep list |
| `GET /api/games/{game_id}/positional-eval` | Play-stream — scrapes ESPN PBP for a live game_id, runs evaluator on each play, returns `evaluations[]` array |
| `GET /api/games/{game_id}/play-by-play` | Raw PBP plays for WP timeline |
| `GET /api/games/{game_id}/fourth-downs` | 4th-down decision rows |
| `POST /api/fourth-down-evaluate` | Evaluate a 4th-down situation |
| `GET /api/historical/week1-2025` | Returns 16 Week 1 2025 game records from nfl_data_py (schedule, scores) |
| `GET /api/historical/plays/{game_id}` | Returns scrimmage plays for a game with home_kep + ep pre-computed per play |
| `GET /api/historical/suggest-lines` | One-step KEP: evaluates current state, returns top N concept chains (principal variations, depth 2) |

All endpoints fall back gracefully: 502 = ESPN fetch failed; 404 = route not found. Frontend `safeFetch` catches non-ok HTTP and uses mock data instead.

---

## 5. Chess Evaluator System (KEP/EP)

**Concept:** Measures positional value in "Kickoff-Equivalent Points" — the score margin at kickoff that yields the same win probability as the current game state. Analogous to centipawns in chess.

**Data flow:**
1. `PositionalEvaluator.evaluate(state)` — rolls out N_SIMS drives from the given state
2. Each drive uses `first_play_concept` classification (from `game_engine.py` snapshot attributes) to bucket drives by opening call: Run / Screen / Short / Medium / Deep
3. `KEPConverter.wp_to_kep(wp)` inverts the WP model (isotonic regression ensures monotonicity) to get KEP
4. Returns `ep_start`, `kep_start`, and per-concept `delta_kep` (concept mean KEP − baseline KEP)
5. All positive delta_kep = concept improves position; all-zero = ceiling/floor hit (blowout — doesn't matter)

**Frontend (GameSummary.jsx chess tab):**
- Chart: Recharts dual-line (KEP solid, EP dashed) over the play timeline
- Detail panel: selected play's EP, KEP, natural-language advantage string, and PLAY CONCEPT RECOMMENDATION ranked bars
- Concept fetch: fires on `[selectedPlayId, activeTab, chessEval]` change; falls back to last evaluation when selectedPlayId is null (race condition guard)

**One-step KEP (current implementation, Historical Lab):**
- `evaluate_one_step()` simulates exactly one scrimmage play per lane, then computes KEP of the resulting state
- Three cases: mid-drive (possession kept, no score) → WP→KEP from new state; scored/terminal → `_drive_end_kep`; turnover → opponent's KEP from exact turnover yardline
- `mean_next_state` captured per concept for chaining into suggested lines
- `suggest_lines()` chains `evaluate_one_step` calls: best concept step 1, then evaluates from `mean_next_state` for step 2 etc.

**KEP reference frame:** home-team reference. `home_kep = kep_off if posteam == home_team else -kep_off`. Positive = home team winning. Sign never flips on possession change — smooth Lichess-style charts, no sine waves.

---

## 6. Mock Data (frontend_analysis/src/api.js)

Five mock games are defined for development without live ESPN:

| game_id | Matchup | Scenario |
|---|---|---|
| live_game_1 | PHI vs KC | Q4 close game |
| live_game_2 | SF vs LAR | Q3 medium leverage |
| live_game_3 | DET vs GB | Q4 critical |
| live_game_4 | MIA @ BUF | **Blowout** — BUF +21, Q3 4:00; KEP arc from 0→+24 ceiling |
| live_game_5 | LAC @ KC | **10-14pt game** — KC +7, Q4 2:00 late-game tension |

live_game_4 and live_game_5 were added to demonstrate KEP ceiling/floor behavior and mid-range leverage scenarios.

---

## 7. Test Commands

```bash
# Run all positional evaluator tests — must stay green through any game_engine.py edit
python -m pytest tests/test_positional_evaluator.py -v

# Syntax check after any game_engine.py edit (cheap, do this first)
python -m py_compile src/nfl_sim/game_engine.py

# Full batch audit — the way to measure any clock/pace/passing-model change at scale
# (64 games x 1000 iterations; writes docs/audit/v_0_2_0_audit/sim_post_cal_metrics.json,
# compare against historical_eda_metrics.json in the same dir). Takes several minutes.
python scripts/simulation_runners/run_weeks_1_to_4_2025.py

# Rebuild the real-2021-2025 comparison baseline (only needed if the definition of a
# tracked metric changes, not for every game_engine.py edit)
python scripts/eda/run_historical_eda.py

# Single-game (N=1) play-by-play dump for manual clock/mechanic verification —
# non-interactive, unlike the legacy scripts/play_by_play_runner.py
python scripts/print_play_by_play_v020.py

# Human-readable football-narrative play-by-play for a single game (N=1) —
# "Mahomes complete to Kelce for 12 yards" instead of a raw state-diff log
python scripts/print_readable_play_by_play.py [AWAY] [HOME]

# Regression test: field position must move by exactly the recorded gain on
# any possession-unchanged normal play. Rerun after any change touching
# quarter transitions, clock handling, or possession-switching logic — this
# is what caught the Round 13 quarter-transition state-corruption bug.
python -c "from scripts.audit_play_continuity import audit; audit(n_games=1000, max_steps=250)"

# Rebuild clock_pace_v_0_1_0/pace_pools.json from real pbp data (only if the grid's
# bucket definitions change, e.g. quarter/time-window/margin-tier boundaries)
python scripts/eda/analyze_clock_pace_grid.py

# Start analytics API
python -m uvicorn src.api.app:app --host 0.0.0.0 --port 8000

# Quick endpoint smoke test
curl "http://localhost:8000/api/positional-evaluator?down=1&distance=10&yardline_100=75&clock=1800&score_differential=0&posteam_timeouts=3&defteam_timeouts=3&n_sims=100"
```

**Standard verification sequence for any `game_engine.py` change:** `py_compile` → `pytest tests/test_positional_evaluator.py` → smoke test (small N via `NFLGameEngine(team, team, year=2025, N=20-500)` directly, or `BatchSimulator`) → full batch audit if the change could plausibly affect play count, pace, or completion rate.

---

## 8. Fragile Areas / Gotchas

- **WP model inversion at kickoff (t=3600):** Known calibration quirk; KEP values at game start may be slightly off. Clamp is ±24. Does not affect relative concept ranking.
- **CORS:** `app.py` allows origins 5173 and 5174. If ports change, update `ALLOWED_ORIGINS` in app.py or set env var.
- **selectedPlayId race condition:** GameSummary.jsx initializes selectedPlayId=null; `loadAllGameDetails` is async. The concept useEffect uses `?? evals[evals.length-1]` fallback to handle null selectedPlayId when chess tab is opened before data loads.
- **Recharts programmatic clicks:** Native DOM MouseEvents dispatched to SVG elements do NOT trigger Recharts' synthetic event handler. Use React state changes (setSelectedPlayId) instead.
- **InDevelopment.jsx (DFS site):** Chess code was removed. There is no `id: 'chess'` page in pagesConfig.js — do not add one without explicit instruction.
- **HistoricalLab.jsx:** Marked "TESTING LAB — REMOVE BEFORE LAUNCH". Routed as `historical-lab` in pagesConfig.js. Remove from pagesConfig.js + App.jsx before going to production.
- **`receive_2h_ko` missing from nfl_data_py:** This field is not in the PBP dataset. `week1_2025.py` defaults it to 0.0 for all plays. Do not try to populate it from nfl_data_py.
- **`src/nfl_sim/data/` must not exist as a Python package:** The YAC model's path-walker traverses up from `__file__` looking for the first directory named `data/`. A `data/` subpackage under `src/nfl_sim/` intercepts the walk and breaks the YAC model. Keep `week1_2025.py` directly in `src/nfl_sim/`, never in a `src/nfl_sim/data/` subdir.
- **HistoricalLab chart click UX (unresolved):** Dot onClick handlers are attached directly to SVG circles in the `dot` render function AND via `activeDot onClick`. Programmatic SVG dispatch does not trigger these (Recharts limitation). Real user clicks should work but behavior was not fully verified. If clicks still feel unreliable, replace with a scrollable play list below the chart — each row is a clickable play showing clock, situation, and KEP. This was the recommendation in the original AGENTS.md note about Recharts.
- **week1_2025.py score fields:** `posteam_score` and `defteam_score` are the running scores per play. `home_score`/`away_score` from nfl_data_py PBP = FINAL game score stored on every row — do NOT use these for display. Derive running home/away score as: `home_score_now = posteam_score if posteam == home_team else defteam_score`.
- **HistoricalLab silent fetch failure:** If the frontend loads before the backend warms up (~10s on first request due to model + PBP loading), the plays fetch fails silently (`.catch(() => {})`), leaving the chart in "No data" state. Fix: hard-refresh the browser after backend is ready. Permanent fix: add error state + retry button to HistoricalLab.
- **Suggested lines direction:** Lines are converted from offense-reference KEP to home-team reference before display. Home possession → sort descending (optimizing for +). Away possession → sort ascending (optimizing for −). BEST badge = play most beneficial to the possessing team in home-KEP terms.
- **`_run_clock`'s quarter-transition logic (game_engine.py):** was BROKEN for years — `end_game` was computed by re-reading `self.quarter` AFTER it had already been incremented for Q3→Q4 transitions, so those lanes immediately satisfied `quarter >= 4` and the game was marked over the instant Q4 *began*, never simulating any of the 4th quarter. Fixed by capturing `pre_quarter = self.quarter.copy()` before any mutation and using that for both the increment-check and the game-over check. **General lesson: any time a decision needs the PRE-mutation state of an array that gets mutated earlier in the same code path, capture a `.copy()` first — don't re-read the live array.**
- **`_run_clock` runs AFTER the current play's state has already been applied — code inside it must not blindly overwrite that state (Round 13).** The Second Half Kickoff Logic used to unconditionally reset `yardline_100`/`down`/`distance`/`possession_is_away` for any lane crossing into Q3, silently discarding whatever the just-resolved play in `simulate_play_step` had actually produced (real gain, first down, everything) — on roughly half of all simulated games. This is the same root pattern as the two entries above (state set earlier in a step getting stomped by code that runs later in the same step, called from a different method) — **third occurrence of this exact bug shape in this file.** Fixed by deferring to the existing `needs_kickoff`-triggered kickoff-resolution code (which already computes the correct post-kickoff state on its own, the same deferred pattern used for every touchdown/field-goal kickoff) instead of pre-empting it. Caught by `scripts/audit_play_continuity.py` — rerun this after any future change to quarter-transition, clock, or possession-switching logic.
- **Hoisting pattern for Phase-7-referenced masks:** `td_mask`, `turnover_down`, `is_oob`, `has_accepted_penalty` etc. must be initialized to `np.zeros(self.N, dtype=bool)` (or the array their branch would have produced) BEFORE the conditional block that normally sets them, not just inside it — Phase 7 (clock management) references them unconditionally later in the same `simulate_play_step` call, and if the originating branch had zero active lanes this step, an un-hoisted local would raise `NameError`.
- **`clock_stopped` must be reset to `False` BEFORE calling `_run_clock`, not after:** `_run_clock` can internally set `clock_stopped=True` for lanes that cross the two-minute-warning threshold during that call. Code that unconditionally does `self.clock_stopped[mask] = False` *after* the `_run_clock` call stomps on that. Caught as a real bug in the `regular_clock` and OOB `normal_oob` branches — both now reset to `False` before calling `_run_clock`.
- **The completion-probability model's `b0` mathematically cancels out** in the open-field catch path (`game_engine.py`, `is_open` branch). `delta_wr` is built as `logit(zone_baseline) - logit(sigmoid(b0 + b1*adot_val))`, and since `logit(sigmoid(x)) = x`, the `b0` terms cancel completely when substituted into `logit_p = b0 + b1*ay_val + delta_wr + sep_bonus`. **Do not try to "calibrate completion rate by shifting b0" — it's a no-op.** Use a separate, explicitly-labeled probability-space offset instead (see `OPEN_FIELD_CALIBRATION_OFFSET` in the code, added clock_physics_v020 Round 7).
- **QB CPOE must be applied in probability space, not logit space:** CPOE is itself a probability-space quantity (percentage points of completion rate over expected). Adding it inside a logit sum before the sigmoid compresses its effect (~1/3 of intended magnitude at typical completion rates). Apply it as `sigmoid(logit_p) + cpoe/100` after the sigmoid, matching the contested-catch path's pattern, not `sigmoid(logit_p + cpoe/100)`.
- **`docs/boxscores/` is not git-tracked** (confirmed: 0 files under version control there) — local-only generated output, regenerated by `run_weeks_1_to_4_2025.py`. Don't assume it reflects what's on GitHub.
- **`player_df` (from `BatchSimulator.run_batch`) can have multiple rows sharing the literal name `"Defense"`** — both teams' DST unit uses this as a generic placeholder `Player` name. A naive `groupby(['game_id','Player'])` will treat different teams' defenses as duplicates. Always include `Team` in the groupby key when aggregating player stats, or use the existing `StatAggregator` rather than reimplementing aggregation ad hoc.
- **The pace grid (`analyze_clock_pace_grid.py`) must exclude out-of-bounds-terminated plays** from its general pool — OOB is modeled as its own explicit mechanism in `game_engine.py` (roll a probability, draw from the pace pool, subtract a reduction). If OOB plays are also blended into the general pool, the OOB timing effect gets double-counted. This was tried both ways in clock_physics_v020 (see Round 1 vs. Round 2 in the audit doc) — the current, correct state excludes OOB from the pool.
- **`train.py` in `air_yards_v_0_1_1/` and (as of Round 9, historical only) `yac_model_v_0_1_1/` do NOT reproduce the currently-deployed models — and this pattern extends to `rush_yards_v_0_1_0` too.** All three models' `metadata.json` were stamped `"version": "V.0.2.0"` and ship zone-specific `{primary,redzone,goalline}_*` artifacts using DNA-filter features (`cpoe_by_filter`/`target_share_by_filter`/`carry_share_by_filter`) — but the training scripts that originally produced those zone-split artifacts were never committed to this repo. Not lost history: `git ls-files src/nfl_sim/models/` returns zero tracked files, period — nothing under `models/` or `data/` (gitignored outright) has ever been in git, so there was nothing to find. `air_yards_v_0_1_1/train.py` and `yac_model_v_0_1_1/train.py` are leftovers from an earlier, single-model (non-zone-split) generation with a different feature set — their EDA outputs (`docs/eda_outputs/{air_yards,yac}/evaluation_v_0_1_1/`) confirm they really were run once, just superseded. `rush_yards_v_0_1_0/` doesn't even have a leftover script. **`yac_model_v_0_1_1/train_zone_split.py` (Round 9) is now the correct, working replacement for YAC** — it reproduces the deployed zone-split architecture and is safe to rerun. `air_yards_v_0_1_1/train.py` and `rush_yards_v_0_1_0` still have no working equivalent — don't run `air_yards_v_0_1_1/train.py` expecting it to refresh the live model, it would silently replace it with a differently-shaped one. See clock_physics_v020 Rounds 8-9 / AGENTS.md §11.7.
- **`game_engine.py`'s YAC computation bypasses `YACModelV011.predict()` entirely** — it calls `self.registry.yac_model._boosters[zone].inplace_predict(X_yac_zone)` directly with a manually-built feature matrix. The `receiver_dna`/`qb_dna` attributes game_engine.py "injects" into the yac_model instance at init (comment: "Dynamically inject V.0.2.0 player DNA") are dead — `YACModelV011.predict()` reads `self.skill_dna`/`self.qb_dna` (different attribute names) and is never called from the hot path anyway. Not a behavioral bug (the real DNA lookups happen via `self.precomputed_skill_target_share` etc.), just misleading dead code if you go looking for where DNA reaches the YAC model.

---

## 9. EFSD — Next Iteration of KEP (planned, not yet built)

**Problem with current KEP:** Inverts the WP model to find "what kickoff margin yields this same WP?" — it's a nonlinear rescaling of WP. Circular. WP defines WP.

**EFSD = Expected Final Score Differential.** Train an XGBoost regressor to predict `final_home_score - final_away_score` from game state features. The model output IS the metric — no WP in the chain, no inversion step.

**Training recipe (modeled on `train_positional_ep.py`):**
1. Pull multi-season PBP from nfl_data_py
2. Join each play to its game's final score margin (home − away)
3. Features: same as WP model (`score_differential`, `game_seconds_remaining`, `down`, `ydstogo`, `yardline_100`, timeouts, `receive_2h_ko`)
4. Target: `final_home_score - final_away_score` (home-team reference)
5. Train XGBoost regressor. Output file: `src/nfl_sim/models/efsd_v_0_1_0/`

**Key properties:**
- Continuous output despite discrete NFL margins (regression = weighted average, not mode)
- No artificial ±24 ceiling
- Time-invariant bands: EFSD already encodes clock; same EFSD in two different game states = genuinely equivalent expected outcome
- If output looks "stepped" around 3/7/10/14, apply Gaussian target smoothing (σ ≈ 1.5 pts) during training

**Preliminary band hypothesis (validate empirically on holdout set):**
| Band | EFSD | Football meaning | Target win rate |
|---|---|---|---|
| Even | ±3 | Within a field goal | ~50% |
| Advantage | ±3 to ±8 | One possession edge | 60–70% |
| Clear advantage | ±8 to ±16 | Two possessions | 75–85% |
| Decisive | ±16+ | Three+ possessions | 90%+ |

Validation: bucket holdout predictions by EFSD range, check actual win rates. Adjust band boundaries until calibrated.

---

## 10. Generated Artifacts (do not commit to source control)

- `src/nfl_sim/models/positional_ep_v_0_1_0/positional_ep_model.json` — trained model (already committed, update only on retrain)
- `src/nfl_sim/models/clock_pace_v_0_1_0/pace_pools.json` + `metadata.json` — generated by `scripts/eda/analyze_clock_pace_grid.py`; large (~465KB), do not hand-edit
- `docs/audit/v_0_2_0_audit/sim_post_cal_metrics.json` — regenerated every time `run_weeks_1_to_4_2025.py` runs; treat as ephemeral output, not source of truth (the doc's narrative in `docs/audit/clock_physics_v020/README.md` captures the meaningful before/after deltas)
- `docs/audit/v_0_2_0_audit/historical_eda_metrics.json` — regenerated by `run_historical_eda.py`; only changes if the real-data pull or metric definitions change
- `docs/boxscores/week_1..week_18/` — generated per-game boxscores, confirmed NOT git-tracked
- `data/interim/sim_results_2025_{games,players}.parquet` — the full 18-week/272-game/N=1000 season dataset (Round 16), regenerated by `scripts/simulation_runners/run_full_season_sim_2025.py`. This is what `src/api/app.py` serves by default (`/api/week_projections`, `/api/games`, etc.) — regenerate after any `game_engine.py` change that should be reflected in the frontend tools. Also not git-tracked (`data/` is gitignored). `data/interim/week_N_full_projections.json` self-heals (recomputed automatically when the parquet's mtime is newer), don't hand-edit.
- `src/nfl_sim/models/{yac_model_v_0_1_1,air_yards_v_0_1_1}/_train_cache.parquet` — cached prepared training dataframes written by each folder's `train_zone_split.py` to speed up repeat runs (objective/feature experiments); delete freely, regenerated from a live `nfl_data_py` pull on next run without it
- `frontend_analysis/node_modules/`, `frontend/node_modules/` — npm installs
- `venv/` — Python virtual environment

---

## 11. Game Engine — Clock Physics & Passing Model (clock_physics_v020, active development)

**Full history:** [docs/audit/clock_physics_v020/README.md](docs/audit/clock_physics_v020/README.md) — 7 rounds, read before touching any of this. Triggered by a roadmap review noticing "we might be missing a couple plays a game."

### 11.1 What's been built (current state of `game_engine.py`'s clock model)

- **Pace model:** `_sample_pace_runoff()` bootstrap-samples snap-to-snap runoff from real empirical pools (`clock_pace_v_0_1_0`), keyed by (quarter/time-window × score-margin tier), replacing an old flat `randint(18,30)` that was measurably wrong (real running-clock mean is ~32-38s, not ~23.5s). Currently has a cumulative **+2s calibration nudge** baked into the return value (Rounds 3-4), justified by residual play-count overshoot, not a real-data measurement.
- **Out-of-bounds:** explicit per-play roll (situational rates by quarter/score-margin, not flat — see the doc's Round 4 table), with two regimes: outside the last 2:00 of Q2 / 5:00 of Q4, OOB shaves 1-5s off the normal pace draw (play-type-specific); inside that window, full stop (clock holds until snap, confirmed via web search against the actual NFL rule).
- **Two-minute warning:** automatic free stoppage, clamped exactly to 2:00 remaining the first time either quarter's clock would cross below it. `self.two_minute_warning_used` resets on quarter transition.
- **Squeeze plays: removed (Round 10).** Used to let teams rush the snap to deliberately fit an extra play in before the warning (Cam's original explicit spec — Q2 flat 95%, Q4 leading 0%/trailing 2+ scores 100%-50%/tied-or-trailing-1-score 30% baseline). Cam decided this was "greedy for an extra play" and had it removed — regular-clock plays in the warning zone now just use the normal `_sample_pace_runoff` pool, which already reflects real hurry-up pace via its own fine-grained time-window buckets. See §11.8.
- **Clock-stop bug fixes:** scoring plays, turnover-on-downs, punts, and field goals now correctly stop the clock (several of these were silent bugs before this session — see §8 fragile-areas entries).
- **The Q4 game_over bug** (see §8) meant the engine never simulated a 4th quarter before this session. This was the single largest fix in terms of impact.

### 11.2 What's been built (completion-rate / passing model, Round 6-7)

- **Screen catch rate recalibrated:** was hardcoded 80%(RB)/95%(WR-TE); real data showed 82.68%/80.01%. Now 83%/80%. The 95% figure was the dominant driver of the whole completion-rate overshoot.
- **QB CPOE fixed to apply in probability space**, not logit space (see §8 — this was silently compressing QB skill differentiation to ~1/3 of intended effect).
- **Baseline calibration offset** (`OPEN_FIELD_CALIBRATION_OFFSET = 0.075`) added to close the remaining gap to the confirmed real 2025 completion-rate target (**64.40%**, computed from real season QB stats, weighted CMP/ATT). Landed at 64.15% — within 0.25pp.
- **Deep-zone completion and zone distribution (screen/standard/deep %) were already accurate** — confirmed via real-data comparison, not touched.

### 11.3 Known interaction: fixing completion rate moved play count again

Fixing the completion-rate bugs (Round 7) increased offensive/total snap overshoot back up (+4.4%→+6.8%, +3.3%→+6.2%) after Rounds 1-6 had brought it down to +2-4%. Mechanism: fewer completions → more incomplete passes → more plays needed per drive to reach the same eventual outcome. **Not a bug in either fix** — both are independently correct — but it means the play-count tuning is not settled. Re-measure before assuming Round 4-6's pace calibration is still appropriate. (Squeeze-play, mentioned here in earlier drafts, was removed in Round 10 — see §11.8.)

### 11.4 Deferred to v0.4.0 (sacks)

Sacks are empirically fixed as of Round 15 (net sacks/game 2.410 vs. real 2.41, +0.2% — see §11.12) via a calibration multiplier, not an architectural fix. The deeper architectural work is still deferred to v0.4.0, but **the plan changed 2026-07-13, and it's important to be precise about which trench file it touches:**

- **Gate 2 (sacks) already uses `trench_dna.json`'s raw metrics, not `trench_tiers_2025.json`.** `sack_rate_allowed`, `def_pressure_rate`, `def_sack_rate` (Cam's stated priorities) are already live Gate 2 features today (`game_engine.py`, inside the per-play sack-feature block) — no migration needed there. The originally-planned "add explicit trench-tier (pass_block_tier vs. pass_rush_tier) matchup to chaos gate 2" is superseded; Gate 2 was never going to use the tier file anyway.
- **`trench_tiers_2025.json`'s 1-5 tier grades (unclear provenance — see `docs/sims/inputs/README.md` §4.1) feed a *different* part of the engine entirely**: the static run-block/pass-block multipliers computed once at engine `__init__` (`run_diff_*_off`/`pass_diff_*_off` from `run_block_tier`/`pass_block_tier`/`run_stuff_tier`/`pass_rush_tier`), plus the `qb_cpoe_z` "box density" feature re-read live by the rush-yards model. **This is the part Cam wants moved onto raw `trench_dna.json` metrics instead** (or an equivalent continuous replacement) — separate work from the sacks model, likely bundled with a rush-yards retrain rather than Gate 2.
- **`avg_time_to_throw_sec`/air-yards variables** are accepted as a proxy for time-to-throw/time-to-pressure in the absence of real pressure-timing data (see the TTT→air-yards idea, `FUTURE_DEVELOPMENT.md`).
- A **new candidate Gate 2 feature, `blitz_rate`, is real and buildable** (via `nfl_data_py.import_ftn_data()`'s `n_blitzers` field, 2022+) but deliberately deferred until more seasons accumulate (~2027-2028) — see `FUTURE_DEVELOPMENT.md`'s "Trench Data Pipeline" section for the full investigation.

Bundled with the injury/administrative-stoppage work below since both are v0.4.0 scope.

### 11.5 Ruled out (don't re-investigate without new evidence)

- OOB reduction magnitude being wrong — confirmed real via play-type-controlled comparison (runs save ~2.18s, passes ~4.24s going OOB), not a confound.
- Yards-per-play / big-play rate being systematically off — checked directly (pre-Round-8), both matched real closely; the completion-rate problem was the actual issue at the time, not yardage. **Superseded by Round 8**: yards-per-*completion* (a different cut) is off — YAC under-prediction, see §11.7. This wasn't visible until completion rate was fixed in Round 7.
- Drive count being wrong — matched real almost exactly (21.31 vs 21.70/game); the play-count overshoot lives entirely in plays-per-drive, not drive frequency.
- "b0 shift" as a completion-rate calibration lever — mathematically a no-op, see §8.

### 11.6 Deferred, parameters agreed (throwaway mechanism)

Target: ~5% of all dropbacks should be throwaways, 75% pressure-related / 25% clean-pocket-no-good-look. Real-data EDA to pin down the exact rate hit a wall — nflfastR's play descriptions only inconsistently flag throwaways in text, undercounting badly with no reliable numeric heuristic to fill the gap. Current mechanism (`is_throwaway = real_sacks & rand<0.20`) only fires as a sack-escape — needs a clean-pocket path independent of `sack_prob` added.

### 11.7 Rounds 8-9: YAC model under-predicts — diagnosed and fixed

Full writeup: Rounds 8-9 in [docs/audit/clock_physics_v020/README.md](docs/audit/clock_physics_v020/README.md). Summary:

- Round 8 diagnosed a −14.4% overall YAC shortfall (worst on deep balls and screens) plus an opposite-direction goalline/redzone over-prediction, and found the deployed YAC/air-yards/rush-yards models all have the same problem: no training script in the repo reproduces them (`yac_model_v_0_1_1/train.py` and `air_yards_v_0_1_1/train.py` train a different, non-zone-split architecture; `rush_yards_v_0_1_0` has no training script at all). Turned out this isn't lost history — `git ls-files src/nfl_sim/models/` returns zero tracked files, ever; nothing under `models/` or `data/` was ever committed, so there was never a commit to find.
- Round 9 wrote a new script, `src/nfl_sim/models/yac_model_v_0_1_1/train_zone_split.py` (live `nfl_data_py` pull + `data/dna/*.json` joins, 2020+, `GroupShuffleSplit` by `game_id`, 70/15/15), and found **two compounding root causes, neither of them a feature gap**:
  1. **Objective function.** The model (and the orphaned original) used `reg:absoluteerror` (median-targeting). YAC is right-skewed, so a median-fit model systematically undershoots the mean — and the mean is what season-long aggregates depend on. Switching to `reg:squarederror` closed almost the entire gap on its own (primary zone: −30.9% → −0.9% on held-out test data).
  2. **Noise-floor inflation**, only visible once the model itself was fixed: `game_engine.py`'s post-model noise injection used a flat minimum scale (6.0 yards) regardless of context. Combined with flooring negative draws at zero, this mechanically inflates the mean wherever real YAC is small — goalline (~0.9-yd mean) got hit hardest (+237% over real, barely moved by the model fix alone), redzone less so, primary negligible. Fixed by scaling the noise to the play's own predicted YAC (`base_scale = max(0.75, yac * 1.25)`) instead of a flat constant.
  3. One experimental feature added and kept: `room_after_catch` (`yardline_100 − air_yards`, i.e. distance from the catch spot to the goal line) — meaningful feature importance (13-88% depending on zone), helps the goalline/redzone case specifically.
- Player-skill traits (elusiveness, broken_tackle_rate, top_speed_mph, avg_separation_yds) were deliberately left out this round — checked NGS coverage first (`avg_separation` has solid history back to 2016 for qualifying pass-catchers; `top_speed_mph` isn't in the standard NGS dataset at all and its `skill_dna.json` source is unclear) — situational-only features for now, `avg_separation_yds` flagged as worth revisiting given the coverage check came back better than expected.
- **Result:** overall YAC −14.4% → −1.6%; pass yards/game −13.0% → −6.2%. **But offensive/total snap-count overshoot did NOT improve** (+6.8%→+7.2%, +6.2%→+6.6%) — the hypothesis that fixing YAC would also fix snap-count was only half right; it's now confirmed a separate problem. See §11.8 (Round 10) for what was tried next.

### 11.8 Round 10: Squeeze-Play Removed — Partial Fix for Snap-Count

Full writeup: Round 10 in the audit doc. Squeeze-play mechanic removed entirely (Cam's call — realistic hurry-up pace stays via the empirical pace pools' own time-window buckets, but no more deliberate "guarantee an extra down" roll). Measured, isolated effect: offensive snaps +7.2%→+5.4%, total snaps +6.6%→+4.7% (~2.3-2.9 fewer combined plays/game — Cam's "a couple of plays" instinct was right, more accurate than Round 5's rougher estimate). **~6-7 combined plays/game still unexplained.** Ranked list of untried options: (1) re-measure plays-per-drive vs. drive count now that completion rate/YAC/squeeze have all changed since Round 5's measurement, (2) air_yards retrain (still −4.4% to −7.3% short, same template as `train_zone_split.py`), (3) sacks recalibration (deferred v0.4.0, currently −29% under real — more sacks could shorten drives), (4) revisit the Round 3-4 pace-pool "+2s calibration nudge" now that upstream models are far more accurate than when it was added, (5) turnover rate (interception gate has a flat `*0.80` scalar, not re-checked recently), (6) administrative stoppages (deferred v0.4.0 — consumes clock without being a play, a direct lever for this exact problem).

### 11.9 Rounds 11-12: Air Yards Retrain + Sacks Diagnosis

Full writeup: Rounds 11-12 in the audit doc. Air yards: same fix pattern as YAC (`air_yards_v_0_1_1/train_zone_split.py`, live pull + DNA joins + grouped split), but the orphaned original already used mean-targeting, so no big win — pass yards/game and snap count both stayed flat. Real bug found and fixed along the way: goalline's 3-way gate has no "deep" class in training data at all (physically can't legally gain 20+ air yards from inside your own 5), which crashes both explicit and auto-inferred `num_class`; fixed with a per-zone/per-level fallback-donor system (`fallback_donor` map in the script) — **the donor must be the adjacent zone by field position** (`redzone` for `goalline`), not a blanket `primary` fallback, which was tried first and made goalline air_yards measurably worse (primary's gate has never seen a play from inside the 5 and extrapolates badly on `yardline_100`).

Sacks: diagnosis only this round, no fix yet. Three findings: (1) `data/dna/trench_tiers_2025.json` already has the `pass_block_tier`/`pass_rush_tier` matchup data the v0.4.0 plan wanted, sitting unused; (2) gate 2's `off_sack_rate_l4`/`def_sack_rate_l4` features are fed literal duplicates of season-aggregate values at inference (`game_engine.py`'s Gate 2 feature construction), not real recent-form data; (3) most promising lever — after gate 2's raw sack probability rolls, ~24% of resulting "sacks" get converted to QB scrambles/throwaways before being counted (`(1-0.05)×(1-0.20)≈0.76`), closely matching the observed −29-31% shortfall. Recommended order: test the diversion hypothesis first (cheap, no retrain), then the L4 fix, then the trench-tier retrain.
- New additive instrumentation in `game_engine.py` (same pattern as the existing Positional Evaluator Hook, §8): `last_play_pre_yardline_100`, `last_play_is_complete_pass`, `last_play_yac`, `last_play_gain`. Diagnostic script: `scripts/eda/analyze_pass_yardage_breakdown.py` (drives `NFLGameEngine` directly, not `BatchSimulator`, since the latter doesn't expose per-play internals) — rerun this after any future YAC/air-yards change to confirm the fix held.
- `yac_model_v_0_1_1/train.py` (the orphaned single-model script) is still in the folder alongside the new `train_zone_split.py` for historical reference — don't confuse the two; `train_zone_split.py` is what produces the currently-deployed `{primary,redzone,goalline}_yac_reg.joblib` files.

### 11.10 Round 13: Quarter-Transition State Corruption (found and fixed)

Full writeup: Round 13 in the audit doc. Triggered by Cam spotting repeated identical down/distance/yardline lines (different players/gains) in a readable single-game play-by-play and asking for a real audit rather than a clock-physics explanation — it was a real bug.

`game_engine.py`'s Second Half Kickoff Logic (inside `_run_clock`, called after the current play's real state has already been applied for the step) used to unconditionally overwrite `yardline_100`/`down`/`distance`/`possession_is_away` for any lane crossing into Q3 — discarding the just-resolved play's real result on roughly half of all simulated games (every game hits the Q2→Q3 boundary exactly once; the bug fires whenever that specific step is a real snap rather than a kickoff/kneel). It was also redundant: the kickoff-resolution code already computes the correct post-kickoff state on its own next call, the same deferred pattern used for every touchdown/field-goal kickoff — the fix was to stop pre-empting that and just set `needs_kickoff=True`, same as everywhere else. A second, smaller fix was needed for possession specifically: a score or turnover-on-downs landing on that same boundary play already correctly flips possession earlier in the step, so the forced "home team receives" default now only applies when possession is unchanged from the start of the step (tracked via a new `self.step_start_possession_is_away` snapshot, since `_run_clock` is a separate method and needs an instance attribute to see it).

New permanent regression test: `scripts/audit_play_continuity.py` — checks that field position always moves by exactly the recorded gain on a possession-unchanged, non-sack/interception/lost-fumble/penalty play. Went from 95 violations (200 games) to 1 (1000 games, and that 1 is a diagnostic-only artifact of stale hooks on an early-return code path, not a real bug). **Season-level snap counts barely moved** (offensive +5.2%→+5.2%, total +4.6%→+4.5%) — the bug replaced "rest of a truncated drive" with "one phantom fresh drive," which nets out close to a wash. Doesn't explain the still-open overshoot (§0/§11.8's options list stands), but was a real correctness bug worth fixing on its own terms.

New additive hooks from this round's investigation (also used by the new readable play-by-play script): `last_play_target_name`, `last_play_rusher_name`, `last_play_is_normal_play`, plus `self.step_start_possession_is_away` (not a `last_play_*` hook — a per-step snapshot consumed by `_run_clock`). New script: `scripts/print_readable_play_by_play.py` — football-narrative play-by-play for a single game (N=1), unlike `print_play_by_play_v020.py`'s raw state-diff log. Known limitation: its 4th-down-decision labeling can show a hypothetical "run/pass for N yards" when the real outcome was a kick — the decision engine appears to evaluate a hypothetical go-for-it option internally even when it decides to kick, and the narrator picks up that hypothetical. Not fixed this round (cosmetic, script-only).

### 11.11 Round 14: Sacks Diversion Hypothesis — Tested

Full writeup: Round 14 in the audit doc. New diagnostic counters `self.gross_sacks` (gate 2's raw `has_sack` roll) and `self.net_sacks` (`actual_sacks`, post-diversion) — whole-game, both teams combined, divide by 2 for a per-team-game figure comparable to real. New script: `scripts/eda/test_sacks_diversion_hypothesis.py`.

Result: gross (pre-diversion) 2.094/game vs. real 2.41 (−13.1%); net (post-diversion) 1.661/game vs. real 2.41 (−31.1%). The diversion pipeline removes 20.7% of gross — closely matching Round 12's theoretical ~24% estimate, so that part of the hypothesis holds. But the raw model is *also* independently under-calibrated (−13.1% before any diversion at all) — the "raw model is already correct, diversion is pure redundant double-counting" framing from Round 12 was too clean. Two compounding gaps, not one: removing/reducing the diversion would only close about half the shortfall; the raw model needs calibrating up too (probability-space offset, or fixing the L4 duplicate-feature bug / adding trench-tier matchup data, both still on the table from Round 12). Nothing changed in the engine this round beyond the two diagnostic counters — decision point for Cam on which lever(s) to pull next.

### 11.12 Round 15: Sacks Fixed

Full writeup: Round 15 in the audit doc. Two changes in `game_engine.py`'s Gate 2 block: throwaway-diversion rate `0.20 → 0.06` (the flat arbitrary constant; `scramble_rate`, real per-QB roster data, left untouched), plus `SACK_PROB_CALIBRATION_MULT = 1.24` multiplicatively applied to gate 2's raw `sack_prob` (same pattern as gate 4's existing `* 0.80`). Tuned empirically via `scripts/eda/test_sacks_diversion_hypothesis.py` — a first pass at 1.30 overshot (net 2.528, +4.9%), scaled down to 1.24 landed at 2.410 vs. real 2.41 (+0.2%).

Season-level cascading effects (full batch audit): sacks/game −29.1%→+0.2%; offensive snaps +5.2%→+4.2%; total snaps +4.6%→+3.8% (a bonus improvement, not the target of this round); completion rate 64.37%→64.81% (fewer zero-yard throwaway "attempts" now that more convert to sacks — minor, not chased further); pass yards/game −8.1%→−10.2% (sacks now correctly subtract real negative yardage instead of some being zero-yard throwaways — an expected trade-off tied to the already-open air-yards gap from Round 11, not a new bug).

**Deliberately deferred to v0.4.0, not fixed this round:** the L4 duplicate-feature bug (Round 12 Finding 2) and the trench-tier matchup addition (Round 12 Finding 1, the original plan's ask) — both need real per-week trench data that doesn't exist in the current pipeline (`trench_dna.json` is season-level only, no week granularity), so fixing the L4 issue properly is the same data-build as the tier-matchup work. Also still separately deferred: the throwaway-mechanism rework itself (§11.6/Round 6, target ~5% of all dropbacks with a clean-pocket path) — cutting the flat throwaway rate today moves further from that eventual target; reconcile the two when that work happens.
