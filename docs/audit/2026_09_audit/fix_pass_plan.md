# Fix-Pass Plan — 2026-09 Audit

Per-finding: **where it's from**, **why we found it** (the evidence), **how to
fix it** + reasonable alternatives. Ordered in the batch sequence from
`phase_7_synthesis.md` §3. Each `###` item is roughly one commit.

Legend: 🔴 blocker · 🟠 significant · 🟡 minor · ⚪ note
Effort: XS (<15 min) · S (<1 hr) · M (half day) · L (multi-day)

Already done this session: **S2-1** (iteration_range), **S2-2** (kickoff
returns), **week-aware toggle** (playoff crash workaround).

**2026-09-22 pass (mid-season revisit):** Cam scoped this pass to Batches
**A, C, H only** — B (dead-code removal), D/F (engine tests + validation
benchmark), and G (publish-prep) + both Cam-gated items are deliberately
deferred until later in the season, not skipped. Within scope: A1/A2/A3 done
(A1's OneDrive move had actually already happened; only cleanup + the
`_require_json` fail-loud work + the B2 backup script were left), C1/C2/C3
done, H1/H2/H4 done, H3 partially done (root doc moves + an expanded
absolute-path sweep; the `docs/boxscores` gitignore+delete call and the
`docs/` grab-bag folding are still open, deliberately not done without
Cam's sign-off on deleting 82 tracked files). See WORKLOG.md 2026-09-22
entry for the full detail and verification.

---

## Batch A — Safety (do first, before any calibration or benchmark)

### A1 · ✅ DONE 2026-09-22 · 🔴 S2-5 — get the repo out of OneDrive sync · effort S
- **From:** Phase 3. **Why found:** a full-tree scan turned up `*-Cams-Desktop`
  conflict copies in 4 locations — `data/interim/` parquets,
  `src/nfl_sim/__pycache__/*.pyc`, `.py` files inside `venv_py38_old/`, and
  **`.obsidian/workspace.json` conflicted 8 separate times**. OneDrive appends
  the machine name when it can't reconcile an edit made on two synced machines.
  It's failing to sync this folder cleanly on a regular basis; luck alone has
  kept it off a roster JSON or a source file.
- **Fix (recommended):** move the working copy to a non-synced path
  (`~/dev/NFLSims` or `C:\dev\NFLSims`), `git clone` there or `git remote` the
  existing one, and use **GitHub as the sync mechanism** between machines. Git
  is the correct tool for this. Delete the existing `*-Cams-Desktop.*`
  artifacts. Add `*-*-Desktop.*` and `* (1).*` to `.gitignore` as a tripwire.
- **Alternative:** keep the folder where it is, exclude it from OneDrive sync
  (OneDrive settings → choose folders). *Rejected* — OneDrive has silently
  re-added excluded folders on client updates before, and it doesn't fix the
  two-machine workflow.
- **Alternative:** OneDrive "Files On-Demand" + mark always-local. Doesn't stop
  conflict-copy creation, just changes where bytes live.

### A2 · ✅ "now" fix DONE 2026-09-22 (real builder still open) · 🔴 S2-6 — back up the un-regeneratable assets · effort S now / M later
- **From:** Phase 3. **Why found:** `data/processed/hardened_pass_training_master_v2_5.csv`
  is **308 MB**, gitignored, and has **no committed builder script** (consumed
  by `efsd`/`positional_ep` training + `script_chainer.py`). The R model
  `.rds` train/val/test splits (`data/{win_probability,field_goal,fourth_down}/`)
  are likewise gitignored and are the only record of how those models were
  built. Disk failure or a OneDrive corruption (see A1) = 4 model families and
  the pass pipeline can't be rebuilt.
- **Fix (now, S):** `scripts/data_utils/backup_critical_assets.py` that pushes
  the un-tracked un-regeneratable set to a **Cloudflare R2 or Backblaze B2**
  bucket (10 GB free, zero egress — same host the `serverless_parquet_datalake.md`
  plan picks). Run it after every material change to those files.
  **Cam chose Backblaze B2 (2026-09-22).** Script written and dry-run
  verified (finds all 13 files, ~295 MB) — needs Cam's own B2 account/bucket
  + `B2_APPLICATION_KEY_ID`/`B2_APPLICATION_KEY`/`B2_BUCKET_NAME` env vars
  before the first real upload; the script refuses to run with a clear error
  if they're unset, never silently no-ops.
- **Fix (real, M):** write the builder for `hardened_pass_training_master`
  (or precisely document its assembly), so it becomes regeneratable like the
  DNA files — then gitignoring it is safe because the recipe is committed. The
  R `.rds` splits fold into the "retrain the 4 R-era models before 1.0"
  roadmap item.

### A3 · ✅ DONE 2026-09-22 · 🟠 S2-3 — fail loud on missing critical inputs · effort S
- **From:** Phase 1. **Why found:** `game_engine._load_json` / `batch._load_json`
  return `{}` for a missing file; `model_registry.load_all` skips a missing
  model dir leaving the attribute `None`; `predict_*` fall back to `0.58` /
  `0.0` / league-average. The AGENTS.md history is a catalogue of this failure
  mode running undetected for months (the `proe` field silently reading 0.0
  for every team; the stale `ModelRegistry` singleton; sacks jumping to 6.4/g).
- **Fix:** a `_require_json(path)` helper used for the non-optional inputs (both
  DNA files, both rosters, `team_to_coach`, trench calibration, every model
  dir) — raises at `NFLGameEngine.__init__` naming the missing path. Keep the
  soft `_load_json` only where a missing value is genuinely expected (a player
  absent from a DNA file → career-average fallback is *correct*).
- **Alternative:** a startup self-check script run before a sim batch. Weaker —
  doesn't protect the API or an ad-hoc `BatchSimulator()` call.

---

## Batch B — Dead-code removal (unblocks the cleanup and the rename)

### B1 · 🟡 S3-4 + S3-10 — delete the sequential-engine path · effort S
- **From:** Phases 1 & 2. **Why found:** every `run_batch` call site in the repo
  passes `vectorized=True` (or the default, which is `True`). That makes
  `batch._simulate_single_game_worker`, `legacy.game_engine_sequential.SequentialNFLGameEngine`,
  the **unconditional** `from legacy.game_engine_sequential import …` at
  `batch.py:21`, and `model_registry.predict_play_selection_proba` (+
  `get_bucket_name`, `get_zone`) all dead. The sequential engine has also
  *diverged* — no OT, no Round-13 fix, none of the 2026 calibration — so
  flipping `vectorized=False` to "debug" would silently give wrong results.
  `predict_play_selection_proba` additionally builds a **10-feature** vector for
  a **9-feature** model (it includes `proe_by_filter`, which the model
  deliberately excludes).
- **Fix:** delete the worker + the import; drop the `vectorized` param (or make
  `False` raise `NotImplementedError`); delete `predict_play_selection_proba` /
  `get_bucket_name` / `get_zone` from `model_registry.py`; then delete
  `legacy/game_engine_sequential.py`. This unblocks the wider `legacy/` sweep
  (B3) and the filename rename (D0).
- **Alternative:** keep the sequential engine as a reference oracle for tests.
  *Rejected* — it's too stale to be a valid oracle; a frozen golden-output
  fixture is better and comes free with B... actually with Batch D.

### B2 · 🟡 S3-5/6/7/8 — small engine + batch cleanups · effort S
- **From:** Phase 1.
- **S3-5** `batch._build_slot_map` distinguishes TE from WR with a **hardcoded
  ~27-surname set** — but the 2026 roster JSONs carry a real `pos == 'TE'`
  (verified). Any TE not on the list (every rookie, anyone traded in) is
  slotted `WR{n}`, corrupting the analytics position tables and the DFS
  optimizer's positional constraints. **Fix:** `is_te = p_traits.get('pos') == 'TE'`.
  Delete the set.
- **S3-6** module-level `warnings.filterwarnings('ignore')` at `batch.py:18`
  silences *all* warnings for any process importing `batch` — including numpy
  divide-by-zero / overflow in the completion / logit math. **Fix:** scope to
  the specific noisy categories, or move into the function that needs it.
- **S3-7** `model_registry.get_zone` is a 6th independent copy of the
  goalline/redzone/primary split AGENTS.md says was consolidated into
  `_classify_zone`. Dies with B1 (it's on the dead scalar path); confirm no
  others survive.
- **S3-8** `BatchSimulator._json_cache` (class-level) is never mtime-checked —
  a long-lived API process serves stale DNA after a `refresh_weekly_dna` run
  until restart. **Fix:** add an mtime check (cheap), or a `ModelRegistry`-style
  explicit invalidate hook.

### B3 · 🟡 dead-file sweep · effort S
- **From:** all phases (consolidated Phase 6 §5).
- Delete: `legacy/` (after B1 removes the one live import), root `reports/`
  (21 stale files), empty `Notebooks/` + `artifacts/`, `venv_py38_old/`,
  `scripts/data_utils/cleanup_data_dir.py` (spent one-time migration),
  `data/interim/_pre_2026-07-21_regen_backup/` + `_stale_backup_pre_full_season_regen/`,
  the `*-Cams-Desktop.*` artifacts, `R/bots/logs/*.log` (17 files),
  `R/scripts/play_selection_*_v_0_1_0.R` (8, superseded by the Python port),
  empty `R/logs` `R/outputs` `R/reports` `R/tests` dirs,
  `scripts/roster_management/compileRosters.js` (lone JS, verify first),
  stale `.pyc` in `src/*/__pycache__/`.
- Add to `.gitignore`: `R/bots/logs/*.log`, `R/bots/live_csv/`,
  `docs/boxscores/` (see F-docs), `*-*-Desktop.*`.
- **Why safe:** git history keeps everything (`git log --all -- <path>` /
  `git show <sha>:<path>`). This is the whole point of the "no `legacy/` folder"
  standard.

---

## Batch C — Tooling foundation

### C1 · ✅ DONE 2026-09-22 · 🟡 pyproject.toml + editable install · effort S
- **From:** Phases 0 (S0.3), 3 (S3-16), 6 (S3-32). **Why found:** no
  `pyproject.toml` / `pytest.ini` / `setup.cfg` anywhere. Consequences: bare
  `pytest` (no path arg) wanders into `scratch/` and `legacy/` and **errors
  out** on a dead `127.0.0.1:8000` request at import; ~10 sim runners do
  `sys.path.append(os.getcwd())` so they only work from the repo root; `src/`
  isn't an installable package.
- **Fix:** a `pyproject.toml` with `[project]` metadata, `[tool.pytest.ini_options]
  testpaths = ["tests"]`, a `[tool.ruff]` block, and `[tool.setuptools] packages`.
  `pip install -e .` in the venv. Then delete the `sys.path` hacks and import
  `nfl_sim` / `src` cleanly everywhere.
- **Alternative:** just add `pytest.ini` with `testpaths` (fixes the wander,
  not the `sys.path` problem). Do the full `pyproject.toml` — it's the same
  effort and fixes both.

### C2 · ✅ DONE 2026-09-22 · 🟡 S3-24 — CI that runs the tests · effort S
- **From:** Phase 4. **Why found:** the only two GitHub workflows are for the
  4th-down bot. **Nothing runs the 97-test suite on push/PR**, and there's no
  build check. The audit is about to land a batch of engine changes with no
  gate.
- **Fix:** `.github/workflows/ci.yml` — on push/PR: `pip install -e .[dev]`,
  `pytest tests/ -q`, then `cd frontend && npm ci && npm run build` and the
  same for `frontend_analysis`. ~30 lines.

### C3 · ✅ DONE 2026-09-22 (manifest only, isolation deliberately not activated -- see WORKLOG) · 🟡 S3-26 — real `renv.lock` for R · effort S
- **From:** Phases 0 (S0.4), 5. **Why found:** `DEVELOPMENT.md` notes "no
  `renv.lock` committed"; the de-facto R manifest lives inside
  `.github/workflows/nfl_live.yml`'s `extra-packages:` list — not
  version-pinned, not usable locally, easy to drift from what the scripts
  actually `library()`. The list also omits `xgboost` (the `.rds` models need
  it — verify).
- **Fix:** `renv::init()` in `R/`, commit `renv.lock`, point the CI workflow at
  `renv::restore()` instead of the hand-list.

---

## Batch D — Engine test suite  ← highest-value org fix

### D1 · 🟠 S2-13 — deterministic invariant tests for `game_engine.py` · effort M
- **From:** Phase 6. **Why found:** the 3,377-line core, the most-changed file
  in the repo, has **zero direct unit tests**. Every calibration round and
  every audit finding has been "run it and eyeball the numbers." 97 tests exist
  but they cover the periphery (DNA blend, week roster, positional eval, live
  pipeline).
- **Fix:** `tests/test_game_engine.py` — seed `np.random`, run
  `NFLGameEngine(away, home, N=200).run_game()` for 2–3 fixed matchups, assert
  invariants that **have actually broken before** (per AGENTS.md / WORKLOG):
  1. field position moves by exactly `play_gain` on a possession-unchanged
     normal play (the Round-13 bug — promote `scripts/audit_play_continuity.py`
     to pytest)
  2. no lane ends `simulate_play_step` with unresolved `time_remaining < 0`
  3. score deltas ∈ {0, 2, 3, 6, 7, 8} only
  4. OT is reachable; a regulation tie is never recorded as a home win
  5. `pAtt` + `sacks` reconcile with the count of `is_pass` decisions (±scrambles)
  6. league sacks/game ∈ [2.0, 3.0] and completion% ∈ [62, 68] (a **drift
     guard** on the calibration constants — the exact silent-failure class
     AGENTS.md keeps describing)
  7. return TDs/game < 0.1 (locks in the S2-2 fix)
- **Alternative for #6:** a separate slow `@pytest.mark.slow` benchmark test
  (N=1000, 8 matchups) run in CI nightly not per-push. Do both — a fast band
  check per-push, the fuller one nightly.

---

## Batch E — Engine calibration + storage (needs D1's floor first)

### E1 · ✅ DONE 2026-09-09 · S2-4 — retrain play-selection with a dropback label · effort M
- **From:** Phase 2. **Why found:** `train.py:134` uses
  `is_pass = (play_type == "pass")`, which in `nfl_data_py` excludes scrambles
  (those are `play_type == "run"`). So the model's 57.5% is *already* net of
  scrambles. But the engine (`game_engine.py:1602`) treats every `is_pass`
  decision as a **dropback** and rolls ~5% of them into scrambles — subtracting
  scrambles a **second time**. Measured: realized box pass rate **0.540 vs real
  0.567** (−2.7 pp), QB rush attempts ~0.7/game high. This is the residual
  pass-volume gap.
- **Fix (recommended):** change the label to
  `is_pass = (play_type == "pass") | (qb_scramble == 1)` — the model then
  predicts P(dropback) (~59.3% real), and the engine's att/sack/scramble split
  of that is correct. Retrain (`train.py` is committed, feature set unchanged),
  verify per-bucket `pred_pass_rate` still tracks `real_pass_rate`, re-run the
  benchmark. Estimated: closes ~1.0 of the 1.4 att/game gap.
- **Alternative:** leave the model, inflate the engine's pass probability by
  `1 / (1 - scramble_rate)` before the coin flip. *Rejected* — pushes a data
  problem into an engine fudge factor, and `scramble_rate` is per-QB and
  applied downstream so the math gets awkward.
- **Alternative:** accept it. Cam has said the current slight under-count is OK
  for week 1 / season projections. Defensible to defer — but it's a clean
  retrain and it's the structural cause, so do it while we're in here.

### E2 · 🟡 S3-12 — one parquet writer + provenance sidecar · effort S
- **From:** Phase 3. **Why found:** `run_full_season_sim_{2025,2026}.py`,
  `run_weeks_1_to_18_2025.py`, `run_week_1_only_2025.py`,
  `generate_season_leaders.py`, `print_week_1_boxscore.py` **all write**
  `data/interim/sim_results_*.parquet`, some partially. There's no metadata in
  the file to tell what a cache on disk actually contains.
  `regenerate_2026_reports.py` exists *because* this pattern silently desynced
  the standings from the leaders once.
- **Fix:** `run_full_season_sim.py` is the sole writer; on write it stamps
  `sim_results_{year}_meta.json` = `{git_sha, engine_mtime, N, weeks, iso_date,
  week_aware}`. Every other script reads only. Also fixes the "did my engine
  change get picked up" gap we just hit manually (see S3-14).
- Also here: **S2-7** — delete `run_week_1_only_2025.py` (its job is covered by
  `run_full_season_sim_2025.py`) or repoint it to a distinct
  `week_1_only_2025_*.parquet`.

### E3 · 🟡 S3-14 — content-hash cache staleness · effort S
- **From:** Phase 3, and **hit live this session** — after the S2-1/S2-2 engine
  fixes, `_cache_is_stale` returned `False` (it only checks DNA/roster/schedule
  mtimes, not engine code), so a `regenerate` run would have silently reused
  the pre-fix parquet. Had to move the parquet aside by hand.
- Also: `cache_input_globs` includes `data/dna/*.json`, which catches the
  **gitignored, regenerated** `qb_dna.json` etc. — a `regenerate_dna` run bumps
  their mtime even when values are byte-identical → a needless full 272-matchup
  re-sim (~25 min).
- **Fix:** hash the sorted content of the real inputs (`current_rosters/**`,
  `overrides/**`, `trench_dna.json`, `coach_dna.json`, `schedule`) **plus the
  engine file mtime / git SHA**, write it into the E2 sidecar, compare that.
  Narrow the globs to files that actually change the sim.

### E4 · 🟡 S3-13 — collapse the year-hardcoded runners · effort S
- **From:** Phase 3. **Why found:** `run_full_season_sim_2025.py` and
  `_2026.py` are near-identical; same for `run_weeks_1_to_18_2025.py`,
  `run_weeks_1_to_4_2025.py`, `run_first_4_games_2025.py`,
  `run_week_1_only_2025.py`. The 2025 ones are also just stale.
- **Fix:** one `run_full_season_sim.py <year> [--weeks N] [--iters N]`.
  ~5 scripts → 1. `regenerate_2026_reports.py` calls it with `2026`.

---

## Batch F — Validation benchmark  ← the gate

### F1 · run the full regenerate on the post-Batch-E engine, compare to the bar
- **From:** Phase 7 §4. The roadmap's "Full system validation" gate has never
  had numbers. Proposed bar (measured vs real 2023–2025):

| metric | pass if |
|---|---|
| game total pts | ±1.5 of ~44 |
| completion % by depth bucket | every bucket ±2.5 pp |
| box pass rate | ±1.5 pp of ~56.7 |
| sacks/game | ±5 % of 2.41 |
| pass yds/team/g | ±5 % of ~225 |
| top-RB season carries | ≤ ~370 |
| return TDs/game | < 0.1 |
| expected-wins spread (std) | ≥ 2.4 — **Cam accepts miss for 2026** |
| test suite (incl. D1) | green |

- If everything except win-distribution clears → **declare Goal 1 validated for
  2026**, correct `GOAL_TRACKER.md`, and proceed to publish (Batch G). Log
  win-distribution as a documented Tier-1.1 follow-up.

---

## Batch G — Publish-prep (only if F1 clears)

### G1 · 🟠 S2-8 — cap the compute endpoints · effort S
- **From:** Phase 4. **Why found:** `SimulationRequest.iterations: int = 10000`
  with no `Field(le=…)`; `/api/simulate` and `/api/optimize` run the sim/solve
  live. One POST with `iterations: 10_000_000` hangs/OOMs the box.
  `docs/api/production_migration.md` already flags this.
- **Fix:** `Field(ge=1, le=10000)` on `iterations` (+ `solve_iterations`), add
  `slowapi` with `5/minute` on those two routes. Both from the migration doc.

### G2 · 🟠 S2-9 — analytics frontend env-var API base · effort S
- **From:** Phase 4. **Why found:** `frontend_analysis/src/api.js` hardcodes
  `const API_BASE = '/api'` and `vite.config.js` hardcodes
  `http://127.0.0.1:8000` — no way to point at a production API. The **DFS
  site already does this right** (`import.meta.env.VITE_API_BASE_URL ?? …`).
- **Fix:** copy the DFS site's `api.js` + `vite.config.js` pattern. ~20 lines.

### G3 · 🟠 S2-10 — visible error state, not silent mock · effort S
- **From:** Phase 4. **Why found:** `safeFetch` does
  `catch (err) { console.warn(...); return fallbackData; }` — a broken API
  makes Game Center / Live WP render `MOCK_PLAY_BY_PLAY` (fabricated
  Mahomes-to-Kelce plays) as if real, with only a console warning.
- **Fix:** on fetch failure, set an error flag the page renders as "data
  unavailable / reconnecting." Keep the mock path behind `import.meta.env.DEV`
  so local dev without the API still works.

### G4 · 🟠 S2-11 — build the missing `/api/fourth-down-evaluate` · effort S
- **From:** Phase 4. **Why found:** `frontend_analysis/src/api.js:272` POSTs to
  `${API_BASE}/fourth-down-evaluate` — **no such route exists** in `app.py`
  (the real ones are `/api/positional-evaluator` /
  `/api/games/{id}/positional-eval`). The 4th Down Explorer's "input your own
  parameters" sandbox — a headline home-page feature — 404s and shows mock
  results (masked by G3's silent fallback).
- **Fix:** add the route (thin wrapper over the existing 4th-down conversion +
  WP models for an arbitrary state dict), or repoint the frontend at the real
  endpoint if its shape matches. Verify against a known game state.

### G5 · 🟡 S3-22 — gate internal pages out of the prod build · effort XS
- **From:** Phase 4. `pagesConfig.js`'s Testing Lab description literally ends
  *"Remove before launch."* Also hide Live WP / BotFeed for the static v1.
- **Fix:** a `prod: false` flag per page in `pagesConfig.js`, filtered at build.

### G6 · static data build step · effort S
- The 5 season files (`season_summaries_2026.csv`, `team_stats_2026.csv`,
  `season_leaders_2026.json`, `matchup_win_probabilities_2026.json`,
  `teams_data.json`) need to land in the deployed bundle. **Fix:** a
  `frontend_analysis/scripts/bundle-season-data.mjs` (or a prebuild npm hook)
  that copies them into `public/data/`, and analytics `api.js` fetches
  `/data/*.json` directly for the season endpoints (no API needed for the
  static tier).

---

## Batch H — Docs

### H1 · ✅ DONE 2026-09-22 · 🟠 rewrite `README.md` · effort S
- **From:** Phases 0 (S0.1), 6. **Why found:** frozen at 2026-07-15 (predates
  all 2026-season work — no mention of the DNA/override system, week-aware sim,
  the 2026 season sim); still says "one API on 8002"; lines 97–100 use
  `file:///c:/Users/txcwa/…` **absolute paths** (global hard-rule violation).
- **Fix:** rewrite against current reality. Relative links only.

### H2 · ✅ DONE 2026-09-22 · 🟡 fix `DEVELOPMENT.md` + `AGENTS.md` + `GOAL_TRACKER.md` · effort S
- **`DEVELOPMENT.md`** §6 states "*Not git-tracked at all, confirmed:
  `docs/boxscores/week_*/`*" — **82 files ARE tracked** (Phase 6). Also ~5 wks
  stale. Fix the claim, refresh the active-areas list.
- **`AGENTS.md`** §0 is **71 lines of session history** — that's WORKLOG's job.
  §7 says "92 tests" (actual 97). Trim §0 to a short pointer + the
  genuinely-active constraints; bump the test count.
- **`GOAL_TRACKER.md`** — multiple false completes (Phase 6 §3): "Tier 1
  Production Release ✅" (not published), "Full system validation ✅" (7 weeks
  of recalibration since + this audit), "per-subproject roadmaps ✅"
  (`docs/roadmaps/` has 1 of ~4), inconsistent target dates. Do this pass
  **after F1** so it reflects the real validation state.

### H3 · 🔄 PARTIAL 2026-09-22 (root doc moves + an expanded absolute-path sweep done; boxscores gitignore+delete and the docs/ grab-bag folding still open, needs Cam's sign-off) · 🟡 root `.md` + `docs/` consolidation · effort S
- **From:** Phases 0 (S0.5), 6 (S3-34/35/36/37).
- Move the 5 stale root planning docs (`DETAILED_GOALS`, `FRONTEND_GOALS`,
  `FANTASY_BETTING_SITE`, `GAME_ANALYSIS_SITE`, `FUTURE_DEVELOPMENT`) to
  `docs/planning/`. Keep only the 6 canonical files at root (README, AGENTS,
  DEVELOPMENT, WORKLOG, PROJECT_ROADMAP, GOAL_TRACKER).
- `.gitignore docs/boxscores/`, delete the 82 tracked files.
- Fold `docs/todo/`, `docs/prompts/`, `docs/study/`, `docs/metrics/`,
  `docs/optimizations/`, `docs/frontend/` into `docs/notes/` or
  `implementation_plans/`. Delete `docs/reports/test run 1/`.
- Fix the `file:///` absolute paths in `src/live/README.md`.

### H4 · ✅ DONE 2026-09-22 · 🟡 S3-9 — document the inert coach levers · effort XS
- **From:** Phase 2. **Cam's call: keep them, may wire later.** So this is a
  doc task: a header/comment block in
  `data/dna/coach_coordinator_levers_2026.csv` and a line in `AGENTS.md` §8 +
  `docs/models/play_selection_v_0_1_0.md` stating which levers are **live**
  (`proe`, `deep_shot_rate`) vs **staged but unconsumed**
  (`air_yards_tendency`, `screen_rate`, `play_action_rate`, `no_huddle_rate`,
  `rpo_rate`, `conservative_score_bias`) — so nobody hand-tunes a dead knob.

---

## Cam-gated items (decide, then slot in)

### D0 · filename `_v_0_1_0` strip · effort M · **needs Cam's scope call**
- **From:** Phase 6. **Why found:** `git ls-files | grep -E '_v_?[0-9]'` →
  **220 tracked files**. `GOAL_TRACKER.md:28` says applying this was ✅ done
  May 2026 — but the **current** global standard is "no version numbers in
  filenames, versioning is git-native." Cam changed the standard afterward.
- **Options:**
  1. **Grandfather the trained-model dirs** (`air_yards_v_0_1_1/`,
     `chaos_v_0_1_0/` — the version is a real artifact ID, a retrain is a new
     artifact not a git revision) + a `metadata.json` `version` field already
     exists there. **Strip everything else** (~90 code files:
     `scripts/roster_management/*_v_0_1_0.py`, `R/scripts/*_v_0_1_0.R`,
     `src/data_pipeline/*_v_0_1_0.py`, `src/nfl_sim/proe_overlay_v_0_1_0.py`,
     `docs/eda_outputs/*_v010/`). Each gets a `# Status: live | vX.Y.Z | date`
     header instead. One mechanical rename commit (git tracks renames, imports
     updated same commit). ← **recommended**
  2. Strip all 220, model dirs included. More disruptive, and the model
     versioning is genuinely meaningful.
  3. Leave it — grandfather everything, update the global standard to allow the
     `_v_x_y_z` convention for this repo. Least work, but it's swimming against
     Cam's own stated preference.

### S2-12 · which live-bot entry point is canonical · effort M · **needs Cam's call**
- **From:** Phase 5. **Why found:** two entry points —
  `src/live/main.py` (Python; the real Aug 14 log is this) and
  `R/bots/run_live_today.R` (what CI runs). Two **different** posting-policy
  implementations. The R one CI runs has **in-memory cooldowns that reset every
  10-minute invocation** — so rate-limiting doesn't function in the CI model.
  Only saved right now by the Bluesky/Mastodon secrets being commented out
  (dry-run).
- **Options:**
  1. **Python `main.py` canonical** ← recommended. Point `nfl_live.yml` at
     `python -m src.live.main` (`QUICK_POLL_COUNT=1`), keep R only for the sim
     subprocess (`R/simulators/`), retire `R/bots/run_*.R` +
     `R/bots/posting_policy.R` + `R/bots/post_targets.R`. The Python side
     already has the SQLite-persistent state, the tests
     (`test_live_pipeline.py`), and the cleaner structure.
  2. **R standalone canonical.** Give `R/bots/posting_policy.R` a persistent
     store (write cooldown state to a file / the SQLite DB), add R smoke tests,
     retire `src/live/main.py` + `posting_policy.py` + `post_targets.py`.
     More work, less test coverage to build on.

---

## Not in the fix-pass — separate workstreams

`app.py` → routers (S3-17) · Publishing **Option B** (small always-on API,
season start) · retrain the 4 R-era models (fg / 4th-down / wp / play-selection
R-era) before 1.0 · `serverless_parquet_datalake.md` (DuckDB + R2) when
publishing scales · win-distribution compression (std 1.9 vs ~2.7) ·
`SKILL_SHRINK` possession-WR residual (−4–5 pp) · trailing-in-Q4 remaining
~2 pp · leading-team Q4 clock-kill overlay · plays-per-game ~1 % low ·
return-yardage refit for punt / INT / fumble (KO already done) ·
20–30 air-yards completion +2.9 pp.
