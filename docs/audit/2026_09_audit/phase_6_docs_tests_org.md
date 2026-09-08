# Phase 6 — Docs, Roadmap, Tests & Organization

**Scope:** the four root docs + roadmap/tracker accuracy, `docs/` structure,
test coverage, filename conventions, dead files. Medium depth; pulls together
threads from Phases 0–5.

---

## 1. Test coverage — the core engine has no unit tests

97 tests across 6 files, all green. What they cover:

| file | covers |
|---|---|
| `test_dna_blender.py` | `src/data_pipeline/` blend math (pure functions) |
| `test_week_roster.py` | `week_roster_v_0_1_0` resolver |
| `test_apply_preseason_overrides.py` | the `catch_rate` → splits sync |
| `test_positional_evaluator.py` | KEP/EP evaluator |
| `test_efsd_evaluator.py` | EFSD metric |
| `test_live_pipeline.py` | `src/live/` state store + posting policy |

**What has zero direct tests:**

- **`src/nfl_sim/game_engine.py`** — the 3,377-line core, the most-changed file
  in the repo. Only exercised by the manual `run_weeks_1_to_4_2025.py` audit
  driver and the ad-hoc `scripts/eda/` measurement scripts.
- `batch.py`, every model inference wrapper (`air_yards`, `yac`, `rush`,
  `chaos`, `fg`, `wp`, `fourth_down_conversion`), `proe_overlay_v_0_1_0.py`,
  `optimizer.py`, the entire API.

**S2-13:** the engine needs a small deterministic test suite — seed the RNG,
run `NFLGameEngine(N=200)` for one matchup, assert invariants that have
*actually broken before* (from AGENTS.md / WORKLOG):

- final `yardline_100` moves by exactly `play_gain` on a possession-unchanged
  normal play (the Round-13 bug — there's already a `scripts/audit_play_continuity.py`,
  make it a pytest)
- no lane ends a step with `time_remaining < 0` that isn't resolved
- score deltas are only ∈ {0, 2, 3, 6, 7, 8}
- OT is reachable; a regulation tie doesn't become a home win
- `pAtt`/`pCmp`/`sacks` counts reconcile with `is_pass` decisions
- sacks/game and completion% land in a sane band (guards the calibration
  constants against silent drift — the exact failure mode AGENTS.md keeps
  describing)

This is the single highest-value organization fix. Every calibration round and
every audit finding so far has been "run it and eyeball the numbers" — there's
no automated floor.

**S3-31:** wire `pytest` into CI (Phase 4 S3-24) — pointless to have 97 tests
nothing runs on push.

**S3-32:** `pyproject.toml` with `[tool.pytest.ini_options] testpaths = ["tests"]`
so `pytest` stops wandering into `scratch/` and `legacy/` and erroring
(Phase 0 S0.3).

---

## 2. Filename conventions — 220 files carry `_v_0_1_0`, against the current standard

`git ls-files | grep -E '_v_?[0-9]'` → **220 tracked files**.

This was a **deliberate past decision**: `GOAL_TRACKER.md` line 28 —
*"Phase 2: Apply `v_0_1_0` versioning to all active scripts ✅ Completed May
2026"*. The **current global standard contradicts it**: *"Do not put version
numbers in filenames. Versioning is git-native."* (`~/.claude/CLAUDE.md`).

Cam changed the standard after this was done. **This needs a Cam call**, not a
unilateral rename:

- **Defensible to keep:** trained-model directories (`air_yards_v_0_1_1/`,
  `chaos_v_0_1_0/`) — the version is a real artifact identifier, and a
  `metadata.json` `version` field already exists alongside. A model retrain is
  a genuine new artifact, not a git revision of the old one.
- **Should probably lose it:** the ~55 `scripts/roster_management/*_v_0_1_0.py`,
  the `R/scripts/*_v_0_1_0.R`, `src/data_pipeline/*_v_0_1_0.py`,
  `src/nfl_sim/proe_overlay_v_0_1_0.py` — these are code files with one live
  version; the `_v_0_1_0` is pure noise and every one of them would need a
  `# Status:` header line instead (the standard's preferred mechanism).

**Recommendation:** grandfather the model dirs, strip the rest in one mechanical
rename commit (git tracks renames, imports updated in the same commit). ~90
files. Do it as its own commit in the fix-pass, not mixed with logic changes.

---

## 3. Doc accuracy — the four root docs + trackers

| doc | state |
|---|---|
| `README.md` | **stale since 2026-07-15** (predates all 2026-season work); `file:///c:/…` absolute paths on lines 97–100 (hard-rule violation). Rewrite. |
| `AGENTS.md` | current, but §0 is **71 lines of session history** — that's WORKLOG's job. §7 says "92 tests" (actual **97**). Trim §0 to a pointer + the genuinely-active constraints. |
| `DEVELOPMENT.md` | **factual error**: §6 states *"Not git-tracked at all, confirmed: `docs/boxscores/week_*/`"* — they **are** tracked (**82 files**). Also ~5 weeks stale (missing QB swaps, week-aware sim, the 2026-09 calibration batch). |
| `WORKLOG.md` | current, format is good. |
| `GOAL_TRACKER.md` | **multiple false completes** (see below). |
| `PROJECT_ROADMAP.md` | header "Last Updated: June 2026" (file touched 2026-09-08). |

### GOAL_TRACKER false completes (also feeds Phase 7 drift)

| row | marked | reality |
|---|---|---|
| "Tier 1 Production Release" | ✅ Completed Sept 2026 | not published; engine in active calibration |
| "Full system validation vs 2024 actuals" | ✅ Completed Aug 2026 | the last 7 weeks were recalibrating core outputs |
| "Create per-sub-project roadmaps (`docs/roadmaps/`)" | ✅ Completed June 2026 | `docs/roadmaps/` has **1 file** of ~4 |
| "Add strict I/O/Purpose docstrings to all `v_0_1_0` functions" | 🔄 In Progress June 2026 | still partial (AGENTS.md: `optimizer.py` 10 fns / 0 documented) |
| target-date column | various | internally inconsistent (rows dated "Feb 2027" / "Oct 2026" marked ✅) |

**S3-33:** a pass over `GOAL_TRACKER.md` to reflect actual state. The tracker's
value is that it's trustworthy at a glance; right now it isn't.

### Stale root planning docs

`DETAILED_GOALS.md`, `FRONTEND_GOALS.md`, `FANTASY_BETTING_SITE.md`,
`GAME_ANALYSIS_SITE.md`, `FUTURE_DEVELOPMENT.md` — all frozen at 2026-07-15,
never revisited. Move to `docs/planning/` (Phase 0 S0.5) and either refresh or
mark them as historical intent.

---

## 4. `docs/` organization

~55 subdirectories. Mostly the `eda_outputs/<topic>/` and `boxscores/week_N/`
patterns (fine — one dir per analysis / per week). The cruft:

| # | sev | item |
|---|---|---|
| S3-34 | S3 | **82 generated boxscore `.md` files tracked** — regenerate on every sim run, noise in every `git status`/diff. DEVELOPMENT.md even claims they're not tracked. `.gitignore` `docs/boxscores/`. |
| S3-35 | S3 | grab-bag top-level dirs: `docs/todo/` (2 files), `docs/prompts/` (7 — Cam's global says prompt templates live in `~/.claude/templates/`), `docs/study/`, `docs/metrics/` (1 file), `docs/optimizations/` (2), `docs/frontend/` (3). Consolidate under `docs/notes/` or fold into `implementation_plans/`. |
| S3-36 | S3 | `docs/reports/test run 1/` — space in dir name, stale test output. Delete. |
| S3-37 | S3 | version-in-dirname: `docs/eda_outputs/{efsd_v010,fd_conversion_v010,fg_v010,wp_v010,sim_v_0_1_1_review}` — same convention question as §2. |
| S4 | S4 | `docs/models/yac_v_0_1_1.md` vs the dir `src/nfl_sim/models/yac_model_v_0_1_1/` — name mismatch. |

Positive: `docs/models/` has a `.md` for all 11 model families and they're
well-maintained. `docs/audit/` (4 dirs) is the right pattern. `docs/eda_outputs/`
backing every calibration constant is genuinely good practice.

---

## 5. Dead files / repo cruft (consolidated from Phases 0–5)

| item | phase | action |
|---|---|---|
| `legacy/` (38 tracked; only `game_engine_sequential.py` load-bearing, and that's dead once S3-4 lands) | 0, 1 | delete after S3-4 |
| root `reports/` (21 stale files) | 0 | delete |
| empty `Notebooks/`, `artifacts/` | 0 | delete |
| `venv_py38_old/` | 0 | delete (3.12 is trusted) |
| `scripts/data_utils/cleanup_data_dir.py` (spent one-time migration) | 3 | delete |
| `data/interim/_pre_*` / `_stale_backup_*` dirs | 3 | delete |
| `*-Cams-Desktop.*` conflict copies (4 locations) | 3 | delete + gitignore tripwire |
| `R/scripts/play_selection_*_v_0_1_0.R` (8, superseded) | 5 | retire |
| `R/bots/logs/*.log` (17, 12 empty) | 5 | gitignore + delete |
| empty `R/logs/`, `R/outputs/`, `R/reports/`, `R/tests/` | 5 | drop |
| stale `.pyc` in `src/*/__pycache__/` (`.cpython-38`, `-Cams-Desktop`) | 1, 5 | `git clean` / gitignore is already there, just sweep |
| `scripts/roster_management/compileRosters.js` (lone JS in Python pipeline) | 2 | check + retire |
| `predict_play_selection_proba` + dead sequential path | 1, 2 | delete |

---

## Fix-pass items from Phase 6

| # | sev | item |
|---|---|---|
| S2-13 | S2 | **engine unit-test suite** — deterministic invariants that have broken before |
| S3-31 | S3 | `pytest` in CI |
| S3-32 | S3 | `pyproject.toml` + `testpaths` |
| S3-33 | S3 | correct `GOAL_TRACKER.md` |
| S2-1/0.1 | S2 | rewrite `README.md` (stale + absolute paths) |
| — | S3 | fix `DEVELOPMENT.md` boxscore claim + refresh; trim `AGENTS.md` §0 |
| §2 | S3 | **filename `_v_0_1_0` strip — Cam decision**, then one rename commit |
| S3-34 | S3 | gitignore `docs/boxscores/` |
| S3-35/36/37 | S3 | `docs/` consolidation |
| (dead files) | S3 | one sweep commit, after the code deletions they depend on |

Nothing here blocks Phase 7.
