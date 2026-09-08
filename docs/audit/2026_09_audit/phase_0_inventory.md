# Phase 0 — Inventory & Baseline

**Baseline:** commit `c2287e0`. Survey only — deep findings belong to Phases 1–6.

---

## 1. Scale

| Area | Files | Notes |
|---|---|---|
| `src/` | 47 py | `nfl_sim/` engine + `api/` + `data_pipeline/` + `live/` |
| `scripts/` | 106 py | `roster_management/` ~55, `eda/` ~30, `simulation_runners/` ~12 |
| `frontend/src` (DFS) | 15 jsx/js | React 19 + Vite, :5173 → API :8002 |
| `frontend_analysis/src` | 10 jsx/js | React 19 + Vite + Recharts, :5174 → API :8000 |
| `R/` | 40 R | live 4th-down bot (active, not legacy) |
| `tests/` | 6 files | 97 tests, all green |
| `legacy/` | 220 files (38 tracked) | see §4 |
| `game_engine.py` alone | ~3,300 lines | the product; Phase 1 |

`data/` on disk is modest right now (largest tracked: `pfr_adv_rushing_stats.csv`,
`trench_dna.json`, `schedules_2015_2024.csv`; big regeneratables gitignored).
Storage trajectory is a Phase 3 topic.

## 2. The four root docs

| Doc | Last real update | State |
|---|---|---|
| `README.md` | **2026-07-15** (stale) | S2 — predates all 2026-season work; still says "one API on 8002", no mention of the DNA/override system, week-aware sim, or the 2026 season sim. Lines 97–100 use `file:///c:/Users/txcwa/…` **absolute paths** (violates the global hard rule). |
| `AGENTS.md` | 2026-09-04 | Current, but **§0 "Current Priorities" is ~70 lines and has become a second worklog** (S3). The handoff contract should point at WORKLOG for history, not inline it. |
| `DEVELOPMENT.md` | 2026-08-05 | Good shape, ~5 weeks stale — missing the nfl.com roster move's later half, QB swaps, week-aware season sim, the 2026-09 calibration batch. S3. |
| `WORKLOG.md` | 2026-09-08 | 546 lines, current. Fine. |

## 3. Root-level `.md` sprawl — S3

11 markdown files at repo root. Beyond the canonical 4 + the 2 roadmap files
(`PROJECT_ROADMAP`, `GOAL_TRACKER`), these are all **frozen at 2026-07-15** and
never updated:

- `DETAILED_GOALS.md`, `FRONTEND_GOALS.md`, `FANTASY_BETTING_SITE.md`,
  `GAME_ANALYSIS_SITE.md`, `FUTURE_DEVELOPMENT.md` — planning docs that belong
  in `docs/` (or `docs/roadmaps/`), not root.
- `MOCK_DATA_INVENTORY.md` (2026-09-08) — active, but root is the wrong home.

Recommendation (Phase 6): keep 6 at root (4 canonical + 2 roadmaps), move the
rest under `docs/planning/`.

## 4. Directory cruft — S3

| Path | Issue |
|---|---|
| `legacy/` (38 tracked) | Only `legacy/game_engine_sequential.py` is load-bearing (`batch.py` imports it as the non-vectorized fallback — Phase 1 will check if that path is even reachable). The rest — `legacy/engine.py`, `legacy/simulation_runners/*`, `legacy/models/*/train.py`, `legacy/test_scripts/*`, R inspection scripts — is pure archive. **Git history is the archive** (global standard). Candidate for deletion once the one live import is confirmed/relocated. |
| `reports/` (root, 21 tracked) | Stale generated output from an old iteration (`monte_carlo_v1/`, `test_run/`, `monte_carlo_player_summary.csv`). Superseded by `docs/reports/`. Delete candidate. |
| `Notebooks/`, `artifacts/` | Empty, 0 tracked. Delete. |
| `scratch/` (18 `.py`, gitignored) | Not tracked (good) but **6+ `test_*.py` files** — pytest auto-collects them and the collection **errors** (one hits a dead `127.0.0.1:8000` request at import). See §6. |
| `venv_py38_old/` | Retired 3.8 env, "delete once 3.12 trusted" (flagged since 2026-07-15). 3.12 is clearly trusted now. Delete. |
| `venv_cfbd/` | Legit (pydantic v1/v2 conflict), gitignored. Keep — but see §5. |
| `ai_assistant_files/` (1 tracked) | Check contents in Phase 6. |

## 5. Dependency manifests

- `requirements.txt` — **clean**: every pin matches the installed venv. Comment
  says "full pytest suite (18/18)" — now 97 tests (S4, stale comment).
- `cfbd` package (college-stats pull) is **not captured anywhere** — it lives
  only in the un-manifested `venv_cfbd/`. Should be a `requirements-cfbd.txt`
  (S3) so a fresh clone can rebuild it.
- `frontend/package.json`, `frontend_analysis/package.json` — present, dated
  2026-07-15. Phase 4 will check for stale/vulnerable deps and lockfile state.
- **R has no `renv.lock`** despite 40 R files and CI (`.gitattributes` comment)
  running them on Linux. Dependency set is "whatever each script's `library()`
  lists". S2 for reproducibility — Phase 5.

## 6. Tooling / config gaps — S2/S3

- **No `pyproject.toml` / `pytest.ini` / `setup.cfg`** — no Python project
  config at all. Consequences:
  - pytest has no `testpaths`, so `python -m pytest` (no path arg) wanders into
    `scratch/` and `legacy/` and **errors out**. You have to know to run
    `pytest tests/`. S2 — trivial fix (`pyproject.toml` with
    `[tool.pytest.ini_options] testpaths = ["tests"]`).
  - No linter/formatter config (ruff/black/mypy). The code is consistent by
    discipline, not tooling. S3 — worth adding a ruff config given the size.
  - `src/` is not an installable package — everything relies on `sys.path`
    hacks (`sys.path.insert(0, …)` in tests and scripts). S3.
- `.gitattributes` **is** present and correct (LF normalization, binary types).
  The LF→CRLF warnings on the checkpoint commit are just git normalizing the
  existing CRLF files once — a one-time `git add --renormalize .` silences them.
  Not a finding.

## 7. Roadmap vs reality — preview (full treatment in Phases 6–7)

- `GOAL_TRACKER.md` marks **"Tier 1 Production Release ✅ Completed Sept 2026"**
  and **"Full system validation vs 2024 actuals ✅ Completed Aug 2026"**. Neither
  is true: the site isn't published (Cam's focus area #4), and the last 7 weeks
  were spent recalibrating core engine outputs (completion % was −14pp at 30+
  air yards; PROE was mis-applied; pass volume still ~2–4pp low). The engine is
  in active calibration, not released. **S2 tracking-accuracy** — the tracker
  has been marked done aspirationally.
- Target-date column is internally inconsistent (rows dated "Feb 2027" /
  "Oct 2026" marked ✅ Completed). The dates aren't trustworthy.
- `PROJECT_ROADMAP.md` header says "Last Updated: June 2026" (file was touched
  2026-09-08 — header not bumped).
- Betting-layer work (`bettingLines.js`, `Leverage.jsx`, sim-vs-market plan
  docs) is progressing while Goal 1 isn't actually locked — a drift signal to
  examine in Phase 7, not necessarily wrong.

## 8. API-doc scatter — S3 (Phase 4)

`docs/api_contract.md`, `docs/api/api_contract.md`, `docs/api/local.md`,
`docs/api/production_migration.md`, and a `docs/api/api/` subdir all exist.
Likely duplication / superseding. Phase 4 untangles it.

---

## Phase 0 findings summary

| # | Sev | Finding | Phase to fix |
|---|---|---|---|
| 0.1 | S2 | `README.md` stale + absolute paths | 6 |
| 0.2 | S2 | `GOAL_TRACKER` marks Tier 1 / validation "Completed" — inaccurate | 6/7 |
| 0.3 | S2 | No `pyproject.toml`; `pytest` wanders into `scratch/`+`legacy/` and errors | 6 |
| 0.4 | S2 | R has no `renv.lock` | 5 |
| 0.5 | S3 | 11 root `.md` files; 5 stale planning docs belong in `docs/` | 6 |
| 0.6 | S3 | `legacy/` (38 tracked) + root `reports/` (21) + empty `Notebooks/`,`artifacts/` — archive cruft | 6 |
| 0.7 | S3 | `venv_py38_old/` never deleted | 6 |
| 0.8 | S3 | `cfbd` dep not manifested | 3 |
| 0.9 | S3 | AGENTS §0 has become a 70-line second worklog | 6 |
| 0.10 | S3 | API docs scattered across 5 locations | 4 |
| 0.11 | S3 | `DEVELOPMENT.md` ~5 weeks stale | 6 |
| 0.12 | S4 | `requirements.txt` "18/18 tests" comment (now 97) | 6 |

Nothing here blocks the deep phases. Proceeding to **Phase 1 — sim core**.
