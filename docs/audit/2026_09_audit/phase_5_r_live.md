# Phase 5 — R Bots & Live Pipeline (flag-only)

**Scope:** `R/` (40 files, 6.5k LOC) and `src/live/` (7 modules) — the live
4th-down decision bot. Flag-only: correctness spot-checks + best-practice
flags, not a line-by-line.

## Architecture (as actually wired)

The bot is a **Python/R hybrid**:

- **`src/live/`** (Python) — daemon loop (`main.py`), ESPN scoreboard scrape
  (`espn_adapter.py`), SQLite state (`state_store.py`), posting policy
  (`posting_policy.py`), Bluesky/Mastodon posting (`post_targets.py`).
  `simulator_bridge.py` shells out to R for the actual sim.
- **`R/`** — the 4th-down simulation (`R/simulators/fourth_down/`,
  `R/core/fourth_down_helper.R`) and the WP / FG / conversion models
  (`R/models/*/*.rds` + their `train_*.R` scripts). Also `R/bots/*.R` — a
  **standalone** R version of the fetch→filter→simulate→post loop.

`R/bots/README.md`: *"Entry points are called by `src/live/main.py` via
subprocess **or run standalone**."*

## S2-12 — two entry points, two posting-policy implementations, and CI runs the broken one

There are **two ways to run the bot**, and they don't agree:

| | `src/live/main.py` | `R/bots/run_live_today.R` |
|---|---|---|
| runs when | manually (the 86 KB `live_20260814.log` is this — `[INFO] 4thDownBot.Main`) | **CI** (`nfl_live.yml`, every 10 min in game windows) |
| posting policy | `src/live/posting_policy.py` — 3% WP gap, 12 posts/hr, **SQLite-backed** cooldowns | `R/bots/posting_policy.R` — different rules (`wp_gap < 0.08` blowout block), **in-memory** cooldowns |
| cooldown state | persists across runs | **`R/bots/posting_policy.R:22`: "reset each R session"** |

The R path is a one-shot invoked fresh every 10 minutes by cron. Its cooldown /
rate-limit state is in-memory, so **it resets on every CI run** → cooldowns and
the hourly cap don't actually function in the CI model. Right now the
Bluesky/Mastodon secrets are commented out in `nfl_live.yml` (dry-run only), so
nothing posts — but the day those get enabled, the CI path can spam. The Python
path (SQLite state) is the one that's actually correct, and it's the one *not*
in CI.

DEVELOPMENT.md and AGENTS.md both list `R/` **and** `src/live/` as "active",
which papers over the fact that **it's undecided which is the canonical bot.**

**Fix (fix-pass — needs a Cam decision):** pick one.
- If **Python `main.py` is canonical**: point `nfl_live.yml` at it
  (`python -m src.live.main` with `QUICK_POLL_COUNT=1`), keep R only for the
  sim subprocess, retire `R/bots/run_*.R` + `R/bots/posting_policy.R` +
  `R/bots/post_targets.R` to `legacy/`.
- If **R standalone is canonical**: give `R/bots/posting_policy.R` a persistent
  store (write cooldown state to a small file / the same SQLite DB), and retire
  `src/live/main.py` + `posting_policy.py` + `post_targets.py`.

Recommend the first — the Python side already has the persistent state, the
tests (`tests/test_live_pipeline.py`), and the cleaner structure.

## S3-25 — R code has zero tests

`R/tests/` contains only a `README.md`. The 4th-down helper, the WP prep, the
ESPN adapter, the posting policy — all untested on the R side. The Python side
has `test_live_pipeline.py` (7 tests). If the R path stays (S2-12), it needs at
least a smoke test of `fourth_down_decision.R` against a known game state.

## S3-26 — R dependency manifest lives in the CI YAML, not `renv.lock`

`nfl_live.yml` lists the R packages under `extra-packages:` (curl, httr,
jsonlite, dplyr, purrr, lubridate, glue, readr, tibble, stringr, data.table).
That's a de-facto manifest — but it's not usable for local dev, it's not
version-pinned, and it's easy to drift from what the scripts actually
`library()`. Generate a real `renv.lock` (Phase 0 S2, `DEVELOPMENT.md` already
notes "no `renv.lock` committed"). Note: the list omits `xgboost`, which the
`.rds` models presumably need — verify the CI job actually loads them or is
running a heuristic fallback.

## S3-27 — 17 committed log files, mostly empty

`R/bots/logs/*.log` — 17 files tracked, **12 of them 0 bytes**, the rest live
game polling output from June–Sept 2026. Logs are runtime artifacts, not source.
Add `R/bots/logs/*.log` and `R/bots/live_csv/` to `.gitignore`, delete the
tracked ones.

## S3-28 — `R/scripts/play_selection_*_v_0_1_0.R` is superseded legacy

8 files (`_eda`, `_calibration`, `_eval`, `_importance`, `_overall_eval`,
`_rz_eda`, `_test`, `_train`) — the R-era play-selection modeling pipeline,
replaced by `src/nfl_sim/models/play_selection_v_0_1_0/train.py` (Phase 2). Plus
version-in-filename. Retire to `legacy/` or delete (git history keeps them).
The other `R/scripts/*.R` (chaos prep, model_1 prep, dna registry, air-yards
eda) — check each against its Python successor in Phase 6.

## S3-29 — empty scaffold directories

`R/logs/`, `R/outputs/`, `R/reports/`, `R/tests/`, plus `R/scripts/README.md`
with no scripts described — dirs containing only a `README.md`. Either use them
or drop them (git doesn't track empty dirs anyway; the README is the only reason
they exist).

## S3-30 — `src/live/README.md` uses `file:///c:/Users/txcwa/...` absolute paths

6+ of them. Same hard-rule violation as `README.md` (Phase 0 S2-1 / 0.1).
Relative repo links.

## S4 (notes)

- `src/live/__pycache__/*.cpython-38.pyc` — stale 3.8 bytecode (the recurring
  pyc-cruft theme; Phase 6 sweep).
- `R/models/*/eval_outputs/*.png` — committed training eval plots. Fine to keep
  (small, they document the model), but note they're build artifacts.
- The `off_score`/`def_score` naming in `src/live/` is *correct* there (real
  dynamic possession) and was deliberately left alone in the 2026-07-21 rename
  — WORKLOG confirms. Not a finding.

## Fix-pass items from Phase 5

| # | sev | item |
|---|---|---|
| S2-12 | S2 | pick one canonical bot entry point; fix CI to run it; retire the other path. **Cam decision.** |
| S3-25 | S3 | add R smoke tests (only if the R standalone path survives S2-12) |
| S3-26 | S3 | real `renv.lock` |
| S3-27 | S3 | gitignore + delete committed logs |
| S3-28 | S3 | retire `R/scripts/play_selection_*.R` |
| S3-29 | S3 | drop empty scaffold dirs |
| S3-30 | S3 | fix absolute paths in `src/live/README.md` |

Nothing here blocks Phase 6.
