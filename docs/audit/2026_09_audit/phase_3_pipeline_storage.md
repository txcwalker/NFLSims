# Phase 3 — Data Pipeline & Storage

**Scope:** `scripts/simulation_runners/`, `src/data_pipeline/` (I/O side),
cache-staleness logic, `regenerate_2026_reports.py` orchestration, and the
storage architecture — Cam's focus area #3 ("how are we doing it, can it be
better; the project is large and growing").

## Storage snapshot

`data/` is **~950 MB**:

| dir | size | tracked | what |
|---|---|---|---|
| `processed/` | **296 MB** | 0 | one file: `hardened_pass_training_master_v2_5.csv` (308 MB) + `starting_position_pool.csv` |
| `interim/` | **213 MB** | 0 | sim-result parquet caches + 2 manual backup dirs + OneDrive conflict copies |
| `_snapshots/` | **143 MB** | 0 | the audit/calibration checkpoints (mine, 2026-09-06 ×2) |
| `current_rosters/` | 38 MB | 64 | 32×(2025+2026) traits tracked; the 18×32 `week_*/` tree + `dfs/` untracked |
| `win_probability/`, `field_goal/`, `fourth_down/` | 7 MB | 0 | R-era `.rds` model train/val/test splits |
| `dna/` | 2.8 MB | 19 | JSON priors + CSV override sheets (mix tracked / gitignored) |
| `overrides/2026/` | 445 KB | 70 | the 32×(season + week) hand-edit sheets — **source of truth** |

**Formats in play:** JSON (DNA, rosters, config, calibration params), CSV
(override sheets edited in Excel, external pulls, 308 MB training master),
Parquet (sim caches), RDS (R model splits), SQLite (`data/live/bot_state.db`).

**Growth rate:** roughly **150–250 MB per season** added — a full-season
players parquet is ~65 MB, plus 32 roster JSONs, the week tree, boxscores,
reports. DFS weekly sims (`dfs_week_N_*.parquet`, ~4 MB each) add up if archived.
At 950 MB now, this is a 3–4 year runway before it's genuinely unwieldy —
**not urgent, but the habits below compound.**

---

## S2-5 — the repo lives inside a syncing OneDrive folder, and OneDrive is writing conflict copies into it

A full-tree scan found conflict copies (`*-Cams-Desktop.*`) in **four**
locations:
```
.obsidian/workspace-Cams-Desktop.json  ... -2.json ... through -8.json   (8 copies)
data/interim/sim_results_2025_games-Cams-Desktop.parquet
data/interim/sim_results_2025_players-Cams-Desktop.parquet
src/nfl_sim/__pycache__/batch.cpython-38-Cams-Desktop.pyc
src/nfl_sim/__pycache__/game_engine.cpython-38-Cams-Desktop.pyc
venv_py38_old/.../pandas/compat/numpy/__init__-Cams-Desktop.py   (and more .py inside the venv)
```

OneDrive appends the machine name when it can't reconcile a file edited on two
synced machines. **The `.obsidian/workspace.json` has been conflicted 8 separate
times** — that file changes constantly, so OneDrive fails to sync this folder
cleanly on a regular basis. So far the collisions have landed on regeneratable
caches, bytecode, and editor state. **It is luck, not design, that none has hit
a roster JSON, an override CSV, or a `.py` source file** — and when it does you
get two silently-diverged copies or a lost edit, with no error. For a project
whose recurring failure mode is "a value read wrong for months" (AGENTS.md),
this is that risk waiting to happen. Borderline **S1**.

**Fix (fix-pass, and it's the highest-leverage storage change):** get the repo
out of the synced path. Options, best first:
1. **Move the working copy to a non-synced local dir** (`~/dev/NFLSims`), push
   to GitHub as the sync mechanism. Git already is the sync tool.
2. If it must stay put, **exclude the folder from OneDrive sync**
   (OneDrive settings → this folder → "Always keep on this device" off / add to
   the exclusion list) — but this is fragile and OneDrive has re-added excluded
   folders on update before.

Also delete the existing `*-Cams-Desktop.*` artifacts and add
`*-*-Desktop.*` / `* (1).*` patterns to `.gitignore` as a tripwire.

---

## S2-6 — un-regeneratable critical assets are gitignored and not backed up anywhere

| asset | size | how it was built | backup |
|---|---|---|---|
| `data/processed/hardened_pass_training_master_v2_5.csv` | **308 MB** | no committed builder script found (consumed by `efsd`/`positional_ep` training + `script_chainer.py`) | **none** |
| `data/{win_probability,field_goal,fourth_down}/*.rds` | 7 MB | R-era, "no committed training script" (AGENTS.md) — these `.rds` splits ARE the only record | **none** |

If the disk fails or OneDrive corrupts these, **4 model families and the pass
training pipeline can't be rebuilt.** Gitignoring them was a repo-size decision;
it silently also made them un-backed-up.

**Fix (fix-pass):**
- Short term: copy both to a cloud bucket (the `serverless_parquet_datalake.md`
  plan already picks Cloudflare R2 / Backblaze B2 — 10 GB free, zero egress).
  A `scripts/data_utils/backup_critical_assets.py` that pushes the un-tracked
  un-regeneratable set.
- Real fix: write the builder for `hardened_pass_training_master` (or document
  precisely how it was assembled) so it's *regeneratable*, then it's like the
  DNA files — safe to gitignore because the recipe is committed. Same for the
  R model splits (part of the "retrain the 4 R-era models before 1.0" roadmap
  item).

---

## S2-7 — `run_week_1_only_2025.py` overwrites the shared full-season cache with 16 games

`run_week_1_only_2025.py:70-71` writes `all_games_df` / `all_players_df` to
`data/interim/sim_results_2025_{games,players}.parquet` — **the shared
full-season cache path** — with only week 1's 16 games. AGENTS.md already
documents this as the footgun that staled the cache before. It's still sitting
in the runners dir with an inviting name.

**Fix:** delete it (its job is covered by `run_full_season_sim_2025.py`), or
repoint it to `data/interim/week_1_only_2025_*.parquet`.

---

## S3-12 — five scripts write the shared sim-result parquet, with no owner or provenance stamp

`run_full_season_sim_{2025,2026}.py`, `run_weeks_1_to_18_2025.py`,
`run_week_1_only_2025.py`, `generate_season_leaders.py`,
`print_week_1_boxscore.py` all write `data/interim/sim_results_*.parquet`. Some
write partial data. There is no run-metadata in the file (git SHA, engine
version, N, date, which weeks) so you can't tell what a cache on disk actually
contains. `regenerate_2026_reports.py` was created *specifically* because this
pattern silently desynced the standings from the leaders once.

**Fix (fix-pass):** one writer. `run_full_season_sim.py` owns the cache; it
stamps a sidecar `sim_results_{year}_meta.json` (git SHA, engine mtime, N,
weeks, timestamp). Everything else *reads only*. `_cache_is_stale` checks the
sidecar, not just mtimes (see S3-14).

---

## S3-13 — year-hardcoded runner duplication

`run_full_season_sim_2025.py` and `_2026.py` are near-identical (the 2026 one is
just `SIM_YEAR = 2026` + the week-aware tree logic). Same for
`run_weeks_1_to_18_2025.py`, `run_weeks_1_to_4_2025.py`,
`run_first_4_games_2025.py`, `run_week_1_only_2025.py`. **~5 scripts collapse to
one `run_full_season_sim.py <year> [--weeks N]`.** The 2025 versions are also
just stale — nobody's simulating 2025 anymore.

---

## S3-14 — `_cache_is_stale` false-positives force unnecessary full re-sims

`cache_input_globs` includes `data/dna/*.json` and `data/dna/*.csv`
(`run_full_season_sim_2026.py:179-180`). That glob catches:

- the **gitignored, regenerated** `qb_dna.json` / `coach_dna.json` / … — every
  `regenerate_dna_v_0_1_0.py` run bumps their mtime even when the *values* are
  byte-identical → cache marked stale → a full 272-matchup re-sim (~20 min)
  for nothing.
- reference CSVs that don't feed the sim (`2025_actuals_for_2026_overrides.csv`).
- any file a `git checkout` or OneDrive sync touched (both set mtime = now).

**Fix:** narrow the globs to the files that actually change the sim
(`current_rosters/**`, `overrides/**`, `trench_dna.json`, `coach_dna.json`,
`schedule`), or move to a content hash (`hashlib` over the sorted input set)
written into the meta sidecar from S3-12. Content hashing also fixes the
git/OneDrive mtime problem for good.

---

## S3-15 — hand-rolled data versioning

- `data/interim/_pre_2026-07-21_regen_backup/`,
  `data/interim/_stale_backup_pre_full_season_regen/` — manual "copy the old
  cache before regenerating" dirs. This is the `legacy/`-folder anti-pattern
  applied to data.
- `data/legacy/` — a gitignored graveyard (`.gitignore:49`) that
  `scripts/data_utils/cleanup_data_dir.py` (a spent one-time migration script,
  should itself be deleted) moved ~27 obsolete files into.
- `hardened_pass_training_master_v2_5.csv` — version in filename; its
  `_v2`, `_v2_2`, `_v2_3` ancestors sit in `data/legacy/`. Four generations of
  one file.

**Fix:** delete the spent migration script and the `_pre_*` backup dirs (the
old caches are regeneratable). For genuine "snapshot before a risky change,"
the `data/_snapshots/YYYY-MM-DD_label/` convention I've been using is fine — but
it needs a **retention policy** (keep last 2, or delete after the change is
confirmed). `_snapshots/` is already 143 MB from two checkpoints.

---

## S3-16 — `sys.path.append(os.getcwd())` in ~10 runners

Every runner except `regenerate_2026_reports.py` bootstraps imports with
`sys.path.append(os.getcwd())` — so it only works when run from the repo root,
and fails opaquely otherwise. `regenerate_2026_reports.py` does it right
(`os.path.dirname(__file__)`). Ties to the Phase 0 "no `pyproject.toml`, `src`
isn't an installable package" finding — the real fix is `pip install -e .` with
a `pyproject.toml`, after which none of these need the hack.

---

## Is a real datastore warranted?

**Not yet — don't migrate now.** Reasoning:

- **Volume** (950 MB, +200/yr) doesn't need a DB. Flat parquet + JSON is fine
  for years.
- **The CSV-in-Excel override workflow is a deliberate feature** Cam wants —
  putting roster state in SQLite would break the thing that makes hand-tuning
  32 teams tractable.
- **What a DB would actually help:** (a) not loading a 65 MB parquet into RAM to
  answer one game's query — but **DuckDB over the existing parquet files**
  solves that with zero migration (query files in place, in-process, no
  server); (b) the "what's in this cache" provenance problem — solved more
  cheaply by the meta sidecar (S3-12).

**Recommended trajectory:**
1. **Now (fix-pass):** S2-5 (repo out of OneDrive), S2-6 (back up the
   un-regeneratable assets), S3-12 (one parquet writer + meta sidecar).
2. **When the publishing work starts (Phase 4):** adopt the
   `serverless_parquet_datalake.md` plan — DuckDB + remote parquet on R2. Use
   DuckDB locally too at that point (read-only, over `data/interim/*.parquet`).
3. **Never, unless it earns its place:** a running SQL server. The serverless
   columnar approach is the right fit for this workload and budget.

---

## Fix-pass items from Phase 3

| # | sev | item |
|---|---|---|
| S2-5 | S2 | repo out of OneDrive sync + delete conflict copies + `.gitignore` tripwire |
| S2-6 | S2 | back up `hardened_pass_training_master_v2_5.csv` + R `.rds` splits to R2/B2; write/document the builder |
| S2-7 | S2 | delete or repoint `run_week_1_only_2025.py` |
| S3-12 | S3 | one parquet writer + `*_meta.json` sidecar (SHA/N/weeks/date) |
| S3-13 | S3 | collapse the 5 year-hardcoded runners → `run_full_season_sim.py <year>` |
| S3-14 | S3 | narrow `_cache_is_stale` globs or switch to content hash |
| S3-15 | S3 | delete `cleanup_data_dir.py` + `_pre_*` backup dirs; add `_snapshots/` retention policy |
| S3-16 | S3 | `pyproject.toml` + `pip install -e .` kills the `sys.path` hack (shared with Phase 0/6) |

Nothing here blocks Phase 4.
