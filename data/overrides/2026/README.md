# 2026 per-team override sheets

Hand-editable, Excel-friendly. These are the **edit surface** for the 2026
season's roster + usage projections until the rolling L4 average takes over
a few weeks in.

```
data/overrides/2026/
  _nfl_com_roster.csv       cached scrape of all 32 nfl.com roster pages (the roster-status source)
  season_long/{TEAM}.csv    32 sheets — "if everyone's healthy" season projection
  week_01/{TEAM}.csv         32 sheets — Week 1 expectation (injured players' share redistributed)
  week_02/{TEAM}.csv         (created when you run build_week_overrides 2)
docs/rosters/2026/{TEAM}.md  readable roster snapshot per team (generated, don't edit)
```

## Columns (both season_long and weekly sheets)

| column | meaning |
|---|---|
| `player_name`, `team`, `pos`, `player_id` | identity (`player_id` best-effort from nfl_data_py, may be blank) |
| `roster_slot` | `active` / `practice_squad` / `ir` / `pup` / `nfi` / `suspended` / `exempt` / `cut` — **you edit this** |
| `return_week` | week the player is expected back; `99` = out for the season. Blank for active/PS. |
| `note` | free text (kept, never applied to the sim) |
| `target_share` … `avg_separation_yds` | 15 flat tunable fields (identical to the old `preseason_overrides_2026.csv`) |
| `rz_target_share`, `rz_carry_share` | red-zone (yardline 6–20) shares |
| `gl_target_share`, `gl_carry_share` | goal-line (yardline ≤ 5) shares |

`roster_slot = cut` = nfl.com has the player off the roster **but they still
had a projected role** — reassign their share to a teammate, then delete the
row. Zero-role players nfl.com dropped are removed automatically.

**A trailing `Totals` row** (blank `pos`, `player_name` = `Totals`) is a
sanity aid — the column sum of every share field. It is ignored everywhere
except the display: `apply_*` skips it, `build_roster_md` skips it, and
`build_week_overrides` recomputes it for the week. `export_team_season_overrides`
re-adds it on a reseed. The sim renormalises shares per position anyway, so
totals over 1.0 (common — backup QB carry shares) are fine; the row just
helps you not wildly over-allocate a WR room.

## The scripts (all in `scripts/roster_management/`)

| script | what it does |
|---|---|
| `scrape_nfl_rosters_v_0_1_0.py [TEAM ...]` | fetch the 32 nfl.com roster pages → `_nfl_com_roster.csv` (stdlib, ~32 GETs, 1s apart). **Run this first to refresh roster status** (weekly during the season, or when news breaks). |
| `export_team_season_overrides_v_0_1_0.py` | (re)seed the 32 `season_long/*.csv` from `preseason_overrides_2026.csv` + `zone_usage_overrides_2026.csv` + `_nfl_com_roster.csv`. **Overwrites.** Prints the review flags (see below). |
| `build_week_overrides_v_0_1_0.py <week> [TEAM]` | generate `week_NN/*.csv` — reserve players not back by `week` (and `cut` players) zeroed, all six shares redistributed pro-rata to `active` same-position players. |
| `build_roster_md_v_0_1_0.py` | regenerate `docs/rosters/2026/{TEAM}.md`. |
| `apply_team_season_overrides_v_0_1_0.py` | 32 season sheets → regenerate `preseason_overrides_2026.csv` (flat) + `zone_usage_overrides_2026.csv` (rz/gl, research columns preserved) → run `apply_preseason_overrides` + `apply_zone_usage_overrides` → `current_rosters/{TEAM}_traits_2026.json` → `sync_roster_membership()` makes the JSON's **player set** (not just values) match the sheets: zeroes anyone deleted from a sheet, creates a full entry (`enrich_player`) for anyone added. |

**This is the only thing the sim actually reads.** `game_engine.py` / `batch.py` load exclusively from `current_rosters/{TEAM}_traits_2026.json` — never any CSV, ever. The sheets are purely the edit surface; running `apply_team_season_overrides` is what makes a sim reflect them. **`current_rosters/*_traits_2026.json` is gitignored** (mechanically regeneratable) — no git safety net on it. If a run ever looks wrong the fix is to re-run `apply_team_season_overrides` (idempotent, rebuilds from the sheets), not `git checkout`.

## Workflows

**Refresh roster status + reseed (start of a week, or after roster news):**
```
venv\Scripts\python.exe scripts/roster_management/scrape_nfl_rosters_v_0_1_0.py
venv\Scripts\python.exe scripts/roster_management/export_team_season_overrides_v_0_1_0.py
# review the printed flags, fix the sheets, then:
venv\Scripts\python.exe scripts/roster_management/apply_team_season_overrides_v_0_1_0.py
venv\Scripts\python.exe scripts/roster_management/build_roster_md_v_0_1_0.py
```

**Season-long tuning (no roster change):**
```
edit data/overrides/2026/season_long/{TEAM}.csv   (Excel — save as CSV, close the file)
venv\Scripts\python.exe scripts/roster_management/apply_team_season_overrides_v_0_1_0.py
venv\Scripts\python.exe scripts/roster_management/build_roster_md_v_0_1_0.py
```

**Prepping a week (first few weeks, before real data):**
```
venv\Scripts\python.exe scripts/roster_management/build_week_overrides_v_0_1_0.py 1
edit data/overrides/2026/week_01/{TEAM}.csv as needed   (e.g. cap a bell-cow backup)
```

Once games are played, the weekly refresh overwrites `week_NN/*.csv` with the
rolling last-4-games average and hand editing stops being needed.

## Review flags `export_team_season_overrides` prints

- **roster_slot=cut, had a role** — nfl.com dropped them; reassign the share, delete the row.
- **matched by last name + first initial** — a fuzzy match (Kenny↔Kenneth, Cam↔Cameron). Sanity-check; if wrong, rename the sheet row to the nfl.com spelling.
- **NOT found on nfl.com roster** — a projected-role player with no nfl.com match. Either a real cut/trade (delete or move the row) or a name spelling the fuzzy match missed (rename to match nfl.com). Kept `active` with a note until you resolve it.

## Two separate JSON trees: season-long vs. DFS weekly

Cam's call (2026-09-04): season-long and DFS are two separate pipelines feeding two
separate sim outputs, and a week's DFS edits must never overwrite the season-long file.

| | sheet | compiled JSON | script |
|---|---|---|---|
| season-long | `season_long/{TEAM}.csv` | `current_rosters/{TEAM}_traits_2026.json` | `apply_team_season_overrides_v_0_1_0.py` |
| DFS week N | `week_NN/{TEAM}.csv` | `current_rosters/dfs/{TEAM}_traits_2026.json` | `apply_team_week_overrides_v_0_1_0.py <week>` |

The DFS compile starts from a **fresh copy of the current season-long JSON** (so every
static career-DNA field a sheet doesn't carry comes along for free) and overlays only
that week's 15 flat + 4 rz/gl fields. **Only the current week's JSON is kept** — it's
overwritten every run, no history; the `week_NN/*.csv` sheets are the historical record.

`NFLGameEngine` / `BatchSimulator` both take a `rosters_dir` kwarg (default
`data/current_rosters`) — pass `rosters_dir="data/current_rosters/dfs"` to sim off the
DFS tree instead. Nothing else about the engine changes.

## Running each sim

```bash
# season-long -- regenerates data/interim/sim_results_2026_{games,players}.parquet,
# what the analytics site (/api/season2026/*) serves
venv\Scripts\python.exe scripts/simulation_runners/run_full_season_sim_2026.py

# DFS week N -- regenerates data/interim/dfs_week_{N}_{games,players}.parquet,
# what /api/week_projections serves (falls back to the season-long slice if this
# hasn't been run for the requested week yet)
venv\Scripts\python.exe scripts/roster_management/apply_team_week_overrides_v_0_1_0.py N
venv\Scripts\python.exe scripts/simulation_runners/run_week_sim_2026.py N
```

`run_week_sim_2026.py` also deletes the stale baked `week_{N}_full_projections.json` /
`week_{N}_sim_results.json` caches in `data/interim/` if present, so the API doesn't keep
serving pre-refresh numbers.

## Redistribution notes

- Pro-rata by existing same-position share (equal split if nobody holds any).
- Team share totals are preserved. If a position has an out player but no
  active same-position teammate, the build prints `!! LOST share` — reassign by hand.
- Pure pro-rata concentrates touches on a thin room's next-man-up (GB
  MarShawn Lloyd 26% → 81% carry with Jacobs out). Accepted; pin a number by
  editing the weekly sheet directly.

## Return-week seeds

`RETURN_WEEK_SEED` in `roster_feed_v_0_1_0.py`: ir → 8, pup/nfi → 9,
suspended → 7 (**placeholder — set the real number**), exempt → 99. Hand-tune
in the sheet. **Josh Jacobs (GB)** = `exempt` / `99` (Commissioner's Exempt
list; assume out all season).

## Roster source

Status comes from the official nfl.com team pages (`_nfl_com_roster.csv`) —
exactly 53 `ACT` per team, correct `EXE` / `SUS` / `RSR` / `PUP` / `DEV`
tags. `roster_feed_v_0_1_0.NFL_STATUS_TO_SLOT` maps the codes. nfl_data_py is
consulted only, best-effort, for a `player_id` bridge.
