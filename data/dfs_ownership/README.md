# DFS Ownership — data archive & model inputs

# Status: live | v0.3 | 2026-09-12

Everything needed to build (and back-test) our own **projected-ownership model**,
a permanent record of every slate's realized ownership / scores / salaries so a
locked-and-gone DK slate can still be reviewed, **paper-trading** (did a lineup we
flagged as "actually playing this" hold up?), and **field analysis** (what actually
happened in a settled contest — winner, percentile cutoffs, hindsight-optimal lineup).

Two halves feed the ownership model, **both** needed per contest:

| half | what | source | when |
|---|---|---|---|
| **features** (X) | DK salary pool + Vegas lines, frozen at lock | `snapshot_slate_salaries.py` (DK drops the pool from its *lobby* at kickoff — but see backfill) | at / just before lock |
| **target** (y) | realized FLEX% + CPT% ownership, actual points, every lineup, lineup duplication | DK **"Export Full Standings" CSV** — one per contest you want in the training set | after the contest settles |

The scripts:

```
scripts/dfs_ownership/
  standings_parser.py           # shared: parses one DK standings CSV -- imported by the 3 below, not run directly
  snapshot_slate_salaries.py    # pre-lock: DK salary pool + Vegas -> salaries_prelock.csv + manifest
  build_ownership_dataset.py    # post-settle: standings CSVs + snapshot -> _processed/{ownership_actuals,features}.parquet
  score_paper_entries.py        # post-settle: paper_entries.json + standings CSVs -> _processed/paper_results.parquet
  eval_field.py                 # post-settle: standings CSVs (+ salaries) -> _processed/field_eval.parquet
  sim_replay_field.py           # post-settle: standings CSVs + paper_entries.json + a week sim -> _processed/sim_replay.parquet
```

`score_paper_entries.py`, `eval_field.py` and `sim_replay_field.py` all feed the
**Evaluation** page (`frontend/src/pages/EvaluationTab.jsx`, 🧪 in the nav) — all
three are read-only JSON views over their parquet output (`GET /api/eval/paper`,
`GET /api/eval/field`, `GET /api/eval/sim_replay`), so run the script, then just
reload the page.

---

## Workflow

**1 — before lock, snapshot the salary pool** (Vegas comes from `data/external/schedule_2026.csv`):

```
venv\Scripts\python.exe scripts/dfs_ownership/snapshot_slate_salaries.py --week 1
```

Writes `salaries_prelock.csv` + `manifest.json` for every week-1 game that has a live
DK "Showdown Captain Mode" slate. `--game SF_LA` limits to one; `--main` also grabs the
classic Main Slate.

Missed a game? DK's *draftables* endpoint keeps serving a locked slate's pool for a
while after the *lobby* listing is gone:

```
venv\Scripts\python.exe scripts/dfs_ownership/snapshot_slate_salaries.py --week 1 --backfill
```

`--backfill` refetches by the `dk_draft_group_id` already in each `manifest.json`, and
flags the result `late_snapshot: true` (DK freezes salaries at final ≈ their lock value).

**2 — after each contest settles, drop its standings CSV in the slate folder.**
On draftkings.com open the contest → **Export** → **Full Standings** (not top-N). Save it
into `data/dfs_ownership/<year>/week_NN/showdown_<away>_<home>/` named:

```
<name>_<price>_<xmax>max.csv
```

| part | meaning | examples |
|---|---|---|
| `name`  | the contest's identity, free text | `hard_count`, `minimax`, `bubble_screen` |
| `price` | entry fee in dollars, fractional ok; `c` suffix = cents | `20`, `.5` ($0.50), `25c` ($0.25), `3.33` |
| `xmax`  | max entries per user | `1max` (single-entry), `5max`, `150max` |

**Field size is not in the filename** — the parser reads it straight off the CSV as
`max(Rank)`. Entry fee and max-entries *are* in the filename because the standings
export doesn't contain them.

**3 — rebuild the dataset:**

```
venv\Scripts\python.exe scripts/dfs_ownership/build_ownership_dataset.py
```

`--year 2026 --week 1` limits the walk. Writes `_processed/ownership_actuals.parquet`
(target) and `_processed/features.parquet` (matrix), one row per (contest × player),
and prints a coverage table.

**4 — (optional) settle paper trades, analyze the field, and/or replay it under our own sim**, any time after step 2:

```
venv\Scripts\python.exe scripts/dfs_ownership/score_paper_entries.py --year 2026 --week 1
venv\Scripts\python.exe scripts/dfs_ownership/eval_field.py --year 2026 --week 1
venv\Scripts\python.exe scripts/dfs_ownership/sim_replay_field.py --year 2026 --week 1
```

- **`score_paper_entries.py`** reads `paper_entries.json` (written by the 📝 button on
  the Showdown Optimizer's results table — flag a lineup as "I'm actually entering
  this" into a named contest) and, for every entry whose `contest_name` now matches a
  dropped-in standings CSV, computes what it would **actually** have scored and where
  it would have ranked. This is the process backcheck: predicted vs actual score,
  predicted vs actual total ownership (join from `ownership_actuals.parquet` — run
  `build_ownership_dataset.py` first), and the real rank/percentile. An entry with no
  settled contest yet is silently skipped, not an error.
- **`eval_field.py`** doesn't need paper entries at all — for every standings CSV it
  reports the winner, score percentile cutoffs (top 1% / 0.1%), the combined-ownership
  profile of the top ~20 finishers (chalky or contrarian winners?), and the
  **hindsight-optimal lineup**: the actual best score achievable under the cap, an
  exact PuLP ILP solve over real results (a `_processed/salary_history.parquet` /
  `salaries_prelock.csv` pairing is needed for this one piece; the rest works off the
  standings CSV alone). Known gap: no "cash line" / cashers count — the standings
  export doesn't carry the contest's payout structure, only entrant scores; reported
  as percentile cutoffs instead. A real cash line would need the payout table
  snapshotted pre-lock (`get_dk_contest_payout`, while the contest is still open) —
  not built.
- **`sim_replay_field.py`** answers a different question from both of the above: not
  "how did we actually do" but "how would this lineup have done against the REAL
  field's real rosters if OUR sim were reality?" It needs paper entries (same as
  `score_paper_entries.py`) AND that week's sim already run
  (`scripts/simulation_runners/run_week_sim_2026.py <week>`, which writes
  `data/interim/dfs_week_{N}_players.parquet` — one row per player per sim iteration,
  so a QB/WR stack's scores stay correlated within an iteration, not independent
  draws). It rescores every real field entrant's real roster off that per-iteration
  sim data, ranks our paper lineup(s) against them for every iteration, and reports
  the resulting rank/percentile distribution. This is the calibration tool: if our
  sim is a good model of reality, our paper lineups should rank well here even
  though the scoring never touches what actually happened. Same known payout-table
  gap as `eval_field.py` above — reported as percentile, not $EV.

  Note: showdown standings parse natively; classic (`main_slate`) Lineup strings use
  a different slot vocabulary (`QB`/`RB`/`WR`/`TE`/`FLEX`/`DST` instead of
  `CPT`/`FLEX`) — `standings_parser.py`'s `slot_re_for()` picks the right one, but
  only `sim_replay_field.py` currently uses it; `eval_field.py` /
  `score_paper_entries.py` / `build_ownership_dataset.py` are still showdown-only.
- Both `score_paper_entries.py` and `sim_replay_field.py` silently skip an entry with
  no settled contest yet, not an error — most of the week it just isn't settled.
  snapshotted pre-lock (`get_dk_contest_payout`, while the contest is still open) —
  not built.

---

## Folder layout

```
data/dfs_ownership/
  README.md
  2026/
    week_01/
      showdown_NE_SEA/                <- id = <format>_<away>_<home>   (or main_slate)
        manifest.json                 <- teams, draft group, Vegas, snapshot log   (auto)
        salaries_prelock.csv          <- DK salary pool at lock                     (auto)
        paper_entries.json            <- lineups flagged "I'm entering this" (📝 button)  (auto)
        hard_count_20_5max.csv        <- DK "Export Full Standings"      (you download, 1+ per slate)
        minimax_.5_150max.csv
        bubble_screen_15_1max.csv
      showdown_SF_LA/
  _processed/
    salary_history.parquet            <- every snapshot ever taken, 1 row / player / run  (auto)
    ownership_actuals.parquet         <- the target (y), rebuilt from all slate folders   (auto)
    features.parquet                  <- the matrix (X), joined 1:1 to the target         (auto)
    paper_results.parquet             <- settled paper trades: predicted vs actual        (auto)
    field_eval.parquet                <- field analysis per settled contest               (auto)
    sim_replay.parquet                <- real field rescored under OUR sim, per paper entry (auto)
```

`<format>` ∈ `showdown` | `classic`.

### git

```
# .gitignore
data/dfs_ownership/**/*.csv                  # a 150-max milly export = 40 MB, 240k rows, entrant data
!data/dfs_ownership/**/salaries_prelock.csv
```

Everything else stays tracked — `salaries_prelock.csv`, `manifest.json`, `paper_entries.json`,
and the `_processed/*.parquet` (they ARE the curated model input, small, and worth the git
history). The raw standings CSVs are big and re-downloadable, so they're local-only; the
parquets are the durable record.

---

## `manifest.json`

Written and merged by the snapshot script. You only ever hand-edit `notes`.

```json
{
  "year": 2026, "week": 1, "slate_format": "showdown",
  "away_team": "NE", "home_team": "SEA",
  "dk_draft_group_id": 151820,
  "gameday": "2026-09-09", "gametime": "20:20",
  "vegas": { "total_line": 44.5, "spread_line_home": 3.5,
             "home_implied": 24.0, "away_implied": 20.5 },
  "salary_snapshots": [
    { "ts": "...", "n_players": 68, "dk_draft_group_id": 151820, "late_snapshot": true }
  ],
  "notes": ""
}
```

## `salaries_prelock.csv`

`name, team, pos, salary, cpt_salary, dk_flex_id, dk_cpt_id, snapshot_ts`

`salary` is the FLEX/base salary; `cpt_salary` is the ×1.5 captain price DK charges.

## `_processed/ownership_actuals.parquet` — target, long format

`year, week, slate_format, slate_id, away_team, home_team, contest_name, entry_fee,
 max_entries, field_size, field_bucket, stakes_tier, player, team, pos,
 flex_own_pct, cpt_own_pct, total_own_pct,      # DK's published %Drafted split when present
 own_source,                                    # "dk_published" | "lineup_recompute"
 flex_own_pct_recomputed, cpt_own_pct_recomputed,   # our count from the Lineup strings (cross-check)
 actual_dk_score,                               # FLEX points; CPT = ×1.5
 top_lineup_dupe_count`                          # most-duplicated single lineup in the field

DK's showdown standings export lists every player **twice** in its summary block —
`Roster Position` = CPT and = FLEX, each with its own `%Drafted` — so the split is
published, not just the combined number. We still recompute from the lineup strings
because (a) it's the only cross-check on DK's numbers and (b) DK never publishes
duplication. On week-1 NE@SEA the two agreed within 0.3%.

## `_processed/features.parquet` — matrix, joined 1:1

`year, week, slate_id, contest_name, player, team, pos, field_size, field_bucket,
 stakes_tier, entry_fee, max_entries, salary, cpt_salary,
 team_implied_total, game_total, spread_home, is_home, implied_total_rank`

That's the **v1** feature set (value + Vegas). v2/v3 below get joined in by the model
training script, not here.

## `paper_entries.json` — your flagged lineups, one file per slate

```json
{ "entries": [ {
  "entry_id": "20260912T041500Z-a1b2c3d4", "created_at": "...",
  "slate_format": "showdown", "source": "optimize" | "lab", "label": "Chalk build",
  "contest_name": "hard_count", "entry_fee": 20.0, "max_entries": 5,
  "players": [ {"slot": "CPT", "name": "...", "team": "...", "pos": "..."}, ... ],
  "model": { "...the full lineup-result object /optimize(_showdown) returned..." }
} ] }
```

Written by `POST /api/paper/entries` (the 📝 button); `contest_name` must match the
`<name>` you'll later give the standings CSV. `model` is stored whole so any field the
optimizer returns is available to the settle script without re-plumbing.

## `_processed/paper_results.parquet` — settled paper trades

`entry_id, created_at, year, week, slate_id, slate_format, source, label, contest_name,
 entry_fee, max_entries, field_size,
 predicted_score, actual_score, score_diff,
 predicted_total_ownership, actual_total_ownership, ownership_diff,
 predicted_ev_pct, predicted_itm_pct, predicted_top1_pct, predicted_top01_pct, predicted_first_pct,
 actual_rank, beat_field_pct, finish_percentile,       # beat_field_pct: 100=best; finish_percentile: lower=better ("3"=top 3%)
 total_salary, over_salary_cap, players`                # players: JSON list of names, for display

## `_processed/field_eval.parquet` — field analysis per settled contest

`slate_id, slate_format, contest_name, entry_fee, max_entries, field_size,
 winner_score, winner_players,
 mean_score, median_score, p90_score, p99_score,
 top1pct_cutoff_score, top01pct_cutoff_score,
 top_n_avg_ownership,                                   # avg combined roster ownership of the top ~20 (or top 1%) finishers
 top_lineup_dupe_count,
 hindsight_optimal_score, hindsight_optimal_salary, hindsight_optimal_players, hindsight_vs_winner`

`hindsight_optimal_*` uses real per-player scores **merged across every settled
contest for that slate** (not just one contest's own summary) — DK only publishes a
player's real score in a contest's export if someone in that field rostered them, so
a small field can be missing a player a bigger field on the same slate captured. Even
merged, a player nobody in ANY settled contest rostered is invisible to us and
defaults to 0 — so hindsight-optimal is a lower bound on the true optimal, not exact.

---

## Model design

### Buckets & selection rule

**Field size is the primary axis** — the target's scale *and* shape change with it:
chalk QB tops ~58% in a 237k milly, less in a 3k single-entry, lumpier still in a
150-entry. Three models:

| `field_bucket` | field size | archetype |
|---|---|---|
| `large` | ≥ 50,000 | Milly-Maker / Kickoff |
| `mid`   | 1,000–49,999 | mid GPP, 3-max / 5-max |
| `small` | < 1,000 | single-entry, small doubles |

**Stakes is a feature, not a bucket** — `stakes_tier` ∈ `casual` (≤ $5), `mid`, `sharp`
(≥ $20). Sharper money fades chalk and stacks more deliberately, so the same field
size at $20 looks different from $0.50. Collect a casual/sharp pair at similar field
size when you can (week-1 NE@SEA already has `minimax` $0.50 / `hard_count` $20).

**Which contests to save:** the ones you actually play, spread across the grid —
don't chase 50 CSVs. Target ~4–6 slates per `field_bucket` before fitting a GBM.

### Model shape

Two **separate** targets — FLEX and CPT ownership behave differently:

- **FLEX%** — value-chalk pile-on, classic-slate-like. `logit(flex_own)` regression.
- **CPT%** — ceiling/name driven, concentrates on 3–5 players, near salary-blind, plus
  a "cheap value captain" contrarian archetype. Its own `logit(cpt_own)` regression.

Start with LightGBM/XGBoost once ~4–6 slates per bucket are archived. Until then the
tuned formula in `_compute_showdown_ownership` (value core + optimal-CPT/FLEX% + Vegas)
is the prior and the baseline to beat.

Cash contests are **not** modeled — cash "ownership" ≈ the cash-optimal build.

### Feature roadmap

- **v1 (done, in `features.parquet`)** — salary, cpt_salary, team implied total, game
  total, spread, is_home, implied-total rank.
- **v2 (join from sim rosters/traits)** — projection (P50) & ceiling, value = proj /
  (salary/1000), value rank within pos and slate; optimal-CPT% / optimal-FLEX% from the
  sim; target/carry/RZ share, snap share, clear-starter vs committee, team pass rate /
  PROE, WR depth rank; one-hot position.
- **v3 (narrative / recency)** — prior-season ownership & points, draft capital, last
  week's points, "punt play" flag (min-salary player with a fresh path to a role),
  questionable/doubtful tag.
- **Interactions** — value × implied-total (a value play in a shootout gets
  hyper-owned); QB ↔ his pass-catchers (stack magnetism → co-ownership).
