# Phase 1 — Sim Core Correctness

**Scope:** `src/nfl_sim/game_engine.py` (~3,300 lines), `batch.py`,
`model_registry.py`, `models/*/inference.py`, `field_simulator.py`.
**Method:** full read of the engine's structural spine (init, play loop,
scoring, clock/OT, possession, aggregation) + every model inference wrapper +
targeted numeric probes.

## Overall read

The engine is **well-built**: no bare `except`, no `TODO`/`FIXME` rot, careful
copy-on-write on shared roster dicts, and the clock/OT/possession machinery has
been hardened over 13+ rounds of the `clock_physics_v020` audit and looks
correct (two-minute warning clamp, Q2→Q3 possession-only flip, OT
guaranteed-possession tracking, safety detection). Scoring signs
(`score_away`/`score_home += 7/3/2` under possession masks) were traced for KO
TD / INT TD / fumble TD / FG / safety and are all consistent.

The findings below are real but localized. Two are worth fixing before the next
benchmark because they plausibly move the exact numbers Cam is trying to
calibrate (pass volume, air yards, scoring).

---

## S2-1 — `iteration_range` not applied to 4 hot-path models (serving overfit trees)

**AGENTS.md §0 (2026-07-21) documented this exact bug and lesson:** an XGBoost
model trained with `early_stopping_rounds` needs
`booster.inplace_predict(..., iteration_range=(0, best_iteration+1))` or
production silently runs the extra post-best trees. It was fixed for **Gate 2b**
and **Gate 4** (`chaos_v_0_1_0/inference.py:106,130`). It was **not** applied to:

| Model | Trained with early stopping | Inference call | Measured drift vs validated model |
|---|---|---|---|
| **play-selection** | `train.py:203` (`early_stopping_rounds=30`) | `game_engine.py:1364` bare | pass-prob **mean 0.02, max 0.15**; worst bucket `redzone_3_short` **peaked at iteration 4, serves 35 trees** (mean Δ 0.157) |
| **air-yards** tri-gate + 3 regressors | `train_zone_split.py:188,208` | `air_yards_v_0_1_1/inference.py:89,104` bare | std-depth **±0.40 yd mean**, deep **±0.92 yd mean** (max 2.9) |
| **YAC** zone regressors | `train_zone_split.py:170` (`n_estimators=600`) | `yac_model_v_0_1_1/inference.py:96` bare | not measured — same mechanism |
| **rush-yards** zone | (check `train`) | `rush_yards_v_0_1_0/inference.py:98` bare | not measured |
| chaos **Gate 2** | (check `train_gate2`) | `game_engine.py:1529` / `chaos/inference.py:93` bare | not measured |

`best_iteration` **is** retained in the saved artifacts (verified — it's in the
booster `attributes()` for both the `.joblib` and `.json` saves), so the fix is
the one-line pattern already used twice in `chaos/inference.py`.

**Why it matters now:** play-selection is the model behind the run/pass split
Cam is trying to calibrate, and its worst-hit buckets are short-yardage /
red-zone — exactly where run/pass mix drives RB carries. Air-yards is "the
largest remaining piece of the pass-yards gap" (AGENTS.md). This is
high-value / low-effort and should be fixed **before** any further play-selection
or pass-volume calibration, so that work targets the validated model.

**Fix:** add `iteration_range` to every inference wrapper (mirror
`chaos/inference.py:51-54`), re-verify predictions, re-run the benchmark, then
resume the run/pass discussion against the corrected baseline.

---

## S2-2 — Kickoff return-yardage model is broken (≈12× too many return TDs)

`game_engine.py:1038`:
```python
ret_vals = np.random.lognormal(mean=0.2134, sigma=2.9122, size=n_ret) - 1.0
```
A lognormal with **σ = 2.9122** has a median of ~0.24 yd but a **mean of ~80 yd**
(extreme right tail). Live measurement (300 DET–GB games, instrumented):

- **6.7 % of kickoff returns → return TD** (real NFL ≈ 0.3–0.5 %)
- **0.59 kickoff-return TDs per game** (real ≈ 0.03–0.05) — **~12× too high**
- ~52 % of returns are ≤ 0 yd; the median return leaves the receiving team at
  its own ~1-yd line (real post-2025-rule returns come out to ~the 27)

That's roughly **+4 points/game/team of pure special-teams noise**, inflating
totals (a good-offense test matchup sim'd to 51.4 total) and adding a ~7-point
coin-flip swing to game outcomes. The `- 1.0` shift and downstream
`clip(final_yds, 1, 99)` mean the absurd tail just becomes "TD" or "own 1".

The punt / INT / fumble return models (`exponential(21.8765) - 13.0`,
`gamma(3.0552, 7.6877) - 7.3231`, `exponential(23.7483) - 9.0`) use the same
shifted-heavy-tail pattern with parameters the inline comments already admit are
**"undocumented — no citation of source/fit methodology found"** and "flagged
for a future real-data recheck." KO is the egregious one; the others deserve the
same EDA pass (`nfl_data_py` has return-yardage columns; a script like
`analyze_kicking_timing.py` already exists as a template).

**Fix:** re-fit all four return distributions against real 2021–2025 PBP. For KO
specifically, a truncated normal (mean ~23, sd ~9, small explicit TD
probability) or an empirical bootstrap pool (same approach as `clock_pace`)
would be far safer than a heavy-tailed parametric.

---

## S2-3 — Silent-degradation loading is pervasive

Every critical-input load fails **soft**, producing a plausible-looking but
wrong sim instead of an error:

- `game_engine._load_json` / `batch._load_json` → `{}` for a missing file.
- `model_registry.load_all` → skips a missing model dir, leaves the attribute
  `None`.
- `_get_starter_static` → `"Unknown"`; `receivers_cache`/`rusher_cache` →
  `["Unknown"]`.
- `predict_play_selection_proba` → `0.58`; `get_qb_val`/`get_skill_val` → `0.0`;
  missing coach PROE → `0.0` (league average).
- `trench_tiers_{year}.json`, `rush_gate_calibration.json` missing → all-zero
  params → the trench gates silently do nothing.

The AGENTS.md history is a catalogue of exactly this failure mode running
undetected for **months**: the `proe` field silently reading 0.0 for every team;
the stale `ModelRegistry` singleton; sacks jumping to 6.4/game. Every one was
"the code ran fine, the output just looked reasonable."

**Fix:** a `_require_json()` / startup assertion layer for the non-optional
inputs (DNA files, both rosters, `team_to_coach`, trench calibration, every
model dir). Fail loud at `NFLGameEngine.__init__` with the missing path. Keep
soft-fail only where a missing value is genuinely expected (a player not in a
DNA file → career-average fallback is correct).

---

## S3-4 — Dead sequential-engine path keeps `legacy/` alive

Every `run_batch` call site in the repo passes `vectorized=True` (or the default,
which is `True`). That makes the following **dead**:

- `batch._simulate_single_game_worker`
- `legacy.game_engine_sequential.SequentialNFLGameEngine`
- the **unconditional** `from legacy.game_engine_sequential import …` at
  `batch.py:21` (the main thing dragging `legacy/` into "active")
- `model_registry.predict_play_selection_proba` + `get_bucket_name` +
  `get_zone` (the scalar per-play interface — the vectorized engine has its own
  path at `game_engine.py:1321-1366`)

Beyond being dead weight, the sequential engine has **diverged**: no OT, no
Round-13 quarter-transition fix, none of the 2026 calibration. If anyone ever
flips `vectorized=False` (e.g. to debug), they get silently wrong results.

**Fix:** delete the sequential worker + the import, drop the `vectorized` param
(or make `False` raise `NotImplementedError`), then delete
`legacy/game_engine_sequential.py` and the now-unreferenced `legacy/` files
(Phase 6 handles the `legacy/` sweep).

---

## S3-5 — `_build_slot_map` TE detection is a hardcoded surname list

`batch.py:147-151` distinguishes TE from WR via a frozen set of ~27 lowercase
surnames (`'kelce'`, `'andrews'`, …). The 2026 roster JSONs carry a real
`pos == 'TE'` (verified: `{'QB','RB','TE','WR'}`). Any TE not on the list — every
rookie, anyone traded in — is slotted `WR{n}`, which corrupts:

- the analytics site's position/usage tables (`get_player_stats_flat` uses this
  `player_to_slot`)
- the DFS optimizer's positional constraints

**Fix:** `is_te = p_traits.get('pos') == 'TE'`. Delete the surname set.

---

## S3-6 — `warnings.filterwarnings('ignore')` at `batch.py:18` (module scope)

Silences **all** warnings for any process that imports `batch.py` — including
`numpy` divide-by-zero / overflow inside the completion-probability and
logit/sigmoid math, and pandas `SettingWithCopyWarning` in the aggregator.
Scope it to the specific noisy categories, or move it into the function that
needs it.

---

## S3-7 — 6th copy of the zone split

`model_registry.get_zone` (hardcoded `<= 5` / `<= 20`) is a 6th independent copy
of the goalline/redzone/primary split that AGENTS.md (2026-07-21) says was
consolidated into `_classify_zone`. It's on the dead scalar path (see S3-4) so
it dies with that, but flag any others in Phases 2–3.

---

## S3-8 — `_json_cache` is never mtime-invalidated

`BatchSimulator._json_cache` is class-level and keyed only by path. In the
long-lived API process, a `refresh_weekly_dna` run that rewrites `*_dna.json`
won't be picked up until the server restarts — the API's *report* cache checks
mtimes, but this JSON cache doesn't. Add an mtime check (cheap) or a
`ModelRegistry`-style explicit invalidation.

---

## S4 (notes, no action required now)

- Long inline **"BUG (found in audit) … left as-is"** comment blocks
  (`game_engine.py:921-935` `max_bleed_time`, others) — documented tech debt
  frozen into 15-line code comments. Better as tracked items than as permanent
  code furniture.
- `ModelRegistry.__init__(model_dir=…)` silently ignores the arg on the 2nd+
  call (singleton already built). Harmless today.
- air-yards `scale = max(base_preds - floor, 0.25)` doesn't preserve the mean
  when a regressor predicts below its level's floor (rare edge case, right
  direction).
- `air_yards/inference.py:95` inverse-CDF sampling can produce `level == 3` on
  floating-point `cum_probs[-1] < 1`; the `for lvl in [0,1,2]` loop leaves those
  samples at `0.0` silently. Negligible frequency.
- `batch._load_skill_dna` keeps the `skill_dna.json` fallback that
  `game_engine._load_skill_dna` removed as "confirmed unreachable" — pick one.

---

## Recommended fix order (when we reach the fix pass)

1. **S2-1 iteration_range** — do this first, it's the cheapest and it changes
   the baseline every later calibration measures against.
2. **S2-2 kickoff returns** — re-fit; re-run benchmark.
3. **S2-3 loud loading** — a guard layer; prevents the next silent regression.
4. S3-4 (delete dead sequential path) — unblocks the `legacy/` cleanup.
5. S3-5, S3-6, S3-8 — small, safe.

Nothing here is a stop-the-world S1, but **S2-1 and S2-2 should land before the
run/pass calibration resumes** — otherwise that work targets a model state that
was never validated, plus ~4 pts/game of special-teams noise.
