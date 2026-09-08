# Phase 2 — Play-Selection & DNA

**Scope:** the play-selection model + its engine integration, the PROE stack
(`proe_overlay_v_0_1_0.py`, `_proe_script_weight`, `_q4_trailing_pass_target`),
the coach levers, `src/data_pipeline/` (DNA blend), and the
`scripts/roster_management/` pipeline (~40 scripts).
**Baseline:** post-S2-1/S2-2 (`4d…`). Real targets pulled from 2024 PBP.

## Overall read

The **play-selection base model is well-calibrated** and the **PROE stack works
as designed**. The pass-volume shortfall Cam flagged is now down to ~1.4
att/game and is explained by a single label-definition mismatch (below), not a
broken rate. The bigger issues here are dead weight: 6 coach levers that do
nothing, a dead+wrong scalar prediction path, and ~40 roster scripts several of
which AGENTS.md itself marks superseded.

### Play-selection: the numbers

`track_playcall` over 8–12 matchups ×250:

| | value |
|---|---|
| play-selection **decision** rate (pass) | **0.575** |
| base model alone (no PROE) | 0.586 |
| after PROE fade + Q4 overlay | 0.575 |
| net PROE effect | **−1.1 pp** |
| realized box pass rate `(pAtt+sack)/(pAtt+sack+rush)` | **0.540** |

`metadata.json` per-bucket `pred_pass_rate` vs `real_pass_rate` match closely in
every high-volume bucket (`primary_1_10` 0.496/0.495, `primary_2_med`
0.540/0.545, `primary_3_med` 0.866/0.867…). The noisy misses are all tiny-n
buckets (`redzone_1_long` n=36, `redzone_4_short` n=50). The base model is fine.

### Real 2024 targets (per team per game)

| | real 2024 | sim (post S2 fixes) |
|---|---|---|
| pass attempts (comp+inc) | 32.0 | 30.6 |
| sacks | 2.4 | 2.4 ✅ |
| scrambles | 2.1 | — (QB rush att incl. kneels 3.5 vs real 2.8) |
| rush plays (incl scr+kneel) | 26.3 | 27.9 |
| scrimmage plays | 61.5 | ~62 ✅ |
| **dropback rate** (att+sack+scr)/scrimmage | **0.593** | — |
| **box pass rate** (att+sack)/(att+sack+rush) | **0.567** | **0.540** |

---

## S2-4 — play-selection label excludes scrambles, but the engine treats a "pass" call as a dropback

`train.py:134`:
```python
df["is_pass"] = (df["play_type"] == "pass").astype(int)
```
In `nfl_data_py`, `play_type == "pass"` is completions + incompletions + **sacks**,
but **not scrambles** (those are `play_type == "run"`, `qb_scramble == 1`). So the
model predicts **P(pass attempt or sack)** — a scramble-excluded number. Its
57.5% is already net of scrambles.

The engine (`game_engine.py:1410`) does `is_pass = rand < adjusted_pass_prob`,
then in the pass block (`:1602`) rolls **every** `is_pass` lane against the QB's
`scramble_rate` (~5%) and converts the hits to runs. The engine's mental model
is `is_pass == dropback`.

**Result: scrambles are subtracted twice.** The model hands over an
already-scramble-net 57.5%, the engine removes ~5% again → realized box pass
rate **0.540 vs real 0.567** (−2.7 pp), and QB rush attempts run ~0.7/game high.
This *is* the residual pass-volume gap.

**Fix:** retrain with a dropback label —
`is_pass = (play_type == "pass") | (qb_scramble == 1)` — so the model predicts
P(dropback) (~59.3% real) and the engine's att/sack/scramble split of that is
correct. `train.py` is committed and runnable; the feature set is unchanged.
Estimated effect: closes ~1.0 of the 1.4 att/game gap.

**Magnitude / priority:** small, and Cam has already said the current slight
under-count is acceptable for week 1 / season projections. But it's the
structural cause, the fix is a clean retrain, and it directly serves the run/pass
calibration goal — worth doing in the fix pass, then re-measuring the script
bands against it.

---

## PROE stack — working as designed (no fix; confirms the GB/CLE approach)

- 2026 coach PROE: **mean −1.95 pp, median −1.66, 26 of 32 negative**, range
  −8.77 (…) to +6.37 (Reid/KC). This is Cam's stated design — PROE is all-time,
  the league has drifted run-heavy, the mean is not expected to be 0.
- `_proe_script_weight` fades the offset toward 0 as the margin blows out
  (faster after halftime). Net applied effect league-wide: **−1.1 pp**, not
  −1.95 — the fade is doing its job.
- End state: decisions land at exactly 57.5%, matching the base model's
  scramble-excluded target.

**This confirms the plan from the pre-audit discussion:** the GB (broken run
game) and CLE (Monken out of the Lamar/Henry context) cases are *not* bugs in
the PROE machinery — they're the "the coach's career number won't hold this
year" case that a disciplined per-season override sheet handles. The mechanism
is sound; nothing to fix here, only the override sheet to build (post-audit).

Game-script *response* by band (tied / trail 1-8 / etc.) needs the full-season
parquet to measure properly — the `track_playcall` cells are too thin per
matchup. Deferred to the fix-pass re-benchmark; `analyze_pass_rate_by_script.py`
is the tool.

---

## S3-9 — 6 coach levers are computed, stored, hand-editable, and read by nothing

Confirmed (grep of `src/nfl_sim/` + `src/api/`): **`air_yards_tendency`,
`screen_rate`, `play_action_rate`, `no_huddle_rate`, `rpo_rate`,
`conservative_score_bias`** are:

- written by `build_full_name_dna.py`, `build_2026_rosters_v_0_1_0.py`,
  `export_coach_coordinator_levers_v_0_1_0.py`
- carried in `coach_dna.json`, every `{TEAM}_traits_2026.json`, and the
  hand-editable `data/dna/coach_coordinator_levers_2026.csv`
- **consumed at runtime by nothing** (the only grep hits are stale `.pyc` for
  deleted modules `engine` / `vectorized_game_engine`)

Only `proe` (→ overlay) and `deep_shot_rate` (→ `coach_aggression`, 4th-down GO
bias) are live. Cam may be hand-tuning `screen_rate` / `play_action_rate` in the
CSV believing they affect the sim.

**Cam's call (2026-09-08): keep them — they may get wired in later.** So the
fix is **documentation, not code**: mark them clearly as currently inert so
nobody (future-Cam included) hand-tunes `screen_rate` in the CSV expecting a
sim effect. Concretely (fix-pass): a header/comment block in
`coach_coordinator_levers_2026.csv`, a line in AGENTS.md §8 (fragile areas) and
`docs/models/play_selection_v_0_1_0.md`, listing which levers are live (`proe`,
`deep_shot_rate`) vs staged-but-unconsumed.

---

## S3-10 — `predict_play_selection_proba` (scalar) is dead *and* wrong

`model_registry.py:146` builds a **10-feature** vector including
`proe_by_filter`. The model's contract is **9 features** and it *deliberately*
excludes PROE (applied as a post-model overlay — `train.py:21-23`). So this
method feeds a mis-shaped vector to the booster.

It's only called by the sequential engine (Phase 1 S3-4 — dead). Delete it with
the sequential path; also removes the 6th copy of the zone split
(`get_zone`, S3-7) and `get_bucket_name`.

---

## S3-11 — `scripts/roster_management/` is 40 scripts, several already superseded

The export/apply pairs (round-trip a hand-editable sheet) are a legit pattern,
but the directory has accreted. AGENTS.md itself flags as superseded:

- `build_rookie_projections_v_0_1_0.py` — "rookie_projections_2026.json is now
  just an empty shell"
- `promote_rookie_to_flat_v_0_1_0.py` — superseded by
  `add_2026_missing_skill_players.py`
- most of `merge_2026_coach_placeholders.py` — superseded by
  `build_2026_ooc_proxies.py` (4 of 5 entries)

Plus:
- `compileRosters.js` — a lone Node script in a Python pipeline; almost
  certainly dead legacy
- `update_rosters_full_name.py` vs `build_full_name_dna.py` — check for overlap
- two trench-override appliers (`apply_trench_overrides` +
  `apply_trench_z_overrides`) and two seeders

Full script-by-script triage → Phase 6 (organization). Flagging here because
it's part of "the DNA/roster layer" and feeds Cam's #1 (redundant processes).

---

## S4 (notes)

- `src/data_pipeline/` DNA blend (`dna_blender_v_0_1_0`, `rolling_stats`,
  `rookie_curves`) — pure functions, has `test_dna_blender.py` coverage, read
  cleanly. `_proe_to_logit_offset`'s `base_pass_rate=0.57` anchor is hardcoded
  (fine — it's the conversion reference, not a tuning knob).
- Stale `.pyc` in `src/nfl_sim/__pycache__/`: `engine.cpython-38`,
  `vectorized_game_engine.cpython-38`, `game_engine.cpython-38-Cams-Desktop` —
  bytecode for deleted/renamed modules. Harmless, but `git clean` / a
  `__pycache__` sweep would tidy it (Phase 6).
- Play-selection `metadata.json` carries full per-bucket train/test metrics —
  genuinely useful, keep this pattern.

---

## Fix-pass items from Phase 2

| # | sev | item | effort |
|---|---|---|---|
| S2-4 | S2 | retrain play-selection with dropback (`\|qb_scramble`) label | retrain + verify |
| S3-9 | S3 | **document** the 6 inert coach levers (Cam: keep, may wire later) — CSV header + AGENTS.md + model doc | doc only |
| S3-10 | S3 | delete dead+wrong scalar `predict_play_selection_proba` — bundled with Phase 1 S3-4 (dead sequential path) | trivial |
| S3-11 | S3 | roster-script triage → rolled into Phase 6 | — |

Nothing here blocks Phase 3.
