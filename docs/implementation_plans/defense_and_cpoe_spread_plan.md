# Team-Strength Spread — CPOE gain + pass-defense in the completion model

**Goal:** widen the sim's team-strength spread (expected-wins std 2.03 → target
≥ 2.4) by fixing two places where the completion model under-transmits real
skill/matchup. **No new defensive concept this iteration** — this reuses
`pass_def_z` (the D-line pass-rush composite) and the existing QB CPOE input.

**Diagnosis it's built on** (2026-09-09, `play_selection_calibration_notes.md`
+ ARI@LAC decomposition): the completion model is
`logit(P) = curve(ay) + SKILL_SHRINK·(logit(anchor) − curve(anchor_adot)) + sep_bonus`,
then `P = sigmoid(...) + qb_cpoe/100 − offset`. It has **zero defensive input**
and CPOE enters at 1:1. Result: a projected bottom-3 team (ARI) posts 62.8%
completion / 6.6 y/a / 4.6 ypc / 20.7 pts against a playoff opponent; the sim
lines them −3.4 vs a Vegas −10.5. Teams whose real badness is defensive or
situational can't be made bad enough.

---

## A — widen the QB CPOE → completion coefficient (+ clip the input)

### What

1. **Clip the raw `qb_cpoe` input** to a real-starter range at assembly
   (`game_engine.py:1527`), before it feeds *any* model:
   ```python
   QB_CPOE_CLIP = (-6.0, 7.0)   # real season-starter CPOE range; the raw
                                # values have small-sample tails (NYJ Zappe
                                # -13.55, IND Richardson -8.67) that are noise
   qb_cpoe = np.clip(
       np.where(self.possession_is_away, away_feat['qb_cpoe'], home_feat['qb_cpoe']),
       *QB_CPOE_CLIP,
   ).astype(np.float32)
   ```

2. **Multiply the completion-side CPOE term** by a gain > 1 at both the
   contested (`:1899`) and open (`:1924`) paths:
   ```python
   QB_CPOE_COMPLETION_GAIN = 1.4   # backtrack: 1.0

   qb_cpoe_val_c = qb_cpoe[is_normal][is_contested] / 100.0 * QB_CPOE_COMPLETION_GAIN
   ...
   qb_cpoe_val   = qb_cpoe[is_normal][is_open]      / 100.0 * QB_CPOE_COMPLETION_GAIN
   ```

### Why a gain > 1 is defensible

- CPOE is "completion over expected", but the *expected* it was measured
  against isn't the sim's `curve + anchor` expected — so 1:1 transfer isn't
  guaranteed to reproduce the real per-QB spread.
- `SKILL_SHRINK = 0.7` already shrinks the *receiver* skill term. QB CPOE is
  the main un-shrunk QB signal; it can reasonably carry a bit more weight.
- Real season-starter completion% spans ~58–72% (14 pp). Clipped CPOE spans
  −6…+7 (13 pp) at 1:1, and the receiver/scheme terms have to cover the rest —
  so 1:1 slightly under-covers the QB share.

### Range check (clipped, ×1.4, in completion-prob points)

| QB | raw CPOE | clipped | Δ completion |
|---|---|---|---|
| NYJ Zappe | −13.55 | −6.0 | **−8.4 pp** |
| IND Richardson | −8.67 | −6.0 | −8.4 pp |
| LV O'Connell | −4.57 | −4.57 | −6.4 pp |
| ARI Brissett | −1.23 | −1.23 | −1.7 pp |
| CIN Burrow | +6.14 | +6.14 | +8.6 pp |
| SEA Darnold | +5.38 | +5.38 | +7.5 pp |

Bad-QB teams (NYJ, IND, LV, NO, CLE) get meaningfully worse; elite-QB teams get
better. **ARI barely moves** — Brissett's CPOE is near average; ARI's problem
isn't the QB, it's the defense (→ B).

### Side effects to check

- `qb_cpoe[is_pass]` also feeds **Gate 2 (sacks)** and the **Gate 4 INT proxy**
  (`:1831`). Clipping is *correct* there too (the model never saw −13 CPOE
  starters; it's extrapolating). Expected: a hair fewer sacks / INTs for the
  ex-outlier QBs. Minor, acceptable.
- The `_completion_cap` capture row (`:1942`) stores `qb_cpoe/100` unscaled —
  leave as-is (it's the raw signal for offline refit); the gain is a model
  param, not an input.

---

## B — put team pass-defense into the completion model

### What

A small per-play completion adjustment from the **defending team's**
`pass_def_z` (raw, not `_trench_spread`-widened — that widening was calibrated
for the sack gates, not completion):

```python
PASS_DEF_COMPLETION_SCALE = 0.015   # completion-prob points per z-unit of the
                                    # opponent's pass-defense composite.
                                    # backtrack: 0.0
```

Build the defending-side vector once in the pass block (mirrors the Gate 2b
pattern at `:1573`), using **raw** `pass_def_z` from `trench_dna` rather than
the spread version:

```python
# raw (un-spread) pass_def_z, defending team = non-possessing team
self._pass_def_z_raw_away = trench_pass_away.get('pass_def_z', 0.0)   # in __init__
self._pass_def_z_raw_home = trench_pass_home.get('pass_def_z', 0.0)
...
pass_def_z_def = np.where(self.possession_is_away[is_normal],
                          self._pass_def_z_raw_home, self._pass_def_z_raw_away)
```

Subtract it at both completion paths:

```python
probs_normal[is_contested] = _sigmoid_arr(logit_p_c) + qb_cpoe_val_c \
                             - PASS_DEF_COMPLETION_SCALE * pass_def_z_def[is_contested]
...
probs_normal[is_open] = _sigmoid_arr(logit_p) + qb_cpoe_val \
                        - PASS_DEF_COMPLETION_SCALE * pass_def_z_def[is_open] \
                        - OPEN_FIELD_CALIBRATION_OFFSET
```

### Range check (raw pass_def_z, scale 0.015)

| defense | raw pass_def_z | Δ opponent completion |
|---|---|---|
| PHI | +2.32 | **−3.5 pp** |
| LA | +1.40 | −2.1 pp |
| HOU | +1.35 | −2.0 pp |
| ARI | −0.93 | **+1.4 pp** |
| MIA | −1.34 | +2.0 pp |
| CHI | −1.51 | +2.3 pp |

So LAC (Herbert etc.) completing against **ARI's** defense gets +1.4 pp; a WR
against **PHI** gets −3.5 pp. This is the lever that makes ARI give up more and
LA's defense matter.

### Honest caveat (put in the code comment)

`pass_def_z` is the pass-**rush** composite (pressure / hurry / sack forced),
**not coverage**. Using it here proxies "teams that rush the passer well also
disrupt the pass game overall" — real correlation ~0.4–0.5, not 1:1. It will
**not** separate a great-coverage / weak-rush team from a great-rush /
weak-coverage team. That needs a real coverage or EPA-allowed feature (deferred
— `AGENTS.md` Gate 4 retrain note, and a future team-defense EDA). This is an
explicit stopgap.

### Why not also touch air yards / YAC

A good pass defense also compresses YAC and forces shorter throws. Deliberately
out of scope this iteration — completion is the highest-leverage single point
and keeps the change auditable. Note as a follow-on.

---

## Interaction & calibration

- **B is ~mean-neutral by construction** — `pass_def_z` is a z-score, league
  mean ≈ 0, so the `−scale·z` term sums to ≈ 0 across the league.
- **A nudges the league completion mean up** slightly: the clip removes the
  extreme negatives, and CPOE has a mildly positive volume-weighted league mean
  (good QBs throw more). Estimate +0.3 to +0.6 pp.
- **Re-centering knob:** `OPEN_FIELD_CALIBRATION_OFFSET` (currently 0.0). After
  A+B, if league completion drifts off ~64.5%, move it (open path only — the
  contested path has no offset, matching the existing design).

**Tuning order:**
1. Land A+B at the values above.
2. Measure league completion% → set `OPEN_FIELD_CALIBRATION_OFFSET`.
3. Measure the per-QB completion spread and the bad-team lines (below). If the
   tails are still too tight, raise `QB_CPOE_COMPLETION_GAIN` (1.4 → 1.6) and/or
   `PASS_DEF_COMPLETION_SCALE` (0.015 → 0.020). Re-center again.

---

## Verification (separate from implementation)

Off the `capture_completion` hook + short sims — **do not** run the full regen
until the constants are settled:

| check | target |
|---|---|
| league completion % | 64.0–64.8 (real ~64.5) |
| completion % by QB, min 300 att | worst starter ~57–59, best ~70–71 (real spread); currently ~62–68 |
| ARI @ LAC line | LAC −5 to −8 (from −3.4; Vegas −10.5) |
| NYJ / CLE / NYG / TEN / MIA lines vs Vegas | within ~3 pts of the number |
| completion % by throw-depth bucket | still every bucket within ±2.5 pp of real |
| sacks / game | still 2.3–2.5 (guard against the CPOE-clip side effect) |

Then a full `regenerate_2026_reports.py` and check:
- expected-wins std 2.03 → **≥ 2.4**
- PF range widens (currently 16–27 ppg; real 17–30) — the best offenses should
  reach ~28–30, worst drop toward ~15–16
- the bottom-6 teams' expected wins drop (LV 4.6, ARI 5.7, TEN 5.5, MIA 6.2,
  CAR 5.7, NYG 5.8 → several should land 4–5)
- spot-check that the top teams (DET, CIN) don't blow past 13 wins

---

## Files touched (implementation)

`src/nfl_sim/game_engine.py` only:
- 3 new module constants (`QB_CPOE_CLIP`, `QB_CPOE_COMPLETION_GAIN`,
  `PASS_DEF_COMPLETION_SCALE`) near the other completion constants (~`:200`)
- `__init__` (~`:418`): store raw `pass_def_z` alongside the existing
  `_trench_spread`'d ones
- `:1527`: clip `qb_cpoe` at assembly
- pass block (~`:1560`): build `pass_def_z_def` vector
- completion block (`:1899`, `:1924`): `× GAIN` on CPOE, `− SCALE·pass_def_z_def`
  on both paths

All module-level constants, all backtrackable to current behavior by setting
`QB_CPOE_COMPLETION_GAIN = 1.0` / `PASS_DEF_COMPLETION_SCALE = 0.0` /
`QB_CPOE_CLIP = (-99, 99)`.

## What this does NOT fix (still on the compression list)

- Run matchup still cancels to ~0 when a bad O-line meets a bad run D (ARI 4.6
  ypc vs LAC). An "absolute floor" on rushing (bad O-line runs poorly even vs a
  bad front) is a separate change.
- No red-zone / 3rd-down / situational defense.
- No team takeaway rate (INT still QB-proxy only; fumbles still league-flat).
- `yds/att` compression from the air-yards + YAC chain regressing toward ~6.8.
