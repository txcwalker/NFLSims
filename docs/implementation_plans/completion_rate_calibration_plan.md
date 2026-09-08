# Completion-Rate Calibration Plan ("A2")

**Status:** Phase 0 in progress (real baseline done, sim-side instrumentation next). Written 2026-09-06.
**Target version:** `v0.3.x` engine bump (completion path only).
**Checkpoint:** `data/_snapshots/2026-09-06_pre_completion_calibration/` (gitignored) — full pre-A2 state + revert instructions, per Cam's "be able to get back to them if this gets out of hand."

## Cam's direction (2026-09-06)

- **Skip Phase 4 / M3 entirely.** Adding receiver ADOT to the air-yards model was left out *on purpose* — ADOT and air-yards are near-collinear, so it would bias the model. Do not revisit.
- **No career ADOT.** Anchor on prior-season (or median-of-last-two, the preseason-override basis); in-season the weekly sim uses L4. NOTE (verified 2026-09-06): the *mechanical* `enrich_player` path actually seeds `adot` from the **5-yr career pool** in `skill_dna.json` — only `target_share`/`carry_share` get the "most recent 2025" treatment. In practice the flagged deep guys' `adot` is a hand-set sheet value that already reflects ~2025. `data/dna/2025_actuals_for_2026_overrides.csv` exists as a prior-season source but **is wired into nothing**. Phase 1 should make prior-season the real mechanical default.
- **Deep specialists are legitimate** (Thornton, Boutte, Pierce) — "their job is to run 40-yard routes every play," and the NFL is trending this way. Don't crush the archetype. Real 2024-25: Thornton ADOT 24.9, Pierce 20.2 — genuinely that deep.
- **Phase 2 = option 2c** (blend the anchor toward the model's own prediction at the actual air-yards, weighted by distance from the anchor depth), possibly with 2a. Not 2b.
- **Phase by phase, measure after each**, stop when the gap closes.
- **rz/gl catch-rate columns (Q3): yes** — seed from historical zone rates, rookies = league average, **not hand-tuned** (Cam won't touch them). Replaces A1's delta-shift.
- Passing yards running low is expected/acceptable; few run high. Watch it but don't chase it here.
- **Order:** A2 now → blowout logic → week-aware sims.

## Phase 0 findings (2026-09-06)

Real completion by target depth, 2021-2025 REG (`scripts/eda/analyze_completion_by_depth.py` → `docs/eda_outputs/completion_by_depth/README.md`):

| air_yards bucket | att % | real cmp % | sim `1.5 − 0.08·AY` then −0.075 (neutral receiver) |
|---|---|---|---|
| ≤0 screen | 21.9 | 76.9 | hardcoded 0.80 WR/TE, 0.83 RB |
| 0–5 | 30.5 | 74.3 | ~0.70 |
| 5–10 | 18.2 | 63.0 | ~0.63 |
| 10–15 | 11.5 | 56.3 | ~0.54 |
| 15–20 | 7.7 | 50.9 | ~0.44 |
| 20–30 | 6.2 | 38.1 | ~0.30 |
| 30+ | 4.0 | 29.3 | ~0.09 |

Overall real: **64.87%** (Cam's 64.40% target confirmed legit).

- **Shape mismatch confirmed:** real curve is flat-and-high 0→5 AY then convex-steep; the sim is one logistic slope, and it undershoots worst at 15+ AY (−7 to −20pp for a neutral receiver before `delta_wr`). `delta_wr` masks this for receivers thrown near their ADOT; it breaks for anyone thrown far from it (deep specialists on short routes, M1).
- **52% of all NFL targets are ≤5 air yards at ~75%.** Sim screens (hardcoded 0.80/0.83) are ~4pp hot — part of why the *population* reads 68% while downfield WRs read low.
- **The sheets aren't the problem.** Flagged receivers' sheet `catch_rate` vs real 2024-25: Diggs 0.81 vs 0.795, A.J. Brown 0.70 vs 0.665, Olave 0.70 vs 0.66, Nabers 0.61 vs 0.62 — all within a few points. Sim realized is 6–20pp below *even the real rates*. The compression is entirely in the engine.
- **Thornton flag:** sheet has 0.4148; real 2024-25 is **0.511** (45 targets, ADOT 24.9, 58% deep). Cam: leave it at 0.41 for now.

### Phase 0 — SIM measurement (2026-09-06)

`scripts/eda/measure_sim_completion_by_depth.py` (light-touch `track_cmp_by_depth`
hook in `game_engine.py`, off by default). 32 matchups (2026 wk 1-2) × 400 iters.
Raw: `docs/eda_outputs/completion_by_depth/sim_vs_real_2026.txt`.

| air_yards | sim att% | real att% | sim cmp% | real cmp% | Δ cmp |
|---|---|---|---|---|---|
| ≤0 | 18.8 | 21.9 | 80.9 | 76.9 | +4.0 |
| 0–5 | 24.4 | 30.5 | 71.4 | 74.3 | −2.9 |
| 5–10 | **28.4** | 18.2 | 66.6 | 63.0 | +3.6 |
| 10–15 | 14.4 | 11.5 | 59.5 | 56.3 | +3.2 |
| 15–20 | 5.1 | 7.7 | 48.1 | 50.9 | −2.8 |
| 20–30 | 3.9 | 6.2 | 35.7 | 38.1 | −2.4 |
| 30+ | 5.1 | 4.0 | **15.1** | **29.3** | **−14.2** |

Sim overall completion **64.67%** vs real 64.87% — the *aggregate* is already right.

**Two distinct problems, not one:**

1. **Completion curve is fine 0–30 yds (±4pp), then the 30+ tail collapses:
   15.1% vs 29.3% real.** The log-linear `logit(P)=b0+b1·AY` keeps dropping past
   30 while real deep completion plateaus ~29%. This is a **Phase 2/3 curve
   fix** — add a floor / flatten the slope past ~25 yds.

2. **The air-yards sampler misallocates throws (M3).** Sim is under-dispersed in
   the normal range and fat-tailed at the extreme: too few ≤5 (43% vs 52%
   real), way too many 5–10 (28% vs 18%), too few 15–30 (9.0% vs 13.9%), too
   many 30+ (5.1% vs 4.0%). Effect on the flagged receivers:
   - **Possession WR** (Olave, AJB…): loses his easy ≤5 / screen completions
     (~75% real) — the sampler reassigns them as 5–15 yd throws (~60–66%) — so
     his *aggregate* drops ~8–10pp even though every per-bucket rate is near
     real. **Phase 2 will NOT fully fix this** — it's a distribution problem,
     not a curve problem.
   - **Deep specialist** (Thornton): the sampler rarely gives him a true 30+
     ball, so most of his targets land 5–15 where his `delta_wr` (M1, anchored
     at ADOT 26) inflates him to ~70%. Phase 2 (2c) DOES fix this half —
     re-anchoring kills the over-correction.

**Implication for scope:** Cam ruled out fixing M3 by adding ADOT to the sampler
(collinearity). But Phase 0 shows the sampler's *shape* is the dominant driver of
the possession-WR gap. Options that are NOT "add ADOT":
   - **3b. Recalibrate the air-yards sampler's dispersion** — it's producing a
     too-narrow core + too-fat 30+ tail. If it samples a parametric residual
     after the point prediction, widen/reshape that; if it's a quantile model,
     check the tail quantiles. No new feature, no collinearity.
   - **3c. Post-hoc reshape** — nudge sampled air-yards toward the real bucket
     distribution per (QB avg air yards, receiver deep_target_rate) cell.
   - **3d. Accept it** — if Phase 2 + the 30+ fix + Phase 1 get the flagged
     receivers within ~4–5pp, call the residual acceptable and move on.
   → **new open question Q3 below.**

This is the "A2" half of the catch-rate investigation opened 2026-09-06. The "A1"
half (make the hand-tuned sheet `catch_rate` actually reach the sim) is **done** —
see [§7](#7-what-a1-already-fixed). A2 is the real modelling work: the sim's
completion probabilities are compressed toward the league mean, so possession
receivers under-catch and deep specialists over-catch relative to their true
rates.

---

## 1. Problem statement & evidence

### The symptom Cam found

Reviewing the 2026 team pages: ~10+ high-profile possession receivers show sim
catch rates well below their override-sheet value (Olave, A.J. Brown, Evans,
Nabers, Wan'Dale Robinson, Diggs, …), while a few deep threats show catch rates
well *above* (Tyquan Thornton: sheet 0.41, sim 0.68).

### Diagnostic (2026-09-06, `scratchpad/catch_rate_diag.py`)

Ran against `data/interim/sim_results_2026_players.parquet` (1000 iterations),
130 WR/TE with ≥40 median targets. Per receiver: true aggregate catch rate
`sum(rec)/sum(targets)` vs sheet flat `catch_rate` vs `splits.primary.catch_rate`
vs the `median(rec)/median(targets)` ratio visible on the team page.

| Finding | Value | Reading |
|---|---|---|
| median-ratio vs true aggregate | median abs diff **0.004** | The team-page number is **not** a reporting artifact. Cam's median-of-independent-stats hypothesis is ruled out. |
| mean gap: realized − sheet `catch_rate` | **−0.028** | Whole receiver population under-catches its sheet value by ~3pp on average… |
| `corr(ADOT, gap_vs_sheet)` | **+0.52** | …but it's not uniform: short/possession receivers are 6–15pp low, deep receivers are at/above. The spread is being **compressed**. |
| `corr(split_primary, realized)` | 0.81 | Realized tracks the zone-split anchor better than the flat value (confirms the engine uses the split) — but still only r=0.81. |
| mean gap: realized − `split_primary` | −0.032 | Even against the number the engine *does* anchor to, realized is ~3pp low — the net of the flat −0.075 offset minus the average separation bonus. |

Population completion % is ~68% in-sim vs Cam's confirmed real 2025 target of
**64.40%** (weighted CMP/ATT across 50 QBs), already flagged in
[AGENTS.md](../../AGENTS.md) §0 as "needs its own dedicated round." **This is that
round.** The population number being *high* while possession WRs run *low* means
short throws / screens / RB checkdowns are running hot and masking the downfield
shortfall — a **spread** problem a flat offset cannot fix.

---

## 2. Root-cause mechanisms

All in `src/nfl_sim/game_engine.py`, the completion block (~lines 1592–1706).

### M1 — the depth-decay curve is anchored at career ADOT, then extrapolated on one global slope

```
logit(P_catch) = b0 + b1·air_yards_on_this_play + delta_wr + sep_bonus
b0 = 1.5,  b1 = -0.08
delta_wr = logit(zone_baseline) − logit(sigmoid(b0 + b1·ADOT_career))
```

`delta_wr` is calibrated so that a throw at the receiver's **career average
depth** completes at their `zone_baseline` (= `splits[zone].catch_rate`). Fine at
the anchor point. The failure is the linear extrapolation away from it:

- **Tyquan Thornton** — ADOT 26.3, `split_primary` 0.505. A 10-yard throw is
  16 yards short of his anchor: `b1·Δ = 0.08·16 = 1.28` logit units of lift ≈
  a jump from ~50% to ~80%. His real intermediate catch rate is ~50%, but the
  model assumes his deep-ball rate implies an elite short-area rate. Net sim
  catch rate 0.68.
- Mirror image for possession guys, milder, plus M2/M3 below.

The zone split already *is* the receiver's real catch rate in that field zone —
the depth adjustment inside a zone should be small and referenced to that zone's
typical air-yards, not projected from a career ADOT that may be 15 yards away.

### M2 — the flat −0.075 open-field calibration offset

`OPEN_FIELD_CALIBRATION_OFFSET = 0.075` is subtracted from every "open"
(`sep_roll > 1.0`, i.e. ~96% of normal targets) completion probability. It was
added in the clock_physics_v020 Round 7 pass to drag the *population* number down
from 69.7% → 64.4%. It's a single scalar applied regardless of depth, so it
penalises a 5-yard throw and a 25-yard throw by the same 7.5 percentage points —
which is a much bigger *relative* hit on the deep throw's lower base rate, and
structurally the wrong shape.

### M3 — the air-yards sampler doesn't fully reproduce receiver ADOT (NOT being fixed via the sampler)

`air_yards_v_0_1_1` (V.0.3.0) features: `down, ydstogo, yardline_100,
score_differential, game_seconds_remaining, cpoe_by_filter, target_share_by_filter,
carry_share_by_filter, play_ttt, avg_air_yards_per_att (QB), deep_target_rate
(receiver)`. Receiver ADOT is deliberately not a feature (collinear with
air-yards → biased model — Cam's call, do not add it). `deep_target_rate` is the
proxy. If deep specialists still get too many short throws in-sim after Phase 1's
ADOT work, the lever is the depth *curve* (Phase 2), not the sampler.

### M4 — small-sample zone splits / ADOT for low-volume players (data, not model)

`splits.*.catch_rate` and `adot` for thin-résumé players are noise: a backup with
6 career targets reads `catch_rate` 1.00; Thornton's 26.3 ADOT is itself a
small-sample artifact (no WR sustains 26 over a full season — ~19–20 is the real
ceiling). A1 partly contains this (it only syncs `catch_rate` for
`target_share ≥ 0.04`), but the underlying `adot` and the zone splits themselves
are still unshrunk.

---

## 3. Goals / non-goals

**Goals**
- Population completion rate stays at **64.40% ± 0.5pp** after the change.
- Per-ADOT-bucket completion (0–5, 5–10, 10–15, 15–20, 20+) each within ~2pp of
  real 2021–2025 by-bucket rates.
- The flagged possession receivers land within ~3pp of their sheet `catch_rate`;
  Thornton-class deep specialists land near their *flat* (all-depth) rate, not
  20pp above it.
- `corr(ADOT, realized − sheet)` drops from +0.52 toward 0.

**Non-goals (this round)**
- Retraining the interception gate, YAC model, or air-yards model architecture.
  (M3 may need an air-yards *feature* add — treat that as a sub-decision, not a
  full retrain.)
- Contested-catch mechanics rework (separate deferred item, AGENTS.md §0).
- Red-zone / goal-line *efficiency* modelling beyond catch rate.

---

## 4. Phased approach

Order agreed with Cam 2026-09-06, revised after Phase 0. Measure after each.

### Phase 0 — Instrumentation + real baseline — DONE
- Real by-depth completion curve: `scripts/eda/analyze_completion_by_depth.py`
  → `docs/eda_outputs/completion_by_depth/README.md`.
- Sim measurement: `track_cmp_by_depth` hook in `game_engine.py` (off by
  default) + `scripts/eda/measure_sim_completion_by_depth.py`. Baseline saved
  to `docs/eda_outputs/completion_by_depth/sim_vs_real_2026.txt`.
- Findings: see the "Phase 0 findings" section above. Curve OK 0–30 yds; 30+
  tail broken (15% vs 29%); air-yards *sampler* mis-shapes the depth mix.

### Phase 1 — Recalibrate the air-yards sampler — DONE (verification pending)

The tri-gate sampler
(`src/nfl_sim/models/air_yards_v_0_1_1/inference.py`) predicts a point depth per
level (frozen XGBoost regressors, means are correct) then adds scatter. The old
scatter was a **symmetric Gaussian** (std 1.5/4.0/12.0) + hard clips [1,19]/≥20.
Two problems: the deep std (12) far exceeded the regressor's real residual RMSE
(7.76), and — the bigger one — the real conditional air-yards distribution
*within* the std and deep levels is **strongly right-skewed**, so a symmetric
bell over-fills the middle depth buckets.

**Change:** std/deep now sample from a **shifted Gamma** —
`sample = floor + (pred − floor)·Gamma(k, 1/k)` — which preserves the mean
(`E = pred`) and adds right-skew via `k` (1 = exponential, ∞ = Gaussian). Screen
stays a clipped Gaussian.

- Params in `metadata.json` `"sampling"` block; sampler reads them into attrs.
- Calibrated by `scripts/eda/calibrate_air_yards_sampler.py`: `--capture` dumps
  the 397k feature rows the engine actually feeds the sampler; `--calibrate`
  grid-searches (screen σ) × (std k, floor) × (deep k, floor) — 360 combos —
  replaying each offline against the real 2021-2025 target-depth histogram.
  Objective = Σ weighted |sim bucket % − real bucket %| + mean-drift penalty.
- **Fit score 42 → 8.** Chosen: `screen σ=2.2`, `std k=2.0 floor=−1`,
  `deep k=1.0 floor=14`. Offline bucket fit (all within ~2pp of real):

| bucket | old sim | new sim | real |
|---|---|---|---|
| ≤0 | 19.0 | 21.3 | 21.9 |
| 0–5 | 20.7 | 28.6 | 30.5 |
| 5–10 | 28.7 | 20.1 | 18.2 |
| 10–15 | 16.6 | 10.7 | 11.5 |
| 15–20 | 5.8 | 7.8 | 7.7 |
| 20–30 | 3.5 | 6.7 | 6.2 |
| 30+ | 5.6 | 4.7 | 4.0 |

- No booster retrain. Reversible (revert the metadata `"sampling"` block).

**Live sim confirmation** (`sim_vs_real_2026_phase1_gamma.txt`, 32 matchups wk1-2):
depth mix now matches real almost exactly —

| bucket | sim att% (was) | sim att% (now) | real |
|---|---|---|---|
| ≤0 | 18.8 | 23.4 | 21.9 |
| 0–5 | 24.4 | 29.4 | 30.5 |
| 5–10 | 28.4 | 19.1 | 18.2 |
| 10–15 | 14.4 | 10.5 | 11.5 |
| 15–20 | 5.1 | 7.2 | 7.7 |
| 20–30 | 3.9 | 6.2 | 6.2 |
| 30+ | 5.1 | 4.1 | 4.0 |

Completion-by-bucket 0–30 now all within ±4pp (mostly ±2). **Two things Phase 1
did NOT fix, as expected:**
- **30+ completion still 13.8% vs 29.3% real** (was 15.1). The depth *count* is
  right now, but the completion *curve* (`b1=−0.08`) keeps diving past 30 while
  real flattens ~29%. Also the sim's 30+ throws average deeper (~45 vs real 39).
  → **Phase 2.**
- **Overall completion 64.67% → 65.61%** (real 64.87). The mix shift toward
  short throws (where completion is high) lifted the population ~1pp. "Real
  curve @ sim's depth mix" = 65.07%, so the sim's per-throw completion is only
  ~0.5pp above correct-for-this-mix. → absorb in **Phase 4** (the flat offset).

Phase 1 succeeded on its stated goal (depth distribution). 97 tests pass.
- **Residual:** the gate routes only ~19% of throws to "screen" vs ~21.9% real —
  a gate-calibration issue not fixable by scatter tuning. Small; deferred.

### Phase 2 — completion depth curve — DONE (verification pending)

Phase 1 fixed the depth *distribution* but not the 30+ *completion* (13.8% vs
29.3% real): the base curve `logit(P) = b0 + b1·air_yards` is a single line
(`b1 = -0.08`), and real completion-vs-depth is convex — steep short, flat deep.

**Change:** `b1·x` → a two-slope piecewise line `_depth_logit(x)` (module-level
constants in `game_engine.py`): `DEPTH_B1_NEAR = -0.09` up to `DEPTH_KNEE = 23`
air yards, `DEPTH_B1_FAR = -0.025` beyond. Used identically in both the contested
and open paths, in both the anchor (`g(adot)`) and the prediction (`g(ay)`) — an
additive constant still cancels out of `delta_wr`'s `g(ay) − g(adot)`
construction, so the per-receiver anchoring property is preserved.

Calibrated by `scripts/eda/calibrate_completion_curve.py` (`--capture` dumps the
per-throw completion inputs via a new `capture_completion` engine hook;
`--calibrate` grid-searches `b1a × b1b × knee` — 125 combos — replaying the exact
completion formula against the real by-depth curve). **Fit err 52 → 16.** Replay
(non-screen throws):

| bucket | old | new | real |
|---|---|---|---|
| 5–10 | 68.2 | 68.4 | 63.0 |
| 10–15 | 60.1 | 59.4 | 56.3 |
| 15–20 | 51.6 | 49.8 | 50.9 |
| 20–30 | 39.0 | 38.7 | 38.1 |
| 30+ | **14.7** | **28.4** | 29.3 |

- **30+ tail fixed.** 15–30 tightens too.
- **Residual, deferred to Phase 3:** 5–15 yds still over-completes ~3–5pp. No
  two-slope curve fixes that without breaking 15–20 — it's a level-shift from
  the anchor (a possession WR thrown a few yards short of his ADOT still gets too
  large a boost — same M1 over-correction as Thornton, milder). Phase 3's 2c
  re-anchor is the fix.
- Reversible: set both slopes to −0.08.

**Live sim confirmation** (`sim_vs_real_2026_phase2_curve.txt`, 32 matchups):

| bucket | Phase 1 sim | Phase 2 sim | real |
|---|---|---|---|
| ≤0 | 80.7 | 80.8 | 76.9 |
| 0–5 | 72.5 | 73.3 | 74.3 |
| 5–10 | 66.8 | 67.2 | 63.0 |
| 10–15 | 58.3 | 57.6 | 56.3 |
| 15–20 | 49.6 | 48.3 | 50.9 |
| 20–30 | 37.5 | 37.1 | 38.1 |
| 30+ | **13.8** | **26.1** | 29.3 |

30+ completion **13.8 → 26.1** (−15.5pp gap → −3.2pp). Everything else within
±4pp. Overall completion 65.61 → **66.29** (real 64.87) — fixing the deep tail
adds completions; **Phase 4 pulls it back, but must be depth-scaled not flat**
(a bigger flat offset would re-break the 30+ bucket). 97 tests pass. Both
Phase 1 and Phase 2 are keepers.

Remaining after Phases 1+2:
- **5–10 over-completes +4.2** (and ≤0/screens +3.9). The 5–10 is the anchor
  over-correction → **Phase 3**. Screens are the hardcoded 0.80/0.83 constant →
  deferred (gate-calibration item).
- **Overall +1.4pp above target** → **Phase 4** (depth-scaled offset).

### Phase 3 — ADOT anchor soft-cap = option 2a — DONE (verification pending)

Design pivot from 2c → **2a**, driven by real data
(`scripts/eda/analyze_completion_by_receiver_adot.py` →
`docs/eda_outputs/completion_by_depth/receiver_adot_check.md`): completion at a
given throw depth is **~flat across receiver ADOT** (Q2/Q3/Q4 within ~2-5pp,
30+ dead flat at 31%). So the model's per-receiver depth re-scaling off personal
ADOT is largely spurious — a distance-weighted blend (2c) is over-engineered.
Simpler: just cap the ADOT used in the anchor.

`_anchor_adot(adot)` in `game_engine.py` — `adot` untouched up to
`ADOT_ANCHOR_KNEE = 16`, then `ADOT_ANCHOR_COMPRESS = 0.75` of the excess
removed (so ADOT 26 → 16 + 0.25·10 = 18.5). Used in the anchor
(`_depth_logit(_anchor_adot(adot))`) in both the contested and open paths; the
prediction still uses the real `air_yards`.

Calibrated (`calibrate_completion_curve.py --calibrate-adot` + a direct
by-ADOT-band replay on the captured throws) against the real receiver-ADOT ×
depth table. Offline effect:

| receiver ADOT band | base sim cmp% | after cap | note |
|---|---|---|---|
| 8–12 (possession) | 62.1 | **62.1** | untouched — every flagged WR is here |
| 12–15 | 59.8 | 59.8 | untouched |
| 15–18 | 53.0 | 52.3 | −0.7 |
| 18–22 | 65.7 | 61.1 | −4.6 |
| ≥22 (deep spec) | 71.5 | 63.5 | −8.0 |

- The ≥22 band still lands ~10pp above a strict target — floored by their
  `zone_baseline` (~0.53) + the fact the air-yards sampler still under-throws
  them (mean throw ~15 vs real ~25). Both are tiny-volume (≥22 = 0.5% of
  throws) and acceptable residual; the sampler under-throw is a gate-calibration
  item (deferred).
- Reversible: `ADOT_ANCHOR_COMPRESS = 0`.

### Phase 4 — sep_bonus fix + model reformulation — DONE (verification pending)

Two findings reshaped this phase:

1. **The real baseline was wrong.** `analyze_completion_by_depth.py` measured
   completion ÷ *all* pass attempts, incl. throwaways / batted balls. The sim's
   completion model only fires on real targets. Corrected baseline (real targets
   only, `receiver_player_id` not null): **overall 67.5%**, buckets
   81.4/76.4/65.9/58.8/52.8/39.2/30.1. The "64.40%" in AGENTS.md is the
   box-score number — a different quantity. **The sim was running ~1.5pp *low*,
   not high** — likely resolving the long-standing "completion too hot" note.

2. **`sep_bonus` double-counted separation.** `0.15·(sep_roll − 1.0)` added
   ~+6pp to every open throw on top of a `zone_baseline` that already reflects
   the receiver's separation. Fixed → **zero-mean per receiver**:
   `SEP_BONUS_SCALE·(sep_roll − receiver_avg_sep)` — only the play-to-play
   deviation matters.

**Model reformulation** (`game_engine.py`, both paths): the fitted two-slope
`_depth_logit` (Phase 2) couldn't match the real curve's shape (steep 3–8, flat
8–18, steep 18–25, flat 25+). Replaced with **the real curve itself, linearly
interpolated in logit space** (`_DEPTH_AY` / `_DEPTH_CMP` from Phase 0). New form:

```
logit(P) = curve(air_yards)
         + SKILL_SHRINK · (logit(anchor_rate) − curve(anchor_adot))
         + sep_deviation                      [open only]
P = sigmoid(logit P) + qb_cpoe − OFFSET       [offset open only]
```

`SKILL_SHRINK` (< 1) trusts the per-receiver term only partially — supported by
the flat-across-ADOT finding. `_anchor_adot` soft-cap (Phase 3) stays.

Calibrated (`calibrate_completion_curve.py --calibrate-p4`, 8-col re-capture
with `avg_separation`): **`SKILL_SHRINK = 0.7`, `SEP_BONUS_SCALE = 0.13`,
`OPEN_FIELD_CALIBRATION_OFFSET = 0.0`** (the old 0.075 was compensating for the
double-count + wrong baseline — both gone). Offline replay (non-screen, runs
~+2.5pp hot vs live): 0–5 fine, 5–30 within ~1pp of real once the replay bias is
removed, 20–30 slightly high.

- Reversible: `SKILL_SHRINK = 1`, `SEP_BONUS_SCALE = 0.15` + revert sep line to
  `(sep_roll − 1.0)`, `OFFSET = 0.075`, `_depth_logit` → flat line.

**Live sim confirmation** (`sim_vs_real_2026_phase4.txt`, 32 matchups) — against
the corrected real-target baseline:

| bucket | sim cmp% | real cmp% | Δ |
|---|---|---|---|
| ≤0 | 80.8 | 81.4 | −0.6 |
| 0–5 | 76.7 | 76.4 | +0.3 |
| 5–10 | 67.1 | 65.9 | +1.2 |
| 10–15 | 59.7 | 58.8 | +0.9 |
| 15–20 | 52.8 | 52.8 | 0.0 |
| 20–30 | 42.2 | 39.2 | +3.0 |
| 30+ | 30.1 | 30.1 | 0.0 |

Overall **68.3%** vs 67.9% (real curve @ sim's depth mix) — **+0.5pp**. Every
bucket within ±1.2pp except 20–30 (+3.0, minor — 6% of throws; bucket-internal
skew toward the 20–22 end). Phase 4 done. 97 tests pass.

**Big-picture finding:** the sim's ~68% population completion — flagged for
sessions as "running hot vs 64.4%" — was **never actually wrong**. 64.4% is
completions ÷ all attempts (throwaways included); the completion model's
comparable target is 67.9%. What was wrong was the *shape* (deep craterd, masked
in aggregate by the flat −0.075 offset). Update AGENTS.md §0 accordingly.

### Phase 5 — Data shrink (M4), only if still needed
- If deep specialists still misbehave after 2c: shrink `adot` toward a
  prior-season / role prior for thin-résumé players, soft-cap sustained WR ADOT
  ~20 (Cam: "fudge it down to 20, keep the ordering"). Sync into
  `preseason_projection` too (weekly-taper desync risk).
- May be unnecessary — 2c is designed to make the anchor value low-sensitivity.

### ~~Phase — add ADOT to the air-yards model~~ — CUT (Cam, 2026-09-06)
Deliberate exclusion (ADOT ≈ air-yards, collinear → biased model). If the gap
persists, the lever is the depth curve (Phase 3) or the sampler dispersion
(Phase 1), never a sampler feature add.

### Phase 6 — Full verification
- `run_full_season_sim_2026.py` re-sim (the real verification, per AGENTS.md's
  "individually-verified pieces ≠ verified whole system" lesson).
- Re-run `catch_rate_diag.py` on the fresh parquet. Acceptance = §3 goals.
- Weeks 1–4 2025 batch audit: sacks/game, completion %, pass yards/game, the new
  by-bucket metrics all within tolerance; no regression in the clock_physics
  numbers.
- Spot-check readable play-by-play for a deep-heavy and a possession-heavy team.

---

## 5. Risks

| Risk | Mitigation |
|---|---|
| Moving completion shape shifts **pass yards/game** (already −10% per AGENTS.md) and **sacks** (throwaway/scramble interplay). | Phase 5 batch audit covers all three; treat pass-yards as a co-metric, not an afterthought. The air-yards gap is separately tracked — don't try to fix it here. |
| Re-anchoring changes **INT rate** (Gate 4 takes `air_yards`, not completion, but the throwaway→sack path is downstream of completion). | Include INT/game in Phase 5 acceptance. |
| Phase 1 ADOT shrink changes **air-yards sampling** for everyone (it's a feature) → knock-on to YAC, yards. | Do Phase 1 in isolation, measure, before Phase 2. |
| `preseason_projection` desync: A1 syncs `catch_rate` splits into `pp` too, but Phase 1's ADOT/split shrink must do the same or the weekly in-season taper re-introduces the raw values over weeks 1–5. | Explicit sub-task in Phase 1. |
| DFS week path (`apply_team_week_overrides` → `apply_sheet_helpers.apply_sheet_to_traits`) does **not** carry A1's catch_rate→splits sync. | [§7](#7-what-a1-already-fixed) — port the same logic, or have the DFS compile re-run `apply_preseason_overrides`'s sync. Small, do alongside Phase 1. |

## 6. Open questions for Cam

Resolved 2026-09-06 (see "Cam's direction" above): Q2 ADOT ceiling → ~20, fudge
the tail down, keep the ordering. Q3 rz/gl columns → yes, historical + league-avg
for rookies, not hand-tuned. Q4 order → A2 → blowout → week-aware.

**Resolved:** Q1 Thornton target → leave the sheet at 0.41 for now (Cam,
2026-09-06).

**Still open:**
1. **Screen completion.** Real ≤0 AY = 76.9%; sim hardcodes 0.80 (WR/TE) /
   0.83 (RB), measured sim ≤0 = 80.9%. Nudge the WR/TE screen constant to ~0.77
   as part of Phase 3, or leave it? (RB screen completion is genuinely higher
   than WR — a by-position real pull would set both.)
2. **The 30+ tail (−14pp).** Straightforward curve fix (floor / flatten past
   ~25 yds). Fold into Phase 2 or do as its own mini-phase first? It's isolated
   and low-risk — recommend doing it first so Phase 2's per-receiver fitting
   isn't fighting a broken tail.
3. **The air-yards sampler shape (M3).** Phase 0 shows it's the dominant driver
   of the possession-WR aggregate gap, and it is NOT fixable by Phase 2's curve
   work. Cam ruled out adding ADOT as a feature (collinearity) — but 3b
   (recalibrate the sampler's dispersion, no new feature) and 3c (post-hoc
   reshape toward the real bucket distribution) are still on the table. Do we
   pursue one of those, or take 3d (accept the residual if Phases 1–2 + the
   30+ fix get the flagged receivers within ~4–5pp)?

---

## 7. What A1 already fixed

**Change:** `scripts/roster_management/apply_preseason_overrides_v_0_1_0.py` now
syncs a hand-tuned flat `catch_rate` into `splits.{primary,redzone,goalline}
.catch_rate` (and the `preseason_projection` mirror):

- `splits.primary.catch_rate` ← the flat sheet value.
- `splits.redzone` / `splits.goalline` ← shifted by the same delta (zone shape
  preserved), clipped to [0.05, 0.99].
- Only for players with `target_share ≥ 0.04` (`MIN_TARGET_SHARE_FOR_RATE_SYNC`)
  — below that the flat value is small-sample noise and the build default is
  left alone.

**Effect (246 players moved on the 2026 apply):** Olave primary 0.644 → 0.699,
Diggs 0.62 → 0.812, A.J. Brown 0.638 → 0.702. Stars whose flat already ≈ their
split (Chase, St. Brown) barely move. Tests: `tests/test_apply_preseason_overrides.py`
(5). Full suite 97 passed. `validate_rosters.py 2026` 32/32.

**What A1 does NOT fix:** everything in §2. Thornton's primary split moved
0.505 → 0.415 but his sim catch rate barely changed — his problem is 100% the
M1 anchor + M4 ADOT, not the split value. A1 is the stopgap; A2 is the fix.

**Known A1 side effect:** for pass-catching RBs (target_share 0.04–0.17) the
redzone/goal-line catch rates delta-shifted up a few points off an already-high
base (JK Dobbins RZ 0.857 → 0.923). Minimal sim impact at those target shares;
the clean fix is Q3 above (real rz/gl catch-rate columns).
