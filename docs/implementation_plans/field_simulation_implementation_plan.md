# NFLSims: DFS Tournament Field Simulation Engine (V1)

This plan replaces three currently-separate, partially-heuristic pieces of the Optimizer —
**GPP Projection** (`GPP_WEIGHTS_BY_TYPE` percentile blend), **Contest Simming** (the uniform-random
field sampler in `/api/optimize`), and **Portfolio Scoring** (`_compute_lineup_stats[_direct]`) —
with one coherent engine: simulate a realistic, archetype-composed tournament field, score our
lineup(s) against it using the same correlated per-iteration draws already used elsewhere in the
app, and let EV / ITM% / Top1% / Top0.1% fall out of that as measured outcomes rather than
formulas layered on top of a static projection number.

Status: draft for review. Nothing in this plan is implemented yet.

---

## 📐 Mathematical & Conceptual Framework

### 1. Why one engine, not three

A GPP lineup's "projection" for optimizer purposes was never really a *projection* — it was a
proxy for "how much does this player's tail matter for winning a top-heavy contest," which is
exactly the question contest simming and portfolio scoring also need to answer. Building three
separate patches risks three inconsistent half-measures. Building this once means:

- **GPP "projection"** stops existing as a stored number. The ILP objective is driven directly by
  correlated per-iteration draws (already built — `_build_correlation_matrix` + Cholesky) evaluated
  against the real payout curve, not a static P25/P50/P75/P95 blend.
- **Contest simming** becomes real: a field sampled from four tunable player archetypes, scored in
  the *same* iteration's game environment as our lineup (correlation-preserving), ranked, and paid
  out via the contest's actual tiers.
- **Portfolio scoring** (EV/ITM%/Top1%/Top0.1%) is a direct tally over N iterations of the above —
  not a separate formula.

### 2. Per-iteration simulation loop

For iteration `i = 1..N` (N ~ 1,000–10,000, resampled with replacement from the ~1,000 real
correlated per-player iterations already stored in the parquet cache):

1. Reuse iteration `i`'s joint player-outcome draw (already computed elsewhere in the app — no new
   simulation of game mechanics needed here).
2. Score our candidate lineup(s) using iteration `i`'s outcomes.
3. Score a **field sample** of `K` field lineups (archetype-composed, see §3) using the *same*
   iteration `i`'s outcomes — this is what keeps a shootout lifting our players and the field's
   players together, honestly.
4. Rank our lineup within `{our lineup} ∪ {K field lineups}`, then extrapolate that rank fraction to
   the contest's real entry count via order statistics (`estimated_rank = rank_fraction × real_field_size`)
   — we do not construct 150,000 literal lineups.
5. Look up the real payout for `estimated_rank` from the contest's actual payout tiers (already
   available via the DK contest picker → `get_dk_contest_payout()`).
6. Record the payout for iteration `i`.

After N iterations: `EV = mean(payouts) / entry_fee - 1`, `ITM% = P(payout > 0)`,
`Top1% = P(estimated_rank ≤ 1% of field)`, `Top0.1%` likewise. All measured, none pre-selected.

### 3. Field composition — four archetypes, one construction pipeline

Real sharps and "fake" sharps use similar-looking tools; the difference is in whether the inputs
feeding those tools are calibrated, not in the tools themselves. So there is **one stochastic,
leverage-and-correlation-aware lineup builder**, parameterized per archetype rather than four
separate algorithms.

| Parameter | Sharp | Fake Sharp | Casually Informed | Toilet Builder |
|---|---|---|---|---|
| Signal quality | Well-calibrated (= our own sim, no injected bias) | Same base signal, but see "mistake roll" below | Recency-biased + name/media-biased | Name/salary-driven only |
| Leverage weighting | Moderate, disciplined — embraces obvious high-conviction chalk, doesn't reflexively fade it | Attempted, same as sharp when no mistake is rolled | Minimal-to-none (chalk-heavy by default, not by design) | None |
| Stacking | Selective/conditional — only in games the archetype's own process has validated (not every game) | Attempted, less disciplined about *which* games justify it | Occasional naive QB+WR1 | None (any correlation is accidental) |
| Favorite-player anchoring | None | Weak | Strongest of the four | Present (fandom-driven) |
| Multi-entry portfolio | Genuine diversification, concentrated within its validated opportunity set | Attempted but shallower (more near-duplicate) | Low-moderate diversity if multi-entering | Single or very few entries, near-identical |
| Weekly mistake chance | ~5-10% chance of *any* mistake; ~5-10% of roster affected when it happens | See §4 below | Always-on low-grade bias (not a discrete roll) | N/A — no real analysis to be "wrong" about |
| Default entry share (tunable, placeholder) | ~10% | ~35-40% | ~30% | ~20-25% |

All percentages/weights in this table are **config, not constants** — see §5 for the parameter file.
Note these are shares of **entries**, not unique people (multi-entry volume is itself a parameter,
which is what lets a small sharp person-count still represent a meaningful entry-share).

### 4. Fake-sharp "mistake" mechanism

Two failure modes, both **must be biased, not just noisy** — unbiased variance around a correct
read doesn't hurt long-run EV in a GPP context, so the flaw has to have a consistent direction to
explain "wins sometimes, bleeds long-run as a group":

- **Pet-player misreads** (the dominant failure mode). Per fake-sharp entrant, per week: a Bernoulli
  roll with **P(mistake) < 0.5** (calibration target: comfortably more than half of fake-sharp
  entrants play a solid, landmine-free week). When it hits, 1+ roster picks get a misread —
  concentrated on **high-uncertainty players**, using each player's own P25–P95 spread as a proxy
  for "genuinely ambiguous role" (new starters, backfield committees, injury-replacement situations
  — the exact category a service like ETR earns its reputation calling correctly). The misread can
  go either direction (overweight or underweight) — this is not "always fades chalk," it's "gets an
  ambiguous read wrong, in either direction, including badly underweighting a play that's simply
  and obviously correct."
- **Ownership overshoot as a hype-chasing stand-in.** We aren't modeling within-week ownership drift
  (no infrastructure for it, explicitly deferred — see §7). Instead: a lineup's 9 rostered players'
  **additive ownership** normally targets a week-relative soft cap (baseline ~130%, itself a function
  of that week's chalk concentration, not a fixed constant). On weeks a fake-sharp entrant *also*
  rolls the mistake above, their additive ownership is allowed to run **10-15 points over that cap**
  — approximating "bought into a play that quietly became the field's chalk by lock" without
  simulating the time dimension that actually caused it. Not every fake-sharp entrant, and not every
  week — only the subset that rolls the mistake.

### 5. Config: `data/dna/field_archetype_params.json` [NEW]

All archetype percentages, parameter weights, and mistake-frequency rates live here, not hardcoded —
this is placeholder data pending real calibration once we're collecting observed contest results
(see the ownership work earlier this cycle for the same philosophy). Shape:

```json
{
  "archetype_shares": { "sharp": 0.10, "fake_sharp": 0.38, "casual": 0.30, "toilet": 0.22 },
  "sharp":      { "leverage_weight": 0.7, "stacking_threshold_pctile": 0.7, "mistake_p": 0.075, "mistake_roster_frac": 0.08, "additive_own_target": 130 },
  "fake_sharp": { "leverage_weight": 0.6, "stacking_threshold_pctile": 0.4, "mistake_p": 0.45,  "pet_misread_pctile_weight": true, "additive_own_overshoot": [10, 15] },
  "casual":     { "leverage_weight": 0.15, "recency_bias_weight": 0.4, "name_bias_weight": 0.3 },
  "toilet":     { "leverage_weight": 0.0, "name_bias_weight": 0.8, "entries_per_person": 1 }
}
```

### 6. Build/score cache split (the performance lever)

Given the goal of "small optimizer tweaks shouldn't cost minutes," field **construction** and field
**scoring** are separate steps with separate cost profiles:

- **Build** (`_build_field_sample()`, expensive): construct `K` archetype-composed field lineups for
  a given slate + contest configuration. Cached per (week, draft_group_id, contest_id) — same
  pattern as the existing weekly ownership/cash-lineup caches. This is allowed to take real time.
- **Score** (cheap, called on every optimize/edit): re-score our candidate lineup(s) against the
  *already-built* cached field sample. No reconstruction, just array arithmetic over the existing
  per-iteration draws.
- Exact `K` (field sample size) and the specific "enough changed, rebuild the field" invalidation
  threshold are **not fixed by this plan** — both need a benchmarked prototype first (see §7).

---

## 🛠️ Proposed Changes

### 1. Backend Models

#### [NEW] `data/dna/field_archetype_params.json`
Tunable archetype config as specified in §5.

#### [NEW] `src/nfl_sim/field_simulator.py`
Core new module, alongside the existing `src/nfl_sim/optimizer.py` (which stays as-is for the raw
ILP primitives this module will call into):
- `load_archetype_params() -> dict` — reads and validates the JSON config.
- `build_field_lineup(players, archetype, params, rng) -> list` — the one shared stochastic
  constructor, branching on `archetype`'s parameters (signal bias, leverage weight, stacking
  discipline, mistake roll) rather than four separate functions.
- `build_field_sample(players, archetype_shares, K, params) -> list[lineup]` — builds the full
  `K`-lineup field, proportioned per §3's shares.
- `score_field_against_iteration(field_sample, iteration_idx) -> np.ndarray` — vectorized scoring
  reusing the existing per-iteration correlated draws.
- `simulate_portfolio(our_lineups, field_sample, payout_structure, real_field_size, n_iterations) -> dict`
  — the per-iteration loop from §2; returns EV/ITM%/Top1%/Top0.1% per lineup plus the portfolio
  aggregate.

### 2. Backend API Service

#### [MODIFY] `src/api/app.py`
- `_compute_ownership()`'s softmax model is retired in favor of ownership computed as **observed
  frequency of each player across the cached field sample** — structurally bounded [0, 100] by
  construction, so the `min(100.0, ...)` clamp added this cycle becomes a redundant safety net
  rather than a load-bearing fix (leave it in regardless, cheap insurance).
- `GPP_WEIGHTS_BY_TYPE` / `computeGppProj` (frontend, see below) is retired; the ILP objective in
  `/api/optimize` draws directly from correlated per-iteration outcomes.
- `_compute_lineup_stats` / `_compute_lineup_stats_direct` and the uniform-random
  `build_field_lineup()` closure inside `/api/optimize` (today's crude version, §"existing gaps"
  from the field-simming discussion) are replaced by calls into the new `field_simulator` module.
- `[NEW]` `POST /api/field_sample?week=&draft_group_id=&contest_id=` — triggers/returns the cached
  field build (§6). `GET` variant to fetch an already-built sample's summary (composition, size,
  build timestamp) for display.
- `/api/optimize` changes to: fetch (not rebuild) the cached field sample, call
  `simulate_portfolio()`, return the same EV/ITM%/Top1%/Top0.1% shape the frontend already expects
  — this endpoint's response contract to the frontend should not need to change.

### 3. Frontend Pages

#### [MODIFY] `frontend/src/pages/Optimizer.jsx`
- Remove `GPP_WEIGHTS_BY_TYPE` / `computeGppProj` / the "GPP Proj" column — no longer a stored
  number to display or quick-set from.
- Add a small field-sample status indicator near Settings (composition %, size, last-built time,
  "Rebuild Field" action) so it's visible when a stale field is being reused.

#### [MODIFY] `frontend/src/pages/Leverage.jsx` / `frontend/src/pages/CashLineups.jsx`
- No structural changes expected — both already consume `ownership_proj`, which will simply start
  reflecting the field-sample-derived number instead of the softmax one, same field name.

---

## 🧪 Verification Plan

### Automated Verification
- `tests/test_field_simulator.py`:
  - Archetype composition of a built field sample matches `archetype_shares` within sampling
    tolerance.
  - No player's field-derived ownership can exceed 100% (structural, but assert it anyway).
  - Sharp-archetype lineups' additive ownership stays at/under the week's soft cap in the
    overwhelming majority of constructed lineups; fake-sharp lineups exceed it only on
    mistake-rolled entrants, at roughly the configured frequency, across a large sample of built
    entrants.
  - Payout lookup for a synthetic contest matches known tier boundaries exactly at the boundaries.

### Manual Verification
- Run a clear, high-conviction chalk case (a Marshawn-Lloyd-style "obvious workhorse role, price is
  too good to fade" player) through the field builder and confirm: sharp-archetype lineups roster
  him near 100%; some fraction of fake-sharp-archetype lineups show a misread (under- or
  over-weighted) instead.
- Compare `/api/optimize`'s EV/ITM%/Top1%/Top0.1% output for a real slate before and after this
  change lands, and sanity-check the numbers against known contest-structure baselines (e.g. ITM%
  should track the contest's real paying-position fraction under a coin-flip-skill assumption).
- Time a sequence of small Optimizer edits (lock a player, tweak a projection) post-launch to
  confirm the build/score cache split is actually keeping iteration fast.

---

## Explicitly deferred (not in this V1)

- **Within-week ownership drift / hype-steam timing** — no infrastructure to track "early-week vs.
  lock-week" ownership yet; the additive-ownership-overshoot mechanism (§4) is the stand-in.
- **Exact field sample size `K` and cache-invalidation threshold** — pending a benchmarked
  prototype; not guessed at in this plan.
- **Multi-entry correlation *within* one archetype persona** (e.g. modeling a single sharp's 20
  entries as a deliberately coordinated portfolio vs. 20 independent archetype draws) — v1 treats
  each field-sample lineup as an independent draw from its archetype; revisit if the resulting field
  doesn't feel realistic enough once we can eyeball it.
