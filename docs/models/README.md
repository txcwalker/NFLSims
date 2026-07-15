# Model Reference — NFLSims Simulation Engine

## Context

Index of every trained model and rules-based decision mechanism in the play-by-play engine (`src/nfl_sim/game_engine.py`). Each entry below is a short, current-state blurb — for full architecture, EDA, and evaluation detail, follow the link to that model's own doc in this folder.

**Maintenance rule: this doc, and every doc it links to, describes current reality only.** When a model is retrained or a mechanism changes, edit the relevant section/doc in place — replace the old feature list/metrics with the new ones. Don't append dated changelog entries, "as of" stamps, or a running history of what used to be true. Round-by-round retrain history belongs in `AGENTS.md`/`WORKLOG.md`; these docs are not that.

Every claim below is verified against the actual call site in `game_engine.py` (the exact array passed to `.predict()`/`inplace_predict()`/`.sample()`), not against code comments or a model's own `metadata.json` alone — both have been found to describe an earlier or aspirational architecture rather than what's deployed.

---

## 1. Architecture Note: The Hot-Loop Bypass

Most models below have a wrapper method in `model_registry.py` (`predict_play_selection_proba`, `predict_sack_proba`, `predict_yac`, `predict_rush_yards`, `predict_4th_down_probas`, `predict_win_probability`, etc.) that is **not called from the live simulation loop** — the only exception is `predict_fg_success`. `game_engine.py` instead reaches directly into each model's booster/coefficient objects and builds feature vectors inline, for speed. Two consequences worth knowing before trusting any wrapper method or its surrounding doc comment:
- Wrapper methods and some `inference.py` `.predict()` methods are dead code for the live sim — only relevant for standalone testing.
- `ModelRegistry.predict_4th_down_probas` and `ModelRegistry.predict_play_selection_proba` are complete, independent reimplementations of logic that also exists (and is what's actually used) in vectorized form inside `game_engine.py`. Nothing keeps the two in sync — treat the `game_engine.py` version as authoritative always.

---

## 2. Models Used in the Live Simulation

### 2.1 Play Selection (pass/run choice) — [play_selection_v_0_1_0.md](play_selection_v_0_1_0.md)
33 independent XGBoost classifiers (11 down/distance buckets × 3 zones), 9 features (situational + QB CPOE + starting-RB target/carry share). Output feeds the PROE overlay (§2.13) before the pass/run Bernoulli draw. `proe_by_filter`, a second redundant PROE feature baked into the classifier, was removed — the overlay is now the sole PROE mechanism.

### 2.2 Air Yards — [air_yards_v_0_1_1.md](air_yards_v_0_1_1.md)
Zone-split "double hurdle": a 3-way gate (screen/standard/deep) per zone, then a dedicated regressor per (zone × gate). 11 features, including `play_ttt` (per-play sampled time-to-throw), `avg_air_yards_per_att`, and `deep_target_rate` — the latter is now the single most important feature in most zones. A real player-name-join bug (DNA full names vs. nflfastR's abbreviated PBP names) was found and fixed here; see the doc for the fix and before/after metrics.

### 2.3 Completion Probability (in-engine, no separate artifact or doc)
- **Location:** `game_engine.py:1175-1293`, built inline — no joblib/metadata.json, no folder.
- **Type:** Logistic depth-decay model, split by a per-play simulated separation roll (`sep_roll = max(0, Normal(avg_separation_yds, 1.0))`):
  - **Contested** (`sep_roll ≤ 1.0`): `logit(P) = b0 + b1·air_yards + delta_wr`, anchored to the receiver's real `contested_catch_rate` at their own ADOT.
  - **Open** (`sep_roll > 1.0`): `logit(P) = b0 + b1·air_yards + delta_wr + sep_bonus`, anchored to the receiver's real zone catch-rate baseline at their own ADOT.
  - Both regimes share `b0 = 1.5`, `b1 = -0.08` and add QB CPOE in probability space (post-sigmoid).
  - **Screens** (`air_yards ≤ 0`) bypass this — flat position-based rate (RB 83%, WR/TE 80%).
  - Final probability clipped to `[0.01, 0.99]`.
- **Calibration constant:** `OPEN_FIELD_CALIBRATION_OFFSET = 0.075`, open-field path only.

### 2.4 YAC (Yards After Catch) — [yac_v_0_1_1.md](yac_v_0_1_1.md)
XGBoost regressor per zone (primary/redzone/goalline), 9 features including `room_after_catch` (dominant near the goal line). Same player-name-join bug as air-yards, fixed the same way — `cpoe_by_filter`/`target_share_by_filter`/`carry_share_by_filter` went from 0% to 5-8% feature importance post-fix.

### 2.5 Rush Yards — [rush_yards_v_0_1_0.md](rush_yards_v_0_1_0.md)
Log-shifted (`ln(y+30)`) XGBoost regressor per zone + empirical residual bootstrap, 7 features. Untouched by the current round of retrains; still accurate.

### 2.6-2.9 Chaos Model Gates 1-4 — [chaos_v_0_1_0.md](chaos_v_0_1_0.md)
Sequential gated pipeline: Gate 1 pre-snap penalty (logistic regression), Gate 2 sack classifier (XGBoost, 11 features including `trench_dna.json`'s raw per-team metrics), Gate 3 sack yardage (fitted Gamma distribution), Gate 4 interception classifier (XGBoost, 6 features — `off_int_rate_l4` was removed, a hardcoded dead constant with zero real variation). Downstream, rules-based: QB scramble escape hatch → throwaway roll → sack-fumble roll.

### 2.10 Field Goal — [fg_v_0_1_0.md](fg_v_0_1_0.md)
Single-feature (`kick_distance`) Logistic Regression, chosen over XGBoost for strict monotonicity. The one model whose `model_registry.py` wrapper (`predict_fg_success`) is actually used live.

### 2.11 4th Down Conversion — [fourth_down_conversion_v_0_1_0.md](fourth_down_conversion_v_0_1_0.md)
XGBoost model, 4 situational features, with boundary-safety overlays (immediate success at `ydstogo ≤ 0`, hard cap at long distances) and a `coach_aggression` DNA overlay added onto the model's raw output before the PUNT/FG/GO decision.

### 2.12 Clock Pace — [clock_pace_v_0_1_0.md](clock_pace_v_0_1_0.md)
Empirical bootstrap, not a trained model by design: per-play lookup of `(time-window × score-margin tier) → real sample pool`, drawn via `np.random.choice`. Built from 116,258 real running-clock snaps.

### 2.13 PROE Overlay
- **File:** `src/nfl_sim/proe_overlay_v_0_1_0.py` — rules-based logit-space adjustment, not a trained model, no folder.
- **Data sources:** `coordinator_atlas.json`'s `off_proe` (historical per-coach PROE, season-level) + `team_to_coach_2025.json`.
- **Mechanism:** `get_coach_proe()` + `_proe_to_logit_offset()`, called once at engine `__init__` to precompute a static `self.proe_offsets[team]`, applied every play: `adjusted_pass_prob = sigmoid(logit(base_pass_prob) + proe_offset)` (`game_engine.py:774-776`). This is the sole PROE mechanism — it's what the frontend's PROE slider controls, and it's the only place coach pass-tendency enters play selection.
- **`play_ttt` (sibling per-play sampling mechanism, `game_engine.py:818-827`):** `play_ttt = clip(Normal(qb_dna.avg_time_to_throw_sec, 0.6), 1.5, 4.5)` — per-QB-median, per-play-random-sample. Feeds Gate 2 (§2.7) and the air-yards model (§2.2).

### 2.14 Empirical/Rules-Based Mechanics With No Model Artifact
- **Kickoff** (`game_engine.py:408-465`): flat touchback probability (20.68%), shifted log-normal return yardage (`mean=0.2134, sigma=2.9122`), flat +7 TD on a 100+ yard return.
- **Punt** (`game_engine.py:546-634`): touchback probability from a hardcoded inline logistic formula (`logit = 2.3127 − 0.0828 × yardline_100`), no model artifact. Punt distance ~ `randint(35,50)`. Fair-catch 29.42% flat. Return-TD 0.91% flat. Return yardage ~ shifted exponential.
- **Mid-play penalties**: pass plays — offensive holding 1.49%, DPI 1.20%, defensive holding 0.90%; run plays — offensive holding 1.49%. Flat empirical constants; accept/decline compares the penalty outcome to the play result.
- **Sack-fumble / general fumbles**: sack-fumble probability `0.0724 × (qb_fumble_rate/0.06) × 0.80`, 50/50 fumble-lost coin flip.
- **Scoring**: every touchdown awards a flat `+7`. **No extra-point or 2-point-conversion modeling exists** — planned before the season launches, not yet started (see `FUTURE_DEVELOPMENT.md`).

---

## 3. Models Not Used in the Live Simulation (Offline/Analysis Tools Only)

Real, trained, deployed models — just not part of the play-by-play path. Listed so nobody assumes they're live when auditing the engine.

### 3.1 Win Probability — [win_probability_v_0_1_0.md](win_probability_v_0_1_0.md)
Not called anywhere in `game_engine.py`. Used only by `nfl_positional_evaluator.py`'s `KEPConverter` for the standalone Positional Evaluator tool. Wiring it into in-sim decision-making is a real idea, deliberately not pursued for now.

### 3.2 Positional EP (Expected Points) — [positional_ep_v_0_1_0.md](positional_ep_v_0_1_0.md)
XGBoost regressor, 4 deliberately minimal features (no roster/DNA/score/clock). Not loaded by `model_registry.py`, not used in `game_engine.py`. Used by `nfl_positional_evaluator.py` directly.

### 3.3 EFSD (Expected Final Score Differential) — [efsd_v_0_1_0.md](efsd_v_0_1_0.md)
XGBoost regressor, same features as Win Probability, predicting final score margin directly. Evaluator-only by design, not wired into the play-by-play engine.

---

## 4. Known Gaps (Not Bugs — Just Absent)

- **No extra-point (PAT) or 2-point-conversion modeling.** Every touchdown is worth a flat 7 points. Planned before the season launches.
- **No real "last-4-games" rolling data anywhere** — every `*_l4` feature is either a duplicate of a season-aggregate value or a hardcoded constant. `trench_dna.json` and friends are season-level only; there's no week-granularity data source yet to build a real rolling window from.
- **`blitz_rate`, `off_pass_block_win_rate`, `times_to_pressure_sec`** — see `FUTURE_DEVELOPMENT.md`'s "Trench Data Pipeline" section.
