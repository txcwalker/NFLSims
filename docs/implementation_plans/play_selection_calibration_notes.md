# Play-Selection Calibration — 2026-09-06

Work done alongside the completion-rate calibration (A2). The base
`play_selection_v_0_1_0` XGBoost model (V.0.3.0, 2020-2025, 9 features incl.
`score_differential` + `leverage`) is **well-calibrated** — its raw output
averages 57.5% pass leaguewide, exactly the real 2021-2025 rate. Two fixes
layered on top, plus the throwaway mechanism which is really a completion issue.

---

## UPDATE 2026-09-09 — V.0.4.0 dropback label (audit S2-4)

The V.0.3.0 label `is_pass = (play_type == "pass")` **excluded QB scrambles**
(nflfastR files those as `play_type == "run"`). The engine treats a "pass"
decision as a dropback and then independently diverts ~5% of them to scrambles
— so scrambles were subtracted twice and the realized box pass rate came out
~3pp under real. Diagnosis (2026-09-09, full week-1 sim + real 2021-25 PBP):

| stage | old | real |
|---|---|---|
| base model alone | 0.587 | `play_type==pass` 5yr **0.577** |
| + PROE + Q4 overlay | 0.575 | — |
| **realized box pass rate** | **0.544** | **0.567** |

Every one of the 32 teams was below its own 5yr rate (mean −3.0pp) — a uniform
league-wide mechanical drain, i.e. NOT a per-coach PROE problem. The base model
does **not** lean run; PROE nets −1.2pp and lands the decision at ≈ real.

**Fix:** retrain with `is_pass = (play_type == "pass") | (qb_scramble == 1)` —
a true DROPBACK rate (2020-25 label rate **0.609**, scrambles 2.9% of plays).
The engine's att/sack/scramble split of *that* is what now matches real box
scores. No PROE change, no feature change, `iteration_range`/`best_iteration`
handling unchanged. Per-bucket `pred` vs `real` stays tight in every
high-volume bucket.

Week-1 sim result (old → new):

| metric | old | new | real |
|---|---|---|---|
| base_pass_prob | 0.587 | 0.618 | dropback 5yr 0.606 |
| adjusted (+PROE) | 0.575 | 0.604 | — |
| pass att / team / g | 30.7 | **32.6** | 32.0 |
| rush att / team / g | 27.6 | **26.2** | 26.3 |
| sacks / team / g | ~2.3 | 2.47 | 2.41 |
| **box pass rate** | 0.544 | **0.572** | 0.567 |

+0.5pp over real now (was −2.3pp under) — negligible, and the right side of the
line given over/unders were reading low. Full-season regenerate follows.

## What changed (`game_engine.py`, all module-level constants, all reversible)

| constant | value | what |
|---|---|---|
| `CLEAN_POCKET_THROWAWAY_RATE` | 0.055 | ~5% of dropbacks become clean-pocket throwaways (was ~0.4% from the sack-diversion path only). Brought QB cmp% 68% → 64.2%, real ~64.5%. Diverted plays are plain incompletions — no target. |
| `PRESSURE_THROWAWAY_DIVERT` | 0.06 | (unchanged) fraction of would-be-sacks that become throwaways |
| `PROE_FADE_SLOPE` / `_MAX` / `_2H_FACTOR` | 0.04 / 0.85 / 2.0 | PROE is an all-time neutral-script tendency. `_proe_script_weight()` fades the per-team PROE offset toward 0 as the margin blows out (faster after halftime), so a run-heavy coach doesn't keep running down 17. One-score games untouched. |
| `Q4_TRAIL_BLEND` | 0.65 | The base model under-reacts to being down 2+ scores in Q4. `_q4_trailing_pass_target()` gives the real empirical pass rate for that situation; the engine blends 65% toward it. Only fires Q4, only down 9+. |

Diagnostics: `scripts/eda/analyze_pass_rate_by_script.py` (real curve),
`scripts/eda/analyze_completion_by_depth.py`, and the `track_playcall` /
`track_cmp_by_depth` / `capture_completion` hooks on `NFLGameEngine`
(`BatchSimulator.run_batch(track_playcall=True)` etc., off by default).

## Post-fix state (32-matchup measurement vs real 2021-2025)

- League pass rate 56.4% vs 57.5% historical. Cam's call: the ~1pp is fine —
  the current coach set genuinely skews run on an all-time basis and the league
  is trending that way.
- Q4 trailing bands all within ~1pp of real EXCEPT one (below).

## Open / deferred

1. **Down exactly 2 scores (9–16), final 4:00 of Q4** — sim ~86% pass vs real
   ~90%. `_q4_trailing_pass_target`'s `two_score` curve isn't aggressive enough
   at the very end. ~1.3% of all plays, near-zero season-stat impact. Cam is
   "tempted to just make every play a pass except 3rd/4th-and-1 or -2" there —
   left as-is for now, revisit if it matters.
2. **Leading teams, final 4:00** — sim passes ~7–12pp more than real (lead 1–8
   Q4<4: +7; lead 9–16 Q4<4: +12). The base model doesn't kill the clock hard
   enough. Deferred (Cam's call) — low season-stat impact, but a
   `_q4_leading_run_target` mirror of the trailing overlay would close it.
3. **~5% fewer total offensive plays** than real (1019 vs ~1080 / team / season)
   — a pace/clock issue, not play-selection. Separate from this work; ties to
   the clock_physics_v020 snap-count thread.
4. **6 of 8 `coach_dna.json` levers are dead code** — `air_yards_tendency`,
   `screen_rate`, `play_action_rate`, `no_huddle_rate`, `rpo_rate`,
   `conservative_score_bias` are read from the file and never consumed. Only
   `proe` and `deep_shot_rate` (→ `coach_aggression`, 4th-down GO bias) are
   live. The zone-split PROE from `coach_coordinator_levers_2026.csv` is only
   used by the positional evaluator, not the main sim.
