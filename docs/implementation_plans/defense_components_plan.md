# Defensive components for the sim — plan (Stages 1-3)

Status: DRAFT 2026-10-08 (Cam + Claude). Stage 1 targeted for Week 6; Stages 2-3 are offseason projects (revisit February).

## Why

Across 30 teams the sim's points scored track real points scored (r = 0.63) but its points allowed do not track real
points allowed (r = -0.04); sim game totals correlate 0.07 with Vegas totals (a naive recent-form total correlates 0.79).
The sim's only defensive inputs are trench composites (`pass_def_z` is a pass-RUSH composite — the engine's own comment
calls it a stopgap for a real coverage feature), the sack gates, and the rush trench gate. Coverage, back-seven run
defense and secondary quality are not modelled; the Week 5 DET @ ARI total (sim 40 vs Vegas 54.5) is the visible symptom.

## What the data says (read before designing)

Script: [scripts/eda/analyze_defense_predictiveness.py](../../scripts/eda/analyze_defense_predictiveness.py); full tables in
`docs/eda_outputs/defense/predictiveness.md`. 2021-25 regular seasons, win probability 5-95%. "opp-adj" = weighted ridge on
offense + defense effects per game (schedule adjustment). Numbers are mean correlations across the 32 teams of each season.

Early-window rating -> LATER opponent-adjusted EPA/play allowed:

| early predictor | wk1-4 -> 5-8 | wk1-8 -> 9-12 | wk1-8 -> 9-17 | wk1-12 -> 13-17 |
|---|---|---|---|---|
| points allowed (raw / opp-adj) | 0.14 / 0.14 | 0.13 / 0.14 | 0.20 / 0.20 | 0.15 / 0.15 |
| yards per play allowed (raw / opp-adj) | 0.07 / 0.07 | 0.21 / 0.21 | 0.25 / 0.25 | 0.21 / 0.20 |
| EPA/play allowed (raw / opp-adj) | 0.10 / 0.11 | 0.16 / 0.16 | 0.20 / 0.20 | 0.18 / 0.18 |
| success rate allowed (opp-adj) | 0.12 | 0.18 | 0.22 | 0.18 |
| rush EPA allowed (opp-adj) | 0.20 | 0.20 | 0.16 | 0.12 |
| pass EPA allowed (opp-adj) | 0.06 | 0.09 | 0.16 | 0.17 |
| EPA per target to WR / TE / RB (opp-adj) | 0.02 / -0.03 / 0.05 | 0.02 / 0.14 / -0.03 | 0.09 / 0.15 / 0.01 | 0.15 / 0.04 / 0.04 |
| *ceiling: reliability of the target window itself* | *0.14* | *0.05* | *0.23* | *0.14* |

Answers to the design questions:
1. **Longer windows help a little** (EPA allowed: 0.10 at 4 games, 0.16-0.20 from 8 games).
2. **Simple stats are as good as fancy ones.** Yards per play allowed (0.25) and success rate (0.22) beat EPA (0.20) for the long horizon;
   points allowed is a bit worse (0.20, and 0.14 against the weeks 5-8 target).
3. **Schedule adjustment does not improve prediction** in this data (raw vs opp-adj are within 0.01 everywhere). We should still adjust, for
   interpretation (30 points allowed to the Lions is not 30 allowed to the Browns), but do not expect it to move accuracy.
4. **By offensive position: no usable signal.** EPA per target allowed to WR, TE or RB is noise in every window (the TE column's 0.14-0.15 is
   one window each and does not repeat). Do not build position-specific defense on this.
5. **Last season's rating adds nothing** (correlation with later defense -0.04 to +0.06; adding it to a regression with the early window
   leaves R^2 at 1-4%). Defense does not persist year to year in this era. The prior should be the league mean, not last year's team.
6. **The ceiling is low.** Even a 9-game defensive rating has split-half reliability of only ~0.23. Most between-team defensive differences
   we see in a month of games are luck. Predictors already sit at or near that ceiling, so no choice of stat will do much better.

Game totals themselves are mostly noise: market totals correlate 0.33 with actual totals (917 games, weeks 5-12, 2018-25; sd of actual
totals 13.5 vs 4.5 for the line), a 4-game scoring form 0.17, and form adds nothing once the market is known. In weeks 1-4 of 2026 (64 games)
the sim's totals correlated 0.28 with actual (market 0.46) and its margins 0.18 (market 0.31); sim vs market was 0.57 (totals) / 0.80 (margins).
The Week 5 sim-vs-market total correlation (0.12) is unusually low because of a few extreme lines (DET @ ARI 54.5, HOU @ TEN 38.5).

## Stage 1 — revised: a small, heavily shrunk team-defense term (target: Week 6)

Given the above, Stage 1 is deliberately modest:
- **Inputs (per team, from nflverse pbp, same weekly refresh as PROE/pace):** yards per play allowed and success rate allowed (best long-horizon
  predictors), rush EPA allowed (best at short horizons), each split pass/run where it helps. Opponent-adjusted for interpretation.
- **Shrinkage:** toward the league mean (NOT last season), with weight set by the measured reliability: roughly 4 games ~ 15-20%, 8 games ~ 30%,
  12+ games ~ 40%. Needs Cam's OK because it differs from the zero-preseason-weight rule used for offensive players (here there is no useful prior).
- **Engine hooks (offsets on existing terms; no retrains):** (1) replace `PASS_DEF_COMPLETION_SCALE * pass_def_z` with the new pass-defense
  rating; (2) rush-yards multiplier from the run rating next to the existing trench gate; (3) leave sacks and turnovers to Gate 2/2b/chaos.
- **Expectation:** the real defensive effect is small (a few tenths of a point per rating step). This will not close gaps like DET @ ARI 40 vs 54.5;
  those gaps are mostly the market's noise plus offense/pace/environment. It will stop the sim treating a defense that has shown nothing as exactly
  average, and it gives the Game Explorer a defensive dial.
- **Manual layer:** `defense_rating_overrides.csv` (team, unit, value, note), like `qb_rate_overrides.csv`, so Cam can hand-adjust a unit (e.g. ARI/DET
  secondaries) and have it flow to Game Explorer sims and the optimizer inputs.
- **Validation:** (a) backtest on 2021-25: rating from weeks 1-8 vs later yards/play and EPA allowed; (b) the 64-game audit must not regress
  (completion %, sacks, snap counts, league scoring); (c) slate check: sim points-allowed vs real points-allowed correlation (was -0.04) and
  sim-vs-market totals; (d) report effect sizes honestly.

## Stage 2 — deferred to offseason (bundle with Stage 3)

- Takeaway rates: the chaos model already carries interception/fumble gates — adjust there or move the logic out; do not
  add a second turnover mechanism. Not a priority.
- Red-zone / goal-line / third-down defense, pressure-to-completion interaction, blitz-conditional effects (FTN charting
  `n_blitzers`, `n_pass_rushers`, 2022+ only).
- Team-specific kicking/return and special-teams effects on field position.

## Stage 3 — offseason (February)

- Player-level defense: CB/S coverage, LB/EDGE roles, with injury handling in the same weekly-sheet / ledger flow used for
  offense.
- Coverage scheme data (man vs zone, box counts) from nflverse participation files (available for past seasons; lags for the
  current one).
- Re-derive the engine's `pass_def_z` stopgap and the static trench tiers on top of the new inputs.
- Re-evaluate whether sim totals need a team-environment term (pace, weather, roof) once defense is in.
