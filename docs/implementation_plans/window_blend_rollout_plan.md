# Window-Blend Rollout Plan

**Status (2026-10-07):** Phase 0 = Option A chosen and executed for Week 5. Phase 1 DONE except 1d (downgraded, see below). Phases 2-5 not started.
**Goal:** every input the sim uses -- for every NFL player and team -- is built each week as
*season-long anchor + last four qualifying games (+ a per-field-class preseason anchor)*, and BOTH sim
consumers read that same blend: the DFS week sims and the rest-of-season / season-long sims.
**Decisions locked (Cam, 2026-10-07):** hand-edits are marked by a column on the week sheet; trench
composites are in scope; rookies are treated as veterans after 4 games with their rookie curve as the
"preseason" value; shadow-grade both blends for a few weeks.

---

## 0. How it fits together (the part that was confusing)

Think of it as a kitchen with three stations:

| Station | What it does | Where |
|---|---|---|
| **1. The blend** (the recipe) | Each week, turns real games into one input per player/team. Preseason numbers are only a fallback/anchor. | `scripts/roster_management/refresh_weekly_dna_v_0_1_0.py` |
| **2. Season-long tree** (the pantry) | The blend's output. Rest-of-season sims read this directly. | `data/current_rosters/{TEAM}_traits_2026.json` |
| **3. Week sheets -> DFS tree** (the plated dish) | Season-long tree + injury/OUT adjustments + hand edits, for the DFS week. DFS sims read this. | `data/overrides/2026/week_NN/*.csv` -> `data/current_rosters/dfs/` |

What has happened so far, in plain terms:
- The week-5 sims you ran used the **old** recipe (legacy blend, last-4 only), because we had not built the new one yet.
- The new recipe now exists and is behind a switch (`--blend-mode window`), but it is **off**.
- Separately, **station 3 has a flaw**: for about half the players it ignores the blend and serves the old
  preseason share from the sheet instead (Week 5: 387 of 780 `target_share`, 222 of 780 `carry_share`
  values; mean gap 0.046 / 0.081). So even with the new recipe switched on, the DFS sims would not see
  it for those players. Fixing that is Phase 1 and it is the thing that matters most.
- The season-long tree (station 2) is fine: re-running the legacy refresh reproduces it exactly.

**Rest-of-season sims:** `run_full_season_sim_2026.py` reads the season-long tree (`data/current_rosters`)
and uses ONE snapshot of inputs for every remaining game. That is inherent: week 9's game cannot use
weeks 5-8 of data that has not been played. When the refresh reruns after each week, the next
rest-of-season run picks up the newer window. (Optionally simulating weeks sequentially, feeding simulated
stats forward, is a larger separate project -- Phase 5, deferred.)

---

## Phase 0 -- Week 5 decision (time-critical: TB@DAL kicks off Thu 2026-10-08, 8:15pm ET)

Option A (recommended): do Phase 1 minimum (1a-1c) tonight, switch Week 5 to window mode, resim.
Pros: Week 5 uses the better inputs (backtest: ~4.7% lower input error; ~8.8% on efficiency stats).
Cons: rushed; normalization (1d) and shadow-grading (Phase 2) not yet in place.
Option B: leave Week 5 on legacy, build Phases 1-2 properly, switch for Week 6.
Pros: no rush, fully validated. Cons: one more week on the weaker blend.
Either way, **save the legacy week-5 outputs** so both can be graded after the games.

## Phase 1 -- Make the sim actually read the blend (blocker) -- DONE 2026-10-07 (1d report-only)

**Start:** now. **Stop:** DFS tree shares for an unedited player equal the live blend; team share sums are sane.

1. **1.0 Investigate (read-only):** does the engine renormalize shares, or use them as raw weights? What
   does it do when a team's `carry_share` sums to 1.30 or 0.90? (Today: target sums 0.98-1.09, carry 0.90-1.30.)
2. **1a Add `hand_edit` column** to `season_long/*.csv` and `week_NN/*.csv` (empty = follow the blend).
   *Migration:* a row whose share equals the player's frozen `preseason_projection` value is NOT an edit;
   one that differs is pre-marked `hand_edit=TRUE` and listed for Cam to confirm/clear.
3. **1b `apply_team_week_overrides`:** overlay a sheet value only when `hand_edit` is set or the row is an
   OUT-redistribution result; every other value follows the live blend.
4. **1c OUT redistribution on live values:** redistribute an OUT player's *live blended* share pro-rata to
   same-position active players (today it works on the sheet's preseason numbers).
5. **1d Per-team normalization:** after 1b/1c, scale non-edited shares so each team's target/carry shares
   hit the required total; hand-edited values stay fixed. Report any team where scaling moved a player >0.02.
6. **1e Ordering guard:** `apply_team_season_overrides` / `apply_preseason_overrides` must not overwrite
   blended fields after a refresh; refuse (or warn loudly) if run in the wrong order.
7. **Tests:** unflagged player == live blend; flagged player == sheet; OUT redistribution conserves the
   total; team sums == target; idempotent on rerun. **No sims run in this phase** beyond one smoke game.

## Phase 2 -- Shadow mode + validation

**Start:** Phase 1 done. **Stop:** 3 graded weeks AND the audit below passes, then window becomes default.

1. **Shadow runs:** every week run both blends; keep `data/eval/{year}/blend_shadow/week_N/{legacy,window}/`
   snapshots; grade both with the existing player-projection grader. Decision rule written before looking:
   window must not be worse than legacy on MAE and 80%-band coverage over the 3 weeks.
2. **Out-of-sample tuning:** extend the PBP cache to 2021-2023, tune anchors/min-volumes on 2021-2023,
   report on 2024-2025 untouched.
3. **League-aggregate audit:** run the full 64-game audit with window inputs (completion %, sacks/game,
   yards, snaps vs `historical_eda_metrics.json`). Per AGENTS.md: audit BEFORE any recalibration; engine
   constants (`SACK_PROB_CALIBRATION_MULT`, `OPEN_FIELD_CALIBRATION_OFFSET`) were tuned on legacy inputs.
4. **Sim-level check:** compare week-5+ DFS projection error (sim vs actual DK points) legacy vs window.

## Phase 3 -- Coverage: every player and team

Each item: backtest first (same harness), then wire, then test. Order by engine impact.
- **3a Trench composites** (`run_block_off_z`, `run_def_z`, `pass_block_off_z`, `pass_def_z` + raw inputs):
  they feed sacks and rush yards. Backtest window vs legacy; wire if better.
- **3b Rookies:** after 4 real games, treat as veterans with the rookie curve value as the preseason anchor;
  before that, current curve/taper. Backtest on 2022-2025 rookie classes (no prior-season proxy needed
  beyond the curve).
- **3c NGS fields** (`avg_time_to_throw_sec`, `avg_separation_yds`) and zone `catch_rate`/`cpoe`.
- **3d Traded/changed-team players:** use the team he played for in each week for team games and share
  denominators (today: one modal team per player).
- **3e Players with no `player_id`, no prior season, or no preseason value:** explicit documented fallback
  (position-average anchor) plus a report of who hit it.
- **3f Defense/special teams/kickers:** confirm what inputs they use; add if any are weekly-updatable.

## Phase 4 -- Hardening

- Legacy-parity regression test (legacy mode must equal a stored golden roster set), schema validator
  (`validate_rosters.py`) run after every refresh, fail on NaN / share outside [0,1] / team sums off.
  (Found: 11 live players carry NaN `adot`/`elusiveness` today.)
- Atomic writes (write all 32 files to a temp dir, validate, then swap) + a per-week snapshot of
  `data/current_rosters/` (it is gitignored, so there is currently no rollback).
- Provenance: each roster file records blend mode, parameter set, completed week, git commit.
- One orchestrated weekly command: refresh -> week sheets -> apply -> (optional) sims, with a checklist
  printout; replaces the manual A1..C3 steps.
- After Phase 2 passes: make `window` the default and delete the legacy path (keep it in git tags).

## Phase 5 -- Rest-of-season (deferred, discuss after Phase 3)

Today: one input snapshot for all remaining games. Later option: sequential season simulation that feeds
simulated stats forward week to week (`_precompute_matchup_tables` was built to be re-callable for this).
Needs its own plan and validation; not required for window mode to be correct.

---

## Risks / open items
- "Preseason" in all backtests = prior-season actuals, not Cam's hand-tuned inputs; shadow grading is the
  only way to measure the real thing.
- Teammate opportunity shifts (e.g. AJ Brown returning) stay manual via `hand_edit`; the refresh should
  print a review list (`usage_shift_flags`) each week.
- 3 seasons of backtest evidence is still modest; keep the legacy path until Phase 2 passes.

---

## Phase 1 outcome (2026-10-07)

- **1.0 finding:** the engine renormalizes target/carry shares per zone among ACTIVE players (`receivers_cache`/`rusher_cache`),
  so shares are relative sampling weights and team sums off 1.0 do not break sampling. The raw values are still model features,
  so **1d is report-only**: `build_week_overrides` prints teams outside 0.90-1.10 (today ~30 teams, mostly carry_share 1.1-1.4;
  MIN carry 1.39 is the worst -- backups/WRs with small designed-run shares add up).
- **1a DONE:** `hand_edit` column on all 32 season-long sheets (`add_hand_edit_column_v_0_1_0.py`): 74 of 812 rows pre-marked
  TRUE (differs from frozen preseason / no preseason value / hand-written note). Review list: `data/overrides/2026/_hand_edit_migration.md`.
- **1b/1c DONE:** `build_team_week_rows` fills non-hand-edit share columns from the live blend before OUT redistribution;
  `apply_team_week_overrides` is unchanged (it already overlays only what differs from live, which is now only redistribution + hand edits).
  Week 5 check: 780 players, 580 equal the live blend, 200 differ -- all explained (OUT/zeroed players, teammate redistribution, hand edits).
- **1e DONE (warning only):** `apply_team_season_overrides` prints a loud warning when roster files already carry real-data blends.
  `export_team_season_overrides` now writes the `hand_edit` column, but a reseed LOSES existing flags.
- **New finding -- QB carry_share:** in the engine it means DESIGNED runs only (scrambles come from `scramble_rate`); PBP carry share
  includes scrambles, so blending it double counts. The first window run gave QB rush att/g 3.98 vs real 3.03 (legacy DFS 2.94 was fine only
  because the sheet overlay forced QBs back to preseason). Window mode now keeps QB carry_share (flat + zones) at preseason: 3.10 att/g and
  16.1 yds vs real 3.03 / 16.7. **The legacy refresh has the same double count in the season-long tree (rest-of-season sims) -- not fixed here.**
  The backtest scored QB carry_share against all carries, so its carry_share numbers include that flaw for QBs.
- **Week 5 sims rerun** (10K x 15 games) on window inputs + live-following sheets. Legacy outputs saved in
  `data/eval/2026/blend_shadow/week_5/legacy/`, window in `.../window/` (local only, gitignored) for Phase 2 grading.
  Per team-game: pass yds 220 -> 228 (real 234.7), total points 43.98 -> 44.56.

## Update 2026-10-08

- **Legacy QB carry_share fixed** (both modes keep QB flat + zone carry_share at preseason).
- **New bug found + fixed in code: legacy refresh aliased `splits` onto `preseason_projection.splits`** (see AGENTS.md section 8). The data is NOT yet repaired:
  live `preseason_projection.splits` equals the current splits everywhere. Repair = `apply_team_season_overrides` (restores preseason from the season_long sheets,
  which still hold the true values) -> `refresh_weekly_dna ... --blend-mode window` -> build/apply week sheets -> resim. Open question before doing it: QB
  goal-line/red-zone carry_share. Real PBP contains real QB sneaks (designed), so the polluted values are not pure noise: current sim QB rush TD/g is 0.180 vs real 0.202
  (legacy sim 0.135); with true preseason zone values it will likely drop. Needs a small backtest or a hand decision (Phase 3 item).
- The `hand_edit` pre-marks that came from rz_/gl_ columns differing from `preseason_projection.splits` are partly contamination -- re-run
  `add_hand_edit_column_v_0_1_0.py` logic against the restored preseason before relying on the review list.

- **QB carry_share revised same day:** instead of pinning QBs to preseason, QB carry_share (flat + zones) is computed from real DESIGNED runs only (scrambles excluded via `qb_scramble`), in both modes. Live data not yet regenerated with this -- needs the refresh -> week sheets -> resim chain.

## Update 2026-10-07 (evening): zero preseason weight

- Cam's decision: from 4 games on, 2026 data only. `PRESEASON_ANCHOR` = 0 for shares/ratios/team; own-2026 fallback for sub-min-volume games; zero-fill for unearned shares;
  zone catch_rate/cpoe fall back to his flat 2026 value. Preseason remains only for < 4 games or no 2026 sample. Phase 2 should still grade legacy vs window (and, in the offseason,
  re-test an anchor using Cam's real preseason inputs -- the 2024/2025 backtest's "preseason" was prior-season ACTUALS).
- Observed effect on Week 5: league totals closer to real (45.7 vs real 46.2 pts/game), pass yds on target (228 vs 229), but pass TDs +21% vs real and individual QB outliers (Lamar 31.1 DK).
