# Clock Physics Audit — v0.2.0 → v0.3.0

**Status:** Task A complete. Task B (B1+B2+B3) implemented, final re-measurement in progress.

This audit exists because a manual review of the roadmap surfaced a suspicion that the simulator runs a couple of plays light per game. Investigating that turned up a larger, previously undocumented gap in how the engine models game-clock runoff, and a complete absence of team-level tempo/pace. This folder tracks that investigation end to end, target version bump `v0.2.0 → v0.3.0`.

---

## Task A — Snap & Penalty Tracking

**Goal:** Before touching the clock model, get a real number on the table for "are we actually missing plays," by tracking total snaps, offensive snaps, and penalties on both the sim side and the real-data side, using matched definitions.

### What was missing

- `docs/audit/v_0_2_0_audit/sim_post_cal_metrics.json` only tracked 3 metrics (sacks/game, pass yards/game, pass TDs/game) — no play-count metric existed in the sim-vs-real comparison at all, despite `total_plays` and `punts` already being computed per game in [`run_weeks_1_to_4_2025.py`](../../../scripts/simulation_runners/run_weeks_1_to_4_2025.py) and simply never carried into the comparison file.
- `game_engine.py` had no counter for accepted penalties at all, and no counter for total snaps including special teams (`self.play_count` only increments on pass/run scrimmage downs — see [`get_game_summaries()`](../../../src/nfl_sim/game_engine.py)).

### Bucket definitions (matched sim ↔ real)

| Metric | Sim source | Real source (`nfl_data_py`) |
|---|---|---|
| `offensive_snaps` | `self.play_count` (pass/run downs) | `play_type in ['pass', 'run']` |
| `special_teams_snaps` | `punts_run + fg_attempts_away/home + kickoffs_run` | `play_type in ['punt', 'field_goal', 'kickoff']` |
| `presnap_penalty_snaps` | `self.presnap_penalty_snaps` (dead-ball false start/encroachment — ends the play step before a down is run) | `play_type == 'no_play'` |
| `penalties_accepted` | `self.penalties_accepted` (presnap **+** mid-play accepted penalties, e.g. holding/DPI that still resolve a down) | `penalty == 1` (any accepted penalty, presnap or mid-play) |
| `total_snaps` | `offensive_snaps + special_teams_snaps + presnap_penalty_snaps` | same |

**Units note:** all of these are **whole-game, both-teams-combined** numbers, not per-team-game — because `game_engine.py`'s counters (`play_count`, `punts_run`, etc.) are single whole-game tallies that don't track which team is on offense. This differs from the existing `sacks_per_game`/`pass_yards_per_game`/`pass_tds_per_game` metrics in `sim_post_cal_metrics.json`, which are per-team-game (player-level stats are inherently team-specific, so those didn't need adjusting). Mixing the two unit conventions in a comparison would produce a spurious ~2x gap, so this is called out explicitly here and in code comments in both audit scripts.

**Known imprecision:** kickoff `posteam` attribution in `nflfastR`/`nfl_data_py` is inconsistent about whether it's the kicking or receiving team across seasons/formats. Since this audit sums both teams per game before averaging, that attribution ambiguity nets out and doesn't affect the whole-game total.

### Files changed

- [`src/nfl_sim/game_engine.py`](../../../src/nfl_sim/game_engine.py) — added `kickoffs_run`, `presnap_penalty_snaps`, `penalties_accepted` counters; exposed `offensive_snaps`, `special_teams_snaps`, `presnap_penalty_snaps`, `total_snaps`, `penalties_accepted` in `get_game_summaries()`.
- [`scripts/eda/run_historical_eda.py`](../../../scripts/eda/run_historical_eda.py) — added real 2021–2025 whole-game averages for the same 5 metrics, written to `historical_eda_metrics.json`.
- [`scripts/simulation_runners/run_weeks_1_to_4_2025.py`](../../../scripts/simulation_runners/run_weeks_1_to_4_2025.py) — carries the new sim-side metrics into `sim_post_cal_metrics.json`.

### Results

Source: `docs/audit/v_0_2_0_audit/sim_post_cal_metrics.json` (sim, 2025 weeks 1–4, 1000 iterations/game) vs. `historical_eda_metrics.json` (real, 2021–2025 regular season, 235,933 plays). Both whole-game, both-teams-combined averages.

| Metric | Sim | Real | Delta | Delta % |
|---|---:|---:|---:|---:|
| **Total snaps/game** | **153.15** | **154.79** | **−1.64** | **−1.1%** |
| Offensive snaps/game | 123.55 | 123.82 | −0.27 | −0.2% |
| Special teams snaps/game | 20.69 | 21.81 | −1.12 | −5.1% |
| Presnap penalty snaps/game | 8.91 | 9.16 | −0.25 | −2.7% |
| Penalties accepted/game | 11.26 | 12.00 | −0.74 | −6.2% |
| Sacks/game (context, pre-existing metric) | 1.54 | 2.41 | −0.87 | −36.1% |
| Pass yards/game (context, pre-existing metric) | 207.34 | 234.67 | −27.33 | −11.6% |

**Finding: the "missing a couple plays a game" hunch is confirmed and precisely sized — the sim runs 1.64 fewer total snaps/game than real football.** But the composition is the interesting part:

- **Offensive snaps are essentially correct** (−0.2%, well within noise for a 1000-iteration Monte Carlo sample). This directly contradicts the code-review prediction that the too-short baseline runoff (`randint(18,30)` vs. real median 38s) would pack in *extra* offensive plays — it isn't showing up net-net, which itself is worth understanding in Task B (something else may be offsetting it, e.g. drives ending in fewer overall possessions).
- **Special teams snaps carry ~68% of the gap** (−1.12 of the −1.64 total, a −5.1% shortfall) — this is the primary driver of the play-count miss, not the offensive clock model directly. This wasn't broken out into punts vs. field goals vs. kickoffs individually in this pass; that's the natural next diagnostic slice before Task B, since one of those three is likely under-firing (fewer kickoffs would mean fewer scores triggering them, fewer punts would mean an efficiency/4th-down-rate mismatch, etc.).
- Sacks and pass yards remain under-produced at roughly the same magnitude as the original 4-week `v_0_2_0_audit` found (sacks now −36%, pass yards −11.6%) — consistent with a known, still-unfixed gap, not new noise.

---

## Task B — Clock/Pace/Timeout Model Rebuild (consolidated scope, pending sign-off)

**Original findings that motivated this** (from code review):

1. **Baseline runoff is short.** [`game_engine.py:1701`](../../../src/nfl_sim/game_engine.py) draws `np.random.randint(18, 30)` (mean ~23.5s) for every non-hurry "normal" play, regardless of team or situation. The real running-clock cohort has mean 32.92s, median 38s, IQR 28–42s per [`docs/eda_outputs/clock/README.md`](../../eda_outputs/clock/README.md).
2. **Hurry-up logic is far narrower than reality.** [`game_engine.py:291`](../../../src/nfl_sim/game_engine.py): `is_hurry` only fires when `quarter == 4 AND time_remaining < 120 AND losing`.
3. **Leading teams bleeding the clock isn't modeled at all.**
4. **No team- or coach-level pace/tempo parameter exists anywhere in the engine or DNA dictionaries** (deferred — see below).

Investigating these turned up a wider set of gaps than originally scoped. Consolidated into three groups:

### B1 — Pace/Runoff Model (replaces the flat `randint(18,30)`)

**Approach:** empirical bootstrap — sample with replacement from the real historical pool of snap-to-snap times matching the current (quarter × score-margin) situation, not a fitted parametric curve. Matches the existing codebase pattern (rush yards' `residuals_pools`).

**Grid:** built and reviewed in [`pace_grid.md`](pace_grid.md) / [`pace_grid.csv`](pace_grid.csv) (source: [`analyze_clock_pace_grid.py`](../../../scripts/eda/analyze_clock_pace_grid.py), 2021–2025 real data, 117,027 running-clock snaps). Cells: Q1 (whole quarter, 3+ score tiers merged into 2-score — too rare/noisy to warrant their own bucket), Q2 (>4:00 / 4:00–2:00 / <2:00), Q3 (>10:00 / 10:00–5:00 / <5:00), Q4 (>10:00 / 10:00–5:00 / 5:00–2:00 / <2:00), each × 7 score-margin tiers (tied, leading/trailing 1-score, 2-score, 3+ score).

**Key findings from the grid:**
- **Q1–Q3: pace is flat (~37–40s median) regardless of score margin.** The original hypothesis that trailing by 2+ scores speeds up play even in Q3 did **not** hold up even after splitting Q3 into 5-minute windows — real teams don't change tempo for score reasons before Q4. Score-driven pace logic is Q4-only.
- **Q2 <2:00 is a separate, non-score-driven effect** — every margin tier collapses toward fast pace together (trailing 3+: 5.0s median, leading 3+: 15.0s median), because both teams want one more possession before halftime regardless of who's ahead. Distinct mechanic from Q4's endgame urgency; keep conceptually separate.
- **Q4 shows the real divergence**, starting clearly at the 10:00–5:00 window and intensifying through 5:00–2:00 and <2:00.
- **Late-Q4 leading/tied cells are contaminated by timeout stoppages**, visible in the spread not just the median (e.g. Q4 5:00–2:00 leading-1-score: median 42.0s but P25 only 8.0s — clearly bimodal). **Resolution:** at pool-construction time, override the leading/tied pools in the Q4 5:00–2:00 and <2:00 windows with the clean pool from an earlier, uncontaminated window (e.g. Q4 10:00–5:00 leading tier), rather than trust the raw noisy one. The apparent slowdown should emerge from the timeout logic (B3) being modeled separately, not from baking it into the pace pool.

### B2 — Clock-Stop Correctness Fixes

Separate from pace — these are rule-based bugs, not calibration issues. Live-play-duration component (0s for a touchback up to ~13–14s for a long return) varies per play; the clock then stops regardless of where the play ends, for these categories:

| Category | Status |
|---|---|
| Incompletions | ✓ Already correct — own `play_ttt`-based runoff, clock stops |
| Turnovers (INT/fumble) | ✓ Clock-stop already implemented and confirmed correct per rule ([source](https://blog.kokasports.com/when-does-the-clock-stop-in-nfl/)) — but uses a flat 10s constant; upgrade to a real variable return-time distribution |
| **Scoring plays (TD, made FG)** | **✗ Bug.** Not excluded from the normal running-clock path — currently gets treated like any other play instead of a short live-play duration + immediate stop |
| **Turnover on downs** | **✗ Bug.** Same issue — `turnover_down` switches possession but isn't excluded from `regular_clock`, so the clock doesn't stop |
| **Missed field goals** | **✗ Needs verification/build** — made-FG path exists (`fg_good_mask`); no confirmed branch yet for the miss case (possession switch to defense + clock stop) |
| Kickoffs | Handled, but flat 8s regardless of touchback vs. long return — needs the variable 0–14s treatment |
| Punts | Handled, but flat `randint(10,15)` — needs a real return-time distribution |

**No separate "play duration" double-counting risk found:** `play_ttt` (time-to-throw) is computed for every pass but currently only consumed in the incomplete-pass formula — it's not added anywhere in the paths B1 touches, so no removal needed there.

### B3 — Timeout Logic Widening

- **Trailing offense** (`is_hurry`'s own timeout-then-spike logic): no change — already correctly gated to `<2:00`, spikes only fire when timeouts are exhausted, matches real behavior (spikes never happen before 2:00, rarely before 1:00).
- **Trailing defense** (`timeout_def_mask`): currently gated to `<2:00` only — too narrow. Widen with two randomized-per-game strategies (50/50 split as a v1 default, no data to calibrate the real split yet):
  - **Early:** burns timeouts between 4:00 and 2:00 remaining
  - **Late:** holds until after the two-minute warning, burns them there

### Deferred — explicitly out of scope for this pass

- **Team/coach-specific pace tempo** (a real `coach_dna.json` field vs. the grid's league-wide pools) — after B1–B3 land.
- **Coach-specific timeout-calling tendency** (vs. the generic 50/50 split in B3) — alongside the pace/tempo work above.
- **Extra random stoppages** (injury timeouts, replay reviews) — audit after B1–B3 land and re-measure against Task A's methodology.
- **Explicit out-of-bounds modeling** (whether a given play ends in bounds or out) — the engine currently has zero modeling of this at all; it's a bigger, separate undertaking (requires modeling *where* a play ends relative to the sideline) and not part of this pass.

**Re-measurement plan:** once B1–B3 are implemented, re-run Task A's exact methodology (`run_historical_eda.py` / `run_weeks_1_to_4_2025.py` snap-count comparison) as a fresh "after" baseline — not to confirm it closes the specific −1.64 snaps/game gap (lengthening the runoff should, if anything, reduce offensive snap count slightly, not add plays back — the gap was concentrated in special teams, which B1–B3 don't touch), but to see where the numbers actually land now that the clock model is realistic.

---

## Implementation Notes (B1–B3, as actually built)

**Critical correction found during smoke-testing, not anticipated in scope:** the pace grid was originally built (per B1's "approach" above) using `analyze_clock_physics.py`'s "running clock" definition, which **excludes** plays where the previous snap went out of bounds. That's correct for that script's own descriptive purpose, but wrong for this grid — `game_engine.py`'s `is_normal_reg` bucket has **no out-of-bounds modeling at all**, so it applies whatever pool it's given to *every* qualifying play regardless of where it ends. Feeding it the OOB-excluded "clean running clock only" pool (mean ~38s) over-estimated runoff for every play and collapsed simulated offensive snaps from ~123/game (accurate) to ~89/game (a ~28% undershoot) in the first smoke test. Fixed by rebuilding the grid to include OOB-terminated plays (excluding only incomplete/timeout/penalty from the prior-play stop condition, matching exactly what `is_normal_reg` itself excludes) — this is now documented directly in [`analyze_clock_pace_grid.py`](../../../scripts/eda/analyze_clock_pace_grid.py)'s `prev_clock_stopped` definition. Grid grew from 117,027 to 134,079 snaps after the fix; pool means dropped only slightly (~38.0 → ~37.5), since OOB plays are a minority of the population — the fix mattered because it was being applied to *every* play, not because OOB plays are common.

**B1 shipped:**
- [`scripts/eda/analyze_clock_pace_grid.py`](../../../scripts/eda/analyze_clock_pace_grid.py) — builds the grid, persists raw per-cell pools to `src/nfl_sim/models/clock_pace_v_0_1_0/pace_pools.json` (75 cells, ~137k values, including the Q4-late leading/tied override).
- [`src/nfl_sim/models/clock_pace_v_0_1_0/inference.py`](../../../src/nfl_sim/models/clock_pace_v_0_1_0/inference.py) — `ClockPaceModelV010`, loads pools + metadata, exposes `get_pool(row_group, margin_tier)`.
- [`src/nfl_sim/model_registry.py`](../../../src/nfl_sim/model_registry.py) — wired in as `self.clock_pace_model`.
- [`src/nfl_sim/game_engine.py`](../../../src/nfl_sim/game_engine.py) — new `_sample_pace_runoff(mask)` vectorized helper (buckets lanes by quarter/time-window × score-margin, bootstrap-samples per unique cell present); replaces the old `randint(18,30)` call in the `is_normal_reg` branch. OT (quarter ≥ 5) reuses Q4's buckets as a reasonable fallback (no dedicated OT data).

**B2 shipped:**
- Hoisted `td_mask` and `turnover_down` to be always-defined (not just inside their originating `if` blocks) so Phase 7 can reference them for exclusion — same category of fix as Task A's `has_accepted_penalty` scoping issue.
- **Scoring plays** (offensive TD via normal play): now excluded from `regular_clock`, given `clip(3 + play_gain/6, 2, 14)` runoff (scales with how far the ball traveled) + immediate stop. Kickoff/punt/turnover-return TDs were already excluded correctly (handled within their own special-teams/turnover branches) — only the main scrimmage-play TD path had the bug.
- **Turnover on downs**: excluded from `regular_clock`, given `randint(4,8)` runoff (no return happens, so no yardage-scaled component) + stop.
- **Missed field goals**: turned out to already be correctly handled (`_switch_possession(scored=False, mask=fg_miss_mask)`) — no bug here, contrary to the original scope table. Made-FG runoff tightened from the shared `randint(5,9)` to `randint(4,6)` (4-5s) per the specific ask; missed FGs keep `randint(5,9)`.
- **Kickoffs**: runoff moved from a flat pre-outcome 8s to post-outcome, scaled by return yards: `randint(4,7)` base (touchback/minimal), `clip(4 + ret_yds/5, 4, 14)` for actual returns.
- **Punts**: same treatment — `randint(4,7)` base for touchback/fair-catch, yardage-scaled for normal returns, `randint(12,15)` for return TDs (long by definition).
- **Turnovers (INT/fumble)**: hoisted a new `turnover_return_yards` array (populated from the existing Gamma/Exponential return-yardage draws, which were already being computed but not fed into the clock); flat 10s replaced with `clip(4 + return_yards/5, 4, 14)`. Pick-six/scoop-and-score get a flat 50-yard-equivalent (`clip → 14`, the max) since those are long by definition.

**B3 shipped:**
- New per-game persistent flag `self.trailing_defense_early_strategy` (50/50 random at engine init, not re-rolled per play).
- `timeout_def_mask` widened: "Early" lanes now fire from 4:00 remaining in Q4 (previously only <2:00); "Late" lanes keep the original <2:00-only gate.
- Trailing-offense timeout/spike logic: unchanged, already correct.

**Post-hoc gap found and fixed:** neither punts nor field goals ever set `self.clock_stopped = True`, unlike every other clock-affecting category (kickoffs, incompletions, turnovers, scoring plays, turnover-on-downs, penalties all did). Since `clock_stopped` persists across plays and gates the spike/timeout-burn logic on the *next* play (`~self.clock_stopped` at the top of `simulate_play_step`), this left it stale for the receiving team's first snap — could wrongly let a team think the clock was still running (and burn a spike/timeout it didn't need) right after a punt or FG attempt. Fixed: both now set `clock_stopped=True` for their full mask (made or missed FG; touchback, fair catch, or return on a punt — the exchange itself always stops the clock regardless of outcome).

**Full clock-stop verification (per explicit request), current state:**

| Category | Stops clock? | Mechanism |
|---|---|---|
| Incompletions | ✓ | `inc_mask`, pre-existing, unchanged |
| Turnovers (INT/fumble, incl. pick-six/scoop-and-score) | ✓ | `turnover_clock` in Phase 7, covers TD and non-TD outcomes alike |
| Turnover on downs | ✓ | `turnover_down_clock` (B2 fix) |
| Scoring plays — offensive TD (pass/run/scramble) | ✓ | `scoring_play_clock` (B2 fix) |
| Scoring plays — made field goal | ✓ | `fg_mask` (fixed above — was missing) |
| Scoring plays — kickoff-return TD | ✓ | `ko_mask` (set unconditionally at top of kickoff handling, before the return outcome is even known) |
| Scoring plays — punt-return TD | ✓ | `punt_mask` (fixed above — was missing) |
| Special teams — kickoffs (all outcomes) | ✓ | `ko_mask` |
| Special teams — punts (all outcomes) | ✓ | `punt_mask` (fixed above — was missing) |
| Special teams — field goals (made or missed) | ✓ | `fg_mask` (fixed above — was missing) |

**Verification:** all 18 existing tests in `tests/test_positional_evaluator.py` pass unchanged. Syntax-checked and smoke-tested after every sub-step (20-iteration batches) before proceeding to the next piece.

**Re-measured after the punt/FG `clock_stopped` fix:**

| Metric | Post-B1+B2+B3 | Post-punt/FG-fix | Real |
|---|---:|---:|---:|
| Total snaps/game | 119.40 | 119.47 | 154.79 |
| Offensive snaps/game | 96.16 | 96.17 | 123.82 |
| Special teams snaps/game | 16.34 | 16.39 | 21.81 |

**Barely moved — and that's expected, not a sign the fix didn't work.** The stale `clock_stopped` bug only mattered in a narrow situation: a punt or FG attempt happens, *and* the very next play also happens to be a late-Q4 hurry-up situation for the team now on offense. That's a small fraction of all punts/FGs across a season, so the aggregate effect is close to unmeasurable at this scale — it's a real correctness fix, just not a lever on total play count. The bigger levers (OOB modeling, injury stoppages, other administrative stops) are still ahead and explicitly not yet implemented.

---

## Major Bug Found: The Engine Never Simulated a 4th Quarter

Found via a manual play-by-play review (Cam's request, see [`sim_check/`](sim_check/) and [`scripts/print_play_by_play_v020.py`](../../../scripts/print_play_by_play_v020.py)) — while eyeballing a full game log, the sim ended abruptly right at the Q3→Q4 transition, before any Q4 plays ran.

**Root cause**, in [`_run_clock`](../../../src/nfl_sim/game_engine.py):
```python
next_qtr = end_qtr & (self.quarter < 4)
self.quarter[next_qtr] += 1          # mutates self.quarter in place
...
end_game = end_qtr & (self.quarter >= 4)   # BUG: reads the just-mutated value
self.game_over[end_game] = True
```
Any lane transitioning Q3→Q4 gets bumped to `quarter == 4` by the increment, then immediately satisfies `quarter >= 4` on the very next line and gets marked `game_over` — the instant Q4 *begins*, not when it ends. **This predates all of today's clock_physics_v020 work** (this method's control flow was never touched by B1-B3) — it's a pre-existing bug, likely present since this vectorized engine was first written.

**Fix:** capture `pre_quarter = self.quarter.copy()` before any mutation, and use that for both the quarter-increment check and the game-over check, so `end_game` only fires for lanes truly already in Q4 whose clock just expired.

**Impact, single-game spot check (KC @ BUF 2025):** play-step calls 130 → 163, offensive snaps (`play_count`) 107 → 126, after the fix.

**This is almost certainly the dominant explanation for Task #10's −22.3% offensive-snap shortfall** — skipping an entire quarter (25% of the game) lines up closely with the measured gap. Full batch re-run in progress to confirm at scale; results below once complete.

### Final Results (post Q4 game_over fix)

| Metric | Post-B1+B2+B3 | Post-Q4-fix | Real | Post-Q4-fix Delta % |
|---|---:|---:|---:|---:|
| Total snaps/game | 119.40 (−22.9%) | **159.07** | 154.79 | **+2.8%** |
| Offensive snaps/game | 96.16 (−22.3%) | **128.68** | 123.82 | **+3.9%** |
| Special teams snaps/game | 16.34 (−25.1%) | **21.07** | 21.81 | **−3.4%** |
| Presnap penalty snaps/game | 6.91 (−24.6%) | **9.32** | 9.16 | **+1.7%** |
| Penalties accepted/game | 8.74 (−27.2%) | **11.78** | 12.00 | **−1.8%** |
| Sacks/game | 1.20 (−50.2%) | 1.63 | 2.41 | −32.4% |
| Pass yards/game | 164.09 (−30.1%) | 221.11 | 234.67 | −5.8% |

**Every snap-count metric is now within ~4% of real** — a huge swing from the −22 to −27% range. Total snaps now runs slightly *over* real (+2.8%) rather than under, which makes sense: the still-missing pieces (OOB modeling, injury timeouts, two-minute warning, other administrative stoppages) would all *preserve* clock and pull this back down toward exact — so a small overshoot right now is the expected state given what's still left to build, not a new problem.

Sacks and pass yards remain the real outstanding gaps (−32.4% and −5.8%), but pass yards actually improved *past* the original pre-Task-B baseline (−11.6%) — better than where this whole investigation started. Sacks are a separate, already-known model-accuracy issue (the chaos-gate sack model under-firing, flagged back in the original engine inventory), not a clock-physics problem — worth its own pass, not part of this thread.

---

## Round 2: Out-of-Bounds, Two-Minute Warning, and Squeeze Plays

Triggered by Cam rewatching games and noticing (a) Bills-specific play counts looked light — traced to real league-wide OOB/administrative time not being modeled, not a Bills-specific issue, and (b) short (3-5s) stoppages on OOB plays that only became full stops inside the last 2:00 of Q2 / 5:00 of Q4.

### OOB heuristic (not a trained model — real-rate-based)

Real 2021-2025 rates: **completed pass 20.86%, run 7.45%** go out of bounds (`docs/eda_outputs` diagnostic). Implementation, per-play:
1. Roll OOB using those rates, only for non-scoring run/completed-pass plays (incomplete passes have their own clock treatment already).
2. **Outside the crunch window:** draw from the same pace pool as a normal play, then subtract a random 3-5s (the brief ball-spot freeze) — clock keeps running (`clock_stopped=False`), same as any continuing-drive play. This matches Cam's field observation almost exactly: OOB-only transitions measure ~4.3s faster than clean running-clock plays in the real data.
3. **Inside the crunch window** (last 2:00 of Q2 / last 5:00 of Q4, confirmed via web search earlier): full stop — runoff collapses to just the live-play action time (3-7s), `clock_stopped=True`.

**Consequence:** the general pace pool had to go back to *excluding* OOB-terminated plays (reverted from the Round-1 fix — see `analyze_clock_pace_grid.py`), since OOB is now its own explicit mechanism. Blending it into the general pool again would double-count the same timing effect.

### Two-minute warning (automatic, free stoppage)

Added `self.two_minute_warning_used` (per-lane, resets each quarter) and clamping logic in `_run_clock`: the first time a runoff would carry `time_remaining` from above 120 to at/below 120 during Q2 or Q4, the decrement is clamped so time lands exactly at 2:00, `clock_stopped` is forced `True`, and no team's timeout count is touched. Reset alongside the quarter-transition logic already fixed in Round 1.

**Bug caught and fixed during verification:** two call sites (`regular_clock` and OOB's `normal_oob` branch) unconditionally set `clock_stopped = False` *after* calling `_run_clock` — which stomped on the warning's `True` if that same call happened to trigger the clamp. Fixed by moving those resets to *before* the `_run_clock` call in both places, confirmed via the play-by-play log (`Play 80 | Q2 02:05 -> Q2 02:00 ... clock_stopped(after)=True`).

### Squeeze plays (rushing the snap to steal an extra play before the warning)

Feasibility requires real buffer above 2:00 — `time_remaining >= 135` if the clock is currently running (need time to line up and snap), or just `> 120` if it's already stopped (incomplete/OOB/etc. — no ticking clock to race). Probabilities (Cam's numbers):

| Situation | First squeeze | Second squeeze |
|---|---:|---:|
| Q2 (any score situation) | 95% | — |
| Q4, leading | 0% | — |
| Q4, trailing 2+ scores | 100% | 50% |
| Q4, tied / trailing 1 score | 30% baseline, scaled up by field position and low timeouts (capped 65%) | — |

Attempting lanes get a fast `randint(8,13)` runoff instead of the normal pace draw; `self.squeeze_plays_used` tracks how many a lane has used, gating the second-squeeze roll to trailing-big situations only.

### Verification

All 18 tests pass. Syntax-checked and smoke-tested after each piece.

### Final Results (Round 2)

| Metric | Post-Q4-fix (Round 1) | Post-OOB/2min/squeeze (Round 2) | Real | Round 2 Delta % |
|---|---:|---:|---:|---:|
| Total snaps/game | 159.07 (+2.8%) | **166.57** | 154.79 | **+7.6%** |
| Offensive snaps/game | 128.68 (+3.9%) | **134.88** | 123.82 | **+8.9%** |
| Special teams snaps/game | 21.07 (−3.4%) | **21.95** | 21.81 | **+0.6%** |
| Presnap penalty snaps/game | 9.32 (+1.7%) | **9.74** | 9.16 | +6.3% |
| Penalties accepted/game | 11.78 (−1.8%) | **12.31** | 12.00 | +2.6% |
| Sacks/game | 1.63 (−32.4%) | 1.72 | 2.41 | −28.6% |
| Pass yards/game | 221.11 (−5.8%) | **231.82** | 234.67 | **−1.2%** |

**Pass yards and special teams snaps are now excellent** (−1.2% and +0.6% respectively). Sacks improved modestly. **But total/offensive snap overshoot grew, not shrank** (+2.8%→+7.6% total, +3.9%→+8.9% offensive) — worth being direct about, since the pre-round framing was "still-missing stoppages should pull this down toward real."

**Why, mechanically:** two of the three new pieces actively *add* plays rather than remove them. The two-minute-warning clamp prevents a single play's runoff from "wasting" time by overshooting past 2:00 in one big chunk — it now stops exactly at 2:00 instead, preserving whatever buffer remains. The squeeze-play mechanic goes further and *deliberately* forces extra fast plays into that buffer window on purpose (per Cam's spec — 95%/100%/50%/30% attempt rates). Both push total plays up. OOB itself is closer to neutral (it shifts ~13.5% of plays to be a few seconds faster, but pulling OOB out of the general pool's blend largely offsets that). Net effect: the two plate-preserving/adding mechanics outweighed OOB's modest downward pressure.

**This means the still-deferred pieces (injury stoppages, other administrative stops) now have more work to do than they would have from a near-exact starting point** — they're the pieces that actually *remove* time without adding a play, so they're what should pull this back down from +7.6%/+8.9% toward exact. Not a sign anything here is wrong — the squeeze/warning mechanics are working exactly as specified — just a reminder that "every piece individually correct" doesn't automatically net to "aggregate exact" until all the pieces (including the deferred ones) are in.

---

## Round 3: +1s Global Pace Calibration

Per Cam: the Round 2 overshoot (~67 offensive plays/team vs. real ~62) wasn't catastrophic but wasn't ideal either. Rather than waiting for the deferred injury/admin-stoppage work (v0.4.0) to pull it back down, added a flat +1 second to `_sample_pace_runoff`'s output — the general "time between plays" draw used by both normal plays and (upstream) OOB's reduction. Sacks deferred to v0.4.0 alongside injury/administrative stoppages, per Cam's call — no changes this round.

### Final Results (Round 3)

| Metric | Round 2 | Round 3 (+1s) | Real | Round 3 Delta % |
|---|---:|---:|---:|---:|
| Total snaps/game | 166.57 (+7.6%) | **162.06** | 154.79 | **+4.7%** |
| Offensive snaps/game | 134.88 (+8.9%) | **131.16** | 123.82 | **+5.9%** |
| Special teams snaps/game | 21.95 (+0.6%) | **21.39** | 21.81 | −1.9% |
| Presnap penalty snaps/game | 9.74 (+6.3%) | **9.51** | 9.16 | +3.8% |
| Penalties accepted/game | 12.31 (+2.6%) | **12.02** | 12.00 | **+0.2%** |
| Sacks/game | 1.72 (−28.6%) | 1.68 | 2.41 | −30.3% (deferred to v0.4.0, no change intended) |
| Pass yards/game | 231.82 (−1.2%) | 225.56 | 234.67 | −3.9% |

Offensive/total snap overshoot roughly cut in half (+8.9%→+5.9%, +7.6%→+4.7%). Penalties accepted is now essentially exact (+0.2%). Pass yards dropped from −1.2% to −3.9% as an expected side effect of fewer total plays — still well within the range Cam considers acceptable given the real league-wide downward trend in QB play. Sacks unchanged (deferred).

---

## Round 4: Situational OOB Rates + Second Pace Nudge

Still ~8 plays/game (4/team) high after Round 3. Two changes:

**Situational OOB rates**, replacing the flat 7.45%(run)/20.86%(pass) baseline in specific windows with real 2021-2025 blended rates (query: score-margin x time-window, same methodology as the pace grid):

| Situation | Real OOB rate | vs. overall baseline (13.6%) |
|---|---:|---|
| Leading, Q4 ≤5:00 | 4.3% | much lower — protecting the lead, not literally never |
| Trailing, Q4 ≤5:00 | 17.7% | elevated, but defense clearly caps it well below "always" |
| Tied, Q4 ≤1:00, yardline_100 >50 | 31.3% (N=115, small sample) | high, matches trailing-like urgency |
| Tied, Q4 >1:00 (not urgent) | *(no override — matched baseline within noise)* | 13.0% ≈ 13.6% baseline |
| Q2 ≤2:00, leading | 22.1% | elevated |
| Q2 ≤2:00, trailing | 20.4% | elevated |
| Q2 ≤2:00, tied | 18.1% | elevated |

Notably, tied-and-not-urgent in Q4 measured right at the overall baseline — the original intuition that these teams "stay in bounds ~90% of the time" didn't hold up against real data, so no override was added there; it just uses the same flat play-type rate as everywhere else.

**Second pace nudge:** `_sample_pace_runoff` now adds +2s cumulative (was +1s in Round 3) — still 8 plays/game high after the first nudge only recovered ~4.

Sacks and injury/administrative stoppages remain deferred to v0.4.0.

### Verification

Syntax check, full 18-test suite, and 20-iteration smoke test all pass before the full batch re-run.

### Final Results (Round 4)

| Metric | Round 3 (+1s) | Round 4 (situational OOB + 2nd nudge) | Real | Round 4 Delta % |
|---|---:|---:|---:|---:|
| Total snaps/game | 162.06 (+4.7%) | **157.97** | 154.79 | **+2.1%** |
| Offensive snaps/game | 131.16 (+5.9%) | **127.84** | 123.82 | **+3.2%** |
| Special teams snaps/game | 21.39 (−1.9%) | 20.88 | 21.81 | −4.3% |
| Presnap penalty snaps/game | 9.51 (+3.8%) | **9.26** | 9.16 | +1.1% |
| Penalties accepted/game | 12.02 (+0.2%) | 11.69 | 12.00 | −2.6% |
| Sacks/game | 1.68 (−30.3%) | 1.62 | 2.41 | −32.7% (deferred, no change intended) |
| Pass yards/game | 225.56 (−3.9%) | 219.88 | 234.67 | −6.3% (expected, fewer total plays) |

**Offensive/total snap overshoot cut roughly in half again** (+5.9%→+3.2%, +4.7%→+2.1%). Every snap-count metric is now within ~4% of real, and the two headline numbers (total and offensive snaps) are within ~2-3%. Special teams and penalties-accepted drifted slightly further from exact (−4.3%, −2.6%) but both remain small, plausibly just noise from a single 1000-iteration/64-game sample rather than a systematic issue.

**Recommendation (made autonomously, Cam stepped away): hold here rather than tuning further.** At +2.1%/+3.2% on the primary snap-count metrics, further nudges would mostly be chasing sample noise rather than a known remaining bug — the productive next steps are the already-identified ones (v0.4.0: sacks trench-tier retrain, injury stoppages, other administrative stops), not more blind pace adjustments. Will flag this recommendation to Cam directly when he's back rather than pushing another round unprompted.

---

## Round 5: Diagnosing the Remaining Overshoot — Drives vs. Plays-per-Drive

Cam did the season-long math on the Round 4 overshoot (~4 offensive plays/game ≈ 2/team ≈ 34 extra plays/team over a 17-game season — "nearly an extra half of football") and pushed back on doing a third blanket pace nudge: what other levers exist, and is the OOB reduction (3-5s) even real?

### OOB reduction: confirmed real, not a confound

Re-ran the OOB-vs-clean comparison **controlling for play type** (the original Round 4 numbers didn't split run vs. pass, leaving open the possibility the gap was a play-type-mix artifact rather than a real rule effect):

| Play type | OOB mean | Clean mean | Gap |
|---|---:|---:|---:|
| Run | 31.82s | 34.00s | **2.18s** |
| Completed pass | 26.98s | 31.21s | **4.24s** |

The effect holds up within each play type separately — not a confound. But the flat `randint(3,6)` reduction overstated the run case (real gap ~2.18s) while roughly matching the pass case (~4.24s). Refined to be play-type-specific: `randint(1,4)` for runs, `randint(3,6)` for completed passes. This is a precision improvement, not a play-count lever — the weighted effect on the OOB population is only ~0.3-0.4s on average, since runs are the minority of OOB events.

### Drives vs. plays-per-drive: found where the overshoot actually lives

Added a diagnostic `drives_count` (increments on every kickoff and every non-scoring possession change) and compared against real drive-level data:

| Metric | Sim | Real | Delta |
|---|---:|---:|---:|
| Drives/game (both teams) | 21.31 | 21.70 | **−1.8%** |
| Plays per drive | 5.98 | 5.71 | **+4.7%** |

**Drive count is essentially correct — the overshoot is concentrated entirely in plays-per-drive.** This rules out down-conversion-rate or turnover-rate issues (those would show up as too many or too few drives) and points at something that extends existing drives without ending them. The squeeze-play mechanic fits that signature exactly — by design, it adds an extra down within a drive near the two-minute marks without ending it. Rough math checks out: 0.27 extra plays/drive × ~21 drives/game ≈ 5-6 extra combined plays/game, consistent with the ~4 offensive plays/game gap.

**Decision point, not yet acted on:** the 50% second-squeeze probability (trailing 2+ scores) was Cam's explicit, deliberate call, not an unspecified implementation detail — didn't feel right to override it autonomously even with this evidence pointing at it. Flagging for Cam: the data suggests dialing back the *second* squeeze specifically (leaving the first-squeeze probabilities at 95%/100%/30-65% untouched) is the best-supported next lever, rather than another blanket pace nudge affecting every play regardless of situation.

---

## Round 6: Completion Rate Investigation — Screen Catch Rate Fix

Cam noticed sim yards/completion running ~11% short (9.70 vs. real 10.93) and asked what levers exist. Investigation (not clock-physics, but surfaced from the same audit thread) found the sim's overall completion rate is **71.47% vs. real 60.18%** — a much bigger issue than the YAC magnitude question that started it.

**Ruled out:** zone distribution (screen/standard/deep % of pass attempts) is essentially a perfect match to real (21.3%/66.5%/12.2% sim vs 21.9%/66.6%/11.5% real) — depth-of-target selection isn't the problem.

**Found:** completion rate *by* zone tells the real story:

| Zone | Sim completion% | Real completion% | Delta |
|---|---:|---:|---:|
| Deep | 35.67% | 36.00% | essentially exact |
| Standard | 73.14% | 65.90% | +7.24pp (logit catch model — deferred, Cam's call) |
| Screen | 92.55% | 76.95% | **+15.6pp — fixed this round** |

Screen catch rate by receiver position, real vs. the sim's hardcoded constants:

| | Sim (old) | Real | Sim (new) |
|---|---:|---:|---:|
| RB screens | 80% | 82.68% | 83% |
| WR/TE screens | **95%** | **80.01%** | **80%** |

The WR/TE constant was the dominant driver of the entire completion-rate overshoot. Fixed in `game_engine.py`'s screen catch-probability block.

**Also investigated: throwaways.** Current mechanism only fires as a 20% escape from would-be-sacks. Real-data EDA to quantify this properly hit a wall — nflfastR's play descriptions only inconsistently flag throwaways in text ("thrown away"), undercounting badly (e.g. 32 of 5,724 incomplete passes in 2024 flagged, but clearly many more short incompletions to real targets exist that aren't throwaways either — no reliable numeric heuristic separates the two). Of the text-confirmed throwaways, 78% had pressure charted / 22% didn't, though this is likely biased toward over-representing pressure (a scrambling QB avoiding a sack is more "newsworthy" to annotate than a clean-pocket checkdown-nothing-there throwaway). **Deferred per Cam's call** ("revisit when we come back") — target parameters for that future work: ~5% of all dropbacks should be throwaways, split 75% pressure-related / 25% clean-pocket. Logged as task #20.

### Final Results (Round 6)

| Metric | Round 4 | Round 6 (screen fix) | Real | Delta % |
|---|---:|---:|---:|---:|
| Total snaps/game | 157.97 | 159.90 | 154.79 | +3.3% |
| Offensive snaps/game | 127.84 | 129.31 | 123.82 | +4.4% |
| Pass yards/game | 219.88 | **219.81** | 234.67 | −6.3% (unchanged — expected, screens are low-yardage completions) |
| Sacks/game | 1.62 | 1.65 | 2.41 | −31.5% (deferred to v0.4.0) |
| Completion rate | 71.47% (pre-fix) | **69.64%** (quick-check estimate) | 60.18% | still elevated — remainder lives in the deferred standard-zone logit model |

Pass yards essentially unchanged, as expected — screens are inherently low-yardage plays, so removing some of them from the completion count doesn't move total yardage much. Completion rate improved but remains above real, since the larger standard-zone logit overshoot (+7.24pp) is intentionally untouched per Cam's preference to leave that model alone for now.

---

## Round 7: Completion Model — CPOE Application Bug + Baseline Recalibration

Cam pulled real 2025 season QB stats and confirmed the league completion rate target precisely: **64.40%** (weighted CMP/ATT across 50 QBs — matches Cam's eyeball of "63-64%" exactly). Asked to make the standard-zone overshoot (+7.24pp) driven by QB skill (DNA) rather than a flat correction. Two separate, independently-backtrackable fixes, per Cam's request to document each clearly in case either needs reverting.

### Fix 1: CPOE was already wired in, but compressed by a logit-space bug

The open-field catch-probability path (`is_open`, the majority of all throws) computed:
```python
delta_qb = qb_cpoe / 100.0
logit_p = b0 + b1*ay_val + delta_wr + delta_qb + sep_bonus
probs_normal[is_open] = sigmoid(logit_p)
```
CPOE (completion percentage over expected) is itself a **probability-space** quantity — by definition, a QB with +3.5 CPOE completes 3.5 percentage points more often than expected. Adding it *inside* the logit sum before the sigmoid compresses it through the sigmoid's nonlinearity. The contested-catch path (low separation) already did this correctly (`probs = contested_wr_rate + qb_cpoe/100`, applied directly in probability space) — only the open-field path had the bug.

**Fix:** moved the CPOE term to be added in probability space, after the sigmoid, matching the already-correct contested-catch path:
```python
logit_p = b0 + b1*ay_val + delta_wr + sep_bonus  # CPOE removed from here
probs_normal[is_open] = sigmoid(logit_p) + (qb_cpoe / 100.0)  # added here instead
```

**Verified directly** (isolated math check, ay_val=8, average separation/receiver):

| QB | OLD (logit-space) | NEW (probability-space) |
|---|---:|---:|
| Anthony Richardson (−10.08 CPOE) | 68.12% | **60.19%** |
| League average (0 CPOE) | 70.27% | 70.27% (unchanged, as expected) |
| Geno Smith (+4.89 CPOE) | 71.28% | **75.16%** |

Spread between worst and best QB went from 3.16pp (old) to 14.97pp (new) — the real CPOE gap between these two QBs is ~15 points, so the fix now tracks reality almost exactly instead of barely differentiating QBs at all.

**Backtrack instructions** (also left in code comments): revert `probs_normal[is_open] = sigmoid(logit_p) + qb_cpoe_val` and the `logit_p` line to include `delta_qb` inside the sum again, restoring the pre-fix logit-space behavior.

### Fix 2: Baseline recalibration

**Important discovery while sizing this:** `b0` mathematically cancels out of the open-field formula entirely. `delta_wr` is constructed as `logit(zone_baseline) - logit(sigmoid(b0 + b1*adot_val))`, and since `logit(sigmoid(x)) = x`, this reduces to `logit(zone_baseline) - b0 - b1*adot_val` — so when added back into `logit_p = b0 + b1*ay_val + delta_wr + sep_bonus`, the `b0` terms cancel completely. Shifting `b0` would have been a no-op. Caught this before writing any code by working through the algebra, not by trial and error.

**Actual fix:** added a new, explicitly-labeled, independently-backtrackable calibration constant `OPEN_FIELD_CALIBRATION_OFFSET = 0.075`, subtracted directly in probability space alongside the CPOE term:
```python
probs_normal[is_open] = sigmoid(logit_p) + qb_cpoe_val - OPEN_FIELD_CALIBRATION_OFFSET
```
Sized to the measured standard-zone overshoot (+7.24pp, from the original zone-by-zone breakdown) — deep and screen zones were already accurate, so the whole population-wide gap concentrates in this one path.

**Backtrack:** delete the `- OPEN_FIELD_CALIBRATION_OFFSET` term (also noted in code comments).

**Result: landed almost exactly on target.** Population-wide completion rate: **64.15%** vs. the confirmed real 2025 target of 64.40% — a 0.25pp miss on the first attempt.

### Final Results (Round 7)

| Metric | Round 6 | Round 7 (CPOE fix + baseline) | Real | Delta % |
|---|---:|---:|---:|---:|
| Completion rate | 69.64% (est.) | **64.15%** | 64.40% | **−0.25pp — essentially exact** |
| Pass yards/game | 219.81 | 204.15 | 234.67 | −13.0% (down further — fewer completions, expected) |
| Offensive snaps/game | 129.31 | 132.28 | 123.82 | **+6.8%** (up from +4.4%) |
| Total snaps/game | 159.90 | 164.36 | 154.79 | **+6.2%** (up from +3.3%) |
| Special teams snaps/game | 21.22 | 22.43 | 21.81 | +2.9% (was −2.7%) |
| Sacks/game | 1.65 | 1.71 | 2.41 | −29.0% (deferred, roughly unchanged) |

**Side effect worth being direct about: play-count overshoot got worse again** (+4.4%→+6.8% offensive, +3.3%→+6.2% total), moving back toward where it was a few rounds ago. Mechanism: fewer completions means more incomplete passes and stalled drives, which need more plays to reach the same eventual outcome (score/punt/turnover) — so a real, necessary passing-model fix has a genuine knock-on effect on the clock-physics play-count work from Rounds 1-6. This isn't a bug in either fix — completion rate is now correct and pass yards moved in the expected direction — but it does mean the still-open **second-squeeze decision from Round 5** (and possibly the pace nudges generally) are worth revisiting now that the passing model is more accurate, rather than treating Round 4-6's play-count tuning as final.

### Final Results

Same methodology as Task A: `sim_post_cal_metrics.json` (2025 weeks 1–4, 1000 iterations/game, post B1+B2+B3) vs. `historical_eda_metrics.json` (real 2021–2025, unchanged from Task A).

| Metric | Sim (pre-Task-B) | Sim (post-B1+B2+B3) | Real | Post-B Delta % |
|---|---:|---:|---:|---:|
| **Total snaps/game** | 153.15 | **119.40** | 154.79 | **−22.9%** |
| **Offensive snaps/game** | 123.55 | **96.16** | 123.82 | **−22.3%** |
| Special teams snaps/game | 20.69 | 16.34 | 21.81 | −25.1% |
| Presnap penalty snaps/game | 8.91 | 6.91 | 9.16 | −24.6% |
| Penalties accepted/game | 11.26 | 8.74 | 12.00 | −27.2% |
| Sacks/game | 1.54 | 1.20 | 2.41 | −50.2% |
| Pass yards/game | 207.34 | 164.09 | 234.67 | −30.1% |

**Headline finding, stated plainly: every metric got worse, not better.** Offensive snap accuracy went from −0.2% (essentially exact, pre-Task-B) to −22.3% (post-Task-B). This is a real, substantial regression on the metric Task A set out to fix, despite B1's pace pools, B2's clock-stop fixes, and B3's timeout widening each being individually correct and verified against real NFL rules and real 2021–2025 data.

**Why, mechanically:** `is_normal_reg` is the majority of all plays (~75–80% of offensive snaps, by real play-type proportions — 60.2% completion rate + ~42% run rate, minus the small scoring/turnover-on-downs share). Its average runoff moved from ~23.5s (the old, factually wrong flat draw) to ~37–38s (the real, correct running-clock-inclusive-of-OOB average) — a ~60% increase concentrated in the bucket that dominates the game's clock budget. That alone mechanically shrinks total play count by a large amount; B2/B3 clawed back some of it (89→96 across incremental smoke tests) but nowhere near enough to fully offset B1's effect.

**On the OOB question — checked directly, resolved, and the grid is correct.** My initial expectation was that including out-of-bounds-terminated plays should pull the pool average down substantially, since OOB stops the clock. Directly measuring it: OOB-only transitions (prev play went OOB, nothing else) have mean 28.39s / median 34.0s — nowhere near the near-zero I assumed. Reason: the clock only holds until the *snap* on an OOB play inside the last 2:00 of Q2 / 5:00 of Q4 (confirmed earlier via web search); outside those windows it resumes on the official's ready-for-play signal well before the snap, so most OOB transitions (which mostly happen outside crunch time) behave close to a normal running-clock play timing-wise. At their real ~12.7% share, blending OOB transitions in only modestly lowers the pool mean — exactly the small move actually observed (38.0 → 37.5). **My assumption was wrong; the grid construction was right all along.**

**What this actually means:** the −22% play-count regression is the correct, fully-explained consequence of fixing a real, substantial, previously undocumented error — the old flat `randint(18,30)` (mean 23.5s) was simply wrong for the dominant play-type bucket, which really does run ~32–38s in reality. The old model's near-perfect total-snap accuracy (−0.2%) was a coincidence: some *other* part of the engine must be under-modeling stoppage/dead-ball time in a way that was compensating for the too-fast pace draw, and that compensating error is now exposed on its own now that pace is correct. **None of B1–B3 should be reverted** — each is independently verified against real NFL rules and real 2021–2025 data. The real next question is what that other compensating gap is — most likely candidate is the "extra random stoppages" item already flagged as deferred in this doc (injury timeouts, replay reviews, and other real administrative stoppages the engine doesn't model at all), which would need to *shrink* total plays further in reality but might be masking a different issue in the sim if the sim's non-pace mechanics (completion rate, turnover rate, drive length) don't match real proportions closely enough. Worth a dedicated look before or alongside the deferred pace/tempo DNA work.

---

## Round 8: Yards-Per-Completion Diagnostic — YAC Is the Root Cause, Not Air Yards

Cam re-raised the yards/completion gap that kicked off Round 6 in the first place (9.70 vs. real 10.93, ~−11%) — this had only ever been *diagnosed* as secondary to the completion-rate bug, never actually fixed. Now that completion rate is essentially exact (Round 7), this is the last major unexplained gap, and Cam's hypothesis was that fixing it might also help the still-open snap-count overshoot (shorter completions → more plays needed to move the same distance/convert downs — consistent with Round 5's finding that plays-per-drive, not drive count, is where the overshoot lives).

### Instrumentation added (additive, does not affect game logic)

Neither air yards, YAC, nor per-play completion/gain were tracked anywhere outside of aggregated player stats, so a same-pattern extension of the existing "Positional Evaluator Hook" (`game_engine.py`, see §8 of AGENTS.md) was added: `last_play_pre_yardline_100`, `last_play_is_complete_pass`, `last_play_yac`, `last_play_gain`. A `pre_yardline_100 = self.yardline_100.copy()` snapshot was added at the very top of `simulate_play_step` (before any mutation) so post-hoc zone classification uses the correct line of scrimmage, not the post-play position. New diagnostic script: `scripts/eda/analyze_pass_yardage_breakdown.py` — pulls real 2021-2025 completed-pass PBP (air_yards, yards_after_catch, yardline_100) and drives `NFLGameEngine` directly (not via `BatchSimulator`, which doesn't expose per-play internals) across a 16-matchup, N=300 sample of real Week 1-4 2025 games, reading the new hook fields after every `simulate_play_step()` call.

### Finding 1: air yards are fine; YAC is the shortfall

| Metric | Sim | Real | Delta |
|---|---:|---:|---:|
| Air yards (mean, completions) | 5.45 | 5.72 | −4.8% |
| **YAC (mean, completions)** | **4.45** | **5.19** | **−14.4%** |
| Yards gained (mean, completions) | 9.41 | 10.93 | −13.9% |

Depth-of-target selection (Round 6 already confirmed attempt-level zone distribution matches real almost exactly) and now the actual air-yards magnitude on completions are both close to real. The gap lives almost entirely in yards-after-catch.

### Finding 2: by volume, the fix belongs in primary/deep/screen zones

| Depth bucket | Share of completions | YAC delta | Yards-gained delta |
|---|---:|---:|---:|
| Standard (0-20 air yds) | 69% | −8.3% | −7.7% |
| Screen (≤0 air yds) | 26% | −18.0% | **−27.8%** (compounds hard — screens are almost all YAC) |
| Deep (≥20 air yds) | 5% | **−34.6%** | −15.1% |

`yac_model_v_0_1_1`'s trained regressors are under-predicting after-the-catch yardage across the board, worst on deep balls and screens (where YAC dominates the total). Standard passes (69% of volume) are the closest to right but still short. **This, not the squeeze-play mechanic, is the best-supported remaining lever for both the pass-yards shortfall and the snap-count overshoot** — shorter real gains per completion mechanically requires more plays per drive to reach the same down-to-down and scoring outcomes.

### Finding 3: redzone/goalline is a *different* bug — over-prediction that gets clipped away, not under-prediction

Cam's hypothesis was that goalline plays (mostly snapped from the 1-2) have almost no physical room for YAC, and that a mismatch there could produce the apparent shortfall. Checking directly: sim and real goalline completions sit at almost the *same* distance from the goal line at the moment of the catch (`yardline_100 − air_yards`: sim 1.33 vs. real 1.28 — essentially identical). So it isn't a mix/distribution artifact. What's actually happening: the goalline YAC booster predicts a mean YAC of **3.08 yards when the physical room at the catch point averages only 1.33 yards** — more than double what the field allows. `game_engine.py` only clips the *combined* `air_yards + yac` to the pre-snap `yardline_100` (correct in principle — total gain can't exceed the pre-snap distance to the goal), not YAC individually against room-remaining-after-air-yards. So the over-prediction is real, but invisible in the final output most of the time because the hard clip silently truncates it — except the clipping is aggressive enough (many lanes' raw predicted gain exceeds the tiny remaining field) that it nets out as an *apparent* shortfall in the final numbers (yards-gained mean 1.78 vs. real 2.70, −34%) despite the underlying issue being over-prediction, not under-prediction. Redzone shows the same mechanism at smaller scale: air yards +12.9% over real, YAC +12.6% over real, but yards-gained still −7.9% under — both components run hot, room-to-goal-after-air runs −11.9% short, and the clip eats the difference.

**Practical read:** goalline (~4% of completions) and redzone (~10%) are a *secondary*, mechanically distinct problem — the models there aren't respecting how little field is left, not that they're systematically shy the way the primary/deep/screen zones are. Worth fixing, but shouldn't be conflated with the main YAC recalibration, and isn't where the bulk of the season-long yardage/play-count gap comes from (that's primary, at 86% of volume).

### Finding 4: the training script in the repo does not match what's actually deployed

Went looking for how to retrain `yac_model_v_0_1_1` and found `src/nfl_sim/models/yac_model_v_0_1_1/train.py` trains a **single**, non-zone-split model with a completely different feature set (`elusiveness`, `broken_tackle_rate`, `catch_rate`, `avg_separation_yds`, `pressure_rate`, `is_screen`/`is_std`/`is_deep` as features, its own from-scratch DNA computed off `data/hardened_pass_training_master_v2_5.csv`) and saves one file, `yac_reg_v_0_1_1.joblib`. What's actually loaded at runtime (`inference.py`) is **three separate zone-specific boosters** (`primary_yac_reg.joblib`, `redzone_yac_reg.joblib`, `goalline_yac_reg.joblib`) using an entirely different feature set (`air_yards`, `yardline_100`, `ydstogo`, `score_differential`, `game_seconds_remaining`, `cpoe_by_filter`, `target_share_by_filter`, `carry_share_by_filter` — matching `metadata.json`, which is itself stamped `"version": "V.0.2.0"` despite living in a `_v_0_1_1` folder). The zone-split, DNA-integrated retrain that produced the artifacts actually running today was never committed to the repo as a script — `train.py` is orphaned, left over from an earlier model generation.

**This isn't isolated to YAC.** `air_yards_v_0_1_1/train.py` has the identical problem: it trains a single global tri-gate + 3 regressors and saves them as `tri_gate_v_0_1_1.joblib` / `screen_reg_v_0_1_1.joblib` / `std_reg_v_0_1_1.joblib` / `deep_reg_v_0_1_1.joblib` (no zone prefix), while `inference.py` loads zone-prefixed files (`{zone}_tri_gate.joblib`, `{zone}_screen_reg.joblib`, etc.) that must have come from a different, uncommitted training run. The air-yards model's *feature engineering* (DNA joins from `data/dna/*.json`, matching the deployed pattern) is a much better structural template than the YAC train.py's bespoke-DNA approach — but neither script, as it sits in the repo, can be run as-is to reproduce or retrain what's actually deployed.

**Consequence for "retrain the YAC model":** there is no existing, working script to just re-run. A new zone-split training script has to be written from scratch, informed by (a) `metadata.json`'s feature schema (the ground truth for what's deployed) and (b) `air_yards_v_0_1_1/train.py`'s DNA-joining structure as the closest available template — not by resurrecting either existing `train.py` verbatim.

**Follow-up, checked against `docs/v_0_2_0_implementation_plan.md` (Cam pointed at this doc):** the three-zone architecture (Model A/B/C = primary/redzone/goalline) is exactly what the plan's §1 "Multi-Zone Spatial Play Architecture" specified — this was an intentional, documented design decision, not an undocumented drift. What the plan doesn't contain is the actual training code. Confirmed the orphaned-script pattern is **at least a third model, not just two**: `rush_yards_v_0_1_0/metadata.json` is also stamped `"V.0.2.0"` and ships the same single-model-plus-zone-triplet artifact layout (`rush_yards_model.json` alongside `primary_/redzone_/goalline_rush_yards_model.json`), but has **no training script at all** in its folder (not even a stale one). `docs/eda_outputs/yac/evaluation_v_0_1_1/` and `docs/eda_outputs/air_yards/evaluation_v_0_1_1/` do exist and match what the checked-in `train.py` scripts would produce — confirming those scripts really were run once, for a first-generation single-model version, before being superseded by whatever produced the actually-deployed zone-split "V.0.2.0" artifacts. Searched git history (`git log --all --grep`) and found no commit referencing the zone-split retrain — it was never committed, in this repo or its history, full stop.

### Status: diagnosis complete, fix not yet started

Nothing has been changed in the model or its calibration this round — only read-only diagnostics and additive instrumentation (see above). Decision point for Cam: scope and order of the actual fix (rewrite the YAC training script zone-by-zone per Finding 4, decide whether to also fix the redzone/goalline over-prediction via a room-to-goal-aware feature or a catch-point YAC clip, and whether to tackle air_yards' identical orphaned-script problem in the same pass since it shares the same root cause).

**Correction while writing this up — the "missing training script" story needed one more check.** Cam pointed at `docs/v_0_2_0_implementation_plan.md`, which documents the zone-split architecture as an intentional design (§1, Model A/B/C = primary/redzone/goalline) — not accidental drift. Checked further: `rush_yards_v_0_1_0` has the identical pattern (metadata stamped `"V.0.2.0"`, zone-split artifacts deployed, but zero training script — not even a stale one). `docs/eda_outputs/{air_yards,yac}/evaluation_v_0_1_1/` confirm the checked-in `train.py` scripts really were run once, for an earlier single-model generation, before being superseded. **Bigger context, found while checking git history for the missing retrain:** `git ls-files src/nfl_sim/models/` returns **zero tracked files** — the entire `src/nfl_sim/models/` directory (every model, every training script, every `.joblib`, all of `air_yards_v_0_1_1`/`yac_model_v_0_1_1`/everything else) has never been committed to this repository, ever. Same story for `data/` (gitignored outright, first line of `.gitignore`). So the "orphaned script" mystery isn't really a mystery — nothing under `models/` or `data/` was ever expected to be in git history in the first place, consistent with how `data/` is already deliberately excluded. This repo relies on OneDrive sync (not git) for these directories. Worth Cam's explicit call on whether that's the intended long-term posture — right now a `git clean` or a lost local copy would take the entire trained-model layer with it, with no recovery path.

---

## Round 9: YAC Retrain — Fixed

Cam's decisions going in: situational features only this round (no player skill traits — checked NGS coverage via `nfl_data_py.import_ngs_data('receiving')` first: `avg_separation` has solid coverage back to 2016 for ~200-220 qualifying pass-catchers/season, better than remembered, but `top_speed_mph` isn't part of the standard NGS dataset at all — no `speed` column in NGS receiving or rushing — so its source in `skill_dna.json` is unclear/likely sparse; dropped both for now, worth revisiting `avg_separation_yds` specifically later). Keep `room_after_catch` (`yardline_100 − air_yards`) as an experimental feature, cut if it doesn't earn its keep. Live `nfl_data_py` pull + `data/dna/*.json` joins (air_yards-style), 2020+ training window, `GroupShuffleSplit` by `game_id` for a 70/15/15 train/val/test split.

New script: `src/nfl_sim/models/yac_model_v_0_1_1/train_zone_split.py` — writes directly into the folder `model_registry.py` already loads from (`primary_/redzone_/goalline_yac_reg.joblib` + `metadata.json`), no wiring changes needed elsewhere for the model files themselves.

### The real root cause: objective function, not features

First training pass (`reg:absoluteerror`, matching the orphaned `train.py`'s original choice) reproduced the exact shortfall pattern from Round 8 on held-out test data: primary zone predicted mean 3.79 vs. real 5.49 (−30.9%). `reg:absoluteerror` optimizes for the conditional **median**; YAC is right-skewed (broken tackles, occasional long runs), so a median-fit model systematically undershoots the **mean** — and the mean is what the season-long aggregate metrics (yards/game) actually depend on. Switching to `reg:squarederror` (mean-targeting) on the same data, same split, same features:

| Zone | `reg:absoluteerror` (pred vs. real) | `reg:squarederror` (pred vs. real) |
|---|---|---|
| Primary | 3.79 vs 5.49 (−30.9%) | **5.44 vs 5.49 (−0.9%)** |
| Redzone | 3.07 vs 3.40 (−9.7%) | **3.67 vs 3.40 (+7.8%)** |
| Goalline | 0.77 vs 0.90 (−15.1%) | **0.90 vs 0.90 (−0.4%)** |

This objective-function bias was almost certainly present since the very first model generation — the orphaned `train.py` used `reg:absoluteerror` too. `room_after_catch` feature importance came in meaningful (13-88% depending on zone/objective, highest at goalline) — kept.

### Second bug, surfaced only after the model itself was fixed: noise-floor inflation

Re-ran the Round 8 real-vs-sim diagnostic (`analyze_pass_yardage_breakdown.py`) with the retrained model live. Overall YAC: −14.4% → **+2.0%**. Primary: −17.0% → **+1.2%**. Deep: −34.6% → **+0.3%**. But goalline was still showing **+237.7%** over real — almost unchanged from the pre-retrain +241%, despite the retrained model itself testing at 0.898 vs. real 0.902 on held-out data.

Root cause: the post-model noise injection in `game_engine.py` (`scale = 6.0 + elusiveness*1.8 + broken_tackle_rate*6.0`, then `yac = max(0, yac + exponential(scale) - scale)`) uses a flat minimum scale of 6.0 regardless of context. That's a reasonable relative magnitude against primary zone's ~5.5-yard mean, but wildly oversized against goalline's ~0.9-yard mean — adding a 6-yard-scale exponential to a ~1-yard base and then flooring at zero truncates most of the left tail, which mechanically inflates the mean. The effect scales inversely with the zone's typical YAC: worst at goalline, smaller at redzone, negligible at primary — exactly the pattern observed. This was always present; Round 8 misattributed it to the model itself, since the model was miscalibrated too at the time and the two problems were confounded.

**Fix:** `base_scale = np.maximum(0.75, yac * 1.25)` — scale the noise to the play's own predicted YAC instead of a flat constant. Preserves primary-zone behavior (ratio ≈1.25 matches the old flat-6.0-vs-~5.5-mean relationship there) while shrinking proportionally wherever the predicted mean is small.

### Final results (Round 9)

| Metric | Round 7 (pre-YAC-fix) | Round 9 (post-fix) | Real | Note |
|---|---:|---:|---:|---|
| **Overall YAC (completions)** | −14.4% | **−1.6%** | — | headline fix |
| Primary zone YAC | −17.0% | **+0.3%** | — | 86% of volume |
| Deep YAC | −34.6% | **−0.1%** | — | |
| Screen YAC | −18.0% | **−0.6%** | — | |
| Redzone YAC | +12.6%* | **+1.0%** | — | *pre-fix sign differs, see Round 8 Finding 3 |
| Goalline YAC | +241%* | **+49.9%** | — | improved 5x, not fully closed — small volume (~4% of completions), tiny absolute error (1.36 vs 0.90 yds) |
| Pass yards/game | 204.15 (−13.0%) | **220.14 (−6.2%)** | 234.67 | more than halved the gap |
| Completion rate | 64.15% | 64.20% | 64.40% | unaffected, as expected |
| Offensive snaps/game | 132.28 (+6.8%) | **132.76 (+7.2%)** | 123.82 | **essentially unchanged — see below** |
| Total snaps/game | 164.36 (+6.2%) | **164.95 (+6.6%)** | 154.79 | **essentially unchanged** |
| Sacks/game | 1.71 (−29.0%) | 1.71 (−29.1%) | 2.41 | untouched, deferred to v0.4.0 as before |

**Honest result on the original motivating question: fixing YAC did NOT fix the snap-count overshoot.** The hypothesis going into this round (shorter real completions → more plays needed per drive → explains both the yardage shortfall and the snap overshoot) is only half confirmed — it fully explains the yardage shortfall, but offensive/total snap counts didn't move (+6.8%→+7.2%, +6.2%→+6.6%, both flat to slightly worse, within simulation noise). This puts Round 5's squeeze-play theory (the 50% second-squeeze probability, Cam's explicit spec, not to be changed without asking) back as the leading unaddressed lever for the snap-count problem specifically, now fully decoupled from the passing-yardage question.

**Residual, not addressed this round:** air_yards is now the largest remaining piece of the pass-yards gap (−4.4% to −7.3% depending on zone/depth, essentially untouched by this round's work) — `air_yards_v_0_1_1` has the exact same orphaned-training-script problem as YAC did, and the same fix pattern (live pull, DNA joins, mean-targeting objective, grouped split) would likely apply directly. Goalline's remaining +49.9% YAC over-prediction is a candidate for a future look but is low-priority given its small volume share and tiny absolute yardage impact. Screens, which Cam flagged as "most of the difference" going into the noise-scale fix, turned out to be resolved by that same fix (−0.6%) — no separate work needed there after all.

---

## Round 10: Squeeze-Play Removal — Measured Contribution to the Snap-Count Overshoot

Cam's read on the still-open snap-count overshoot: keep offenses playing faster approaching the two-minute warning (realistic), but remove the "greedy for an extra play" mechanic (the discrete squeeze-play roll that deliberately rushed the snap specifically to guarantee an extra down before the warning fired) — expecting it to only account for a couple of plays a game, not the whole gap.

**Removed** the entire squeeze-play mechanic (`game_engine.py`, the `squeeze_prob`/`attempt_roll`/`first_squeeze`/`second_squeeze` block and the `squeeze_plays_used` counter). Replaced with a single call to `self._sample_pace_runoff(is_normal_reg)` for the whole regular-clock population, including the warning-zone window. This isn't a loss of realism: `_sample_pace_runoff`'s empirical pools are already keyed by fine-grained time windows including "Q2 4:00-2:00" and "Q4 5:00-2:00" (`analyze_clock_pace_grid.py`), which reflect real teams' actual hurry-up pace in that window from real 2021-2025 data — no separate mechanic is needed to produce faster snaps there, and no artificial extra-play guarantee has to ride along with it. `_run_clock`'s two-minute-warning clamp is untouched and still fires exactly at 2:00 regardless of how it's approached.

**Measured impact** (isolated — same models, same everything else, before/after this one change):

| Metric | Before (Round 9) | After (Round 10) | Real | Note |
|---|---:|---:|---:|---|
| Offensive snaps/game | 132.76 (+7.2%) | **130.47 (+5.4%)** | 123.82 | −2.3 plays/game |
| Total snaps/game | 164.95 (+6.6%) | **162.10 (+4.7%)** | 154.79 | −2.85 plays/game |

Cam's instinct ("surely this only wins a couple of plays a game") was right — closer to reality than Round 5's rougher inference-based estimate of ~5-6 combined plays/game. This closes about a third of the offensive-snap overshoot; **roughly 6-7 combined plays/game are still unexplained** and need a different lever. (Pass yards/game moved slightly further from real in this run, −6.2%→−7.8% — almost certainly Monte Carlo noise between separate simulation runs rather than a real effect, since this change touches only clock/play-count logic, not the passing model.)

### Remaining options for the snap-count overshoot (not yet investigated, prioritized)

1. **Re-measure drives-vs-plays-per-drive** (Round 5's original diagnostic, `drives_count` vs. real 21.70/game) now that completion rate, YAC, and squeeze-play have all changed since that measurement — confirm the overshoot is still concentrated in plays-per-drive before picking a lever, rather than assuming Round 5's finding still holds unchanged.
2. **Air yards retrain** — the last untouched piece of the passing-yardage picture (still −4.4% to −7.3% short by zone/depth). Same rationale as the original YAC hypothesis: shorter real gains per completion could mean more plays needed per drive. Worth testing directly since YAC's fix didn't move snap count — this may or may not either, but it's already planned work for the yardage gap regardless, so it's a low-cost test.
3. **Sacks recalibration** (deferred to v0.4.0, currently −29-30% under real at ~1.7 vs. 2.41/game) — correctly-calibrated sacks would mean more negative-yardage downs and harder 3rd/4th-and-long situations, which plausibly shortens drives (more punts, fewer plays run) rather than lengthening them. Currently scoped as its own v0.4.0 project bundled with trench-tier work; worth knowing it may also help here.
4. **The Round 3-4 pace-pool "+2s calibration nudge"** (`_sample_pace_runoff`, see §11.1) was added as a hack to reduce play count back when the passing model was much less accurate. That justification may no longer hold now that completion rate and YAC are both essentially exact — worth re-examining whether it's still doing useful work or just adding noise now that the actual passing model driving play count is far more accurate than when the nudge was calibrated.
5. **Turnover rate** (interception gate currently scales real model output by a flat `*0.80`, per `game_engine.py`) — if turnovers run below real rate, drives last longer on average before ending. Not checked this round.
6. **Administrative stoppages** (injury timeouts, replay reviews — deferred to v0.4.0) — these consume real clock time without being a play, so adding them would directly reduce how many snaps fit into a real ~60-minute game clock. A clean, direct lever, currently out of scope but worth remembering it's not just a "realism nice-to-have" — it bears directly on this exact problem.

No changes made this round beyond the squeeze-play removal — options 1-6 above are unstarted, listed for Cam's prioritization.

---

## Round 11: Air Yards Retrain — Architecture Fixed, Aggregate Calibration Roughly at Parity

Cam picked options 2 (air yards retrain) and 3 (sacks recalibration) from Round 10's list. This round covers air yards; sacks findings are Round 12 below.

Same problem, same fix pattern as YAC (Rounds 8-9): `air_yards_v_0_1_1/train.py` trains a single, non-zone-split double-hurdle model (one 3-way gate + 3 regressors) with a different feature set and saves without the zone prefix `inference.py` expects (`tri_gate_v_0_1_1.joblib` vs. `{zone}_tri_gate.joblib`). New script: `train_zone_split.py`, same methodology as YAC's (live pull 2020-2025, DNA joins, `GroupShuffleSplit` by `game_id`, 70/15/15). One structural difference from YAC going in: **the orphaned script already used `reg:squarederror`** for its regressors, so there was no known median-vs-mean objective bug to fix here — expectation going in was a smaller win than YAC's, and that held.

**New problem hit during training:** goalline has essentially zero real "deep" (air_yards >= 20) throws — physically can't legally gain 20+ air yards from inside your own 5. This means goalline's 3-way gate only ever sees 2 classes in training data, which crashes both explicit (`num_class=3`, crashes at prediction/scoring time on a shape mismatch) and implicit (`num_class` auto-inferred, crashes at fit time with `num_class=0`) approaches. Fix: detect the missing class upfront and skip zone-specific gate training there, falling back to a donor zone's gate instead. **First attempt used `primary` as the universal fallback donor and made things worse** (goalline air_yards −29.5%→−42.6%) — primary's gate was trained exclusively on `yardline_100 > 20` and had never seen a play from inside the 5, so it was extrapolating wildly on the one feature that matters most there, badly distorting the screen/std/deep mix. Switched the fallback to the *adjacent* zone by field position (`redzone`, which spans 6-20 and has thin-but-real deep-class training data of its own, 95 samples) — this is now the correct general pattern for any zone/level missing sufficient training data (see `train_zone_split.py`'s `fallback_donor` map).

**Per-level test-set calibration was good across the board** (pred_mean vs. real_mean): primary screen −2.78/−2.78, std 7.68/7.59, deep 29.34/29.26; redzone screen −2.69/−2.71, std 8.00/7.83; goalline screen −1.84/−1.78, std 2.87/2.91. No objective-function bug found, as expected.

**But the live in-engine and season-level numbers barely moved:**

| Metric | Before (Round 10) | After (Round 11) | Real |
|---|---:|---:|---:|
| Overall air yards (completions) | −5.6% | −6.5% | — |
| Pass yards/game | −7.8% | −8.1% | 234.67 |
| Offensive snaps/game | +5.4% | +5.2% | 123.82 |

**Honest read: this retrain mostly fixed reproducibility (a real, working, git-visible script that matches the deployed architecture) rather than accuracy** — the previously-deployed zone-split model (whatever undocumented process built it) was apparently already reasonably calibrated in aggregate, unlike YAC's original 14-34% miss. The one clear regression risk (goalline mix, still −42.2% air_yards even after the redzone-fallback fix, small volume ~4.5% of attempts) remains a real, unresolved residual — the fallback-donor fix helped in principle but didn't show up much in the measured number, suggesting the goalline gate's mix problem isn't fully explained by donor choice alone. Not chased further this round given its small volume share; worth a dedicated look later if goalline-specific accuracy becomes a priority.

---

## Round 12: Sacks Recalibration — Diagnosis (Retrain Not Yet Started)

Sacks currently run 1.67-1.71/game vs. real 2.41/game (−29% to −31%), unchanged across every round this session since it's untouched — deferred to v0.4.0 per the original plan, bundled with trench-tier work. Cam asked to move on it. This round is diagnosis only — investigated what's actually available and what's actually happening before writing any training code.

### Finding 1: the trench-tier matchup data the plan wanted already exists, unused

The v0.4.0 plan called for adding "explicit trench-tier (pass_block_tier vs. pass_rush_tier) matchup to chaos gate 2." `data/dna/trench_tiers_2025.json` already has exactly this — `pass_block_tier` and `pass_rush_tier` (1-5 grades, 31 teams) alongside `run_block_tier`/`run_stuff_tier`/`qb_cpoe_z`. Gate 2's actual trained feature list (`chaos_v_0_1_0/metadata.json`) has none of these — only continuous rate-based features (pressure rate, sack rate, CPOE, time-to-throw). This data has been sitting ready to use.

### Finding 2: a live inference-time bug — L4 features are duplicated, not real

Gate 2 was trained on `off_sack_rate_l4` and `def_sack_rate_l4` as features distinct from `sack_rate_allowed`/`def_sack_rate` (visible in the trained feature list). But at inference (`game_engine.py`'s Gate 2 feature construction), the code literally does:
```python
t_sack_rate_allowed[is_pass], # off_sack_rate_l4
t_def_sack_rate[is_pass], # def_sack_rate_l4
```
— feeding the exact same season-aggregate values into both the "season" and "last 4 games" slots. The model expects genuine recent-form signal in those two slots and never gets it. Real rolling-4-game sack rates aren't currently computed anywhere in the engine's DNA pipeline.

### Finding 3 (the big one): a downstream pipeline diverts roughly a quarter of predicted sacks away before they're counted

No calibration multiplier is applied to gate 2's raw `sack_prob` (unlike gate 4's interception path, which has an explicit `* 0.80`). But after `has_sack` is rolled, two further conversions happen before a "sack" is actually counted in the stats:
```python
escapes = has_sack & (rand < scramble_rate)          # QB scrambles away, ~5% typical
real_sacks = has_sack & ~escapes
is_throwaway = real_sacks & (rand < 0.20)            # 20% of the remainder becomes a throwaway
actual_sacks = real_sacks & ~is_throwaway
```
Rough math: `(1 − ~0.05) × (1 − 0.20) ≈ 0.76` — meaning only about 76% of what gate 2 predicts as `has_sack` ever gets recorded as an actual sack. If gate 2's raw prediction is calibrated against real recorded sack rate (the natural assumption — that's what it would have been trained on), this 24%-ish leakage is **on top of** an already-correct calibration, not a correction for something the model is missing. That's strikingly close to the observed 29-31% shortfall. Not yet confirmed which is true (model under-calibrated on its own vs. this pipeline double-counting a diversion that real sack-rate data already nets out) — needs a direct empirical check (instrument raw `sack_prob` similar to the YAC/air-yards hooks, compare gross vs. net rate) before touching anything, same discipline as Rounds 8-9.

### Status: diagnosis only, nothing changed

Recommended order for the actual fix, cheapest/most-likely-impactful first:
1. **Instrument and empirically test the scramble/throwaway diversion hypothesis** (Finding 3) — cheap, no retrain needed if confirmed; the fix would just be reducing the escape/throwaway rates (or removing the double-count) rather than touching the model at all.
2. **Fix the L4 duplicate-feature bug** (Finding 2) — either compute real rolling-4-game sack rates, or simplify gate 2's feature list to drop the fake L4 slots if that's not worth building yet.
3. **Add pass_block_tier/pass_rush_tier matchup** (Finding 1) and retrain gate 2 zone-style or as a single model (chaos_v_0_1_0 isn't currently zone-split) — the data is ready; this is the deferred plan's original ask, but per Finding 3 it may not be the biggest lever for the aggregate shortfall.

---

## Round 13: Quarter-Transition State Corruption — Found and Fixed

Cam requested a full readable play-by-play for a single simulated game to "get a feel for how it flows" (led to two new scripts: `scripts/print_readable_play_by_play.py`, a football-narrative version of `print_play_by_play_v020.py`, and new additive hooks `last_play_target_name`/`last_play_rusher_name`/`last_play_is_normal_play`). While reviewing the transcript, Cam spotted repeated identical down/distance/yardline lines with *different* players and gains attached — e.g. "PHI 1 & 10 at DAL 29" appearing twice in a row with two different rushers — and asked directly whether this was a genuine bug rather than something to explain away as clock-pacing behavior. It was a genuine bug, and a real one.

### The audit

Built `scripts/audit_play_continuity.py`: runs many games in parallel and checks one invariant every step — on a "normal" play (not a sack/interception/lost-fumble/accepted-penalty), if possession didn't change hands, field position must move by exactly the recorded gain. First run (200 games): **95 violations**. Every single one's `actual_yl` landed on a small set of fixed values (mostly **70**, the hardcoded second-half-kickoff spot) regardless of what the real play should have produced — a fixed constant showing up in place of a computed value is never random corruption, it's a specific line of code stomping state.

### Root cause

`game_engine.py`'s Second Half Kickoff Logic runs *inside* `_run_clock`, which is called from `simulate_play_step` **after** the current play's real yardage/down/distance have already been applied for the step. That logic used to do this unconditionally, every time a lane crossed into Q3:
```python
self.possession_is_away[q3_mask] = False
self.yardline_100[q3_mask] = 70
self.down[q3_mask] = 1
self.distance[q3_mask] = 10
self.needs_kickoff[q3_mask] = True
```
This directly overwrote whatever the just-resolved play had actually produced — discarding a real, already-recorded gain, first-down conversion, everything — on **every single game**, right at the Q2→Q3 boundary. It was also entirely redundant: the kickoff-resolution code (`ko_mask = active & self.needs_kickoff`, top of `simulate_play_step`) already computes the correct post-kickoff `down`/`distance`/`yardline_100` on its own the next call (touchback vs. return), exactly like it already does for every touchdown/field-goal-triggered kickoff. The `yardline_100=70`/`down=1`/`distance=10` presets here were never needed — the real bug was doing them *immediately*, in the same step, instead of just setting `needs_kickoff=True` and letting the existing deferred-kickoff pattern (already used everywhere else in the codebase) handle it on the next call.

**Player stats were not corrupted by this** — rushing/passing/receiving yardage crediting happens earlier in the function, before this code runs, so box-score numbers for that specific play were already correct. What got discarded was the *drive's continuity*: down, distance, field position, and (before the second fix below) sometimes possession.

### Fix, in two parts

1. **Deleted the premature `yardline_100`/`down`/`distance` overwrite entirely** — kept only `needs_kickoff=True` and `clock_stopped=True`, deferring to the existing kickoff-resolution code exactly like every other trigger. This alone cut violations from 95 to 6 (200-game sample).
2. **Remaining 6 were a possession-assignment edge case**: a score or a turnover-on-downs landing on the exact play that also ends Q2 already correctly changes possession via `_switch_possession` earlier in the step — but the unconditional `possession_is_away[q3_mask] = False` was re-forcing "home team receives" right after, clobbering that. Fixed by snapshotting possession at the very start of `simulate_play_step` (`self.step_start_possession_is_away`, a new instance attribute so `_run_clock`, a separate method, can read it) and only forcing the home-team-receives default for lanes whose possession is unchanged from the start of the step.

**Result, verified at scale (1000 games): 95 → 1 violation.** The single remaining one is a diagnostic-only false positive — an early-return code path (kneel-only steps don't reach the hook-update block) leaves the `last_play_*` hooks stale for that call, and my own script's independent pre/post snapshots agree nothing actually changed (70→70) — not a real state corruption, just a gap in the additive instrumentation's coverage on that specific path.

### Season-level impact: negligible, and that's an honest, important result

| Metric | Before | After | Real |
|---|---:|---:|---:|
| Offensive snaps/game | +5.2% | +5.2% | 123.82 |
| Total snaps/game | +4.6% | +4.5% | 154.79 |
| Pass yards/game | −7.8% | −8.1% | 234.67 |

Essentially unchanged. This makes sense in hindsight: the bug replaced "the rest of a drive in progress" with "one phantom fresh kickoff-drive" — plays lost from the truncated drive are roughly offset by plays gained from the drive that replaces it, netting out close to a wash at the season-aggregate level. **This does not explain the still-open snap-count overshoot** (Round 10's options list stands, unchanged in priority). But it was unambiguously worth fixing regardless — it was corrupting real game state in roughly half of all simulated games (every game has exactly one Q2→Q3 transition, and this fired whenever that transition landed on a real offensive snap rather than a kickoff/kneel/spike), a genuine correctness bug rather than a clock-physics calibration question, exactly the distinction Cam asked about.

`scripts/audit_play_continuity.py` is a keeper — a real regression test, not a one-off. Worth rerunning after any future change that touches quarter transitions, clock handling, or possession-switching logic.

---

## Round 14: Sacks Diversion Hypothesis — Tested, Partially Confirmed

Round 12's hypothesis: gate 2's raw sack probability might already be calibrated against real recorded sack rate, with the downstream scramble-escape (~5%) + throwaway (20% of the remainder) diversion pipeline removing an *additional* ~24% on top of an already-correct number — which would make the diversion pure double-counting and the fix trivial (reduce/remove it, no retrain needed).

**Instrumentation:** two new whole-game diagnostic counters in `game_engine.py`, `gross_sacks` (gate 2's raw `has_sack` roll, before any diversion) and `net_sacks` (`actual_sacks`, what actually gets counted — matches the `sacks_taken` stat tracked all session). New script: `scripts/eda/test_sacks_diversion_hypothesis.py` — runs 16 real matchups at N=1000 and compares both rates (both teams combined ÷ 2, matching real's per-team-game definition) against the real target.

**Result:**

| Metric | Sim | Real | Delta |
|---|---:|---:|---:|
| Gross sacks/team-game (pre-diversion) | 2.094 | 2.41 | **−13.1%** |
| Net sacks/team-game (post-diversion) | 1.661 | 2.41 | **−31.1%** |

Diversion removed 0.433 sacks/team-game — **20.7% of gross**, closely matching the theoretical ~24% estimate from Round 12's math. So the diversion mechanism's *size* was correctly diagnosed.

**But the "clean" version of the hypothesis doesn't hold:** gate 2's raw model is *also* under-calibrated on its own, independent of the diversion — even before any scramble/throwaway removal, the raw sack probability only produces 2.094/game vs. the real 2.41/game target, a genuine −13.1% shortfall. So this isn't one bug, it's two compounding ones: a real model-calibration gap (~13%) plus a real diversion effect (~21% of what's left) stacking to the observed −31%.

**Implication for the fix:** removing or reducing the diversion alone would move the number from −31.1% to roughly −13.1% — a big improvement, but not a full fix. Closing the remaining gap needs the raw model itself calibrated higher too — either a probability-space calibration offset (the same pattern used for the completion-rate fix in Round 7), or addressing it as part of the Round 12 findings still on the table (the L4 duplicate-feature bug, which may organically lift calibration once fixed; the trench-tier retrain, the v0.4.0 plan's original ask). Nothing changed in the engine this round beyond the two diagnostic counters — decision point for Cam on which lever(s) to pull and in what order.

---

## Round 15: Sacks — Diversion Reduced + Calibration Applied

Cam's call: fix genuine bugs, do the diversion reduction (Round 14's option 1) and calibration offset (option 2) now, defer the trench-tier retrain to v0.4.0 as already planned. One scoping decision made and flagged rather than assumed silently: the L4 duplicate-feature bug (Round 12 Finding 2) needs real per-week trench data that doesn't exist in the current pipeline (`trench_dna.json` is season-level only, no week granularity) — building that is the same kind of data-enrichment work as the trench-tier retrain, so it's bundled into v0.4.0 rather than force-fixed today.

**Two changes, both in `game_engine.py`'s Gate 2 (sacks) block:**

1. **Throwaway-diversion rate cut from 0.20 to 0.06.** `scramble_rate` (the other half of the diversion) was left untouched — it's real per-QB roster data (mobile QBs escape pressure more often), not an arbitrary constant, unlike the flat 20% throwaway rate. Reasoning: gate 2 was almost certainly trained on real recorded sack counts, which are already net of real QBs' own scrambling/throwing-away tendencies — stacking a second, engine-level diversion of the same magnitude on top double-counts that same real-world behavior. This does move further from the separately-deferred throwaway-rate target (~5% of all dropbacks, AGENTS.md §11.6/Round 6) — that target assumes a not-yet-built clean-pocket path contributing 25% of throwaways; reconcile the two when that work happens, not now.
2. **`SACK_PROB_CALIBRATION_MULT = 1.24`** applied to gate 2's raw `sack_prob`, same multiplicative-calibration pattern already used for gate 4's interception probability (`* 0.80`). Tuned empirically (not guessed) via `test_sacks_diversion_hypothesis.py`: first pass at 1.30 overshot (net 2.528 vs. target 2.41, +4.9%), scaled down proportionally to 1.24 and re-measured.

**Result, verified:**

| Metric | Round 14 (pre-fix) | Round 15 (post-fix) | Real |
|---|---:|---:|---:|
| Net sacks/game | 1.661 (−31.1%) | **2.410 (+0.2%)** | 2.41 |
| Gross sacks/game | 2.094 (−13.1%) | 2.579 (+7.0%) | 2.41 |
| Diversion removed | 20.7% of gross | 6.5% of gross | — |

Season-level cascading effects (full 64-game batch audit):

| Metric | Before | After | Real |
|---|---:|---:|---:|
| Sacks/game | −29.1% | **+0.2%** | 2.41 |
| Offensive snaps/game | +5.2% | **+4.2%** | 123.82 |
| Total snaps/game | +4.6% | **+3.8%** | 154.79 |
| Completion rate | 64.37% | 64.81% | 64.40% |
| Pass yards/game | −8.1% | −10.2% | 234.67 |

Two side effects, both understood and expected rather than new bugs: completion rate nudged up slightly (fewer zero-yard throwaway "attempts" in the denominator now that more of them are correctly counted as sacks instead) — still close, not worth chasing further on its own. Pass yards/game widened from −8.1% to −10.2% — sacks now correctly subtract real negative yardage instead of some of those plays being zero-yard throwaways, which mechanically pulls net passing yardage down further; this connects directly to the already-open air-yards shortfall (Round 11) rather than being a new problem. **Bonus, not the target of this round:** snap-count overshoot improved slightly on its own (offensive +5.2%→+4.2%, total +4.6%→+3.8%) — sacks apparently end drives slightly more efficiently than the throwaways they're replacing.

---

## Round 16: Full 18-Week Season Dataset Regenerated (Default Data Source)

Cam confirmed sack-yardage attribution was already correct (checked: `pYds` is only credited on completions, gated by `play_is_complete`, which sacks never satisfy — no fix needed) and asked for a full 1000-iteration-per-game simulation of all 18 weeks of the 2025 season, to become the default dataset backing the frontend tools (`HistoricalLab`, `GameSummary`, the DFS site) — generated data files, API serves them by default, not hardcoded. Scoped to backend/data generation this round; frontend UI wiring is a deferred follow-up. Single-game narrative views (WP/KEP timeline, chess evaluator) keep using real historical PBP, unaffected by this — the new simulated dataset powers stat projections/leaders/standings only. Default week for weekly viewing: 1 (already the case — `/api/week_projections`'s `week` parameter already defaults to `1`).

**Found the existing pipeline was already built for this** — `src/api/app.py` already reads from `data/interim/sim_results_2025_{games,players}.parquet` by default (`/api/weeks`, `/api/week_projections`, `/api/games`, etc.), and `scripts/simulation_runners/run_full_season_sim_2025.py` already simulates all 272 REG-season matchups and writes to those exact paths. The existing cache was stale: only Week 1's 16 games, generated before every fix this session (squeeze removal, quarter-transition bug, YAC/air-yards retrains, sacks calibration) — overwritten at some point by a narrower `run_week_1_only_2025.py` run. Backed up the stale files (not deleted — `data/interim/_stale_backup_pre_full_season_regen/`) and reran the full-season script.

**Result:** all 272 REG games (weeks 1-18) simulated at N=1000 with the current engine — 3149 seconds (~52 min) for the core simulation, plus a bonus 100-season playoff Monte Carlo already bundled into the same script (`docs/reports/season_summaries.csv`, `docs/reports/season_standings.md`). Verified: `sim_results_2025_games.parquet` (272 unique game_ids, 272000 rows) and `sim_results_2025_players.parquet` (10,795,000 rows) both cover all 18 weeks with the correct per-week game counts (14-16, matching bye weeks). Tested the API directly (not just the files) — `get_week_projections()` with no arguments correctly returns week 1's 16 games / 635 players from the fresh cache, and the stale on-disk JSON cache self-healed (the endpoint recomputes and re-saves `data/interim/week_N_full_projections.json` automatically when the parquet cache's mtime is newer).

**Full-season calibration check** (272 games × 1000 iterations, not just the 64-game/4-week sample used for most of this session's tuning): completion rate 64.88% vs. real 64.40%; sacks/team-game 2.418 vs. real 2.41; pass yards/team-game 210.83 vs. real 234.67 (−10.2%, matching the already-known, already-explained air-yards gap). Everything holds up at full scale.
