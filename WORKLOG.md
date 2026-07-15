# WORKLOG.md
<!-- Reverse-chronological session ledger. Newest entry at top. -->
<!-- Format: ### [YYYY-MM-DD] Handoff from [Model] -->

---

### [2026-07-11] Handoff from Claude Sonnet 5 (session 2 — YAC diagnosis + retrain)

- **Active Task:** `clock_physics_v020` audit, Rounds 8-9 — diagnosed and fixed the yards-per-completion shortfall (9.70 vs. real 10.93, ~−11%) that Round 6 had only ever diagnosed as secondary to completion rate, never actually fixed. Picked up cold in a fresh context window from the prior session's handoff below.

- **Files Modified/Created:**
  - [`src/nfl_sim/game_engine.py`](src/nfl_sim/game_engine.py): added additive diagnostic instrumentation (`last_play_pre_yardline_100`, `last_play_is_complete_pass`, `last_play_yac`, `last_play_gain`, a `pre_yardline_100` snapshot at the top of `simulate_play_step`). Added `room_after_catch` as a 9th YAC-model feature. Fixed the post-model noise-injection scale (`base_scale = max(0.75, yac * 1.25)` instead of a flat `6.0`) that was mechanically inflating YAC wherever the real mean is small (goalline worst).
  - [`src/nfl_sim/models/yac_model_v_0_1_1/train_zone_split.py`](src/nfl_sim/models/yac_model_v_0_1_1/train_zone_split.py) (new): the actual, working zone-split YAC training script — live `nfl_data_py` pull (2020-2025) + `data/dna/*.json` joins, `GroupShuffleSplit` by `game_id` for 70/15/15 train/val/test, `reg:squarederror` objective (mean-targeting, see below). Produces `{primary,redzone,goalline}_yac_reg.joblib` + `metadata.json` directly in the folder `model_registry.py` already loads from.
  - [`scripts/eda/analyze_pass_yardage_breakdown.py`](scripts/eda/analyze_pass_yardage_breakdown.py) (new): real-vs-sim diagnostic comparing air yards/YAC/total gain by zone and by depth bucket. Drives `NFLGameEngine` directly (not `BatchSimulator`) to read the new per-play hook fields.
  - [`docs/audit/clock_physics_v020/README.md`](docs/audit/clock_physics_v020/README.md): Round 8 (diagnosis) and Round 9 (fix) written up in full.
  - [`AGENTS.md`](AGENTS.md): §0 priorities, §11.7, and several fragile-area notes updated to reflect the fix and the new open items.

- **Key findings, in order of discovery:**
  1. Air yards were already well-calibrated; the shortfall was entirely in YAC (yards after catch) — worst on deep balls and screens.
  2. Redzone/goalline had an *opposite-direction* problem (over-prediction, masked by a downstream clip) — turned out to be two separate bugs, both eventually fixed.
  3. Investigated retraining and found `yac_model_v_0_1_1/train.py` (and `air_yards_v_0_1_1/train.py`, and `rush_yards_v_0_1_0` with no script at all) don't reproduce what's actually deployed — a zone-split "V.0.2.0" architecture that superseded an earlier single-model generation without ever committing the new training code. Traced this to a much bigger fact: `git ls-files src/nfl_sim/models/` returns **zero tracked files, ever** — the entire models directory (and `data/`, gitignored outright) has never been in git. Flagged for Cam to decide whether that's intentional (OneDrive-sync-only for these directories).
  4. Root cause #1 (the big one): the model was trained with `reg:absoluteerror` (median-targeting) instead of `reg:squarederror` (mean-targeting). YAC is right-skewed, so median-fit systematically undershoots the mean that season-long aggregates depend on. This one fix closed most of the gap (primary zone: −30.9% → −0.9% on held-out test data).
  5. Root cause #2 (surfaced only after #1 was fixed): a flat noise-scale floor (6.0 yards) in `game_engine.py`'s post-model noise injection, combined with flooring negative draws at zero, mechanically inflated the mean wherever real YAC is small (goalline hit hardest). Fixed by scaling noise to the play's own predicted YAC instead.
  6. Added `room_after_catch` (yardline_100 − air_yards) as an experimental feature per Cam's direction (give the model yardline + air yards, let it learn the taper) — confirmed high feature importance, kept.
  7. Deliberately left out player-skill traits (elusiveness, broken_tackle_rate, top_speed_mph, avg_separation_yds) this round on Cam's call — checked NGS data coverage first and found `avg_separation_yds` has decent real coverage back to 2016 (better than Cam remembered) while `top_speed_mph` isn't in the standard NGS dataset at all (likely genuinely sparse) — situational-only features for now, `avg_separation_yds` flagged as worth a future incremental add.

- **Verification Performed:** `py_compile` + `pytest tests/test_positional_evaluator.py` (18/18) after every `game_engine.py` edit. Re-ran `analyze_pass_yardage_breakdown.py` after the model retrain AND again after the noise-scale fix to confirm each fix's effect in isolation. Full batch audit (`run_weeks_1_to_4_2025.py`, 64 games × 1000 iterations) run before/after to measure season-level cascading effects.

- **Current System Status:** All 18 tests passing. YAC: −14.4% → −1.6% overall (essentially exact in primary/deep/screen/standard; goalline improved 5x, +241%→+49.9%, small residual on a ~4%-of-completions, tiny-absolute-yardage zone). Pass yards/game: −13.0% → −6.2% (more than halved). Completion rate unaffected as expected (64.20% vs. real 64.40%). **Offensive/total snap-count overshoot did NOT improve** (+6.8%→+7.2% offensive, +6.2%→+6.6% total) — the hypothesis that fixing YAC would also fix snap-count is now confirmed only half right. Sacks unchanged (−29.1%, still deferred to v0.4.0).

- **Immediate Next Steps for the Next Agent:**
  1. **Snap-count overshoot is the next open thread**, now fully decoupled from passing accuracy. Round 5's squeeze-play theory (plays-per-drive running hot, not drive count) is the leading suspect again — the 50% second-squeeze probability is Cam's explicit spec, don't change without asking.
  2. **air_yards is now the largest remaining piece of the pass-yards gap** (−4.4% to −7.3% depending on zone/depth). `air_yards_v_0_1_1` has the identical orphaned-script problem YAC had; the same fix recipe (live pull + DNA joins, `reg:squarederror`, grouped split) would likely apply directly — `yac_model_v_0_1_1/train_zone_split.py` is a ready-made template.
  3. Goalline YAC's remaining +49.9% over-prediction is a candidate for a future look, low priority (small volume, tiny absolute yardage).
  4. **Ask Cam directly about the git-tracking gap** for `src/nfl_sim/models/` and `data/` before assuming it's fine to leave as-is — this wasn't addressed this session beyond flagging it.
  5. Re-run `analyze_pass_yardage_breakdown.py` after any future YAC/air-yards change to confirm nothing regressed.

---

### [2026-07-11] Handoff from Claude Sonnet 5 (session 1)

- **Active Task:** `clock_physics_v020` audit — engine clock-physics rebuild + completion-rate model fixes. Full round-by-round history: [docs/audit/clock_physics_v020/README.md](docs/audit/clock_physics_v020/README.md) (7 rounds). Session ending on context-window limit, not task completion — this is a mid-thread handoff.

- **Files Modified:**
  - [`src/nfl_sim/game_engine.py`](src/nfl_sim/game_engine.py): the vast majority of this session's work. Fixed a long-standing bug where the game never simulated a 4th quarter (`_run_clock`'s `end_game` check re-read `self.quarter` after it had already been mutated for Q3→Q4 transitions). Rebuilt the clock/pace model (empirical bootstrap pools replacing a flat `randint(18,30)`), added an explicit out-of-bounds mechanism, an automatic two-minute-warning stoppage, and a "squeeze play" mechanic (teams rushing a snap to fit an extra play in before the warning, per Cam's explicit probabilities). Fixed several clock-stop bugs (scoring plays, turnover-on-downs, punts, field goals weren't stopping the clock). Recalibrated the screen catch rate (was hardcoded 80/95% RB/WR-TE, real data showed 82.68/80.01%) and fixed QB CPOE being applied in logit space instead of probability space (was compressing QB skill differentiation to ~1/3 of intended effect). Added a probability-space baseline calibration offset to hit the confirmed real completion-rate target (64.40%). New persistent counters: `oob_plays`, `drives_count` (diagnostic), `two_minute_warning_used`, `squeeze_plays_used`.
  - [`src/nfl_sim/model_registry.py`](src/nfl_sim/model_registry.py): wired in the new `clock_pace_v_0_1_0` model.
  - `src/nfl_sim/models/clock_pace_v_0_1_0/` (new): `inference.py` (ClockPaceModelV010), `pace_pools.json` + `metadata.json` (generated, empirical runoff pools by quarter/time-window x score-margin tier).
  - [`scripts/eda/analyze_clock_pace_grid.py`](scripts/eda/analyze_clock_pace_grid.py) (new): builds the pace grid/pools from real 2021-2025 pbp data.
  - [`scripts/print_play_by_play_v020.py`](scripts/print_play_by_play_v020.py) (new): non-interactive single-game play-by-play dump for manual clock verification (found the Q4 bug via this).
  - `scripts/eda/analyze_plays_per_game.py` (new): league-wide plays/game EDA, triggered by a Bills-specific observation (turned out to be their defense forcing short drives, not their offense).
  - [`scripts/simulation_runners/run_weeks_1_to_4_2025.py`](scripts/simulation_runners/run_weeks_1_to_4_2025.py) and [`scripts/eda/run_historical_eda.py`](scripts/eda/run_historical_eda.py): extended with new tracked metrics (snap counts by category, completion rate) across the session.
  - [`AGENTS.md`](AGENTS.md): updated this session with current priorities, new fragile-areas entries, updated test commands and repo layout.

- **Verification Performed:** `python -m py_compile src/nfl_sim/game_engine.py` and `python -m pytest tests/test_positional_evaluator.py -v` (18/18 passing) after every sub-change. Full batch audit (`run_weeks_1_to_4_2025.py`, 64 games x 1000 iterations) rerun ~10 times across the session to measure each change against real 2021-2025 data.

- **Current System Status:** All 18 tests passing. Engine runs cleanly end-to-end. Completion rate is essentially exact (64.15% vs. real 64.40% target). Offensive/total snap count is currently running **+6.8%/+6.2% over real** — this got worse (not better) after the Round 7 completion-rate fixes and is the main open thread (see below). Sacks remain ~30% under real (deferred, not a bug in this session's work).

- **Immediate Next Steps for the Next Agent:**
  1. **Do not start a new investigation without reading [docs/audit/clock_physics_v020/README.md](docs/audit/clock_physics_v020/README.md) first** — several plausible-sounding hypotheses were already tested and ruled out this session (yards/big-play rate, drive count, OOB reduction magnitude — see §11.5 of AGENTS.md for the list). Re-litigating these wastes a turn.
  2. **Open decision point:** the play-count overshoot (+6.8% offensive snaps) most likely traces back to the squeeze-play mechanic (Round 5 finding: drive count matches real almost exactly, but plays-per-drive runs hot — squeeze plays add a down within a drive without ending it). The 50% second-squeeze probability (trailing 2+ scores) was Cam's explicit, deliberate spec — do not change it unilaterally; ask him directly whether to dial it back, now that the completion-rate fix has shifted the baseline again.
  3. **Deferred to v0.4.0** (has Cam's scope agreement, not yet started): sacks recalibration (add trench-tier OL/DL matchup to chaos gate 2, retrain) bundled with injury stoppages and other administrative clock stoppages (replay review, etc.).
  4. **Deferred, parameters already agreed:** throwaway mechanism rework — target ~5% of dropbacks, 75% pressure-related / 25% clean-pocket. Current mechanism only fires as a 20% escape from would-be-sacks; needs a clean-pocket path independent of `sack_prob`.
  5. Re-run the full batch audit after any of the above to keep `docs/audit/clock_physics_v020/README.md`'s round-by-round numbers current.

---

### [2026-06-23] Handoff from Claude Sonnet 4.6 (session 3)

- **Active Task:** HistoricalLab score + lines framing fixes; EFSD design discussion
- **Files Modified:**
  - [`src/nfl_sim/week1_2025.py`](src/nfl_sim/week1_2025.py): Replaced `home_score`/`away_score` (which nfl_data_py stores as FINAL game score on every row) with `posteam_score`/`defteam_score` (running score at each play). Added both to `REQUIRED_COLS` and per-play row dict.
  - [`frontend_analysis/src/pages/HistoricalLab.jsx`](frontend_analysis/src/pages/HistoricalLab.jsx):
    - **Score fix:** Derive running home/away score from `posteam_score`/`defteam_score` + possession. `homeScoreNow = isHomePoss ? posteam_score : defteam_score`. Scoreboard now shows score at the selected moment in the game, not the final score.
    - **Lines framing fix:** `LinesPanel` now converts offense-reference `delta_kep` to home-team reference (`delta_home = isHomePoss ? delta_kep : -delta_kep`) and re-sorts: home possession → descending (optimizing for +), away possession → ascending (optimizing for −). BEST badge goes on the play that most benefits the possessing team in home-KEP terms.
    - Added possession context header: "PHI possesses — optimizing for +" / "DAL possesses — optimizing for −" with team color swatch.
    - BEST badge now uses the possessing team's color instead of generic cyan.
    - KEP trajectory in lines panel now labeled "home KEP" with team-colored values.
    - Tooltip score now derived from `posteam_score`/`defteam_score` correctly.
  - [`frontend_analysis/src/pages/HistoricalLab.jsx`](frontend_analysis/src/pages/HistoricalLab.jsx): Added `frontend_analysis/.claude/launch.json` for preview server (port 5174).

- **Root cause of "No data" issue:** Race condition — frontend loads before backend finishes warming up (~10s to load PBP + models). Silent `.catch(() => {})` hides the error. Fix: hard-refresh browser after backend is ready. Long-term fix: add retry or error state with a "Reload" button.

- **Verified working:** Q1 first play shows DAL 0 – PHI 0. Q2 0:27 with DAL possession shows correct running score + lines optimizing for − (away team). BEST badge in Cowboys navy on the play that most pushes home KEP negative.

- **Immediate Next Steps:**
  1. Train EFSD model (see design session entry below)
  2. Write implementation plan (Gemini handoff)
  3. Fix silent fetch failure UX in HistoricalLab (add error state + retry button)
  4. Full site walkthrough when ready

---

### [2026-06-23] Design session — KEP alternatives and EFSD

- **Topic:** Conceptual discussion on KEP metric definition and alternatives
- **Key decision:** EFSD (Expected Final Score Differential) is the preferred next iteration of KEP

**Problem with current KEP:**
Current KEP inverts the WP model to find "what kickoff margin yields this same WP?" — it's a nonlinear rescaling of WP, not an independent measure. WP defines WP. Circular.

**EFSD approach:**
- Training: pull historical PBP, join each play to its game's final score margin (home - away), train XGBoost regressor to predict that margin from game state features (same features as WP model).
- Output IS the metric — no inversion step, no WP in the chain.
- Scale: naturally continuous (regression outputs weighted averages across many outcomes), not discrete even though NFL margins cluster around 3/7/10.
- Discretization risk: NFL margin distribution is lumpy (3/7/10/14 are overrepresented). Fix if needed: apply Gaussian smoothing (σ ≈ 1.5) to target during training. Build first, check chart smoothness, add smoothing only if prediction surface looks stepped.

**Key property: time-invariant bands**
EFSD already encodes clock into its predictions (clock is a feature). An EFSD of +7 at halftime and +7 with 30 seconds left represent genuinely equivalent expected outcomes — the model gives different scores to each game state, so same output = same expected destination.

**Preliminary band hypothesis (to validate empirically):**
| Band | EFSD | Football meaning |
|---|---|---|
| Even | ±3 | Within a field goal, anyone's game |
| Advantage | ±3 to ±8 | One possession edge, not safe |
| Clear advantage | ±8 to ±16 | Two possessions, opponent needs multiple things |
| Decisive | ±16+ | Three+ possessions, very hard to recover |

Validation method: on holdout set, bucket EFSD predictions and check actual win rates per band. Target: Even→~50%, Advantage→60-70%, Clear→75-85%, Decisive→90%+. Adjust band boundaries until calibrated.

**What EFSD does better than current KEP:**
- No artificial ±24 ceiling (a team up 35-0 late can show EFSD +32, not clamped)
- More intuitive at end of game ("expected to win by 9" vs "equivalent to +18 at kickoff")
- Independent of WP model — anchors to scoring outcomes directly

**What to watch for:**
- EFSD values will NOT map linearly to current score differential (e.g., up 10 at halftime → EFSD ≈ +5 due to regression to mean). This will surprise users at first. Display current score alongside EFSD prominently.
- "Feels different" intuition (halftime vs 30 seconds) is path anxiety, not positional difference. If EFSD is the same, the situations are equivalent — correct behavior, not a bug.

**Status:** Design agreed. Implementation deferred until current Historical Lab is further validated. Next step when ready: write `train_efsd.py` modeled on `train_positional_ep.py`, swap target variable, validate bands on holdout set.

---

### [2026-06-23] Handoff from Claude Sonnet 4.6 (session 2)

- **Active Task:** HistoricalLab polish — team colors, score display, chart click reliability
- **Files Modified:**
  - [`frontend_analysis/src/pages/HistoricalLab.jsx`](frontend_analysis/src/pages/HistoricalLab.jsx):
    - Added `TEAM_COLORS` lookup table (all 32 teams, primary colors) and `teamColor(abbr)` helper
    - Eval bar now uses team-specific colors for fill gradients, divider dot, and KEP value (no more generic cyan/orange)
    - Added live scoreboard block inside `EvalBar` — shows `awayScore – homeScore` with the leading score in its team color; updates with selected play
    - X-axis label corrected to "← Game Progression →" (was incorrectly showing team names)
    - Y-axis label updated to "↑ {homeTeam}  KEP  {awayTeam} ↓" to convey which direction favors which team
    - Chart tooltip now shows running score at hover point ("DAL 7 – PHI 14") above situation line, using team colors
    - Chart dots: every play now shows a small persistent dot (r=2.5, translucent) — selected play shows large solid dot (r=6). Each dot has a direct `onClick` handler on the SVG circle, bypassing Recharts synthetic event limitations.
    - Added `useRef(topRef)` + `scrollIntoView({ behavior: 'smooth' })` in `selectPlay()` — clicking a chart dot scrolls the eval bar + lines panel into view.
  - [`src/nfl_sim/week1_2025.py`](src/nfl_sim/week1_2025.py): Added `home_score` and `away_score` to `REQUIRED_COLS` and per-play row dict so running score is available at every play.

- **Backend restart:** PID 7564 → new process after Python change (cleared PBP cache, reloaded with score fields).

- **Open UX issue (not resolved):** Chart point clicks — user still not seeing the expected behavior (score at clicked play vs. end-of-game score). Root cause not fully identified this session. Dot onClick handlers are wired; `activeDot onClick` and `LineChart onClick` both call `selectPlay`. Possible remaining causes: (a) dots are small and hard to hit precisely, (b) Recharts `dot` render function creates new elements on every render which may interfere with event binding. Consider replacing chart click with a scrollable play list below the chart as a more reliable interaction model.

- **Immediate Next Steps for the Next Agent:**
  1. Resolve chart click UX — verify whether dot onClick is actually firing (add temporary console.log). If not, replace with a play list below the chart (each row is a clickable play, shows clock + situation + KEP).
  2. Full UI review walkthrough with Cam (both sites, all tabs).
  3. Remove `HistoricalLab` from nav/routing before production launch.

---

### [2026-06-23] Handoff from Claude Sonnet 4.6 (session 1)

- **Active Task:** Historical Testing Lab — chess evaluator over Week 1 2025 NFL games
- **Files Modified / Created:**
  - [`src/nfl_sim/nfl_positional_evaluator.py`](src/nfl_sim/nfl_positional_evaluator.py): Added `evaluate_one_step()` (simulates exactly ONE scrimmage play per lane, computes KEP of resulting state via three-case logic: mid-drive → direct WP→KEP, scored/terminal → `_drive_end_kep`, turnover → opponent KEP from turnover yardline) and `suggest_lines()` (chains `evaluate_one_step` calls using `mean_next_state` to build depth-2 principal-variation concept chains).
  - [`src/nfl_sim/week1_2025.py`](src/nfl_sim/week1_2025.py): New file (directly in `src/nfl_sim/`, NOT a subpackage — see fragile notes). Loads Week 1 2025 schedule and PBP via `nfl_data_py`. `get_game_list()` returns 16 games. `get_game_plays(game_id, evaluator)` returns scrimmage plays with `home_kep` (home-team reference) and `ep` pre-computed per play.
  - [`src/api/app.py`](src/api/app.py): Added `_WEEK1_MODULE` lazy singleton and three new endpoints: `GET /api/historical/week1-2025`, `GET /api/historical/plays/{game_id}`, `GET /api/historical/suggest-lines`.
  - [`frontend_analysis/src/pages/HistoricalLab.jsx`](frontend_analysis/src/pages/HistoricalLab.jsx): New page. Lichess-style layout: `EvalBar` (horizontal split, home=positive/cyan, away=negative/orange, proportional fill, divider dot), `LinesPanel` (top-3 concept chains with BEST badge and KEP trajectory), `KEPTooltip`, and full-game KEP timeline (Recharts LineChart, home_kep solid cyan, EP dashed orange, zero reference line, click to select play and fetch lines).
  - [`frontend_analysis/src/pagesConfig.js`](frontend_analysis/src/pagesConfig.js): Added `historical-lab` page entry (showInNavbar: true, "⚗ Testing Lab" label).
  - [`frontend_analysis/src/App.jsx`](frontend_analysis/src/App.jsx): Imported `HistoricalLab`, added `case 'historical-lab'` to render switch.

- **Verification Performed:**
  - DAL @ PHI (20-24): 128 plays loaded, PHI home_kep climbs to +24 at final kneel — chart shows steady PHI advantage after halftime, correct.
  - BAL @ BUF (40-41): BUF (home, positive) chart shows deep dip when BAL had momentum, then late surge to +24. 41-40 thriller clearly visible.
  - Suggested lines computed and displayed on play selection. Three concept chains rendered with BEST badge, KEP trajectory, and delta_kep deltas.
  - Eval bar animates correctly between plays (home positive = cyan fills right, away negative = orange fills left).
  - Nav link "⚗ Testing Lab" visible in header, routes correctly.

- **Key Design Decisions:**
  - **One-step KEP (not drive KEP):** Chosen because drive KEP's +17 for a 4th-down conversion was counterintuitive. One-step shows the immediate value of a play call, not the full drive. Analogous to chess showing the immediate move value, not the endgame.
  - **Home-team KEP reference:** `home_kep = kep_off if posteam == home_team else -kep_off`. Positive = home winning. Sign never flips on possession change — no sine waves. This is the only correct framing for a game-wide timeline chart.
  - **`receive_2h_ko` defaulted to 0.0:** Field not present in nfl_data_py; acceptable approximation for testing.

- **Current System Status:** Backend PID from prior session; verify still running before testing. All 18 pytest tests unaffected (no changes to tested code paths). Historical Lab page fully functional end-to-end with live backend.

- **Immediate Next Steps for the Next Agent:**
  1. Full UI review walkthrough with Cam (both sites, all tabs).
  2. Remove `HistoricalLab` from nav/routing before production launch (`pagesConfig.js` + `App.jsx`).
  3. Consider performance optimization for `get_game_plays` — currently re-computes KEP for all plays on every request (fine for testing, slow for scale).
  4. Consider depth-3 lines once depth-2 is validated as correct.

---

### [2026-06-22] Handoff from Claude Sonnet 4.6

- **Active Task:** Chess Evaluator — UI wiring, concept recommendations, multi-scenario preview
- **Files Modified:**
  - [`frontend_analysis/vite.config.js`](frontend_analysis/vite.config.js): Added `/api` proxy to `http://127.0.0.1:8000`. Previously missing, which caused concept recommendation fetches to fail in the preview browser and created a hardcoded-URL dependency. All frontend JS now uses relative `/api/` paths.
  - [`frontend_analysis/src/api.js`](frontend_analysis/src/api.js): Changed `API_BASE` from `'http://localhost:8000/api'` to `'/api'` to respect the vite proxy. Added two new mock games: `live_game_4` (MIA@BUF blowout, BUF +21) and `live_game_5` (LAC@KC 10-14pt game) with full `MOCK_PLAY_BY_PLAY`, `MOCK_GAME_STATS`, `MOCK_FOURTH_DOWNS`, and `MOCK_CHESS_EVALUATOR` entries.
  - [`frontend_analysis/src/pages/GameSummary.jsx`](frontend_analysis/src/pages/GameSummary.jsx): Three improvements: (1) Added `conceptResult`/`conceptLoading` state and a `useEffect` that fires `GET /api/positional-evaluator` when a chess play is selected. (2) Fixed `selectedEval` fallback to use last evaluation when `selectedPlayId` is null. (3) Fixed concept useEffect to use `?? evals[evals.length-1]` when `selectedPlayId` is null (race condition: tab opened before async data load completes). Added "PLAY CONCEPT RECOMMENDATION" ranked bar section to chess play detail panel.
  - [`frontend/src/pages/InDevelopment.jsx`](frontend/src/pages/InDevelopment.jsx): Removed all chess code (was dead code — no `id: 'chess'` page in pagesConfig.js). Cleaned up imports and state vars.
  - [`frontend/vite.config.js`](frontend/vite.config.js): Added `port: 5173, strictPort: true` to pin DFS site to correct port.

- **Verification Performed:**
  - SF@LAR (live_game_2): Chess tab shows KEP +5.44, concept recommendation loads live — Medium (39 sims) BEST at +6.063 delta_kep; all concepts positive (dominant position, flexibility doesn't matter much).
  - BUF@MIA (live_game_4): KEP ceiling +24.00; all five concepts show delta_kep = 0.000 — correct behavior, game is decided, no call matters.
  - Backend confirmed running: `GET /api/positional-evaluator` returns correct JSON from both `curl` and in-browser `fetch`.
  - Root cause of "Start the backend to see..." bug identified and fixed: `selectedPlayId` was null at tab-switch time due to async load race, so `ev` lookup returned undefined and early-returned before the fetch.

- **Decisions / Design Notes:**
  - KEP framing open question: should the chart show **play-start KEP** (current impl — KEP of the game state when the play begins) or **drive KEP** (the KEP of the full drive that started with this play)? See conceptual discussion in WORKLOG entry below for pros/cons.
  - Future idea logged: show **top "lines"** from sims like chess engines do — the chain of play concepts that produced the best KEP trajectory.

- **Current System Status:** All 18 tests still green (not re-run this session; no Python changes). Five mock games visible on analytics home. Chess evaluator + concept recommendation working end-to-end. Backend must be running (`python -m uvicorn src.api.app:app --host 0.0.0.0 --port 8000`) for live data.

- **Immediate Next Steps for the Next Agent:**
  1. Decide play-KEP vs. drive-KEP framing (see Cam's open question in this entry).
  2. Implement "top lines" display — show the sequence of drive concepts with best simulated KEP from each evaluation.
  3. Full UI review walkthrough when Cam is ready.
  4. Consider making the KEP chart clickable (currently Recharts dot clicks don't update state because native DOM events bypass React synthetic handlers — use a list of plays below the chart as the click target instead).

---

### [2026-06-21] Handoff from Claude Sonnet 4.6

- **Active Task:** Chess-Style Positional Evaluator — V1 implementation (Phase 1–5 complete)
- **Files Modified:**
  - [`src/nfl_sim/game_engine.py`](src/nfl_sim/game_engine.py): Added 6 `last_play_*` instance attributes and snapshot writes at end of `simulate_play_step()` to expose first-play concept classification to the evaluator.
  - [`src/nfl_sim/nfl_positional_evaluator.py`](src/nfl_sim/nfl_positional_evaluator.py): New file. Contains `KICKOFF_REFERENCE` constant, `KEPConverter` (WP-inversion via isotonic regression), and `PositionalEvaluator` (drive-rollout harness with snapshot-and-freeze lane isolation; returns `ep_start`, `kep_start`, per-concept `delta_kep`).
  - [`src/nfl_sim/models/positional_ep_v_0_1_0/train_positional_ep.py`](src/nfl_sim/models/positional_ep_v_0_1_0/train_positional_ep.py): XGBoost EP regression training script (val RMSE 0.3675, test RMSE 0.3665).
  - [`src/nfl_sim/models/positional_ep_v_0_1_0/positional_ep_inference.py`](src/nfl_sim/models/positional_ep_v_0_1_0/positional_ep_inference.py): `PositionalEPModelV010` inference wrapper with `predict_expected_points()` and `predict_batch()`.
  - [`src/nfl_sim/models/positional_ep_v_0_1_0/positional_ep_model.json`](src/nfl_sim/models/positional_ep_v_0_1_0/positional_ep_model.json): Trained XGBoost artifact (531KB).
  - [`src/nfl_sim/models/positional_ep_v_0_1_0/metadata.json`](src/nfl_sim/models/positional_ep_v_0_1_0/metadata.json): Version, features, target, and training metrics.
  - [`src/live/espn_adapter.py`](src/live/espn_adapter.py): Added `parse_plays_to_states()` — sibling of `parse_plays_to_fd_rows` that keeps all scrimmage plays (downs 1–4) for the play-stream endpoint.
  - [`src/api/app.py`](src/api/app.py): Added `_POSITIONAL_EVALUATOR` lazy singleton, `get_positional_evaluator()`, `DEFAULT_OFF_TEAM/DEFAULT_DEF_TEAM` constants; `GET /api/positional-evaluator` (slider tool) and `GET /api/games/{game_id}/positional-eval` (play-stream) endpoints; updated CORS to include both frontend ports (5173 + 5174).
  - [`frontend/src/pages/InDevelopment.jsx`](frontend/src/pages/InDevelopment.jsx): Chess section now wired live — sliders (down, distance, field pos, clock, score margin, timeouts) debounce-fetch `GET /api/positional-evaluator` on port 8000; renders real EP/KEP cards and per-concept `delta_kep` ranked bars with BEST badge.
  - [`frontend_analysis/src/api.js`](frontend_analysis/src/api.js): `MOCK_CHESS_EVALUATOR` replaced with per-play `evaluations[]` format matching real API; `getChessEvaluator` now calls `/api/games/{gameId}/positional-eval`.
  - [`frontend_analysis/src/pages/GameSummary.jsx`](frontend_analysis/src/pages/GameSummary.jsx): Chess tab redesigned — left panel shows KEP + EP dual-line Recharts timeline (clickable points select play); right panel shows selected play's positional detail with natural-language KEP interpretation.
  - [`tests/test_positional_evaluator.py`](tests/test_positional_evaluator.py): 18 unit tests across `PositionalEPModelV010`, `KEPConverter`, and `PositionalEvaluator` (monotonicity, range, drive-end rate, concept finiteness, clock-aware KEP ordering).

- **Verification Performed:**
  - `python -m pytest tests/test_positional_evaluator.py -v` → **18/18 passed** (8.1s)
  - Live API validated in prior session: slider endpoint returns correct EP/KEP/concepts; EP rises from 1.09 (midfield) to 5.61 (goal line); bad game_id → 502; live ESPN fetch verified via synthetic payload (Q4 late-game KEP = +14.99).

- **Current System Status:** All 18 tests green. V1 feature complete across all 5 phases. Frontend requires live backend on port 8000 to show live data (degrades gracefully to mock/error banner otherwise). No live NFL games currently so play-stream endpoint untestable end-to-end until season.

- **Immediate Next Steps for the Next Agent:**
  1. **Full UI review** — Cam wants to do a complete walkthrough once all parts are assembled. Load the strategy site (port 5174) and DFS site (port 5173) with the API running and verify the chess slider + GameSummary chess tab render correctly.
  2. **KEP absolute magnitude calibration** — noted as future work. Relative concept ranking is stable but absolute KEP values shift with clock calibration quirks in the WP model at t=3600. Address once the V1 full review is done.
  3. **AGENTS.md update** — the new files (`nfl_positional_evaluator.py`, `positional_ep_v_0_1_0/`, test file, two new API endpoints) should be added to the active-files section of AGENTS.md.
  4. **Season readiness** — when NFL season starts, smoke-test `GET /api/games/{live_game_id}/positional-eval` against a real ESPN PBP feed end-to-end.
  5. **CORS note** — default `ALLOWED_ORIGINS` in `app.py` now includes 5173 and 5174. If either frontend moves ports, update or set `ALLOWED_ORIGINS` env var.
