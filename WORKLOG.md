# WORKLOG.md
<!-- Reverse-chronological session ledger. Newest entry at top. -->
<!-- Format: ### [YYYY-MM-DD] Handoff from [Model] -->

---

### [2026-07-22] Handoff from Claude Sonnet 5 (sim input bug list closed out; 2026 Roster & DNA Blending Pipeline, Phases 0-6)

- **Active Task:** Two threads. (1) Closed out the last two open items on `docs/sims/inputs/README.md`'s known-bugs list (trench_tiers `LA`/`CIN` key issues). (2) Built the full 2026 season rollover: real 2026 rosters in the engine's format, plus a formal rule (agreed with Cam in conversation) for how a player's simulated stats blend recent form vs. season-long baseline vs. preseason projection as the season progresses. Plan written and approved via plan mode: `C:\Users\txcwa\.claude\plans\giggly-launching-thacker.md` (absolute path, outside the repo — referenced here for continuity, not a violation of the no-absolute-paths rule below).
- **Confirmed design (Cam's spec):** steady-state (game 6+): `stat = (2/3)*L4 + (1/3)*season_to_date`. Taper (games 1-5): projection weight 100/80/60/40/20% before games 1-5, actual-so-far fills the rest, zero projection weight from game 6 on. Rookies: usage fields (target_share/carry_share) and efficiency fields get independently-tunable parametric curves (ramp start/steady week, early/late value), since usage is coach-gated separately from real talent. Same formula/taper applies to team defense. Scope: offensive skill positions only (QB/RB/WR/TE) at the player level; defense is team-level only.

- **Files Modified/Created (grouped by phase):**
  - **Bug-list close-out (pre-Phase-0):** [`data/dna/trench_tiers_2025.json`](data/dna/trench_tiers_2025.json): `"LAR"` → `"LA"` (rename only, preserved existing curated values), added a missing `"CIN"` entry (tier fields are confirmed dead code; `qb_cpoe_z` is still live, set to an explicitly-flagged estimate). [`docs/sims/inputs/README.md`](docs/sims/inputs/README.md): bugs #1/#2 closed — **no open items remain on the list.**
  - **Phase 0 (engine year-param fix):** [`src/nfl_sim/game_engine.py`](src/nfl_sim/game_engine.py): two paths (`team_to_coach_2025.json`, `trench_tiers_2025.json`) were hardcoded to 2025 regardless of the `year` param the engine already accepted — now interpolate `self.year`. New [`data/dna/team_to_coach_2026.json`](data/dna/team_to_coach_2026.json) — built from two independently cross-checked live sources (Wikipedia + Gridiron Experts), not copied forward, because the *existing* 2025 file turned out to already be stale (Dallas/Vegas/New England/Jacksonville coaches were wrong even for 2025). New [`data/dna/trench_tiers_2026.json`](data/dna/trench_tiers_2026.json) (copy-forward, tier fields dead, `qb_cpoe_z` flagged as needing a real-2026-starter revisit).
  - **Phase 1 (career DNA rebuild through 2025):** [`scripts/roster_management/build_full_name_dna.py`](scripts/roster_management/build_full_name_dna.py): season range 2021-2024 → 2021-2025, metadata bumped to V.0.3.0. Rebuilt `qb_dna.json`/`rb_dna.json`/`wr_dna.json`/`te_dna.json`/`skill_dna.json`. **Found along the way:** `coach_dna.json` and `trench_dna.json`'s legacy fields (`def_pressure_rate`/`def_sack_rate`/`sack_rate_allowed` — still live inputs to the sacks-gate model) were *also* stuck on 2015-2024 — every 2025 sim had silently been using generic defaults instead of real team data for those three fields. Fixed via [`R/scripts/build_dna_registry_v_0_1_0.R`](R/scripts/build_dna_registry_v_0_1_0.R) (SEASONS extended to 2015:2025; restructured so it no longer touches `qb_dna.json`/`skill_dna.json` at all — those are now solely Python's — and writes `trench_dna.json`'s legacy fields to a staging file instead of overwriting the file directly) + new [`scripts/roster_management/merge_trench_dna_legacy.py`](scripts/roster_management/merge_trench_dna_legacy.py) (merges the staged legacy fields into `trench_dna.json` without disturbing the v0.4.0 composite fields already there for 2025, self-verifies before deleting the staging file). Also fixed a stale hardcoded `"2015-2024"` metadata string in the R script (now derives from `SEASONS`).
  - **Phase 2 (blending engine, pure functions):** New [`src/data_pipeline/rolling_stats_v_0_1_0.py`](src/data_pipeline/rolling_stats_v_0_1_0.py), [`src/data_pipeline/dna_blender_v_0_1_0.py`](src/data_pipeline/dna_blender_v_0_1_0.py), [`src/data_pipeline/rookie_curves_v_0_1_0.py`](src/data_pipeline/rookie_curves_v_0_1_0.py), [`tests/test_dna_blender.py`](tests/test_dna_blender.py) (27 tests). **Found and fixed:** first draft matched `nfl_data_py` PBP on `receiver_player_name`/`passer_player_name`, which is short-name form (`J.Chase`) — always empty against the full display names the rest of the pipeline uses. Fixed to match on `player_id` (gsis_id), same approach `build_full_name_dna.py` already uses. **Found and fixed a second field-naming mismatch:** rolling-stats used the DNA-file field names (`yac_per_reception`, `avg_target_depth_yds`); `current_rosters`' top-level schema actually uses `yac_per_rec`/`adot` (the DNA names only apply inside the nested `splits` sub-object) — would have silently broken the blend once Phase 5 tried to write results back.
  - **Phase 3 (2026 roster shell):** New [`scripts/roster_management/build_2026_rosters_v_0_1_0.py`](scripts/roster_management/build_2026_rosters_v_0_1_0.py). Pulls `nfl_data_py`'s 2026 rosters (QB/RB/WR/TE only), joins returning players against Phase 1's rebuilt career DNA, uses each returner's actual 2025 role as their 2026 projection basis, flags all 225 `entry_year: 2026` rookies with `draft_capital` rather than guessing a share for them. QA gate diffs the output against real round-1 draft picks and fails loudly on any miss — caught 8 real gaps in `nfl_data_py`'s roster feed (not a bug in the matching logic, verified directly against the raw source; both round-1 picks were backup-slot rookie QBs behind entrenched veterans) and self-heals by patching them in from the draft data. `scripts/roster_management/validate_rosters.py` generalized to take a year argument (was hardcoded to 2025). Rebuilt a second time later in the session to add `player_id` (needed for Phase 5's real-PBP join) and a frozen `preseason_projection` snapshot (so the taper always blends against a stable original baseline, not a re-derived one).
  - **Phase 4 (rookie projections):** New [`scripts/roster_management/build_rookie_projections_v_0_1_0.py`](scripts/roster_management/build_rookie_projections_v_0_1_0.py) → [`data/dna/rookie_projections_2026.json`](data/dna/rookie_projections_2026.json) (225 rookies, sorted by draft round). Every curve value is an explicitly-flagged placeholder for Cam to hand-edit. QB rookies get no `usage_curve` — a QB1 promotion is a status flip driven by real evidence, not a gradual ramp (see Phase 5).
  - **Phase 5 (weekly refresh):** New [`scripts/roster_management/refresh_weekly_dna_v_0_1_0.py`](scripts/roster_management/refresh_weekly_dna_v_0_1_0.py) — pulls real PBP through the completed week, blends per player, and includes starter-flip detection (compares real recent attempt/carry volume between same-position teammates, flips `status` when someone's clearly taken over — `game_engine.py`'s `_get_starter_static()` sorts by static career volume and has no other way to react to an in-season change). Fixed a real edge case found during testing: `nfl_data_py` returns a fully columnless frame (not just zero rows) when a season has no PBP at all yet, which crashed the pipeline before the fix.
  - **Phase 6 (team defense parity):** New [`scripts/roster_management/build_2026_trench_shell_v_0_1_0.py`](scripts/roster_management/build_2026_trench_shell_v_0_1_0.py) establishes `trench_dna.json["2026"]` (carries forward 2025, freezes a `preseason_projection`). Wired `refresh_team_defense()` into the Phase 5 script using Phase 2's `blend_team_dna()`. **Scoping note, not fully closed:** only the 3 simple-rate fields get weekly-refreshed — the v0.4.0 composite z-scores (`run_block_off_z` etc.) stay frozen at 2025 values all season; recomputing those weekly would mean re-running the full z-scoring methodology against a partial-season sample, materially more work than a rolling average, out of scope this session.
  - Not part of this work, pre-existing at session start and untouched: a large backlog of already-modified files (`docs/boxscores/*`, `docs/reports/*`, `pfr_adv_rushing_stats.csv`, `schedules_2015_2024.csv`, `app.py`, `batch.py`, `proe_overlay_v_0_1_0.py`, several model files) — flagged to Cam, not investigated or committed this session.

- **Verification Performed:** Every phase backtested against real data before being trusted, not synthetic-only. Phase 1: qb/rb/wr/te career stats spot-checked (Burrow/Stafford/Mahomes CPOE all sane, `seasons_observed` correctly bumped to 5); trench merge self-verified both legacy and composite fields coexist post-merge. Phase 2: rolling-stats validated against real 2025 Ja'Marr Chase and CIN defensive data. Phase 3: `validate_rosters.py` passes all 32 teams for both 2025 (regression) and 2026; no team missing any of the 4 positions. Phase 4: a real generated rookie entry run end-to-end through `resolve_rookie_curves()` → `blend_player_dna()` across games 1/3/5/6. Phase 5: blend math checked against real 2025 Chase/CIN data at every taper stage (exact match to hand-calculated values); starter-flip logic tested against real Joe Burrow 2025 data including his actual injury absence (weeks 3-12) — correctly stayed inert on zero real data rather than forcing a wrong flip, which surfaced during testing and validated the logic rather than exposing a bug. Full production script (`refresh_weekly_dna_v_0_1_0.py`) run end-to-end against 2026/week 0. `python -m pytest tests/test_dna_blender.py` — 27/27 passing throughout.
- **Current System Status:** All new/modified files from this session uncommitted as of this entry — Cam asked for WORKLOG update + commits in logical chunks next. No known regressions in touched files. Real 2026 season PBP doesn't exist yet (confirmed: `nfl_data_py.import_pbp_data([2026])` returns a 404/empty), so Phase 5/6's real-in-season-data paths are backtest-verified against 2025 but not yet exercised live — that will happen naturally once week 1 of 2026 is played and Cam runs the refresh by hand (scheduling automation deliberately deferred, see below).
- **Immediate Next Steps for the Next Agent:**
  1. Cam wants to discuss what data/projections belong in the repo vs. staying local-only (repo size + not exposing the exact sim-generation methodology) — raised at the very end of this session, not yet resolved. Don't assume an answer; pick up the conversation.
  2. Coordinator-level DNA for first-time 2026 head coaches (Minter, Brady, Monken, Schottenheimer, Coen, Kubiak, Hafley, Moore, Glenn — no `coach_dna.json` entry, no HC-level nflfastR history exists for them) is still open. Cam is planning to hand-supply team/year mappings for their prior OC/DC roles; I confirmed a team-season proxy is mechanically buildable from the same PBP source once he does. Not started.
  3. Weekly refresh (`refresh_weekly_dna_v_0_1_0.py`) is deliberately **not** automated/scheduled — Cam wants to run it by hand for at least the first few weeks of the season before considering the `schedule` skill's cron-based routine.
  4. Rookie curve values in `rookie_projections_2026.json` are 100% placeholders (position-baseline guesses, clearly flagged in the file's own metadata) — Cam said he needs to think more about the exact numbers, especially for delayed-role rookies. Not blocking, but don't treat the current values as real.
  5. The unrelated ~67K-line pre-existing backlog (see above) is still sitting uncommitted — Cam was offered help sorting/committing it separately and hasn't responded yet.

---

### [2026-07-21] Handoff from Claude Sonnet 5 (magic-numbers documentation pass, QB scramble yardage recalibration)

- **Active Task:** Direct follow-up to the same day's "Fragile / confusing cleanup pass" entry below — Cam asked what other numeric constants in `game_engine.py` were un/under-documented (beyond what that round already fixed), then gave a per-item disposition for each finding.
- **Files Modified/Created:**
  - [`scripts/eda/analyze_scramble_yardage.py`](scripts/eda/analyze_scramble_yardage.py) (new): measures real QB scramble yardage via `nfl_data_py`'s `qb_scramble` PBP column directly (already known reliable from this session's earlier fumble-rate EDA) — no heuristic needed despite Cam's initial assumption the data couldn't distinguish scrambles from designed QB runs. Output: [`docs/eda_outputs/qb_scramble_yardage/README.md`](docs/eda_outputs/qb_scramble_yardage/README.md).
  - [`src/nfl_sim/game_engine.py`](src/nfl_sim/game_engine.py): scramble yardage model `Normal(5, 4)` → **`Normal(7.0, 6.1)`**, matching the real 2020-2025 mean (6.995) and std (6.074) measured by the new script — previous model was undershooting the real mean by ~2 yards. Six documentation-only additions (no behavior change): return-yardage distribution params (kickoff/punt/interception/fumble) flagged as unverified against real data, deprioritized (rare plays, and Cam wants any future recheck to weight the clock-timing effect over exact yardage shape) but explicitly not dropped; QB time-to-throw distribution confirmed as intentional simplified design (not an oversight), with the deferred future idea of tying it to trench-matchup quality noted; the completion-probability model's separation-roll std and its 1-yard contested-vs-open cutoff flagged for future research (the cutoff is believed to match the community definition of a contested catch, unverified); the YAC noise formula's elusiveness/broken-tackle-rate coefficients (1.8, 6.0) flagged as an offseason investigation (unlike the formula's base_scale term, which already has a full Round 9 citation).
  - [`AGENTS.md`](AGENTS.md): added the un/under-documented-constants findings and per-item dispositions to §0, added the new script to §1's repo layout.
- **Verification Performed:** New EDA script run against real 2020-2025 PBP (n=6,230 scrambles). Smoke test (1000-game batch, `BatchSimulator`) after the scramble-yardage change: no NaNs, sane score distributions. Documentation-only edits carry no behavioral risk by construction (comments only).
- **Current System Status:** All changes uncommitted. No known regressions. Cam is separately rerunning `run_full_season_sim_2025.py` to refresh the stale parquet cache flagged in the prior entry (unrelated to this round's changes, but same outstanding item).
- **Immediate Next Steps for the Next Agent:**
  1. None of this round's 6 documentation-only flags are scheduled — they're intentionally deferred (return-yardage distributions: low priority, rare plays; QB TTT-trench tie-in: "sounds involved," on hold; separation/contested-catch verification: future research; YAC coefficients: offseason).
  2. If revisiting return-yardage distributions later, measure the clock-timing implication alongside yardage shape — that was Cam's stated priority, not yardage accuracy alone.
  3. Nothing has been committed this session — check with Cam before committing.

---

### [2026-07-21] Handoff from Claude Sonnet 5 (fragile/confusing cleanup pass — game_engine.py cleanup audit's remaining section)

- **Active Task:** Continuation of the same-day session (see the 2026-07-21 entry directly below this one for the trench-gates/bug-fixes/optimization-pass work that came first). This entry covers the audit's last untouched section: [docs/audit/game_engine_cleanup/README.md](docs/audit/game_engine_cleanup/README.md)'s "Fragile / confusing (not bugs, but worth cleaning up)" list, item by item.
- **Files Modified/Created:**
  - [`src/nfl_sim/game_engine.py`](src/nfl_sim/game_engine.py): `qb_fumble_rate` mislabeling confirmed already resolved by the earlier sack-fumble rework this session (no code change needed, just verified). Fixed two stale/inaccurate comments (a dead line-number citation; a misleading claim about how turnover-return TDs avoid double-handling — corrected to explain the real mechanism, `td_mask` never including them). `regular_clock`'s hand-maintained NOT-list (`~inc_mask & ~turnover_clock & ...`) replaced with an additive `clock_handled` mask each Phase 7 category ORs itself into — a future new category can't be silently forgotten anymore. Extracted 5 independently-duplicated `yardline_100<=5/<=20` zone-classification copies (one more than the audit's original count of 3 — also duplicated in the completion-probability model) into one `_classify_zone()` helper + named `GOALLINE_YARDLINE`/`REDZONE_YARDLINE` constants. Kickoff runoff now differentiates touchback (near-instant, was wrongly sharing the return's 4-7s range) from return (unchanged). Punt base runoff (touchback/fair-catch cases) `randint(4,7)` → `randint(7,10)`. FG runoff (both made and missed) now a flat 4s, replacing a made/missed asymmetry (4-6s vs 5-9s) real data doesn't support. `off_score`/`def_score` renamed to `away_score`/`home_score` throughout (see below).
  - [`src/nfl_sim/batch.py`](src/nfl_sim/batch.py), [`src/api/app.py`](src/api/app.py), [`scripts/simulation_runners/run_weeks_1_to_4_2025.py`](scripts/simulation_runners/run_weeks_1_to_4_2025.py), `run_weeks_1_to_18_2025.py`, `run_first_4_games_2025.py`, `run_full_season_sim_2025.py`, [`scripts/eda/diagnose_scoring_efficiency.py`](scripts/eda/diagnose_scoring_efficiency.py): `off_score`/`def_score` → `away_score`/`home_score` (was always away/home in this path, never real offense/defense). `src/live/` (`espn_adapter.py`, `posting_policy.py`) deliberately left alone — there the same field names correctly mean real, dynamic possession-based offense/defense, a different and legitimate concept; confirmed zero data/import crossover with the sim path before touching anything. `legacy/` also left alone (archival).
  - [`scripts/eda/analyze_kicking_timing.py`](scripts/eda/analyze_kicking_timing.py) (new): real 2021-2025 PBP EDA verifying kickoff touchback rate and kickoff/punt/FG live-play timing, same `game_clock_elapsed` (consecutive-play delta) methodology as `analyze_clock_pace_grid.py`. Output: [`docs/eda_outputs/kicking_timing/README.md`](docs/eda_outputs/kicking_timing/README.md) + 2 CSVs.
  - [`AGENTS.md`](AGENTS.md): added a resolved fragile-area note for the `off_score`/`def_score` rename (including the known residual risk below), updated the `_run_clock`-adjacent notes.
- **Key finding along the way:** the kickoff touchback rate constant (`0.2068`) turned out to already be correctly calibrated to real 2025 data (measured 576/2785 = 0.20682, matching to 4 decimals) — it just had zero citation. Real per-season rates confirm the new 2025 kickoff rule's effect is dramatic: 2021-2024 ranged 57.5%-73.0%, 2025 dropped to 20.68%.
- **Verification Performed:** Every pure-refactor change (`regular_clock`, zone-classification extraction, stale comments) verified via same-seed-twice determinism checks (bit-for-bit identical) — these are boolean-algebra-equivalent or non-functional changes, confirmed to produce zero behavioral drift. The kickoff/punt/FG timing changes and the sack-fumble/off_score work are real behavior changes, verified via smoke tests (multiple matchups, no NaNs/crashes, sane score distributions) rather than parity, as expected for intentional calibration changes. `py_compile` clean on all 8 touched Python files.
- **Current System Status:** All changes uncommitted (working tree, same as the earlier same-day entry). No known regressions. **Known residual risk (Cam is aware, planning to address shortly):** `data/interim/sim_results_2025_{games,players}.parquet` (gitignored, generated by `run_full_season_sim_2025.py`) was not regenerated by this session — if it predates this rename, it still has the old `off_score`/`def_score` column names on disk, which would break `app.py`'s `/api/week_projections` (or anything else reading that cache) until `run_full_season_sim_2025.py` is rerun.
- **Immediate Next Steps for the Next Agent:**
  1. Cam is rerunning `scripts/simulation_runners/run_full_season_sim_2025.py` shortly to refresh the now-stale parquet cache — don't duplicate that effort if picking this up soon after.
  2. A broader magic-numbers documentation pass is still open (Cam asked "what other numbers are un/underdocumented" as a live follow-up at the end of this session) — see this entry's companion investigation, not yet acted on.
  3. `game_sec`'s inverted-quarter representation (`(4-quarter)*900 + time_remaining`) was investigated and deliberately left as-is — it's a trained-model feature (Gate 1, 4th-down model, play-selection/air-yards/YAC/rush buckets all depend on this exact representation), already computed efficiently (once per step, not recomputed), and Cam confirmed the only real complaint (human readability of raw threshold checks like `game_sec <= 420`) isn't worth the change. Don't revisit without a new reason.
  4. Nothing has been committed this session — check with Cam before committing.

---

### [2026-07-21] Handoff from Claude Sonnet 5 (trench matchup gates, 9 critical bug fixes, dead-code cleanup, Gate 2b, game_engine.py optimization pass)

- **Active Task:** Multi-phase `game_engine.py` work, in order: (1) EDA-grounded trench (O-line/D-line) matchup mechanics for rushing, (2) a full code-review audit (task brief: [docs/audit/game_engine_cleanup/README.md](docs/audit/game_engine_cleanup/README.md)) covering 9 critical bugs + dead code + optimization opportunities, (3) fixing all 9 critical bugs, (4) dead/write-only code removal, (5) a pass-side trench mechanic (Gate 2b), (6) the audit's optimization-opportunities list, (7) a follow-up architecture fix to `_run_clock`. No single task brief covers the whole arc; this entry is the record.

- **Files Modified/Created (grouped by phase):**
  - **Rush trench gate:** [`scripts/eda/build_trench_dna_composites.py`](scripts/eda/build_trench_dna_composites.py) (new) — within-season z-scored run-block/run-defense composites into `data/dna/trench_dna.json`. [`scripts/model_training/measure_rush_organic_tail_rates.py`](scripts/model_training/measure_rush_organic_tail_rates.py) (new) — measures organic negative/explosive rush rates from real simulated games (gate forced to 0), used to calibrate a 3-way mode-frequency gate (`p_neg_gate`/`p_exp_gate`/`p_rescue_gate`) replacing the old flat `run_mult_*` multiplier in [`src/nfl_sim/game_engine.py`](src/nfl_sim/game_engine.py). `data/external/pfr_adv_rushing_stats.csv`/`schedules_2015_2024.csv` extended to include 2025 (were stale at 2024).
  - **9 critical bug fixes** (all in `game_engine.py`, see [docs/audit/game_engine_cleanup/README.md](docs/audit/game_engine_cleanup/README.md) for full detail on each): OT was unreachable (`self.quarter` could never hit 5) — added `is_playoff` param, full OT state, rewrote `_run_clock`'s end-of-quarter block with correct reg-season-vs-playoff timeout/period rules; sack probability missing a `np.clip` (could exceed 1.0); kneel-down mathematically unreachable on 4th down (`max_bleed_time` always ≤0 there) — added a `concede_mask` (42s/0-timeouts rule); a would-be TD could be silently erased by a same-play fumble roll; QB scrambles were invisible to fumble risk and the big-play counter; punt returns weren't re-clipped into valid field position (kickoffs were); return-yardage stats could disagree with actual field position (negative raw values floored for the stat but not the yardline update); Gate 2 fed duplicated last-4-games features (documented, deferred to v0.4.0); pass-branch penalty-decline logic was asymmetric vs. the run branch (fixed after confirming via real PBP data that declined penalties read `penalty=0` in the structured column). Also added safety detection (was entirely missing) as a prerequisite for the OT fix.
  - **Dead-code cleanup:** removed `drives_count`, `gross_sacks`/`net_sacks` (diagnostic-only), `fg_made_away/home`, `fourth_down_goes_away/home`, `oob_plays`/`oob_eligible_plays`, several write-only locals, `get_stats_report` (dead stub), `_load_skill_dna`'s unreachable fallback — all confirmed via grep-before-cut, not visual inspection.
  - **Gate 2b (pass-side trench mechanic):** [`scripts/eda/build_trench_dna_pass_composites.py`](scripts/eda/build_trench_dna_pass_composites.py) (new) — pass-block/pass-rush composites (`pass_block_off_z`/`pass_def_z`) into `trench_dna.json`, mirroring the run-side script. [`src/nfl_sim/models/chaos_v_0_1_0/train_gate2b_trench_correction.py`](src/nfl_sim/models/chaos_v_0_1_0/train_gate2b_trench_correction.py) (new) — trains a small XGBoost correction gate (3 features: Gate 2's own calibrated probability + the two new composites) that adjusts Gate 2's sack probability without retraining Gate 2 itself. [`src/nfl_sim/models/chaos_v_0_1_0/inference.py`](src/nfl_sim/models/chaos_v_0_1_0/inference.py): loads `gate_2b_trench_correction.joblib`, adds `predict_sack_proba_corrected`. `game_engine.py`: wired in after Gate 2's existing calibration, removed the old flat `pass_mult_*` trench-tier multiplier (kept the unrelated QB-CPOE component it was bundled with).
  - **`game_engine.py` optimization pass** (all 8 items from the audit's opportunities list): added `_precompute_matchup_tables()` (per-team/per-zone CPOE/target-share/carry-share/trench-feature tables, computed once instead of every play across 5 call sites — Phase 3 play-selection, sack-gate, air-yards, YAC, run-resolution) plus module-level `_zone_split_vector`/`_zone_scalar_vector` helpers; precomputed per-receiver DNA trait dicts (`receiver_traits`) replacing live `self.dna['skill']` lookups + a `get_safe_float` closure that was being redefined every play; `receivers_cache`/`rusher_cache` now store real `np.array`s instead of converting fresh every play; fumble mechanics reworked — deleted a per-carrier DNA `fumble_rate` multiplier that was mathematically always 1.0 (the field doesn't exist in any DNA file) and replaced the mislabeled `qb_fumble_rate` (actually QB sack-rate) sack-fumble scaling with a flat, real-data rate (`SACK_FUMBLE_RATE = 0.1253`, 2020-2025 PBP, Cam's call to use raw with no dampener); `_predict_4th_down_probas_batch`'s pandas DataFrame replaced with `booster.inplace_predict` in [`src/nfl_sim/models/fourth_down_conversion_v_0_1_0/inference.py`](src/nfl_sim/models/fourth_down_conversion_v_0_1_0/inference.py) — **found and fixed a real bug in passing**: this model (and Gate 2b, and Gate 4) were trained with early stopping but `inplace_predict` wasn't passing `iteration_range`, so production silently used more trees than what was validated (max divergence 0.058 probability for Gate 4); fixed all three via a stored `iteration_range` in `ChaosModelV010.__init__`. Small items: `is_offensive` now draws only `sum(has_penalty)` randoms instead of `N`; `_sigmoid_arr`/`_logit_arr` hoisted to module level.
  - **`_run_clock` architecture** (tabled during the optimization pass, done as a follow-up): consolidated 9 disjoint per-category clock calls in Phase 7 into one combined call (`combined_seconds` array written per-category, one `_run_clock` call at the end) — cut redundant full-N overhead but revealed `_run_clock`'s internal OT-entry coinflip reordering relative to other randomness. Fixed properly: split `_run_clock` into pure runoff+2-minute-warning mechanics, and moved all quarter/OT-period-transition logic (including the coinflip) into a new `_resolve_quarter_transitions(active)` method called exactly once per step via a new `self.step_start_active` snapshot — restores full seeded determinism.
  - Also modified this session (not detailed above — see `git diff` for specifics): `src/api/app.py`, `src/nfl_sim/proe_overlay_v_0_1_0.py`, `src/nfl_sim/models/positional_ep_v_0_1_0/*`, `data/dna/coach_dna.json`, and several docs (`docs/api/local.md`, `docs/models/README.md`, `docs/sims/inputs/README.md`, `docs/study/ML_Concepts.md`).

- **Verification Performed:**
  - Rush gate: before/after bucket comparison (real team-games bucketed by matchup composite vs. real EDA targets).
  - 9 bug fixes + OT: dedicated verification round per bug plus a full-batch smoke test; the `~self.is_playoff` fancy-indexing bug (bitwise-not on a Python bool silently corrupting lane 0 every quarter boundary) was found via manual debug-print tracing and confirmed with `~True == -2` in isolation.
  - Gate 2b: training-time brier/logloss/PR-AUC + a bucket-level real-vs-predicted sack-rate table (confirms correction concentrated at matchup extremes, masked by the aggregate metric); live-engine sack rate checked post-deploy.
  - Optimization pass: items 1-3/5/6 verified via **exact seeded parity** (identical aggregate stats — pYds, rYds, sacks, fumbles, win totals — across a 3000-game batch, same seed, before/after each change). Item 4 (fumble mechanics, a real behavior change) verified via rate statistics instead (general rate unchanged as expected, new sack-fumble rate lands near 12.53%).
  - `_run_clock` consolidation: exact seeded parity failed as expected (internal coinflip reordering) — validated instead via large-N statistical A/B (pooled 40k vs. 60k games): tie-rate and OT-rate differences both consistent with sampling noise (p=0.37, p=0.21). The follow-up split restored **exact bit-for-bit determinism** (verified: same seed run twice → identical output) and was re-validated statistically against the consolidated-only baseline (p=0.35, p=0.95).
  - Wall-clock timing measured throughout: modest (~3.5%) improvement from items 1-6; `_run_clock` itself measured at only ~1% of total game time even fully consolidated, so its contribution to overall speed was never going to be large — kept for correctness/clarity, not performance.

- **Current System Status:** All changes uncommitted (working tree). No crashes, no NaNs in batch output, OT/ties resolve correctly, full determinism restored. Known, deliberately-unaddressed gaps (flagged to Cam, not fixed): general (non-sack) fumble rate is calibrated below the raw real per-play rate (~0.5% sim vs. ~1.1-1.5% real for run/completion/scramble) — untouched since it wasn't in scope and the history behind the existing `*0.80` calibration constant isn't known; overall sack rate runs ~5-15% hot vs. real depending on matchup (pre-existing, not caused by Gate 2b — confirmed via a bypass test showing Gate 2b mildly *improves* it).

- **Immediate Next Steps for the Next Agent:**
  1. Nothing currently blocked. Natural next candidates, none urgent: recalibrate the general fumble-rate base constants against real per-play rates; investigate the ~5-15% sack-rate overshoot (candidate: retrain Gate 2 itself, previously rejected mid-session to avoid disturbing its verified calibration, but may be worth revisiting given the drift found this session).
  2. Nothing has been committed yet — Cam has not asked for a commit/PR this session; check with him before committing this much surface area at once.
  3. Update `AGENTS.md` to reflect: OT/safety now implemented (was previously "unreachable" per the old audit), Gate 2b live, the trench-tier `pass_mult`/`run_mult` mechanism fully retired, and the `_run_clock`/`_resolve_quarter_transitions` split (companion task to this WORKLOG entry, in progress).

### [2026-07-15] Handoff from Claude Opus 4.8 (full repo audit — Python 3.12 upgrade, first git history, docs, dead-code cleanup)

- **Active Task:** Full-scale repo audit requested by Cam: the entire working tree (Python backend, both frontends, docs, trained models, R live bot) had never been committed to git — nothing was pushable. Landed as three merged PRs: [#1](https://github.com/txcwalker/NFLSims/pull/1) (environment + first commit of everything), [#2](https://github.com/txcwalker/NFLSims/pull/2) (module docstrings + READMEs), [#3](https://github.com/txcwalker/NFLSims/pull/3) (dead-code cleanup). No dedicated task-brief doc exists for this; this entry is the record.

- **Files Modified/Created (grouped — this touched most of the repo, first-ever commit for most of it):**
  - **Environment:** [`.gitignore`](.gitignore) rewritten (scoped `data/`, un-ignored `tests/`, fixed `venv/` mismatch, excluded legacy data/binaries, OneDrive sync-conflict dupes, runtime bot output); new [`.gitattributes`](.gitattributes) (LF normalization); new [`requirements.txt`](requirements.txt) pinned to a validated Python 3.12 set; [`README.md`](README.md) corrected (Python 3.10+ → 3.12).
  - **`venv/`:** rebuilt Python 3.8.10 → 3.12.13 (old env kept as `venv_py38_old/`, not yet deleted).
  - **`src/`** (49 files): first commit — engine, API, live bot, data pipeline.
  - **`src/nfl_sim/models/`** (103 files): first commit of all 11 trained model families; every `.joblib`/pickle re-saved under the new xgboost 3.3.0 / scikit-learn 1.9.0 (predictions verified byte-identical to pre-resave — no drift).
  - **`scripts/`, `tests/`** (51 files): first commit of tooling + test suite.
  - **`frontend/`, `frontend_analysis/`** (57 files): first commit of both React sites.
  - **`docs/`, `.agents/`, `ai_assistant_files/`** (300 files): first commit of the documentation tree.
  - **`legacy/`** (28 code files; data/binaries ~2.35GB deliberately gitignored): archived old R/Python implementation.
  - Root planning docs ([`AGENTS.md`](AGENTS.md), `GOAL_TRACKER.md`, `PROJECT_ROADMAP.md`, etc.) and `data/dna`, `data/current_rosters`, `data/external` (curated inputs): first commit.
  - **`R/`** (root): reconciled as **active** code (it's the live 4th-down bot, not deprecated — see AGENTS.md §2 correction below), merging cleanly with Cam's concurrent "Prepping Actions" commits on `main`. Retired a superseded `predict_fd.R`/`load_fd_model.R` pair to `legacy/models/fourth_down/`. Fixed [`nfl_manuel.yml`](.github/workflows/nfl_manuel.yml)'s CI step ordering.
  - [`src/nfl_sim/game_engine.py`](src/nfl_sim/game_engine.py): removed a confirmed-dead DNA-injection block (4 lines; set attributes nothing read).
  - `legacy/models/air_yards_v_0_1_1/{train.py,evaluate.py}`, `legacy/models/yac_model_v_0_1_1/{train.py,evaluate.py}`: moved here (via `git mv`, history preserved) from the active model dirs — these are superseded single-model trainers that would silently corrupt the deployed zone-split models if run; each now has a `RETIRED` header pointing to the real `train_zone_split.py`.
  - 10 `src/` files gained module-level docstrings (`game_engine.py`, `model_registry.py`, `batch.py`, `visuals.py`, `script_chainer.py`, `api/app.py`, and 4 `data_pipeline/` files); 3 new READMEs (`tests/`, `frontend/src/`, `frontend_analysis/src/`).
  - [`AGENTS.md`](AGENTS.md) §2: corrected the stale "R deprecated" claim (root `R/` is the live bot) and the "legacy/ do not touch" claim (`batch.py` actively imports `legacy.game_engine_sequential`).

- **Verification Performed:**
  - `python -m pytest tests/test_positional_evaluator.py -v` → 18/18 passing, both before and after every change batch.
  - `python -m py_compile` on every edited engine/API file.
  - Full-season batch audit (`run_weeks_1_to_4_2025.py`) run on Python 3.8 vs. 3.12 — every tracked metric matched within ~0.4% (Monte Carlo noise, no systematic drift); completion rate matched to 0.016% specifically, ruling out silent model-version corruption.
  - Direct engine smoke test (`NFLGameEngine(...).simulate_play_step()` × 300) — reaches Q4 cleanly on the rebuilt venv.
  - R live-bot smoke test: piped a real 4th-&-2 game state through `Rscript run_one_sim.R` — loaded `.rds` models and returned a sensible GO/punt/FG recommendation end-to-end.
  - Secrets scan on every commit before push (no hardcoded credentials; bot creds are all env-sourced via `os.getenv`).

- **Current System Status:** All tests passing. PRs #1, #2, #3 merged into `main`; local repo clean and synced; feature branches deleted (local + remote). Active venv is Python 3.12.13 per `requirements.txt`. `venv_py38_old/` retained as a fallback, not yet deleted. Both frontends, the FastAPI backend, and the R live bot all verified working post-merge.

- **Immediate Next Steps for the Next Agent:**
  1. **In progress / up next (Cam's explicit request):** decide the fate of `MODEL_DEVELOPMENT_STANDARD.md` — referenced by `PROJECT_ROADMAP.md`/`GOAL_TRACKER.md`/`AGENTS.md` as the model-audit gate but does not exist in the repo. Either author it or strip the dangling references.
  2. **Function-level docstrings** (Inputs/Outputs/Purpose per Cam's global standard) across `src/` — deliberately deferred to be done **case-by-case**, not as one large pass. `optimizer.py` (10 functions, 0 documented) is a representative gap.
  3. Delete `venv_py38_old/` once Python 3.12 is fully trusted in daily use.
  4. `HistoricalLab` (frontend_analysis) is still marked "remove before launch" — not urgent, but a real pre-launch task.
  5. A code-quality/performance optimization pass on `src/` (especially `game_engine.py`) has **not** started — this audit was scoped to reorg + docs + dead-code only.

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
