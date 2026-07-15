# Sim Input Reference

<!-- Status: live | 2026-07-13 -->

## Context

This doc catalogs every input that feeds the Monte Carlo engine (`src/nfl_sim/game_engine.py`), where each one comes from, and — critically — **what actually happens when you change it**. That last part is not uniform. A handful of inputs are direct 1:1 model features (change the number, the model sees the new number). Most of the user-facing sliders are not: they're post-processing overlays applied *after* the underlying Monte Carlo run, which behave very differently from what "adjusting an input" usually implies. This doc exists so nobody (human or AI) has to re-derive that distinction from scratch — see [§1](#1-how-to-read-this-doc--behavior-categories) before trusting any slider to do what its label suggests.

Scope: `src/nfl_sim/game_engine.py`'s live vectorized `NFLGameEngine` path (what `src/api/app.py` actually calls via `BatchSimulator.run_batch(vectorized=True)`, the only mode the API ever uses) — not the legacy `SequentialNFLGameEngine`, and not `model_registry.py`'s `predict_*()` wrapper methods, which are dead code in the live path (the engine reaches into the raw boosters directly and builds its own feature vectors inline).

Built from a full-repo audit on 2026-07-13. Re-verify against the code before relying on any "unused" claim below after future engine changes — this is a snapshot, not a live-generated doc.

---

## 1. How to Read This Doc — Behavior Categories

Every input in [§3](#3-user-facing-input-surface) is tagged with one of these:

| Tag | Behavior | Practical implication |
|---|---|---|
| **A — Direct DNA feed** | The value is written into `sim.rosters`/`sim.dna` in memory and becomes an actual model input feature or sampling weight inside `game_engine.py`. | Only takes effect if it triggers a genuine **live** Monte Carlo rerun (`bypass_cache=True` in `run_simulation()`). On a cached/prepopulated baseline view, changing this value has **zero visible effect** until "Run Engine" actually fires a live simulation. |
| **B — Linear scaling overlay** | Applied *after* the Monte Carlo run completes. Multiplies every player's already-simulated counting stats (yards, attempts, TDs, etc.) by a uniform ratio. | Does not change *what* the engine simulated — a "64 plays" pace setting doesn't make the engine run 64 plays, it takes whatever the engine already produced and scales it by `target_plays / actual_simulated_plays`. |
| **C — Redistribution overlay** | Applied after the run. Takes a team-level total that's already fixed by the simulation and reallocates *credit* for it among players according to the requested shares (normalized), falling back to workload-proportional if no share given. | The team's total (e.g. total rushing TDs) is **not** under user control this way — only who gets credit for it. |
| **D — Importance-reweighting overlay** | Applied after the run. Computes a Gaussian-kernel weight per simulated iteration based on how close that iteration's outcome is to the requested target, then reports weighted averages/percentiles across the *existing* iterations. | Does not generate a new outcome at exactly the requested line — it re-weights the ~1,000 already-simulated worlds toward the ones that already looked like that scenario. Directional, not deterministic. |
| **E — Cosmetic only** | Never touches `sim.rosters`/`sim.dna`. Pure display/downstream-metric math. | Changing it has no effect on any simulated number, ever — it only affects a derived display field (e.g. ownership leverage). |
| **F — Sent but not exposed** | Exists in the API payload/model with a DNA-derived default, but the frontend has no control for it. | Currently only changeable by editing the API request directly (e.g. via a script or curl) — not reachable through the Simulator UI. |

---

## 2. Non-DNA Baseline Inputs (not user-editable)

| Input | Source | Feeds |
|---|---|---|
| `spread_line`, `total_line` | `data/external/schedule_2025.csv` (Vegas odds feed) | The baseline "Vegas Market Line" shown for every game; the reference point that `spread_override`/`total_override` are measured against |
| `away_team`/`home_team`, `gameday`/`gametime`/`weekday` | Same schedule CSV | Game identity, slate-tagging (DK/FD main vs. showdown-only) |
| `year` | Request param, defaults 2025 | Which season's roster/DNA files load |
| `iterations` | Request param (UI: "Sim Iterations" dropdown, 1,000/5,000/10,000) | **Only matters on a genuine live rerun** (`sim.run_batch(iterations=N)`). Ignored entirely on a cache-hit — cached results reflect however many iterations the parquet was built with (currently 1,000, see `AGENTS.md` §10) |
| `apply_weighting` | Inferred by caller, not user-facing as a toggle | Gates whether category **D** (importance reweighting) applies at all — `false` for baseline/prepopulation calls, `true` for the individual-game "Run Engine" and full-slate flows |

---

## 3. User-Facing Input Surface

Everything below is exposed somewhere in `frontend/src/pages/Simulator.jsx`'s `GameSimulatorWorkspace` — the **only** page in the app that can influence a sim rerun (`DfsSummary.jsx` is read-only over `allSimResults`; `Optimizer.jsx`'s inputs — lineup count, salary cap, exposure, entry fee — operate on already-simulated projections and are a separate downstream stage, not sim inputs, so they're out of scope here).

### 3.1 Team-level ("Playbook Overrides" panel)

| UI control | Backend field | Category | What it actually does |
|---|---|---|---|
| Pace (Plays per Game) slider | `team_overrides[team].plays_per_game` | **B** | `pace_factor = target_plays / avg_simulated_plays`; every volume stat (`pAtt, pCmp, pYds, int, rAtt, rYds, rTD, recTD, targets, rec, recYds, fumbles, sacks_taken`) for that team's players gets multiplied by this factor |
| Pressure Rate % slider | `team_overrides[team].def_pressure_rate` | **A** | Feeds the sacks-gate (Gate 2) XGBoost model as a direct input feature (defense's pressure rate) — only live on a rerun |
| PROE % slider | `team_overrides[team].proe` | **A**, but via a dedicated mechanism | Not read directly by `game_engine.py`'s `self.dna`. Injected into a **separate global overlay module** (`src/nfl_sim/proe_overlay_v_0_1_0.py`) via a `ProcessPoolExecutor` worker initializer (`worker_init()` in `app.py`), converted to a logit-space offset (`_proe_to_logit_offset`), and applied to the base pass/run play-selection probability. Only live on a rerun. |
| ~~*(not exposed)*~~ | ~~`team_overrides[team].deep_shot_rate`~~ | **cut 2026-07-13** | Removed from `TeamOverride`, `get_rosters()`, and `run_simulation()` entirely — was silently corrupting the coach's real DNA value on every live rerun due to bug #3 below, and (discovered afterward, see bug #9) never actually reached the deployed air-yards model in the first place. `coach_dna.json`'s real per-coach value is untouched now that nothing can overwrite it. |
| ~~*(not exposed)*~~ | ~~`team_overrides[team].conservative_score_bias`~~ | **cut 2026-07-13** | Same cut, same reasoning as `deep_shot_rate` above. |

### 3.2 Player-level ("Adjust Workloads" table)

| UI column                    | Backend field                        | Category | What it actually does                                                                                                                                                                                                              |
| ---------------------------- | ------------------------------------ | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Target Share                 | `player_overrides[i].target_share`   | **A**    | Overwrites `sim.rosters[team][name]['target_share']` — this is the actual sampling weight the engine uses to randomly pick which receiver is targeted on every pass play, plus a model-feature input. Only live on a rerun.        |
| Rushing Share                | `player_overrides[i].carry_share`    | **A**    | Same pattern — actual carry-selection sampling weight plus model feature. Only live on a rerun.                                                                                                                                    |
| Rushing TD Share             | `player_overrides[i].rush_td_share`  | **C**    | Team's total simulated rushing TDs is unchanged; this only reallocates which player gets credit for them (normalized against all players' `rush_td_share` on that team, falling back to rush-workload-proportional if none is set) |
| Rec TD Share                 | `player_overrides[i].rec_td_share`   | **C**    | Same redistribution mechanism as above, for receiving TDs                                                                                                                                                                          |
| Ownership %                  | `player_overrides[i].ownership_proj` | **E**    | Never touches the simulation. Purely feeds `ownership_leverage = dk_points / ownership_proj`, a DFS-strategy display metric only                                                                                                   |
| *(not exposed)*              | `player_overrides[i].catch_rate`     | **F**    | Written into `sim.dna['skill'][name]['catch_rate']` on a live rerun — feeds the zone catch-rate baseline. Always sent at its DNA-baseline default; no UI control exists to change it                                               |
| ~~*(sent, functionally inert)*~~ | ~~`player_overrides[i].td_share`~~ | **cut 2026-07-13** | Was a legacy combined field (`rush_td_share + rec_td_share`), never read by `run_simulation()` — only the split `rush_td_share`/`rec_td_share` fields ever mattered. Removed from `PlayerOverride` and all payload-construction sites. `get_rosters()`'s **display-only** `td_share` column (the roster table) is unrelated and still present. |

### 3.3 Game-level scenario controls

| UI control | Backend field(s) | Category | What it actually does |
|---|---|---|---|
| Vegas/Sim Spread & Total scenario buttons (Standard / Shootout ±10 / Defensive ∓10) | `spread_override`, `total_override` | **D** | See §1 — Gaussian importance-reweighting of existing iterations, not a forced outcome |
| "Custom Implied Team Totals Override" (per-team implied score text boxes) | Same `spread_override`/`total_override`, back-derived from the two typed-in scores (`handleImpliedScoreChange`) | **D** | UI sugar over the same mechanism above — typing team scores just computes the equivalent spread/total and feeds it through the identical reweighting overlay |

---

## 4. DNA & Roster Data Inventory

Ten files in `data/dna/` plus 32 per-team files in `data/current_rosters/{TEAM}_traits_{year}.json`. Full field-by-field consumption map:

### 4.1 `trench_tiers_2025.json` — `{TEAM: {run_block_tier, pass_block_tier, run_stuff_tier, pass_rush_tier, qb_cpoe_z}}`

All 5 fields are live: `run_block_tier`/`run_stuff_tier` → static run-blocking multiplier at init; `pass_block_tier`/`pass_rush_tier` → static pass-protection multiplier at init; `qb_cpoe_z` → folds into the pass multiplier at init *and* is re-read live as a "box density" feature for the rush-yards model.

### 4.2 `coordinator_atlas.json` — `{off_proe: {coach: float}, def_proe_allowed: {coach: float}}`

`off_proe` is the source of truth for the PROE overlay (§3.1). `def_proe_allowed` is tracked but **never consumed anywhere in Python**.

### 4.3 `team_to_coach_2025.json` — `{TEAM: "Coach Name"}`

Pure join key between team abbreviation and every coach-indexed dataset. Fully consumed.

### 4.4 `coach_dna.json` — per-coach `{air_yards_tendency, deep_shot_rate, screen_rate, play_action_rate, no_huddle_rate, rpo_rate, conservative_score_bias, seasons_observed, total_pass_plays}`

**Correction (2026-07-13):** all 7 rate fields are built into per-play arrays (`coach_ay_tend`, `coach_deep`, `coach_screen`, `coach_pa`, `coach_no_huddle`, `coach_rpo`, `coach_cons`) right before the air-yards model call — but **none of them are actually passed to it**. Confirmed against the deployed model's own `metadata.json` (`src/nfl_sim/models/air_yards_v_0_1_1/metadata.json`): its real feature list is `[down, ydstogo, yardline_100, score_differential, game_seconds_remaining, cpoe_by_filter, target_share_by_filter, carry_share_by_filter]` — 8 features, no coach traits at all. The 7 arrays are dead code, leftover from an earlier (richer) model architecture that was simplified during a later retrain without removing the now-orphaned computation — see bug #9. `seasons_observed`/`total_pass_plays` are separately metadata-only, unused.

### 4.5 `trench_dna.json` — per-season, per-team `{sack_rate_allowed, avg_air_yards, def_pressure_rate, def_sack_rate, blitz_rate, avg_ay_allowed, off_pass_block_win_rate, times_to_pressure_sec}`

Only `def_pressure_rate`, `def_sack_rate`, `sack_rate_allowed` are live (all three feed the sacks-gate model). `avg_air_yards`, `avg_ay_allowed` are unused; `blitz_rate`, `off_pass_block_win_rate`, `times_to_pressure_sec` are documented in the file's own metadata as always-empty placeholders.

### 4.6 `qb_dna.json` — per-QB `{cpoe, avg_air_yards_per_att, deep_ball_rate, scramble_rate, sack_rate, play_action_rate, under_pressure_cpoe, pressure_rate, seasons_observed, total_attempts, avg_time_to_throw_sec, ngs_aggressiveness_index, splits.{primary,redzone,goalline}.{cpoe,sack_rate,play_action_rate}}`

`cpoe` (and its zone-split variants, via `cpoe_by_filter`) is the single most-wired trait in the whole system — feeds completion probability (both contested and open-field paths), play-selection, sacks-gate, interception-gate, air-yards (one of its real 8 features — see the correction in §4.4), and YAC models. `total_attempts` drives starter-QB selection. `avg_time_to_throw_sec` seeds the per-play time-to-throw sampling distribution (`play_ttt = clip(Normal(avg_time_to_throw_sec, 0.6), 1.5, 4.5)` — a genuine per-QB-median, per-play-random-sample mechanism) and feeds the sacks-gate (Gate 2) model as a direct feature.

**Correction (2026-07-13):** `avg_air_yards_per_att`, `deep_ball_rate`, `play_action_rate`, `under_pressure_cpoe`, `ngs_aggressiveness_index`, and `sack_rate` (this file's copy) were previously documented here as air-yards model features — they are not. Same dead-code pattern as §4.4's coach fields: built into arrays (`ay_q_avg_air`, `ay_q_deep`, `ay_q_pa`, `ay_q_pressure`, `ay_q_agg`, `ay_q_sack`) right before the air-yards call, never actually included in the 8-feature vector the deployed model consumes. `sack_rate` still genuinely feeds the fumble-probability multiplier — that part was correct. **Notably, `play_ttt` (the per-play sampled time-to-throw) is not connected to air yards either** — despite the "more time to throw → deeper routes develop" intuition, it currently only affects the sacks-gate model. See bug #9. `seasons_observed`, top-level `pressure_rate`, and the split-level `sack_rate`/`play_action_rate` are unused.

Note: `scramble_rate` in this file is also dead for air-yards (same reason); the actual scramble-vs-sack dice roll at runtime reads the separate roster-file copy (§4.8).

### 4.7 `rb_dna.json` / `wr_dna.json` / `te_dna.json` / `skill_dna.json`

Identical schema (`skill_dna.json` is a byte-identical union of the other three). Per-player `{position, target_share, carry_share, air_yards_share, catch_rate, avg_target_depth_yds, yac_per_reception, elusiveness, broken_tackle_rate, ypc, deep_target_rate, route_profile, seasons_observed, total_targets, total_carries, avg_separation_yds, avg_cushion_yds, top_speed_mph, efficiency, percent_attempts_gte_eight_defenders, avg_time_to_los, rush_yards_over_expected_per_att, rush_pct_over_expected, contested_catch_rate, splits.{primary,redzone,goalline}.{target_share,carry_share,catch_rate,yac_per_reception}}`.

Live: `target_share`/`carry_share` (and zone splits, via `target_share_by_filter`/`carry_share_by_filter`) feed the real air-yards model features (the roster-file copies, not these, drive the actual sampling weight — see §4.8); `catch_rate` and its zone splits anchor the zone completion-probability baseline; `avg_target_depth_yds` calibrates the receiver-offset term in the completion model (both the open-field and, as of 2026-07-13, contested paths); `avg_separation_yds` drives the per-play contested-vs-open roll; `contested_catch_rate` is the base probability on the contested branch, also now depth-tapered; `elusiveness`/`broken_tackle_rate` scale YAC noise variance.

**Correction (2026-07-13):** `deep_target_rate`, `air_yards_share`, `yac_per_reception` were previously documented here as air-yards features — they are not. Same dead-code pattern as §4.4/§4.6: built into `deep_target_rate_recv`/`air_yards_share_recv`/`yac_per_reception_recv` arrays, never included in the real 8-feature air-yards vector. See bug #9.

Unused or broken: `position` is read under the wrong key (`'pos'`) so it's effectively dead; `route_profile`, `seasons_observed`, `total_carries`, `avg_cushion_yds`, `top_speed_mph`, `efficiency`, `percent_attempts_gte_eight_defenders`, `avg_time_to_los`, `rush_yards_over_expected_per_att`, `rush_pct_over_expected` are all unused. `ypc` from this file specifically is unused (a *roster*-file `ypc` is read instead — see §4.10 — and even that turns out to be a dead computation).

**Referenced but absent from the data:** code reads `carrier_dna.get('fumble_rate', 0.015)` for the in-game fumble multiplier — no file in this family has a `fumble_rate` field, so this silently always returns the `0.015` default and the fumble multiplier is always exactly `1.0`.

### 4.8 `data/current_rosters/{TEAM}_traits_{year}.json` — per-player, per-team, weekly snapshot

Mirrors the relevant DNA-file schema per position, plus `pos`/`status` and (for receivers) `adot`/`yac_per_rec`. This is the *live, per-game* copy distinct from the career-long DNA-file averages.

**This is the copy the engine actually samples from for two of the most important mechanics:**
- `target_share` → the real probability weight for who gets targeted on a pass play (not the DNA-file copy)
- `carry_share` → the real probability weight for who gets the ball on a run play (not the DNA-file copy)
- `scramble_rate` → the real QB scramble-vs-sack roll (not `qb_dna.json`'s copy)
- `status` → gates eligibility (`'active'` only) for being targeted/carrying/starting; mutated in-memory when a user picks a specific starter in the DFS slate UI

Also live: `pos` (stat-report tagging, sampling-pool filters, slot-map construction).

**Dead computations found:** `ypc` → `efficiency_z`, and two fields that don't exist in any roster file (`yac_per_att`, always defaults to `2.0`; `btk_rate`, always defaults to `0.10`) → `yac_att_z`/`btk_rate_z` — all three z-scores are computed per-rusher but **never appear in the feature stack actually passed to the rush-yards model**. Every other field in the roster copy (`adot`, `cpoe`, `sack_rate`, `pressure_rate`, `splits`, etc.) is present in the JSON but ignored in favor of the DNA-atlas copy of the same trait.

---

## 5. Known Bugs & Data Gaps Found During This Audit

Flagging these here since they surfaced directly from tracing "what actually feeds the sim" — worth a deliberate decision on each rather than silent defaulting:

1. **Rams unreachable in trench tiers.** `trench_tiers_2025.json` keys the Rams as `"LAR"`; every other file/roster in the system uses `"LA"`. The Rams' actual tier data is never looked up — always falls back to the hardcoded default (3.0/3.0/3.0/3.0/0.0).
2. **Bengals missing from trench tiers entirely.** No `"CIN"` entry in `trench_tiers_2025.json` — same all-default fallback.
3. **`coordinator_atlas.json` field-name mismatch.** `app.py` reads `off_deep_shot_rate`/`off_conservative_score_bias` from this file, but it only ever contains `off_proe`/`def_proe_allowed`. Both lookups always return `{}` and silently fall back to hardcoded defaults (0.12, 0.0) in the `/api/rosters` display. The real values live in `coach_dna.json`, not here.
4. **`coach_dna.json` has no `proe` field**, but `app.py`'s override-detection comparison (`abs(settings.proe - coach_dna.get("proe", settings.proe))`) checks against one — always evaluates to `0`, meaning a PROE-only slider change can't by itself trigger the cache-bypass check (the other three team settings compared on the same lines work correctly).
5. **`fumble_rate` doesn't exist in the skill-DNA files** — the in-game fumble-probability multiplier always defaults to `1.0` regardless of ball-carrier.
6. **`yac_per_att`/`btk_rate` don't exist in roster files**, and even the resulting z-scored features (`yac_att_z`/`btk_rate_z`) are computed and then never used — dead data *and* dead computation stacked on top of each other.
7. **`position` field read under the wrong key** (`'pos'` instead of `'position'`) in the skill-DNA lookup — always falls through to the roster's own `pos` or a hardcoded `'WR'` default.
8. **`self.coach_aggression`** (derived from `coach_dna.json`'s `deep_shot_rate`) is computed once at engine init and never read again — vestigial.
9. **A much bigger dead-code block, found 2026-07-13 while scoping a time-to-throw feature request.** Right before every air-yards model call, `game_engine.py` builds **7 per-play QB arrays** (`ay_q_avg_air`=`avg_air_yards_per_att`, `ay_q_deep`=`deep_ball_rate`, `ay_q_scramble`=`scramble_rate`, `ay_q_sack`=`sack_rate`, `ay_q_pa`=`play_action_rate`, `ay_q_pressure`=`under_pressure_cpoe`, `ay_q_agg`=`ngs_aggressiveness_index`), **7 coach arrays** (`coach_ay_tend`=`air_yards_tendency`, `coach_deep`=`deep_shot_rate`, `coach_screen`=`screen_rate`, `coach_pa`=`play_action_rate`, `coach_no_huddle`=`no_huddle_rate`, `coach_rpo`=`rpo_rate`, `coach_cons`=`conservative_score_bias` — i.e. all 7 of `coach_dna.json`'s rate fields), and **3 receiver arrays** (`deep_target_rate_recv`=`deep_target_rate`, `air_yards_share_recv`=`air_yards_share`, `yac_per_reception_recv`=`yac_per_reception`) — **17 arrays total**, all confirmed by grep to be assigned and never read again anywhere in the file. A stale comment (`game_engine.py:973`) still describes a 32-feature air-yards model matching this richer set, but the actually-deployed model (`air_yards_v_0_1_1`, confirmed via its own `metadata.json`) only ever uses 8 features (`down, ydstogo, yardline_100, score_differential, game_seconds_remaining, cpoe_by_filter, target_share_by_filter, carry_share_by_filter`). This looks like leftover computation from an earlier, richer architecture that was simplified during a later retrain (Round 11, per `AGENTS.md` §11.9) without removing the now-orphaned feature-building code. Purely a performance/clarity issue (dead computation, not wrong output) — but it also means several fields this doc previously described as "feeding the air-yards model" (§4.4's coach fields, most of §4.6's QB fields, §4.7's `deep_target_rate`/`air_yards_share`/`yac_per_reception`) don't actually reach it. Corrected in place above.

None of these were fixed as part of this audit — this doc is the record of what was found, for a deliberate decision on each rather than continuing to silently default. (Bugs #1-8 predate 2026-07-13; bug #9 and the corrections above were found and fixed in the doc, but not in code, on that date.)
