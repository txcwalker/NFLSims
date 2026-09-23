# Volume-Weighted DNA Blend Plan

**Status:** Design confirmed by Cam, not yet implemented (scope is bigger than one session — see phased plan below). Written 2026-09-17.
**Target:** replaces `dna_blender_v_0_1_0.py`'s fixed taper schedule (`TAPER_SCHEDULE = {1: 1.0, 2: 0.8, 3: 0.6, 4: 0.4, 5: 0.2}`).
**Companion fix, already landed this session (see bottom):** `scramble_rate` was silently `0.0` for every QB, every week — real bug, unrelated to the design question below, fixed first.

## Cam's direction (2026-09-17)

- **Trigger:** Jordan Love's `cpoe` dropped from `preseason_projection.cpoe` 2.954 to the blended game-2 value 0.432 after one real game — over a 2.5-point swing. Traced and confirmed **not a bug**: `dna_blender_v_0_1_0.taper_weights(2)` returns `(0.8, 0.2)` exactly per spec, and `0.8×2.954 + 0.2×(-9.654) = 0.432` (Love's real week-1 CPOE was ~-9.65, one of the worst in the league that week). The math is right; Cam's read was that the *design* produces too large a swing off one data point.
- **Root cause:** the taper weights the **week**, not the **sample size** behind it. A 3-attempt cameo and a 40-attempt start both get the identical fixed 20% weight at game 2 — `rolling_stats_v_0_1_0.rolling_average()` is a plain per-game mean, never weighted by attempts/targets/carries.
- **Confirmed not CPOE-specific.** A scan of all 32 current 2026 roster files (`preseason_projection.<field>` vs. the current blended value) found the same pattern broadly: Kenny Pickett's `cpoe` moved -13.1 points, Sam Darnold's -5.9; several low-target bench receivers' `catch_rate` moved **exactly** -20% off a single 0-for-1-target game (mechanically: `0.8 × preseason + 0.2 × 0`).
- **Cam's fix, confirmed:** replace the fixed taper with a **pooled, volume-weighted average** — same idea as combining two samples' raw counts instead of averaging two rates:

  ```
  blended_rate = (hist_volume × hist_rate + season_volume × season_rate) / (hist_volume + season_volume)
  ```

  where `volume` is each field's real denominator (pass attempts for `cpoe`, targets for `catch_rate`, carries for `ypc`, dropbacks for `sack_rate`/`scramble_rate`, etc.), `hist_*` = whatever currently backs `preseason_projection` (built by `rebuild_veteran_baseline_v_0_1_0.py`, historical PBP/NGS window), `season_*` = real 2026 PBP/NGS accumulated so far.
- **Cutover:** once the current season's volume covers **4 real games**, drop `hist_volume`/`hist_rate` entirely and blend exclusively from the trailing 4-game window (real 2026 data only) — replaces the current steady-state's `(2/3)·L4 + (1/3)·season_to_date`. **Window = 4**, Cam's call, because it reuses the existing `compute_l4()` primitive rather than adding a new window-length parameter; trivial to move to 6 later if it doesn't smooth enough in practice.

## Scope: which fields get a real volume denominator

From `rolling_stats_v_0_1_0.PLAYER_RATE_FIELDS` + `PLAYER_NGS_FIELDS` (the only fields this pipeline has live, real per-week data for):

| Field | Volume denominator | Source |
|---|---|---|
| `cpoe`, `avg_air_yards_per_att`, `sack_rate`, `scramble_rate` | pass attempts (+ sacks/scrambles for the two rate fields) | `pbp_pass`/`pbp_run`, already computed as `n_att`/`n_sack`/`n_scramble` in `build_player_game_log()` |
| `catch_rate`, `adot`, `deep_target_rate`, `target_share` | targets | `by_week.size()` on the receiver-side `p_pass` slice |
| `ypc`, `carry_share` | carries | `by_week.size()` on `p_run` |
| `yac_per_rec`, `elusiveness`, `broken_tackle_rate` | completions (no volume concept of their own — piggyback on the catch that produced the YAC) | `complete_pass == 1` slice |
| `avg_time_to_throw_sec`, `avg_separation_yds` | NGS's own weekly attempt/target column | `import_ngs_data` |
| `contested_catch_rate` | **N/A — stays out of scope.** Already documented (`rolling_stats_v_0_1_0.py` module docstring) as having no real per-week signal anywhere in `nfl_data_py`; stays a frozen, one-time synthetic value, untouched by this change. |

## Phased build plan

1. **DONE (2026-09-17). Expose volume, not just rate, from `build_player_game_log()` / `rolling_stats_for_player()`.** `build_player_game_log()` and `build_player_ngs_game_log()` now return `(log, volume)` tuples — `volume` mirrors `log`'s keys exactly, valued at each field's real weekly sample size (pass attempts for `cpoe`, targets for `catch_rate`, carries for `ypc`, dropbacks for `sack_rate`/`scramble_rate`, completions for `yac_per_rec`/`elusiveness`/`broken_tackle_rate`, NGS's own `attempts`/`targets` columns for the two NGS fields). New generic primitive `rolling_sum()` (+ `compute_season_to_date_volume()`/`compute_l4_volume()`/`rolling_volume_for_fields()`) sums volume over the same season/L4 windows `rolling_average()` already uses for rates — empty windows return `0`, not `None` (zero real volume is a valid, poolable value, unlike "no rate exists yet"). `rolling_stats_for_player()` now returns a 4-tuple `(season, l4, season_volume, l4_volume)`; its three existing callers (`refresh_weekly_dna_v_0_1_0.py`, `rebuild_veteran_baseline_v_0_1_0.py`, `build_prior_season_actuals_v_0_1_0.py`) updated to unpack it (volume discarded for now — not yet consumed anywhere, purely additive). Verified: reran `refresh_weekly_dna_v_0_1_0.py 2026 1` against real data post-change — Jordan Love's `cpoe`/`scramble_rate` came back byte-identical to the pre-Phase-1 run, confirming zero behavior change to production output. 12 new tests (volume primitives + `build_player_game_log`'s volume output + `rolling_stats_for_player`'s 4-tuple); full suite `109/109` passing.
2. **DONE (2026-09-17). Persist historical volume alongside `preseason_projection`.** `rebuild_veteran_baseline_v_0_1_0.py` now writes a sibling `preseason_projection_volume` block (real total attempts/targets/carries/dropbacks/completions across the `source_years` window) for every player it covers.
   - **Risk found and designed around before running anything for real:** this script's normal behavior is to overwrite every rate field with a freshly-pulled real average — including any of Cam's hand-tuned values, since `apply_team_season_overrides` writes hand-tuned CSV values into `preseason_projection` too (confirmed in `apply_sheet_helpers_v_0_1_0.py`'s `_overlay_row()`). `data/current_rosters/*_traits_2026.json` is gitignored — no git safety net — so a real rebuild run would silently and irrecoverably clobber hand-tuned preseason values. **Not run for real.**
   - Added a `--volume-only` mode instead: computes volume from the real PBP/NGS pull exactly as before, but writes *only* `preseason_projection_volume`, never touching `traits[field]`/`preseason_projection[field]`. Ran this against the live 2026 roster tree (`2024 2025` source years, matching the script's own documented example — flagged as an assumption, not confirmed against whatever window actually built the current `preseason_projection` values, since no prior run of this script is logged anywhere in `AGENTS.md`/`WORKLOG.md`): **0 rate fields changed, 5,029 volume fields written across 631 players.** Spot-verified Jordan Love (QB — `cpoe`/`sack_rate`/`scramble_rate`/etc. byte-identical before/after, volume shows 903 pass attempts, 938-939 dropbacks, 52 carries over the 2-season window) and Ja'Marr Chase (WR — 360 targets, 252 completions).
   - QB `target_share`/`carry_share` (already hand-computed special cases outside the generic per-field loop — QBs are never real receivers) get matching hand-computed volume: `target_share` volume forced to `0`, `carry_share` volume from real carry counts (`qb_carry_share()` now returns `(rate, volume)`).
3. **DONE (2026-09-17). Rewrite `dna_blender_v_0_1_0.py` + wire it into `refresh_weekly_dna_v_0_1_0.py` (Phases 3+4 together, per the "not yet started" note below).**
   - **`taper_weights()`/`TAPER_SCHEDULE`/`steady_state_blend()` were NOT deleted** — a real scoping issue surfaced while implementing: `blend_player_dna()` is called from three places in `refresh_weekly_dna_v_0_1_0.py`, but Phase 1/2 only ever built real volume for ONE of them (a veteran's flat fields). The other two have no volume to pool: **rookies** (on the curve track, not `rebuild_veteran_baseline`'s population — giving them zero historical volume would let one real game instantly override their curve_override's usage ramp instead of smoothing it, a regression nobody asked for) and **zone-split fields** (`target_share`/`carry_share`/`cpoe`/`catch_rate` per primary/redzone/goalline — already have their own different small-sample solution, `MIN_ZONE_SAMPLES` gating, deliberately out of scope here) and **team defense** (`TEAM_RATE_FIELDS` — out of scope per the plan's own field table above). So `blend_player_dna()`/`blend_team_dna()` now take optional `projection_volume`/`season_volume` kwargs: given both, uses the new pooled-volume mechanism below; either omitted (`None`, the default), falls back to the exact original fixed-taper/steady-state code (renamed `_taper_blend`, byte-identical body) — every existing caller that can't supply real volume keeps working exactly as before, zero behavior change for rookies/zones/team-defense.
   - **New mechanism** (`pooled_volume_blend()`): games 1-4, `(hist_volume*hist_rate + season_volume*season_rate) / (hist_volume+season_volume)`, falling back to whichever side has real (positive-volume) data when only one does. Game 5+ (`STEADY_STATE_START_GAME = WINDOW+1 = 5`): drops the historical anchor entirely, prefers L4 (trailing 4 real games) per field, falling back to season-to-date then the frozen projection if a field's L4 window is empty.
   - `refresh_weekly_dna_v_0_1_0.py`'s `blend_one_player()`: non-rookie (veteran) players now pass `projection_volume=traits.get("preseason_projection_volume")` and the real `season_volume` from `rolling_stats_for_player()`'s (now-consumed) 3rd/4th return values into the flat-field `blend_player_dna()` call. Rookies pass `projection_volume=None` explicitly (forcing the taper fallback). The per-zone `blend_player_dna()` call and `refresh_team_defense()`'s `blend_team_dna()` call are **untouched** — no volume args, so they keep using `_taper_blend` automatically.
   - **Verified against the real motivating cases** — reran `refresh_weekly_dna_v_0_1_0.py 2026 1` against live data:

     | Player | preseason `cpoe` (hist volume) | OLD blended (fixed 80/20) | NEW blended (pooled) |
     |---|---|---|---|
     | Jordan Love | 2.954 (903 attempts) | 0.432 (Δ −2.52) | 2.343 (Δ **−0.61**) |
     | Kenny Pickett | −11.856 (98 attempts) | −24.916 (Δ −13.06) | −12.516 (Δ **−0.66**) |
     | Sam Darnold | 5.376 (1,095 attempts) | −0.542 (Δ −5.92) | 5.295 (Δ **−0.08**) |

     Same real games, dramatically saner swings — proportional to real sample size (Pickett's small historical volume still lets week 1 move him a bit; Darnold's huge volume barely moves at all) instead of a flat 20% regardless of attempts.
   - 20 new tests (`pooled_volume_blend()` arithmetic/fallbacks, `blend_player_dna()`'s cutover/fallback behavior, `blend_team_dna()` forwarding) + all prior tests unchanged and still passing (they exercise the no-volume path, confirmed identical). Full suite: `121/121` passing.

## Not yet started

5. **Regenerate the DFS tree + rerun the week-2 sim** off the now-updated season-long data (same 3-step pipeline used after the `scramble_rate` fix: `apply_team_week_overrides_v_0_1_0.py 2` → `run_week_sim_2026.py 2`).

---

## Companion fix already landed this session: `scramble_rate` was always 0.0

Found while investigating the CPOE swing (not part of the design change above — a real, independent bug).

**Root cause:** `rolling_stats_v_0_1_0.py`'s old `scramble_rate` computation averaged `qb_scramble` over `pbp_pass` (`play_type == "pass"` rows only). nflverse codes a scramble as `play_type == "run"` (QB credited as rusher, not passer) — so within the pass-only slice, `qb_scramble` is definitionally always 0. Every QB who played, every week, silently computed `scramble_rate = 0.0`. This exact gotcha was already diagnosed once before, in [`docs/eda_outputs/qb_scramble_rate_2025.md`](../eda_outputs/qb_scramble_rate_2025.md:57) — the fix just never made it into the shared `rolling_stats_v_0_1_0.py` module the weekly refresh pipeline actually calls.

**Impact:** `game_engine.py` reads `scramble_rate` directly to drive in-sim scramble frequency — this wasn't cosmetic, every in-season QB's real scramble tendency was being overridden toward whatever the taper left of the (correct) preseason number.

**Fix:** `build_player_game_log()` now pulls scrambles from `pbp_run` (`rusher_player_id == player_id & qb_scramble == 1`), combined with pass attempts into a real dropback rate (`scrambles / (scrambles + pass_attempts)`), matching the already-validated convention from the EDA doc above. See [`src/data_pipeline/rolling_stats_v_0_1_0.py`](../../src/data_pipeline/rolling_stats_v_0_1_0.py:130). Two regression tests added in `tests/test_dna_blender.py` (`TestBuildPlayerGameLogScrambleRate`) — synthetic PBP confirming scrambles now count, and confirming a truly scramble-free game still reads `0.0`. `python -m pytest tests/test_dna_blender.py` — 32/32 passing.
