# game_engine.py Cleanup Audit

**Status:** Documentation phase complete (full file read, 6 passes: `__init__` +
5 parallel chunks covering all of `simulate_play_step` plus the tail helper/
reporting methods). Nothing has been fixed yet — this is the inventory to
triage against before touching anything, per Cam's request to see the whole
picture before attacking it.

This audit exists because `game_engine.py` (2,450 lines, with `simulate_play_step`
alone spanning ~1,660 of them) hadn't had a correctness/redundancy/performance
pass in one place before. Six sections were read in full and cross-checked
against actual usage (grep for every flagged variable, not just visual
inspection) rather than skimmed.

---

## How to read this doc

Findings are grouped by severity, not by file location — when we triage, we'll
likely want to knock out all the CRITICAL items first regardless of which
phase they're in. Each item has the file:line, what's wrong, the evidence, and
a rough impact/frequency estimate where relevant.

---

## CRITICAL — real correctness bugs

### 1. Overtime is unreachable — every regulation tie is recorded as a fabricated home win
`game_engine.py:2152, 2192` — `self.quarter` only advances while `pre_quarter < 4`
and the game force-ends the instant Q4 hits 0 with no tie check. `self.quarter`
is initialized to 1 and that increment is the *only* place it's ever written
(confirmed via project-wide grep) — it can never reach 5. A spec for this
exists at `docs/prompts/engine_overtime_spec.md` but was never implemented.
`get_game_summaries` (`:2229`) then computes `winner = away if score_away>score_home
else home` — so a tied game is recorded as a home win with the correct 0
spread but a fabricated winner. Also makes `_sample_pace_runoff`'s "Quarter 5+
(OT)" bucket permanently dead code.

### 2. Sack probability can exceed 1.0 for elite pass-rush matchups
`game_engine.py:935` — `sack_prob = ...inplace_predict(X_g2) * SACK_PROB_CALIBRATION_MULT`
(`SACK_PROB_CALIBRATION_MULT = 1.24`) has no `np.clip`, unlike the identical
pattern elsewhere in the file (`:833`, `adjusted_pass_prob` IS clipped). For
any lane where the raw model output ≥ 0.81, the calibrated value exceeds 1.0
and `rand() < sack_prob` becomes unconditionally true — that lane is
guaranteed a sack every time it recurs in a similar game state, silently
capping out realism for exactly the extreme matchups the model should be
differentiating.

### 3. Kneel-down (victory formation) can mathematically never trigger on an actual 4th down
`game_engine.py:409-420`, root cause `:411` — `max_bleed_time = (4-self.down)*40
- timeouts_def*40`. At `down==4`, `(4-down)*40=0`, so `max_bleed_time <= 0`
always. `kneel_mask` requires `time_remaining < max_bleed_time`, which is
unsatisfiable since time can't be negative. In practice a leading team usually
finishes the game via clock expiry before reaching down 4, but any leading
team that legitimately reaches 4th down with time still on the clock — the
literal "kneel out the game" situation — falls through to the punt/FG/GO
decision model instead. Real teams always kneel here.

### 4. A would-be touchdown can be silently erased and reclassified as a turnover
`game_engine.py:1624-1645` (fumble roll) — never checks whether `play_gain`
would have crossed the goal line before rolling a fumble. Phase 6 only exempts
`play_is_fumble_lost` plays from the TD check (`:1808`). So a play that gained
enough to score can still be rolled as a lost fumble and routed into the
turnover branch — the TD never happens. Low frequency (~0.5% fumble rate ×
TD-gain plays) but a genuine gap, and the recovery-spot math briefly produces
an out-of-range yardline on these plays before being clipped.

### 5. QB scrambles are structurally invisible to fumble risk and the big-play counter
`game_engine.py:1625` (`valid_carrier_mask = is_run | (is_pass & play_is_complete)`)
and `:1910` (`big_play = ... & (is_run | (is_pass & play_is_complete))`) — both
masks miss scrambles entirely, because `play_is_complete` is only ever set
inside `no_sack_pass`, which explicitly excludes scrambles (`:1026`). A 20+
yard scramble never increments `plays_over_20_yds`, and no scramble can ever
fumble. Confirmed via grep: no other fumble path references
`play_is_scramble`. This is a systematic gap, not an edge case — scrambles are
common.

### 6. Punt returns aren't re-clipped into valid field position, unlike kickoff returns
`game_engine.py:684-689` vs. `501-529` — punt-return yardage (`ret_yds`, a
shifted exponential that can go as low as ≈-13, i.e. a big return loss) is
only bounded on the upper side (`max_ret`) before being subtracted from
`yardline_100`, with no floor after. The kickoff-return branch has the correct
symmetric clip (`:529`); punts are missing the equivalent, so a deep punt
combined with a large return-loss roll can push `yardline_100` past 99 — an
invalid field position (real football: a safety).

### 7. Return-yardage stats and actual field position can silently disagree
`game_engine.py:1719/1727` (interception) and `1769/1777` (fumble) — raw
sampled return yardage can be negative, and is floored at 0 for the recorded
stat but NOT floored before being applied to the actual field-position update.
Rough estimate: ~15-30% of non-TD returns land in the negative-raw-value
regime where the stat and the actual yardline move disagree.

### 8. Gate 2 (sack model) is fed duplicated features instead of real trend data
`game_engine.py:888, 920-921` — the trained model's `off_sack_rate_l4` /
`def_sack_rate_l4` features (per `chaos_v_0_1_0/metadata.json`) are fed the
exact same season-average values already used for `sack_rate_allowed`/
`def_sack_rate`. Confirmed via grep: no last-4-games data is plumbed anywhere
in this file or `data/dna/`. Whatever incremental signal the model learned
from short-term trend deltas at training time is zeroed out at inference.
Matches a known, already-documented finding (AGENTS.md §11.9 Round 12) — not
new, but worth including here since it's a real correctness gap in this file.

### 9. Asymmetric penalty-vs-turnover decline logic between pass and run branches
`game_engine.py:1431` vs. `1591-1601` — the pass branch explicitly declines a
penalty when an interception is the better outcome for the offense (correct
real-football logic). The run branch has no equivalent: `has_accepted_penalty`
is finalized before Phase 5's fumble roll even happens, so a run-play holding
call can never be declined in favor of a defensive fumble recovery. Rare
(~1.49% × ~0.5% joint) but a real structural inconsistency.

---

## Dead / write-only code

Confirmed via grep — each of these is written somewhere and never read
anywhere else in the file or reporting methods.

| Variable | Written | Notes |
|---|---|---|
| `drives_count` | `:174`, incremented `:475`, `:2206` | Never read/exposed anywhere |
| `gross_sacks` / `net_sacks` | `:181-182`, incremented `:938`, `:985` | Diagnostic-only, built for the now-resolved Round 14 sacks investigation |
| `fg_made_away` / `fg_made_home` | `:715-716` | Never read |
| `fourth_down_goes_away` / `fourth_down_goes_home` | `:735-736` | Never read |
| `oob_plays` / `oob_eligible_plays` | `:1889-1890` | Never read |
| `fg_attempts_away`/`home` | `:702-703` | Only ever summed into total snaps — never exposed per-team, so FG rate can't be reconstructed from output |
| `is_away`, `qb_names`, `rb_names` (play-selection bucket loop) | `:771-773` | Recomputed then never read — the zone-loop rebuilds its own version instead |
| `g2_features` | `:887` | Allocated, never touched — actual model input built separately as `X_g2` |
| `avg_sep` | `:1043`/`1081` | Exact duplicate of `avg_separation_yds_recv`, which is the one actually used |
| `target_share_recv` | `:1044`/`1067` | Superseded by the zone-split version used everywhere else |
| `qb_names_ay` / `qb_names_yac` | `:1097`/`1313` | Computed, never referenced — QB identity re-derived a second way instead |
| `cpoe_by_filter_run` init | `:1489` | Immediately overwritten at `:1504`, dead initialization |
| `get_stats_report` | `:2208-2224` | Stub — loop body is comments + `pass`, always returns `[]`. Confirmed dead for this class; real callers all target the legacy `SequentialNFLGameEngine` or unrelated `NFLGameSimulator`, never `NFLGameEngine` |
| `_load_skill_dna` fallback to `skill_dna.json` | `:362-363` | Provably unreachable — rb/wr/te DNA files are never all empty (270/260/123 entries confirmed) |

**Also:** `self.trench_tiers` (`trench_tiers_2025.json`, unclear provenance)
still drives the pass-side multiplier even though the run side migrated off
it today — a known, already-flagged (AGENTS.md) half-finished migration, not
new.

---

## Optimization opportunities

The file has a deliberate, good pattern — precompute per-game/per-team-static
data in `__init__` (`precomputed_qb_cpoe`, `receivers_cache`, etc.) to keep
the hot loop (`simulate_play_step`, called every down for every one of the N
parallel lanes) free of repeated dict lookups. Several sections don't follow
it:

- **Pass resolution (`:1056-1090`)** re-fetches receiver/QB DNA traits
  (catch rate, elusiveness, broken-tackle rate, target depth, etc.) fresh from
  `self.dna[...]` on every play for every unique target — none of it changes
  during a game. Should join the existing `precomputed_*` dicts.
- **Play-selection/sack-gate section (`:786-796`, `900-912`)** re-derives
  `away_qb`/`home_qb`/`away_rb`/`home_rb` and their CPOE/target-share/
  carry-share values from nested dict lookups every bucket×zone×step, despite
  `qb_starters`/`rb_starters` never changing mid-game.
- **Run resolution (`:1635`)** does a per-unique-carrier `fumble_rate` DNA
  lookup every step instead of a precomputed per-player array.
- **`receivers_cache`/`rusher_cache` (`:872`, `1479`)** are stored as plain
  Python lists and converted to `np.array(...)` fresh on every single play —
  should just be stored as arrays once in `__init__`.
- **`_run_clock` (`:2119-2193`)** is called 8-10x per play step, each call
  allocating a fresh full-`N` array and recomputing full-`N` comparisons even
  when the mask covers a handful of lanes — real cost given it's the hottest
  per-play function in the file.
- **`_predict_4th_down_probas_batch` (`:2391-2396`)** builds a fresh
  `pandas.DataFrame` every call purely to satisfy a model's `.predict()`
  signature — pandas overhead in a per-play hot path.
- **Near-duplicate logic**: the CPOE/target-share/carry-share zone-lookup
  loop is written out almost verbatim twice — once for air-yards features
  (`:1104-1128`), once for YAC features (`:1320-1343`), ~200 lines apart.
  Worth extracting into one shared helper.
- **Small items**: `get_safe_float`/`_sigmoid_arr`/`_logit_arr` are pure
  functions redefined as closures on every call (one of them even redefined
  per-receiver inside a loop) instead of being module-level; `is_offensive`
  (`:561`) draws N random numbers when only `sum(has_penalty)` are needed.

---

## Fragile / confusing (not bugs, but worth cleaning up)

- **`qb_fumble_rate` (`:990-995`) is actually the QB's *sack* rate**, not a
  fumble rate — reads `qb_dna.get('sack_rate', 0.06)`. Likely an intentional
  proxy (no real fumble-propensity field exists in the DNA schema) but the
  name will mislead the next editor.
- **Stale comment citing wrong line numbers** (`:1616-1618`) — claims
  `has_accepted_penalty` is only assigned inside the is_pass/is_run branches
  and cites "lines 720-722" as the init site. Both claims are stale: it's
  unconditionally initialized at `:859`, and 720-722 is unrelated field-goal
  runoff code. Not currently harmful, but will send a future editor to the
  wrong place.
- **Misleading comment at `:1923-1926`** claims turnover-return TDs are
  "already excluded from active/regular_clock via their own branches earlier"
  — verified false, they're handled correctly but via a *different* branch
  (`turnover_clock`) than the comment implies.
- **A large number of uncited magic numbers** scattered across nearly every
  phase (kickoff/punt touchback rates, return-distribution parameters,
  fumble-probability coefficients, 4th-down decision weights, catch-probability
  calibration constants). Contrasts with the file's better-documented sections
  (e.g. `SACK_PROB_CALIBRATION_MULT`, the OOB rates), which do cite real data
  sources. Not urgent individually, but worth a pass for traceability.
- **`game_sec = (4-quarter)*900 + time_remaining`** is an inverted-quarter
  representation that requires mental decoding at every threshold check
  (e.g. "last 30s of Q2" is written as a specific `game_sec` range).
- **Hardcoded zone-boundary constants** (`yardline_100 <= 5` / `<= 20`) are
  repeated independently 3 separate times with no shared constant — a future
  recalibration would need three synchronized edits.
- **`regular_clock`'s exclusion list (`:1997`)** is a hand-maintained NOT-list
  — any future Phase 7 outcome category must remember to be added, with no
  structural guard against forgetting.
- **`off_score`/`def_score` field names** in `get_game_summaries` (`:2232-2233`)
  are hardcoded to mean away/home, but the same field names mean
  "team-of-interest" in the legacy `batch.py` worker — a cross-path naming trap
  for anyone comparing the two.

---

## Not yet triaged into a fix order

This doc is inventory only. Next step: prioritize together and decide what to
tackle and in what order.
