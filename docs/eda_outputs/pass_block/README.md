# Pass-Block / Pass-Rush Metrics — Quick EDA
<!-- Status: live | 2026-07-16 -->

**Question:** same as the run-block/run-defense EDA — pick real, EDA-validated metrics for the pass-blocking (offense) and pass-rush (defense) cells of the trench-tier rebuild. This is EDA to inform a data source, not model training — see `AGENTS.md` §0 for the overall scope.

**Data:** `nfl_data_py.import_weekly_pfr(s_type='pass')`, 2018-2024, 224 team-seasons per side. This table has both `team` and `opponent` per row, so defense-side aggregation is a direct opponent-join, no separate schedule join needed (unlike the rushing case).

## A real data-quality finding, not just a metric choice

**`def_times_blitzed`/`def_times_hurried`/`def_times_hitqb` are confirmed 100% NULL across every season checked (2020-2024)** — they exist as column names in `nfl_data_py` but carry zero real data. Building anything on them would have silently produced garbage. The fix: derive the same information by opponent-joining the real, fully-populated offense-side columns (`times_pressured`, `times_hurried`, `times_hit`) — same technique already proven for run-defense metrics, where no native defense column existed at all.

## Findings

**Offense (pass-blocking) — all 3 candidates worth keeping.** Checked against each other and against the already-live `sack_rate_allowed`:

| | vs. `sack_rate_allowed` (existing) |
|---|---|
| `pressured_pct_allowed` | 0.615 |
| `hurry_rate_allowed` | **−0.013** (essentially independent) |
| `hit_rate_allowed` | 0.141 |

None are highly redundant with what's already live, and `hurry_rate_allowed`/`hit_rate_allowed` are barely correlated with each other either (r=−0.15) — hurries and hits appear to capture genuinely different protection-breakdown events (a QB who gets rid of the ball under duress is "hurried" but may avoid being "hit"). **Recommendation: add all 3** alongside the existing `sack_rate_allowed` — a clean 4-metric offense set, no further picking needed.

**Defense (pass-rush) — 2 of 3 new candidates are near-duplicates of existing data.**

| | vs. `def_pressure_rate` (existing, already live in Gate 2) |
|---|---|
| `pressure_rate_forced` | **0.863** — near-duplicate |
| `hit_rate_forced` | **0.901** — near-duplicate |
| `hurry_rate_forced` | 0.213 — genuinely distinct |

Only `hurry_rate_forced` adds real new information. **Recommendation: don't pad to 4 with a redundant metric just to match the offense side's count** — better to keep `def_pressure_rate` + `def_sack_rate` (already live) + `hurry_rate_forced` (3 genuinely distinct signals) than add a near-duplicate for symmetry.

**Year-over-year stability** — moderate and consistent across all 4 keepers (r≈0.30-0.36), similar magnitude to the moderate-persistence run-block metrics. None are pure noise, none are highly deterministic — normal for single-season team stats.

**Face validity** (2018-2024 averages) — Tampa Bay, New Orleans, Las Vegas, Jacksonville, Baltimore have the lowest pressure-allowed (best protection); Tennessee, NY Jets, NY Giants, Minnesota, Seattle the highest — matches well-documented O-line reputations for several of these teams over this window.

## Final recommended set

- **Offense (pass-blocking):** `sack_rate_allowed` (existing) + `pressured_pct_allowed`, `hurry_rate_allowed`, `hit_rate_allowed` (new) — 4 metrics.
- **Defense (pass-rush):** `def_pressure_rate` + `def_sack_rate` (existing) + `hurry_rate_forced` (new) — 3 metrics, deliberately not padded to 4.

## Caveats

- Same multi-year-blending note as the run-side EDA — single-season values are noisy (r≈0.3), a real per-team feature should blend across years.
- `implied_dropbacks` (used to weight team-season averages) is back-solved from `times_pressured / times_pressured_pct`, the same technique used for `ybc`'s implied attempts on the run side — an approximation, not a directly reported volume column.

## Files

- `docs/eda_outputs/pass_block_offense_team_season.csv`, `docs/eda_outputs/pass_block_defense_team_season.csv`
- Generating script: `scripts/eda/analyze_pass_block_metrics.py`
