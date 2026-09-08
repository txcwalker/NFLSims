# Run-Block Metrics — Quick EDA
<!-- Status: live | 2026-07-16 -->

**Question:** of the 4 run-blocking metrics chosen to pair with YBC in a future `trench_dna.json` extension (`ybc_per_att`, `rush_pct_over_expected`, `stuff_rate`, `avg_time_to_los` — see `AGENTS.md` §0/§11.4 and `docs/sims/inputs/README.md` §5 bug #1), do they look like real, usable signal, or noise? **Decision (2026-07-16): offense-only, deliberately.** These feed a pure offense run-blocking multiplier, not a matchup differential against opponent run-defense quality — mirroring the deployed `air_yards_v_0_1_1` model, which also has zero defense-side inputs. Defense-side (run-stopping) data is real work for later, not a blocker for this.

**Data:** 215 team-seasons, 2018-2024 (`docs/eda_outputs/run_block_metrics_team_season.csv`, built by `scripts/eda/analyze_run_block_metric_correlations.py`). Sourced from `nfl_data_py` (NGS rushing, standard PBP) and the existing `data/external/pfr_adv_rushing_stats.csv`. Excludes QB scrambles/kneels — designed runs only.

## Findings

**Distributions** (`distributions.png`) — all four are roughly unimodal and reasonably bell-shaped, no bimodal splits or extreme outliers that would suggest a data or join problem. Ranges: `ybc_per_att` 1.6-3.7 yds, `rush_pct_over_expected` 28-51%, `stuff_rate` 13-28%, `avg_time_to_los` 2.5-3.1 sec.

**Year-over-year stability** (`year_over_year_stability.png`) — checks whether a team's value this year predicts its value next year (real, persistent team quality) vs. being mostly random noise:

| Metric | Year N → N+1 r |
|---|---|
| `avg_time_to_los` | **0.42** |
| `ybc_per_att` | **0.39** |
| `stuff_rate` | 0.21 |
| `rush_pct_over_expected` | 0.19 |

`avg_time_to_los` and `ybc_per_att` show the most persistence — consistent with them being closer to true O-line/scheme traits. `rush_pct_over_expected` is noisier year-to-year, plausibly because it partly reflects the *runner's* talent (which turns over via free agency/draft more than the O-line does), not just blocking. None of these are pure noise (r=0 would mean no relationship at all), but none are highly deterministic either — reasonable for a single-season team stat.

**Face validity** (`top_bottom_teams.png`) — 2018-2024 team averages, best/worst 8 per metric. Baltimore, Green Bay, Kansas City, and Philadelphia — all reputationally strong O-lines/run-scheme over this window — cluster in the "best" group across multiple metrics. Miami and Cleveland cluster in "worst." This matches independent, non-statistical knowledge of these teams' run games, which is a meaningful sanity check before trusting the numbers.

**No red flags found** — no missing-data pileups, no implausible values, no metric that behaves opposite of its intended direction (see the `efficiency` naming-trap note from the correlation-check session, which does NOT apply to any of these 4 — all four are labeled so higher/lower-is-better matches intuition once you know `stuff_rate` and `avg_time_to_los` are "lower is better").

## Caveats

- Single-season noise is real (r's above aren't near 1.0) — any eventual per-team multiplier should probably use a multi-year blend (matching the pattern already used elsewhere in this project, e.g. `roster_manager.py`'s 70/30 damping toward league baselines), not a single season's raw value.
- `rush_pct_over_expected` in particular carries real runner-talent confound, not pure O-line signal — worth keeping in mind when interpreting a team's number, even though it earned its spot on independence-from-YBC grounds.
- This is offense-side only, deliberately (see decision note above). A great run-blocking team currently gets the same multiplier against a great run defense as against a weak one — an accepted, temporary accuracy gap, not an oversight. Defense-side (run-stopping) mirror metrics are real future work (opponent-join against this same data), not something this pass was blocked on.

## Files

- `distributions.png`, `year_over_year_stability.png`, `top_bottom_teams.png` — the three plots above.
- `summary_stats.csv` — mean/std/min/max/quartiles per metric.
- `team_season_ranked.csv` — full 215-row team-season table.
- Generating scripts: `scripts/eda/analyze_run_block_metric_correlations.py` (data pull + correlation matrix), `scripts/eda/run_run_block_eda.py` (this EDA).
