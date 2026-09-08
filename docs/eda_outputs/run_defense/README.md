# Run-Defense Metrics — Quick EDA
<!-- Status: live | 2026-07-16 -->

**Question:** the defense-side mirror of `docs/eda_outputs/run_block/` — do the run-stopping metrics look like real, usable signal? There is no native "defense" stat anywhere in the pipeline for rushing (confirmed: `import_seasonal_pfr` only supports `pass`/`rec`/`rush`; standard `import_seasonal_data` has no run-stop columns) — every metric here is **derived** by opponent-joining the same offense-side rushing data (`scripts/eda/analyze_run_defense_metrics.py`).

**Data:** 222 team-seasons, 2018-2024. `stuff_rate_forced`/`aly_allowed` come straight from raw PBP (already tags every play with `defteam`). `ybc_allowed_per_att`/`rush_pct_over_expected_allowed`/`pct_stacked_box_forced` needed an opponent join against `data/external/schedules_2015_2024.csv` (PFR/NGS data is keyed by the rusher's own team, not who they played).

## Findings

**Distinct skill from pass-rush, not redundant.** Cross-checked all 5 run-D candidates against the *already-live* `def_pressure_rate` (2024, 32 teams) — all weak (`|r|` between 0.06 and 0.40). Run-stopping and pass-rushing are genuinely different team capabilities, confirming this is worth building as its own feature set rather than assuming the existing sacks-model inputs already cover it.

**Year-over-year stability** — a different pattern than the offense side:

| Metric | Year N → N+1 r |
|---|---|
| `pct_stacked_box_forced` | **0.43** |
| `stuff_rate_forced` | **0.42** |
| `aly_allowed` | 0.39 |
| `ybc_allowed_per_att` | 0.12 |
| `rush_pct_over_expected_allowed` | 0.12 |

Interesting asymmetry vs. offense: on offense, `ybc_per_att` was one of the *more* stable metrics (r=0.39); on defense, `ybc_allowed_per_att` is one of the *least* stable (r=0.12). Plausible read: yards-before-contact is heavily influenced by the specific runner an offense trots out, so on defense it's diluted across whichever backs a team happened to face that season — less a stable defensive trait, more a function of schedule luck. `stuff_rate_forced` and `aly_allowed` (both tied to the failure/floor rate, not raw yardage) are the more trustworthy defensive signals.

**Face validity** (`top_bottom_teams.png`) — Tampa Bay, Buffalo, Baltimore, Indianapolis, New Orleans cluster in "best" run defense across multiple metrics — all reputationally sound defensive fronts over 2018-2024. No glaring mismatches with real-world expectation.

**One honest interpretive caveat:** unlike the other four metrics, `pct_stacked_box_forced`'s "better/worse" direction is genuinely ambiguous — a defense might stack the box because it's already winning early downs and forcing obvious running situations, not purely as a stable scheme choice. Treat it as context, not a clean outcome measure, when it comes time to actually weight these into a multiplier.

## Caveats

- Same single-season noise caveat as the offense side — a multi-year blend will matter more here given the weaker persistence on 2 of the 5 metrics.
- `pct_stacked_box_forced` may be more useful as a *contextualizing* variable (e.g., adjusting expectations when a team faces heavy boxes) than as a standalone "good/bad" run-D grade.

## Files

- `distributions.png`, `year_over_year_stability.png`, `top_bottom_teams.png`
- `summary_stats.csv`, `team_season_ranked.csv`
- Generating scripts: `scripts/eda/analyze_run_defense_metrics.py`, `scripts/eda/run_run_defense_eda.py`
