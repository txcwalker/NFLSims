# Defensive predictiveness (2021-2025, mean across seasons)


## Target: later points allowed (opp-adj)

| predictor (early window) | wk1-4 -> wk5-8 | wk1-8 -> wk9-12 | wk1-8 -> wk9-17 | wk1-12 -> wk13-17 |
|---|---|---|---|---|
| points allowed, raw | 0.23 | 0.12 | 0.14 | 0.16 |
| points allowed, opp-adjusted | 0.23 | 0.12 | 0.13 | 0.16 |
| yards/play allowed, raw | 0.12 | 0.11 | 0.12 | 0.14 |
| yards/play allowed, opp-adj | 0.12 | 0.11 | 0.11 | 0.13 |
| EPA/play allowed, raw | 0.10 | 0.10 | 0.11 | 0.14 |
| EPA/play allowed, opp-adj (light) | 0.11 | 0.10 | 0.10 | 0.14 |
| EPA/play allowed, opp-adj | 0.11 | 0.10 | 0.10 | 0.14 |
| success rate allowed, opp-adj | 0.10 | 0.11 | 0.11 | 0.12 |
| pass EPA allowed, opp-adj | 0.07 | 0.06 | 0.09 | 0.11 |
| rush EPA allowed, opp-adj | 0.15 | 0.12 | 0.08 | 0.16 |
| CPOE allowed, opp-adj | 0.06 | -0.02 | 0.07 | 0.14 |
| EPA/target to WR, opp-adj | -0.04 | -0.04 | 0.05 | 0.20 |
| EPA/target to TE, opp-adj | 0.03 | 0.12 | 0.15 | 0.05 |
| EPA/target to RB, opp-adj | 0.12 | 0.02 | -0.02 | -0.08 |
| *target ceiling (reliability)* | *0.06* | *0.17* | *0.23* | *0.08* |

## Target: later EPA/play allowed (opp-adj)

| predictor (early window) | wk1-4 -> wk5-8 | wk1-8 -> wk9-12 | wk1-8 -> wk9-17 | wk1-12 -> wk13-17 |
|---|---|---|---|---|
| points allowed, raw | 0.14 | 0.13 | 0.20 | 0.15 |
| points allowed, opp-adjusted | 0.14 | 0.14 | 0.20 | 0.15 |
| yards/play allowed, raw | 0.07 | 0.21 | 0.25 | 0.21 |
| yards/play allowed, opp-adj | 0.07 | 0.21 | 0.25 | 0.20 |
| EPA/play allowed, raw | 0.10 | 0.16 | 0.20 | 0.18 |
| EPA/play allowed, opp-adj (light) | 0.11 | 0.16 | 0.20 | 0.19 |
| EPA/play allowed, opp-adj | 0.11 | 0.16 | 0.20 | 0.18 |
| success rate allowed, opp-adj | 0.12 | 0.18 | 0.22 | 0.18 |
| pass EPA allowed, opp-adj | 0.06 | 0.09 | 0.16 | 0.17 |
| rush EPA allowed, opp-adj | 0.20 | 0.20 | 0.16 | 0.12 |
| CPOE allowed, opp-adj | 0.04 | 0.09 | 0.17 | 0.19 |
| EPA/target to WR, opp-adj | 0.02 | 0.02 | 0.09 | 0.15 |
| EPA/target to TE, opp-adj | -0.03 | 0.14 | 0.15 | 0.04 |
| EPA/target to RB, opp-adj | 0.05 | -0.03 | 0.01 | 0.04 |
| *target ceiling (reliability)* | *0.14* | *0.05* | *0.23* | *0.14* |

## Prior season + early window (pooled, standardized)

```json
{
 "later points allowed (opp-adj)": {
  "wk5-8": {
   "corr_prior_alone": -0.045,
   "corr_early_alone": 0.091,
   "beta_prior": -0.076,
   "beta_early": 0.111,
   "R2": 0.014
  },
  "wk9-17": {
   "corr_prior_alone": 0.03,
   "corr_early_alone": 0.095,
   "beta_prior": 0.013,
   "beta_early": 0.093,
   "R2": 0.009
  }
 },
 "later EPA/play allowed (opp-adj)": {
  "wk5-8": {
   "corr_prior_alone": -0.037,
   "corr_early_alone": 0.077,
   "beta_prior": -0.063,
   "beta_early": 0.094,
   "R2": 0.01
  },
  "wk9-17": {
   "corr_prior_alone": 0.059,
   "corr_early_alone": 0.199,
   "beta_prior": 0.022,
   "beta_early": 0.194,
   "R2": 0.04
  }
 }
}
```