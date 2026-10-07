# Window-blend backtest (2024 + 2025)

Error of the INPUT predictions (weighted MAE per field), target weeks 5-18, scored on rows every method can predict. Preseason proxy = same player's prior-season value. `rel` = mean over fields of (method wMAE / current_l4 wMAE - 1); negative = better than today's blend. 95% interval is a player-cluster bootstrap.


## players - 2024 (rows=20446)

| method | rel_vs_current | lo | hi |
|---|---|---|---|
| flat_pre50 | -0.0744 | -0.0971 | -0.0515 |
| season_only | -0.0612 | -0.0862 | -0.0359 |
| ramp_a_mean | -0.0501 | -0.0776 | -0.0226 |
| ramp_a_median | -0.0498 | -0.0775 | -0.0220 |
| flat_mean | -0.0497 | -0.0769 | -0.0217 |
| flat_median | -0.0495 | -0.0770 | -0.0215 |
| ramp_b_mean | -0.0493 | -0.0769 | -0.0221 |
| ramp_b_median | -0.0490 | -0.0768 | -0.0215 |
| flat_mean_nomin | -0.0328 | -0.0614 | -0.0030 |
| steady_2_3 | -0.0235 | -0.0534 | 0.0080 |
| preseason | -0.0167 | -0.0455 | 0.0115 |
| current_l4 | 0.0000 | -0.0292 | 0.0328 |

## players - 2025 (rows=20032)

| method | rel_vs_current | lo | hi |
|---|---|---|---|
| flat_pre50 | -0.0700 | -0.0935 | -0.0462 |
| season_only | -0.0461 | -0.0715 | -0.0191 |
| flat_median | -0.0446 | -0.0677 | -0.0212 |
| flat_mean | -0.0444 | -0.0677 | -0.0210 |
| ramp_a_median | -0.0421 | -0.0649 | -0.0177 |
| ramp_a_mean | -0.0420 | -0.0646 | -0.0180 |
| ramp_b_median | -0.0398 | -0.0623 | -0.0152 |
| ramp_b_mean | -0.0397 | -0.0620 | -0.0158 |
| flat_mean_nomin | -0.0345 | -0.0582 | -0.0111 |
| steady_2_3 | -0.0197 | -0.0461 | 0.0099 |
| preseason | -0.0159 | -0.0410 | 0.0118 |
| current_l4 | -0.0000 | -0.0263 | 0.0288 |

## players - both (rows=40478)

| method | rel_vs_current | lo | hi |
|---|---|---|---|
| flat_pre50 | -0.0726 | -0.0911 | -0.0500 |
| season_only | -0.0541 | -0.0755 | -0.0290 |
| flat_mean | -0.0472 | -0.0682 | -0.0246 |
| flat_median | -0.0472 | -0.0682 | -0.0246 |
| ramp_a_mean | -0.0463 | -0.0673 | -0.0235 |
| ramp_a_median | -0.0462 | -0.0672 | -0.0235 |
| ramp_b_mean | -0.0448 | -0.0658 | -0.0220 |
| ramp_b_median | -0.0447 | -0.0657 | -0.0219 |
| flat_mean_nomin | -0.0338 | -0.0565 | -0.0090 |
| steady_2_3 | -0.0217 | -0.0456 | 0.0045 |
| preseason | -0.0169 | -0.0370 | 0.0091 |
| current_l4 | -0.0000 | -0.0248 | 0.0260 |

### per-field wMAE (players, both years)

| field | n | preseason | season_only | current_l4 | steady_2_3 | flat_mean | flat_median | ramp_a_mean | ramp_a_median | ramp_b_mean | ramp_b_median | flat_mean_nomin | flat_pre50 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| adot | 4908 | 3.5120 | 3.4656 | 3.6462 | 3.5488 | 3.4582 | 3.4577 | 3.4648 | 3.4641 | 3.4719 | 3.4714 | 3.5247 | 3.3511 |
| avg_air_yards_per_att | 749 | 1.7534 | 1.7221 | 1.8453 | 1.8108 | 1.7342 | 1.7358 | 1.7371 | 1.7386 | 1.7402 | 1.7417 | 1.7573 | 1.6621 |
| broken_tackle_rate | 4593 | 0.1776 | 0.1784 | 0.1869 | 0.1830 | 0.1824 | 0.1825 | 0.1829 | 0.1830 | 0.1833 | 0.1834 | 0.1841 | 0.1758 |
| carry_share | 2168 | 0.1297 | 0.1013 | 0.1018 | 0.1013 | 0.0984 | 0.0984 | 0.0977 | 0.0977 | 0.0975 | 0.0975 | 0.0984 | 0.1063 |
| catch_rate | 4912 | 0.1866 | 0.1870 | 0.1987 | 0.1930 | 0.1880 | 0.1881 | 0.1884 | 0.1884 | 0.1887 | 0.1888 | 0.1927 | 0.1809 |
| cpoe | 748 | 7.2913 | 7.1198 | 8.0000 | 7.8376 | 7.2775 | 7.2848 | 7.2896 | 7.2942 | 7.3011 | 7.3052 | 7.5442 | 6.9912 |
| deep_target_rate | 4912 | 0.1057 | 0.1041 | 0.1087 | 0.1067 | 0.1053 | 0.1052 | 0.1054 | 0.1053 | 0.1056 | 0.1055 | 0.1064 | 0.1026 |
| elusiveness | 4548 | 2.7972 | 2.8190 | 3.0227 | 2.9195 | 2.8698 | 2.8654 | 2.8784 | 2.8754 | 2.8849 | 2.8828 | 2.9268 | 2.7113 |
| sack_rate | 749 | 0.0360 | 0.0352 | 0.0370 | 0.0364 | 0.0354 | 0.0355 | 0.0355 | 0.0355 | 0.0355 | 0.0356 | 0.0357 | 0.0345 |
| scramble_rate | 749 | 0.0330 | 0.0314 | 0.0338 | 0.0332 | 0.0316 | 0.0317 | 0.0317 | 0.0317 | 0.0317 | 0.0318 | 0.0322 | 0.0309 |
| target_share | 4623 | 0.0561 | 0.0500 | 0.0514 | 0.0506 | 0.0503 | 0.0503 | 0.0503 | 0.0503 | 0.0503 | 0.0503 | 0.0503 | 0.0504 |
| yac_per_rec | 4593 | 2.8624 | 2.8556 | 3.0861 | 2.9866 | 2.8986 | 2.8970 | 2.9022 | 2.9012 | 2.9072 | 2.9064 | 2.9794 | 2.7669 |
| ypc | 2226 | 1.6956 | 1.7681 | 1.8378 | 1.7871 | 1.7778 | 1.7746 | 1.7829 | 1.7803 | 1.7869 | 1.7846 | 1.7775 | 1.6508 |

## teams - 2024 (rows=1248)

| method | rel_vs_current | lo | hi |
|---|---|---|---|
| flat_pre50 | -0.0693 | -0.1102 | -0.0290 |
| preseason | -0.0607 | -0.1007 | -0.0173 |
| season_only | -0.0578 | -0.1092 | -0.0123 |
| steady_2_3 | -0.0319 | -0.0856 | 0.0169 |
| ramp_a_mean | -0.0211 | -0.0764 | 0.0268 |
| flat_mean_nomin | -0.0210 | -0.0756 | 0.0291 |
| ramp_a_median | -0.0209 | -0.0763 | 0.0269 |
| ramp_b_mean | -0.0204 | -0.0768 | 0.0280 |
| ramp_b_median | -0.0202 | -0.0766 | 0.0281 |
| flat_mean | -0.0193 | -0.0747 | 0.0303 |
| flat_median | -0.0191 | -0.0746 | 0.0305 |
| current_l4 | 0.0000 | -0.0557 | 0.0501 |

## teams - 2025 (rows=1248)

| method | rel_vs_current | lo | hi |
|---|---|---|---|
| flat_pre50 | -0.0581 | -0.0995 | -0.0119 |
| season_only | -0.0456 | -0.0889 | 0.0024 |
| preseason | -0.0380 | -0.0790 | 0.0080 |
| steady_2_3 | -0.0267 | -0.0751 | 0.0209 |
| flat_mean_nomin | -0.0181 | -0.0680 | 0.0291 |
| flat_mean | -0.0168 | -0.0670 | 0.0313 |
| flat_median | -0.0164 | -0.0665 | 0.0315 |
| ramp_a_mean | -0.0124 | -0.0615 | 0.0357 |
| ramp_a_median | -0.0121 | -0.0614 | 0.0359 |
| ramp_b_mean | -0.0089 | -0.0580 | 0.0406 |
| ramp_b_median | -0.0087 | -0.0579 | 0.0408 |
| current_l4 | 0.0000 | -0.0510 | 0.0502 |

## teams - both (rows=2496)

| method | rel_vs_current | lo | hi |
|---|---|---|---|
| flat_pre50 | -0.0639 | -0.0914 | -0.0362 |
| season_only | -0.0520 | -0.0804 | -0.0191 |
| preseason | -0.0497 | -0.0746 | -0.0227 |
| steady_2_3 | -0.0294 | -0.0601 | 0.0030 |
| flat_mean_nomin | -0.0196 | -0.0511 | 0.0133 |
| flat_mean | -0.0181 | -0.0495 | 0.0151 |
| flat_median | -0.0178 | -0.0492 | 0.0156 |
| ramp_a_mean | -0.0168 | -0.0492 | 0.0165 |
| ramp_a_median | -0.0166 | -0.0490 | 0.0168 |
| ramp_b_mean | -0.0148 | -0.0475 | 0.0184 |
| ramp_b_median | -0.0146 | -0.0473 | 0.0186 |
| current_l4 | 0.0000 | -0.0329 | 0.0335 |

### per-field wMAE (teams, both years)

| field | n | preseason | season_only | current_l4 | steady_2_3 | flat_mean | flat_median | ramp_a_mean | ramp_a_median | ramp_b_mean | ramp_b_median | flat_mean_nomin | flat_pre50 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| def_pressure_rate | 832 | 0.0599 | 0.0595 | 0.0633 | 0.0611 | 0.0620 | 0.0620 | 0.0620 | 0.0620 | 0.0621 | 0.0621 | 0.0619 | 0.0591 |
| def_sack_rate | 832 | 0.0342 | 0.0355 | 0.0371 | 0.0361 | 0.0365 | 0.0365 | 0.0366 | 0.0366 | 0.0368 | 0.0368 | 0.0364 | 0.0343 |
| sack_rate_allowed | 832 | 0.0350 | 0.0338 | 0.0356 | 0.0346 | 0.0350 | 0.0350 | 0.0350 | 0.0350 | 0.0350 | 0.0351 | 0.0349 | 0.0339 |

## Zone shares -- SUPERSEDED, do not use

> This first zone run also scored weeks where the player was inactive as a true 0% share, which inflated the no-gate result. The corrected, availability-gate backtest is in `zone_availability_results.md`.

### Zone shares (red zone / goal line), weighted by team zone plays

| kind | zone | n | preseason | pooled8 | anchor_0.3 | anchor_0.5 | anchor_0.7 |
|---|---|---|---|---|---|---|---|
| carry_share | goalline | 1594 | 0.2548 | 0.2295 | 0.2469 | 0.2478 | 0.2497 |
| carry_share | redzone | 3094 | 0.1838 | 0.1587 | 0.1782 | 0.1783 | 0.1798 |
| target_share | goalline | 2149 | 0.1918 | 0.1869 | 0.1933 | 0.1928 | 0.1923 |
| target_share | redzone | 5943 | 0.1129 | 0.1033 | 0.1144 | 0.1135 | 0.1131 |