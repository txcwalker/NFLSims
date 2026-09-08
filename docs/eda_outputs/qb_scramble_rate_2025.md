# 2025 QB Scramble Rates

Real 2025 regular-season play-by-play (nflverse/nfl_data_py). Scramble rate = scrambles ÷ dropbacks (pass attempts + scrambles). Minimum 100 dropbacks to qualify (45 QBs). Sorted highest to lowest.

Source data: [data/dna/qb_scramble_rate_2025.csv](../../data/dna/qb_scramble_rate_2025.csv)

| Rank | QB | Pass Attempts | Scrambles | Dropbacks | Scramble Rate |
|---|---|---|---|---|---|
| 1 | Jayden Daniels | 205 | 38 | 243 | 15.6% |
| 2 | Tyrod Taylor | 147 | 22 | 169 | 13.0% |
| 3 | Justin Fields | 233 | 28 | 261 | 10.7% |
| 4 | Drake Maye | 540 | 62 | 602 | 10.3% |
| 5 | Jaxson Dart | 375 | 38 | 413 | 9.2% |
| 6 | Patrick Mahomes | 537 | 52 | 589 | 8.8% |
| 7 | Josh Allen | 506 | 48 | 554 | 8.7% |
| 8 | Russell Wilson | 129 | 12 | 141 | 8.5% |
| 9 | Kyler Murray | 177 | 16 | 193 | 8.3% |
| 10 | Lamar Jackson | 338 | 30 | 368 | 8.2% |
| 11 | Justin Herbert | 566 | 49 | 615 | 8.0% |
| 12 | Jalen Hurts | 486 | 40 | 526 | 7.6% |
| 13 | Shedeur Sanders | 235 | 18 | 253 | 7.1% |
| 14 | Marcus Mariota | 244 | 18 | 262 | 6.9% |
| 15 | Caleb Williams | 594 | 43 | 637 | 6.8% |
| 16 | Baker Mayfield | 584 | 42 | 626 | 6.7% |
| 17 | Trevor Lawrence | 600 | 43 | 643 | 6.7% |
| 18 | Bryce Young | 506 | 33 | 539 | 6.1% |
| 19 | J.J. McCarthy | 273 | 17 | 290 | 5.9% |
| 20 | Brock Purdy | 298 | 18 | 316 | 5.7% |
| 21 | Sam Rattler | 272 | 16 | 288 | 5.6% |
| 22 | Bo Nix | 638 | 36 | 674 | 5.3% |
| 23 | Tyler Shough | 356 | 20 | 376 | 5.3% |
| 24 | C.J. Stroud | 446 | 25 | 471 | 5.3% |
| 25 | Dillon Gabriel | 204 | 11 | 215 | 5.1% |
| 26 | Daniel Jones | 409 | 22 | 431 | 5.1% |
| 27 | Jordan Love | 461 | 24 | 485 | 4.9% |
| 28 | Carson Wentz | 188 | 9 | 197 | 4.6% |
| 29 | Michael Penix Jr. | 290 | 13 | 303 | 4.3% |
| 30 | Davis Mills | 169 | 7 | 176 | 4.0% |
| 31 | Dak Prescott | 631 | 26 | 657 | 4.0% |
| 32 | Jacoby Brissett | 530 | 22 | 552 | 4.0% |
| 33 | Bailey Cook | 173 | 7 | 180 | 3.9% |
| 34 | Cameron Ward | 592 | 23 | 615 | 3.7% |
| 35 | Geno Smith | 503 | 18 | 521 | 3.5% |
| 36 | Jake Browning | 134 | 4 | 138 | 2.9% |
| 37 | Joe Burrow | 278 | 8 | 286 | 2.8% |
| 38 | Sam Darnold | 502 | 14 | 516 | 2.7% |
| 39 | Mac Jones | 304 | 8 | 312 | 2.6% |
| 40 | Tua Tagovailoa | 415 | 11 | 426 | 2.6% |
| 41 | Aaron Rodgers | 526 | 12 | 538 | 2.2% |
| 42 | Matthew Stafford | 617 | 7 | 624 | 1.1% |
| 43 | Joe Flacco | 432 | 4 | 436 | 0.9% |
| 44 | Jared Goff | 614 | 5 | 619 | 0.8% |
| 45 | Kirk Cousins | 281 | 2 | 283 | 0.7% |

**Notes:**
- League range: 0.7% (Cousins) to 15.6% (Daniels) — roughly a 22x spread between the least and most mobile qualifying starter.
- `qb_scramble` plays are coded as `run` plays crediting the QB as rusher, not passer — the initial pull double-checked this against `passer_player_name` and came back all zeros before the fix (scrambles need to be grouped by `rusher_player_name`, dropbacks by `passer_player_name`).
