# Clock Pace Grid (2021-2025 Regular Season)

Built from real pbp data: previous play was not incomplete/timeout/penalty, and no possession change. **Deliberately includes out-of-bounds-terminated plays** (unlike `docs/eda_outputs/clock/README.md`'s 'running clock' cohort, which excludes them) — this grid feeds `game_engine.py`'s `is_normal_reg` bucket, which has no out-of-bounds modeling and applies this pool to every qualifying play regardless of where it ends, so the pool must reflect the real blended mix of continuous-clock and OOB-stopped snaps, not the OOB-excluded subset. Score-margin tiers are from the posteam's perspective. Cells with N < 100 are flagged as thin.

| Situation | Margin Tier | N | Mean | Median | P25 | P75 | Thin? |
| :--- | :--- | ---: | ---: | ---: | ---: | ---: | :---: |
| Q1 | Trailing 2-score | 553 | 37.7s | 38.0s | 35.0s | 41.0s |  |
| Q1 | Trailing 1-score | 5,604 | 38.0s | 39.0s | 35.0s | 42.0s |  |
| Q1 | Tied | 11,857 | 38.0s | 39.0s | 35.0s | 42.0s |  |
| Q1 | Leading 1-score | 2,992 | 38.5s | 39.0s | 36.0s | 42.0s |  |
| Q1 | Leading 2-score | 203 | 38.3s | 39.0s | 36.0s | 42.0s |  |
| Q2 >4:00 | Trailing 3+ score | 417 | 37.6s | 39.0s | 35.0s | 42.0s |  |
| Q2 >4:00 | Trailing 2-score | 2,129 | 38.1s | 39.0s | 35.0s | 42.0s |  |
| Q2 >4:00 | Trailing 1-score | 4,870 | 38.3s | 39.0s | 36.0s | 43.0s |  |
| Q2 >4:00 | Tied | 2,313 | 38.7s | 40.0s | 36.0s | 42.0s |  |
| Q2 >4:00 | Leading 1-score | 3,898 | 38.6s | 40.0s | 36.0s | 43.0s |  |
| Q2 >4:00 | Leading 2-score | 1,199 | 38.9s | 40.0s | 37.0s | 43.0s |  |
| Q2 >4:00 | Leading 3+ score | 186 | 38.3s | 39.0s | 36.0s | 42.0s |  |
| Q2 4:00-2:00 | Trailing 3+ score | 138 | 36.6s | 38.0s | 32.0s | 42.0s |  |
| Q2 4:00-2:00 | Trailing 2-score | 408 | 37.9s | 39.0s | 36.0s | 42.0s |  |
| Q2 4:00-2:00 | Trailing 1-score | 835 | 37.6s | 39.0s | 35.0s | 42.0s |  |
| Q2 4:00-2:00 | Tied | 281 | 38.4s | 40.0s | 36.0s | 43.0s |  |
| Q2 4:00-2:00 | Leading 1-score | 688 | 38.5s | 40.0s | 36.0s | 43.0s |  |
| Q2 4:00-2:00 | Leading 2-score | 307 | 37.7s | 39.0s | 35.5s | 42.5s |  |
| Q2 4:00-2:00 | Leading 3+ score | 76 | 38.8s | 40.0s | 37.8s | 44.0s | ⚠️ |
| Q2 <2:00 | Trailing 3+ score | 245 | 19.1s | 19.0s | 8.0s | 24.0s |  |
| Q2 <2:00 | Trailing 2-score | 669 | 20.6s | 20.0s | 9.0s | 28.0s |  |
| Q2 <2:00 | Trailing 1-score | 1,334 | 21.1s | 21.0s | 9.0s | 30.0s |  |
| Q2 <2:00 | Tied | 425 | 20.7s | 20.0s | 8.0s | 29.0s |  |
| Q2 <2:00 | Leading 1-score | 1,115 | 20.6s | 20.0s | 8.0s | 30.0s |  |
| Q2 <2:00 | Leading 2-score | 546 | 21.3s | 20.5s | 8.0s | 33.0s |  |
| Q2 <2:00 | Leading 3+ score | 173 | 23.1s | 22.0s | 10.0s | 36.0s |  |
| Q3 >10:00 | Trailing 3+ score | 545 | 35.9s | 37.0s | 32.0s | 41.0s |  |
| Q3 >10:00 | Trailing 2-score | 1,129 | 37.7s | 39.0s | 35.0s | 42.0s |  |
| Q3 >10:00 | Trailing 1-score | 1,834 | 37.6s | 39.0s | 35.0s | 42.0s |  |
| Q3 >10:00 | Tied | 459 | 38.5s | 40.0s | 35.0s | 43.0s |  |
| Q3 >10:00 | Leading 1-score | 1,658 | 38.0s | 39.0s | 36.0s | 42.0s |  |
| Q3 >10:00 | Leading 2-score | 852 | 37.8s | 39.0s | 35.0s | 42.0s |  |
| Q3 >10:00 | Leading 3+ score | 443 | 38.5s | 39.0s | 36.0s | 42.0s |  |
| Q3 10:00-5:00 | Trailing 3+ score | 696 | 36.4s | 38.0s | 33.0s | 41.0s |  |
| Q3 10:00-5:00 | Trailing 2-score | 1,179 | 37.7s | 39.0s | 35.0s | 42.0s |  |
| Q3 10:00-5:00 | Trailing 1-score | 1,902 | 38.0s | 39.0s | 35.0s | 43.0s |  |
| Q3 10:00-5:00 | Tied | 483 | 38.5s | 40.0s | 36.0s | 42.0s |  |
| Q3 10:00-5:00 | Leading 1-score | 1,422 | 38.5s | 40.0s | 36.0s | 43.0s |  |
| Q3 10:00-5:00 | Leading 2-score | 903 | 38.5s | 40.0s | 36.0s | 43.0s |  |
| Q3 10:00-5:00 | Leading 3+ score | 556 | 38.5s | 40.0s | 36.0s | 43.0s |  |
| Q3 <5:00 | Trailing 3+ score | 846 | 35.4s | 37.0s | 29.0s | 41.0s |  |
| Q3 <5:00 | Trailing 2-score | 1,099 | 37.5s | 39.0s | 35.0s | 42.0s |  |
| Q3 <5:00 | Trailing 1-score | 1,622 | 38.0s | 39.0s | 35.2s | 42.0s |  |
| Q3 <5:00 | Tied | 414 | 39.0s | 40.0s | 37.0s | 43.0s |  |
| Q3 <5:00 | Leading 1-score | 1,620 | 38.7s | 40.0s | 36.0s | 43.0s |  |
| Q3 <5:00 | Leading 2-score | 921 | 39.2s | 40.0s | 37.0s | 43.0s |  |
| Q3 <5:00 | Leading 3+ score | 586 | 39.8s | 41.0s | 38.0s | 44.0s |  |
| Q4 >10:00 | Trailing 3+ score | 876 | 32.6s | 33.0s | 26.0s | 39.2s |  |
| Q4 >10:00 | Trailing 2-score | 1,170 | 35.8s | 38.0s | 32.0s | 42.0s |  |
| Q4 >10:00 | Trailing 1-score | 1,529 | 37.6s | 39.0s | 35.0s | 42.0s |  |
| Q4 >10:00 | Tied | 347 | 38.9s | 40.0s | 36.0s | 42.0s |  |
| Q4 >10:00 | Leading 1-score | 1,467 | 38.6s | 40.0s | 36.0s | 43.0s |  |
| Q4 >10:00 | Leading 2-score | 853 | 39.0s | 41.0s | 37.0s | 43.0s |  |
| Q4 >10:00 | Leading 3+ score | 676 | 40.6s | 42.0s | 38.0s | 44.0s |  |
| Q4 10:00-5:00 | Trailing 3+ score | 1,059 | 31.0s | 30.0s | 24.0s | 38.0s |  |
| Q4 10:00-5:00 | Trailing 2-score | 1,187 | 32.2s | 33.0s | 25.0s | 40.0s |  |
| Q4 10:00-5:00 | Trailing 1-score | 1,518 | 37.6s | 39.0s | 35.0s | 42.0s |  |
| Q4 10:00-5:00 | Tied | 371 | 39.4s | 40.0s | 37.0s | 43.0s |  |
| Q4 10:00-5:00 | Leading 1-score | 1,524 | 39.7s | 41.0s | 37.0s | 44.0s |  |
| Q4 10:00-5:00 | Leading 2-score | 1,042 | 39.9s | 42.0s | 39.0s | 44.0s |  |
| Q4 10:00-5:00 | Leading 3+ score | 769 | 41.6s | 43.0s | 41.0s | 45.0s |  |
| Q4 5:00-2:00 | Trailing 3+ score | 853 | 29.8s | 28.0s | 23.0s | 37.0s |  |
| Q4 5:00-2:00 | Trailing 2-score | 832 | 26.4s | 25.0s | 21.0s | 31.0s |  |
| Q4 5:00-2:00 | Trailing 1-score | 1,210 | 34.0s | 37.0s | 27.0s | 42.0s |  |
| Q4 5:00-2:00 | Tied | 275 | 38.1s | 40.0s | 36.0s | 44.0s |  |
| Q4 5:00-2:00 | Leading 1-score | 1,088 | 32.8s | 42.0s | 10.0s | 44.0s |  |
| Q4 5:00-2:00 | Leading 2-score | 545 | 30.2s | 42.0s | 7.0s | 45.0s |  |
| Q4 5:00-2:00 | Leading 3+ score | 505 | 40.8s | 44.0s | 41.0s | 45.0s |  |
| Q4 <2:00 | Trailing 3+ score | 577 | 25.3s | 24.0s | 19.0s | 34.0s |  |
| Q4 <2:00 | Trailing 2-score | 483 | 19.0s | 21.0s | 15.0s | 23.0s |  |
| Q4 <2:00 | Trailing 1-score | 1,186 | 19.1s | 19.0s | 9.0s | 25.0s |  |
| Q4 <2:00 | Tied | 244 | 20.0s | 19.0s | 7.0s | 29.2s |  |
| Q4 <2:00 | Leading 1-score | 320 | 13.3s | 6.0s | 4.0s | 17.2s |  |
| Q4 <2:00 | Leading 2-score | 169 | 23.5s | 20.0s | 6.0s | 42.0s |  |
| Q4 <2:00 | Leading 3+ score | 204 | 30.5s | 35.0s | 17.8s | 44.0s |  |
