# Clock Pace Grid (2021-2025 Regular Season)

Built from real pbp data: previous play was not incomplete/timeout/penalty, and no possession change. **Deliberately includes out-of-bounds-terminated plays** (unlike `docs/eda_outputs/clock/README.md`'s 'running clock' cohort, which excludes them) — this grid feeds `game_engine.py`'s `is_normal_reg` bucket, which has no out-of-bounds modeling and applies this pool to every qualifying play regardless of where it ends, so the pool must reflect the real blended mix of continuous-clock and OOB-stopped snaps, not the OOB-excluded subset. Score-margin tiers are from the posteam's perspective. Cells with N < 100 are flagged as thin.

| Situation | Margin Tier | N | Mean | Median | P25 | P75 | Thin? |
| :--- | :--- | ---: | ---: | ---: | ---: | ---: | :---: |
| Q1 | Trailing 2-score | 1,028 | 27.7s | 36.0s | 9.8s | 41.0s |  |
| Q1 | Trailing 1-score | 6,962 | 36.6s | 39.0s | 34.0s | 42.0s |  |
| Q1 | Tied | 13,721 | 38.0s | 39.0s | 35.0s | 42.0s |  |
| Q1 | Leading 1-score | 4,564 | 31.5s | 38.0s | 25.0s | 41.0s |  |
| Q1 | Leading 2-score | 435 | 25.6s | 34.0s | 6.0s | 40.0s |  |
| Q2 >4:00 | Trailing 3+ score | 815 | 26.1s | 36.0s | 0.0s | 41.0s |  |
| Q2 >4:00 | Trailing 2-score | 2,458 | 37.8s | 39.0s | 35.0s | 42.0s |  |
| Q2 >4:00 | Trailing 1-score | 5,855 | 37.0s | 39.0s | 35.0s | 42.0s |  |
| Q2 >4:00 | Tied | 2,739 | 38.5s | 40.0s | 36.0s | 43.0s |  |
| Q2 >4:00 | Leading 1-score | 4,907 | 36.0s | 39.0s | 35.0s | 42.0s |  |
| Q2 >4:00 | Leading 2-score | 1,676 | 32.6s | 38.0s | 28.8s | 42.0s |  |
| Q2 >4:00 | Leading 3+ score | 278 | 31.1s | 37.0s | 22.0s | 42.0s |  |
| Q2 4:00-2:00 | Trailing 3+ score | 262 | 26.3s | 34.5s | 0.0s | 41.0s |  |
| Q2 4:00-2:00 | Trailing 2-score | 497 | 37.3s | 39.0s | 35.0s | 42.0s |  |
| Q2 4:00-2:00 | Trailing 1-score | 986 | 36.6s | 39.0s | 34.0s | 42.0s |  |
| Q2 4:00-2:00 | Tied | 347 | 38.1s | 40.0s | 36.0s | 43.0s |  |
| Q2 4:00-2:00 | Leading 1-score | 873 | 36.4s | 39.0s | 35.0s | 43.0s |  |
| Q2 4:00-2:00 | Leading 2-score | 402 | 34.4s | 39.0s | 33.0s | 42.0s |  |
| Q2 4:00-2:00 | Leading 3+ score | 113 | 32.4s | 39.0s | 25.0s | 42.0s |  |
| Q2 <2:00 | Trailing 3+ score | 2,930 | 7.3s | 5.0s | 0.0s | 10.0s |  |
| Q2 <2:00 | Trailing 2-score | 824 | 19.5s | 19.0s | 7.0s | 27.0s |  |
| Q2 <2:00 | Trailing 1-score | 1,670 | 19.8s | 19.0s | 7.0s | 28.0s |  |
| Q2 <2:00 | Tied | 500 | 20.0s | 19.0s | 8.0s | 28.0s |  |
| Q2 <2:00 | Leading 1-score | 1,443 | 19.0s | 18.0s | 7.0s | 27.0s |  |
| Q2 <2:00 | Leading 2-score | 738 | 18.7s | 17.0s | 7.0s | 28.0s |  |
| Q2 <2:00 | Leading 3+ score | 269 | 17.9s | 15.0s | 6.0s | 30.0s |  |
| Q3 >10:00 | Trailing 3+ score | 693 | 33.8s | 37.0s | 31.0s | 41.0s |  |
| Q3 >10:00 | Trailing 2-score | 1,283 | 37.1s | 38.0s | 35.0s | 42.0s |  |
| Q3 >10:00 | Trailing 1-score | 2,112 | 37.2s | 39.0s | 35.0s | 42.0s |  |
| Q3 >10:00 | Tied | 523 | 38.0s | 39.0s | 35.0s | 43.0s |  |
| Q3 >10:00 | Leading 1-score | 1,954 | 36.8s | 39.0s | 35.0s | 42.0s |  |
| Q3 >10:00 | Leading 2-score | 1,044 | 35.1s | 38.0s | 33.0s | 42.0s |  |
| Q3 >10:00 | Leading 3+ score | 555 | 35.5s | 39.0s | 34.0s | 42.0s |  |
| Q3 10:00-5:00 | Trailing 3+ score | 906 | 33.3s | 37.0s | 30.0s | 41.0s |  |
| Q3 10:00-5:00 | Trailing 2-score | 1,389 | 37.2s | 39.0s | 35.0s | 42.0s |  |
| Q3 10:00-5:00 | Trailing 1-score | 2,270 | 36.8s | 39.0s | 35.0s | 43.0s |  |
| Q3 10:00-5:00 | Tied | 569 | 37.7s | 39.0s | 35.0s | 42.0s |  |
| Q3 10:00-5:00 | Leading 1-score | 1,786 | 36.7s | 40.0s | 35.0s | 43.0s |  |
| Q3 10:00-5:00 | Leading 2-score | 1,145 | 35.3s | 39.0s | 34.0s | 42.0s |  |
| Q3 10:00-5:00 | Leading 3+ score | 716 | 34.9s | 39.0s | 33.8s | 43.0s |  |
| Q3 <5:00 | Trailing 3+ score | 1,211 | 31.1s | 35.0s | 25.0s | 41.0s |  |
| Q3 <5:00 | Trailing 2-score | 1,458 | 35.3s | 38.0s | 32.0s | 42.0s |  |
| Q3 <5:00 | Trailing 1-score | 2,192 | 35.0s | 38.0s | 32.0s | 42.0s |  |
| Q3 <5:00 | Tied | 549 | 36.7s | 39.0s | 35.0s | 43.0s |  |
| Q3 <5:00 | Leading 1-score | 2,211 | 35.5s | 39.0s | 33.0s | 43.0s |  |
| Q3 <5:00 | Leading 2-score | 1,293 | 35.4s | 39.0s | 34.0s | 43.0s |  |
| Q3 <5:00 | Leading 3+ score | 847 | 35.0s | 40.0s | 34.0s | 43.0s |  |
| Q4 >10:00 | Trailing 3+ score | 1,145 | 29.3s | 32.0s | 24.0s | 39.0s |  |
| Q4 >10:00 | Trailing 2-score | 1,335 | 35.1s | 38.0s | 31.0s | 42.0s |  |
| Q4 >10:00 | Trailing 1-score | 1,834 | 36.2s | 39.0s | 34.0s | 42.0s |  |
| Q4 >10:00 | Tied | 399 | 38.3s | 40.0s | 36.0s | 43.0s |  |
| Q4 >10:00 | Leading 1-score | 1,782 | 36.8s | 40.0s | 36.0s | 43.0s |  |
| Q4 >10:00 | Leading 2-score | 1,092 | 35.9s | 40.0s | 36.0s | 43.0s |  |
| Q4 >10:00 | Leading 3+ score | 877 | 36.6s | 41.0s | 36.0s | 44.0s |  |
| Q4 10:00-5:00 | Trailing 3+ score | 1,486 | 26.2s | 28.0s | 21.0s | 38.0s |  |
| Q4 10:00-5:00 | Trailing 2-score | 1,378 | 32.1s | 33.5s | 25.0s | 40.0s |  |
| Q4 10:00-5:00 | Trailing 1-score | 1,839 | 35.9s | 39.0s | 34.0s | 42.0s |  |
| Q4 10:00-5:00 | Tied | 449 | 38.5s | 40.0s | 37.0s | 43.0s |  |
| Q4 10:00-5:00 | Leading 1-score | 1,801 | 38.5s | 41.0s | 37.0s | 44.0s |  |
| Q4 10:00-5:00 | Leading 2-score | 1,285 | 37.5s | 42.0s | 38.0s | 44.0s |  |
| Q4 10:00-5:00 | Leading 3+ score | 965 | 38.7s | 43.0s | 40.0s | 45.0s |  |
| Q4 5:00-2:00 | Trailing 3+ score | 2,135 | 14.9s | 6.0s | 0.0s | 28.0s |  |
| Q4 5:00-2:00 | Trailing 2-score | 954 | 26.0s | 25.0s | 21.0s | 32.0s |  |
| Q4 5:00-2:00 | Trailing 1-score | 1,429 | 32.4s | 36.0s | 25.0s | 41.0s |  |
| Q4 5:00-2:00 | Tied | 325 | 37.4s | 40.0s | 36.0s | 44.0s |  |
| Q4 5:00-2:00 | Leading 1-score | 1,300 | 32.0s | 42.0s | 8.0s | 44.0s |  |
| Q4 5:00-2:00 | Leading 2-score | 677 | 29.0s | 41.0s | 6.0s | 45.0s |  |
| Q4 5:00-2:00 | Leading 3+ score | 633 | 37.9s | 43.0s | 40.0s | 45.0s |  |
| Q4 <2:00 | Trailing 3+ score | 2,103 | 11.3s | 5.0s | 0.0s | 21.0s |  |
| Q4 <2:00 | Trailing 2-score | 651 | 17.9s | 19.0s | 11.5s | 23.0s |  |
| Q4 <2:00 | Trailing 1-score | 1,525 | 17.9s | 18.0s | 8.0s | 24.0s |  |
| Q4 <2:00 | Tied | 321 | 19.7s | 18.0s | 7.0s | 29.0s |  |
| Q4 <2:00 | Leading 1-score | 1,113 | 23.6s | 25.0s | 7.0s | 38.0s |  |
| Q4 <2:00 | Leading 2-score | 650 | 29.6s | 34.0s | 20.0s | 41.0s |  |
| Q4 <2:00 | Leading 3+ score | 777 | 33.9s | 38.0s | 30.0s | 42.0s |  |
