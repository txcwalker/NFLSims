# Kicking Timing + Touchback Rate EDA (2021-2025 Regular Season)

Verifies the previously-uncited constants in `game_engine.py` for kickoff/punt/FG live-play clock runoff, and the kickoff touchback rate. Methodology: same `game_clock_elapsed` (previous-play game-seconds-remaining delta) approach as `scripts/eda/analyze_clock_pace_grid.py`.

## Kickoff touchback rate by season

Confirms the 2025 kickoff-rule change directly rather than assuming it:

| Season | Touchback Rate | N |
| :--- | ---: | ---: |
| 2021 | 0.5754 | 2,777 |
| 2022 | 0.5975 | 2,698 |
| 2023 | 0.7302 | 2,698 |
| 2024 | 0.6432 | 2,803 |
| 2025 | 0.2068 | 2,785 |

## Live-play timing (seconds)

| Category | N | Mean | Median | P25 | P75 |
| :--- | ---: | ---: | ---: | ---: | ---: |
| kickoff_touchback | 7,542 | 0.0s | 0.0s | 0.0s | 0.0s |
| kickoff_return | 6,188 | 5.4s | 5.0s | 5.0s | 6.0s |
| punt_touchback | 762 | 8.1s | 8.0s | 8.0s | 9.0s |
| punt_fair_catch | 2,873 | 7.0s | 7.0s | 7.0s | 8.0s |
| punt_return | 6,873 | 10.1s | 10.0s | 9.0s | 12.0s |
| fg_made | 4,550 | 3.5s | 4.0s | 3.0s | 4.0s |
| fg_missed | 694 | 3.8s | 4.0s | 4.0s | 5.0s |
| fg_blocked | 99 | 5.6s | 6.0s | 4.0s | 7.0s |
