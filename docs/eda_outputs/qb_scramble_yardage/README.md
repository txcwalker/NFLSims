# QB Scramble Yardage EDA (2020-2025 Regular Season)

Verifies `game_engine.py`'s scramble-yardage model (`Normal(5, 4)`, clipped at floor -2) against real data. Uses `nfl_data_py`'s `qb_scramble` PBP column directly -- a real scramble is already distinguished from a designed QB run/sneak in the source data, so no heuristic (e.g. excluding 1-2 yard QB runs) is needed.

| Metric | Value |
| :--- | ---: |
| N | 6,230 |
| Mean | 6.99 |
| Std Dev | 6.07 |
| Median | 6.0 |
| P25 | 3.0 |
| P75 | 9.0 |
| Min | -1.0 |
| Max | 61.0 |

**Current model:** `Normal(5, 4)`, clipped at floor -2.

**Real data:** mean 6.99 (vs. modeled 5 -- undershooting by ~2.0 yards), std 6.07 (vs. modeled 4). Distribution is right-skewed (mean > median), which a Normal doesn't capture, but Cam's call is a Normal approximation is fine for now -- just needs the real mean/std, not a distribution-shape change.
