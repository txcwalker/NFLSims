# Exploratory Data Analysis Scripts

This folder contains one-off and recurring EDA/diagnostic scripts used to compare the simulation engine against real historical NFL data, and to investigate specific statistical questions that come up during model audits (e.g. the `clock_physics_v020` rounds — see [docs/audit/clock_physics_v020/README.md](../../docs/audit/clock_physics_v020/README.md)).

## Scripts & Functions

* **`run_historical_eda.py`**: Computes real 2021-2025 comparison metrics into `historical_eda_metrics.json` — the "ground truth" baseline the simulator is measured against.
* **`analyze_clock_pace_grid.py`**: Rebuilds `src/nfl_sim/models/clock_pace_v_0_1_0/pace_pools.json` (empirical snap-to-snap runoff pools) from real play-by-play data.
* **`analyze_clock_physics.py`**: EDA on real-game clock/pace behavior.
* **`analyze_pass_yardage_breakdown.py`**: Real-vs-sim diagnostic comparing air yards, YAC, and total gain by field zone and depth bucket. Drives `NFLGameEngine` directly to read per-play diagnostic hooks.
* **`analyze_plays_per_game.py`**: League-wide plays-per-game EDA.
* **`analyze_3rd_downs.py`**: Third-down conversion/tendency analysis.
* **`analyze_redzone_efficiency.py`**: Redzone scoring-efficiency EDA.
* **`analyze_kickoffs_2024.py`**: Kickoff outcome analysis on 2024 data.
* **`check_run_gains.py`**: Rush-yardage/gain distribution check against real data.
* **`diagnose_score_deficiency.py`**: Investigates simulated scoring shortfalls against real scoring rates.
* **`diagnose_scoring_efficiency.py`**: Broader scoring/efficiency diagnostics using real PBP data.
* **`fit_return_distributions.py`**: Fits statistical distributions for kick/punt return yardage.
* **`run_returns_eda.py`**: EDA on kick/punt return outcomes.
* **`inspect_single_game.py`**: Single-game inspection/debug utility.
* **`test_sacks_diversion_hypothesis.py`**: Tests whether gate 2's raw (pre-diversion) sack probability already approximates the real recorded sack rate — see AGENTS.md §11.11.
