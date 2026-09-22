
# NFL Simulation: Future Development Roadmap

This document serves as the storage for high-level architectural ideas and features planned for future major releases.

## **V3.0: The Scheme Pass**
*   **Coach/Coordinator DNA**: Integrate specific play-calling preferences (PROE - Pass Rate Over Expected) for each OC/HC.
*   **Scheme Modifiers**: Run/Pass models should weight "Scheme Fit" (e.g., Zone vs Gap blocking, West Coast vs Vertical passing).
*   **Granular Weather**: Wind and temperature effects on passing accuracy and kick distances.

## **2027 Version: The Full Physics Era**
*   **Alternative Decision Formulas**: 
    - Log-Yards EV: `Success% * log(Yards + 1)`
    - Sqrt-Yards EV: `Success% * sqrt(Yards)`
    - *Note: These are squashed alternatives to the current Catch-Squared approach.*
*   **Progression Depth**: Expand to 4th, 5th, and 6th reads for elite processors.
*   **Granular Injuries**: Transition from "Active/Inactive" to body-part specific fatigue and injury risk.
*   **Dynamic Development**: Player traits should evolve during a season (Hot/Cold streaks).
*   **Advanced Defensive Coverage**: Move from "Box Density" to specific coverage shells (Cover 2, Cover 3, Man).

## **Trench Data Pipeline — Candidate Data Sources (revisit each offseason)**

`trench_dna.json`'s three genuinely-empty NGS-branded placeholder fields (`blitz_rate`, `off_pass_block_win_rate`, `times_to_pressure_sec` — see `docs/sims/inputs/README.md` §4.5/§5) were investigated 2026-07-13 for real-data feasibility:

*   **`blitz_rate` — buildable now, deliberately deferred.** `nfl_data_py.import_ftn_data()` (FTN charting data, free, part of the existing `nflverse` pipeline this project already uses) has a real `n_blitzers` field per play, confirmed 0% null in a live 2024 pull. Only available from the **2022 season onward** (~3-4 seasons as of 2026), vs. the rest of `trench_dna.json`'s 2015-2024 depth — decided the sample is too thin to train against yet. **Revisit once 5+ seasons are available (~2027-2028 offseason).** When ready: join `import_ftn_data()` to the existing nflfastR PBP pull via `nflverse_game_id`/`nflverse_play_id`, aggregate to team-season `blitz_rate` (e.g. `mean(n_blitzers > 0)`), and add as a new Gate 2 (sacks) feature alongside `def_pressure_rate`/`def_sack_rate`/`sack_rate_allowed` (which Gate 2 already reads from `trench_dna.json` today — this is a straightforward feature addition, independent of the separate `trench_tiers_2025.json`→`trench_dna.json` migration below the sacks work in `AGENTS.md` §11.4, which affects the run/pass-block multipliers and rush-yards box-density feature, not Gate 2).
*   **`off_pass_block_win_rate` / `times_to_pressure_sec` — not obtainable through the current pipeline.** Confirmed directly: `nfl_data_py.import_ngs_data()` only supports `stat_type` in `{passing, rushing, receiving}` — no pressure/blocking category. Checked FTN's full charting schema too (29 columns) — no equivalent metric there either. Pass Block Win Rate specifically is a real NFL Next Gen Stats metric, but lives behind NFL's proprietary NGS API (`docs.ngs.nfl.com`), which requires direct integration with NGS Support — not reachable through the free `nflverse` pipeline. **If we want these, the path is a separate data pipeline/subscription (NGS API partnership, or a paid charting provider like PFF), not an extension of the current nflfastR/FTN-based build.** Worth periodically checking whether NGS opens broader public access.
