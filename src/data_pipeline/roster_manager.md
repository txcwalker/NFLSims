# roster_manager.py — Player DNA Ingest & Trait Generator

### Why do we need it
A physics simulation engine requires descriptive player inputs (target shares, carry shares, completion modifiers, time-to-throw, depth-of-target) to drive play execution. This script ingests raw play-by-play datasets and automatically compiles these traits for each player, updating rosters as the season progresses.

### What is it doing
* **Multi-Year Data Blend**: Loads baseline data (the previous year's play-by-play) and blends it with current-year data up to the requested maximum week.
* **Volume Distribution Calculation**: Computes each player's carry share (relative to team run plays) and target share (relative to team pass plays) to define how plays get distributed.
* **Position Identification**: Automatically assigns positions (`QB`, `RB`, `WR/TE`) based on pass-vs-rush attempt counts.
* **Efficiency Metric Extraction**: Compiles average depth of target (`adot`), average yards after catch (`yac`), yards per carry (`ypc`), completion percentages, and QB sack rates.
* **JSON Serialization**: Exports compiled attributes to organized JSON files (e.g. `data/rosters/KC_traits.json`).

### Subject matter expertise utilized
* **Statistical Damping**: Blends individual player yards per carry and depth of target with default league baselines (e.g. 8.5 adot, 4.5 yac, 4.2 ypc) using a 70/30 weighting to prevent small-sample size spikes.
* **Recency Decay**: Focuses on rolling context to ensure rosters represent current, active depth charts rather than stale season-opening structures.

### Decisions made & metrics/reasoning used
* **Value Clipping**: Restricts `adot` to [5.0, 14.0] and `yac` to [2.0, 7.0] to prevent extreme outlier data points from breaking game engine logic.
* **QB Scramble Default**: Hardcodes a baseline 5% scramble rate for QBs to model escape yardage during pass play breakdowns.

### Why this metric vs alternatives
* **Blended Rolling JSON vs Raw Averages**: Extracting standardized JSON traits rather than passing raw statistics ensures the simulation engine can look up player capabilities instantly in memory without having to parse large database tables during simulation runs.

### What project goal does this accomplish
Automates player profile generation from raw NFL play-by-play logs, providing the simulation engine with up-to-date, mathematically sound roster inputs.
