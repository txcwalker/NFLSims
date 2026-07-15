# batch.py — Monte Carlo Simulation Dispatcher

### Why do we need it
A single game simulation is subject to high variance. To get a clear picture of expected outcomes, point spreads, win probabilities, and fantasy ceilings, we must run thousands of simulations. This file manages these large Monte Carlo runs and aggregates the resulting logs.

### What is it doing
* **Asset Loading**: Loads player DNA registries, team coaches, roster traits, and trench pressure ratings once per batch.
* **Vectorized Monte Carlo Runs**: Triggers vectorized simulations (`NFLGameEngine`) across thousands of games concurrently.
* **Sequential Monte Carlo Fallback**: Provides a reliable sequential run mode (`_simulate_single_game_worker`) when parallel processes are not desired.
* **Slot Mapping**: Computes player slot alignments (e.g. QB1, RB1, RB2, WR1, TE1) dynamically by sorting target/carry shares and roster details.
* **Statistical Aggregation**: Computes averages, medians, standard deviations, and 5th/95th percentile boundaries for game outcomes and player fantasy stats.

### Subject matter expertise utilized
* **Statistical Percentiles**: Generates 5th percentile (floor) and 95th percentile (ceiling) boundaries to capture the full range of fantasy outcomes, helping identify high-value sleepers and reliable options.
* **Windows Process Safety**: Restricts lazy-loaded plotting libraries in child worker processes to bypass Windows paging file DLL collisions.

### Decisions made & metrics/reasoning used
* **Dynamic Slot Assignment**: Automatically isolates starting Tight Ends based on historical keywords (e.g., "kelce", "kincaid") to separate WR and TE slot classifications.
* **Aggregator Caching**: Uses a precomputed player-to-slot dictionary to avoid repeating costly roster searches during statistical loops.

### Why this metric vs alternatives
* **Vectorized Batch vs Multiprocessing Pool**: Vectorized execution (`vectorized=True`) is selected because it runs thousands of games simultaneously in memory using NumPy, making it over 10x faster than standard multiprocessing pools.

### What project goal does this accomplish
Acts as the central coordination layer for Monte Carlo runs, converting raw play-by-play simulations into structured projections and distributions.
