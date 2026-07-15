# visuals.py — Simulation Data Visualizer

### Why do we need it
While raw statistical summaries are informative, visual charts (such as bell curves, point spreads, and scoring distributions) provide a much faster way to check simulation pacing and analyze player ceiling projections.

### What is it doing
* **KDE Fantasy Distributions**: Generates a kernel density estimate (KDE) plot of a specific player's fantasy points across all simulations, marking the median, 5th percentile (floor), and 95th percentile (ceiling).
* **Game Totals Histograms**: Creates histograms showing the combined scores of both teams across all simulated games, adding an average total line.
* **Spread Distribution Histograms**: Produces histograms showing the point spread distribution relative to a selected team.
* **Lazy Loading**: Imports plotting libraries (`matplotlib` and `seaborn`) inside methods rather than at the module level.

### Subject matter expertise utilized
* **KDE Bell Curves**: Employs kernel density estimates to smooth out fantasy scoring frequency bars, presenting a clean projection curve.
* **DraftKings & FanDuel Integrations**: Automatically shifts visualization columns (`dk_score` vs `fd_score`) based on the requested scoring rules.

### Decisions made & metrics/reasoning used
* **Lazy Import Strategy**: Matplotlib and Seaborn are lazy-imported locally inside methods. This prevents Windows child worker processes from loading Matplotlib at startup, bypassing DLL load failures during parallel runs.
* **Output Path Generation**: Automatically checks for and creates target chart folders (`reports/charts`) if they are missing.

### Why this metric vs alternatives
* **Kernel Density Estimate (KDE) vs Standard Histograms**: KDE smoothing is selected for fantasy scoring distributions because it presents a clean, continuous probability curve that makes it easy for users to spot floor and ceiling thresholds.

### What project goal does this accomplish
Generates statistical charts from simulation logs, helping users identify fantasy value ranges and point spread trends.
