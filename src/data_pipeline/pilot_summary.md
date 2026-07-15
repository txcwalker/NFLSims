# pilot_summary.py — Multi-Week Standings Compiler

### Why do we need it
A single game simulation provides matchup-specific details, but to understand overall engine pacing, scoring baselines, and team strengths across a league season, we need to aggregate multi-week logs into standings and record variance charts.

### What is it doing
* **File Aggregation**: Concatenates game summaries and player projection CSVs across selected simulation weeks (e.g. Weeks 1–4 of a pilot season).
* **Expected Standings Computation**: Iterates through simulated outcomes to sum up expected win rates (xW), points for (PF), points against (PA), and total plays.
* **Monte Carlo Standings Projections**: Runs 1,000 parallel season-simulations using the game win rates, capturing win ceilings (90th percentile) and floors (10th percentile).
* **Standings Formatting**: Organizes all active teams into divisions, reporting their average weekly scoring, offensive yards, median records, and variance boundaries.

### Subject matter expertise utilized
* **Bayesian Standings Projection**: Translates individual simulated win probabilities into overall team win ranges rather than treating standings as fixed outcomes.
* **Yardage-to-Scoring Diagnostic**: Computes team yardage averages (YDS/G) against simulated scoring outputs (PF/G) to ensure offensive conversion ratios are aligned with league averages.

### Decisions made & metrics/reasoning used
* **Divisional Groupings**: Hardcodes the standard 8 NFL divisions to structure the text output standings cleanly.
* **10/90 Percentile Bounds**: Used a 10th and 90th percentile cutoff to define the variance range, giving a realistic envelope of best-case and worst-case season results.

### Why this metric vs alternatives
* **Expected Wins (xW) vs Simulated Standings**: Using a combination of average expected wins and a 1,000-pass Monte Carlo simulation is superior to a simple win-loss tally because it captures the true underlying statistical strength of each team, smoothing out scheduling noise.

### What project goal does this accomplish
Aggregates week-to-week simulation outputs into high-level standings, allowing the user to verify overall league balance and engine stability across multiple weeks.
