# scoring.py — Fantasy Points Processor

### Why do we need it
A key output of our NFL simulations is player fantasy projections. This script processes player game-by-game statistics to calculate accurate fantasy scores under standard DraftKings and FanDuel scoring rules.

### What is it doing
* **DraftKings (DK) Scoring**: Calculates fantasy points:
  * Passing yards (0.04/yd), passing touchdowns (4 pts), interceptions (-1 pt), and a +3 point bonus for reaching 300+ passing yards.
  * Rushing yards (0.1/yd), rushing touchdowns (6 pts), and a +3 point bonus for reaching 100+ rushing yards.
  * Receptions (1 pt), receiving yards (0.1/yd), receiving touchdowns (6 pts), and a +3 point bonus for reaching 100+ receiving yards.
  * Fumbles lost (-1 pt).
* **FanDuel (FD) Scoring**: Calculates fantasy points:
  * Passing yards (0.04/yd), passing touchdowns (4 pts), and interceptions (-2 pts). No yardage bonuses.
  * Rushing yards (0.1/yd) and rushing touchdowns (6 pts).
  * Receptions (0.5 pt), receiving yards (0.1/yd), and receiving touchdowns (6 pts).
  * Fumbles lost (-2 pts).
* **Usage Aggregation**: Sums rushes and catches to report player total touches, cleaning up any missing or NaN values.

### Subject matter expertise utilized
* **Milestone Bonuses**: Implements DraftKings yardage milestones (300+ passing, 100+ rushing, 100+ receiving) using threshold checks to match official contest scoring.
* **Point-Per-Reception (PPR) Variants**: Distinguishes between Full PPR (DraftKings) and Half PPR (FanDuel) rules.

### Decisions made & metrics/reasoning used
* **NaN Handling**: Proactively cleans and translates missing stats or NaNs to 0 before calculating scores, preventing calculation crashes.
* **Value Rounding**: Rounds final fantasy scores to exactly two decimal places.

### Why this metric vs alternatives
* **Programmatic Calculations vs Hardcoded Values**: Programmatic calculation allows the engine to evaluate fantasy scores dynamically on simulated play logs, making it easy to test custom scoring rules.

### What project goal does this accomplish
Translates physical simulation statistics into fantasy points, enabling accurate projections and DFS lineup analysis.
