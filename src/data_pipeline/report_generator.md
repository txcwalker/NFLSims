# report_generator.py — Weekly Simulation Report Formatter

### Why do we need it
While raw CSV logs are excellent for database storage and programmatic parsing, they are difficult for users to inspect by hand. This tool formats raw game and player data into clean, structured, human-readable text box scores.

### What is it doing
* **Input Ingestion**: Loads weekly simulation summary CSVs (`week_X_hybrid_game_summary.csv` and `week_X_hybrid_player_projections.csv`).
* **Matchup Segmentation**: Loops through simulated games, grouping players by their game IDs and sorting them by their average DraftKings projected fantasy scores.
* **Format Generation**: Compiles an organized box-score log text file:
  * Game header with average plays, final scores, and win rates.
  * Tabular roster breakdown listing top-15 players with their positions, teams, projected fantasy scores, yards, touchdowns, and specific usage formats (e.g. completions/attempts for QBs, rushes/targets for skill players).

### Subject matter expertise utilized
* **Dynamic Position Formatting**: Tailors the reported usage metrics: QBs display completions, attempts, and rushes; skill players (RBs/WRs/TEs) display rushes, catches, and targets.
* **High-Value Player Highlighting**: Sorts lists by projected fantasy score (Avg DK) to highlight high-value options for DFS lineup building.

### Decisions made & metrics/reasoning used
* **Touchdowns and Yards Aggregation**: Sums passing, rushing, and receiving stats to present simple, consolidated offensive production metrics (`Avg_Yds`, `Avg_TDs`).
* **Text Output Layout**: Uses fixed column widths (e.g. `{'PLAYER':<20}`) to ensure alignment when viewed in raw text readers.

### Why this metric vs alternatives
* **Text Format vs JSON/HTML**: Text files were selected to provide zero-overhead, instantly readable logs that git can track easily as diffs without needing a separate browser rendering pass.

### What project goal does this accomplish
Formats raw week-to-week simulation outputs into readable box scores, allowing quick review of game tempos and player workloads.
