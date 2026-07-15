# Data Pipeline Package (`src/data_pipeline/`)

This package houses components that ingest raw play-by-play data, build and manage team traits, and compile multi-week simulation logs into divisional standings reports.

## Directory Structure

```
src/data_pipeline/
├── README.md             # This documentation file
├── backtest_weights.py   # Backtester comparing season vs L4 recency blending weights
├── backtest_weights.md   # Documentation for backtest_weights.py
├── pilot_summary.py      # Aggregates multi-week logs into standings and win record variance reports
├── pilot_summary.md      # Documentation for pilot_summary.py
├── report_generator.py   # Formats weekly game outcomes and top players into clean text summaries
├── report_generator.md   # Documentation for report_generator.py
├── roster_manager.py     # Generates rolling player DNA JSON files using recency metrics
└── roster_manager.md     # Documentation for roster_manager.py
```

## Description of Files

* **[`backtest_weights.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/data_pipeline/backtest_weights.py)**:
  * Evaluates different blending proportions for player DNA traits by checking simulation outcomes against real-world fantasy scoring.

* **[`pilot_summary.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/data_pipeline/pilot_summary.py)**:
  * Compiles multi-week simulation outputs, aggregates standings, and runs season outcome projections.

* **[`report_generator.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/data_pipeline/report_generator.py)**:
  * Summarizes simulated game results and player usage distributions into structural weekly reports.

* **[`roster_manager.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/data_pipeline/roster_manager.py)**:
  * Build rolling player DNA maps by blending baseline stats with current-year recency weights.
