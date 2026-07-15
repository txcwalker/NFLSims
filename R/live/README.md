# R/live/ — ESPN Data Adapter

Contains the core R-side ESPN scraper. Sourced by `R/bots/fetch_live_plays_espn.R`.

| File | Purpose |
|---|---|
| `espn_adapter.R` | Fetches scoreboard and play-by-play from ESPN's unofficial API. Normalizes plays to a stable schema with guaranteed `down` column. Includes retry/backoff, dedup, and robust JSON parsing. |

> [!NOTE]
> All test and debug scripts (`test_espn_*.R`, `debug_espn_structure.R`, etc.) have been removed.
> The Python-side equivalent lives in `src/live/espn_adapter.py`.
