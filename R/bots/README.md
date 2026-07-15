# R/bots/ — Live Game Bot Pipeline

Orchestrates live 4th down analysis during NFL games.
Entry points are called by `src/live/main.py` via subprocess or run standalone.

## Active Files

| File | Purpose |
|---|---|
| `run_fd_live_once.R` | One-shot: fetch → filter 4th downs → simulate → policy |
| `run_live_loop.R` | Continuous game watcher loop |
| `run_live_today.R` | One-shot daily runner |
| `run_live_chunk.R` | Processes a single chunk of plays |
| `posting_policy.R` | Rules for when/what to post |
| `post_targets.R` | Social media posting targets |
| `fetch_live_plays_espn.R` | ESPN play fetcher (wraps `R/live/espn_adapter.R`) |
| `fetch_upcoming_schedule.R` | Loads upcoming game schedule |
| `diagnose_setup.R` | Environment diagnostic — run locally to verify setup |

## Runtime Outputs

- `live_csv/` — Per-run CSV + log artifacts written by the bot (auto-cleared periodically)
- `logs/` — Runtime logs written by the bot daemon
