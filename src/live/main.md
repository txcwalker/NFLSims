# main.py — Live Bot Daemon Loop

### Why do we need it
To run a fully automated social media account that comments on fourth-down coaching decisions, we need an active daemon. It runs in the background, checks active NFL game statuses, fetches live play updates, runs simulations on new plays, and publishes posts.

### What is it doing
* **Scoreboard Polling**: Frequently fetches the live ESPN NFL scoreboard feed (checking for active, finished, or upcoming games).
* **Play Polling Loop**: For active games, queries full play-by-play logs, isolating newly run plays.
* **Deduplication Validation**: Interacts with the SQLite database to skip plays that have already been evaluated.
* **Simulation Bridge Trigger**: Dispatches new fourth-down plays to the Python-R simulation bridge to compute expected win probabilities for Go, Punt, and FG options.
* **Posting Decisions & Social Dispatch**: Evaluates completed simulations against the posting policy gates. For qualified plays, it generates the text post, publishes it to Bluesky/Mastodon, logs it in the database, and appends it to a daily CSV log archive.
* **Smart Sleep Pacing**: Dynamically adjusts polling rates (sleeping for 15s during active games, or sleeping until 20 minutes before kickoff when offline).

### Subject matter expertise utilized
* **Dynamic Pacing & Rate Control**: Integrates tight timing limits (e.g. min 15s polling during game day, up to 1-hour sleep cycles when offline) to stay within ESPN API rate thresholds.
* **Resilient Error Recovery**: Catches and logs all loop exceptions, letting the bot automatically recover and resume polling rather than crashing during live coverage.

### Decisions made & metrics/reasoning used
* **Standalone Primetime Identification**: Automatically detects if there is exactly one active game on the scoreboard (`is_standalone_prime`). This enables broader coverage for high-profile primetime slots (MNF, TNF, SNF).
* **Force Fetch Override**: Includes a `FORCE_FETCH` environment variable to test the polling loop on upcoming games when no live matches are active.

### Why this metric vs alternatives
* **FastAPI Daemon vs Scheduled Cron Job**: Selected a persistent loop daemon with smart sleep pacing rather than cron scheduling because live games require high-frequency polling (every 15 seconds) to comment on plays immediately, which cron cannot reliably coordinate.

### What project goal does this accomplish
Acts as the central orchestrator for the live bot, managing scoreboard monitoring, simulations, and social postings.
