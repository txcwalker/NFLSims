# state_store.py — SQLite Database State Manager

### Why do we need it
A real-time bot polling ESPN API feeds needs a persistent storage layer to remember which plays have already been processed (preventing infinite posting loops), which drives have had updates, and when social posts were published to stay within rate limits.

### What is it doing
* **Database Initialization**: Sets up the SQLite database (`data/live/bot_state.db`) and builds the schema tables (`processed_plays` and `post_history`) if they are missing.
* **Play Deduplication**: Records seen play IDs in `processed_plays` and checks against it to ensure the bot never processes the same play twice.
* **Game Cooldown Checks**: Queries recent post history to enforce game-level cooldown limits (e.g. 45–60s), ensuring the bot does not flood the feed with consecutive posts.
* **Drive Gating**: Validates if a specific drive ID has already been posted to prevent consecutive updates on the same drive.
* **Global Rate Limiting**: Tracks global postings over the last 60 minutes to prevent the bot from exceeding a hard threshold of 12 posts per hour.

### Subject matter expertise utilized
* **Transaction Safety**: Integrates clean connection openings and structural `try/finally` blocks to guarantee SQLite connections are closed, preventing database lockups on high-frequency live loops.
* **Timestamp Comparisons**: Uses UTC comparisons (`datetime.utcnow()`) to evaluate rate intervals, bypassing local timezone issues on different servers.

### Decisions made & metrics/reasoning used
* **Separate Log Tables**: Separates seen plays from actual social postings, keeping the deduplication table fast and indexing small, while maintaining rich metadata in the posting history table.
* **Drive Block**: Implemented an absolute drive-block gate. Once a drive is posted, no further plays on that same drive will trigger posts, preventing clutter.

### Why this metric vs alternatives
* **SQLite vs JSON/File State**: Selected SQLite over a simple JSON state file because database transactions are thread-safe, handle concurrent reads/writes without data loss, and allow fast index lookups on growing logs.

### What project goal does this accomplish
Acts as the memory and state manager of the live bot, preventing duplicate analyses, enforcing rate limits, and securing transaction logs.
