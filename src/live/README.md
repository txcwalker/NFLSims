# Live Bot Package (`src/live/`)

This package contains the automated live game-day daemon, scoreboard scraper, database tracking state, and social posting pipeline for the NFL 4th Down Decision Bot.

## Directory Structure

```
src/live/
├── README.md           # This documentation file
├── espn_adapter.py     # Parses ESPN live play-by-play payloads into 4th down schema rows
├── espn_adapter.md     # Documentation for espn_adapter.py
├── main.py             # Main daemon loop orchestrating polling, runs, and posting schedules
├── main.md             # Documentation for main.py
├── post_targets.py     # Bluesky (atproto) and Mastodon (Mastodon.py) API posting clients
├── post_targets.md     # Documentation for post_targets.py
├── posting_policy.py   # Evaluates decision errors and formats narrative social posts
├── posting_policy.md   # Documentation for posting_policy.py
├── simulator_bridge.py # Process bridge spawning R simulations from Python
├── simulator_bridge.md # Documentation for simulator_bridge.py
├── state_store.py      # SQLite database manager tracking seen plays, cooldowns, and rate limits
└── state_store.md      # Documentation for state_store.py
```

## Description of Files

* **[`espn_adapter.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/live/espn_adapter.py)**:
  * Translates raw ESPN API scoreboard feeds into structured 4th down game states, resolving possessions and field locations.

* **[`main.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/live/main.py)**:
  * Runs the primary execution loop, managing game-day polling intervals and invoking the simulator and posting pipelines.

* **[`post_targets.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/live/post_targets.py)**:
  * Dispatches verified postings to Bluesky and Mastodon, handling credentials and dry-run outputs.

* **[`posting_policy.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/live/posting_policy.py)**:
  * Contains the criteria gates (e.g., mistakes >= 3% WP) that determine if a play is worth posting, and drafts the text templates.

* **[`simulator_bridge.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/live/simulator_bridge.py)**:
  * Spawns an R subprocess to run the simulation models and return expected win probabilities.

* **[`state_store.py`](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFL_Exploration/src/live/state_store.py)**:
  * Interacts with SQLite to ensure no play is processed twice, no drive has duplicate posts, and posting rates stay within limits.
