# post_targets.py — Social Posting Clients

### Why do we need it
To publish automated analytics alerts, the bot needs to authenticate and communicate with social media platforms. This file houses the official API integration clients for Bluesky and Mastodon.

### What is it doing
* **SDK Initialization**: Graciously imports and checks the availability of official Python SDKs: `atproto` (for Bluesky) and `Mastodon.py` (for Mastodon).
* **Mastodon Dispatch**: Authenticates using server URLs and access tokens to publish posts under the standard 500-character limit.
* **Bluesky Dispatch**: Logs in using user handles and app passwords to publish posts under the standard 300-character limit.
* **Dry Run Mode**: Checks environment variables (`DRY_RUN=1`) to redirect post outputs to standard stdout printing instead of making live API calls during testing.

### Subject matter expertise utilized
* **Character-Limit Clipping**: Gracefully truncates and appends ellipsis (`…`) to messages that exceed platform limits, preventing posting errors.
* **Graceful Fallbacks**: Uses try-except wraps around imports to let the daemon start up and run even if one of the platform SDK libraries is missing.

### Decisions made & metrics/reasoning used
* **Dry-Run Defaulting**: Treats missing credentials as dry-run fallbacks, printing to console instead of throwing connection errors.
* **Concurrent Post Dispatch**: Dispatches posts to both platforms simultaneously (`post_everywhere`), returning a combined dictionary of success states.

### Why this metric vs alternatives
* **Official Python SDKs vs Direct HTTP Requests**: Used official SDKs (`atproto` and `Mastodon.py`) rather than writing manual raw HTTP requests because the SDKs handle session refreshes and complex authentication headers out-of-the-box.

### What project goal does this accomplish
Exposes a clean API client that dispatches verified bot recommendations and analysis to Bluesky and Mastodon.
