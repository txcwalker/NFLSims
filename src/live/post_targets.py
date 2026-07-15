# src/live/post_targets.py
# Social Posting Clients for Bluesky and Mastodon.
# Uses official Python SDKs: atproto (Bluesky) and Mastodon.py (Mastodon).
# References: docs/database_guide.md, R/bots/post_targets.R
# ------------------------------------------------------------------------------

import os
import logging
from dotenv import load_dotenv

# Load env variables on module import
load_dotenv()

logger = logging.getLogger("4thDownBot.PostTargets")

# Optional SDK imports (wrapped to allow graceful error tracking if missing)
_ATPROTO_AVAILABLE = False
try:
    from atproto import Client
    _ATPROTO_AVAILABLE = True
except ImportError:
    logger.warning("atproto SDK not found. Bluesky posting will be unavailable.")

_MASTODON_AVAILABLE = False
try:
    from mastodon import Mastodon
    _MASTODON_AVAILABLE = True
except ImportError:
    logger.warning("Mastodon.py SDK not found. Mastodon posting will be unavailable.")


def is_dry_run() -> bool:
    """
    Returns True if DRY_RUN is active, printing posts to stdout instead of calling live APIs.
    """
    return os.getenv("DRY_RUN", "0") == "1"


def post_mastodon(text: str, char_limit: int = 500) -> bool:
    """
    Posts status to Mastodon instance using Mastodon.py.
    """
    # Trim to character limit
    safe_text = text[:char_limit-1] + "…" if len(text) > char_limit else text
    
    if is_dry_run():
        print(f"\n[DRY_RUN:Mastodon] Posting:\n{safe_text}\n{'-'*40}")
        return True

    server = os.getenv("MASTO_SERVER")
    token = os.getenv("MASTO_TOKEN")
    
    if not server or not token:
        logger.debug("Mastodon not configured: missing MASTO_SERVER or MASTO_TOKEN.")
        return False
        
    if not _MASTODON_AVAILABLE:
        logger.error("Cannot post to Mastodon: Mastodon.py SDK is not installed.")
        return False
        
    try:
        # Guarantee server scheme exists
        if not server.startswith("http://") and not server.startswith("https://"):
            server = f"https://{server}"
            
        masto = Mastodon(
            access_token=token,
            api_base_url=server
        )
        masto.status_post(safe_text)
        logger.info("Successfully posted to Mastodon.")
        return True
    except Exception as e:
        logger.error(f"Mastodon posting failure: {e}")
        return False


def post_bluesky(text: str, char_limit: int = 300) -> bool:
    """
    Posts status to Bluesky using the official atproto SDK.
    """
    # Trim to character limit
    safe_text = text[:char_limit-1] + "…" if len(text) > char_limit else text
    
    if is_dry_run():
        print(f"\n[DRY_RUN:Bluesky] Posting:\n{safe_text}\n{'-'*40}")
        return True

    handle = os.getenv("BSKY_HANDLE")
    password = os.getenv("BSKY_APP_PASSWORD")
    
    if not handle or not password:
        logger.debug("Bluesky not configured: missing BSKY_HANDLE or BSKY_APP_PASSWORD.")
        return False
        
    if not _ATPROTO_AVAILABLE:
        logger.error("Cannot post to Bluesky: atproto SDK is not installed.")
        return False
        
    try:
        client = Client()
        client.login(handle, password)
        client.send_post(safe_text)
        logger.info("Successfully posted to Bluesky.")
        return True
    except Exception as e:
        logger.error(f"Bluesky posting failure: {e}")
        return False


def post_everywhere(text: str) -> dict:
    """
    Simultaneously posts to both Mastodon and Bluesky.
    
    Returns:
        dict: Success state of each posting client.
    """
    masto_ok = post_mastodon(text)
    bsky_ok = post_bluesky(text)
    
    return {
        "mastodon": masto_ok,
        "bluesky": bsky_ok,
        "any": masto_ok or bsky_ok
    }
