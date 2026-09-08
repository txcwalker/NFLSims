# src/live/main.py
# Main Daemon Entry Point for NFL 4th Down Live Bot.
# References: docs/database_guide.md, R/bots/run_live_loop.R, R/bots/run_live_today.R
# ------------------------------------------------------------------------------

import os
import sys
import time
import logging
from datetime import datetime, timezone
from typing import Optional, Dict, Any
import requests
import csv

# Add the project root to path if running directly
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")))

from src.live.state_store import (
    init_db, is_play_processed, mark_play_processed, log_post, record_evaluated_play,
)
from src.live.espn_adapter import parse_plays_to_fd_rows
from src.live.decision_engine import evaluate_fourth_down
from src.live.posting_policy import should_post_decision, format_post
from src.live.post_targets import post_everywhere

# Configure Logging
os.makedirs("R/bots/logs", exist_ok=True)
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(name)s: %(message)s",
    handlers=[
        logging.StreamHandler(sys.stdout),
        logging.FileHandler(f"R/bots/logs/live_{datetime.utcnow().strftime('%Y%m%d')}.log")
    ]
)
logger = logging.getLogger("4thDownBot.Main")

ESPN_HOSTS = ["site.api.espn.com", "site.web.api.espn.com"]
SCOREBOARD_PATH = "/apis/site/v2/sports/football/nfl/scoreboard"
# NOTE: the lighter-weight /playbyplay endpoint was found (2026-08-14) to return
# an empty {} for at least some completed games (confirmed on a real finished
# preseason game) while still returning HTTP 200 -- /summary is more complete
# (includes drives + header in one call, matches what other ESPN-hidden-API
# consumers use) and was verified to reliably have real drive/play data on the
# same game. Used for both live polling and any completed-game lookups.
PBP_PATH = "/apis/site/v2/sports/football/nfl/summary"
CSV_CACHE_DIR = "data/live"

# ESPN's undocumented API has had intermittent, unannounced 403s since ~2026-08-05
# (a known, widely-reported issue, not something specific to us -- verified via
# burst testing that both site.api.espn.com and the site.web.api.espn.com
# fallback recover cleanly within a few retries). This helper absorbs both:
# transient failures get retried with backoff, and a host that's actively
# misbehaving gets swapped for the fallback host on the next attempt.
_ESPN_RETRIES = 3
_ESPN_BACKOFF_BASE = 1.0  # seconds; doubles each retry (1s, 2s, 4s)


def _espn_get(path: str, params: Optional[Dict[str, Any]] = None, timeout: int = 10) -> Optional[Dict[str, Any]]:
    """
    GETs an ESPN endpoint with retry-with-backoff and a fallback host, since
    the primary host has had intermittent unannounced 403s recently.
    """
    last_err = None
    for attempt in range(_ESPN_RETRIES):
        host = ESPN_HOSTS[min(attempt, len(ESPN_HOSTS) - 1)]
        url = f"https://{host}{path}"
        try:
            resp = requests.get(url, params=params, timeout=timeout)
            if resp.status_code == 200:
                return resp.json()
            last_err = f"HTTP {resp.status_code} from {host}"
            logger.warning(f"ESPN request to {host} failed (attempt {attempt + 1}/{_ESPN_RETRIES}): {last_err}")
        except Exception as e:
            last_err = str(e)
            logger.warning(f"ESPN request to {host} errored (attempt {attempt + 1}/{_ESPN_RETRIES}): {e}")

        if attempt < _ESPN_RETRIES - 1:
            time.sleep(_ESPN_BACKOFF_BASE * (2 ** attempt))

    logger.error(f"ESPN request to {path} failed after {_ESPN_RETRIES} attempts: {last_err}")
    return None


# Short-lived in-process cache for the two ESPN calls everything else is built
# on. Two independent reasons this exists:
#   1. A single GameSummary page load fires 5 detail endpoints (play-by-play,
#      stats, fourth-downs, player-stats, positional-eval) that each call
#      get_game_pbp() for the SAME game_id -- without this, that's 5 real
#      ESPN hits per page load, not 1.
#   2. The frontend's 30s live-game auto-refresh (added 2026-08-22) would
#      otherwise poll ESPN directly every 30s per open tab, on top of the
#      live bot's own polling once it's live -- ESPN's undocumented API has
#      had intermittent unannounced 403s under load (see _espn_get above),
#      so this is deliberately conservative.
# TTL is kept under the 30s auto-refresh interval so a poll always gets
# genuinely fresh data; it only dedupes calls that land within the same
# ~20s window (a page load's parallel fetches, or repeated navigation).
# Purely in-memory and intentionally NOT a growing log -- each cache write
# also prunes anything older than 10 minutes, so process memory stays
# bounded by "how many distinct games/dates you've looked at recently."
_CACHE_TTL_SECONDS = 20.0
_CACHE_PRUNE_AGE_SECONDS = 600.0
_scoreboard_cache: Dict[str, tuple] = {}  # key -> (fetched_at, data)
_pbp_cache: Dict[str, tuple] = {}  # game_id -> (fetched_at, data)


def _prune_stale(cache: Dict[str, tuple], now: float) -> None:
    stale_keys = [k for k, (fetched_at, _) in cache.items() if now - fetched_at > _CACHE_PRUNE_AGE_SECONDS]
    for k in stale_keys:
        del cache[k]


def fetch_scoreboard(dates: Optional[str] = None) -> Optional[Dict[str, Any]]:
    """
    Fetches the NFL scoreboard from ESPN. Defaults to today; pass dates as
    an ESPN-format YYYYMMDD string (e.g. "20260814") to fetch a specific
    day's slate instead -- used by /api/live-games?date= for reviewing a
    past day's slate without waiting for the next live window.

    Cached for _CACHE_TTL_SECONDS (see module comment above).
    """
    cache_key = dates or "__today__"
    now = time.time()
    cached = _scoreboard_cache.get(cache_key)
    if cached and (now - cached[0]) < _CACHE_TTL_SECONDS:
        return cached[1]

    params = {"dates": dates} if dates else None
    data = _espn_get(SCOREBOARD_PATH, params=params)
    if data is not None:
        _scoreboard_cache[cache_key] = (now, data)
        _prune_stale(_scoreboard_cache, now)
    return data


def get_game_pbp(game_id: str) -> Optional[Dict[str, Any]]:
    """
    Fetches play-by-play logs for a specific game ID.

    Cached for _CACHE_TTL_SECONDS (see module comment above).
    """
    now = time.time()
    cached = _pbp_cache.get(game_id)
    if cached and (now - cached[0]) < _CACHE_TTL_SECONDS:
        return cached[1]

    data = _espn_get(PBP_PATH, params={"event": game_id})
    if data is not None:
        _pbp_cache[game_id] = (now, data)
        _prune_stale(_pbp_cache, now)
    return data


def write_csv_archive(row: Dict[str, Any], reason: str) -> None:
    """
    Appends a logged play row to a rolling daily CSV file for historical record.
    """
    os.makedirs(CSV_CACHE_DIR, exist_ok=True)
    csv_path = os.path.join(CSV_CACHE_DIR, f"live_{datetime.utcnow().strftime('%Y%m%d')}.csv")
    
    # Flatten the row dictionary to a record row
    archive_row = {**row, "posted_flag": 1, "posted_reason": reason}
    
    # Exclude detailed simulation metadata structures to keep CSV clean
    for k in list(archive_row.keys()):
        if isinstance(archive_row[k], (dict, list)):
            del archive_row[k]
            
    file_exists = os.path.exists(csv_path)
    with open(csv_path, "a", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=archive_row.keys())
        if not file_exists:
            writer.writeheader()
        writer.writerow(archive_row)


def process_active_game(game_id: str, team_map: Dict[str, str], is_standalone_prime: bool) -> int:
    """
    Processes active play-by-play state for a game ID and runs simulator/posting checks.
    """
    pbp_data = get_game_pbp(game_id)
    if not pbp_data:
        return 0
        
    drives = pbp_data.get("drives", {})
    plays = []
    
    # Aggregate both previous and current plays
    previous_drives = drives.get("previous", [])
    for d in previous_drives:
        plays.extend(d.get("plays", []))
        
    current_drive = drives.get("current", {})
    if current_drive and "plays" in current_drive:
        plays.extend(current_drive.get("plays", []))
        
    if not plays:
        return 0
        
    # Extract only new, unseen 4th downs
    normalized_plays = parse_plays_to_fd_rows(game_id, plays, team_map)
    processed_count = 0
    
    for play in normalized_plays:
        play_id = play["play_id"]
        
        # Deduplication check
        if is_play_processed(game_id, play_id):
            continue
            
        logger.info(f"New 4th Down play detected: Game {game_id}, Play {play_id} - {play['text']}")

        # Run Simulator (in-process Python decision engine -- see decision_engine.py)
        try:
            sim_res = evaluate_fourth_down(play)
        except Exception as e:
            logger.error(f"Skipping play {play_id} due to decision engine error: {e}", exc_info=True)
            # Mark as processed to prevent endless loops on broken plays
            mark_play_processed(game_id, play_id)
            continue
            
        # Merge simulator outputs back into play row
        play["wp_go"] = sim_res.get("go_for_it_ev")
        play["wp_punt"] = sim_res.get("punt_wp")
        play["wp_fg"] = sim_res.get("field_goal_ev")
        play["best_action"] = sim_res.get("recommendation", {}).get("action")
        play["punt_suppressed"] = sim_res.get("punt_suppressed", False)
        play["fg_suppressed"] = sim_res.get("fg_suppressed", False)
        
        best_wp = sim_res.get("recommendation", {}).get("wp")
        called_action = play.get("called_action")
        wp_called = None
        if called_action == "go":
            wp_called = play["wp_go"]
        elif called_action == "fg":
            wp_called = play["wp_fg"]
        elif called_action == "punt":
            wp_called = play["wp_punt"]
            
        wp_gap = 0.0
        if wp_called is not None and wp_called != "NA" and best_wp is not None and best_wp != "NA":
            wp_gap = float(best_wp) - float(wp_called)
        play["wp_gap"] = wp_gap

        # Deduplicate permanently in SQLite
        mark_play_processed(game_id, play_id)
        processed_count += 1

        # Gated Posting Policy Evaluation
        game_meta = {"is_standalone_prime": is_standalone_prime}
        should_post, reason = should_post_decision(play, game_meta)

        # Revisionist mistake flag -- lowercase both sides. called_action comes
        # from espn_adapter.infer_called_action() ("go"/"fg"/"punt"); best_action
        # comes from decision_engine's recommendation (also lowercase as of the
        # Python port, but compare defensively in case that ever changes).
        is_revisionist = (
            str(called_action or "").lower() != str(play.get("best_action") or "").lower()
        ) and (wp_gap >= 0.03)
        post_text = format_post(play, revisionist=is_revisionist)

        # Full audit trail: every evaluated 4th down gets saved for review,
        # regardless of whether the posting-policy gate approved it.
        record_evaluated_play(play, sim_res, should_post, reason, post_text)

        if should_post:
            logger.info(f"Play {play_id} matches posting gate: '{reason}'. Preparing social post.")

            # Post everywhere!
            post_results = post_everywhere(post_text)

            if post_results.get("any"):
                # Record successful post and append to CSV archive
                log_post(game_id, play["drive_id"], play_id, reason, wp_gap)
                write_csv_archive(play, reason)
                logger.info(f"Social post successfully published for play {play_id}.")
        else:
            logger.info(f"Play {play_id} evaluated but skipped (Reason: {reason}).")

    return processed_count


def parse_kickoff_time(kickoff_str: str) -> Optional[datetime]:
    """
    Parses ESPN UTC Kickoff string into Python UTC Datetime.
    """
    try:
        # e.g., "2026-09-10T23:20Z"
        clean = kickoff_str.replace("Z", "+00:00")
        return datetime.fromisoformat(clean)
    except Exception as e:
        logger.warning(f"Error parsing kickoff time {kickoff_str}: {e}")
        return None


def run_live_daemon() -> None:
    """
    Main running execution loop. Manages scheduling, active polling, and sleep states.
    """
    init_db()
    logger.info("NFL 4th Down Bot Daemon started.")
    
    # Check FORCE_FETCH override
    force_fetch = os.getenv("FORCE_FETCH", "false").lower() == "true"
    
    while True:
        try:
            sb = fetch_scoreboard()
            if not sb:
                logger.warning("Scoreboard offline. Sleeping for 60 seconds.")
                time.sleep(60)
                continue
                
            events = sb.get("events", [])
            active_games = []
            upcoming_games = []
            
            for ev in events:
                comp = ev.get("competitions", [{}])[0]
                status_state = comp.get("status", {}).get("type", {}).get("state", "").lower()
                
                # Extract competitors
                competitors = comp.get("competitors", [])
                team_map = {}
                for comp_team in competitors:
                    side = comp_team.get("homeAway")
                    abbr = comp_team.get("team", {}).get("abbreviation")
                    if side and abbr:
                        team_map[side] = abbr
                        
                event_info = {
                    "id": str(ev.get("id")),
                    "name": ev.get("name"),
                    "state": status_state,
                    "date": ev.get("date"),
                    "team_map": team_map
                }
                
                if status_state == "in":
                    active_games.append(event_info)
                elif status_state == "pre":
                    upcoming_games.append(event_info)
                    
            # 1. Active Game Mode (Live Game Day)
            if active_games or force_fetch:
                # If forcing without active games, simulate with the first upcoming event
                if force_fetch and not active_games:
                    if upcoming_games:
                        target = upcoming_games[0]
                        logger.info(f"[FORCE_FETCH] Simulating play polling for upcoming event: {target['name']}")
                        process_active_game(target["id"], target["team_map"], is_standalone_prime=True)
                    else:
                        logger.warning("[FORCE_FETCH] Force fetch active, but no events found on scoreboard.")
                else:
                    is_standalone_prime = len(active_games) == 1
                    logger.info(f"Active games detected: {len(active_games)}. Entering high-speed live poll.")
                    
                    for game in active_games:
                        try:
                            process_active_game(game["id"], game["team_map"], is_standalone_prime=is_standalone_prime)
                        except Exception as e:
                            logger.error(f"Error processing game {game['id']}: {e}", exc_info=True)
                            
                # Sleep for 15-20 seconds as requested in specifications
                time.sleep(15)
                
            # 2. Offline / Sleep Mode
            else:
                if upcoming_games:
                    # Find the earliest kickoff today
                    now = datetime.now(timezone.utc)
                    earliest_kickoff = None
                    for g in upcoming_games:
                        k_time = parse_kickoff_time(g["date"])
                        if k_time and k_time > now:
                            if earliest_kickoff is None or k_time < earliest_kickoff:
                                earliest_kickoff = k_time
                                
                    if earliest_kickoff:
                        # Sleep until 20 minutes before the earliest kickoff
                        sleep_delta = earliest_kickoff - now
                        sleep_secs = int(sleep_delta.total_seconds()) - (20 * 60)
                        
                        if sleep_secs > 60:
                            logger.info(f"No active games. Next kickoff in {sleep_delta}. Sleeping for {sleep_secs} seconds.")
                            time.sleep(min(3600, sleep_secs))  # Cap sleep at 1 hour to check for updates
                            continue
                            
                # Fallback standard sleep
                logger.info("No active games or immediate upcoming kickoffs. Sleeping for 1 hour.")
                time.sleep(3600)
                
        except KeyboardInterrupt:
            logger.info("Daemon interrupted by user. Exiting cleanly.")
            sys.exit(0)
        except Exception as e:
            logger.error(f"Fatal loop exception: {e}", exc_info=True)
            time.sleep(60)


if __name__ == "__main__":
    run_live_daemon()
