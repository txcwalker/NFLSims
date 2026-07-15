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

from src.live.state_store import init_db, is_play_processed, mark_play_processed, log_post
from src.live.espn_adapter import parse_plays_to_fd_rows
from src.live.simulator_bridge import run_simulation
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

SCOREBOARD_URL = "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard"
PBP_URL = "https://site.api.espn.com/apis/site/v2/sports/football/nfl/playbyplay?event={game_id}"
CSV_CACHE_DIR = "data/live"


def fetch_scoreboard() -> Optional[Dict[str, Any]]:
    """
    Fetches the live NFL scoreboard from ESPN.
    """
    try:
        resp = requests.get(SCOREBOARD_URL, timeout=10)
        if resp.status_code == 200:
            return resp.json()
        logger.error(f"Scoreboard request failed: HTTP {resp.status_code}")
    except Exception as e:
        logger.error(f"Error fetching scoreboard: {e}")
    return None


def get_game_pbp(game_id: str) -> Optional[Dict[str, Any]]:
    """
    Fetches play-by-play logs for a specific game ID.
    """
    try:
        resp = requests.get(PBP_URL.format(game_id=game_id), timeout=10)
        if resp.status_code == 200:
            return resp.json()
        logger.error(f"PBP request for {game_id} failed: HTTP {resp.status_code}")
    except Exception as e:
        logger.error(f"Error fetching PBP for {game_id}: {e}")
    return None


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
        
        # Run Simulator
        sim_res = run_simulation(play)
        if "error" in sim_res:
            logger.error(f"Skipping play {play_id} due to simulator bridge error: {sim_res['error']}")
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
            
        # Deduplicate permanently in SQLite
        mark_play_processed(game_id, play_id)
        processed_count += 1
        
        # Gated Posting Policy Evaluation
        game_meta = {"is_standalone_prime": is_standalone_prime}
        should_post, reason = should_post_decision(play, game_meta)
        
        if should_post:
            logger.info(f"Play {play_id} matches posting gate: '{reason}'. Preparing social post.")
            
            # Revisionist mistake flag
            is_revisionist = (called_action != play["best_action"]) and (wp_gap >= 0.03)
            post_text = format_post(play, revisionist=is_revisionist)
            
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
