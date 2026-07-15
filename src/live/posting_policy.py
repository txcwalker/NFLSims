# src/live/posting_policy.py
# Social Posting Policy & Narrative Formatter for 4th Down Bot.
# References: docs/database_guide.md, R/bots/posting_policy.R
# ------------------------------------------------------------------------------

import logging
from typing import Dict, Any, Tuple
from src.live.state_store import is_game_in_cooldown, is_drive_posted, get_global_posts_last_hour

logger = logging.getLogger("4thDownBot.PostingPolicy")


def should_post_decision(row: Dict[str, Any], game_meta: Dict[str, Any], db_path: str = "data/live/bot_state.db") -> Tuple[bool, str]:
    """
    Evaluates whether a 4th down decision matches our gated posting policies.
    
    Returns:
        A tuple of (should_post: bool, reason: str).
    """
    game_id = str(row.get("game_id"))
    drive_id = str(row.get("drive_id") or f"d_{row.get('play_id')}")
    
    # Extract predicted Win Probabilities
    wp_go = row.get("wp_go")
    wp_punt = row.get("wp_punt")
    wp_fg = row.get("wp_fg")
    
    # Clean and filter NAs from WPs
    wps = {}
    if wp_go is not None and wp_go != "NA":
        wps["go"] = float(wp_go)
    if wp_punt is not None and wp_punt != "NA" and not row.get("punt_suppressed"):
        wps["punt"] = float(wp_punt)
    if wp_fg is not None and wp_fg != "NA" and not row.get("fg_suppressed"):
        wps["fg"] = float(wp_fg)
        
    if not wps:
        return False, "no_valid_predictions"
        
    wp_best = max(wps.values())
    best_action = max(wps, key=wps.get)
    
    called_action = str(row.get("called_action") or "").lower()
    wp_called = wps.get(called_action)
    
    wp_gap = 0.0
    if wp_called is not None:
        wp_gap = wp_best - wp_called
        
    # 1. Gate 1: Mistakes (errors >= 3% WP)
    # Always eligible, always bypasses obviousness blocks.
    clear_mistake = (called_action in wps) and (best_action != called_action) and (wp_gap >= 0.03)
    
    # Calculate Leverage
    leverage = wp_best - (sum(wps.values()) / len(wps))
    
    # 2. Obvious Situation blocks (only suppress correct coaching actions)
    spread = max(wps.values()) - min(wps.values()) if len(wps) >= 2 else 0.0
    obvious_spread_block = spread >= 0.30
    must_go_block = ("punt" in wps) and (wps["punt"] < 0.10)
    obvious_block = obvious_spread_block or must_go_block
    
    # Check Context gates
    off_score = int(row.get("off_score") or 0)
    def_score = int(row.get("def_score") or 0)
    margin = abs(off_score - def_score)
    qtr = int(row.get("qtr") or 1)
    
    one_score_game = margin <= 8
    
    # Check Standalone Primetime (Gate 3)
    # Passed via game_meta from main daemon tracking active scoreboards
    is_standalone_prime = bool(game_meta.get("is_standalone_prime", False))
    
    # Eligibility checks
    eligible = False
    reason = "none"
    
    if clear_mistake:
        eligible = True
        reason = "clear_mistake"
    elif is_standalone_prime:
        # Standalone gets coverage for all non-obvious plays
        if not obvious_block:
            eligible = True
            reason = "prime_standalone"
    elif one_score_game:
        # Standard one-possession leverage coverage
        if not obvious_block and leverage >= 0.04:
            eligible = True
            reason = "one_score_leverage"
            
    if not eligible:
        return False, "not_eligible"
        
    # 3. Cooldowns & Rate Limits
    # Mistakes bypass the standard game-level cooldown to post instantly
    is_masto_bsky_prime = is_standalone_prime or game_meta.get("window") in ("MNF", "TNF", "SNF")
    cd_secs = 45 if is_masto_bsky_prime else 60
    
    if is_game_in_cooldown(game_id, cd_secs, db_path=db_path) and not clear_mistake:
        return False, "game_cooldown_block"
        
    # Drive block is absolute (prevents duplicate plays on same drive)
    if is_drive_posted(game_id, drive_id, db_path=db_path):
        return False, "drive_already_posted"
        
    # Global hourly rate limit is absolute (max 12 posts per hour)
    if get_global_posts_last_hour(db_path=db_path) >= 12:
        return False, "global_rate_limit_block"
        
    return True, reason


def format_post(row: Dict[str, Any], revisionist: bool = False) -> str:
    """
    Formats the textual post narrative for Mastodon and Bluesky.
    """
    qtr = int(row.get("qtr") or 1)
    q_label = "OT" if qtr >= 5 else f"Q{qtr}"
    clock = row.get("clock") or "?:??"
    
    off = row.get("off") or "Offense"
    def_team = row.get("def") or "Defense"
    off_score = row.get("off_score")
    def_score = row.get("def_score")
    
    score_str = f"{off} {off_score} vs {def_team} {def_score}" if off_score is not None and def_score is not None else "Score N/A"
    
    # Format Win Probabilities
    parts = []
    wp_fg = row.get("wp_fg")
    wp_go = row.get("wp_go")
    wp_punt = row.get("wp_punt")
    
    if wp_fg is not None and wp_fg != "NA" and not row.get("fg_suppressed"):
        parts.append(f"FG {round(float(wp_fg)*100)}%")
    if wp_go is not None and wp_go != "NA":
        parts.append(f"GO {round(float(wp_go)*100)}%")
    if wp_punt is not None and wp_punt != "NA" and not row.get("punt_suppressed"):
        parts.append(f"PUNT {round(float(wp_punt)*100)}%")
        
    all_line = "  ".join(parts)
    
    yardline_100 = row.get("yardline_100")
    if yardline_100 is None or yardline_100 == "NA":
        yard_str = "Own ?"
    else:
        y100 = int(float(yardline_100))
        yard_str = f"Opp {y100}" if y100 <= 50 else f"Own {100 - y100}"
        
    header = f"4th & {row.get('ydstogo')} at {yard_str} | {score_str} | {q_label} {clock} ({off} ball)"
    
    if revisionist:
        called = str(row.get("called_action") or "go").lower()
        best = str(row.get("best_action") or "go").lower()
        
        # WP values
        wp_called_val = row.get(f"wp_{called}")
        wp_best_val = row.get(f"wp_{best}")
        
        called_pct = f"{round(float(wp_called_val)*100)}%" if wp_called_val is not None and wp_called_val != "NA" else "N/A"
        best_pct = f"{round(float(wp_best_val)*100)}%" if wp_best_val is not None and wp_best_val != "NA" else "N/A"
        
        delta_pct = "N/A"
        if wp_best_val is not None and wp_best_val != "NA" and wp_called_val is not None and wp_called_val != "NA":
            delta_pct = f"{round(abs(float(wp_best_val) - float(wp_called_val))*100)}%"
            
        action_names = {"go": "Go For It", "fg": "Field Goal", "punt": "Punt"}
        called_name = action_names.get(called, called.upper())
        best_name = action_names.get(best, best.upper())
        
        correct_str = "CORRECT" if called == best else "INCORRECT"
        
        summary_line = (
            f"The {off} made the {correct_str} decision. "
            f"{called_name} gave them a {called_pct} win probability, "
            f"whereas {best_name} would have given them {best_pct} "
            f"(a delta of {delta_pct})."
        )
    else:
        best = str(row.get("best_action") or "go").lower()
        wp_best_val = row.get(f"wp_{best}")
        best_pct = f"{round(float(wp_best_val)*100)}%" if wp_best_val is not None and wp_best_val != "NA" else "N/A"
        rec_act = best.upper()
        summary_line = f"Model Recommendation: {rec_act} (WP {best_pct})"
        
    lines = [header, summary_line]
    if all_line:
        lines.append(f"All Scenarios: {all_line}")
        
    return "\n".join(lines) + "\n#nfl #4thDown #analytics"
