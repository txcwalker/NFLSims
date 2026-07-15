# src/live/espn_adapter.py
# ESPN Scoreboard and Play-by-Play Scraper and Parser for 4th Down Bot.
# References: docs/database_guide.md, R/live/espn_adapter.R
# ------------------------------------------------------------------------------

import logging
import re
from typing import Dict, Any, List, Tuple, Optional

logger = logging.getLogger("4thDownBot.EspnAdapter")


def parse_possession_text(text: str, default_team: Optional[str] = None) -> Tuple[Optional[str], Optional[int]]:
    """
    Parses ESPN possession text (e.g. 'KC 45', '50', 'MID 50', 'MIDFIELD') to extract
    the possession team abbreviation and the yardline location.
    """
    if not text:
        return None, None
        
    text = text.strip()
    parts = text.split()
    
    if len(parts) >= 2:
        # Check for midfield variants like "MID 50" or "MIDFIELD 50"
        if parts[0].upper() in ("MID", "MIDFIELD"):
            digits = "".join(c for c in parts[1] if c.isdigit())
            yard = int(digits) if digits else 50
            return default_team, yard
            
        # Standard format: KC 45
        team = parts[0].upper()
        digits = "".join(c for c in parts[1] if c.isdigit())
        yard = int(digits) if digits else None
        return team, yard
        
    elif len(parts) == 1:
        val = parts[0].upper()
        if val.isdigit():
            # e.g., '50'
            return default_team, int(val)
        elif val in ("50", "MIDFIELD", "MID"):
            return default_team, 50
        else:
            # Single team string (e.g., 'KC')
            return val, None
            
    return None, None


def compute_yardline_100(posteam: Optional[str], poss_team: Optional[str], poss_yard: Optional[int]) -> Optional[int]:
    """
    Translates field spot (KC 45) to yards from opponent endzone (yardline_100).
    """
    if poss_yard is None:
        return None
        
    if not posteam or not poss_team:
        # Fallback to assuming it's opponent yardline
        return poss_yard
        
    if posteam.upper() == poss_team.upper():
        # Marked on our side of the field -> we are 100 - yard yards away from opponent endzone
        return 100 - poss_yard
    else:
        # Marked on defteam's side -> we are yard yards away
        return poss_yard


def get_game_seconds_remaining(period: int, clock_display: str) -> int:
    """
    Converts quarter clock (MM:SS) and quarter number to total game seconds remaining.
    """
    if not clock_display or ":" not in clock_display:
        return 0
        
    try:
        parts = clock_display.split(":")
        mm = int(parts[0])
        ss = int(parts[1])
        qtr_seconds = mm * 60 + ss
        
        if period <= 4:
            return (4 - period) * 900 + qtr_seconds
        else:
            return qtr_seconds
    except Exception as e:
        logger.warning(f"Error parsing clock {clock_display} for period {period}: {e}")
        return 0


def infer_down_from_text(text: str) -> Optional[int]:
    """
    Fallback parser to extract down number from text description when missing in metadata.
    """
    if not text:
        return None
    text = text.lower()
    if "1st" in text or "first down" in text:
        return 1
    if "2nd" in text or "second down" in text:
        return 2
    if "3rd" in text or "third down" in text:
        return 3
    if "4th" in text or "fourth down" in text:
        return 4
    return None


def infer_called_action(play_type: str) -> Optional[str]:
    """
    Infers the actual called action on the 4th down play.
    """
    pt = play_type.lower()
    if "punt" in pt:
        return "punt"
    if "field_goal" in pt or "field goal" in pt or "kick" in pt:
        return "fg"
    if any(action in pt for action in ("run", "pass", "kneel", "spike", "sack", "scramble")):
        return "go"
    return None


def parse_plays_to_fd_rows(game_id: str, plays: List[Dict[str, Any]], team_map: Dict[str, str], season: Optional[int] = None, week: Optional[int] = None) -> List[Dict[str, Any]]:
    """
    Normalizes ESPN play-by-play data to a standardized 4th down schema.
    """
    home_abbr = team_map.get("home", "").upper()
    away_abbr = team_map.get("away", "").upper()
    
    normalized_rows = []
    
    for p in plays:
        play_id = str(p.get("id"))
        if not play_id:
            continue
            
        # Parse text and description
        text = p.get("text") or p.get("description") or ""
        
        # Parse period / quarter
        period = 1
        period_obj = p.get("period")
        if isinstance(period_obj, dict):
            period = int(period_obj.get("number") or 1)
        elif period_obj is not None:
            period = int(period_obj)
            
        # Parse clock
        clock_display = ""
        clock_obj = p.get("clock")
        if isinstance(clock_obj, dict):
            clock_display = clock_obj.get("displayValue") or ""
        elif clock_obj is not None:
            clock_display = str(clock_obj)
            
        # Extract down
        down = None
        start = p.get("start") or {}
        if "down" in start:
            down = start.get("down")
        elif "shortDownDistanceText" in start:
            sddt = str(start.get("shortDownDistanceText") or "").lower()
            if sddt.startswith("1"): down = 1
            elif sddt.startswith("2"): down = 2
            elif sddt.startswith("3"): down = 3
            elif sddt.startswith("4"): down = 4
            
        if down is None:
            down = infer_down_from_text(text)
            
        # We only keep 4th down plays for the decision bot
        if down != 4:
            continue
            
        # Parse possession text
        team_abbr = (p.get("team", {}).get("abbreviation") or "").upper()
        
        start_poss_text = (
            start.get("possessionText") or
            start.get("possession", {}).get("displayValue") or
            start.get("team", {}).get("abbreviation") or
            team_abbr
        )
        
        # Resolve possession team and yardline using midfield adapter
        poss_team, poss_yard = parse_possession_text(start_poss_text, default_team=team_abbr)
        posteam = team_abbr or poss_team
        if not posteam:
            continue
            
        posteam = posteam.upper()
        defteam = away_abbr if posteam == home_abbr else home_abbr
        
        yardline_100 = compute_yardline_100(posteam, poss_team, poss_yard)
        if yardline_100 is None:
            continue
            
        ydstogo = start.get("distance")
        if ydstogo is None:
            continue
            
        # Scores
        start_home = start.get("homeScore")
        start_away = start.get("awayScore")
        
        posteam_score = start_home if posteam == home_abbr else start_away
        defteam_score = start_away if posteam == home_abbr else start_home
        
        score_diff = 0
        if posteam_score is not None and defteam_score is not None:
            score_diff = int(posteam_score) - int(defteam_score)
            
        # Called play type
        play_type_text = p.get("type", {}).get("text") or ""
        called_action = infer_called_action(play_type_text)
        
        # Quarter seconds clock derivation
        sec_left = get_game_seconds_remaining(period, clock_display)
        
        normalized_row = {
            "game_id": game_id,
            "drive_id": p.get("driveId") or f"d_{play_id}",
            "play_id": play_id,
            "season": season,
            "week": week,
            "qtr": period,
            "game_seconds_remaining": sec_left,
            "clock": clock_display,
            "off": posteam,
            "def": defteam,
            "off_score": posteam_score,
            "def_score": defteam_score,
            "margin": abs(score_diff),
            "score_differential": score_diff,
            "down": 4,
            "ydstogo": int(ydstogo),
            "yardline_100": float(yardline_100),
            "called_action": called_action,
            "play_type": play_type_text,
            "text": text
        }
        normalized_rows.append(normalized_row)

    return normalized_rows


def parse_plays_to_states(plays: List[Dict[str, Any]], home_abbr: str, away_abbr: str) -> List[Dict[str, Any]]:
    """
    Inputs:
        plays      : list of raw ESPN play dicts (from drives.previous/current[].plays)
        home_abbr  : home team abbreviation (e.g. "KC")
        away_abbr  : away team abbreviation (e.g. "BUF")

    Outputs:
        list of per-play game-state dicts, one per valid scrimmage play, each with:
          play_id, qtr, clock, game_seconds_remaining, off, def, down, ydstogo,
          yardline_100, score_differential (offense perspective), text

    Purpose:
        Sibling of parse_plays_to_fd_rows that keeps ALL scrimmage plays (downs 1-4),
        not just 4th downs. Feeds the positional evaluator's per-play KEP/EP sequence
        for the in-game evaluation bar. Reuses the same field/clock/possession primitives.
        Plays with unresolvable down, yardline, or distance are skipped.
    """
    home_abbr = (home_abbr or "").upper()
    away_abbr = (away_abbr or "").upper()
    states = []

    for p in plays:
        play_id = str(p.get("id") or "")
        text = p.get("text") or p.get("description") or ""

        # Period / quarter
        period = 1
        period_obj = p.get("period")
        if isinstance(period_obj, dict):
            period = int(period_obj.get("number") or 1)
        elif period_obj is not None:
            period = int(period_obj)

        # Clock
        clock_display = ""
        clock_obj = p.get("clock")
        if isinstance(clock_obj, dict):
            clock_display = clock_obj.get("displayValue") or ""
        elif clock_obj is not None:
            clock_display = str(clock_obj)

        start = p.get("start") or {}

        # Down (metadata, then short-text, then narrative fallback)
        down = start.get("down")
        if down is None and "shortDownDistanceText" in start:
            sddt = str(start.get("shortDownDistanceText") or "").lower()
            for d in (1, 2, 3, 4):
                if sddt.startswith(str(d)):
                    down = d
                    break
        if down is None:
            down = infer_down_from_text(text)
        if down not in (1, 2, 3, 4):
            continue

        # Possession + field position
        team_abbr = (p.get("team", {}).get("abbreviation") or "").upper()
        start_poss_text = (
            start.get("possessionText") or
            start.get("possession", {}).get("displayValue") or
            start.get("team", {}).get("abbreviation") or
            team_abbr
        )
        poss_team, poss_yard = parse_possession_text(start_poss_text, default_team=team_abbr)
        posteam = (team_abbr or poss_team or "").upper()
        if not posteam:
            continue
        defteam = away_abbr if posteam == home_abbr else home_abbr

        yardline_100 = compute_yardline_100(posteam, poss_team, poss_yard)
        if yardline_100 is None:
            continue

        ydstogo = start.get("distance")
        if ydstogo is None:
            continue

        # Scores (offense perspective)
        start_home = start.get("homeScore")
        start_away = start.get("awayScore")
        posteam_score = start_home if posteam == home_abbr else start_away
        defteam_score = start_away if posteam == home_abbr else start_home
        score_diff = 0
        if posteam_score is not None and defteam_score is not None:
            score_diff = int(posteam_score) - int(defteam_score)

        sec_left = get_game_seconds_remaining(period, clock_display)

        states.append({
            "play_id": play_id,
            "qtr": period,
            "clock": clock_display,
            "game_seconds_remaining": sec_left,
            "off": posteam,
            "def": defteam,
            "down": int(down),
            "ydstogo": int(ydstogo),
            "yardline_100": float(yardline_100),
            "score_differential": score_diff,
            "text": text,
        })

    return states
