import pandas as pd
import numpy as np

def calculate_fantasy_points(stats, scoring_type="DK"):
    """
    Calculates fantasy points for a given stats dictionary.
    Ensures NaNs are treated as 0.
    """
    # Clean NaNs to 0
    s = {k: (0 if pd.isna(v) else v) for k, v in stats.items()}
    
    p_yds = s.get('pYds', 0); p_td = s.get('pTD', 0); p_int = s.get('int', 0)
    r_yds = s.get('rYds', 0); r_td = s.get('rTD', 0)
    rec = s.get('rec', 0); rec_yds = s.get('recYds', 0); rec_td = s.get('recTD', 0)
    fumbles = s.get('fumbles', 0)

    if scoring_type == "DK":
        score = (p_yds * 0.04) + (p_td * 4) - (p_int * 1)
        if p_yds >= 300: score += 3
        score += (r_yds * 0.1) + (r_td * 6)
        if r_yds >= 100: score += 3
        score += (rec * 1) + (rec_yds * 0.1) + (rec_td * 6)
        if rec_yds >= 100: score += 3
        score -= (fumbles * 1)
        
    elif scoring_type == "FD":
        score = (p_yds * 0.04) + (p_td * 4) - (p_int * 2)
        score += (r_yds * 0.1) + (r_td * 6)
        score += (rec * 0.5) + (rec_yds * 0.1) + (rec_td * 6)
        score -= (fumbles * 2)
    
    return round(score, 2)

def get_player_summary(stats):
    """Adds fantasy scores and total touches to a player's stat dict."""
    stats['dk_score'] = calculate_fantasy_points(stats, "DK")
    stats['fd_score'] = calculate_fantasy_points(stats, "FD")
    
    # Clean rAtt and rec for touches
    r_att = 0 if pd.isna(stats.get('rAtt', 0)) else stats.get('rAtt', 0)
    rec = 0 if pd.isna(stats.get('rec', 0)) else stats.get('rec', 0)
    stats['touches'] = r_att + rec
    return stats
