import os
import sys
import numpy as np
import pandas as pd

# Adjust path to import from project root
sys.path.append(os.getcwd())

from src.nfl_sim.game_engine import NFLGameEngine

def inspect_single_game():
    engine = NFLGameEngine(away_team="DAL", home_team="PHI", year=2025, N=1)
    
    play_idx = 0
    while not engine.game_over[0]:
        poss = "DAL" if engine.possession_is_away[0] else "PHI"
        down = engine.down[0]
        dist = engine.distance[0]
        yd = engine.yardline_100[0]
        qtr = engine.quarter[0]
        time_rem = engine.time_remaining[0]
        score_away = engine.score_away[0]
        score_home = engine.score_home[0]
        needs_ko = engine.needs_kickoff[0]
        
        # Intercept before step
        # Run step
        engine.simulate_play_step()
        
        # Check what happened (we can check stats or yardline change)
        play_idx += 1
        print(f"Play {play_idx:02d} | Q{qtr} {time_rem//60:02d}:{time_rem%60:02d} | Poss: {poss} | {down}&{dist} at {yd} | Score: {score_away}-{score_home} | needs_ko: {needs_ko}")

if __name__ == '__main__':
    inspect_single_game()
