import os
import sys
import numpy as np
import pandas as pd

# Adjust path to import from project root
sys.path.append(os.getcwd())

from src.nfl_sim.game_engine import NFLGameEngine

def check_run_gains():
    engine = NFLGameEngine(away_team="DAL", home_team="PHI", year=2025, N=1000)
    
    # Let's inspect the models or run a few steps and capture run play details
    # We want to see: when a run play happens on 3rd down, what are the yardlines, distances, predicted gains, noise, and final gains?
    
    run_plays = []
    
    while len(run_plays) < 1000 and not np.all(engine.game_over):
        active = ~engine.game_over
        is_3rd_run = (engine.down == 3) & active & ~engine.needs_kickoff
        
        # Save state before play
        yd_before = engine.yardline_100.copy()
        dist_before = engine.distance.copy()
        poss_before = engine.possession_is_away.copy()
        
        # We want to intercept the play choices.
        # Let's run a single step
        
        # Let's count rAtt changes
        rAtt_before = {}
        for team in [engine.away_team, engine.home_team]:
            rAtt_before[team] = {}
            for p, s in engine.player_stats[team].items():
                rAtt_before[team][p] = s['rAtt'].copy()
                
        engine.simulate_play_step()
        
        rAtt_after = {}
        for team in [engine.away_team, engine.home_team]:
            rAtt_after[team] = {}
            for p, s in engine.player_stats[team].items():
                rAtt_after[team][p] = s['rAtt'].copy()
                
        # Find which games ran a play
        for i in range(engine.N):
            if not active[i]:
                continue
            # Did this game run the ball?
            was_run = False
            rusher_name = None
            for team in [engine.away_team, engine.home_team]:
                for p in rAtt_before[team]:
                    if rAtt_after[team][p][i] > rAtt_before[team][p][i]:
                        was_run = True
                        rusher_name = p
                        break
                if was_run:
                    break
                    
            if was_run:
                # Calculate play gain
                # Note: we need to handle possession change
                if poss_before[i] == engine.possession_is_away[i]:
                    gain = yd_before[i] - engine.yardline_100[i]
                else:
                    gain = yd_before[i] - (100 - engine.yardline_100[i])
                    
                converted = (gain >= dist_before[i])
                
                run_plays.append({
                    'down': 3 if is_3rd_run[i] else (1 if yd_before[i]==75 else 2), # approximate
                    'is_3rd': bool(is_3rd_run[i]),
                    'yardline_100': int(yd_before[i]),
                    'distance': int(dist_before[i]),
                    'gain': int(gain),
                    'converted': bool(converted),
                    'rusher': rusher_name
                })
                
    df = pd.DataFrame(run_plays)
    print("Total run plays recorded:", len(df))
    print("\nAll run plays summary:")
    print(df.describe())
    
    print("\n3rd down run plays summary:")
    df_3rd = df[df['is_3rd'] == True]
    print(df_3rd.describe())
    
    print("\nFirst 20 3rd down run plays details:")
    print(df_3rd.head(20).to_string())

if __name__ == '__main__':
    check_run_gains()
