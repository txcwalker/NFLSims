from src.nfl_sim.engine import NFLGameSimulator
import pandas as pd
import numpy as np

def test_clock_debug():
    away = "KC"
    home = "DET"
    
    sim = NFLGameSimulator(away, home)
    log = []
    
    while not sim.game_over:
        q_start = sim.quarter
        t_start = sim.time_remaining
        
        sim.simulate_play()
        
        t_end = sim.time_remaining
        # Calculate how much time actually elapsed on the game clock
        # Handle quarter transitions
        if sim.quarter == q_start:
            elapsed = t_start - t_end
        else:
            elapsed = t_start + (900 - t_end) if not sim.game_over else t_start
            
        log.append({
            'play': sim.total_plays,
            'q': q_start,
            'runoff': elapsed
        })
    
    df = pd.DataFrame(log)
    print(f"Total Plays: {len(df)}")
    print(f"Avg Runoff: {df['runoff'].mean():.2f}")
    print(f"Q1 Avg Runoff: {df[df['q']==1]['runoff'].mean():.2f}")
    print(f"Q4 Avg Runoff: {df[df['q']==4]['runoff'].mean():.2f}")
    
    # Check for zero runoff plays
    zeros = df[df['runoff'] <= 0]
    if not zeros.empty:
        print(f"WARNING: Found {len(zeros)} plays with zero or negative runoff!")

if __name__ == "__main__":
    test_clock_debug()
