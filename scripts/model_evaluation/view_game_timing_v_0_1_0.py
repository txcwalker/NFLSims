from src.nfl_sim.engine import NFLGameSimulator
import pandas as pd

def view_game_timing():
    sim = NFLGameSimulator("KC", "DET")
    print(f"{'Q':<2} | {'Time':<8} | {'Play Type':<15} | {'Result':<20} | {'Runoff':<6} | {'New Time'}")
    print("-" * 75)
    
    last_time = 900
    last_q = 1
    
    while not sim.game_over:
        q_start = sim.quarter
        t_start = sim.time_remaining
        
        # Capture the actual result from the engine
        result_msg = sim.simulate_play()
        
        t_end = sim.time_remaining
        q_end = sim.quarter
        
        # Calculate runoff
        if q_start == q_end:
            runoff = t_start - t_end
        else:
            runoff = t_start + (900 - t_end) if not sim.game_over else t_start
            
        # Format time
        time_str = f"{int(t_start//60):02d}:{int(t_start%60):02d}"
        new_time_str = f"{int(t_end//60):02d}:{int(t_end%60):02d}"
        
        # Categorize for the log
        play_type = "OFFENSE"
        if "PUNT" in result_msg or "FIELD GOAL" in result_msg:
            play_type = "SPEC TEAMS"
        elif "PENALTY" in result_msg:
            play_type = "PENALTY"
        
        print(f"{q_start:<2} | {time_str:<8} | {play_type:<15} | {result_msg[:25]:<25} | {int(runoff):<6} | {new_time_str}")
        
        if sim.total_plays > 150: break # Safety

if __name__ == "__main__":
    view_game_timing()
