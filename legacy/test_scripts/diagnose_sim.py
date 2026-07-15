from src.nfl_sim.engine import NFLGameSimulator
import pandas as pd

def diagnose_game():
    sim = NFLGameSimulator("DET", "KC")
    print(f"Starting Diagnostic Sim: {sim.away_team} vs {sim.home_team}")
    print(f"Starters: {sim.starters}")
    
    passes = 0
    runs = 0
    completions = 0
    total_pyards = 0
    
    for i in range(120):
        offense = sim.possession
        # We'll peak inside the engine for this test
        snap_state = {'yardline_100': sim.yardline_100, 'down': sim.down}
        is_pass = sim.registry.predict_event('play_selection', snap_state) == 1
        
        if is_pass: passes += 1
        else: runs += 1
        
        res = sim.simulate_play()
        
    stats = sim.get_stats_report()
    print("\n--- FINAL STATS ---")
    print(stats[stats['pAtt'] > 0][['Player', 'pAtt', 'pCmp', 'pYds', 'pTD']])
    print(stats[stats['rAtt'] > 0][['Player', 'rAtt', 'rYds', 'rTD']])
    print(f"\nPass/Run Ratio: {passes}/{runs}")
    print(f"Final Score: {sim.scores}")

if __name__ == "__main__":
    diagnose_game()
