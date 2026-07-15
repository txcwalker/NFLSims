import sys
import os
sys.path.append(os.getcwd())

from src.nfl_sim.engine import NFLGameSimulator
import pandas as pd

def run_production_test():
    print("Starting NFL Physics Evaluation: BUF vs KC...")
    sim = NFLGameSimulator(team_off="BUF", team_def="KC")
    pbp_log = sim.run_game()
    
    # Version 20
    pbp_path = 'v20_play_by_play.csv'
    pbp_log.to_csv(pbp_path, index=False)
    
    stats_df = sim.get_stats_report()
    stats_path = 'v20_box_score.csv'
    stats_df.to_csv(stats_path, index=False)
    
    decision_df = pd.DataFrame(sim.decision_log)
    decision_path = 'v20_decision_log.csv'
    decision_df.to_csv(decision_path, index=False)
    
    print(f"Simulation Complete. Final Score: {sim.score_off} - {sim.score_def}")
    print(f" - Play-by-Play: {pbp_path}")
    print(f" - Box Score: {stats_path}")
    print(f" - Decision Log: {decision_path}")
    
    print("\n--- Matchup Recap ---")
    print(f"BUF QB: {sim.rosters['BUF'].stats['QB']['pYds']} yds, {sim.rosters['BUF'].stats['QB']['pTD']} TD")
    print(f"KC QB: {sim.rosters['KC'].stats['QB']['pYds']} yds, {sim.rosters['KC'].stats['QB']['pTD']} TD")
    print(f"Total Plays: {len(pbp_log)}")

if __name__ == "__main__":
    run_production_test()
