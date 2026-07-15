import nfl_data_py as nfl
import pandas as pd
import numpy as np
import sys
import os
sys.path.append(os.getcwd())

from src.data_pipeline.roster_manager import RosterManager
from src.nfl_sim.engine import NFLGameSimulator
from src.nfl_sim.scoring import calculate_fantasy_points

def get_actual_fantasy_scores(game_id):
    """Pulls real-world fantasy scores for a specific game."""
    pbp = nfl.import_pbp_data([2023])
    game_pbp = pbp[pbp['game_id'] == game_id]
    
    actuals = {}
    # Passing
    for p, group in game_pbp.groupby('passer_player_name'):
        if pd.isna(p): continue
        stats = {
            'pYds': group['yards_gained'].sum(),
            'pTD': group['pass_touchdown'].sum(),
            'int': group['interception'].sum(),
            'rYds': game_pbp[game_pbp['rusher_player_name'] == p]['yards_gained'].sum()
        }
        actuals[p] = calculate_fantasy_points(stats, "DK")
    
    # Receiving/Rushing
    for p, group in game_pbp.groupby('receiver_player_name'):
        if pd.isna(p): continue
        stats = {
            'rec': group['complete_pass'].sum(),
            'recYds': group['yards_gained'].sum(),
            'recTD': group['pass_touchdown'].sum()
        }
        # Add rushing for RBs
        rush_group = game_pbp[game_pbp['rusher_player_name'] == p]
        stats['rYds'] = rush_group['yards_gained'].sum()
        stats['rTD'] = rush_group['rush_touchdown'].sum()
        
        actuals[p] = calculate_fantasy_points(stats, "DK")
    
    return actuals

def run_backtest():
    game_id = "2023_14_BUF_KC"
    actual_scores = get_actual_fantasy_scores(game_id)
    
    weights = [1.0, 0.75, 0.5, 0.25, 0.0]
    results = []

    print(f"Starting Backtest for Game: {game_id}")
    
    for w in weights:
        print(f" - Testing Weight {int(w*100)}% Season / {int((1-w)*100)}% L4...")
        
        # 1. Update Roster DNA for this weight
        rm = RosterManager(2023)
        rm.save_team_traits("KC", weight_season=w)
        rm.save_team_traits("BUF", weight_season=w)
        
        # 2. Run 50 sims for this weight (Faster for backtest)
        total_error = 0
        player_errors = {}
        
        sim_scores = {}
        for _ in range(50):
            sim = NFLGameSimulator("BUF", "KC")
            # Minimalist game run
            for _ in range(100): # 100 plays per game
                sim.simulate_play()
            
            stats_df = sim.get_stats_report()
            for _, row in stats_df.iterrows():
                p = row['Player']
                # Calculate DK Score
                score = calculate_fantasy_points(row.to_dict(), "DK")
                sim_scores[p] = sim_scores.get(p, []) + [score]

        # 3. Calculate Error
        # We only look at players who actually scored in the real game and were in our sim
        players_to_test = [p for p in actual_scores if p in sim_scores]
        for p in players_to_test:
            sim_avg = np.mean(sim_scores[p])
            error = abs(sim_avg - actual_scores[p])
            total_error += error
            player_errors[p] = round(error, 2)
            
        results.append({
            'Weight': f"{int(w*100)}/{int((1-w)*100)}",
            'Total_Error': round(total_error, 2),
            'Avg_Player_Error': round(total_error / len(players_to_test), 2) if players_to_test else 0
        })

    report = pd.DataFrame(results)
    print("\n" + "="*50)
    print(" BACKTEST RESULTS: SEASON vs L4 WEIGHTING")
    print("="*50)
    print(report.to_string(index=False))
    print("="*50)

if __name__ == "__main__":
    run_backtest()
