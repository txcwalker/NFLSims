import sys
import os
import pandas as pd
import numpy as np
import time

# Add project root to system path
sys.path.append(os.getcwd())

from src.nfl_sim.batch import BatchSimulator

def run_week_1_sims(iterations=1000):
    print(f"\n==========================================")
    print(f" Simulating ONLY Week 1 of 2025 Season ({iterations} iterations)")
    print(f"==========================================\n")
    
    # Load 2025 Schedule
    schedule_path = "data/external/schedule_2025.csv"
    if not os.path.exists(schedule_path):
        print(f"Error: Schedule file not found at {schedule_path}")
        return
        
    sched_df = pd.read_csv(schedule_path)
    week_1_games = sched_df[(sched_df["week"] == 1) & (sched_df["game_type"] == "REG")]
    
    if week_1_games.empty:
        print("Error: No Week 1 games found in the schedule.")
        return
        
    print(f"Found {len(week_1_games)} games to simulate for Week 1.")
    
    all_games_list = []
    all_players_list = []
    
    start_time = time.time()
    for idx, row in week_1_games.iterrows():
        away = row["away_team"]
        home = row["home_team"]
        game_id = row["game_id"]
        
        print(f"Simulating Matchup {idx+1}: {away} at {home} (game_id: {game_id})...")
        batch = BatchSimulator(away, home, year=2025)
        game_df, player_df = batch.run_batch(iterations=iterations, vectorized=True)
        
        # Format game_df to match schema
        game_df = game_df.rename(columns={'game_id': 'iteration'})
        game_df['game_id'] = game_id
        game_df['away_team'] = away
        game_df['home_team'] = home
        game_df['div_game'] = row.get('div_game', 0)
        all_games_list.append(game_df)
        
        # Format player_df to match schema
        if player_df is not None and not player_df.empty:
            player_df = player_df.rename(columns={'game_id': 'iteration'})
            player_df['game_id'] = game_id
            all_players_list.append(player_df)
            
    print(f"\nSimulation phase complete in {time.time() - start_time:.2f} seconds.")
    
    # Combine results
    all_games_df = pd.concat(all_games_list, ignore_index=True)
    all_players_df = pd.concat(all_players_list, ignore_index=True)
    
    # Parquet cache paths
    games_cache_path = "data/interim/sim_results_2025_games.parquet"
    players_cache_path = "data/interim/sim_results_2025_players.parquet"
    
    os.makedirs(os.path.dirname(games_cache_path), exist_ok=True)
    all_games_df.to_parquet(games_cache_path, index=False)
    all_players_df.to_parquet(players_cache_path, index=False)
    
    print(f"Successfully saved Week 1 simulations to parquet cache!")
    print(f" - Games cached: {games_cache_path}")
    print(f" - Players cached: {players_cache_path}")

if __name__ == "__main__":
    run_week_1_sims(iterations=1000)
