import sys
import os
import time
import pandas as pd
import numpy as np
import nfl_data_py as nfl
import warnings
warnings.filterwarnings('ignore')

sys.path.append(os.getcwd())
from src.nfl_sim.batch import BatchSimulator, StatAggregator

def run_week_1_sims():
    year = 2025
    week = 1
    iterations = 10000
    
    print("\n" + "="*70)
    print(" NFL 2025 SEASON - WEEK 1 MONTE CARLO SIMULATOR")
    print(f" Simulating all matchups ({iterations} iterations per game)")
    print("="*70)
    
    # 1. LOAD 2025 SCHEDULE
    schedule_cache_path = os.path.join('data', f'schedule_{year}.csv')
    if os.path.exists(schedule_cache_path):
        print(f"Loading {year} NFL schedule from cache: {schedule_cache_path}...")
        sched = pd.read_csv(schedule_cache_path)
    else:
        print(f"Fetching {year} NFL schedule via nfl_data_py (this can take 3-5 minutes on first run, please wait)...")
        try:
            sched = nfl.import_schedules([year])
            # Ensure data/ directory exists
            if not os.path.exists('data'):
                os.makedirs('data')
            sched.to_csv(schedule_cache_path, index=False)
            print(f"Schedule successfully fetched and cached to {schedule_cache_path}!")
        except Exception as e:
            print(f"Error loading schedule: {e}")
            return
        
    week_games = sched[sched['week'] == week].to_dict('records')
    
    if not week_games:
        print("Error: Could not retrieve schedule for 2025 Week 1.")
        return
        
    print(f"Found {len(week_games)} matchups to simulate.\n")
    
    all_game_dfs = []
    all_player_dfs = []
    
    overall_start_time = time.time()
    
    # Run each game sequentially using multi-core per game
    for idx, game in enumerate(week_games):
        away = game['away_team']
        home = game['home_team']
        print(f"[{idx+1}/{len(week_games)}] Simulating: {away} @ {home}...")
        
        try:
            start_time = time.time()
            batch = BatchSimulator(away, home, year=year)
            game_df, player_df = batch.run_batch(iterations=iterations, vectorized=True)
            
            # Add game-specific identifiers
            game_df['week'] = week
            game_df['matchup'] = f"{away}@{home}"
            player_df['week'] = week
            player_df['matchup'] = f"{away}@{home}"
            
            all_game_dfs.append(game_df)
            all_player_dfs.append(player_df)
            
            duration = time.time() - start_time
            print(f" -> Completed {iterations} simulations in {duration:.1f}s\n")
            
        except Exception as e:
            print(f"❌ Error simulating {away} @ {home}: {e}\n")
            
    if not all_game_dfs:
        print("No simulations completed successfully.")
        return
        
    # 2. CONSOLIDATE RESULTS
    print("Consolidating all simulation records...")
    combined_games = pd.concat(all_game_dfs, ignore_index=True)
    combined_players = pd.concat(all_player_dfs, ignore_index=True)
    
    # 3. AGGREGATE STATS
    print("Running aggregation engines...")
    agg = StatAggregator()
    player_summary = agg.aggregate_player_stats(combined_players)
    
    # Sort summaries beautifully
    player_summary = player_summary.sort_values('dk_score_avg', ascending=False)
    
    # 4. SAVE REPORTS
    output_dir = 'reports/week_1_2025'
    if not os.path.exists(output_dir): 
        os.makedirs(output_dir)
        
    game_path = f"{output_dir}/game_summaries.csv"
    player_path = f"{output_dir}/player_summaries.csv"
    raw_players_path = f"{output_dir}/raw_player_results.csv"
    pbp_path = f"{output_dir}/play_by_play_diagnostics.json"
    
    # Save play-by-play structural diagnostics
    pbp_columns = ['matchup', 'game_id', 'punts', 'fourth_down_decisions', 'fg_attempts_details', 'td_details']
    pbp_df = combined_games[pbp_columns]
    pbp_df.to_json(pbp_path, orient='records', indent=2)
    print(f"Saved play-by-play diagnostics JSON: {pbp_path}")
    
    # Drop nested lists before writing CSV
    clean_combined_games = combined_games.drop(columns=['fourth_down_decisions', 'fg_attempts_details', 'td_details'])
    clean_combined_games.to_csv(game_path, index=False)
    
    player_summary.to_csv(player_path, index=False)
    combined_players.to_csv(raw_players_path, index=False)
    
    overall_duration = time.time() - overall_start_time
    
    # 5. TERMINAL RECAP
    print("\n" + "="*70)
    print(" WEEK 1 2025 SIMULATION COMPLETE")
    print("="*70)
    print(f"Total Matchups Simmed: {len(all_game_dfs)}")
    print(f"Total Simulated Games: {len(combined_games)}")
    print(f"Total Processing Time: {overall_duration:.1f} seconds ({overall_duration/60:.1f} minutes)")
    print("-" * 70)
    
    # Top 5 QBs by Average Fantasy Score
    print("\n*** PROJECTED TOP 5 QBS (DK avg):")
    qbs = player_summary[player_summary['Slot'] == 'QB'].head(5)
    for i, (_, row) in enumerate(qbs.iterrows(), 1):
        print(f" {i}. {row.get('Player', 'Unknown')} ({row['Team']}): {row['dk_score_avg']} DK Pts (Ceiling: {row['dk_score_p95']})")
        
    # Top 5 RBs by Average Fantasy Score
    print("\n*** PROJECTED TOP 5 RBS (DK avg):")
    rbs = player_summary[player_summary['Slot'].str.startswith('RB')].head(5)
    for i, (_, row) in enumerate(rbs.iterrows(), 1):
        print(f" {i}. {row.get('Player', 'Unknown')} ({row['Team']}, {row['Slot']}): {row['dk_score_avg']} DK Pts (Ceiling: {row['dk_score_p95']})")
        
    # Top 5 WRs by Average Fantasy Score
    print("\n*** PROJECTED TOP 5 WRS (DK avg):")
    wrs = player_summary[player_summary['Slot'].str.startswith('WR')].head(5)
    for i, (_, row) in enumerate(wrs.iterrows(), 1):
        print(f" {i}. {row.get('Player', 'Unknown')} ({row['Team']}, {row['Slot']}): {row['dk_score_avg']} DK Pts (Ceiling: {row['dk_score_p95']})")
        
    # Top 5 TEs by Average Fantasy Score
    print("\n*** PROJECTED TOP 5 TES (DK avg):")
    tes = player_summary[player_summary['Slot'].str.startswith('TE')].head(5)
    for i, (_, row) in enumerate(tes.iterrows(), 1):
        print(f" {i}. {row.get('Player', 'Unknown')} ({row['Team']}, {row['Slot']}): {row['dk_score_avg']} DK Pts (Ceiling: {row['dk_score_p95']})")
        
    print("\n" + "="*70)
    print(f"Reports successfully generated in '{output_dir}/'")
    print(" - game_summaries.csv      : Matchup-level spreads, win rates, and totals")
    print(" - player_summaries.csv    : Full player projection stat-lines and percentiles")
    print(" - raw_player_results.csv   : Game-by-game player scores for all iterations")
    print("="*70 + "\n")

if __name__ == "__main__":
    run_week_1_sims()
