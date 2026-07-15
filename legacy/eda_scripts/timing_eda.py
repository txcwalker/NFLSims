import nfl_data_py as nfl
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def run_timing_eda():
    years = [2020, 2021, 2022, 2023, 2024]
    print(f"Loading Play-by-Play data for {years}...")
    pbp = nfl.import_pbp_data(years)
    
    # Filter for valid offensive plays
    pbp = pbp[pbp['play_type'].isin(['pass', 'run'])]
    pbp = pbp[pbp['quarter_seconds_remaining'].notna()]
    
    # Sort by game and time to calculate delta
    pbp = pbp.sort_values(['game_id', 'qtr', 'quarter_seconds_remaining'], ascending=[True, True, False])
    
    # Calculate seconds elapsed from PREVIOUS play to CURRENT play
    pbp['prev_time'] = pbp.groupby('game_id')['quarter_seconds_remaining'].shift(1)
    pbp['seconds_elapsed'] = pbp['prev_time'] - pbp['quarter_seconds_remaining']
    
    # Filter out weird deltas (quarter breaks, etc.)
    pbp = pbp[(pbp['seconds_elapsed'] > 0) & (pbp['seconds_elapsed'] < 60)]
    
    # 1. AVG TIME BY PLAY TYPE
    print("\n--- AVG SECONDS ELAPSED BY PLAY TYPE ---")
    print(pbp.groupby('play_type')['seconds_elapsed'].mean())
    
    # 2. BREAKDOWN BY QUARTER
    print("\n--- AVG SECONDS ELAPSED BY QUARTER ---")
    print(pbp.groupby('qtr')['seconds_elapsed'].mean())
    
    # 3. 4TH QUARTER LEVERAGE
    pbp_q4 = pbp[pbp['qtr'] == 4].copy()
    pbp_q4['leverage_score'] = pbp_q4['score_differential'] * pbp_q4['quarter_seconds_remaining']
    
    # Binning leverage
    pbp_q4['leverage_bin'] = pd.cut(pbp_q4['leverage_score'], 
                                   bins=[-np.inf, -5000, -1000, 1000, 5000, np.inf],
                                   labels=['Trailing Big', 'Trailing Close', 'Neutral', 'Leading Close', 'Leading Big'])
    
    print("\n--- 4TH QUARTER LEVERAGE TIMING ---")
    print(pbp_q4.groupby('leverage_bin')['seconds_elapsed'].mean())
    
    # 4. HISTOGRAM OF OFFENSIVE SNAPS
    game_snaps = pbp.groupby('game_id').size()
    
    print("\n--- SNAP COUNT DISTRIBUTION ---")
    print(f"Mean Snaps: {game_snaps.mean():.1f}")
    print(f"Median Snaps: {game_snaps.median():.1f}")
    print(f"Std Dev: {game_snaps.std():.1f}")
    
    # Summary stats for report
    stats = {
        'avg_pass_time': pbp[pbp['play_type'] == 'pass']['seconds_elapsed'].mean(),
        'avg_run_time': pbp[pbp['play_type'] == 'run']['seconds_elapsed'].mean(),
        'q4_hurry_up': pbp_q4[pbp_q4['leverage_score'] < -1000]['seconds_elapsed'].mean(),
        'q4_clock_kill': pbp_q4[pbp_q4['leverage_score'] > 1000]['seconds_elapsed'].mean(),
        'mean_snaps': game_snaps.mean()
    }
    
    pd.Series(stats).to_json('data/timing_eda_results.json')
    print("\nEDA Results saved to data/timing_eda_results.json")

if __name__ == "__main__":
    run_timing_eda()
