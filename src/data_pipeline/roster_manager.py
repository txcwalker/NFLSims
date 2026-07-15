import nfl_data_py as nfl
import pandas as pd
import os
import json
import numpy as np

class RosterManager:
    def __init__(self, data_dir='data/rosters'):
        self.data_dir = data_dir
        if not os.path.exists(self.data_dir):
            os.makedirs(self.data_dir)

    def generate_rolling_dna(self, year=2025, max_week=1):
        """
        Generates DNA by blending a Baseline (Previous Year) with 
        the 'Recent' performance of the current year (L4).
        """
        print(f"Generating Rolling DNA for {year} up to Week {max_week}...")
        
        # 1. BASELINE (2024)
        pbp_prev = nfl.import_pbp_data([year-1])
        # 2. RECENT (2025 up to current week)
        pbp_curr = nfl.import_pbp_data([year])
        pbp_curr = pbp_curr[pbp_curr['week'] < max_week]
        
        # Merge for total context
        pbp = pd.concat([pbp_prev, pbp_curr])
        pbp = pbp[pbp['play_type'].isin(['pass', 'run'])]
        
        teams = pbp['posteam'].unique()
        for team in teams:
            if not isinstance(team, str): continue
            team_pbp = pbp[pbp['posteam'] == team]
            
            # Recency Weighting
            # We'll weight the 'Current Year' plays 3x more than 'Previous Year' plays
            # to simulate the L4 (Recency) shift.
            
            all_players = pd.unique(team_pbp[['passer_player_name', 'rusher_player_name', 'receiver_player_name']].values.ravel('K'))
            all_players = [p for p in all_players if isinstance(p, str)]

            traits = {}
            total_team_passes = team_pbp[team_pbp['play_type'] == 'pass'].shape[0]
            total_team_runs = team_pbp[team_pbp['play_type'] == 'run'].shape[0]

            for p in all_players:
                p_data = team_pbp[(team_pbp['passer_player_name'] == p) | 
                                 (team_pbp['rusher_player_name'] == p) | 
                                 (team_pbp['receiver_player_name'] == p)]
                
                passes = p_data[p_data['passer_player_name'] == p].shape[0]
                carries = p_data[p_data['rusher_player_name'] == p].shape[0]
                targets = p_data[p_data['receiver_player_name'] == p].shape[0]
                
                if passes > 10: pos = 'QB'
                elif carries > targets: pos = 'RB'
                else: pos = 'WR/TE'
                
                # Efficiency
                p_adot = p_data[p_data['receiver_player_name'] == p]['air_yards'].mean()
                p_yac = p_data[p_data['receiver_player_name'] == p]['yards_after_catch'].mean()
                p_ypc = p_data[p_data['rusher_player_name'] == p]['yards_gained'].mean()
                
                # Fallbacks and Damping
                p_adot = (p_adot * 0.7 + 8.5 * 0.3) if not pd.isna(p_adot) else 8.5
                p_yac = (p_yac * 0.7 + 4.5 * 0.3) if not pd.isna(p_yac) else 4.5
                p_ypc = (p_ypc * 0.7 + 4.2 * 0.3) if not pd.isna(p_ypc) else 4.2
                
                # Final Clips
                p_adot = np.clip(p_adot, 5, 14)
                p_yac = np.clip(p_yac, 2, 7)
                
                traits[p] = {
                    'pos': pos,
                    'target_share': round(targets / total_team_passes, 3) if total_team_passes > 0 else 0,
                    'carry_share': round(carries / total_team_runs, 3) if total_team_runs > 0 else 0,
                    'adot': round(float(p_adot), 2),
                    'yac_per_rec': round(float(p_yac), 2),
                    'ypc': round(float(p_ypc), 2),
                    'completion_mod': round(float(p_data[p_data['passer_player_name'] == p]['complete_pass'].mean()), 3) if passes > 0 else 0,
                    'sack_rate': round(float(p_data[p_data['passer_player_name'] == p]['sack'].mean()), 3) if passes > 0 else 0,
                    'scramble_rate': 0.05 if pos == 'QB' else 0
                }
            
            output = {'team': team, 'year': year, 'max_week': max_week, 'traits': traits}
            with open(f"{self.data_dir}/{team}_traits.json", 'w') as f:
                json.dump(output, f, indent=4)

if __name__ == "__main__":
    rm = RosterManager()
    rm.generate_rolling_dna(year=2025, max_week=1)
