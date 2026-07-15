import sys
import os
sys.path.append(os.getcwd())

from src.nfl_sim.batch import BatchSimulator, StatAggregator
from src.nfl_sim.visuals import Visualizer
import pandas as pd

def run_monte_carlo():
    team_a = "BUF"; team_b = "KC"
    batch = BatchSimulator(team_a, team_b)
    
    # 1. RUN SIMULATIONS (100 Iterations)
    game_df, player_df = batch.run_batch(iterations=100)
    
    # 2. AGGREGATE STATS
    agg = StatAggregator()
    player_summary = agg.aggregate_player_stats(player_df)
    game_summary = agg.aggregate_game_stats(game_df, team_a)
    
    # 3. WIN/LOSS CORRELATION (Player stats in Wins vs Losses)
    win_loss_stats = []
    for (team, slot), group in player_df.groupby(['Team', 'Slot']):
        in_win = group[group['winner'] == team]['dk_score'].mean()
        in_loss = group[group['winner'] != team]['dk_score'].mean()
        win_loss_stats.append({
            'Team': team, 'Slot': slot, 
            'Avg_DK_in_Win': round(in_win, 2), 
            'Avg_DK_in_Loss': round(in_loss, 2),
            'Win_Diff': round(in_win - in_loss, 2) if (not pd.isna(in_win) and not pd.isna(in_loss)) else 0
        })
    wl_df = pd.DataFrame(win_loss_stats)
    
    # 4. GENERATE VISUALS
    viz = Visualizer(output_dir='reports/monte_carlo_v1')
    viz.plot_game_totals(game_df)
    viz.plot_spread_distribution(game_df, team_a)
    # Focus on WR1 and RB1 for both teams
    for t in [team_a, team_b]:
        for s in ['WR1', 'RB1', 'QB']:
            viz.plot_fantasy_distribution(player_df, t, s, "DK")
            viz.plot_fantasy_distribution(player_df, t, s, "FD")
    
    # 5. SAVE REPORTS
    if not os.path.exists('reports'): os.makedirs('reports')
    player_summary.to_csv('reports/monte_carlo_player_summary.csv', index=False)
    wl_df.to_csv('reports/monte_carlo_win_loss_correlation.csv', index=False)
    
    # 6. TERMINAL RECAP
    print("\n" + "="*50)
    print(" MONTE CARLO RECAP (100 Games)")
    print("="*50)
    print(f"Matchup: {team_a} vs {team_b}")
    print(f"Avg Total Score: {game_summary['Total_Avg']}")
    print(f"Avg Spread: {game_summary['Spread_Avg']} (relative to {team_a})")
    print(f"Win Rate ({team_a}): {game_summary['Win_Rate_A']*100:.1f}%")
    print("-" * 30)
    print(f"BUF WR1 Ceiling (95%): {player_summary.loc[(player_summary['Slot']=='WR1')&(player_summary['Team']=='BUF'), 'dk_score_p95'].values[0]} DK Pts")
    print(f"BUF WR1 Floor (5%): {player_summary.loc[(player_summary['Slot']=='WR1')&(player_summary['Team']=='BUF'), 'dk_score_p05'].values[0]} DK Pts")
    print("="*50)
    print("Reports generated in 'reports/monte_carlo_v1/'")

if __name__ == "__main__":
    run_monte_carlo()
