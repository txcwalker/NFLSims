"""Aggregates multi-week simulation logs into league standings.

Concatenates several weeks of game-summary and player CSVs, computes expected
standings (xW, PF, PA, plays) per team, and runs a 1,000-season Monte Carlo over
the simulated win rates to produce win floors/ceilings (10th/90th percentile),
organized into the 8 NFL divisions — a diagnostic for overall engine balance
and pacing.

Entry point: run as a script.
Full design rationale: see pilot_summary.md.
"""

import pandas as pd
import os
import numpy as np

DIVISIONS = {
    'AFC East': ['BUF', 'MIA', 'NYJ', 'NE'],
    'AFC North': ['BAL', 'CLE', 'PIT', 'CIN'],
    'AFC South': ['JAX', 'IND', 'TEN', 'HOU'],
    'AFC West': ['KC', 'LV', 'LAC', 'DEN'],
    'NFC East': ['PHI', 'DAL', 'NYG', 'WAS'],
    'NFC North': ['MIN', 'DET', 'GB', 'CHI'],
    'NFC South': ['ATL', 'NO', 'TB', 'CAR'],
    'NFC West': ['SF', 'SEA', 'LAR', 'LA', 'ARI']
}

def generate_pilot_summary(weeks=[1, 2, 3, 4], path_sims=1000):
    all_games = []
    all_players = []
    
    for w in weeks:
        g_path = f'reports/season_sim/week_{w}_hybrid_game_summary.csv'
        p_path = f'reports/season_sim/week_{w}_hybrid_player_projections.csv'
        if os.path.exists(g_path): all_games.append(pd.read_csv(g_path))
        if os.path.exists(p_path): all_players.append(pd.read_csv(p_path))
        
    if not all_games: return

    df_games = pd.concat(all_games)
    df_players = pd.concat(all_players)

    team_stats = {}
    for team in df_players['Team'].unique():
        team_stats[team] = {'xW': 0, 'PF': 0, 'PA': 0, 'G': 0, 'YF': 0, 'records': []}

    for _, g in df_games.iterrows():
        for t_key, is_away in [(g['away_team'], True), (g['home_team'], False)]:
            if t_key not in team_stats: team_stats[t_key] = {'xW': 0, 'PF': 0, 'PA': 0, 'G': 0, 'YF': 0, 'records': []}
            win_rate = g['away_win_rate'] if is_away else (1 - g['away_win_rate'])
            team_stats[t_key]['xW'] += win_rate
            team_stats[t_key]['PF'] += g['avg_away_score'] if is_away else g['avg_home_score']
            team_stats[t_key]['PA'] += g['avg_home_score'] if is_away else g['avg_away_score']
            team_stats[t_key]['G'] += 1

    for _ in range(path_sims):
        season_wins = {t: 0 for t in team_stats}
        for _, g in df_games.iterrows():
            if np.random.random() < g['away_win_rate']: season_wins[g['away_team']] += 1
            else: season_wins[g['home_team']] += 1
        for t in team_stats: team_stats[t]['records'].append(season_wins[t])

    for team in team_stats:
        recs = team_stats[team]['records']
        team_stats[team]['median_W'] = int(np.median(recs))
        team_stats[team]['ceil_W'] = int(np.percentile(recs, 90))
        team_stats[team]['floor_W'] = int(np.percentile(recs, 10))
        team_stats[team]['YF'] = df_players[df_players['Team'] == team]['Avg_Yds'].sum() / team_stats[team]['G']

    standings = pd.DataFrame.from_dict(team_stats, orient='index')
    standings['xL'] = standings['G'] - standings['xW']

    output_path = 'reports/season_sim/pilot_summary_report.txt'
    with open(output_path, 'w') as f:
        f.write(f"{'='*110}\n")
        f.write(f" 4-WEEK PILOT SEASON SUMMARY (MONTH 1 - 2023 BASELINE)\n")
        f.write(f"{'='*110}\n\n")
        
        for div_name, teams in DIVISIONS.items():
            f.write(f"--- {div_name} STANDINGS ---\n")
            f.write(f"{'-'*110}\n")
            f.write(f"{'TEAM':<10} {'xW':<8} {'xL':<8} {'PF/G':<8} {'YDS/G':<10} {'MEDIAN':<10} {'VARIANCE RANGE (90/10)':<25}\n")
            f.write(f"{'-'*110}\n")
            
            div_df = standings[standings.index.isin(teams)].sort_values('xW', ascending=False)
            for team, row in div_df.iterrows():
                med = f"{int(row['median_W'])}-{int(row['G'] - row['median_W'])}"
                var = f"Ceil: {int(row['ceil_W'])}-{int(row['G']-row['ceil_W'])} | Floor: {int(row['floor_W'])}-{int(row['G']-row['floor_W'])}"
                f.write(f"{team:<10} {row['xW']:<8.2f} {row['xL']:<8.2f} {row['PF']/row['G']:<8.1f} {row['YF']:<10.1f} {med:<10} {var:<25}\n")
            f.write("\n")
            
    print(f"Refined Pilot Summary generated at: {output_path}")

if __name__ == "__main__":
    generate_pilot_summary()
