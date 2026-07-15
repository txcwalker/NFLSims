import sys
import os
import pandas as pd
import numpy as np
import json
import time

# Add project root to system path
sys.path.append(os.getcwd())

from src.nfl_sim.batch import BatchSimulator, StatAggregator

TEAM_DIVISIONS = {
    'BUF': ('AFC', 'East'), 'MIA': ('AFC', 'East'), 'NE': ('AFC', 'East'), 'NYJ': ('AFC', 'East'),
    'BAL': ('AFC', 'North'), 'CIN': ('AFC', 'North'), 'CLE': ('AFC', 'North'), 'PIT': ('AFC', 'North'),
    'HOU': ('AFC', 'South'), 'IND': ('AFC', 'South'), 'JAX': ('AFC', 'South'), 'TEN': ('AFC', 'South'),
    'DEN': ('AFC', 'West'), 'KC': ('AFC', 'West'), 'LV': ('AFC', 'West'), 'LAC': ('AFC', 'West'),
    
    'DAL': ('NFC', 'East'), 'NYG': ('NFC', 'East'), 'PHI': ('NFC', 'East'), 'WAS': ('NFC', 'East'),
    'CHI': ('NFC', 'North'), 'DET': ('NFC', 'North'), 'GB': ('NFC', 'North'), 'MIN': ('NFC', 'North'),
    'ATL': ('NFC', 'South'), 'CAR': ('NFC', 'South'), 'NO': ('NFC', 'South'), 'TB': ('NFC', 'South'),
    'ARI': ('NFC', 'West'), 'LA': ('NFC', 'West'), 'SF': ('NFC', 'West'), 'SEA': ('NFC', 'West'),
}

def generate_matchup_boxscore(away, home, week, game_df, player_df, iterations):
    """Generates a detailed statistical boxscore for the given matchup."""
    avg_away_score = game_df['off_score'].mean()
    avg_home_score = game_df['def_score'].mean()
    away_win_pct = (game_df['winner'] == away).mean() * 100
    home_win_pct = (game_df['winner'] == home).mean() * 100
    
    avg_plays = game_df['total_plays'].mean()
    avg_explosive = game_df['plays_over_20_yds'].mean()
    avg_punts = game_df['punts'].mean()
    
    # Get player stats averages
    player_avg = player_df.groupby(['Player', 'Team', 'Slot']).mean(numeric_only=True).reset_index()
    
    # Sort and split by category
    passing_df = player_avg[player_avg['pAtt'] > 0.5].sort_values('pYds', ascending=False)
    rushing_df = player_avg[player_avg['rAtt'] > 0.5].sort_values('rYds', ascending=False)
    receiving_df = player_avg[player_avg['targets'] > 0.5].sort_values('recYds', ascending=False)
    
    # Boxscore Markdown
    box_md = f"""## 📊 Matchup: {away} at {home}
**Monte Carlo Simulated Averages across {iterations} Iterations**

### 🏈 Game Summary
* **Final Projected Score**: **{away} {avg_away_score:.1f}** - **{home} {avg_home_score:.1f}**
* **Win Probability**: {away} {away_win_pct:.1f}% | {home} {home_win_pct:.1f}%
* **Avg Total Scrimmage Plays**: {avg_plays:.1f}
* **Avg Combined Punts**: {avg_punts:.1f}
* **Avg Explosive Plays (>20 yds)**: {avg_explosive:.1f}

### 🎯 Passing Statistics
| Player | Team | Slot | Cmp / Att | Pass Yards | Pass TDs | INTs | Sacks Taken | DK Fantasy |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for _, row in passing_df.iterrows():
        box_md += f"| **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['pCmp']:.1f} / {row['pAtt']:.1f} | {row['pYds']:.1f} | {row['pTD']:.1f} | {row.get('int', 0.0):.1f} | {row.get('sacks_taken', 0.0):.1f} | **{row['dk_score']:.2f}** |\n"
        
    box_md += """
### 🏃 Rushing Statistics
| Player | Team | Slot | Attempts | Yards | TDs | Long | DK Fantasy |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for _, row in rushing_df.iterrows():
        box_md += f"| **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['rAtt']:.1f} | {row['rYds']:.1f} | {row['rTD']:.1f} | {row.get('rYds', 0.0)/max(1, row['rAtt']):.1f} avg | **{row['dk_score']:.2f}** |\n"
        
    box_md += """
### 👐 Receiving Statistics
| Player | Team | Slot | Targets | Receptions | Yards | TDs | DK Fantasy |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for _, row in receiving_df.iterrows():
        box_md += f"| **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['targets']:.1f} | {row['rec']:.1f} | {row['recYds']:.1f} | {row['recTD']:.1f} | **{row['dk_score']:.2f}** |\n"

    # Sort and split Defense/DST stats
    defense_df = player_avg[player_avg['Slot'] == 'DST'].sort_values('dk_score', ascending=False)
    box_md += """
### 🛡️ Defense / Special Teams (DST)
| Team | Sacks | INTs | Fumbles Rec | Def TDs | Points Allowed | DK Fantasy |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for _, row in defense_df.iterrows():
        box_md += f"| **{row['Team']} DST** | {row.get('def_sack', 0.0):.1f} | {row.get('def_int', 0.0):.1f} | {row.get('def_fumble_rec', 0.0):.1f} | {row.get('def_td', 0.0):.1f} | {row.get('pts_allowed', 0.0):.1f} | **{row['dk_score']:.2f}** |\n"
        
    return box_md

def run_all_weekly_sims(iterations=1000):
    print(f"\n==========================================")
    print(f" Simulating Weeks 1-18 Boxscores & Summaries")
    print(f"==========================================\n")
    
    # Load 2025 Schedule
    sched_df = pd.read_csv("data/external/schedule_2025.csv")
    
    # Cache files paths
    games_cache_path = "data/interim/sim_results_2025_games.parquet"
    players_cache_path = "data/interim/sim_results_2025_players.parquet"
    
    # Load from cache if possible
    use_cache = os.path.exists(games_cache_path) and os.path.exists(players_cache_path)
    if use_cache:
        print("Loading pre-simulated season data from Parquet cache...")
        all_games_df = pd.read_parquet(games_cache_path)
        all_players_df = pd.read_parquet(players_cache_path)
    else:
        print("No Parquet cache found. Matches will be simulated on the fly...")
        
    all_nfl_teams = set(TEAM_DIVISIONS.keys())
    
    for week in range(1, 19):
        print(f"\n--- Processing Week {week} ---")
        week_games = sched_df[(sched_df["week"] == week) & (sched_df["game_type"] == "REG")]
        
        if week_games.empty:
            print(f"No regular season games found in week {week}.")
            continue
            
        # Detect bye weeks
        active_teams = set(week_games["away_team"]).union(set(week_games["home_team"]))
        bye_teams = sorted(list(all_nfl_teams - active_teams))
        
        week_boxscores = []
        week_boxscore_dir = f"docs/boxscores/week_{week}"
        os.makedirs(week_boxscore_dir, exist_ok=True)
        
        for idx, row in week_games.iterrows():
            away = row["away_team"]
            home = row["home_team"]
            game_id = row["game_id"]
            
            if use_cache:
                game_df = all_games_df[all_games_df['game_id'] == game_id]
                player_df = all_players_df[all_players_df['game_id'] == game_id]
            else:
                batch = BatchSimulator(away, home, year=2025)
                game_df, player_df = batch.run_batch(iterations=iterations, vectorized=True)
                
            game_box_md = generate_matchup_boxscore(away, home, week, game_df, player_df, iterations)
            week_boxscores.append(game_box_md)
            
        # Write consolidated weekly boxscore
        week_boxscore_path = os.path.join(week_boxscore_dir, f"week_{week}_boxscore.md")
        
        bye_str = ", ".join(bye_teams) if bye_teams else "None"
        header_md = f"# 📊 NFL 2025 Week {week} Complete Boxscores\n"
        header_md += f"**Monte Carlo Simulated Averages across {iterations} Iterations**\n\n"
        header_md += f"> [!NOTE]\n"
        header_md += f"> **Teams on Bye**: {bye_str}\n\n"
        header_md += "---\n\n"
        
        with open(week_boxscore_path, 'w', encoding='utf-8') as f:
            f.write(header_md + "\n\n---\n\n".join(week_boxscores))
        print(f"Created consolidated week boxscore with bye annotations: {week_boxscore_path}")
        
    print("\nAll 18 weekly boxscores generated successfully!")

if __name__ == "__main__":
    run_all_weekly_sims(iterations=1000)
