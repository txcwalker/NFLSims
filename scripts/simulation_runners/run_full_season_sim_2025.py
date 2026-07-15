import sys
import os
import pandas as pd
import numpy as np
import json
import random
import time

# Headless matplotlib to avoid GUI issues in background processes
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

# Add project root to system path
sys.path.append(os.getcwd())

from src.nfl_sim.batch import BatchSimulator

# --- TEAM AND DIVISION METADATA ---
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

# Full names mapping for plotting/directory names if needed, otherwise use abbreviation
TEAM_FULL_NAMES = {
    'BUF': 'Buffalo Bills', 'MIA': 'Miami Dolphins', 'NE': 'New England Patriots', 'NYJ': 'New York Jets',
    'BAL': 'Baltimore Ravens', 'CIN': 'Cincinnati Bengals', 'CLE': 'Cleveland Browns', 'PIT': 'Pittsburgh Steelers',
    'HOU': 'Houston Texans', 'IND': 'Indianapolis Colts', 'JAX': 'Jacksonville Jaguars', 'TEN': 'Tennessee Titans',
    'DEN': 'Denver Broncos', 'KC': 'Kansas City Chiefs', 'LV': 'Las Vegas Raiders', 'LAC': 'Los Angeles Chargers',
    'DAL': 'Dallas Cowboys', 'NYG': 'New York Giants', 'PHI': 'Philadelphia Eagles', 'WAS': 'Washington Commanders',
    'CHI': 'Chicago Bears', 'DET': 'Detroit Lions', 'GB': 'Green Bay Packers', 'MIN': 'Minnesota Vikings',
    'ATL': 'Atlanta Falcons', 'CAR': 'Carolina Panthers', 'NO': 'New Orleans Saints', 'TB': 'Tampa Bay Buccaneers',
    'ARI': 'Arizona Cardinals', 'LA': 'Los Angeles Rams', 'SF': 'San Francisco 49ers', 'SEA': 'Seattle Seahawks',
}

# Matchup Cache for Playoff simulations to avoid repeating 1000-sim batches
PLAYOFF_MATCHUP_CACHE = {}

def simulate_matchup_cached(away, home):
    """Simulates a matchup of 1,000 runs or retrieves from cache."""
    key = f"{away}_at_{home}"
    if key in PLAYOFF_MATCHUP_CACHE:
        return PLAYOFF_MATCHUP_CACHE[key]
    
    batch = BatchSimulator(away, home, year=2025)
    game_df, _ = batch.run_batch(iterations=1000, vectorized=True)
    
    # Store minimal info to save memory: a list of dicts with scores and winner
    results = []
    for _, row in game_df.iterrows():
        results.append({
            'off_score': row['off_score'], # Away team score
            'def_score': row['def_score'], # Home team score
            'winner': row['winner']
        })
        
    PLAYOFF_MATCHUP_CACHE[key] = results
    return results

def run_playoff_game(away, home):
    """Simulates a single playoff matchup, sampling 11 games to find the winner."""
    results = simulate_matchup_cached(away, home)
    
    # Sample 11 outcomes
    samples = random.sample(results, 11)
    
    away_wins = sum(1 for r in samples if r['off_score'] > r['def_score'])
    home_wins = sum(1 for r in samples if r['def_score'] > r['off_score'])
    
    # Return winner (if tied 5-5-1, use sum of scores as tiebreaker for playoff progression)
    if away_wins > home_wins:
        return away
    elif home_wins > away_wins:
        return home
    else:
        # Fallback to sum of scores
        away_score = sum(r['off_score'] for r in samples)
        home_score = sum(r['def_score'] for r in samples)
        return away if away_score >= home_score else home

def get_seeding_sort_key(team_stats):
    """Sort key for breaking standings/seeding ties: overall win %, div win %, point diff."""
    wins = team_stats['wins']
    losses = team_stats['losses']
    ties = team_stats['ties']
    total_games = wins + losses + ties
    win_pct = (wins + 0.5 * ties) / total_games if total_games > 0 else 0.0
    
    div_wins = team_stats['div_wins']
    div_losses = team_stats['div_losses']
    div_ties = team_stats['div_ties']
    total_div = div_wins + div_losses + div_ties
    div_pct = (div_wins + 0.5 * div_ties) / total_div if total_div > 0 else 0.0
    
    point_diff = team_stats['pf'] - team_stats['pa']
    
    return (win_pct, div_pct, point_diff)

def simulate_full_season_and_playoffs(iterations=1000, num_seasons=100):
    print(f"\n==========================================")
    print(f" Simulating NFL 2025 Full Season")
    print(f" Iterations per game: {iterations}")
    print(f" Number of Season Monte Carlos: {num_seasons}")
    print(f"==========================================\n")
    
    # 1. LOAD 2025 SCHEDULE
    sched_df = pd.read_csv("data/external/schedule_2025.csv")
    reg_games = sched_df[sched_df["game_type"] == "REG"]
    
    # 2. CACHING OR PRE-SIMULATION PHASE
    games_cache_path = "data/interim/sim_results_2025_games.parquet"
    players_cache_path = "data/interim/sim_results_2025_players.parquet"
    
    if os.path.exists(games_cache_path) and os.path.exists(players_cache_path):
        print("Loading pre-simulated season results from Parquet cache...")
        all_games_df = pd.read_parquet(games_cache_path)
        all_players_df = pd.read_parquet(players_cache_path)
    else:
        print("Pre-simulating all 272 regular season matchups...")
        all_games_list = []
        all_players_list = []
        
        start_time = time.time()
        for idx, row in reg_games.iterrows():
            away = row["away_team"]
            home = row["home_team"]
            game_id = row["game_id"]
            
            print(f"Simulating game {idx+1}/272: {away} at {home}...")
            batch = BatchSimulator(away, home, year=2025)
            game_df, player_df = batch.run_batch(iterations=iterations, vectorized=True)
            
            # Format game_df
            game_df = game_df.rename(columns={'game_id': 'iteration'})
            game_df['game_id'] = game_id
            game_df['away_team'] = away
            game_df['home_team'] = home
            game_df['div_game'] = row['div_game']
            all_games_list.append(game_df)
            
            # Format player_df
            if player_df is not None and not player_df.empty:
                player_df = player_df.rename(columns={'game_id': 'iteration'})
                player_df['game_id'] = game_id
                all_players_list.append(player_df)
                
        print(f"Matchup simulation phase complete in {time.time() - start_time:.2f} seconds.")
        
        all_games_df = pd.concat(all_games_list, ignore_index=True)
        all_players_df = pd.concat(all_players_list, ignore_index=True)
        
        os.makedirs(os.path.dirname(games_cache_path), exist_ok=True)
        all_games_df.to_parquet(games_cache_path, index=False)
        all_players_df.to_parquet(players_cache_path, index=False)
        print("Saved simulation results to Parquet cache.")

    # 3. RE-STRUCTURE DATA FOR FAST RETRIEVAL
    # Convert games df to a nested dictionary grouped by game_id for sampling
    matchup_sim_results = {}
    for game_id, group in all_games_df.groupby('game_id'):
        matchup_sim_results[game_id] = {
            'away': group['away_team'].iloc[0],
            'home': group['home_team'].iloc[0],
            'div_game': group['div_game'].iloc[0],
            'outcomes': group[['off_score', 'def_score', 'winner']].to_dict(orient='records')
        }

    # 4. INITIALIZE SEASON-LONG STATISTICS TRACKING
    playoff_tracking = {
        team: {
            'made_playoffs': 0,
            'made_divisional': 0,
            'made_conference': 0,
            'made_superbowl': 0,
            'won_superbowl': 0,
            'season_wins': [],
            'season_losses': [],
            'season_ties': [],
            'season_pf': [],
            'season_pa': [],
            'season_div_wins': [],
            'season_div_losses': [],
            'season_div_ties': [],
        }
        for team in TEAM_DIVISIONS.keys()
    }
    
    # 5. RUN SEASON MONTE CARLO LOOPS (100 Seasons)
    print(f"\nSimulating {num_seasons} season runs...")
    for s in range(num_seasons):
        if (s + 1) % 10 == 0 or s == 0:
            print(f" - Simulating season {s+1}/{num_seasons}...")
            
        # Standings for this specific season
        season_standings = {
            team: {
                'wins': 0.0, 'losses': 0.0, 'ties': 0.0,
                'pf': 0.0, 'pa': 0.0,
                'div_wins': 0.0, 'div_losses': 0.0, 'div_ties': 0.0,
            }
            for team in TEAM_DIVISIONS.keys()
        }
        
        # Simulating regular season via 11-game random sampling
        for game_id, data in matchup_sim_results.items():
            away = data['away']
            home = data['home']
            div_game = data['div_game']
            outcomes = data['outcomes']
            
            # Sample 11 outcomes
            samples = random.sample(outcomes, 11)
            
            away_wins = sum(1 for r in samples if r['off_score'] > r['def_score'])
            home_wins = sum(1 for r in samples if r['def_score'] > r['off_score'])
            
            # Determine game winner and update record
            if away_wins > home_wins:
                season_standings[away]['wins'] += 1
                season_standings[home]['losses'] += 1
                if div_game == 1:
                    season_standings[away]['div_wins'] += 1
                    season_standings[home]['div_losses'] += 1
            elif home_wins > away_wins:
                season_standings[home]['wins'] += 1
                season_standings[away]['losses'] += 1
                if div_game == 1:
                    season_standings[home]['div_wins'] += 1
                    season_standings[away]['div_losses'] += 1
            else:
                # 5-5-1 or equal wins is a tie
                season_standings[away]['ties'] += 0.5
                season_standings[home]['ties'] += 0.5
                season_standings[away]['wins'] += 0.5
                season_standings[away]['losses'] += 0.5
                season_standings[home]['wins'] += 0.5
                season_standings[home]['losses'] += 0.5
                if div_game == 1:
                    season_standings[away]['div_ties'] += 0.5
                    season_standings[home]['div_ties'] += 0.5
                    season_standings[away]['div_wins'] += 0.5
                    season_standings[away]['div_losses'] += 0.5
                    season_standings[home]['div_wins'] += 0.5
                    season_standings[home]['div_losses'] += 0.5
                    
            # Accumulate scores (expected PF/PA for this season iteration)
            avg_away_score = sum(r['off_score'] for r in samples) / 11.0
            avg_home_score = sum(r['def_score'] for r in samples) / 11.0
            
            season_standings[away]['pf'] += avg_away_score
            season_standings[away]['pa'] += avg_home_score
            season_standings[home]['pf'] += avg_home_score
            season_standings[home]['pa'] += avg_away_score

        # Update cumulative standings averages and list of records
        for team, stats in season_standings.items():
            playoff_tracking[team]['season_wins'].append(stats['wins'])
            playoff_tracking[team]['season_losses'].append(stats['losses'])
            playoff_tracking[team]['season_ties'].append(stats['ties'])
            playoff_tracking[team]['season_pf'].append(stats['pf'])
            playoff_tracking[team]['season_pa'].append(stats['pa'])
            playoff_tracking[team]['season_div_wins'].append(stats['div_wins'])
            playoff_tracking[team]['season_div_losses'].append(stats['div_losses'])
            playoff_tracking[team]['season_div_ties'].append(stats['div_ties'])
            
        # 6. PLAYOFF SEEDING FOR THIS SEASON
        # Seeds for AFC and NFC
        conference_seeds = {'AFC': [], 'NFC': []}
        
        for conf in ['AFC', 'NFC']:
            conf_teams = [t for t, (c, _) in TEAM_DIVISIONS.items() if c == conf]
            
            # Find Division Winners
            div_winners = []
            for division in ['East', 'North', 'South', 'West']:
                div_teams = [t for t in conf_teams if TEAM_DIVISIONS[t][1] == division]
                # Sort division teams by record key
                div_teams_sorted = sorted(div_teams, key=lambda t: get_seeding_sort_key(season_standings[t]), reverse=True)
                div_winners.append(div_teams_sorted[0])
                
            # Assign seeds 1-4 to division winners (sorted by record)
            div_winners_sorted = sorted(div_winners, key=lambda t: get_seeding_sort_key(season_standings[t]), reverse=True)
            
            # Wild Cards: Next 3 best records in conference (excluding division winners)
            wildcards = [t for t in conf_teams if t not in div_winners]
            wildcards_sorted = sorted(wildcards, key=lambda t: get_seeding_sort_key(season_standings[t]), reverse=True)
            
            # Combine Seeds 1-7
            seeds_list = div_winners_sorted + wildcards_sorted[:3]
            conference_seeds[conf] = seeds_list
            
            # Track Made Playoffs
            for team in seeds_list:
                playoff_tracking[team]['made_playoffs'] += 1

        # 7. RUN PLAYOFF SIMULATOR FOR THIS SEASON
        for conf in ['AFC', 'NFC']:
            seeds = conference_seeds[conf]
            
            # Wild Card Round
            # Seed 1 Bye. Seed 2 vs 7, Seed 3 vs 6, Seed 4 vs 5
            wc_winner_2 = run_playoff_game(seeds[6], seeds[1]) # 7 at 2
            wc_winner_3 = run_playoff_game(seeds[5], seeds[2]) # 6 at 3
            wc_winner_4 = run_playoff_game(seeds[4], seeds[3]) # 5 at 4
            
            wc_winners = [seeds[0], wc_winner_2, wc_winner_3, wc_winner_4]
            wc_winners.sort(key=lambda t: seeds.index(t))
            
            # Track Divisional Reached
            for team in wc_winners:
                playoff_tracking[team]['made_divisional'] += 1
                
            # Divisional Round
            div_winner_1 = run_playoff_game(wc_winners[3], wc_winners[0]) # Lowest remaining at 1
            div_winner_2 = run_playoff_game(wc_winners[2], wc_winners[1]) # Other two
            
            div_winners = [div_winner_1, div_winner_2]
            div_winners.sort(key=lambda t: seeds.index(t))
            
            # Track Conference Reached
            for team in div_winners:
                playoff_tracking[team]['made_conference'] += 1
                
            # Conference Championship
            conf_champ = run_playoff_game(div_winners[1], div_winners[0])
            playoff_tracking[conf_champ]['made_superbowl'] += 1
            conference_seeds[f"{conf}_champ"] = conf_champ
            
        # Super Bowl
        afc_champ = conference_seeds["AFC_champ"]
        nfc_champ = conference_seeds["NFC_champ"]
        
        sb_winner = run_playoff_game(afc_champ, nfc_champ)
        playoff_tracking[sb_winner]['won_superbowl'] += 1
        
    print("\nAll seasons simulated successfully.")

    # 8. POST-PROCESS PLAYER STATS (Season Cumulative Totals and Percentiles)
    print("\nCalculating player stats cumulative season percentiles...")
    metrics = ['pAtt', 'pCmp', 'pYds', 'pTD', 'int', 'rAtt', 'rYds', 'rTD', 'rec', 'recYds', 'recTD', 'targets', 'fumbles', 'fumbles_lost', 'dk_score', 'fd_score', 'def_sack', 'def_int', 'def_fumble_rec', 'def_td', 'pts_allowed']
    
    # Sum stats per player, team, slot, pos, and iteration to get season-long totals
    season_player_grouped = all_players_df.groupby(['Player', 'Team', 'Pos', 'Slot', 'iteration'])[metrics].sum().reset_index()
    
    # Calculate percentiles (min=0%, 5%, 25%, median=50%, 75%, 95%, max=100%)
    quantiles = [0.0, 0.05, 0.25, 0.50, 0.75, 0.95, 1.0]
    player_summary = season_player_grouped.groupby(['Player', 'Team', 'Pos', 'Slot'])[metrics].quantile(quantiles)
    player_summary_unstacked = player_summary.unstack()
    player_summary_unstacked.columns = [f"{col[0]}_p{int(col[1]*100):02d}" for col in player_summary_unstacked.columns]
    player_summary_df = player_summary_unstacked.reset_index()

    # 9. BUILD SYSTEMATIC DIRECTORIES AND GENERATE REPORTS
    docs_report_dir = "docs/reports"
    os.makedirs(docs_report_dir, exist_ok=True)
    
    # Prepare expected/median stats dataframe for CSV and standings md
    summary_data = []
    
    for team, tracking in playoff_tracking.items():
        conf, div = TEAM_DIVISIONS[team]
        
        # Calculate percentiles for wins
        wins_dist = tracking['season_wins']
        losses_dist = tracking['season_losses']
        ties_dist = tracking['season_ties']
        pf_dist = tracking['season_pf']
        pa_dist = tracking['season_pa']
        
        med_wins = np.percentile(wins_dist, 50)
        med_losses = np.percentile(losses_dist, 50)
        med_ties = np.percentile(ties_dist, 50)
        
        summary_data.append({
            'Team': team,
            'Conference': conf,
            'Division': div,
            'Wins_Median': med_wins,
            'Losses_Median': med_losses,
            'Ties_Median': med_ties,
            'Wins_Min': np.min(wins_dist),
            'Wins_P05': np.percentile(wins_dist, 5),
            'Wins_P25': np.percentile(wins_dist, 25),
            'Wins_P75': np.percentile(wins_dist, 75),
            'Wins_P95': np.percentile(wins_dist, 95),
            'Wins_Max': np.max(wins_dist),
            'PF_Avg': np.mean(pf_dist),
            'PA_Avg': np.mean(pa_dist),
            'Div_Wins_Avg': np.mean(tracking['season_div_wins']),
            'Div_Losses_Avg': np.mean(tracking['season_div_losses']),
            'Div_Ties_Avg': np.mean(tracking['season_div_ties']),
            'Playoffs_%': (tracking['made_playoffs'] / num_seasons) * 100,
            'Divional_%': (tracking['made_divisional'] / num_seasons) * 100,
            'Conference_%': (tracking['made_conference'] / num_seasons) * 100,
            'SuperBowl_%': (tracking['made_superbowl'] / num_seasons) * 100,
            'Champion_%': (tracking['won_superbowl'] / num_seasons) * 100,
        })
        
        # --- TEAM DIRECTORY CREATION ---
        team_dir = f"{docs_report_dir}/2025/teams/{team}"
        os.makedirs(team_dir, exist_ok=True)
        
        # 1. Save Win Distribution Histogram
        plt.figure(figsize=(7, 4.5))
        plt.hist(wins_dist, bins=np.arange(0, 19) - 0.5, rwidth=0.8, color='#1f77b4', edgecolor='black', alpha=0.85)
        plt.title(f"{TEAM_FULL_NAMES[team]} ({team}) Simulated Win Distribution\n(100 Monte Carlo Seasons)", fontsize=11, fontweight='bold')
        plt.xlabel("Simulated Wins", fontsize=10)
        plt.ylabel("Number of Seasons", fontsize=10)
        plt.xticks(np.arange(0, 18))
        
        # Add percentile lines
        plt.axvline(med_wins, color='red', linestyle='--', linewidth=1.5, label=f'Median: {med_wins:.1f}')
        plt.axvline(np.percentile(wins_dist, 25), color='orange', linestyle=':', linewidth=1.2, label=f'25th Pct: {np.percentile(wins_dist, 25):.1f}')
        plt.axvline(np.percentile(wins_dist, 75), color='green', linestyle=':', linewidth=1.2, label=f'75th Pct: {np.percentile(wins_dist, 75):.1f}')
        plt.legend(loc='upper right', framealpha=0.9, fontsize=9)
        plt.grid(axis='y', linestyle='--', alpha=0.5)
        plt.tight_layout()
        plt.savefig(f"{team_dir}/win_distribution.png", dpi=150)
        plt.close()
        
        # 2. Save Roster and Cumulative Player Stats Report
        team_players = player_summary_df[player_summary_df['Team'] == team]
        
        roster_md = f"# 📋 {TEAM_FULL_NAMES[team]} 2025 Roster & Simulated Season Stats\n"
        roster_md += f"**Cumulative season statistics computed over 1,000 simulations**\n"
        roster_md += f"*Shows the floor (min/p05), median (p50), and ceiling (p95/max) projections*\n\n"
        
        # Sort players by position (QBs, RBs, WRs, TEs, DST) and projection value
        for pos_name, pos_group in [('Quarterbacks', ['QB']), ('Running Backs', ['RB1', 'RB2', 'RB3']), ('Wide Receivers & Tight Ends', ['WR1', 'WR2', 'WR3', 'TE1', 'TE2', 'WR/TE']), ('Defense & Special Teams', ['DST'])]:
            roster_md += f"## {pos_name}\n\n"
            sub_players = team_players[team_players['Slot'].isin(pos_group) | team_players['Pos'].isin(pos_group)]
            if sub_players.empty:
                roster_md += "*No active players in this category.*\n\n"
                continue
                
            if 'QB' in pos_group:
                roster_md += "| Player | Slot | Stat | Min | 5th Pct | 25th Pct | Median | 75th Pct | 95th Pct | Max |\n"
                roster_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
                for _, p in sub_players.iterrows():
                    for stat_lbl, stat_col in [('Pass Yds', 'pYds'), ('Pass TDs', 'pTD'), ('INTs', 'int'), ('DK Fantasy', 'dk_score')]:
                        roster_md += f"| **{p['Player']}** | {p['Slot']} | {stat_lbl} | {p[f'{stat_col}_p00']:.1f} | {p[f'{stat_col}_p05']:.1f} | {p[f'{stat_col}_p25']:.1f} | **{p[f'{stat_col}_p50']:.1f}** | {p[f'{stat_col}_p75']:.1f} | {p[f'{stat_col}_p95']:.1f} | {p[f'{stat_col}_p100']:.1f} |\n"
            elif 'RB1' in pos_group:
                roster_md += "| Player | Slot | Stat | Min | 5th Pct | 25th Pct | Median | 75th Pct | 95th Pct | Max |\n"
                roster_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
                for _, p in sub_players.iterrows():
                    for stat_lbl, stat_col in [('Rush Att', 'rAtt'), ('Rush Yds', 'rYds'), ('Rush TDs', 'rTD'), ('Receptions', 'rec'), ('DK Fantasy', 'dk_score')]:
                        roster_md += f"| **{p['Player']}** | {p['Slot']} | {stat_lbl} | {p[f'{stat_col}_p00']:.1f} | {p[f'{stat_col}_p05']:.1f} | {p[f'{stat_col}_p25']:.1f} | **{p[f'{stat_col}_p50']:.1f}** | {p[f'{stat_col}_p75']:.1f} | {p[f'{stat_col}_p95']:.1f} | {p[f'{stat_col}_p100']:.1f} |\n"
            elif 'WR1' in pos_group:
                roster_md += "| Player | Slot | Stat | Min | 5th Pct | 25th Pct | Median | 75th Pct | 95th Pct | Max |\n"
                roster_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
                for _, p in sub_players.iterrows():
                    for stat_lbl, stat_col in [('Targets', 'targets'), ('Receptions', 'rec'), ('Rec Yds', 'recYds'), ('Rec TDs', 'recTD'), ('DK Fantasy', 'dk_score')]:
                        roster_md += f"| **{p['Player']}** | {p['Slot']} | {stat_lbl} | {p[f'{stat_col}_p00']:.1f} | {p[f'{stat_col}_p05']:.1f} | {p[f'{stat_col}_p25']:.1f} | **{p[f'{stat_col}_p50']:.1f}** | {p[f'{stat_col}_p75']:.1f} | {p[f'{stat_col}_p95']:.1f} | {p[f'{stat_col}_p100']:.1f} |\n"
            else:
                roster_md += "| DST Team | Stat | Min | 5th Pct | 25th Pct | Median | 75th Pct | 95th Pct | Max |\n"
                roster_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
                for _, p in sub_players.iterrows():
                    for stat_lbl, stat_col in [('Sacks', 'def_sack'), ('INTs', 'def_int'), ('Fumble Rec', 'def_fumble_rec'), ('TDs', 'def_td'), ('Pts Allowed', 'pts_allowed'), ('DK Fantasy', 'dk_score')]:
                        roster_md += f"| **{p['Player']}** | {stat_lbl} | {p[f'{stat_col}_p00']:.1f} | {p[f'{stat_col}_p05']:.1f} | {p[f'{stat_col}_p25']:.1f} | **{p[f'{stat_col}_p50']:.1f}** | {p[f'{stat_col}_p75']:.1f} | {p[f'{stat_col}_p95']:.1f} | {p[f'{stat_col}_p100']:.1f} |\n"
            roster_md += "\n"
            
        with open(f"{team_dir}/player_stats_and_roster.md", "w", encoding="utf-8") as f:
            f.write(roster_md)
            
    summary_df = pd.DataFrame(summary_data)
    
    # Save CSV summary
    summary_df.to_csv(f"{docs_report_dir}/season_summaries.csv", index=False)
    print(f"Saved season summaries CSV to {docs_report_dir}/season_summaries.csv")
    
    # 10. RENDER STANDINGS MD BY DIVISION/CONFERENCE (Using Median Records)
    standings_md = f"# 🏆 NFL 2025 Expected Standings & Playoff Probabilities\n"
    standings_md += f"**Calculated from expected records & playoff brackets simulated across {num_seasons} seasons**\n"
    standings_md += f"*(Traditional W/L outcomes sampled via 11-game sets per matchup. Standing records show Medians)*\n\n"
    
    for conf in ['AFC', 'NFC']:
        standings_md += f"## {conf} Conference\n\n"
        
        for div in ['East', 'North', 'South', 'West']:
            standings_md += f"### {conf} {div}\n"
            standings_md += "| Team | Median Record | Range (5%-95%) | Expected Div Rec | Points For (Avg) | Points Against (Avg) | Diff | Playoffs % | Made Divisional % | Made Conf Champ % | Made Super Bowl % | Super Bowl Winner % |\n"
            standings_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
            
            div_df = summary_df[(summary_df['Conference'] == conf) & (summary_df['Division'] == div)]
            # Sort by expected wins
            div_df = div_df.sort_values(by=['Wins_Median', 'Wins_P75', 'PF_Avg'], ascending=False)
            
            for _, r in div_df.iterrows():
                record_str = f"{r['Wins_Median']:.1f} - {r['Losses_Median']:.1f}"
                if r['Ties_Median'] > 0:
                    record_str += f" - {r['Ties_Median']:.1f}"
                    
                range_str = f"{r['Wins_P05']:.0f} to {r['Wins_P95']:.0f} wins"
                
                div_record_str = f"{r['Div_Wins_Avg']:.1f} - {r['Div_Losses_Avg']:.1f}"
                if r['Div_Ties_Avg'] > 0:
                    div_record_str += f" - {r['Div_Ties_Avg']:.1f}"
                    
                diff = r['PF_Avg'] - r['PA_Avg']
                diff_indicator = f"+{diff:.1f}" if diff > 0 else f"{diff:.1f}"
                
                standings_md += f"| **{r['Team']}** | {record_str} | {range_str} | {div_record_str} | {r['PF_Avg']:.1f} | {r['PA_Avg']:.1f} | `{diff_indicator}` | {r['Playoffs_%']:.1f}% | {r['Divional_%']:.1f}% | {r['Conference_%']:.1f}% | {r['SuperBowl_%']:.1f}% | **{r['Champion_%']:.1f}%** |\n"
            standings_md += "\n"
            
    with open(f"{docs_report_dir}/season_standings.md", "w", encoding="utf-8") as f:
        f.write(standings_md)
        
    print(f"Saved season standings markdown report to {docs_report_dir}/season_standings.md")

if __name__ == "__main__":
    simulate_full_season_and_playoffs(iterations=1000, num_seasons=100)
    print("\nFull season simulation and playoff trees completed successfully!")
