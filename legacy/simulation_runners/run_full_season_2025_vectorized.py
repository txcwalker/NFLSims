import sys
import os
import time
import pandas as pd
import numpy as np
import warnings
import json
warnings.filterwarnings('ignore')

sys.path.append(os.getcwd())
from src.nfl_sim.game_engine import NFLGameEngine
from src.nfl_sim.batch import StatAggregator

# Division Mappings
DIVISIONS = {
    "AFC East": ["BUF", "MIA", "NE", "NYJ"],
    "AFC North": ["BAL", "CIN", "CLE", "PIT"],
    "AFC South": ["HOU", "IND", "JAX", "TEN"],
    "AFC West": ["DEN", "KC", "LV", "LAC"],
    "NFC East": ["DAL", "NYG", "PHI", "WAS"],
    "NFC North": ["CHI", "DET", "GB", "MIN"],
    "NFC South": ["ATL", "CAR", "NO", "TB"],
    "NFC West": ["ARI", "LA", "SF", "SEA"]
}

TEAM_TO_DIV = {}
for div, teams in DIVISIONS.items():
    for t in teams:
        TEAM_TO_DIV[t] = div

def load_json(path):
    if os.path.exists(path):
        with open(path, 'r') as f:
            return json.load(f)
    return {}

def build_slot_map(team, traits):
    qbs, rbs, receivers = [], [], []
    for name, p_traits in traits.items():
        pos = p_traits.get('pos', 'WR/TE')
        if pos == 'QB': qbs.append((name, p_traits))
        elif pos == 'RB': rbs.append((name, p_traits))
        else: receivers.append((name, p_traits))
            
    slot_map = {}
    qbs.sort(key=lambda x: x[1].get('carry_share', 0.0) + x[1].get('target_share', 0.0), reverse=True)
    for idx, (name, _) in enumerate(qbs):
        slot_map[name] = 'QB' if idx == 0 else f'QB{idx+1}'
        
    rbs.sort(key=lambda x: x[1].get('carry_share', 0.0), reverse=True)
    for idx, (name, _) in enumerate(rbs):
        slot_map[name] = f'RB{idx+1}'
        
    te_names = {
        'kincaid', 'knox', 'kelce', 'gray', 'morris', 'davidson', 'wiley', 'fortson', 'hendershot', 
        'pitts', 'andrews', 'kittle', 'laporta', 'bowers', 'njoku', 'goedert', 'engram', 'ferguson', 
        'schultz', 'kmet', 'freiermuth', 'henry', 'likely', 'musgrave', 'kraft', 'otton', 'conklin'
    }
    receivers.sort(key=lambda x: x[1].get('target_share', 0.0), reverse=True)
    wr_idx, te_idx = 1, 1
    for name, _ in receivers:
        is_te = any(te in name.lower() for te in te_names)
        if is_te:
            slot_map[name] = f'TE{te_idx}'
            te_idx += 1
        else:
            slot_map[name] = f'WR{wr_idx}'
            wr_idx += 1
    return slot_map

def run_full_season_vectorized():
    year = 2025
    iterations = 1000
    weeks = list(range(1, 19)) # Weeks 1 to 18
    
    print("\n" + "="*80)
    print(" NFL 2025 SEASON - FULL 18-WEEK VECTORIZED SIMULATOR")
    print(f" Simulating {iterations} iterations per game concurrently using NumPy")
    print("="*80)
    
    schedule_cache_path = os.path.join('data', f'schedule_{year}.csv')
    if not os.path.exists(schedule_cache_path):
        print(f"Error: Schedule file not found at {schedule_cache_path}!")
        return
    sched = pd.read_csv(schedule_cache_path)
    
    standings_data = {
        team: {
            'team': team,
            'division': TEAM_TO_DIV.get(team, 'Unknown'),
            'sim_wins': 0,
            'sim_losses': 0,
            'exp_wins': 0.0,
            'exp_losses': 0.0,
            'points_scored_sum': 0.0,
            'points_against_sum': 0.0,
            'games_played': 0
        }
        for team in TEAM_TO_DIV.keys()
    }
    
    all_weeks_player_dfs = []
    overall_start_time = time.time()
    
    # Load shared assets once
    dna = {
        'qb': load_json('data/dna/qb_dna.json'),
        'skill': load_json('data/dna/skill_dna.json'),
        'coach': load_json('data/dna/coach_dna.json'),
        'trench': load_json('data/dna/trench_dna.json')
    }
    team_coaches = load_json('data/team_to_coach_2025.json')
    trench_tiers = load_json('data/trench_tiers_2025.json')
    
    for week in weeks:
        print(f"\nSIMULATING WEEK {week}...")
        week_games = sched[(sched['week'] == week) & (sched['game_type'] == 'REG')].to_dict('records')
        if not week_games:
            print(f"No regular season games found for Week {week}.")
            continue
            
        all_game_dfs = []
        all_player_dfs = []
        
        week_start_time = time.time()
        for idx, game in enumerate(week_games):
            away = game['away_team']
            home = game['home_team']
            
            # Load game rosters
            rosters = {
                away: load_json(f"data/current_rosters/{away}_traits_{year}.json").get('traits', {}),
                home: load_json(f"data/current_rosters/{home}_traits_{year}.json").get('traits', {})
            }
            player_to_slot = {
                away: build_slot_map(away, rosters[away]),
                home: build_slot_map(home, rosters[home])
            }
            
            # Vectorized Engine Run!
            sim = VectorizedNFLGameEngine(
                away, home, year=year, N=iterations,
                dna=dna, team_coaches=team_coaches, rosters=rosters, trench_tiers=trench_tiers
            )
            sim.run_game()
            
            # Retrieve summaries
            game_summaries = sim.get_game_summaries()
            raw_player_results = sim.get_player_stats_flat(player_to_slot)
            
            game_df = pd.DataFrame(game_summaries)
            player_df = pd.DataFrame(raw_player_results)
            
            game_df['week'] = week
            game_df['matchup'] = f"{away}@{home}"
            player_df['week'] = week
            player_df['matchup'] = f"{away}@{home}"
            
            all_game_dfs.append(game_df)
            all_player_dfs.append(player_df)
            
            # --- Update Standings Averages & Rolls ---
            avg_away_score = game_df['off_score'].mean()
            avg_home_score = game_df['def_score'].mean()
            away_win_pct = (game_df['winner'] == away).mean()
            home_win_pct = (game_df['winner'] == home).mean()
            
            standings_data[away]['exp_wins'] += away_win_pct
            standings_data[away]['exp_losses'] += home_win_pct
            standings_data[home]['exp_wins'] += home_win_pct
            standings_data[home]['exp_losses'] += away_win_pct
            
            # Probabilistic Record Roll
            roll = np.random.random()
            if roll < home_win_pct:
                standings_data[home]['sim_wins'] += 1
                standings_data[away]['sim_losses'] += 1
            else:
                standings_data[away]['sim_wins'] += 1
                standings_data[home]['sim_losses'] += 1
                
            # Score updates
            standings_data[away]['points_scored_sum'] += avg_away_score
            standings_data[away]['points_against_sum'] += avg_home_score
            standings_data[away]['games_played'] += 1
            standings_data[home]['points_scored_sum'] += avg_home_score
            standings_data[home]['points_against_sum'] += avg_away_score
            standings_data[home]['games_played'] += 1
            
            print(f"  [{idx+1}/{len(week_games)}] Completed: {away} @ {home}")
            
        week_duration = time.time() - week_start_time
        print(f" -> Week {week} simulated in {week_duration:.1f}s!")
        
        combined_players = pd.concat(all_player_dfs, ignore_index=True)
        # Keep only player averages per week to prevent memory overflow during 18-week aggregates
        # We group by player name and slot, sum raw numbers and divide by N (games) later
        # Group weekly player summaries
        weekly_grouped = combined_players.groupby(['Player', 'Team', 'Slot']).sum().reset_index()
        all_weeks_player_dfs.append(weekly_grouped)
        
    print("\nSimulations completed. Compiling league-wide standings and leaderboards...")
    
    # Standings Page (Test Run 2)
    write_standings_page(standings_data)
    
    # Stat Leaders Page (Test Run 2)
    if all_weeks_player_dfs:
        all_players_combined = pd.concat(all_weeks_player_dfs, ignore_index=True)
        # Group across all 18 weeks
        write_stat_leaders_page(all_players_combined, iterations)
        
    overall_duration = time.time() - overall_start_time
    print("\n" + "="*80)
    print(" ALL SIMULATIONS & REPORTS SUCCESSFULLY GENERATED!")
    print(f" Total processing time: {overall_duration/60:.1f} minutes ({overall_duration:.1f}s)")
    print("="*80)

def write_standings_page(standings_data):
    reports_dir = 'docs/reports/test run 2'
    os.makedirs(reports_dir, exist_ok=True)
    standings_list = list(standings_data.values())
    
    def get_sort_key(t):
        p_diff = t['points_scored_sum'] - t['points_against_sum']
        return (t['sim_wins'], p_diff)
        
    division_blocks = ""
    for div, teams in DIVISIONS.items():
        div_teams = [t for t in standings_list if t['team'] in teams]
        div_teams.sort(key=get_sort_key, reverse=True)
        
        division_blocks += f"### 🏆 {div}\n"
        division_blocks += "| Team | Simulated Record | Win % | Expected Record | Points Scored | Points Against | Net Diff |\n"
        division_blocks += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: |\n"
        
        for t in div_teams:
            p_scored = t['points_scored_sum'] / max(1, t['games_played'])
            p_against = t['points_against_sum'] / max(1, t['games_played'])
            net_diff = p_scored - p_against
            sim_rec = f"{t['sim_wins']}-{t['sim_losses']}"
            exp_rec = f"{t['exp_wins']:.2f}-{t['exp_losses']:.2f}"
            win_pct = (t['sim_wins'] / max(1, t['games_played'])) * 100
            division_blocks += f"| **{t['team']}** | `{sim_rec}` | {win_pct:.1f}% | `{exp_rec}` | {p_scored:.1f} | {p_against:.1f} | `{net_diff:+.1f}` |\n"
        division_blocks += "\n"
        
    conferences = {
        "AFC": ["AFC East", "AFC North", "AFC South", "AFC West"],
        "NFC": ["NFC East", "NFC North", "NFC South", "NFC West"]
    }
    
    conf_blocks = ""
    for conf, div_list in conferences.items():
        conf_teams = [t for t in standings_list if t['division'] in div_list]
        conf_teams.sort(key=get_sort_key, reverse=True)
        conf_blocks += f"### 🔴 {conf} Conference Standings\n"
        conf_blocks += "| Rank | Team | Division | Simulated Record | Expected Record | Points Scored | Points Against | Net Diff |\n"
        conf_blocks += "| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: |\n"
        for idx, t in enumerate(conf_teams, 1):
            p_scored = t['points_scored_sum'] / max(1, t['games_played'])
            p_against = t['points_against_sum'] / max(1, t['games_played'])
            net_diff = p_scored - p_against
            sim_rec = f"{t['sim_wins']}-{t['sim_losses']}"
            exp_rec = f"{t['exp_wins']:.2f}-{t['exp_losses']:.2f}"
            conf_blocks += f"| {idx} | **{t['team']}** | {t['division']} | `{sim_rec}` | `{exp_rec}` | {p_scored:.1f} | {p_against:.1f} | `{net_diff:+.1f}` |\n"
        conf_blocks += "\n"
        
    standings_md = f"""# 🏆 NFL 2025 Full Season Standings
**Generated using Vectorized Monte Carlo Matchup Projections (1,000 Iterations/Game)**

This standings sheet presents the simulated results and point tallies after a full 18-week NFL regular season. 
It lists the **Simulated Record** (determined by a probabilistic roll on the win/loss percentage of each match to introduce realistic season-long variance) and the **Expected Record** (the mathematically exact summation of matchups win probabilities).

---

## 📈 1. Divisional Standings

{division_blocks}

---

## 📊 2. Conference Standings

{conf_blocks}
"""
    standings_path = os.path.join(reports_dir, 'full_season_standings.md')
    with open(standings_path, 'w', encoding='utf-8') as f:
        f.write(standings_md)
    print(f"Created standings page: {standings_path}")

def write_stat_leaders_page(player_df, N):
    reports_dir = 'docs/reports/test run 2'
    os.makedirs(reports_dir, exist_ok=True)
    
    cols = ['pAtt', 'pCmp', 'pYds', 'pTD', 'int', 'rAtt', 'rYds', 'rTD', 'recYds', 'recTD', 'targets', 'rec', 'fumbles', 'fumbles_lost', 'sacks_taken', 'dk_score', 'fd_score']
    for c in cols:
        if c not in player_df.columns: player_df[c] = 0.0
        else: player_df[c] = player_df[c].fillna(0.0)
            
    player_groups = []
    # Aggregate sums across all weekly groups
    for (player_name, team, slot), group in player_df.groupby(['Player', 'Team', 'Slot']):
        games = len(group)
        if games < 2: continue # Only keep active players with games played
        
        player_groups.append({
            'Player': player_name, 'Team': team, 'Slot': slot, 'Games': games,
            'pAtt': group['pAtt'].sum() / (games * N),
            'pCmp': group['pCmp'].sum() / (games * N),
            'pYds': group['pYds'].sum() / (games * N),
            'pTD': group['pTD'].sum() / (games * N),
            'int': group['int'].sum() / (games * N),
            'rAtt': group['rAtt'].sum() / (games * N),
            'rYds': group['rYds'].sum() / (games * N),
            'rTD': group['rTD'].sum() / (games * N),
            'recYds': group['recYds'].sum() / (games * N),
            'recTD': group['recTD'].sum() / (games * N),
            'targets': group['targets'].sum() / (games * N),
            'rec': group['rec'].sum() / (games * N),
            'fumbles': group['fumbles'].sum() / (games * N),
            'fumbles_lost': group['fumbles_lost'].sum() / (games * N),
            'sacks_taken': group['sacks_taken'].sum() / (games * N),
            'dk_score': group['dk_score'].sum() / (games * N),
            'fd_score': group['fd_score'].sum() / (games * N)
        })
        
    df_leaders = pd.DataFrame(player_groups)
    
    # QBs
    qbs = df_leaders[df_leaders['Slot'] == 'QB'].sort_values('pYds', ascending=False).head(15)
    qb_table = """### 🎯 Passing Leaders (Top 15 by Yards per Game)
| Rank | Player | Team | Games | Pass Yards/G | Pass TDs/G | Cmp / Att | INTs/G | Sacks Taken/G | DK Fantasy |
| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for idx, (_, row) in enumerate(qbs.iterrows(), 1):
        qb_table += f"| {idx} | **{row['Player']}** | {row['Team']} | {row['Games']:.0f} | {row['pYds']:.1f} | {row['pTD']:.2f} | {row['pCmp']:.1f} / {row['pAtt']:.1f} | {row['int']:.2f} | {row['sacks_taken']:.2f} | **{row['dk_score']:.2f}** |\n"
        
    # Rushing
    rushing = df_leaders.sort_values('rYds', ascending=False).head(15)
    rush_table = """### 🏃 Rushing Leaders (Top 15 by Yards per Game)
| Rank | Player | Team | Slot | Games | Rush Yards/G | Rush TDs/G | Attempts/G | YPC | DK Fantasy |
| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for idx, (_, row) in enumerate(rushing.iterrows(), 1):
        ypc = row['rYds'] / max(0.1, row['rAtt'])
        rush_table += f"| {idx} | **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['Games']:.0f} | {row['rYds']:.1f} | {row['rTD']:.2f} | {row['rAtt']:.1f} | {ypc:.2f} | **{row['dk_score']:.2f}** |\n"
        
    # Receiving
    receiving = df_leaders.sort_values('recYds', ascending=False).head(20)
    rec_table = """### 👐 Receiving Leaders (Top 20 by Yards per Game)
| Rank | Player | Team | Slot | Games | Rec Yards/G | Rec TDs/G | Targets/G | Receptions/G | DK Fantasy |
| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for idx, (_, row) in enumerate(receiving.iterrows(), 1):
        rec_table += f"| {idx} | **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['Games']:.0f} | {row['recYds']:.1f} | {row['recTD']:.2f} | {row['targets']:.1f} | {row['rec']:.1f} | **{row['dk_score']:.2f}** |\n"
        
    # Turnovers & Sacks
    turnovers = df_leaders[(df_leaders['int'] > 0.02) | (df_leaders['fumbles_lost'] > 0.02)].sort_values('int', ascending=False).head(15)
    to_table = """### 💥 Turnovers & Chaos Leaders (Most Interceptions & Sacks Suffered)
| Rank | Player | Team | Slot | Games | INTs/G | Fumbles/G | Fumbles Lost/G | Sacks Taken/G | Turnovers/G |
| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for idx, (_, row) in enumerate(turnovers.iterrows(), 1):
        tot_to = row['int'] + row['fumbles_lost']
        to_table += f"| {idx} | **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['Games']:.0f} | {row['int']:.2f} | {row['fumbles']:.2f} | {row['fumbles_lost']:.2f} | {row['sacks_taken']:.2f} | **{tot_to:.2f}** |\n"
        
    leaders_md = f"""# 📊 NFL 2025 Full Season Player Statistical Leaderboards
**Generated from Vectorized Monte Carlo Matchup Projections (1,000 Iterations/Game)**

This sheet tracks the league statistical leaders across a full 18-week NFL regular season. 
All statistics are **per-game averages** calculated across the 1,000 game iterations simulated for every team.

---

{qb_table}

---

{rush_table}

---

{rec_table}

---

{to_table}
"""
    leaders_path = os.path.join(reports_dir, 'full_season_stat_leaders.md')
    with open(leaders_path, 'w', encoding='utf-8') as f:
        f.write(leaders_md)
    print(f"Created stat leaders page: {leaders_path}")

if __name__ == '__main__':
    run_full_season_vectorized()
