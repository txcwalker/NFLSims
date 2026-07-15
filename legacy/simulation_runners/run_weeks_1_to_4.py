import sys
import os
import time
import pandas as pd
import numpy as np
import warnings
import json
import concurrent.futures
warnings.filterwarnings('ignore')

sys.path.append(os.getcwd())
from src.nfl_sim.game_engine import NFLGameEngine
from src.nfl_sim.scoring import get_player_summary
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
    qbs = []
    rbs = []
    receivers = []
    
    for name, p_traits in traits.items():
        pos = p_traits.get('pos', 'WR/TE')
        if pos == 'QB':
            qbs.append((name, p_traits))
        elif pos == 'RB':
            rbs.append((name, p_traits))
        else:
            receivers.append((name, p_traits))
            
    slot_map = {}
    
    # Map QBs
    qbs.sort(key=lambda x: x[1].get('carry_share', 0.0) + x[1].get('target_share', 0.0), reverse=True)
    for idx, (name, _) in enumerate(qbs):
        if idx == 0:
            slot_map[name] = 'QB'
        else:
            slot_map[name] = f'QB{idx+1}'
            
    # Map RBs
    rbs.sort(key=lambda x: x[1].get('carry_share', 0.0), reverse=True)
    for idx, (name, _) in enumerate(rbs):
        slot_map[name] = f'RB{idx+1}'
        
    # Map Receivers
    te_names = {
        'kincaid', 'knox', 'kelce', 'gray', 'morris', 'davidson', 'wiley', 'fortson', 'hendershot', 
        'pitts', 'andrews', 'kittle', 'laporta', 'bowers', 'njoku', 'goedert', 'engram', 'ferguson', 
        'schultz', 'kmet', 'freiermuth', 'henry', 'likely', 'musgrave', 'kraft', 'otton', 'conklin'
    }
    
    receivers.sort(key=lambda x: x[1].get('target_share', 0.0), reverse=True)
    
    wr_idx = 1
    te_idx = 1
    for name, _ in receivers:
        is_te = False
        for te in te_names:
            if te in name.lower():
                is_te = True
                break
        if is_te:
            slot_map[name] = f'TE{te_idx}'
            te_idx += 1
        else:
            slot_map[name] = f'WR{wr_idx}'
            wr_idx += 1
            
    return slot_map

def simulate_single_game_sequential(args):
    """
    Worker function: Simulates 500 iterations of a single game sequentially.
    Reuses DNA and rosters in memory to avoid redundant file loads and massive pickle sizes.
    """
    away, home, year, iterations = args
    
    # Load shared assets for this worker process
    dna = {
        'qb': load_json('data/dna/qb_dna.json'),
        'skill': load_json('data/dna/skill_dna.json'),
        'coach': load_json('data/dna/coach_dna.json'),
        'trench': load_json('data/dna/trench_dna.json')
    }
    team_coaches = load_json('data/team_to_coach_2025.json')
    rosters = {
        away: load_json(f"data/current_rosters/{away}_traits_{year}.json").get('traits', {}),
        home: load_json(f"data/current_rosters/{home}_traits_{year}.json").get('traits', {})
    }
    trench_tiers = load_json('data/trench_tiers_2025.json')
    
    # Pre-build slot map once per game batch
    player_to_slot = {
        away: build_slot_map(away, rosters[away]),
        home: build_slot_map(home, rosters[home])
    }
    
    game_summaries = []
    raw_player_results = []
    
    for i in range(iterations):
        sim = NFLGameEngine(
            away,
            home,
            year=year,
            dna=dna,
            team_coaches=team_coaches,
            rosters=rosters,
            trench_tiers=trench_tiers
        )
        sim.run_game()
        
        summary = {
            'game_id': i,
            'off_score': sim.scores[away],
            'def_score': sim.scores[home],
            'total': sim.scores[away] + sim.scores[home],
            'spread': sim.scores[away] - sim.scores[home],
            'winner': away if sim.scores[away] > sim.scores[home] else home,
            'total_plays': sim.play_count,
            'plays_over_20_yds': getattr(sim, 'plays_over_20_yds', 0),
            'punts': getattr(sim, 'punts_run', 0),
            'fourth_down_decisions': getattr(sim, 'fourth_down_decisions', []),
            'fg_attempts_details': getattr(sim, 'fg_attempts_details', []),
            'td_details': getattr(sim, 'td_details', [])
        }
        game_summaries.append(summary)
        
        for row in sim.get_stats_report():
            row['game_id'] = i
            row['winner'] = summary['winner']
            
            # Map Slot
            player_name = row.get('Player')
            team = row.get('Team')
            slot = player_to_slot.get(team, {}).get(player_name, row.get('Pos', 'WR/TE'))
            row['Slot'] = slot
            
            # Add Fantasy summary scores
            row = get_player_summary(row)
            raw_player_results.append(row)
            
    return game_summaries, raw_player_results

def run_multi_week_sims():
    year = 2025
    iterations = 500
    weeks = [1, 2, 3, 4]
    
    print("\n" + "="*80)
    print(" NFL 2025 SEASON - Weeks 1-4 Highly Optimized Parallel Matchup Simulator")
    print(f" Simulating 500 iterations per game sequentially inside CPU workers")
    print("="*80)
    
    # Load Schedule
    schedule_cache_path = os.path.join('data', f'schedule_{year}.csv')
    if not os.path.exists(schedule_cache_path):
        print(f"Error: Schedule file not found at {schedule_cache_path}!")
        return
        
    sched = pd.read_csv(schedule_cache_path)
    
    # Standings State
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
    
    # We will use ProcessPoolExecutor across the games in a week, capped at 4 workers to prevent memory limits on Windows
    max_workers = min(4, max(1, os.cpu_count() - 1))
    
    for week in weeks:
        print("\n" + "#"*70)
        print(f" SIMULATING WEEK {week} (Parallel Matchups across {max_workers} CPU Workers)")
        print("#"*70)
        
        week_games = sched[(sched['week'] == week) & (sched['game_type'] == 'REG')].to_dict('records')
        if not week_games:
            print(f"No games found for Week {week}.")
            continue
            
        print(f"Spawning {len(week_games)} parallel matchups...")
        
        # Prepare parallel arguments
        args_list = [
            (game['away_team'], game['home_team'], year, iterations)
            for game in week_games
        ]
        
        week_start_time = time.time()
        
        # Run parallel games with live progress updates
        results_dict = {}
        futures = {}
        with concurrent.futures.ProcessPoolExecutor(max_workers=max_workers) as executor:
            for args in args_list:
                away, home, _, _ = args
                f = executor.submit(simulate_single_game_sequential, args)
                futures[f] = (away, home)
                
            completed_count = 0
            for f in concurrent.futures.as_completed(futures):
                away, home = futures[f]
                completed_count += 1
                try:
                    res = f.result()
                    results_dict[(away, home)] = res
                    print(f"  [{completed_count}/{len(week_games)}] Completed: {away} @ {home}")
                except Exception as e:
                    print(f"  [{completed_count}/{len(week_games)}] ❌ Error simulating {away} @ {home}: {e}")
                    raise e
                    
        # Reorder results back to the original schedule order
        results = [results_dict[(args[0], args[1])] for args in args_list]
            
        week_duration = time.time() - week_start_time
        print(f" -> All matches simulated in {week_duration:.1f}s! Consolidating reports...")
        
        all_game_dfs = []
        all_player_dfs = []
        
        boxscore_dir = f'docs/boxscores/week_{week}'
        output_dir = f'reports/week_{week}_2025'
        os.makedirs(boxscore_dir, exist_ok=True)
        os.makedirs(output_dir, exist_ok=True)
        
        week_boxscores = []
        for idx, game in enumerate(week_games):
            away = game['away_team']
            home = game['home_team']
            
            game_summaries, raw_player_results = results[idx]
            
            # Convert to DataFrames
            game_df = pd.DataFrame(game_summaries)
            player_df = pd.DataFrame(raw_player_results)
            
            # Add identifiers
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
            
            # Standings expected records
            standings_data[away]['exp_wins'] += away_win_pct
            standings_data[away]['exp_losses'] += home_win_pct
            standings_data[home]['exp_wins'] += home_win_pct
            standings_data[home]['exp_losses'] += away_win_pct
            
            # Probabilistic Winner Roll
            roll = np.random.random()
            if roll < home_win_pct:
                sim_winner = home
                sim_loser = away
            else:
                sim_winner = away
                sim_loser = home
                
            standings_data[sim_winner]['sim_wins'] += 1
            standings_data[sim_loser]['sim_losses'] += 1
            
            # Score sums
            standings_data[away]['points_scored_sum'] += avg_away_score
            standings_data[away]['points_against_sum'] += avg_home_score
            standings_data[away]['games_played'] += 1
            
            standings_data[home]['points_scored_sum'] += avg_home_score
            standings_data[home]['points_against_sum'] += avg_away_score
            standings_data[home]['games_played'] += 1
            
            # Generate matchup boxscore
            game_box_md = generate_matchup_boxscore(away, home, week, game_df, player_df, boxscore_dir)
            week_boxscores.append(game_box_md)
            
        # Write consolidated weekly boxscore
        week_boxscore_path = os.path.join(boxscore_dir, f"week_{week}_boxscore.md")
        with open(week_boxscore_path, 'w', encoding='utf-8') as f:
            f.write(f"# 📊 NFL 2025 Week {week} Complete Boxscores\n\n" + "\n\n---\n\n".join(week_boxscores))
        print(f"Created consolidated week boxscore: {week_boxscore_path}")
        
        combined_games = pd.concat(all_game_dfs, ignore_index=True)
        combined_players = pd.concat(all_player_dfs, ignore_index=True)
        
        all_weeks_player_dfs.append(combined_players)
        
        # Save Weekly CSVs
        game_path = f"{output_dir}/game_summaries.csv"
        player_path = f"{output_dir}/player_summaries.csv"
        raw_players_path = f"{output_dir}/raw_player_results.csv"
        pbp_path = f"{output_dir}/play_by_play_diagnostics.json"
        
        pbp_columns = ['matchup', 'game_id', 'punts', 'fourth_down_decisions', 'fg_attempts_details', 'td_details']
        pbp_df = combined_games[pbp_columns]
        pbp_df.to_json(pbp_path, orient='records', indent=2)
        
        clean_combined_games = combined_games.drop(columns=['fourth_down_decisions', 'fg_attempts_details', 'td_details'])
        clean_combined_games.to_csv(game_path, index=False)
        
        # Aggregate weekly player stats
        agg = StatAggregator()
        player_summary = agg.aggregate_player_stats(combined_players)
        player_summary = player_summary.sort_values('dk_score_avg', ascending=False)
        player_summary.to_csv(player_path, index=False)
        combined_players.to_csv(raw_players_path, index=False)
        
        # Write Weekly Markdown Summaries
        write_weekly_markdown_summaries(week, clean_combined_games, player_summary)
        
    print("\nSimulations completed. Compiling league-wide standings and leaderboards...")
    
    # Standings Page
    write_standings_page(standings_data)
    
    # Stat Leaders Page
    if all_weeks_player_dfs:
        all_players_combined = pd.concat(all_weeks_player_dfs, ignore_index=True)
        write_stat_leaders_page(all_players_combined)
        
    overall_duration = time.time() - overall_start_time
    print("\n" + "="*80)
    print(" ALL SIMULATIONS & REPORTS SUCCESSFULLY GENERATED!")
    print(f" Total processing time: {overall_duration/60:.1f} minutes ({overall_duration:.1f}s)")
    print("="*80)

def generate_matchup_boxscore(away, home, week, game_df, player_df, boxscore_dir):
    """Generates a detailed statistical boxscore for the given matchup."""
    avg_away_score = game_df['off_score'].mean()
    avg_home_score = game_df['def_score'].mean()
    away_win_pct = (game_df['winner'] == away).mean() * 100
    home_win_pct = (game_df['winner'] == home).mean() * 100
    
    avg_plays = game_df['total_plays'].mean()
    avg_explosive = game_df['plays_over_20_yds'].mean()
    avg_punts = game_df['punts'].mean()
    
    # Get player stats averages
    player_avg = player_df.groupby(['Player', 'Team', 'Slot']).mean().reset_index()
    
    # Sort and split by category
    passing_df = player_avg[player_avg['pAtt'] > 0.5].sort_values('pYds', ascending=False)
    rushing_df = player_avg[player_avg['rAtt'] > 0.5].sort_values('rYds', ascending=False)
    receiving_df = player_avg[player_avg['targets'] > 0.5].sort_values('recYds', ascending=False)
    
    # Boxscore Markdown
    box_md = f"""# 📊 Week {week} Matchup Boxscore: {away} at {home}
**Monte Carlo Simulated Averages across 500 Iterations**

## 🏈 Game Summary
* **Final Projected Score**: **{away} {avg_away_score:.1f}** - **{home} {avg_home_score:.1f}**
* **Win Probability**: {away} {away_win_pct:.1f}% | {home} {home_win_pct:.1f}%
* **Avg Total Scrimmage Plays**: {avg_plays:.1f}
* **Avg Combined Punts**: {avg_punts:.1f}
* **Avg Explosive Plays (>20 yds)**: {avg_explosive:.1f}

---

## 🎯 Passing Statistics
| Player | Team | Slot | Cmp / Att | Pass Yards | Pass TDs | INTs | Sacks Taken | DK Fantasy |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for _, row in passing_df.iterrows():
        box_md += f"| **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['pCmp']:.1f} / {row['pAtt']:.1f} | {row['pYds']:.1f} | {row['pTD']:.1f} | {row.get('int', 0.0):.1f} | {row.get('sacks_taken', 0.0):.1f} | **{row['dk_score']:.2f}** |\n"
        
    box_md += """
## 🏃 Rushing Statistics
| Player | Team | Slot | Attempts | Yards | TDs | Long | DK Fantasy |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for _, row in rushing_df.iterrows():
        box_md += f"| **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['rAtt']:.1f} | {row['rYds']:.1f} | {row['rTD']:.1f} | {row.get('rYds', 0.0)/max(1, row['rAtt']):.1f} avg | **{row['dk_score']:.2f}** |\n"
        
    box_md += """
## 👐 Receiving Statistics
| Player | Team | Slot | Targets | Receptions | Yards | TDs | DK Fantasy |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for _, row in receiving_df.iterrows():
        box_md += f"| **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['targets']:.1f} | {row['rec']:.1f} | {row['recYds']:.1f} | {row['recTD']:.1f} | **{row['dk_score']:.2f}** |\n"

    box_path = os.path.join(boxscore_dir, f"{away}_at_{home}.md")
    with open(box_path, 'w', encoding='utf-8') as f:
        f.write(box_md)
        
    return box_md

def write_weekly_markdown_summaries(week, game_df, player_summary):
    """Converts weekly CSV outputs into structured markdown files in docs/reports/"""
    reports_dir = 'docs/reports'
    os.makedirs(reports_dir, exist_ok=True)
    
    # 1. Game summaries markdown
    matchups = game_df['matchup'].unique()
    game_rows = []
    
    for m in matchups:
        m_df = game_df[game_df['matchup'] == m]
        teams = m.split('@')
        away, home = teams[0], teams[1]
        
        away_wins = len(m_df[m_df['winner'] == away])
        home_wins = len(m_df[m_df['winner'] == home])
        away_pct = (away_wins / len(m_df)) * 100
        home_pct = (home_wins / len(m_df)) * 100
        
        game_rows.append({
            'Matchup': m,
            'Simulations': len(m_df),
            'Away Score': f"{m_df['off_score'].mean():.1f}",
            'Home Score': f"{m_df['def_score'].mean():.1f}",
            'Spread': f"{m_df['spread'].mean():+.1f}",
            'Total': f"{m_df['total'].mean():.1f}",
            'Plays': f"{m_df['total_plays'].mean():.1f}",
            'Explosive': f"{m_df['plays_over_20_yds'].mean():.1f}",
            'Punts': f"{m_df['punts'].mean():.1f}",
            'Away Win %': f"{away_pct:.1f}%",
            'Home Win %': f"{home_pct:.1f}%",
            'Projected Winner': home if home_pct > away_pct else away
        })
        
    game_md = f"""# 🏈 Week {week} Game Matchup Simulation Summaries
**Monte Carlo Simulated Averages across 500 Iterations**

| Matchup | Simulations | Avg Away Score | Avg Home Score | Avg Spread | Avg Total | Avg Plays | Plays > 20 | Punts | Away Win % | Home Win % | Projected Winner |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for r in game_rows:
        game_md += f"| **{r['Matchup']}** | {r['Simulations']} | {r['Away Score']} | {r['Home Score']} | `{r['Spread']}` | {r['Total']} | {r['Plays']} | {r['Explosive']} | {r['Punts']} | {r['Away Win %']} | {r['Home Win %']} | **{r['Projected Winner']}** |\n"
        
    game_path = os.path.join(reports_dir, f"week_{week}_2025_game_summaries.md")
    with open(game_path, 'w', encoding='utf-8') as f:
        f.write(game_md)
        
    # 2. Player summaries markdown
    qbs = player_summary[player_summary['Slot'] == 'QB'].head(15)
    qb_table = """### 🎯 Quarterbacks (Top 15)
| Player | Team | Pass Att | Completions | Pass Yards | Pass TDs | INTs | Sacks Taken | Rush Att | Rush Yards | Rush TDs | DK Avg |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for _, row in qbs.iterrows():
        qb_table += f"| **{row['Player']}** | {row['Team']} | {row['pAtt_avg']:.1f} | {row['pCmp_avg']:.1f} | {row['pYds_avg']:.1f} | {row['pTD_avg']:.1f} | {row.get('int_avg', 0.0):.1f} | {row.get('sacks_taken_avg', 0.0):.1f} | {row['rAtt_avg']:.1f} | {row['rYds_avg']:.1f} | {row['rTD_avg']:.1f} | **{row['dk_score_avg']:.2f}** |\n"

    rbs = player_summary[player_summary['Slot'].str.startswith('RB', na=False)].head(15)
    rb_table = """### 🏃 Running Backs (Top 15)
| Player | Team | Slot | Rush Att | Rush Yards | Rush TDs | Rec | Rec Yards | Rec TDs | DK Avg |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for _, row in rbs.iterrows():
        rb_table += f"| **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['rAtt_avg']:.1f} | {row['rYds_avg']:.1f} | {row['rTD_avg']:.1f} | {row['rec_avg']:.1f} | {row['recYds_avg']:.1f} | {row['recTD_avg']:.1f} | **{row['dk_score_avg']:.2f}** |\n"

    wrs = player_summary[player_summary['Slot'].str.startswith('WR', na=False)].head(20)
    wr_table = """### 👐 Wide Receivers (Top 20)
| Player | Team | Slot | Targets | Rec | Rec Yards | Rec TDs | DK Avg |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for _, row in wrs.iterrows():
        wr_table += f"| **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['targets_avg']:.1f} | {row['rec_avg']:.1f} | {row['recYds_avg']:.1f} | {row['recTD_avg']:.1f} | **{row['dk_score_avg']:.2f}** |\n"

    tes = player_summary[player_summary['Slot'].str.startswith('TE', na=False)].head(15)
    te_table = """### 🏈 Tight Ends (Top 15)
| Player | Team | Slot | Targets | Rec | Rec Yards | Rec TDs | DK Avg |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for _, row in tes.iterrows():
        te_table += f"| **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['targets_avg']:.1f} | {row['rec_avg']:.1f} | {row['recYds_avg']:.1f} | {row['recTD_avg']:.1f} | **{row['dk_score_avg']:.2f}** |\n"

    player_md = f"""# 📈 Week {week} Player Fantasy Projections Summaries
**Monte Carlo Simulated Averages across 500 Iterations**

{qb_table}

{rb_table}

{wr_table}

{te_table}
"""
    player_path = os.path.join(reports_dir, f"week_{week}_2025_player_summaries.md")
    with open(player_path, 'w', encoding='utf-8') as f:
        f.write(player_md)

def write_standings_page(standings_data):
    """Generates a premium standings report under docs/reports/weeks_1_to_4_standings.md"""
    reports_dir = 'docs/reports'
    os.makedirs(reports_dir, exist_ok=True)
    
    standings_list = list(standings_data.values())
    
    # Sort key: Simulated Wins (Desc), then Point Diff (Desc)
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
        
    standings_md = f"""# 🏆 NFL 2025 Weeks 1-4 Cumulative Standings
**Generated using Monte Carlo Matchup Projections (500 Iterations/Game)**

This standings sheet presents the simulated results and point tallies after 4 weeks of the NFL season. 
It lists the **Simulated Record** (determined by a probabilistic roll on the win/loss percentage of each match to introduce realistic season-long variance) and the **Expected Record** (the mathematically exact summation of matchups win probabilities).

---

## 📈 1. Divisional Standings

{division_blocks}

---

## 📊 2. Conference Standings

{conf_blocks}
"""
    
    standings_path = os.path.join(reports_dir, 'weeks_1_to_4_standings.md')
    with open(standings_path, 'w', encoding='utf-8') as f:
        f.write(standings_md)
    print(f"Created standings page: {standings_path}")

def write_stat_leaders_page(player_df):
    """Generates a premium leaderboards page at docs/reports/weeks_1_to_4_stat_leaders.md"""
    reports_dir = 'docs/reports'
    os.makedirs(reports_dir, exist_ok=True)
    
    cols = ['pAtt', 'pCmp', 'pYds', 'pTD', 'int', 'rAtt', 'rYds', 'rTD', 'recYds', 'recTD', 'targets', 'rec', 'fumbles', 'fumbles_lost', 'sacks_taken', 'dk_score', 'fd_score']
    for c in cols:
        if c not in player_df.columns:
            player_df[c] = 0.0
        else:
            player_df[c] = player_df[c].fillna(0.0)
            
    player_groups = []
    for (player_name, team, slot), group in player_df.groupby(['Player', 'Team', 'Slot']):
        games = len(group) / 500.0
        if games < 0.5: continue
        
        player_groups.append({
            'Player': player_name,
            'Team': team,
            'Slot': slot,
            'Games': games,
            'pAtt': group['pAtt'].sum() / (games * 500),
            'pCmp': group['pCmp'].sum() / (games * 500),
            'pYds': group['pYds'].sum() / (games * 500),
            'pTD': group['pTD'].sum() / (games * 500),
            'int': group['int'].sum() / (games * 500),
            'rAtt': group['rAtt'].sum() / (games * 500),
            'rYds': group['rYds'].sum() / (games * 500),
            'rTD': group['rTD'].sum() / (games * 500),
            'recYds': group['recYds'].sum() / (games * 500),
            'recTD': group['recTD'].sum() / (games * 500),
            'targets': group['targets'].sum() / (games * 500),
            'rec': group['rec'].sum() / (games * 500),
            'fumbles': group['fumbles'].sum() / (games * 500),
            'fumbles_lost': group['fumbles_lost'].sum() / (games * 500),
            'sacks_taken': group['sacks_taken'].sum() / (games * 500),
            'dk_score': group['dk_score'].mean(),
            'fd_score': group['fd_score'].mean()
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
    turnovers = df_leaders[(df_leaders['int'] > 0.05) | (df_leaders['fumbles_lost'] > 0.05)].sort_values('int', ascending=False).head(15)
    to_table = """### 💥 Turnovers & Chaos Leaders (Most Interceptions & Sacks Suffered)
| Rank | Player | Team | Slot | Games | INTs/G | Fumbles/G | Fumbles Lost/G | Sacks Taken/G | Turnovers/G |
| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for idx, (_, row) in enumerate(turnovers.iterrows(), 1):
        tot_to = row['int'] + row['fumbles_lost']
        to_table += f"| {idx} | **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['Games']:.0f} | {row['int']:.2f} | {row['fumbles']:.2f} | {row['fumbles_lost']:.2f} | {row['sacks_taken']:.2f} | **{tot_to:.2f}** |\n"

    leaders_md = f"""# 📊 NFL 2025 Weeks 1-4 Player Statistical Leaderboards
**Generated from Monte Carlo Matchup Projections (500 Iterations/Game)**

This sheet tracks the league statistical leaders across the first 4 weeks of the NFL regular season. 
All statistics are **per-game averages** calculated across the 500 game iterations simulated for every team.

---

{qb_table}

---

{rush_table}

---

{rec_table}

---

{to_table}
"""
    leaders_path = os.path.join(reports_dir, 'weeks_1_to_4_stat_leaders.md')
    with open(leaders_path, 'w', encoding='utf-8') as f:
        f.write(leaders_md)
    print(f"Created stat leaders page: {leaders_path}")

if __name__ == '__main__':
    run_multi_week_sims()
