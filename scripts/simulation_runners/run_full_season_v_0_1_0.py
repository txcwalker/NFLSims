import nfl_data_py as nfl
import pandas as pd
import numpy as np
from concurrent.futures import ProcessPoolExecutor
from src.nfl_sim.engine import NFLGameSimulator
from src.nfl_sim.scoring import calculate_fantasy_points
from src.data_pipeline.roster_manager import RosterManager
from src.data_pipeline.report_generator import generate_weekly_report
import os
import time

def simulate_game_batch(game_info, iterations=500):
    away = game_info['away_team']
    home = game_info['home_team']
    
    game_results = []
    player_results = {}
    
    for _ in range(iterations):
        sim = NFLGameSimulator(away, home)
        while not sim.game_over:
            sim.simulate_play()
        
        game_results.append({
            'away_score': sim.scores[away],
            'home_score': sim.scores[home],
            'total_plays': sim.total_plays,
            'winner': away if sim.scores[away] > sim.scores[home] else home
        })
        
        stats_df = sim.get_stats_report()
        for _, row in stats_df.iterrows():
            p = row['Player']
            p_stats = row.to_dict()
            p_stats['dk_score'] = calculate_fantasy_points(p_stats, "DK")
            if p not in player_results: player_results[p] = []
            player_results[p].append(p_stats)

    game_summary = {
        'game_id': game_info['game_id'],
        'away_team': away,
        'home_team': home,
        'away_win_rate': round(sum(1 for r in game_results if r['winner'] == away) / iterations, 3),
        'avg_total': round(sum(r['away_score'] + r['home_score'] for r in game_results) / iterations, 2),
        'avg_away_score': round(sum(r['away_score'] for r in game_results) / iterations, 2),
        'avg_home_score': round(sum(r['home_score'] for r in game_results) / iterations, 2),
        'avg_plays': round(sum(r['total_plays'] for r in game_results) / iterations, 1),
    }
    
    player_rows = []
    for player, stat_list in player_results.items():
        pdf = pd.DataFrame(stat_list)
        player_rows.append({
            'game_id': game_info['game_id'],
            'Player': player,
            'Pos': pdf['Pos'].iloc[0],
            'Team': pdf['Team'].iloc[0],
            'Avg_DK': round(pdf['dk_score'].mean(), 2),
            'Ceiling_DK': round(pdf['dk_score'].quantile(0.95), 2),
            'Avg_Yds': round((pdf['pYds'].fillna(0) + pdf['rYds'].fillna(0) + pdf['recYds'].fillna(0)).mean(), 2),
            'Avg_TDs': round((pdf['pTD'].fillna(0) + pdf['rTD'].fillna(0) + pdf['recTD'].fillna(0)).mean(), 2),
            'Avg_pAtt': round(pdf['pAtt'].mean(), 1),
            'Avg_pCmp': round(pdf['pCmp'].mean(), 1),
            'Avg_rAtt': round(pdf['rAtt'].mean(), 1),
            'Avg_Targets': round(pdf['targets'].mean(), 1),
            'Avg_Rec': round(pdf['rec'].mean(), 1),
        })
            
    return game_summary, player_rows

def run_pilot_season(year=2023, start_week=1, end_week=4, iterations=500):
    rm = RosterManager()
    
    # 1. LOAD FULL SEASON SCHEDULE
    sched = nfl.import_schedules([year])
    
    for week in range(start_week, end_week + 1):
        print(f"\n{'='*50}")
        print(f" STARTING PILOT WEEK {week}")
        print(f"{'='*50}")
        
        # 2. UPDATE ROSTER DNA (L4 / Rolling)
        rm.generate_rolling_dna(year=year, max_week=week)
        
        # 3. GET WEEKLY GAMES
        week_games = sched[sched['week'] == week].to_dict('records')
        
        print(f"Simulating {len(week_games)} games ({iterations} iterations each)...")
        start_time = time.time()
        
        all_game_summaries = []
        all_player_projections = []

        with ProcessPoolExecutor() as executor:
            futures = [executor.submit(simulate_game_batch, g, iterations) for g in week_games]
            for future in futures:
                game_sum, player_rows = future.result()
                all_game_summaries.append(game_sum)
                all_player_projections.extend(player_rows)

        # 4. SAVE REPORTS
        if not os.path.exists('reports/season_sim'): os.makedirs('reports/season_sim')
        
        game_csv = f'reports/season_sim/week_{week}_hybrid_game_summary.csv'
        player_csv = f'reports/season_sim/week_{week}_hybrid_player_projections.csv'
        
        pd.DataFrame(all_game_summaries).to_csv(game_csv, index=False)
        pd.DataFrame(all_player_projections).sort_values('Avg_DK', ascending=False).to_csv(player_csv, index=False)
        
        # 5. GENERATE TXT REPORT
        generate_weekly_report(week)
        
        duration = time.time() - start_time
        print(f"WEEK {week} COMPLETE: {duration:.2f}s")

if __name__ == "__main__":
    run_pilot_season(start_week=1, end_week=4, iterations=500)
