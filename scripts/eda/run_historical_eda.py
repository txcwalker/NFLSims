import pandas as pd
import numpy as np
import nfl_data_py as nfl
import os
import json

def run_historical_eda():
    print("==================================================")
    print("Running Historical NFL EDA (2021-2025)")
    print("==================================================")
    
    seasons = list(range(2021, 2026))
    print(f"Importing play-by-play data for seasons: {seasons}")
    df_raw = nfl.import_pbp_data(seasons)
    print(f"Loaded {len(df_raw)} play-by-play rows.")
    
    # Filter to Regular Season games only
    # The column in nfl_data_py is 'season_type' (e.g. 'REG', 'POST')
    if 'season_type' in df_raw.columns:
        df = df_raw[df_raw['season_type'] == 'REG'].copy()
    elif 'game_type' in df_raw.columns:
        df = df_raw[df_raw['game_type'] == 'REG'].copy()
    else:
        print("Columns in df_raw:", list(df_raw.columns)[:20])
        raise KeyError("Could not find season_type or game_type in columns.")
    print(f"Filtered to Regular Season: {len(df)} rows.")
    
    # --------------------------------------------------
    # 1. Sacks per Game & Avg Passing Yards per Game
    # --------------------------------------------------
    # We want game-level statistics per team-game
    # In nfl_data_py, every play has home_team, away_team, and posteam.
    # Group by game_id and posteam
    df_pass = df[df['play_type'] == 'pass']
    
    # Sacks per game
    # Let's count sacks per game for the team that had possession (posteam)
    # A play is a sack if sack == 1
    # Let's aggregate by game_id and posteam
    game_team = df.dropna(subset=['posteam']).groupby(['game_id', 'posteam', 'season'])
    
    # Sacks
    game_sacks = df[df['sack'] == 1].groupby(['game_id', 'posteam'])['sack'].sum().rename('sacks')
    # Passing yards
    # Note: pass_yards in nfl_data_py can be null on sacks, etc. passing_yards is standard field.
    game_pass_yds = df[df['play_type'] == 'pass'].groupby(['game_id', 'posteam'])['passing_yards'].sum().rename('pass_yds')
    # Passing TDs
    game_pass_tds = df[df['pass_touchdown'] == 1].groupby(['game_id', 'posteam'])['pass_touchdown'].sum().rename('pass_tds')
    
    # Combine
    game_stats = df.dropna(subset=['posteam']).drop_duplicates(subset=['game_id', 'posteam'])[['game_id', 'posteam', 'season']].copy()
    game_stats = game_stats.merge(game_sacks, on=['game_id', 'posteam'], how='left').fillna(0)
    game_stats = game_stats.merge(game_pass_yds, on=['game_id', 'posteam'], how='left').fillna(0)
    game_stats = game_stats.merge(game_pass_tds, on=['game_id', 'posteam'], how='left').fillna(0)
    
    overall_sacks_per_game = game_stats['sacks'].mean()
    overall_pass_yds_per_game = game_stats['pass_yds'].mean()
    overall_pass_tds_per_game = game_stats['pass_tds'].mean()

    print(f"Overall Sacks per Team-Game: {overall_sacks_per_game:.3f}")
    print(f"Overall Pass Yards per Team-Game: {overall_pass_yds_per_game:.1f}")
    print(f"Overall Pass TDs per Team-Game: {overall_pass_tds_per_game:.3f}")

    # --------------------------------------------------
    # 1b. Snap counts & penalties per game (clock_physics_v020 audit)
    # Bucket definitions matched to game_engine.py's get_game_summaries():
    #   offensive_snaps      = play_type in ['pass', 'run']            (== sim play_count)
    #   special_teams_snaps  = play_type in ['punt','field_goal','kickoff']
    #   presnap_penalty_snaps= play_type == 'no_play'                  (dead-ball presnap penalty, no down run)
    #   penalties_accepted   = penalty == 1                            (ANY accepted penalty, presnap or mid-play)
    #   total_snaps          = offensive_snaps + special_teams_snaps + presnap_penalty_snaps
    #
    # IMPORTANT: game_engine.py's counters are single WHOLE-GAME tallies (both teams
    # combined, e.g. self.play_count increments regardless of which team has the ball).
    # So these real-data metrics are summed across BOTH teams per game_id, NOT averaged
    # per team-game like sacks/pass_yards/pass_tds above — otherwise the comparison
    # would be off by ~2x for no real reason. Kickoff posteam attribution in nflfastR
    # is inconsistent (kicking vs receiving team varies by season/format); this nets
    # out across many games when summing both teams per game_id.
    # --------------------------------------------------
    df_valid = df.dropna(subset=['posteam'])

    game_off_snaps = df_valid[df_valid['play_type'].isin(['pass', 'run'])].groupby(['game_id'])['play_type'].count().rename('offensive_snaps')
    game_st_snaps = df_valid[df_valid['play_type'].isin(['punt', 'field_goal', 'kickoff'])].groupby(['game_id'])['play_type'].count().rename('special_teams_snaps')
    game_penalty_snaps = df_valid[df_valid['play_type'] == 'no_play'].groupby(['game_id'])['play_type'].count().rename('presnap_penalty_snaps')
    game_penalties_accepted = df_valid[df_valid['penalty'] == 1].groupby(['game_id'])['penalty'].count().rename('penalties_accepted')

    snap_stats = df_valid[['game_id']].drop_duplicates().copy()
    snap_stats = snap_stats.merge(game_off_snaps, on='game_id', how='left').fillna(0)
    snap_stats = snap_stats.merge(game_st_snaps, on='game_id', how='left').fillna(0)
    snap_stats = snap_stats.merge(game_penalty_snaps, on='game_id', how='left').fillna(0)
    snap_stats = snap_stats.merge(game_penalties_accepted, on='game_id', how='left').fillna(0)
    snap_stats['total_snaps'] = snap_stats['offensive_snaps'] + snap_stats['special_teams_snaps'] + snap_stats['presnap_penalty_snaps']

    overall_offensive_snaps_per_game = snap_stats['offensive_snaps'].mean()
    overall_special_teams_snaps_per_game = snap_stats['special_teams_snaps'].mean()
    overall_presnap_penalty_snaps_per_game = snap_stats['presnap_penalty_snaps'].mean()
    overall_penalties_accepted_per_game = snap_stats['penalties_accepted'].mean()
    overall_total_snaps_per_game = snap_stats['total_snaps'].mean()

    print(f"Overall Offensive Snaps per Game (both teams): {overall_offensive_snaps_per_game:.2f}")
    print(f"Overall Special Teams Snaps per Game (both teams): {overall_special_teams_snaps_per_game:.2f}")
    print(f"Overall Presnap Penalty Snaps per Game (both teams): {overall_presnap_penalty_snaps_per_game:.3f}")
    print(f"Overall Penalties Accepted per Game (both teams): {overall_penalties_accepted_per_game:.3f}")
    print(f"Overall Total Snaps per Game (both teams): {overall_total_snaps_per_game:.2f}")
    
    # --------------------------------------------------
    # 2. Red Zone and 5-Zone Efficiency by Team
    # --------------------------------------------------
    # RZ: plays starting inside opponent 20. But efficiency is usually defined at drive level.
    # Let's define it as: how often a drive that enters the redzone (yardline_100 <= 20) or 5-zone (yardline_100 <= 5) results in a TD vs FG.
    # Let's extract drives. In nfl_data_py, we have drive (drive number) and game_id.
    # Let's find for each drive (game_id, drive) the minimum yardline_100.
    drive_min_yd = df.groupby(['game_id', 'drive', 'posteam', 'season'])['yardline_100'].min().reset_index()
    
    # Find the result of each drive. Let's find the last play of each drive to see the drive_result.
    # In nfl_data_py, we have 'fixed_drive_result' or 'drive_end_transition'. Let's look at drive_result.
    # Actually, we can get it from the last play's touchdown or field goal or turnover.
    # Let's merge the drive min yardline with the drive results.
    # Let's find the drive result by looking at all plays in the drive:
    # Did it have a touchdown (pass or rush TD)? Did it have a made FG?
    # We can check if any play in the drive has touchdown == 1 (and it's a pass/rush play, i.e., offensive TD).
    # Or field_goal_result == 'made'.
    # Let's do this at play level:
    drive_events = df.groupby(['game_id', 'drive']).agg({
        'touchdown': 'max',
        'field_goal_result': lambda x: (x == 'made').any(),
        'interception': 'max',
        'fumble_lost': 'max'
    }).reset_index()
    
    drive_summary = drive_min_yd.merge(drive_events, on=['game_id', 'drive'], how='left').fillna(0)
    
    # Calculate RZ and 5Z drives
    rz_drives = drive_summary[drive_summary['yardline_100'] <= 20]
    z5_drives = drive_summary[drive_summary['yardline_100'] <= 5]
    
    def get_efficiency(df_drives):
        total = len(df_drives)
        if total == 0: return {}
        tds = df_drives['touchdown'].sum()
        fgs = df_drives['field_goal_result'].sum()
        turnovers = (df_drives['interception'] == 1).sum() + (df_drives['fumble_lost'] == 1).sum()
        return {
            'total_drives': int(total),
            'td_rate': float(tds / total),
            'fg_rate': float(fgs / total),
            'turnover_rate': float(turnovers / total)
        }
        
    rz_eff_overall = get_efficiency(rz_drives)
    z5_eff_overall = get_efficiency(z5_drives)
    
    print(f"Overall Red Zone Efficiency: TD={rz_eff_overall.get('td_rate',0)*100:.1f}%, FG={rz_eff_overall.get('fg_rate',0)*100:.1f}%, TO={rz_eff_overall.get('turnover_rate',0)*100:.1f}%")
    print(f"Overall 5-Zone Efficiency: TD={z5_eff_overall.get('td_rate',0)*100:.1f}%, FG={z5_eff_overall.get('fg_rate',0)*100:.1f}%, TO={z5_eff_overall.get('turnover_rate',0)*100:.1f}%")
    
    # --------------------------------------------------
    # 3. Fourth Down Go-For-It Rates by Yard Line Zone
    # --------------------------------------------------
    # For 4th downs, how often do teams go for it?
    # A play is a 4th down if down == 4.
    # A team "goes for it" if play_type in ['pass', 'run'] (excluding punts, field goals, spikes, kneels, penalties).
    # In nfl_data_py, we can filter to down == 4, and look at play_type.
    # play_type can be 'field_goal', 'punt', 'pass', 'run', 'no_play' (penalties).
    df_fd = df[(df['down'] == 4) & (df['play_type'].isin(['field_goal', 'punt', 'pass', 'run']))].copy()
    df_fd['go'] = df_fd['play_type'].isin(['pass', 'run']).astype(int)
    
    # 3 Zones:
    # Zone 1: RZ (yardline_100 <= 20)
    # Zone 2: Opponent 10 (yardline_100 <= 10)
    # Zone 3: Opponent 5 (yardline_100 <= 5)
    fd_rz = df_fd[df_fd['yardline_100'] <= 20]
    fd_10 = df_fd[df_fd['yardline_100'] <= 10]
    fd_5 = df_fd[df_fd['yardline_100'] <= 5]
    
    print(f"4th Down Go Rate in Red Zone (<=20): {fd_rz['go'].mean()*100:.2f}% (N={len(fd_rz)})")
    print(f"4th Down Go Rate in Opp 10 (<=10): {fd_10['go'].mean()*100:.2f}% (N={len(fd_10)})")
    print(f"4th Down Go Rate in Opp 5 (<=5): {fd_5['go'].mean()*100:.2f}% (N={len(fd_5)})")
    
    # --------------------------------------------------
    # 4. Number of 40+ Yard Plays by Team-Season
    # --------------------------------------------------
    # A play is 40+ yards if yards_gained >= 40 (excluding punts/kickoffs/penalties).
    # Let's filter to offensive plays: play_type in ['pass', 'run']
    df_off = df[df['play_type'].isin(['pass', 'run'])]
    big_plays = df_off[df_off['yards_gained'] >= 40]
    
    # Calculate average 40+ yard plays per team-season
    big_plays_count = big_plays.groupby(['season', 'posteam'])['yards_gained'].count().reset_index()
    avg_big_plays_per_team_season = big_plays_count['yards_gained'].mean()
    print(f"Avg 40+ Yard Plays per Team-Season: {avg_big_plays_per_team_season:.1f}")
    
    # Let's break down by pass vs run
    big_pass_plays = big_plays[big_plays['play_type'] == 'pass'].groupby(['season', 'posteam'])['yards_gained'].count().reset_index()
    big_run_plays = big_plays[big_plays['play_type'] == 'run'].groupby(['season', 'posteam'])['yards_gained'].count().reset_index()
    print(f"Avg 40+ Yard Pass Plays per Team-Season: {big_pass_plays['yards_gained'].mean():.1f}")
    print(f"Avg 40+ Yard Run Plays per Team-Season: {big_run_plays['yards_gained'].mean():.1f}")
    
    # --------------------------------------------------
    # Save the output to JSON/CSV for reporting
    # --------------------------------------------------
    results = {
        'sacks_per_game': float(overall_sacks_per_game),
        'pass_yards_per_game': float(overall_pass_yds_per_game),
        'pass_tds_per_game': float(overall_pass_tds_per_game),
        'offensive_snaps_per_game': float(overall_offensive_snaps_per_game),
        'special_teams_snaps_per_game': float(overall_special_teams_snaps_per_game),
        'presnap_penalty_snaps_per_game': float(overall_presnap_penalty_snaps_per_game),
        'penalties_accepted_per_game': float(overall_penalties_accepted_per_game),
        'total_snaps_per_game': float(overall_total_snaps_per_game),
        'rz_efficiency': rz_eff_overall,
        'z5_efficiency': z5_eff_overall,
        'go_rate_rz': float(fd_rz['go'].mean()),
        'go_rate_10': float(fd_10['go'].mean()),
        'go_rate_5': float(fd_5['go'].mean()),
        'big_plays_per_team_season': float(avg_big_plays_per_team_season),
        'big_pass_plays_per_team_season': float(big_pass_plays['yards_gained'].mean()),
        'big_run_plays_per_team_season': float(big_run_plays['yards_gained'].mean())
    }
    
    os.makedirs('docs/audit/v_0_2_0_audit', exist_ok=True)
    with open('docs/audit/v_0_2_0_audit/historical_eda_metrics.json', 'w') as f:
        json.dump(results, f, indent=4)
        
    print("\nSaved historical EDA metrics to docs/audit/v_0_2_0_audit/historical_eda_metrics.json")

if __name__ == '__main__':
    run_historical_eda()
