import os
import sys
import numpy as np
import pandas as pd
import nfl_data_py as nfl
import json

# Adjust path to import from project root
sys.path.append(os.getcwd())

from src.nfl_sim.game_engine import NFLGameEngine

def analyze_historical_3rd_downs():
    print("==================================================")
    print("Loading 10 Years of NFL 3rd Downs (2016-2025)")
    print("==================================================")
    
    seasons = list(range(2016, 2026))
    pbp = nfl.import_pbp_data(seasons)
    
    # Filter to regular season
    if 'season_type' in pbp.columns:
        pbp_reg = pbp[pbp['season_type'] == 'REG'].copy()
    else:
        pbp_reg = pbp[pbp['game_type'] == 'REG'].copy()
        
    print(f"Loaded {len(pbp_reg)} regular season plays.")
    
    # Filter to 3rd downs on run/pass plays (excluding specials/penalties that nullify the play entirely)
    # We want to look at 3rd downs where a play actually occurred (pass, run, sack).
    pbp_3rd = pbp_reg[(pbp_reg['down'] == 3) & (pbp_reg['play_type'].isin(['pass', 'run']))].copy()
    pbp_3rd['is_pass'] = (pbp_3rd['play_type'] == 'pass').astype(int)
    pbp_3rd['is_run'] = (pbp_3rd['play_type'] == 'run').astype(int)
    
    # nflfastR columns:
    # 'first_down' (1 or 0)
    # 'yards_gained' (yards gained on the play)
    # 'sack' (1 or 0)
    # 'incomplete_pass' (1 or 0)
    # 'interception' (1 or 0)
    
    pbp_3rd['converted'] = (pbp_3rd['first_down'] == 1).astype(int)
    
    # Define distance buckets
    # Short: 1-3 yards
    # Med: 4-6 yards
    # Long: 7+ yards
    pbp_3rd['dist_bucket'] = 'long'
    pbp_3rd.loc[pbp_3rd['ydstogo'] <= 3, 'dist_bucket'] = 'short'
    pbp_3rd.loc[(pbp_3rd['ydstogo'] >= 4) & (pbp_3rd['ydstogo'] <= 6), 'dist_bucket'] = 'med'
    
    results = {}
    for bucket in ['short', 'med', 'long']:
        df_b = pbp_3rd[pbp_3rd['dist_bucket'] == bucket]
        total_plays = len(df_b)
        if total_plays == 0:
            continue
            
        pass_plays = df_b[df_b['is_pass'] == 1]
        run_plays = df_b[df_b['is_run'] == 1]
        
        pass_pct = len(pass_plays) / total_plays
        run_pct = len(run_plays) / total_plays
        
        avg_pass_yds = pass_plays['yards_gained'].mean()
        avg_run_yds = run_plays['yards_gained'].mean()
        
        # Sacks inside pass plays
        sack_rate = (pass_plays['sack'] == 1).mean() if len(pass_plays) > 0 else 0
        
        # Success rates
        overall_conv = df_b['converted'].mean()
        pass_conv = pass_plays['converted'].mean() if len(pass_plays) > 0 else 0
        run_conv = run_plays['converted'].mean() if len(run_plays) > 0 else 0
        
        results[bucket] = {
            'total_plays': int(total_plays),
            'pass_pct': float(pass_pct),
            'run_pct': float(run_pct),
            'avg_pass_yds': float(avg_pass_yds),
            'avg_run_yds': float(avg_run_yds),
            'sack_rate': float(sack_rate),
            'overall_conv': float(overall_conv),
            'pass_conv': float(pass_conv),
            'run_conv': float(run_conv)
        }
        
    return results

def analyze_simulation_3rd_downs():
    print("\n==================================================")
    print("Running Simulation Play-by-Play Diagnostics for 3rd Downs")
    print("==================================================")
    
    N = 1000
    engine = NFLGameEngine(
        away_team="DAL",
        home_team="PHI",
        year=2025,
        N=N
    )
    
    # Store play-level dictionaries of 3rd down plays in the sim
    # Each entry: {'bucket': 'short'/'med'/'long', 'is_pass': 0/1, 'is_run': 0/1, 'gain': yards, 'converted': 0/1, 'is_sack': 0/1}
    sim_plays = []
    
    last_poss_away = engine.possession_is_away.copy()
    
    while not np.all(engine.game_over):
        active = ~engine.game_over
        
        # 3rd down check BEFORE play step
        is_3rd = (engine.down == 3) & active & ~engine.needs_kickoff
        dists = engine.distance.copy()
        yd_before = engine.yardline_100.copy()
        poss_before = engine.possession_is_away.copy()
        
        score_away_before = engine.score_away.copy()
        score_home_before = engine.score_home.copy()
        
        # Player stats to determine run vs pass vs sack
        pAtt_before = np.zeros(N)
        rAtt_before = np.zeros(N)
        sacks_before = np.zeros(N)
        for team in [engine.away_team, engine.home_team]:
            for p, s in engine.player_stats[team].items():
                pAtt_before += s['pAtt']
                rAtt_before += s['rAtt']
                sacks_before += s['sacks_taken']
                
        # Execute play
        engine.simulate_play_step()
        
        pAtt_after = np.zeros(N)
        rAtt_after = np.zeros(N)
        sacks_after = np.zeros(N)
        for team in [engine.away_team, engine.home_team]:
            for p, s in engine.player_stats[team].items():
                pAtt_after += s['pAtt']
                rAtt_after += s['rAtt']
                sacks_after += s['sacks_taken']
                
        # Check active indices where it was 3rd down
        if np.any(is_3rd):
            for i in np.where(is_3rd)[0]:
                was_pass = (pAtt_after[i] > pAtt_before[i])
                was_run = (rAtt_after[i] > rAtt_before[i])
                was_sack = (sacks_after[i] > sacks_before[i])
                
                # Check conversions
                td_scored = (
                    (poss_before[i] and (engine.score_away[i] - score_away_before[i] == 7)) or
                    (not poss_before[i] and (engine.score_home[i] - score_home_before[i] == 7))
                )
                converted = (engine.down[i] == 1) or td_scored
                
                # Yardage gain
                # If possession didn't change, it's yd_before - yd_after
                # If possession did change, and no TD, the new yardline is from other side.
                # So we can look at the gain from the engine or estimate it
                if poss_before[i] == engine.possession_is_away[i]:
                    gain = yd_before[i] - engine.yardline_100[i]
                else:
                    # Possession flipped. If it's a punt/field goal we don't care, but this is pass/run.
                    # On turnovers (INT/Fumble), yards gained is still relevant.
                    # Since it is a bit complex, let's look at the gain by assuming the play ended at the turnover spot
                    # or just estimate as (yd_before[i] - (100 - engine.yardline_100[i])) which represents yardline change.
                    # Let's use 100 - engine.yardline_100[i] as new yardline from original perspective.
                    gain = yd_before[i] - (100 - engine.yardline_100[i])
                
                # Determine bucket
                dist_val = dists[i]
                if dist_val <= 3:
                    bucket = 'short'
                elif dist_val <= 6:
                    bucket = 'med'
                else:
                    bucket = 'long'
                    
                # Note: only track scrimmage pass/run plays (exclude punts/FGs that might occur on 3rd down, though rare)
                if was_pass or was_run or was_sack:
                    sim_plays.append({
                        'bucket': bucket,
                        'is_pass': 1 if (was_pass or was_sack) else 0,
                        'is_run': 1 if was_run else 0,
                        'is_sack': 1 if was_sack else 0,
                        'gain': float(gain),
                        'converted': 1 if converted else 0
                    })
                    
    # Process simulated plays
    df_sim = pd.DataFrame(sim_plays)
    results = {}
    if len(df_sim) > 0:
        for bucket in ['short', 'med', 'long']:
            df_b = df_sim[df_sim['bucket'] == bucket]
            total_plays = len(df_b)
            if total_plays == 0:
                continue
                
            pass_plays = df_b[df_b['is_pass'] == 1]
            run_plays = df_b[df_b['is_run'] == 1]
            
            pass_pct = len(pass_plays) / total_plays
            run_pct = len(run_plays) / total_plays
            
            avg_pass_yds = pass_plays['gain'].mean()
            avg_run_yds = run_plays['gain'].mean()
            
            sack_rate = pass_plays['is_sack'].mean() if len(pass_plays) > 0 else 0
            
            overall_conv = df_b['converted'].mean()
            pass_conv = pass_plays['converted'].mean() if len(pass_plays) > 0 else 0
            run_conv = run_plays['converted'].mean() if len(run_plays) > 0 else 0
            
            results[bucket] = {
                'total_plays': int(total_plays),
                'pass_pct': float(pass_pct),
                'run_pct': float(run_pct),
                'avg_pass_yds': float(avg_pass_yds),
                'avg_run_yds': float(avg_run_yds),
                'sack_rate': float(sack_rate),
                'overall_conv': float(overall_conv),
                'pass_conv': float(pass_conv),
                'run_conv': float(run_conv)
            }
            
    return results, df_sim

def generate_report(hist, sim, df_sim):
    os.makedirs("docs/eda_outputs/3rd_down_check", exist_ok=True)
    
    content = """# 📊 NFLSims 3rd Down Deep-Dive EDA Report

This report provides a detailed comparison of **play calling, average gains, and conversion success rates** on 3rd downs between **1,000 simulated games** and **10 years of NFL play-by-play data (2016-2025)**.

---

## 📈 1. Side-by-Side Comparison Matrix

"""
    for bucket in ['short', 'med', 'long']:
        h_data = hist.get(bucket, {})
        s_data = sim.get(bucket, {})
        
        b_name = bucket.capitalize()
        if bucket == 'short':
            b_desc = "Short (1–3 yards to go)"
        elif bucket == 'med':
            b_desc = "Medium (4–6 yards to go)"
        else:
            b_desc = "Long (7+ yards to go)"
            
        content += f"### 🏈 3rd & {b_desc}\n\n"
        content += "| Metric | NFL 10-Yr Avg | Simulation Avg | Delta / Variance |\n"
        content += "| :--- | :---: | :---: | :---: |\n"
        
        # Overall Conv
        h_conv = h_data.get('overall_conv', 0)
        s_conv = s_data.get('overall_conv', 0)
        content += f"| **Overall Conversion Rate** | **{h_conv*100:.1f}%** | **{s_conv*100:.1f}%** | {s_conv*100 - h_conv*100:+.1f}% |\n"
        
        # Play Selection
        h_pass = h_data.get('pass_pct', 0)
        s_pass = s_data.get('pass_pct', 0)
        content += f"| Pass Play Selection % | {h_pass*100:.1f}% | {s_pass*100:.1f}% | {s_pass*100 - h_pass*100:+.1f}% |\n"
        
        h_run = h_data.get('run_pct', 0)
        s_run = s_data.get('run_pct', 0)
        content += f"| Run Play Selection % | {h_run*100:.1f}% | {s_run*100:.1f}% | {s_run*100 - h_run*100:+.1f}% |\n"
        
        # Pass/Run Success
        h_pconv = h_data.get('pass_conv', 0)
        s_pconv = s_data.get('pass_conv', 0)
        content += f"| Pass Conversion Rate | {h_pconv*100:.1f}% | {s_pconv*100:.1f}% | {s_pconv*100 - h_pconv*100:+.1f}% |\n"
        
        h_rconv = h_data.get('run_conv', 0)
        s_rconv = s_data.get('run_conv', 0)
        content += f"| Run Conversion Rate | {h_rconv*100:.1f}% | {s_rconv*100:.1f}% | {s_rconv*100 - h_rconv*100:+.1f}% |\n"
        
        # Average Gains
        h_py = h_data.get('avg_pass_yds', 0)
        s_py = s_data.get('avg_pass_yds', 0)
        content += f"| Avg Yards Gained (Pass) | {h_py:.2f} yds | {s_py:.2f} yds | {s_py - h_py:+.2f} yds |\n"
        
        h_ry = h_data.get('avg_run_yds', 0)
        s_ry = s_data.get('avg_run_yds', 0)
        content += f"| Avg Yards Gained (Run) | {h_ry:.2f} yds | {s_ry:.2f} yds | {s_ry - h_ry:+.2f} yds |\n"
        
        h_sack = h_data.get('sack_rate', 0)
        s_sack = s_data.get('sack_rate', 0)
        content += f"| Sack Rate per Pass | {h_sack*100:.1f}% | {s_sack*100:.1f}% | {s_sack*100 - h_sack*100:+.1f}% |\n\n"
        
    # Analysis & Insights
    content += """## 🔍 2. Deep-Dive Diagnostics & Root Cause Analysis

Based on the side-by-side matrices, here are the critical failure modes identified:

### ⚠️ The 3rd & Short (1-3 yards) Meltdown
1. **Pass/Run Split**: In actual NFL games, teams run the ball on 3rd & short about **45-50%** of the time because it is high-probability short-yardage. Look at how our simulation calls plays here compared to real life.
2. **Short Yardage Gains**: Real-life 3rd & short runs average over **3-4 yards**, which is enough to move the chains. In the simulation, check if the run game fails to pick up the 1-2 yards needed or if there's a truncation/scaling issue.
3. **Pass Success**: On 3rd & short, pass completions should be high (due to quick slants/flat routes). If simulation pass conversion is low, it suggests route-depth selection is either too deep or completion rates are artificially low in tight coverage.

### ⚙️ Engine Level Observations
* Note if there are any specific lines of code in the engine where the yards gained is computed or where the route depth is selected that might explain the disparity.
"""
    
    report_path = "docs/eda_outputs/3rd_down_check/3rd_down_diagnostics.md"
    with open(report_path, 'w', encoding='utf-8') as f:
        f.write(content)
    print(f"Report written to {report_path}")

if __name__ == '__main__':
    hist = analyze_historical_3rd_downs()
    sim, df_sim = analyze_simulation_3rd_downs()
    generate_report(hist, sim, df_sim)
