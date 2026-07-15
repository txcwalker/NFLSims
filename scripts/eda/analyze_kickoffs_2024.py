import os
import sys
import pandas as pd
import numpy as np
import nfl_data_py as nfl

def analyze_2024_kickoffs():
    print("==================================================")
    # Load 2024 play-by-play data (the first season with the dynamic kickoff rule)
    print("Loading 2024 NFL Play-by-Play Data...")
    pbp = nfl.import_pbp_data([2024])
    
    # Filter to regular season
    if 'season_type' in pbp.columns:
        pbp_reg = pbp[pbp['season_type'] == 'REG'].copy()
    else:
        pbp_reg = pbp[pbp['game_type'] == 'REG'].copy()
        
    print(f"Loaded {len(pbp_reg)} regular season plays.")
    
    # Find all kickoff plays
    kickoffs = pbp_reg[pbp_reg['play_type'] == 'kickoff'].copy()
    print(f"Total kickoffs: {len(kickoffs)}")
    
    # Analyze touchback rate
    # nflfastR has 'touchback' column (1 if touchback, 0 otherwise)
    touchback_rate = kickoffs['touchback'].mean()
    print(f"Touchback rate: {touchback_rate*100:.2f}%")
    
    # Find the starting yardline of the subsequent drive for each kickoff
    # We can do this by matching the kickoff to the first play of the next drive, or
    # looking at drive-level data.
    # In nflfastR, each kickoff is part of a drive, or precedes a drive.
    # Let's group by game_id and drive, and find the first scrimmage play of the drive that follows a kickoff.
    # Actually, the kickoff itself is often coded as part of the drive or has the drive number.
    # Let's inspect the first scrimmage play (run, pass, sack, punt, fg) of each drive that started with a kickoff.
    
    # Let's find game_id and drive numbers for kickoffs
    ko_drives = kickoffs[['game_id', 'drive']].drop_duplicates()
    
    # Find scrimmage plays in those same drives
    scrimmage = pbp_reg[pbp_reg['play_type'].isin(['pass', 'run', 'punt', 'field_goal', 'no_play'])]
    
    # Merge to get plays in kickoff drives
    ko_drive_plays = pd.merge(scrimmage, ko_drives, on=['game_id', 'drive'], how='inner')
    
    # Get the first play of each of these drives
    first_plays = ko_drive_plays.groupby(['game_id', 'drive']).first().reset_index()
    
    # We want to know if the kickoff was a touchback or returned.
    ko_touchback_info = kickoffs.groupby(['game_id', 'drive'])['touchback'].max().reset_index()
    
    # Drop touchback if already in first_plays to avoid suffix collision during merge
    if 'touchback' in first_plays.columns:
        first_plays = first_plays.drop(columns=['touchback'])
    first_plays = pd.merge(first_plays, ko_touchback_info, on=['game_id', 'drive'], how='inner')
    
    # Calculate starting field position (yardline_100 is yards to opponent's endzone)
    # So 75 yardline_100 means starting at the 25-yard line.
    # 70 yardline_100 means starting at the 30-yard line.
    avg_start_overall = first_plays['yardline_100'].mean()
    avg_start_touchback = first_plays[first_plays['touchback'] == 1]['yardline_100'].mean()
    avg_start_return = first_plays[first_plays['touchback'] == 0]['yardline_100'].mean()
    
    print("\nStarting Field Position (Yards to Endzone):")
    print(f"Overall Avg: {avg_start_overall:.2f} yards to go (approx. {100-avg_start_overall:.2f} yardline)")
    print(f"Touchback Avg: {avg_start_touchback:.2f} yards to go (approx. {100-avg_start_touchback:.2f} yardline)")
    print(f"Return Avg: {avg_start_return:.2f} yards to go (approx. {100-avg_start_return:.2f} yardline)")
    
    # Save the results to markdown
    report_dir = "docs/eda_outputs/score_deficiency"
    os.makedirs(report_dir, exist_ok=True)
    report_path = os.path.join(report_dir, "kickoff_eda_2024.md")
    
    content = f"""# 🏈 2024 NFL Kickoff Rules & Field Position EDA

This report analyzes kickoffs during the **2024 NFL regular season** (the first season of the new dynamic kickoff rules) to determine touchback rates and the resulting starting field positions.

---

## 📈 1. Key Kickoff Metrics (2024)

| Metric | Value | Meaning / Context |
| :--- | :---: | :--- |
| **Total Kickoffs Analyzed** | **{len(kickoffs)}** | All regular season kickoffs |
| **Touchback Rate** | **{touchback_rate*100:.1f}%** | Percentage of kickoffs resulting in a touchback |
| **Return Rate** | **{(1-touchback_rate)*100:.1f}%** | Percentage of kickoffs that were returned |
| **Avg Starting Field Position (Overall)** | **{avg_start_overall:.1f} yds** to go | Average drive start at the **{100-avg_start_overall:.1f}-yard line** |
| **Avg Start on Touchbacks** | **{avg_start_touchback:.1f} yds** to go | Average drive start at the **{100-avg_start_touchback:.1f}-yard line** |
| **Avg Start on Returns** | **{avg_start_return:.1f} yds** to go | Average drive start at the **{100-avg_start_return:.1f}-yard line** |

---

## 🔍 2. Analysis of the 2024 Dynamic Kickoff Rule
Under the 2024/2025 kickoff rules:
1. **Touchback to the 30-Yard Line**: A touchback on a kickoff that lands in the end zone or is kicked out of the end zone in the air spots the ball at the **30-yard line** (70 yards to go).
2. **Touchback to the 20-Yard Line**: A touchback where the ball lands in the landing zone, rolls into the end zone, and is downed spots the ball at the **20-yard line** (80 yards to go).
3. **Returns**: Returned kickoffs in 2024 averaged a starting position at the **{100-avg_start_return:.1f}-yard line**, as coverage teams were set up further downfield.

### 🛠️ Proposed Simulator Calibration
To match the **2024/2025 NFL kickoff environment**:
* Instead of hardcoding all kickoffs to **75.0 yards** (25-yard line), we should roll a distribution for kickoff starting field position:
  * **{touchback_rate*100:.1f}%** chance of a Touchback: Spot the ball at the **30-yard line** (70 yards to go) or **20-yard line** (80 yards to go), averaging **{avg_start_touchback:.1f} yards** to go.
  * **{(1-touchback_rate)*100:.1f}%** chance of a Return: Spot the ball with a distribution centered around **{avg_start_return:.1f} yards** to go (e.g. `Normal(28.5, 5.0)`).
"""
    with open(report_path, 'w', encoding='utf-8') as f:
        f.write(content)
        
    print(f"\nEDA report written to: {report_path}")

if __name__ == '__main__':
    analyze_2024_kickoffs()
