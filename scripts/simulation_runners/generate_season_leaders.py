import os
import pandas as pd
import numpy as np

def generate_season_leaders():
    players_cache_path = "data/interim/sim_results_2025_players.parquet"
    if not os.path.exists(players_cache_path):
        print(f"Error: Cache file {players_cache_path} not found. Please run the full season simulation first.")
        return
        
    print("Loading player simulation cache...")
    df = pd.read_parquet(players_cache_path)
    
    print("Aggregating season cumulative stats per player per iteration...")
    metrics = ['pAtt', 'pCmp', 'pYds', 'pTD', 'int', 'rAtt', 'rYds', 'rTD', 'rec', 'recYds', 'recTD', 'targets', 'dk_score']
    
    # Group by player/team/pos/slot/iteration to sum across all games played in each iteration
    season_totals = df.groupby(['Player', 'Team', 'Pos', 'Slot', 'iteration'])[metrics].sum().reset_index()
    
    # Calculate Expected (Mean) and Median season-long totals
    print("Calculating expected and median stats...")
    expected_stats = season_totals.groupby(['Player', 'Team', 'Pos', 'Slot'])[metrics].mean().reset_index()
    median_stats = season_totals.groupby(['Player', 'Team', 'Pos', 'Slot'])[metrics].median().reset_index()
    
    # Add Total TDs metric (Rush TDs + Rec TDs)
    expected_stats['total_tds'] = expected_stats['rTD'] + expected_stats['recTD']
    median_stats['total_tds'] = median_stats['rTD'] + median_stats['recTD']
    
    # Rename columns for merging
    avg_cols = {col: f"{col}_avg" for col in metrics + ['total_tds']}
    med_cols = {col: f"{col}_med" for col in metrics + ['total_tds']}
    
    expected_stats = expected_stats.rename(columns=avg_cols)
    median_stats = median_stats.rename(columns=med_cols)
    
    # Combine stats
    leaders_df = pd.merge(expected_stats, median_stats, on=['Player', 'Team', 'Pos', 'Slot'])
    
    # Generate Markdown Report
    leaders_md = "# 🏆 NFL 2025 Simulated Season Cumulative Leaders\n"
    leaders_md += "**Aggregated season-long cumulative totals computed over 1,000 Monte Carlo iterations**\n"
    leaders_md += "*(Shows both Expected / Average and Median season outcomes)*\n\n"
    
    # 1. PASSING LEADERS (Yards)
    leaders_md += "## 🎯 Passing Yards Leaders\n"
    leaders_md += "| Rank | Player | Team | Pass Att (Avg) | Cmp (Avg) | Pass Yards (Avg) | Pass Yards (Med) | Pass TDs (Avg) | INTs (Avg) | DK Avg (Season) |\n"
    leaders_md += "| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
    pass_leaders = leaders_df[leaders_df['Slot'].str.startswith('QB')].sort_values('pYds_avg', ascending=False).head(15)
    for idx, (_, r) in enumerate(pass_leaders.iterrows()):
        leaders_md += f"| {idx+1} | **{r['Player']}** | {r['Team']} | {r['pAtt_avg']:.1f} | {r['pCmp_avg']:.1f} | **{r['pYds_avg']:.1f}** | {r['pYds_med']:.1f} | {r['pTD_avg']:.1f} | {r['int_avg']:.1f} | **{r['dk_score_avg']:.1f}** |\n"
    leaders_md += "\n"
    
    # 2. RUSHING LEADERS (Yards)
    leaders_md += "## 🏃 Rushing Yards Leaders\n"
    leaders_md += "| Rank | Player | Team | Slot | Rush Att (Avg) | Rush Yards (Avg) | Rush Yards (Med) | Rush TDs (Avg) | Rec (Avg) | DK Avg (Season) |\n"
    leaders_md += "| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
    rush_leaders = leaders_df[leaders_df['Slot'].str.startswith('RB')].sort_values('rYds_avg', ascending=False).head(15)
    for idx, (_, r) in enumerate(rush_leaders.iterrows()):
        leaders_md += f"| {idx+1} | **{r['Player']}** | {r['Team']} | {r['Slot']} | {r['rAtt_avg']:.1f} | **{r['rYds_avg']:.1f}** | {r['rYds_med']:.1f} | {r['rTD_avg']:.1f} | {r['rec_avg']:.1f} | **{r['dk_score_avg']:.1f}** |\n"
    leaders_md += "\n"
    
    # 3. RECEIVING LEADERS (Yards)
    leaders_md += "## 👐 Receiving Yards Leaders\n"
    leaders_md += "| Rank | Player | Team | Slot | Targets (Avg) | Rec (Avg) | Rec Yards (Avg) | Rec Yards (Med) | Rec TDs (Avg) | DK Avg (Season) |\n"
    leaders_md += "| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
    rec_leaders = leaders_df[leaders_df['Slot'].isin(['WR1', 'WR2', 'WR3', 'TE1', 'TE2', 'WR/TE'])].sort_values('recYds_avg', ascending=False).head(20)
    for idx, (_, r) in enumerate(rec_leaders.iterrows()):
        leaders_md += f"| {idx+1} | **{r['Player']}** | {r['Team']} | {r['Slot']} | {r['targets_avg']:.1f} | {r['rec_avg']:.1f} | **{r['recYds_avg']:.1f}** | {r['recYds_med']:.1f} | {r['recTD_avg']:.1f} | **{r['dk_score_avg']:.1f}** |\n"
    leaders_md += "\n"

    # 4. RECEPTION LEADERS
    leaders_md += "## 📈 Receptions Leaders\n"
    leaders_md += "| Rank | Player | Team | Slot | Targets (Avg) | Receptions (Avg) | Receptions (Med) | Rec Yards (Avg) | Rec TDs (Avg) | DK Avg (Season) |\n"
    leaders_md += "| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
    reception_leaders = leaders_df.sort_values('rec_avg', ascending=False).head(20)
    for idx, (_, r) in enumerate(reception_leaders.iterrows()):
        leaders_md += f"| {idx+1} | **{r['Player']}** | {r['Team']} | {r['Slot']} | {r['targets_avg']:.1f} | **{r['rec_avg']:.1f}** | {r['rec_med']:.1f} | {r['recYds_avg']:.1f} | {r['recTD_avg']:.1f} | **{r['dk_score_avg']:.1f}** |\n"
    leaders_md += "\n"

    # 5. TARGET LEADERS
    leaders_md += "## 🎯 Targets Leaders\n"
    leaders_md += "| Rank | Player | Team | Slot | Targets (Avg) | Targets (Med) | Receptions (Avg) | Rec Yards (Avg) | Rec TDs (Avg) | DK Avg (Season) |\n"
    leaders_md += "| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
    target_leaders = leaders_df.sort_values('targets_avg', ascending=False).head(20)
    for idx, (_, r) in enumerate(target_leaders.iterrows()):
        leaders_md += f"| {idx+1} | **{r['Player']}** | {r['Team']} | {r['Slot']} | **{r['targets_avg']:.1f}** | {r['targets_med']:.1f} | {r['rec_avg']:.1f} | {r['recYds_avg']:.1f} | {r['recTD_avg']:.1f} | **{r['dk_score_avg']:.1f}** |\n"
    leaders_md += "\n"

    # 6. TOUCHDOWN LEADERS
    leaders_md += "## 🏈 Touchdown Leaders (Scrimmage TDs)\n"
    leaders_md += "| Rank | Player | Team | Slot | Rush TDs (Avg) | Rec TDs (Avg) | Total TDs (Avg) | Total TDs (Med) | DK Avg (Season) |\n"
    leaders_md += "| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
    td_leaders = leaders_df.sort_values('total_tds_avg', ascending=False).head(15)
    for idx, (_, r) in enumerate(td_leaders.iterrows()):
        leaders_md += f"| {idx+1} | **{r['Player']}** | {r['Team']} | {r['Slot']} | {r['rTD_avg']:.1f} | {r['recTD_avg']:.1f} | **{r['total_tds_avg']:.1f}** | {r['total_tds_med']:.1f} | **{r['dk_score_avg']:.1f}** |\n"
    leaders_md += "\n"

    # Save to docs/reports/season_leaders.md
    output_path = "docs/reports/season_leaders.md"
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(leaders_md)
    print(f"Successfully generated season leaders report at: {output_path}")

if __name__ == "__main__":
    generate_season_leaders()
