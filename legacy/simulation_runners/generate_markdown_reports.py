import os
import shutil
import pandas as pd
import numpy as np

def generate_reports():
    print("=" * 60)
    print(" GENERATING MARKDOWN REPORTS FROM SIMULATION OUTPUTS")
    print("=" * 60)

    # 1. Ensure target directory exists
    reports_dir = os.path.join('docs', 'reports')
    if not os.path.exists(reports_dir):
        os.makedirs(reports_dir)
        print(f"Created directory: {reports_dir}")

    # 2. Convert game_summaries.csv to Markdown
    game_csv_path = os.path.join('reports', 'week_1_2025', 'game_summaries.csv')
    if os.path.exists(game_csv_path):
        print(f"Processing game summaries from: {game_csv_path}...")
        df_games = pd.read_csv(game_csv_path)
        
        # Group by matchup to present game-level metrics
        matchups = df_games['matchup'].unique()
        summary_rows = []
        
        for m in matchups:
            m_df = df_games[df_games['matchup'] == m]
            total_sims = len(m_df)
            
            # Split off@def into away@home
            teams = m.split('@')
            away = teams[0]
            home = teams[1]
            
            # Count wins
            away_wins = len(m_df[m_df['winner'] == away])
            home_wins = len(m_df[m_df['winner'] == home])
            
            away_pct = (away_wins / total_sims) * 100
            home_pct = (home_wins / total_sims) * 100
            
            avg_away_score = m_df['off_score'].mean()
            avg_home_score = m_df['def_score'].mean()
            avg_spread = m_df['spread'].mean()
            avg_total = m_df['total'].mean()
            
            # Additional requested metrics
            avg_plays = m_df['total_plays'].mean() if 'total_plays' in m_df.columns else 0.0
            avg_plays_over_20 = m_df['plays_over_20_yds'].mean() if 'plays_over_20_yds' in m_df.columns else 0.0
            
            summary_rows.append({
                'Matchup': m,
                'Simulations': total_sims,
                'Avg Away Score': f"{avg_away_score:.1f}",
                'Avg Home Score': f"{avg_home_score:.1f}",
                'Avg Spread': f"{avg_spread:+.1f}",
                'Avg Total': f"{avg_total:.1f}",
                'Avg Plays': f"{avg_plays:.1f}",
                'Plays > 20': f"{avg_plays_over_20:.1f}",
                'Away Win %': f"{away_pct:.1f}%",
                'Home Win %': f"{home_pct:.1f}%",
                'Projected Winner': home if home_pct > away_pct else away
            })
            
        summary_df = pd.DataFrame(summary_rows)
        
        # Create game summaries markdown content
        game_md_content = f"""# 🏈 NFL 2025 Week 1 Matchup Simulation Summaries
**Generated from Monte Carlo Simulation Engine V.0.1.1**

This report aggregates all simulated matchup records, scores, and snap metrics across all iterations.

## 📊 Matchup Overview & Projected Outlines

| Matchup | Simulations | Avg Away Score | Avg Home Score | Avg Spread | Avg Total | Avg Plays | Plays > 20 | Away Win % | Home Win % | Projected Winner |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
        for _, row in summary_df.iterrows():
            game_md_content += f"| **{row['Matchup']}** | {row['Simulations']} | {row['Avg Away Score']} | {row['Avg Home Score']} | `{row['Avg Spread']}` | {row['Avg Total']} | {row['Avg Plays']} | {row['Plays > 20']} | {row['Away Win %']} | {row['Home Win %']} | **{row['Projected Winner']}** |\n"
            
        game_md_path = os.path.join(reports_dir, 'week_1_2025_game_summaries.md')
        with open(game_md_path, 'w', encoding='utf-8') as f:
            f.write(game_md_content)
        print(f"Created game summaries markdown: {game_md_path}")
    else:
        print(f"Warning: Game summaries CSV not found at {game_csv_path}")

    # 3. Convert player_summaries.csv to Position-Based Markdown Reports
    player_csv_path = os.path.join('reports', 'week_1_2025', 'player_summaries.csv')
    if os.path.exists(player_csv_path):
        print(f"Processing player summaries from: {player_csv_path}...")
        df_players = pd.read_csv(player_csv_path)
        
        # Sort by fantasy value
        df_players = df_players.sort_values('dk_score_avg', ascending=False)
        
        # Filter and generate QBs
        qbs = df_players[df_players['Slot'] == 'QB'].head(15)
        qb_table = """### 🎯 Quarterbacks (Top 15)
| Player | Team | Pass Att | Completions | Pass Yards | Pass TDs | Rush Att | Rush Yards | Rush TDs | DK Avg | DK Ceiling (p95) |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
        for _, row in qbs.iterrows():
            p_att = row.get('pAtt_avg', 0.0)
            p_cmp = row.get('pCmp_avg', 0.0)
            r_att = row.get('rAtt_avg', 0.0)
            qb_table += f"| **{row['Player']}** | {row['Team']} | {p_att:.1f} | {p_cmp:.1f} | {row['pYds_avg']:.1f} | {row['pTD_avg']:.1f} | {r_att:.1f} | {row['rYds_avg']:.1f} | {row['rTD_avg']:.1f} | **{row['dk_score_avg']:.2f}** | {row['dk_score_p95']:.2f} |\n"

        # Filter and generate RBs
        rbs = df_players[df_players['Slot'].str.startswith('RB', na=False)].head(15)
        rb_table = """### 🏃 Running Backs (Top 15)
| Player | Team | Slot | Rush Att | Rush Yards | Rush TDs | Rec | Rec Yards | Rec TDs | DK Avg | DK Ceiling (p95) |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
        for _, row in rbs.iterrows():
            r_att = row.get('rAtt_avg', 0.0)
            rb_table += f"| **{row['Player']}** | {row['Team']} | {row['Slot']} | {r_att:.1f} | {row['rYds_avg']:.1f} | {row['rTD_avg']:.1f} | {row['rec_avg']:.1f} | {row['recYds_avg']:.1f} | {row['recTD_avg']:.1f} | **{row['dk_score_avg']:.2f}** | {row['dk_score_p95']:.2f} |\n"

        # Filter and generate WRs
        wrs = df_players[df_players['Slot'].str.startswith('WR', na=False)].head(25)
        wr_table = """### 👐 Wide Receivers (Top 25)
| Player | Team | Slot | Targets | Rec | Rec Yards | Rec TDs | DK Avg | DK Ceiling (p95) |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
        for _, row in wrs.iterrows():
            wr_table += f"| **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['targets_avg']:.1f} | {row['rec_avg']:.1f} | {row['recYds_avg']:.1f} | {row['recTD_avg']:.1f} | **{row['dk_score_avg']:.2f}** | {row['dk_score_p95']:.2f} |\n"

        # Filter and generate TEs
        tes = df_players[df_players['Slot'].str.startswith('TE', na=False)].head(15)
        te_table = """### 🏈 Tight Ends (Top 15)
| Player | Team | Slot | Targets | Rec | Rec Yards | Rec TDs | DK Avg | DK Ceiling (p95) |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
"""
        for _, row in tes.iterrows():
            te_table += f"| **{row['Player']}** | {row['Team']} | {row['Slot']} | {row['targets_avg']:.1f} | {row['rec_avg']:.1f} | {row['recYds_avg']:.1f} | {row['recTD_avg']:.1f} | **{row['dk_score_avg']:.2f}** | {row['dk_score_p95']:.2f} |\n"

        player_md_content = f"""# 📈 NFL 2025 Week 1 Player Fantasy Projections Summaries
**Generated from Monte Carlo Simulation Engine V.0.1.1**

This report summarizes position-by-position projected player performance averages and fantasy ceiling metrics.

{qb_table}

{rb_table}

{wr_table}

{te_table}
"""
        player_md_path = os.path.join(reports_dir, 'week_1_2025_player_summaries.md')
        with open(player_md_path, 'w', encoding='utf-8') as f:
            f.write(player_md_content)
        print(f"Created player summaries markdown: {player_md_path}")
    else:
        print(f"Warning: Player summaries CSV not found at {player_csv_path}")

    # 4. Move sim evaluation reports from docs/ to docs/reports/
    for ver in ['0_1_0', '0_1_1']:
        old_path = os.path.join('docs', f'sim_evaluation_report_v_{ver}.md')
        new_path = os.path.join(reports_dir, f'sim_evaluation_report_v_{ver}.md')
        if os.path.exists(old_path):
            try:
                shutil.move(old_path, new_path)
                print(f"Moved: {old_path} -> {new_path}")
            except Exception as e:
                print(f"Error moving {old_path}: {e}")
        else:
            # Maybe it is already in the target directory?
            if os.path.exists(new_path):
                print(f"Sim evaluation report V.{ver} is already in: {new_path}")
            else:
                print(f"Warning: Evaluation report not found at {old_path}")

    print("=" * 60)
    print(" ALL REPORTS SUCCESSFULY CONVERTED AND CONSOLIDATED!")
    print("=" * 60)

if __name__ == "__main__":
    generate_reports()
