import sys
import os
import pandas as pd
import numpy as np
import json

# Add project root to system path
sys.path.append(os.getcwd())

from src.nfl_sim.batch import BatchSimulator, StatAggregator

def generate_matchup_boxscore(away, home, week, game_df, player_df, boxscore_dir, iterations):
    """Generates a detailed statistical boxscore for the given matchup and writes it to disk."""
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
    box_md = f"""# 📊 Week {week} Matchup Boxscore: {away} at {home}
**Monte Carlo Simulated Averages across {iterations} Iterations**

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

    # Sort and split Defense/DST stats
    defense_df = player_avg[player_avg['Slot'] == 'DST'].sort_values('dk_score', ascending=False)
    box_md += """
## 🛡️ Defense / Special Teams (DST)
| Team | Sacks | INTs | Fumbles Rec | Def TDs | Points Allowed | DK Fantasy |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: |
"""
    for _, row in defense_df.iterrows():
        box_md += f"| **{row['Team']} DST** | {row.get('def_sack', 0.0):.1f} | {row.get('def_int', 0.0):.1f} | {row.get('def_fumble_rec', 0.0):.1f} | {row.get('def_td', 0.0):.1f} | {row.get('pts_allowed', 0.0):.1f} | **{row['dk_score']:.2f}** |\n"

    # Write individual boxscore file, saving over the current files
    box_path = os.path.join(boxscore_dir, f"{away}_at_{home}.md")
    with open(box_path, 'w', encoding='utf-8') as f:
        f.write(box_md)
        
    return box_md

def run_first_4_weeks(iterations=1000):
    print(f"\n==========================================")
    print(f" Simulating NFL 2025 Weeks 1-4 ({iterations} iterations)")
    print(f"==========================================\n")
    
    # Load 2025 Schedule
    sched_df = pd.read_csv("data/external/schedule_2025.csv")
    
    # Extract games for weeks 1, 2, 3, 4
    target_games = sched_df[(sched_df["week"].isin([1, 2, 3, 4])) & (sched_df["game_type"] == "REG")]
    
    # Initialize tracking structures
    team_expected_wins = {}
    team_expected_losses = {}
    all_game_rows = []
    all_player_dfs = []
    
    all_teams = set(sched_df["away_team"]).union(set(sched_df["home_team"]))
    for team in all_teams:
        team_expected_wins[team] = 0.0
        team_expected_losses[team] = 0.0
        
    for week in [1, 2, 3, 4]:
        print(f"\n--- Simulating Week {week} ---")
        week_games = target_games[target_games["week"] == week]
        
        week_boxscores = []
        boxscore_dir = f"docs/boxscores/week_{week}"
        os.makedirs(boxscore_dir, exist_ok=True)
        
        for idx, row in week_games.iterrows():
            away = row["away_team"]
            home = row["home_team"]
            game_id = row["game_id"]
            
            print(f"Simulating Matchup: {away} @ {home}...")
            batch = BatchSimulator(away, home, year=2025)
            
            # Run Vectorized batch sims
            game_df, player_df = batch.run_batch(iterations=iterations, vectorized=True)
            
            # Compute game metrics
            avg_away = game_df["off_score"].mean()
            avg_home = game_df["def_score"].mean()
            avg_spread = game_df["spread"].mean()
            avg_total = game_df["total"].mean()
            avg_plays = game_df["total_plays"].mean()
            avg_big_plays = game_df["plays_over_20_yds"].mean()
            avg_punts = game_df["punts"].mean()
            
            away_win = (game_df["winner"] == away).mean()
            home_win = 1.0 - away_win
            
            # Accumulate expected wins
            team_expected_wins[away] += away_win
            team_expected_losses[away] += home_win
            team_expected_wins[home] += home_win
            team_expected_losses[home] += away_win
            
            projected_winner = away if away_win > home_win else home
            spread_indicator = f"+{avg_spread:.1f}" if avg_spread > 0 else f"{avg_spread:.1f}"
            
            all_game_rows.append({
                "Week": week,
                "Matchup": f"{away}@{home}",
                "Simulations": iterations,
                "Avg Away Score": round(avg_away, 1),
                "Avg Home Score": round(avg_home, 1),
                "Avg Spread": spread_indicator,
                "Avg Total": round(avg_total, 1),
                "Avg Plays": round(avg_plays, 1),
                "Plays > 20": round(avg_big_plays, 1),
                "Punts": round(avg_punts, 1),
                "Away Win %": f"{away_win*100:.1f}%",
                "Home Win %": f"{home_win*100:.1f}%",
                "Projected Winner": projected_winner
            })
            
            player_df["game_id"] = game_id
            all_player_dfs.append(player_df)
            
            # Generate and save individual boxscore
            game_box_md = generate_matchup_boxscore(away, home, week, game_df, player_df, boxscore_dir, iterations)
            week_boxscores.append(game_box_md)
            
        # Write consolidated weekly boxscore
        week_boxscore_path = os.path.join(boxscore_dir, f"week_{week}_boxscore.md")
        with open(week_boxscore_path, 'w', encoding='utf-8') as f:
            f.write(f"# 📊 NFL 2025 Week {week} Complete Boxscores\n\n" + "\n\n---\n\n".join(week_boxscores))
        print(f"Created consolidated week boxscore: {week_boxscore_path}")
        
    print(f"\nAll weeks simulated successfully.")
    
    # -------------------------------------------------------------
    # BUILD REPORTS IN docs/reports
    # -------------------------------------------------------------
    report_dir = "docs/reports"
    os.makedirs(report_dir, exist_ok=True)
    
    # 1. Save Game Summaries CSV
    game_summary_df = pd.DataFrame(all_game_rows)
    game_summary_df.to_csv(f"{report_dir}/game_summaries.csv", index=False)
    print(f"Saved game summaries CSV to {report_dir}/game_summaries.csv")
    
    # 2. Save Player Summary CSV (per-game stats)
    if all_player_dfs:
        combined_player_df = pd.concat(all_player_dfs)
        agg = StatAggregator()
        player_summary = agg.aggregate_player_stats(combined_player_df)
        player_summary.to_csv(f"{report_dir}/player_summary.csv", index=False)
        print(f"Saved player stats summary CSV to {report_dir}/player_summary.csv")
        
        # 3. Calculate CUMULATIVE TOTALS across the 4 weeks for leaders report
        # combined_player_df has columns: game_id, game_id, winner, dk_score, etc.
        # We group by game_id and Player to average across iterations first, then sum across games!
        player_game_avg = combined_player_df.groupby(['Player', 'Team', 'Slot', 'game_id']).mean(numeric_only=True).reset_index()
        
        # Sum the averages of each game to get the expected cumulative totals
        cumulative_player_stats = player_game_avg.groupby(['Player', 'Team', 'Slot']).agg({
            'pAtt': 'sum',
            'pCmp': 'sum',
            'pYds': 'sum',
            'pTD': 'sum',
            'int': 'sum',
            'rAtt': 'sum',
            'rYds': 'sum',
            'rTD': 'sum',
            'rec': 'sum',
            'recYds': 'sum',
            'recTD': 'sum',
            'dk_score': 'sum',
            'fd_score': 'sum'
        }).reset_index()
        
        qbs = cumulative_player_stats[cumulative_player_stats["Slot"].str.startswith("QB")].sort_values("dk_score", ascending=False).head(15)
        rbs = cumulative_player_stats[cumulative_player_stats["Slot"].str.startswith("RB")].sort_values("dk_score", ascending=False).head(15)
        wrs = cumulative_player_stats[cumulative_player_stats["Slot"].str.startswith("WR") | cumulative_player_stats["Slot"].str.startswith("TE")].sort_values("dk_score", ascending=False).head(20)
        
        leaders_md = f"# 🏆 Player Leaders (Expected Cumulative TOTALS after Week 4)\n"
        leaders_md += f"**Sum of expected outcomes across all Weeks 1-4 simulated games ({iterations} iterations)**\n\n"
        
        leaders_md += "### 🎯 Passing Leaders (Total Pass Yards, Touchdowns, and Fantasy Points)\n"
        leaders_md += "| Player | Team | Pass Att | Completions | Pass Yards | Pass TDs | INTs | Total DK | Total FD |\n"
        leaders_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
        for _, r in qbs.iterrows():
            leaders_md += f"| **{r['Player']}** | {r['Team']} | {r['pAtt']:.1f} | {r['pCmp']:.1f} | {r['pYds']:.1f} | {r['pTD']:.1f} | {r['int']:.1f} | **{r['dk_score']:.2f}** | **{r['fd_score']:.2f}** |\n"
            
        leaders_md += "\n### 🏃 Rushing Leaders (Total Rushing Yards, Touchdowns, and Fantasy Points)\n"
        leaders_md += "| Player | Team | Slot | Rush Att | Rush Yards | Rush TDs | Receptions | Rec Yards | Total DK | Total FD |\n"
        leaders_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
        for _, r in rbs.iterrows():
            leaders_md += f"| **{r['Player']}** | {r['Team']} | {r['Slot']} | {r['rAtt']:.1f} | {r['rYds']:.1f} | {r['rTD']:.1f} | {r['rec']:.1f} | {r['recYds']:.1f} | **{r['dk_score']:.2f}** | **{r['fd_score']:.2f}** |\n"
            
        leaders_md += "\n### 👐 Receiving Leaders (Total Receptions, Receiving Yards, Touchdowns, and Fantasy Points)\n"
        leaders_md += "| Player | Team | Slot | Receptions | Rec Yards | Rec TDs | Total DK | Total FD |\n"
        leaders_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
        for _, r in wrs.iterrows():
            leaders_md += f"| **{r['Player']}** | {r['Team']} | {r['Slot']} | {r['rec']:.1f} | {r['recYds']:.1f} | {r['recTD']:.1f} | **{r['dk_score']:.2f}** | **{r['fd_score']:.2f}** |\n"
            
        with open(f"{report_dir}/leaders.md", "w", encoding="utf-8") as f:
            f.write(leaders_md)
        print(f"Saved cumulative leaders markdown report to {report_dir}/leaders.md")
        
    # 4. Render Standings Markdown
    standings_df = pd.DataFrame([
        {
            "Team": team,
            "Expected Wins": team_expected_wins[team],
            "Expected Losses": team_expected_losses[team],
            "Win Rate": team_expected_wins[team] / (team_expected_wins[team] + team_expected_losses[team]) if (team_expected_wins[team] + team_expected_losses[team]) > 0 else 0.0
        }
        for team in all_teams
    ]).sort_values("Expected Wins", ascending=False)
    
    standings_md = f"# 🏆 Expected Standings after Week 4 (Simulated Games)\n\n"
    standings_md += "| Team | Expected Record | Win Rate |\n"
    standings_md += "| :--- | :---: | :---: |\n"
    for _, row in standings_df.iterrows():
        standings_md += f"| **{row['Team']}** | {row['Expected Wins']:.2f} - {row['Expected Losses']:.2f} | {row['Win Rate']*100:.1f}% |\n"
        
    with open(f"{report_dir}/standings.md", "w", encoding="utf-8") as f:
        f.write(standings_md)
    print(f"Saved standings markdown report to {report_dir}/standings.md")

if __name__ == "__main__":
    run_first_4_weeks(iterations=1000)
    print("\nSimulations and reports completed successfully!")
