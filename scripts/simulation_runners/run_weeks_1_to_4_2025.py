import sys
import os
import pandas as pd
import numpy as np

import json

# Add project root to system path
sys.path.append(os.getcwd())

from src.nfl_sim.batch import BatchSimulator, StatAggregator

def generate_matchup_boxscore(away, home, week, game_df, player_df, boxscore_dir, iterations):
    """Generates a detailed statistical boxscore for the given matchup."""
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

    # Bypassing writing individual boxscore files to avoid OneDrive sync latency.
    # The consolidated week boxscore will still contain all of these details.
    # box_path = os.path.join(boxscore_dir, f"{away}_at_{home}.md")
    # with open(box_path, 'w', encoding='utf-8') as f:
    #     f.write(box_md)
        
    return box_md

def run_all_simulations(iterations=1000):
    print(f"\n==========================================")
    print(f" Simulating Weeks 1-4 of 2025 Season ({iterations} iterations)")
    print(f"==========================================\n")
    
    # Load 2025 Schedule
    sched_df = pd.read_csv("data/external/schedule_2025.csv")
    
    # Initialize tracking structures
    team_expected_wins = {}
    team_expected_losses = {}
    all_player_summaries = []
    all_game_rows = []
    all_snap_metrics = []
    
    # Initialize teams
    all_teams = set(sched_df["away_team"]).union(set(sched_df["home_team"]))
    for team in all_teams:
        team_expected_wins[team] = 0.0
        team_expected_losses[team] = 0.0
        
    for week in [1, 2, 3, 4]:
        print(f"\n--- Simulating Week {week} ---")
        week_games = sched_df[(sched_df["week"] == week) & (sched_df["game_type"] == "REG")]
        
        if week_games.empty:
            print(f"No games found in week {week}.")
            continue
            
        all_player_dfs = []
        week_boxscores = []
        week_boxscore_dir = f"docs/boxscores/week_{week}"
        os.makedirs(week_boxscore_dir, exist_ok=True)
        
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

            # clock_physics_v020 audit: snap-count/penalty tracking (per-game averages)
            all_snap_metrics.append({
                "total_snaps": game_df["total_snaps"].mean(),
                "offensive_snaps": game_df["offensive_snaps"].mean(),
                "special_teams_snaps": game_df["special_teams_snaps"].mean(),
                "presnap_penalty_snaps": game_df["presnap_penalty_snaps"].mean(),
                "penalties_accepted": game_df["penalties_accepted"].mean(),
            })

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
                "Matchup": f"**{away}@{home}**",
                "Simulations": iterations,
                "Avg Away Score": round(avg_away, 1),
                "Avg Home Score": round(avg_home, 1),
                "Avg Spread": f"`{spread_indicator}`",
                "Avg Total": round(avg_total, 1),
                "Avg Plays": round(avg_plays, 1),
                "Plays > 20": round(avg_big_plays, 1),
                "Punts": round(avg_punts, 1),
                "Away Win %": f"{away_win*100:.1f}%",
                "Home Win %": f"{home_win*100:.1f}%",
                "Projected Winner": f"**{projected_winner}**"
            })
            
            player_df["game_id"] = game_id
            all_player_dfs.append(player_df)
            
            # Generate and cache individual matchup boxscores
            game_box_md = generate_matchup_boxscore(away, home, week, game_df, player_df, week_boxscore_dir, iterations)
            week_boxscores.append(game_box_md)
            
        # Write consolidated weekly boxscore
        week_boxscore_path = os.path.join(week_boxscore_dir, f"week_{week}_boxscore.md")
        with open(week_boxscore_path, 'w', encoding='utf-8') as f:
            f.write(f"# 📊 NFL 2025 Week {week} Complete Boxscores\n\n" + "\n\n---\n\n".join(week_boxscores))
        print(f"Created consolidated week boxscore: {week_boxscore_path}")
        
        # Combine and aggregate all players for the week
        if all_player_dfs:
            combined_player_df = pd.concat(all_player_dfs)
            agg = StatAggregator()
            player_summary = agg.aggregate_player_stats(combined_player_df)
            all_player_summaries.append(player_summary)
            
    # -------------------------------------------------------------
    # RENDER GAME SUMMARIES MD
    # -------------------------------------------------------------
    audit_dir = "docs/audit/v_0_2_0_audit"
    os.makedirs(audit_dir, exist_ok=True)
    game_md_path = f"{audit_dir}/game_summaries_weeks_1_4.md"
    
    with open(game_md_path, "w", encoding="utf-8") as f:
        f.write(f"# 🏈 Weeks 1-4 Game Matchup Simulation Summaries\n")
        f.write(f"**Monte Carlo Simulated Averages across {iterations} Iterations**\n\n")
        
        f.write("| Week | Matchup | Simulations | Avg Away Score | Avg Home Score | Avg Spread | Avg Total | Avg Plays | Plays > 20 | Punts | Away Win % | Home Win % | Projected Winner |\n")
        f.write("| :--- | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n")
        
        for g in all_game_rows:
            f.write(f"| Week {g['Week']} | {g['Matchup']} | {g['Simulations']} | {g['Avg Away Score']:.1f} | {g['Avg Home Score']:.1f} | {g['Avg Spread']} | {g['Avg Total']:.1f} | {g['Avg Plays']:.1f} | {g['Plays > 20']:.1f} | {g['Punts']:.1f} | {g['Away Win %']} | {g['Home Win %']} | {g['Projected Winner']} |\n")
            
    print(f"Saved Game Summaries to: {game_md_path}")
    
    # -------------------------------------------------------------
    # RENDER TEAM STANDINGS MD
    # -------------------------------------------------------------
    standings_df = pd.DataFrame([
        {
            "Team": team,
            "Expected Wins": team_expected_wins[team],
            "Expected Losses": team_expected_losses[team],
            "Win Rate": team_expected_wins[team] / (team_expected_wins[team] + team_expected_losses[team])
        }
        for team in all_teams
    ]).sort_values("Expected Wins", ascending=False)
    
    standings_md_path = f"{audit_dir}/standings_4_weeks.md"
    with open(standings_md_path, "w", encoding="utf-8") as f:
        f.write(f"# 🏆 NFL Standings after Week 4 (Expected Record)\n")
        f.write(f"**Calculated from expected win rates across {iterations} Monte Carlo simulations**\n\n")
        
        f.write("| Rank | Team | Expected Record | Expected Wins | Expected Losses | Win Rate |\n")
        f.write("| :---: | :--- | :---: | :---: | :---: | :---: |\n")
        for idx, r in enumerate(standings_df.iterrows()):
            _, row = r
            f.write(f"| {idx+1} | **{row['Team']}** | {row['Expected Wins']:.2f} - {row['Expected Losses']:.2f} | {row['Expected Wins']:.2f} | {row['Expected Losses']:.2f} | {row['Win Rate']*100:.1f}% |\n")
            
    print(f"Saved Standings to: {standings_md_path}")
    
    # -------------------------------------------------------------
    # RENDER PLAYER LEADERS MD
    # -------------------------------------------------------------
    combined_player_summary = pd.concat(all_player_summaries)
    
    # Sum the averages across the 4 weeks
    cumulative_player_stats = combined_player_summary.groupby(['Player', 'Team', 'Slot']).agg({
        'pAtt_avg': 'sum',
        'pCmp_avg': 'sum',
        'pYds_avg': 'sum',
        'pTD_avg': 'sum',
        'int_avg': 'sum',
        'sacks_taken_avg': 'sum',
        'rAtt_avg': 'sum',
        'rYds_avg': 'sum',
        'rTD_avg': 'sum',
        'targets_avg': 'sum',
        'rec_avg': 'sum',
        'recYds_avg': 'sum',
        'recTD_avg': 'sum',
        'dk_score_avg': 'sum'
    }).reset_index()
    
    leaders_md_path = f"{audit_dir}/leaders_4_weeks.md"
    
    qbs = cumulative_player_stats[cumulative_player_stats["Slot"].str.startswith("QB")].sort_values("pYds_avg", ascending=False).head(15)
    rbs = cumulative_player_stats[cumulative_player_stats["Slot"].str.startswith("RB")].sort_values("rYds_avg", ascending=False).head(15)
    wrs = cumulative_player_stats[cumulative_player_stats["Slot"].str.startswith("WR") | cumulative_player_stats["Slot"].str.startswith("TE")].sort_values("recYds_avg", ascending=False).head(20)
    
    with open(leaders_md_path, "w", encoding="utf-8") as f:
        f.write(f"# 📈 Player Statistics Leaders (Expected Cumulative after Week 4)\n")
        f.write(f"**Aggregated weekly expected values over {iterations} simulations**\n\n")
        
        # Quarterbacks
        f.write("### 🎯 Passing Leaders (Top 15 QBs)\n")
        f.write("| Player | Team | Pass Att | Completions | Comp % | Pass Yards | Pass TDs | INTs | Sacks Taken | DK Avg |\n")
        f.write("| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n")
        for _, r in qbs.iterrows():
            comp_pct = (r['pCmp_avg'] / r['pAtt_avg'] * 100) if r['pAtt_avg'] > 0 else 0.0
            f.write(f"| **{r['Player']}** | {r['Team']} | {r['pAtt_avg']:.1f} | {r['pCmp_avg']:.1f} | {comp_pct:.1f}% | {r['pYds_avg']:.1f} | {r['pTD_avg']:.1f} | {r['int_avg']:.1f} | {r['sacks_taken_avg']:.1f} | **{r['dk_score_avg']/4:.2f}** |\n")
            
        # Running Backs
        f.write("\n\n### 🏃 Rushing Leaders (Top 15 RBs)\n")
        f.write("| Player | Team | Slot | Rush Att | Rush Yards | Rush TDs | Rec | Rec Yards | Rec TDs | DK Avg |\n")
        f.write("| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n")
        for _, r in rbs.iterrows():
            f.write(f"| **{r['Player']}** | {r['Team']} | {r['Slot']} | {r['rAtt_avg']:.1f} | {r['rYds_avg']:.1f} | {r['rTD_avg']:.1f} | {r['rec_avg']:.1f} | {r['recYds_avg']:.1f} | {r['recTD_avg']:.1f} | **{r['dk_score_avg']/4:.2f}** |\n")
            
        # Wide Receivers & Tight Ends
        f.write("\n\n### 👐 Receiving Leaders (Top 20 WRs/TEs)\n")
        f.write("| Player | Team | Slot | Targets | Rec | Receptions | Rec Yards | Rec TDs | DK Avg |\n")
        f.write("| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n")
        for _, r in wrs.iterrows():
            f.write(f"| **{r['Player']}** | {r['Team']} | {r['Slot']} | {r['targets_avg']:.1f} | {r['rec_avg']:.1f} | {r['rec_avg']:.1f} | {r['recYds_avg']:.1f} | {r['recTD_avg']:.1f} | **{r['dk_score_avg']/4:.2f}** |\n")
            
    print(f"Saved Leaders to: {leaders_md_path}")
    
    # -------------------------------------------------------------
    # BUILD AUDIT COMPARATIVE ANALYSIS REPORT
    # -------------------------------------------------------------
    # Calculate simulated averages for the audit report
    # Group by week/game and compute average per team per game
    snap_metrics_df = pd.DataFrame(all_snap_metrics)
    sim_stats = {
        'sacks_per_game': float(cumulative_player_stats['sacks_taken_avg'].sum() / (32 * 4)), # 32 teams, 4 weeks
        'pass_yards_per_game': float(cumulative_player_stats['pYds_avg'].sum() / (32 * 4)),
        'pass_tds_per_game': float(cumulative_player_stats['pTD_avg'].sum() / (32 * 4)),
        # clock_physics_v020 Round 7: population-wide completion rate (all real starters, weeks 1-4)
        'completion_rate_pct': float(cumulative_player_stats['pCmp_avg'].sum() / cumulative_player_stats['pAtt_avg'].sum() * 100),
        # clock_physics_v020 audit: whole-game (both teams combined) snap/penalty averages
        'total_snaps_per_game': float(snap_metrics_df['total_snaps'].mean()),
        'offensive_snaps_per_game': float(snap_metrics_df['offensive_snaps'].mean()),
        'special_teams_snaps_per_game': float(snap_metrics_df['special_teams_snaps'].mean()),
        'presnap_penalty_snaps_per_game': float(snap_metrics_df['presnap_penalty_snaps'].mean()),
        'penalties_accepted_per_game': float(snap_metrics_df['penalties_accepted'].mean()),
    }
    
    # Write historical comparison JSON
    with open(f"{audit_dir}/sim_post_cal_metrics.json", "w") as f:
        json.dump(sim_stats, f, indent=4)
        
    print("Saved post-calibration simulation summary metrics.")

if __name__ == "__main__":
    run_all_simulations(iterations=1000)
    print("\nSimulations completed successfully!")
