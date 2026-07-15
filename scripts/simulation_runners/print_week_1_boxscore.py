"""
Writes docs/boxscores/week_1/week_1_boxscore.md from the full-season parquet
cache (data/interim/sim_results_2025_{games,players}.parquet, regenerated
Round 16, clock_physics_v020) — no re-simulation needed, just reads the
already-computed N=1000 results for Week 1's 16 games. Reuses the exact
boxscore-formatting function from run_weeks_1_to_18_2025.py.
"""
import sys
import os
sys.path.append(os.getcwd())
sys.path.append(os.path.join(os.getcwd(), "scripts", "simulation_runners"))

import pandas as pd
from run_weeks_1_to_18_2025 import generate_matchup_boxscore, TEAM_DIVISIONS

WEEK = 1
ITERATIONS = 1000


def run():
    sched_df = pd.read_csv("data/external/schedule_2025.csv")
    week_games = sched_df[(sched_df["week"] == WEEK) & (sched_df["game_type"] == "REG")]

    games_cache_path = "data/interim/sim_results_2025_games.parquet"
    players_cache_path = "data/interim/sim_results_2025_players.parquet"
    all_games_df = pd.read_parquet(games_cache_path)
    all_players_df = pd.read_parquet(players_cache_path)

    all_nfl_teams = set(TEAM_DIVISIONS.keys())
    active_teams = set(week_games["away_team"]).union(set(week_games["home_team"]))
    bye_teams = sorted(list(all_nfl_teams - active_teams))

    week_boxscores = []
    week_boxscore_dir = f"docs/boxscores/week_{WEEK}"
    os.makedirs(week_boxscore_dir, exist_ok=True)

    for idx, row in week_games.iterrows():
        away, home, game_id = row["away_team"], row["home_team"], row["game_id"]
        game_df = all_games_df[all_games_df["game_id"] == game_id]
        player_df = all_players_df[all_players_df["game_id"] == game_id]
        print(f"Building boxscore: {away} @ {home}...")
        week_boxscores.append(generate_matchup_boxscore(away, home, WEEK, game_df, player_df, ITERATIONS))

    week_boxscore_path = os.path.join(week_boxscore_dir, f"week_{WEEK}_boxscore.md")
    bye_str = ", ".join(bye_teams) if bye_teams else "None"
    header_md = f"# \U0001F4CA NFL 2025 Week {WEEK} Complete Boxscores\n"
    header_md += f"**Monte Carlo Simulated Averages across {ITERATIONS} Iterations**\n\n"
    header_md += f"> [!NOTE]\n> **Teams on Bye**: {bye_str}\n\n---\n\n"

    with open(week_boxscore_path, "w", encoding="utf-8") as f:
        f.write(header_md + "\n\n---\n\n".join(week_boxscores))

    print(f"Saved: {week_boxscore_path}")


if __name__ == "__main__":
    run()
