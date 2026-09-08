"""Simulate a single 2026 week off the DFS-specific roster tree
(data/current_rosters/dfs/, compiled by apply_team_week_overrides_v_0_1_0.py)
and cache the result separately from the season-long parquet cache.

This is the DFS counterpart to run_full_season_sim_2026.py's per-game loop
-- same BatchSimulator pattern, same output schema -- but only for the
requested week's games, and reading rosters from the DFS tree
(rosters_dir="data/current_rosters/dfs") instead of the season-long one, so
a week's injury-adjusted usage never touches the season-long cache/rosters.

Before running, make sure data/current_rosters/dfs/*_traits_2026.json is
current for this week:
    venv\\Scripts\\python.exe scripts/roster_management/apply_team_week_overrides_v_0_1_0.py <week>

Output (overwrites -- only the current week's cache is kept, per Cam
2026-09-04, same as the DFS roster JSON itself):
    data/interim/dfs_week_{week}_games.parquet
    data/interim/dfs_week_{week}_players.parquet

Also deletes the two "baked full-response" JSON caches
(week_{week}_full_projections.json, week_{week}_sim_results.json) if
present -- they were baked from the OLD data source and app.py's own
mtime-staleness checks don't know a DFS-specific parquet even exists, so
they'd otherwise keep serving pre-refresh numbers.

Usage:
    venv\\Scripts\\python.exe scripts/simulation_runners/run_week_sim_2026.py <week> [iterations]
"""
import os
import sys
import time

sys.path.append(os.getcwd())

import pandas as pd  # noqa: E402

from src.nfl_sim.batch import BatchSimulator  # noqa: E402

SIM_YEAR = 2026
ROSTERS_DIR = os.path.join("data", "current_rosters", "dfs")
SCHEDULE_PATH = os.path.join("data", "external", f"schedule_{SIM_YEAR}.csv")


def simulate_week(week, iterations=1000):
    print(f"\n{'='*60}\n Simulating NFL {SIM_YEAR} Week {week} (DFS roster tree)\n"
         f" Iterations per game: {iterations}\n{'='*60}\n")

    if not os.path.isdir(ROSTERS_DIR) or not os.listdir(ROSTERS_DIR):
        raise SystemExit(
            f"{ROSTERS_DIR} is empty -- run "
            f"scripts/roster_management/apply_team_week_overrides_v_0_1_0.py {week} first.")

    sched_df = pd.read_csv(SCHEDULE_PATH)
    week_games = sched_df[(sched_df["week"] == week) & (sched_df["game_type"] == "REG")]
    if week_games.empty:
        raise SystemExit(f"No REG games found for week {week} in {SCHEDULE_PATH}.")

    games_cache_path = os.path.join("data", "interim", f"dfs_week_{week}_games.parquet")
    players_cache_path = os.path.join("data", "interim", f"dfs_week_{week}_players.parquet")

    all_games_list, all_players_list = [], []
    start_time = time.time()
    for idx, row in week_games.iterrows():
        away, home, game_id = row["away_team"], row["home_team"], row["game_id"]
        print(f"Simulating {idx+1}/{len(week_games)}: {away} at {home}...")
        batch = BatchSimulator(away, home, year=SIM_YEAR, rosters_dir=ROSTERS_DIR)
        game_df, player_df = batch.run_batch(iterations=iterations, vectorized=True)

        game_df = game_df.rename(columns={"game_id": "iteration"})
        game_df["game_id"] = game_id
        game_df["away_team"] = away
        game_df["home_team"] = home
        game_df["div_game"] = row["div_game"]
        all_games_list.append(game_df)

        if player_df is not None and not player_df.empty:
            player_df = player_df.rename(columns={"game_id": "iteration"})
            player_df["game_id"] = game_id
            all_players_list.append(player_df)

    print(f"Week {week} simulation complete in {time.time() - start_time:.2f}s.")

    all_games_df = pd.concat(all_games_list, ignore_index=True)
    all_players_df = pd.concat(all_players_list, ignore_index=True)
    os.makedirs(os.path.dirname(games_cache_path), exist_ok=True)
    all_games_df.to_parquet(games_cache_path, index=False)
    all_players_df.to_parquet(players_cache_path, index=False)
    print(f"Saved {games_cache_path}\nSaved {players_cache_path}")

    # invalidate stale baked full-response caches app.py would otherwise
    # keep serving in preference to this fresh DFS-specific data
    for stale in (os.path.join("data", "interim", f"week_{week}_full_projections.json"),
                 os.path.join("data", "interim", f"week_{week}_sim_results.json")):
        if os.path.exists(stale):
            os.remove(stale)
            print(f"Removed stale baked cache: {stale}")

    return all_games_df, all_players_df


if __name__ == "__main__":
    if len(sys.argv) < 2 or not sys.argv[1].isdigit():
        print(__doc__)
        sys.exit(1)
    wk = int(sys.argv[1])
    iters = int(sys.argv[2]) if len(sys.argv) > 2 else 1000
    simulate_week(wk, iterations=iters)
