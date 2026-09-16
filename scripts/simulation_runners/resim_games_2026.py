"""Re-simulate a subset of a week's games in place, without touching the
other games already cached in data/interim/dfs_week_{week}_{games,players}.parquet.

run_week_sim_2026.py always rebuilds the whole week (every game gets a fresh
RNG draw), which is wasteful and needlessly perturbs unrelated games' numbers
when only one or two teams' rosters actually changed (e.g. a confirmed
inactive/starter update after a roster override edit). This script reruns
only the named games and splices their rows back into the existing parquet,
leaving every other game's cached rows byte-for-byte untouched.

Before running, make sure data/current_rosters/dfs/*_traits_2026.json is
current for the affected teams:
    venv\\Scripts\\python.exe scripts/roster_management/apply_team_week_overrides_v_0_1_0.py <week> <TEAM>

Usage:
    venv\\Scripts\\python.exe scripts/simulation_runners/resim_games_2026.py <week> <AWAY>_<HOME> [<AWAY>_<HOME> ...] [--iterations N]

Example:
    venv\\Scripts\\python.exe scripts/simulation_runners/resim_games_2026.py 1 ATL_PIT TB_CIN
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


def resim_games(week, matchups, iterations=10000):
    games_cache_path = os.path.join("data", "interim", f"dfs_week_{week}_games.parquet")
    players_cache_path = os.path.join("data", "interim", f"dfs_week_{week}_players.parquet")
    if not os.path.exists(games_cache_path) or not os.path.exists(players_cache_path):
        raise SystemExit(f"No existing week {week} cache found -- run run_week_sim_2026.py {week} first.")

    sched_df = pd.read_csv(SCHEDULE_PATH)
    week_games = sched_df[(sched_df["week"] == week) & (sched_df["game_type"] == "REG")]

    existing_games = pd.read_parquet(games_cache_path)
    existing_players = pd.read_parquet(players_cache_path)

    fresh_games_list, fresh_players_list = [], []
    resimmed_game_ids = []
    start_time = time.time()
    for away, home in matchups:
        row = week_games[(week_games["away_team"] == away) & (week_games["home_team"] == home)]
        if row.empty:
            raise SystemExit(f"No week {week} schedule entry for {away} @ {home}.")
        row = row.iloc[0]
        game_id = row["game_id"]
        resimmed_game_ids.append(game_id)

        print(f"Re-simulating {away} @ {home} ({game_id}), {iterations} iterations...")
        batch = BatchSimulator(away, home, year=SIM_YEAR, rosters_dir=ROSTERS_DIR)
        game_df, player_df = batch.run_batch(iterations=iterations, vectorized=True)

        game_df = game_df.rename(columns={"game_id": "iteration"})
        game_df["game_id"] = game_id
        game_df["away_team"] = away
        game_df["home_team"] = home
        game_df["div_game"] = row["div_game"]
        fresh_games_list.append(game_df)

        if player_df is not None and not player_df.empty:
            player_df = player_df.rename(columns={"game_id": "iteration"})
            player_df["game_id"] = game_id
            fresh_players_list.append(player_df)

    print(f"Re-simulation complete in {time.time() - start_time:.2f}s.")

    fresh_games_df = pd.concat(fresh_games_list, ignore_index=True)
    fresh_players_df = pd.concat(fresh_players_list, ignore_index=True)

    kept_games = existing_games[~existing_games["game_id"].isin(resimmed_game_ids)]
    kept_players = existing_players[~existing_players["game_id"].isin(resimmed_game_ids)]

    merged_games = pd.concat([kept_games, fresh_games_df], ignore_index=True)
    merged_players = pd.concat([kept_players, fresh_players_df], ignore_index=True)

    merged_games.to_parquet(games_cache_path, index=False)
    merged_players.to_parquet(players_cache_path, index=False)
    print(f"Saved {games_cache_path}\nSaved {players_cache_path}")
    print(f"Re-simmed: {resimmed_game_ids} -- all other games in the week left untouched.")

    # Same staleness guard run_week_sim_2026.py uses -- app.py's own
    # mtime-staleness check on these baked JSON caches doesn't know the DFS
    # parquet exists, so they'd otherwise keep serving pre-resim numbers.
    for stale in (os.path.join("data", "interim", f"week_{week}_full_projections.json"),
                 os.path.join("data", "interim", f"week_{week}_sim_results.json")):
        if os.path.exists(stale):
            os.remove(stale)
            print(f"Removed stale baked cache: {stale}")

    return merged_games, merged_players


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(1)
    wk = int(sys.argv[1])
    iters = 10000
    args = sys.argv[2:]
    if "--iterations" in args:
        i = args.index("--iterations")
        iters = int(args[i + 1])
        args = args[:i] + args[i + 2:]
    pairs = [tuple(a.split("_", 1)) for a in args]
    resim_games(wk, pairs, iterations=iters)
