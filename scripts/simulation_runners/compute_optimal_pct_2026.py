"""Precompute optimal_pct for a DFS week: how often each player appears in
the TRUE optimal DK Classic lineup, across EVERY iteration of that week's
sim (not a small live sample -- see /api/week_projections in src/api/app.py,
which prefers this file's output over its own small-sample live fallback).

Uses solve_optimal_lineup_milp (src/nfl_sim/optimizer.py) -- a real MILP
solve via PuLP+CBC, ~0.05-0.2s per iteration for a full multi-game slate,
exact rather than a time-boxed approximation. Runs across all of a week's
sim iterations (typically 10,000), so this takes on the order of minutes to
tens of minutes -- meant to be run as a batch step after run_week_sim_2026.py,
not live on a request.

Only players with a real DK salary are eligible (same rule
get_week_projections() already applies -- a player DK hasn't priced can't
actually be rostered in a real lineup).

Output: data/interim/week_{week}_optimal_pct.json
    {"week": int, "iterations": int, "counts": [{"name", "team", "count"}, ...]}

Usage:
    venv\\Scripts\\python.exe scripts/simulation_runners/compute_optimal_pct_2026.py <week>
"""
import os
import sys
import json
import time

sys.path.append(os.getcwd())

import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402

from src.api.app import (  # noqa: E402
    BASE_DIR, SCHEDULE_CSV_PATH, _get_dfs_week_players, get_week_salaries,
    ALL_PLAYERS_CACHED,
)
from src.nfl_sim.optimizer import solve_optimal_lineup_milp  # noqa: E402

SIM_YEAR = 2026
PROGRESS_EVERY = 200


def compute_optimal_pct(week, year=SIM_YEAR):
    sched_df = pd.read_csv(SCHEDULE_CSV_PATH)
    week_games_df = sched_df[(sched_df["week"] == week) & (sched_df["game_type"] == "REG")]
    if week_games_df.empty:
        raise SystemExit(f"No REG games found for week {week} in {SCHEDULE_CSV_PATH}.")
    week_game_ids = week_games_df["game_id"].unique().tolist()

    wp = _get_dfs_week_players(week)
    if wp is None:
        wp = ALL_PLAYERS_CACHED[ALL_PLAYERS_CACHED["game_id"].isin(week_game_ids)].copy()
    if wp is None or wp.empty:
        raise SystemExit(f"No simulated player data for week {week} -- run run_week_sim_2026.py {week} first.")

    print(f"Resolving DK salaries for week {week}...")
    salaries = get_week_salaries(week_games_df, year, None)
    priced_keys = [pk for pk in salaries if salaries[pk] is not None]
    if not priced_keys:
        raise SystemExit(f"No DK-priced players for week {week} -- DK salaries may not be live for this slate.")

    player_keys = priced_keys
    player_salaries = np.array([salaries[pk] for pk in player_keys])

    player_positions = {}
    for (p, t, pos), _ in wp.groupby(["Player", "Team", "Pos"]):
        player_positions[(p, t)] = pos
    player_positions_list = [player_positions.get(pk, "WR") for pk in player_keys]
    clean_positions = [pos.replace("1", "").replace("2", "").replace("3", "")
                       .replace("4", "").replace("5", "").replace("6", "")
                       for pos in player_positions_list]

    unique_iterations = sorted(wp["iteration"].unique())
    n_iterations = len(unique_iterations)
    print(f"Week {week}: {len(player_keys)} priced players, {n_iterations} sim iterations.")

    # Single O(rows) pass to build {iteration: {(player, team): score}} --
    # per-iteration boolean-mask filtering (wp[wp['iteration']==it]) would be
    # O(rows) EACH time, i.e. O(rows * n_iterations) overall; this is the same
    # vectorized-extract-then-single-loop pattern get_week_projections() uses.
    wp_players = wp["Player"].values
    wp_teams = wp["Team"].values
    wp_iterations = wp["iteration"].values
    wp_dk_scores = wp["dk_score"].values
    iter_scores = {it: {} for it in unique_iterations}
    for i in range(len(wp_players)):
        iter_scores[wp_iterations[i]][(wp_players[i], wp_teams[i])] = wp_dk_scores[i]

    optimal_counts = {pk: 0 for pk in player_keys}
    start_time = time.time()
    solved = 0
    for idx, it in enumerate(unique_iterations, start=1):
        scores_arr = np.array([iter_scores[it].get(pk, 0.0) for pk in player_keys])
        opt_lineup = solve_optimal_lineup_milp(player_keys, player_salaries, clean_positions, scores_arr)
        for pk in opt_lineup:
            optimal_counts[pk] = optimal_counts.get(pk, 0) + 1
        if opt_lineup:
            solved += 1
        if idx % PROGRESS_EVERY == 0 or idx == n_iterations:
            elapsed = time.time() - start_time
            rate = elapsed / idx
            eta = rate * (n_iterations - idx)
            print(f"  {idx}/{n_iterations} iterations solved "
                 f"({elapsed:.0f}s elapsed, ~{eta:.0f}s remaining)")

    if solved == 0:
        raise SystemExit(f"Week {week}: no iteration produced a legal lineup -- "
                         f"check the player pool has enough priced players at every position.")

    out_path = os.path.join(BASE_DIR, "data", "interim", f"week_{week}_optimal_pct.json")
    counts = [{"name": name, "team": team, "count": count}
             for (name, team), count in optimal_counts.items() if count > 0]
    counts.sort(key=lambda r: r["count"], reverse=True)
    with open(out_path, "w") as f:
        json.dump({"week": week, "iterations": n_iterations, "counts": counts}, f, indent=2)

    print(f"Wrote {out_path} -- {len(counts)} players with nonzero optimal_pct, "
         f"top: {counts[0]['name']} ({counts[0]['count']}/{n_iterations} = "
         f"{100.0*counts[0]['count']/n_iterations:.1f}%)" if counts else f"Wrote {out_path}")


if __name__ == "__main__":
    if len(sys.argv) < 2 or not sys.argv[1].isdigit():
        print(__doc__)
        sys.exit(1)
    compute_optimal_pct(int(sys.argv[1]))
