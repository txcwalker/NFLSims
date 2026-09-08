"""Regenerates every docs/reports/*_2026.* file the frontend_analysis
Season2026 page reads, in dependency order, so they can never drift out of
sync the way they did in the 2026-08-18 standings-vs-leaders bug (see
WORKLOG.md).

Root cause that made this script necessary: run_full_season_sim_2026.py is
the only one of these scripts that checks whether the shared parquet cache
(data/interim/sim_results_2026_{games,players}.parquet) is stale against the
DNA/roster inputs and re-simulates when it is. generate_team_stats_2026.py,
generate_season_leaders_2026.py, and generate_matchup_win_probabilities_2026.py
all just blindly read whatever parquet cache is on disk -- so running the
season sim alone (e.g. after a DNA edit) silently invalidates the other
three's output without regenerating it. Always run this script instead of
calling run_full_season_sim_2026.py by itself.

Order matters: run_full_season_sim_2026.py first (writes the parquet caches
plus season_summaries_2026.csv and teams_data.json); everything else reads
those caches or that CSV and has no dependents here.

Usage: python regenerate_2026_reports.py
"""
import subprocess
import sys
import os

REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))

# Match run_full_season_sim_2026.py's toggle: the week-tree build step only
# matters when the sim will actually read the trees (NFLSIM_WEEK_AWARE=1).
# Off by default (2026-09-08) -- season-long starters, regular season + playoffs.
WEEK_AWARE_ROSTERS = os.environ.get("NFLSIM_WEEK_AWARE", "0") == "1"

STEPS = [
    *([("Week-by-week roster trees (injury returns + mid-season QB swaps)",
        [sys.executable, "scripts/roster_management/build_season_week_rosters_v_0_1_0.py", "2026", "18"])]
      if WEEK_AWARE_ROSTERS else []),
    ("Full season sim + playoffs (parquet caches, standings, teams_data.json)",
     [sys.executable, "scripts/simulation_runners/run_full_season_sim_2026.py"]),
    ("Team stats (team_stats_2026.csv)",
     [sys.executable, "scripts/simulation_runners/generate_team_stats_2026.py"]),
    ("Season leaders (season_leaders_2026.json)",
     [sys.executable, "scripts/simulation_runners/generate_season_leaders_2026.py"]),
    ("Matchup win probabilities (matchup_win_probabilities_2026.json)",
     [sys.executable, "scripts/simulation_runners/generate_matchup_win_probabilities_2026.py"]),
    ("Playoff summary (playoff_summary_2026.md)",
     [sys.executable, "scripts/simulation_runners/generate_playoff_summary_2026.py"]),
]


def main():
    for label, cmd in STEPS:
        print(f"\n=== {label} ===")
        result = subprocess.run(cmd, cwd=REPO_ROOT)
        if result.returncode != 0:
            raise SystemExit(f"FAILED: {label} (exit {result.returncode}) -- stopping, later steps may depend on this one.")
    print("\n=== All 2026 season report files regenerated from the same simulation run. ===")


if __name__ == "__main__":
    main()
