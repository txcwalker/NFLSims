"""
Tests the Round 12 sacks-diversion hypothesis (clock_physics_v020 Round 14):
does gate 2's raw sack probability already approximate the real recorded
sack rate, with the scramble-escape/throwaway diversion pipeline then
needlessly siphoning off ~24% of it on top of an already-correct number?

Compares, per team-game (both teams combined / 2, matching how
run_historical_eda.py's real 'sacks_per_game' is defined):
  - gross_sacks_per_game : gate 2's raw has_sack roll, before any diversion
  - net_sacks_per_game   : actual_sacks, what actually gets counted (this is
                           the same "sacks_per_game" tracked all session)
against the real target of 2.41/game.
"""
import sys
import os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

import numpy as np
import pandas as pd
from src.nfl_sim.batch import BatchSimulator
from src.nfl_sim.game_engine import NFLGameEngine

REAL_SACKS_PER_GAME = 2.41

MATCHUPS = [
    ("DAL", "PHI"), ("KC", "LAC"), ("TB", "ATL"), ("CIN", "CLE"),
    ("MIA", "IND"), ("CAR", "JAX"), ("LV", "NE"), ("ARI", "NO"),
    ("PIT", "NYJ"), ("NYG", "WAS"), ("TEN", "DEN"), ("SF", "SEA"),
    ("DET", "GB"), ("HOU", "LA"), ("BAL", "BUF"), ("MIN", "CHI"),
]


def run(n_iterations=1000, year=2025):
    gross_per_team_game = []
    net_per_team_game = []

    for away, home in MATCHUPS:
        print(f"Simulating {away} @ {home} (N={n_iterations})...")
        batch = BatchSimulator(away, home, year=year)
        engine = NFLGameEngine(
            away, home, year=year,
            dna=batch.dna, team_coaches=batch.team_coaches,
            rosters=batch.rosters, trench_tiers=batch.trench_tiers,
            N=n_iterations,
        )
        engine.run_game()

        # both teams combined -> divide by 2 for a per-team-game figure,
        # matching real's per-(game_id, posteam) definition
        gross_per_team_game.extend((engine.gross_sacks / 2.0).tolist())
        net_per_team_game.extend((engine.net_sacks / 2.0).tolist())

    gross_mean = float(np.mean(gross_per_team_game))
    net_mean = float(np.mean(net_per_team_game))

    print("\n" + "=" * 70)
    print(f"{'Metric':30s} {'Sim':>10s} {'Real':>10s} {'Delta%':>10s}")
    print(f"{'gross_sacks_per_game (pre-diversion)':30s} {gross_mean:10.3f} {REAL_SACKS_PER_GAME:10.3f} "
          f"{(gross_mean - REAL_SACKS_PER_GAME) / REAL_SACKS_PER_GAME * 100:9.1f}%")
    print(f"{'net_sacks_per_game (post-diversion)':30s} {net_mean:10.3f} {REAL_SACKS_PER_GAME:10.3f} "
          f"{(net_mean - REAL_SACKS_PER_GAME) / REAL_SACKS_PER_GAME * 100:9.1f}%")
    print(f"\nDiversion removed {(gross_mean - net_mean):.3f} sacks/team-game "
          f"({(1 - net_mean / gross_mean) * 100:.1f}% of gross), theoretical estimate was ~24%.")

    return gross_mean, net_mean


if __name__ == "__main__":
    run()
