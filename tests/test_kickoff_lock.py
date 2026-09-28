"""
tests/test_kickoff_lock.py
==========================
# Status: live | 2026-09-25

Tests for the kickoff lock (2026-09-25): once a game kicks off, its sim is
frozen so the Evaluation tab grades the last PRE-kickoff prediction.
  - sim_run_status.kickoff_ts / has_kicked_off (ET schedule -> absolute time)
  - resim_games_2026.resim_games: skips kicked-off games, raises if ALL are
    locked (so the in-app roster toggle reports "locked"), --force overrides
  - run_week_sim_2026.simulate_week: carries kicked-off games' rows forward
    unchanged, re-sims the rest, stamps sim_run_at

The two writers run end to end inside a temp repo layout with BatchSimulator
swapped for a tiny fake, so no real sims run.

Run from repo root:
    python -m pytest tests/test_kickoff_lock.py -v
"""
import os
import sys
import tempfile
import unittest

import pandas as pd

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, ROOT)
sys.path.insert(0, os.path.join(ROOT, "scripts", "simulation_runners"))

import resim_games_2026  # noqa: E402
import run_week_sim_2026  # noqa: E402
from sim_run_status import kickoff_ts, has_kicked_off, SIM_RUN_AT_COL  # noqa: E402

PAST = {"gameday": "2020-09-10", "gametime": "20:20"}      # long since kicked off
FUTURE = {"gameday": "2099-09-10", "gametime": "13:00"}    # never kicked off
OLD_SCORE = 99                                               # marks rows from the pre-existing cache


class FakeBatch:
    """Stand-in for BatchSimulator: 3 iterations, home always wins 24-17."""
    def __init__(self, away, home, year=None, rosters_dir=None):
        self.away, self.home = away, home

    def run_batch(self, iterations, vectorized=True):
        n = 3
        games = pd.DataFrame({"game_id": range(n), "away_score": [17] * n, "home_score": [24] * n,
                              "total": [41] * n, "spread": [-7] * n, "winner": [self.home] * n})
        players = pd.DataFrame({"game_id": range(n), "Player": ["p"] * n, "Team": [self.home] * n})
        return games, players


class TestKickoffTime(unittest.TestCase):
    def test_eastern_to_utc(self):
        # Thu 2026-09-24 8:15 PM EDT = 2026-09-25 00:15 UTC
        self.assertEqual(kickoff_ts({"gameday": "2026-09-24", "gametime": "20:15"}),
                         pd.Timestamp("2026-09-25 00:15", tz="UTC").timestamp())

    def test_standard_time_after_dst_ends(self):
        # 2026-11-29 1:00 PM EST (UTC-5) = 18:00 UTC
        self.assertEqual(kickoff_ts({"gameday": "2026-11-29", "gametime": "13:00"}),
                         pd.Timestamp("2026-11-29 18:00", tz="UTC").timestamp())

    def test_has_kicked_off(self):
        row = {"gameday": "2026-09-24", "gametime": "20:15"}
        ko = kickoff_ts(row)
        self.assertFalse(has_kicked_off(row, now=ko - 1))
        self.assertTrue(has_kicked_off(row, now=ko))

    def test_bad_data_never_locks(self):
        self.assertIsNone(kickoff_ts({"gameday": None, "gametime": None}))
        self.assertFalse(has_kicked_off({}))


class _TempRepo(unittest.TestCase):
    """Temp working dir with a 2-game week-3 schedule (one played, one future),
    an existing week cache for both, and a non-empty DFS roster dir."""
    def setUp(self):
        self._cwd = os.getcwd()
        self._tmp = tempfile.TemporaryDirectory()
        os.chdir(self._tmp.name)
        for d in ("data/external", "data/interim", "data/current_rosters/dfs"):
            os.makedirs(d)
        open("data/current_rosters/dfs/GB_traits_2026.json", "w").write("{}")
        pd.DataFrame([
            {"game_id": "g_past", "week": 3, "game_type": "REG", "away_team": "ATL", "home_team": "GB", "div_game": 0, **PAST},
            {"game_id": "g_future", "week": 3, "game_type": "REG", "away_team": "NE", "home_team": "BUF", "div_game": 1, **FUTURE},
        ]).to_csv("data/external/schedule_2026.csv", index=False)
        old_games = pd.DataFrame([{"iteration": i, "away_score": OLD_SCORE, "home_score": OLD_SCORE, "total": 2 * OLD_SCORE,
                                   "game_id": g} for g in ("g_past", "g_future") for i in range(2)])
        old_players = pd.DataFrame([{"iteration": i, "Player": "old", "game_id": g}
                                    for g in ("g_past", "g_future") for i in range(2)])
        old_games.to_parquet("data/interim/dfs_week_3_games.parquet", index=False)
        old_players.to_parquet("data/interim/dfs_week_3_players.parquet", index=False)
        self._patches = [(m, m.BatchSimulator) for m in (resim_games_2026, run_week_sim_2026)]
        for m, _ in self._patches:
            m.BatchSimulator = FakeBatch

    def tearDown(self):
        for m, orig in self._patches:
            m.BatchSimulator = orig
        os.chdir(self._cwd)
        self._tmp.cleanup()

    @staticmethod
    def games():
        return pd.read_parquet("data/interim/dfs_week_3_games.parquet")


class TestResimLock(_TempRepo):
    def test_all_locked_raises_and_leaves_cache_untouched(self):
        before = self.games()
        with self.assertRaises(ValueError):
            resim_games_2026.resim_games(3, [("ATL", "GB")], iterations=3)
        pd.testing.assert_frame_equal(before, self.games())

    def test_future_game_resimmed_and_stamped(self):
        resim_games_2026.resim_games(3, [("NE", "BUF")], iterations=3)
        g = self.games()
        fut = g[g["game_id"] == "g_future"]
        self.assertTrue((fut["home_score"] == 24).all())
        self.assertTrue(fut[SIM_RUN_AT_COL].notna().all())
        self.assertTrue((g[g["game_id"] == "g_past"]["home_score"] == OLD_SCORE).all())   # untouched

    def test_mixed_request_skips_only_the_locked_game(self):
        resim_games_2026.resim_games(3, [("ATL", "GB"), ("NE", "BUF")], iterations=3)
        g = self.games()
        self.assertTrue((g[g["game_id"] == "g_past"]["home_score"] == OLD_SCORE).all())
        self.assertTrue((g[g["game_id"] == "g_future"]["home_score"] == 24).all())

    def test_force_overrides_lock(self):
        resim_games_2026.resim_games(3, [("ATL", "GB")], iterations=3, force=True)
        g = self.games()
        self.assertTrue((g[g["game_id"] == "g_past"]["home_score"] == 24).all())


class TestWeekSimLock(_TempRepo):
    def test_played_game_carried_forward_rest_resimmed(self):
        run_week_sim_2026.simulate_week(3, iterations=3)
        g = self.games()
        past, fut = g[g["game_id"] == "g_past"], g[g["game_id"] == "g_future"]
        self.assertEqual(len(past), 2)                                  # the old 2 rows, not 3 new ones
        self.assertTrue((past["home_score"] == OLD_SCORE).all())
        self.assertTrue((fut["home_score"] == 24).all())
        self.assertTrue(fut[SIM_RUN_AT_COL].notna().all())
        players = pd.read_parquet("data/interim/dfs_week_3_players.parquet")
        self.assertTrue((players[players["game_id"] == "g_past"]["Player"] == "old").all())

    def test_force_resims_everything(self):
        run_week_sim_2026.simulate_week(3, iterations=3, force=True)
        self.assertTrue((self.games()["home_score"] == 24).all())

    def test_played_game_without_cache_is_simmed(self):
        os.remove("data/interim/dfs_week_3_games.parquet")
        os.remove("data/interim/dfs_week_3_players.parquet")
        run_week_sim_2026.simulate_week(3, iterations=3)
        g = self.games()
        self.assertEqual(set(g["game_id"]), {"g_past", "g_future"})
        self.assertTrue(g[SIM_RUN_AT_COL].notna().all())               # stamp proves it's post-kickoff


if __name__ == "__main__":
    unittest.main()
