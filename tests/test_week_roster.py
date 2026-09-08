"""
tests/test_week_roster.py
=========================
# Status: live | v2.0.0 | 2026-09-02

Unit tests for the flat per-team override sheet week resolver
(src/data_pipeline/week_roster_v_0_1_0).

Run from repo root:
    python -m pytest tests/test_week_roster.py -v
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from src.data_pipeline.week_roster_v_0_1_0 import (
    parse_return_week, is_available, resolve_week_rows,
)


def _rows():
    """GB-shaped fixture: exempt bell-cow RB, IR WR (back wk 8), a healthy
    backfield + WR room, and a practice-squad body."""
    return [
        {"player_name": "Starter RB", "pos": "RB", "roster_slot": "exempt",
         "return_week": "99", "target_share": "0.09", "carry_share": "0.63", "note": ""},
        {"player_name": "Backup RB", "pos": "RB", "roster_slot": "active",
         "return_week": "", "target_share": "0.05", "carry_share": "0.26", "note": ""},
        {"player_name": "3rd RB", "pos": "RB", "roster_slot": "active",
         "return_week": "", "target_share": "0.01", "carry_share": "0.04", "note": ""},
        {"player_name": "WR1", "pos": "WR", "roster_slot": "ir",
         "return_week": "8", "target_share": "0.25", "carry_share": "0.0", "note": ""},
        {"player_name": "WR2", "pos": "WR", "roster_slot": "active",
         "return_week": "", "target_share": "0.20", "carry_share": "0.0", "note": ""},
        {"player_name": "WR3", "pos": "WR", "roster_slot": "active",
         "return_week": "", "target_share": "0.15", "carry_share": "0.0", "note": ""},
        {"player_name": "PS WR", "pos": "WR", "roster_slot": "practice_squad",
         "return_week": "", "target_share": "0.0", "carry_share": "0.0", "note": ""},
    ]


class TestParseReturnWeek(unittest.TestCase):
    def test_blank_is_never(self):
        self.assertEqual(parse_return_week(""), 99)
        self.assertEqual(parse_return_week(None), 99)

    def test_numeric(self):
        self.assertEqual(parse_return_week("8"), 8)
        self.assertEqual(parse_return_week(8.0), 8)


class TestIsAvailable(unittest.TestCase):
    def test_active_always(self):
        self.assertTrue(is_available({"roster_slot": "active"}, 1))

    def test_reserve_gated_by_return_week(self):
        r = {"roster_slot": "ir", "return_week": "8"}
        self.assertFalse(is_available(r, 5))
        self.assertTrue(is_available(r, 8))
        self.assertTrue(is_available(r, 12))

    def test_exempt_never(self):
        self.assertFalse(is_available({"roster_slot": "exempt", "return_week": "99"}, 18))


class TestResolveWeek1(unittest.TestCase):
    def setUp(self):
        self.new, self.report = resolve_week_rows(_rows(), week=1)
        self.by = {r["player_name"]: r for r in self.new}

    def test_all_rows_preserved(self):
        self.assertEqual(len(self.new), 7)

    def test_out_players_zeroed(self):
        self.assertEqual(self.by["Starter RB"]["carry_share"], 0.0)
        self.assertEqual(self.by["WR1"]["target_share"], 0.0)
        self.assertIn("OUT wk1", self.by["Starter RB"]["note"])

    def test_carry_redistributed_pro_rata(self):
        # pool 0.63 split 0.26 : 0.04
        self.assertAlmostEqual(self.by["Backup RB"]["carry_share"],
                               0.26 + 0.63 * (0.26 / 0.30), places=4)
        self.assertAlmostEqual(self.by["3rd RB"]["carry_share"],
                               0.04 + 0.63 * (0.04 / 0.30), places=4)

    def test_team_carry_total_preserved(self):
        total = sum(r["carry_share"] for r in self.new)
        self.assertAlmostEqual(total, 0.63 + 0.26 + 0.04, places=4)

    def test_wr_target_redistributed_excludes_practice_squad(self):
        # WR1's 0.25 splits 0.20 : 0.15 between WR2 : WR3 only (not PS WR)
        self.assertAlmostEqual(self.by["WR2"]["target_share"],
                               0.20 + 0.25 * (0.20 / 0.35), places=4)
        self.assertEqual(self.by["PS WR"]["target_share"], 0.0)


class TestResolveMidSeason(unittest.TestCase):
    def test_ir_back_by_week_10(self):
        new, _ = resolve_week_rows(_rows(), week=10)
        by = {r["player_name"]: r for r in new}
        self.assertAlmostEqual(by["WR1"]["target_share"], 0.25, places=6)   # back -> curated
        self.assertAlmostEqual(by["WR2"]["target_share"], 0.20, places=6)
        self.assertAlmostEqual(by["WR3"]["target_share"], 0.15, places=6)

    def test_exempt_still_out_week_18(self):
        new, _ = resolve_week_rows(_rows(), week=18)
        by = {r["player_name"]: r for r in new}
        self.assertEqual(by["Starter RB"]["carry_share"], 0.0)
        self.assertGreater(by["Backup RB"]["carry_share"], 0.7)


class TestZoneShares(unittest.TestCase):
    """rz_* / gl_* shares redistribute the same way as the overall shares."""
    def test_redzone_and_goalline_carry_redistributed(self):
        rows = [
            {"player_name": "Bell Cow", "pos": "RB", "roster_slot": "ir",
             "return_week": "9", "target_share": "0.08", "carry_share": "0.60",
             "rz_target_share": "0.06", "rz_carry_share": "0.55",
             "gl_target_share": "0.04", "gl_carry_share": "0.70", "note": ""},
            {"player_name": "Backup", "pos": "RB", "roster_slot": "active",
             "return_week": "", "target_share": "0.04", "carry_share": "0.30",
             "rz_target_share": "0.03", "rz_carry_share": "0.30",
             "gl_target_share": "0.02", "gl_carry_share": "0.25", "note": ""},
        ]
        new, _ = resolve_week_rows(rows, week=1)
        by = {r["player_name"]: r for r in new}
        self.assertEqual(by["Bell Cow"]["gl_carry_share"], 0.0)
        # backup is the only recipient -> absorbs the whole pool in every field
        self.assertAlmostEqual(by["Backup"]["gl_carry_share"], 0.95, places=4)
        self.assertAlmostEqual(by["Backup"]["rz_carry_share"], 0.85, places=4)
        self.assertAlmostEqual(by["Backup"]["carry_share"], 0.90, places=4)


class TestLostShare(unittest.TestCase):
    def test_lost_when_no_active_same_pos(self):
        rows = [
            {"player_name": "Lone TE", "pos": "TE", "roster_slot": "ir",
             "return_week": "9", "target_share": "0.22", "carry_share": "0.0", "note": ""},
        ]
        _, report = resolve_week_rows(rows, week=1)
        self.assertIn("LOST", report[0]["method"])
        self.assertEqual(report[0]["recipients"], [])


if __name__ == "__main__":
    unittest.main()
