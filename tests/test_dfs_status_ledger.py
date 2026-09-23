"""
tests/test_dfs_status_ledger.py
===============================
# Status: live | 2026-09-22

Unit tests for sticky gameday Active/Inactive toggles:
  - week_roster_v_0_1_0.effective_dfs_status / reserve_signature (pure)
  - resolve_week_rows' new "force_active" handling (IR override)
  - scripts/roster_management/dfs_status_ledger.record_toggle /
    statuses_for_week / save+load round trip

Run from repo root:
    python -m pytest tests/test_dfs_status_ledger.py -v
"""
import os
import sys
import tempfile
import unittest

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, ROOT)
sys.path.insert(0, os.path.join(ROOT, "scripts", "roster_management"))

from src.data_pipeline.week_roster_v_0_1_0 import (  # noqa: E402
    effective_dfs_status, reserve_signature, resolve_week_rows,
)
from dfs_status_ledger import (  # noqa: E402
    record_toggle, statuses_for_week, save_ledger, load_ledger,
)

IR_WR = {"player_name": "WR1", "pos": "WR", "roster_slot": "ir", "return_week": "8",
         "target_share": "0.25", "carry_share": "0.0", "note": ""}
WR2 = {"player_name": "WR2", "pos": "WR", "roster_slot": "active", "return_week": "",
       "target_share": "0.20", "carry_share": "0.0", "note": ""}
WR3 = {"player_name": "WR3", "pos": "WR", "roster_slot": "active", "return_week": "",
       "target_share": "0.15", "carry_share": "0.0", "note": ""}
CUT_WR = {"player_name": "Gone WR", "pos": "WR", "roster_slot": "cut", "return_week": "",
          "target_share": "0.10", "carry_share": "0.0", "note": ""}


class TestEffectiveStatus(unittest.TestCase):
    def test_no_entries_is_none(self):
        self.assertIsNone(effective_dfs_status([], 3, WR2))

    def test_out_is_sticky_forward(self):
        entries = [{"week": 3, "status": "out"}]
        self.assertIsNone(effective_dfs_status(entries, 2, WR2))       # before the toggle
        self.assertEqual(effective_dfs_status(entries, 3, WR2), "out")
        self.assertEqual(effective_dfs_status(entries, 9, WR2), "out")  # carried forward

    def test_latest_entry_wins(self):
        entries = [{"week": 3, "status": "out"}, {"week": 5, "status": "active"}]
        self.assertEqual(effective_dfs_status(entries, 4, WR2), "out")
        self.assertEqual(effective_dfs_status(entries, 6, WR2), "active")

    def test_ir_override_same_stint(self):
        sig = reserve_signature(IR_WR, 3)
        self.assertEqual(sig, "ir|8")
        entries = [{"week": 3, "status": "active", "reserve_sig": sig}]
        self.assertEqual(effective_dfs_status(entries, 4, IR_WR), "force_active")

    def test_ir_override_does_not_cancel_new_stint(self):
        # Cam later puts the player on a NEW IR stint (return week 14) --
        # the old week-3 override must not keep him active.
        entries = [{"week": 3, "status": "active", "reserve_sig": "ir|8"}]
        new_stint = dict(IR_WR, return_week="14")
        self.assertEqual(effective_dfs_status(entries, 10, new_stint), "active")

    def test_healthy_player_has_no_signature(self):
        self.assertIsNone(reserve_signature(WR2, 3))
        self.assertIsNone(reserve_signature(IR_WR, 8))   # back by week 8


class TestForceActiveResolve(unittest.TestCase):
    def test_ir_player_force_active_keeps_full_share(self):
        rows = [dict(IR_WR, dfs_status="force_active"), dict(WR2), dict(WR3)]
        new, report = resolve_week_rows(rows, week=3)
        by = {r["player_name"]: r for r in new}
        self.assertAlmostEqual(by["WR1"]["target_share"], 0.25)
        self.assertAlmostEqual(by["WR2"]["target_share"], 0.20)   # no redistribution happened
        self.assertEqual([r for r in report if r["pool"] > 0], [])

    def test_force_active_ir_player_absorbs_teammates_share(self):
        rows = [dict(IR_WR, dfs_status="force_active"), dict(WR2, dfs_status="out"), dict(WR3)]
        new, _ = resolve_week_rows(rows, week=3)
        by = {r["player_name"]: r for r in new}
        self.assertEqual(by["WR2"]["target_share"], 0.0)
        self.assertGreater(by["WR1"]["target_share"], 0.25)

    def test_cut_player_cannot_be_forced(self):
        rows = [dict(CUT_WR, dfs_status="force_active"), dict(WR2)]
        new, _ = resolve_week_rows(rows, week=3)
        by = {r["player_name"]: r for r in new}
        self.assertEqual(by["Gone WR"]["target_share"], 0.0)


class TestLedger(unittest.TestCase):
    def test_retoggle_same_week_replaces(self):
        ledger = {}
        record_toggle(ledger, "GB", "WR2", 3, "out", WR2)
        record_toggle(ledger, "GB", "WR2", 3, "active", WR2)
        entries = ledger["GB"]["wr2"]["entries"]
        self.assertEqual(len(entries), 1)
        self.assertEqual(entries[0]["status"], "active")

    def test_ir_active_stamps_signature(self):
        ledger = {}
        record_toggle(ledger, "GB", "WR1", 3, "active", IR_WR)
        self.assertEqual(ledger["GB"]["wr1"]["entries"][0]["reserve_sig"], "ir|8")

    def test_statuses_for_week(self):
        ledger = {}
        record_toggle(ledger, "GB", "WR2", 3, "out", WR2)
        record_toggle(ledger, "GB", "WR1", 3, "active", IR_WR)
        season = [IR_WR, WR2, WR3]
        self.assertEqual(statuses_for_week(ledger, "GB", 2, season), {})
        self.assertEqual(statuses_for_week(ledger, "GB", 5, season),
                         {"wr2": "out", "wr1": "force_active"})

    def test_round_trip(self):
        ledger = {}
        record_toggle(ledger, "GB", "WR2", 3, "out", WR2)
        with tempfile.TemporaryDirectory() as d:
            save_ledger(ledger, 2026, d)
            self.assertEqual(load_ledger(2026, d), ledger)
            self.assertEqual(load_ledger(2099, d), {})


if __name__ == "__main__":
    unittest.main()
