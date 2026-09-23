"""
tests/test_sim_run_status.py
============================
# Status: live | 2026-09-23

Unit tests for scripts/simulation_runners/sim_run_status.py -- the
"sims running" marker + atomic parquet writes behind the site's auto-refresh.

Run from repo root:
    python -m pytest tests/test_sim_run_status.py -v
"""
import json
import os
import sys
import tempfile
import time
import unittest

import pandas as pd

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, os.path.join(ROOT, "scripts", "simulation_runners"))

from sim_run_status import (  # noqa: E402
    run_marker, read_run_marker, marker_path, atomic_to_parquet, STALE_AFTER_S,
)


class TestRunMarker(unittest.TestCase):
    def test_marker_present_only_during_run(self):
        with tempfile.TemporaryDirectory() as d:
            self.assertIsNone(read_run_marker(3, d))
            with run_marker(3, iterations=10000, base_dir=d):
                m = read_run_marker(3, d)
                self.assertEqual(m["week"], 3)
                self.assertEqual(m["iterations"], 10000)
                self.assertIsNone(m["games"])
            self.assertIsNone(read_run_marker(3, d))

    def test_marker_removed_on_exception(self):
        with tempfile.TemporaryDirectory() as d:
            with self.assertRaises(RuntimeError):
                with run_marker(3, base_dir=d):
                    raise RuntimeError("sim crashed")
            self.assertFalse(os.path.exists(marker_path(3, d)))

    def test_stale_marker_ignored(self):
        with tempfile.TemporaryDirectory() as d:
            path = marker_path(3, d)
            os.makedirs(os.path.dirname(path))
            with open(path, "w", encoding="utf-8") as f:
                json.dump({"week": 3, "started_at": time.time() - STALE_AFTER_S - 60}, f)
            self.assertIsNone(read_run_marker(3, d))

    def test_other_week_not_reported(self):
        with tempfile.TemporaryDirectory() as d:
            with run_marker(3, base_dir=d):
                self.assertIsNone(read_run_marker(4, d))


class TestAtomicParquet(unittest.TestCase):
    def test_round_trip_and_no_tmp_left(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "x.parquet")
            df = pd.DataFrame({"a": [1, 2, 3]})
            atomic_to_parquet(df, path)
            atomic_to_parquet(df.assign(a=[4, 5, 6]), path)   # overwrite in place
            self.assertEqual(pd.read_parquet(path)["a"].tolist(), [4, 5, 6])
            self.assertFalse(os.path.exists(path + ".tmp"))


if __name__ == "__main__":
    unittest.main()
