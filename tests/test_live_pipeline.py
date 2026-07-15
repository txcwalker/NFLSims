# tests/test_live_pipeline.py
# Automated Pipeline Test Suite for NFL 4th Down Live Bot.
# References: docs/database_guide.md
# ------------------------------------------------------------------------------

import os
import sys
import unittest
import sqlite3
import tempfile
from datetime import datetime, timedelta

# Add project root to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from src.live.state_store import (
    init_db, is_play_processed, mark_play_processed,
    log_post, is_game_in_cooldown, is_drive_posted, get_global_posts_last_hour
)
from src.live.espn_adapter import parse_possession_text, compute_yardline_100, parse_plays_to_fd_rows
from src.live.simulator_bridge import run_simulation
from src.live.posting_policy import should_post_decision, format_post
from src.live.post_targets import is_dry_run, post_everywhere


class TestStateStore(unittest.TestCase):
    def setUp(self):
        # Create a temporary database file
        self.db_fd, self.db_path = tempfile.mkstemp()
        init_db(self.db_path)

    def tearDown(self):
        os.close(self.db_fd)
        os.unlink(self.db_path)

    def test_database_initialization(self):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table'")
        tables = [r[0] for r in cursor.fetchall()]
        conn.close()
        self.assertIn("processed_plays", tables)
        self.assertIn("post_history", tables)

    def test_play_deduplication(self):
        game_id = "401547400"
        play_id = "102030"
        
        self.assertFalse(is_play_processed(game_id, play_id, self.db_path))
        mark_play_processed(game_id, play_id, self.db_path)
        self.assertTrue(is_play_processed(game_id, play_id, self.db_path))

    def test_cooldown_and_rate_limits(self):
        game_id = "401547400"
        drive_id = "d_5"
        play_id = "102030"
        
        self.assertFalse(is_game_in_cooldown(game_id, 60, self.db_path))
        self.assertFalse(is_drive_posted(game_id, drive_id, self.db_path))
        self.assertEqual(get_global_posts_last_hour(60, self.db_path), 0)
        
        log_post(game_id, drive_id, play_id, "clear_mistake", 0.05, self.db_path)
        
        self.assertTrue(is_game_in_cooldown(game_id, 60, self.db_path))
        self.assertTrue(is_drive_posted(game_id, drive_id, self.db_path))
        self.assertEqual(get_global_posts_last_hour(60, self.db_path), 1)


class TestEspnAdapter(unittest.TestCase):
    def test_possession_midfield_parsing(self):
        # Midfield cases
        team, yard = parse_possession_text("50", default_team="KC")
        self.assertEqual(team, "KC")
        self.assertEqual(yard, 50)
        
        team, yard = parse_possession_text("MID 50", default_team="BAL")
        self.assertEqual(team, "BAL")
        self.assertEqual(yard, 50)
        
        team, yard = parse_possession_text("MIDFIELD", default_team="KC")
        self.assertEqual(team, "KC")
        self.assertEqual(yard, 50)
        
        # Standard cases
        team, yard = parse_possession_text("KC 45")
        self.assertEqual(team, "KC")
        self.assertEqual(yard, 45)

    def test_compute_yardline_100(self):
        # Marked on our side (yardline_100 = 100 - yard)
        self.assertEqual(compute_yardline_100("KC", "KC", 40), 60)
        # Marked on their side (yardline_100 = yard)
        self.assertEqual(compute_yardline_100("KC", "BAL", 40), 40)
        # Midfield
        self.assertEqual(compute_yardline_100("KC", "KC", 50), 50)


class TestSimulatorBridge(unittest.TestCase):
    def test_run_simulation_bridge(self):
        # Create a mock valid game state
        game_state = {
            "down": 4,
            "yardline_100": 45,
            "ydstogo": 4,
            "game_seconds_remaining": 1200,
            "score_differential": -2,
            "roof": "outdoors",
            "surface": "grass",
            "wind": 6,
            "temp": 62,
            "spread_line": -1.5,
            "total_line": 45.5,
            "posteam_timeouts_remaining": 2,
            "defteam_timeouts_remaining": 2
        }
        
        # Call simulator subprocess bridge
        res = run_simulation(game_state)
        
        self.assertNotIn("error", res)
        self.assertIn("base_wp", res)
        self.assertIn("recommendation", res)
        self.assertEqual(res["recommendation"]["action"], "GO")


class TestPostingPolicy(unittest.TestCase):
    def setUp(self):
        self.db_fd, self.db_path = tempfile.mkstemp()
        init_db(self.db_path)

    def tearDown(self):
        os.close(self.db_fd)
        os.unlink(self.db_path)

    def test_should_post_gates(self):
        # Case A: A massive coaching mistake (should always post, bypassing obviousness/cooldown blocks)
        row = {
            "game_id": "123",
            "drive_id": "d_1",
            "play_id": "100",
            "wp_go": 0.45,
            "wp_punt": 0.15,
            "wp_fg": 0.10,
            "best_action": "go",
            "called_action": "punt",
            "off_score": 14,
            "def_score": 17,
            "qtr": 4,
            "ydstogo": 2,
            "yardline_100": 40
        }
        game_meta = {"is_standalone_prime": False}
        
        post, reason = should_post_decision(row, game_meta, self.db_path)
        self.assertTrue(post)
        self.assertEqual(reason, "clear_mistake")

    def test_post_formatter(self):
        row = {
            "game_id": "123",
            "drive_id": "d_1",
            "play_id": "100",
            "wp_go": 0.45,
            "wp_punt": 0.15,
            "wp_fg": 0.10,
            "best_action": "go",
            "called_action": "punt",
            "off": "KC",
            "def": "BAL",
            "off_score": 14,
            "def_score": 17,
            "qtr": 4,
            "clock": "2:30",
            "ydstogo": 2,
            "yardline_100": 40
        }
        
        post_str = format_post(row, revisionist=True)
        self.assertIn("INCORRECT", post_str)
        self.assertIn("Punt", post_str)
        self.assertIn("Go For It", post_str)
        self.assertIn("#nfl #4thDown #analytics", post_str)


if __name__ == "__main__":
    unittest.main()
