# src/live/state_store.py
# SQLite Database State Manager for 4th Down Bot.
# References: docs/database_guide.md
# ------------------------------------------------------------------------------

import os
import sqlite3
from datetime import datetime, timedelta
from typing import Optional

DEFAULT_DB_PATH = "data/live/bot_state.db"


def init_db(db_path: str = DEFAULT_DB_PATH) -> None:
    """
    Initializes the SQLite database file and creates tables if they do not exist.
    """
    db_dir = os.path.dirname(db_path)
    if db_dir:
        os.makedirs(db_dir, exist_ok=True)

    conn = sqlite3.connect(db_path)
    try:
        cursor = conn.cursor()
        
        # Deduplication table for seen plays
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS processed_plays (
                game_id TEXT,
                play_id TEXT,
                processed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                PRIMARY KEY (game_id, play_id)
            )
        """)
        
        # Log table for social postings
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS post_history (
                game_id TEXT,
                drive_id TEXT,
                play_id TEXT,
                posted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                reason TEXT,
                wp_gap REAL
            )
        """)
        conn.commit()
    finally:
        conn.close()


def is_play_processed(game_id: str, play_id: str, db_path: str = DEFAULT_DB_PATH) -> bool:
    """
    Checks if a play has already been processed to prevent duplicates.
    """
    conn = sqlite3.connect(db_path)
    try:
        cursor = conn.cursor()
        cursor.execute(
            "SELECT 1 FROM processed_plays WHERE game_id = ? AND play_id = ?",
            (game_id, play_id)
        )
        return cursor.fetchone() is not None
    finally:
        conn.close()


def mark_play_processed(game_id: str, play_id: str, db_path: str = DEFAULT_DB_PATH) -> None:
    """
    Inserts a play ID into the deduplication ledger.
    """
    conn = sqlite3.connect(db_path)
    try:
        cursor = conn.cursor()
        cursor.execute(
            "INSERT OR IGNORE INTO processed_plays (game_id, play_id) VALUES (?, ?)",
            (game_id, play_id)
        )
        conn.commit()
    finally:
        conn.close()


def log_post(game_id: str, drive_id: str, play_id: str, reason: str, wp_gap: Optional[float] = None, db_path: str = DEFAULT_DB_PATH) -> None:
    """
    Logs a successful post event for rate limit tracking.
    """
    conn = sqlite3.connect(db_path)
    try:
        cursor = conn.cursor()
        cursor.execute(
            "INSERT INTO post_history (game_id, drive_id, play_id, reason, wp_gap) VALUES (?, ?, ?, ?, ?)",
            (game_id, drive_id, play_id, reason, wp_gap)
        )
        conn.commit()
    finally:
        conn.close()


def is_game_in_cooldown(game_id: str, cooldown_seconds: int = 60, db_path: str = DEFAULT_DB_PATH) -> bool:
    """
    Checks if the game has had a post within the specified cooldown window.
    """
    conn = sqlite3.connect(db_path)
    try:
        cursor = conn.cursor()
        cutoff = (datetime.utcnow() - timedelta(seconds=cooldown_seconds)).strftime("%Y-%m-%d %H:%M:%S")
        cursor.execute(
            "SELECT 1 FROM post_history WHERE game_id = ? AND posted_at >= ? LIMIT 1",
            (game_id, cutoff)
        )
        return cursor.fetchone() is not None
    finally:
        conn.close()


def is_drive_posted(game_id: str, drive_id: str, db_path: str = DEFAULT_DB_PATH) -> bool:
    """
    Checks if the specified drive has already been posted to prevent consecutive posts on the same drive.
    """
    conn = sqlite3.connect(db_path)
    try:
        cursor = conn.cursor()
        cursor.execute(
            "SELECT 1 FROM post_history WHERE game_id = ? AND drive_id = ? LIMIT 1",
            (game_id, drive_id)
        )
        return cursor.fetchone() is not None
    finally:
        conn.close()


def get_global_posts_last_hour(hour_limit_minutes: int = 60, db_path: str = DEFAULT_DB_PATH) -> int:
    """
    Returns the count of global posts sent in the last hour.
    """
    conn = sqlite3.connect(db_path)
    try:
        cursor = conn.cursor()
        cutoff = (datetime.utcnow() - timedelta(minutes=hour_limit_minutes)).strftime("%Y-%m-%d %H:%M:%S")
        cursor.execute(
            "SELECT COUNT(*) FROM post_history WHERE posted_at >= ?",
            (cutoff,)
        )
        res = cursor.fetchone()
        return res[0] if res else 0
    finally:
        conn.close()
