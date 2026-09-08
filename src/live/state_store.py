# src/live/state_store.py
# SQLite Database State Manager for 4th Down Bot.
# References: docs/database_guide.md
# ------------------------------------------------------------------------------

import json
import os
import sqlite3
from datetime import datetime, timedelta
from typing import Any, Dict, Optional

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

        # Full audit trail: every evaluated 4th down, posted or not. Separate
        # from processed_plays (dedup-only, no payload) and post_history
        # (posted-only) -- this is the "review everything the bot decided"
        # table that feeds the /api/live-bot-feed testing surface.
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS evaluated_plays (
                game_id TEXT,
                play_id TEXT,
                drive_id TEXT,
                evaluated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                off TEXT,
                def TEXT,
                qtr INTEGER,
                clock TEXT,
                down INTEGER,
                ydstogo REAL,
                yardline_100 REAL,
                off_score INTEGER,
                def_score INTEGER,
                called_action TEXT,
                best_action TEXT,
                wp_go REAL,
                wp_punt REAL,
                wp_fg REAL,
                base_wp REAL,
                wp_gap REAL,
                should_post INTEGER,
                post_reason TEXT,
                post_text TEXT,
                raw_json TEXT
            )
        """)
        conn.commit()
    finally:
        conn.close()


def record_evaluated_play(
    row: Dict[str, Any],
    sim_res: Dict[str, Any],
    should_post: bool,
    reason: str,
    post_text: Optional[str] = None,
    db_path: str = DEFAULT_DB_PATH,
) -> None:
    """
    Persists a full audit record for every evaluated 4th down, regardless of
    whether it cleared the posting-policy gate. This is what lets a human
    review the bot's complete decision history after the fact, not just the
    plays it actually posted about.
    """
    conn = sqlite3.connect(db_path)
    try:
        cursor = conn.cursor()
        cursor.execute(
            """
            INSERT INTO evaluated_plays (
                game_id, play_id, drive_id, off, def, qtr, clock, down, ydstogo,
                yardline_100, off_score, def_score, called_action, best_action,
                wp_go, wp_punt, wp_fg, base_wp, wp_gap, should_post, post_reason,
                post_text, raw_json
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(row.get("game_id")),
                str(row.get("play_id")),
                str(row.get("drive_id") or ""),
                row.get("off"),
                row.get("def"),
                row.get("qtr"),
                row.get("clock"),
                row.get("down"),
                row.get("ydstogo"),
                row.get("yardline_100"),
                row.get("off_score"),
                row.get("def_score"),
                row.get("called_action"),
                row.get("best_action"),
                row.get("wp_go"),
                row.get("wp_punt"),
                row.get("wp_fg"),
                sim_res.get("base_wp"),
                row.get("wp_gap"),
                1 if should_post else 0,
                reason,
                post_text,
                json.dumps({**row, **{"sim_res": sim_res}}, default=str),
            ),
        )
        conn.commit()
    finally:
        conn.close()


def get_recent_evaluated_plays(limit: int = 100, db_path: str = DEFAULT_DB_PATH) -> list:
    """Returns the most recent evaluated plays, newest first, as a list of dicts."""
    conn = sqlite3.connect(db_path)
    try:
        conn.row_factory = sqlite3.Row
        cursor = conn.cursor()
        cursor.execute(
            "SELECT * FROM evaluated_plays ORDER BY evaluated_at DESC LIMIT ?",
            (limit,),
        )
        return [dict(r) for r in cursor.fetchall()]
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
