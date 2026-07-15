# 📊 NFL 4th Down Bot: Database Schema & Access Guide

This document acts as the definitive schema reference and access guide for the local SQLite database used by the NFL 4th Down Bot.

---

## 🔍 Overview
- **File Path**: `data/live/bot_state.db`
- **Engine**: SQLite 3 (serverless, self-contained, stored as a single local file)
- **Purpose**: Provides thread-safe play deduplication, rate limit tracking, and temporal posting logs.

---

## 🗄️ Database Schema

### 1. `processed_plays` Table
This table acts as a high-performance deduplication log. It prevents the bot from evaluating or posting the same play multiple times.

| Column | Type | Constraints | Description |
| :--- | :--- | :--- | :--- |
| `game_id` | `TEXT` | `PRIMARY KEY` (Part 1) | The unique identifier of the game (e.g. ESPN Event ID). |
| `play_id` | `TEXT` | `PRIMARY KEY` (Part 2) | The unique play identifier within that game. |
| `processed_at` | `TIMESTAMP` | `DEFAULT CURRENT_TIMESTAMP` | The exact UTC timestamp when the play was evaluated. |

*SQL Definition:*
```sql
CREATE TABLE IF NOT EXISTS processed_plays (
    game_id TEXT,
    play_id TEXT,
    processed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (game_id, play_id)
);
```

---

### 2. `post_history` Table
This table logs all social media postings made by the bot. It is used to calculate rate limits and cooldown windows (e.g., game-level and global hourly limits).

| Column | Type | Constraints | Description |
| :--- | :--- | :--- | :--- |
| `game_id` | `TEXT` | Not Null | The unique identifier of the game. |
| `drive_id` | `TEXT` | Not Null | The unique identifier of the game drive. |
| `play_id` | `TEXT` | Not Null | The unique play identifier that triggered the post. |
| `posted_at` | `TIMESTAMP` | `DEFAULT CURRENT_TIMESTAMP` | The exact UTC timestamp when the post was successfully sent. |
| `reason` | `TEXT` | Not Null | The posting policy gate matched (e.g., `clear_mistake`, `one_score_2H`, `prime_all`). |
| `wp_gap` | `REAL` | Optional | The Win Probability difference between the optimal and called actions (if a mistake). |

*SQL Definition:*
```sql
CREATE TABLE IF NOT EXISTS post_history (
    game_id TEXT,
    drive_id TEXT,
    play_id TEXT,
    posted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    reason TEXT,
    wp_gap REAL
);
```

---

## 🛠️ Programmatic Access Guide (Python)

SQLite is built directly into Python's standard library. Use the `sqlite3` module to interact with the database.

### 1. Writing to the DB (Thread-Safe Transaction)
```python
import sqlite3

def log_processed_play(game_id: str, play_id: str, db_path: str = "data/live/bot_state.db"):
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
```

### 2. Querying Cooldown Status (Read-Only)
```python
import sqlite3
from datetime import datetime, timedelta

def check_game_cooldown(game_id: str, db_path: str = "data/live/bot_state.db") -> bool:
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()
    
    # Query posts within the last 60 seconds
    one_minute_ago = (datetime.utcnow() - timedelta(seconds=60)).strftime("%Y-%m-%d %H:%M:%S")
    cursor.execute(
        "SELECT COUNT(*) FROM post_history WHERE game_id = ? AND posted_at >= ?",
        (game_id, one_minute_ago)
    )
    post_count = cursor.fetchone()[0]
    conn.close()
    
    return post_count > 0  # True if in cooldown
```

---

## 👥 Human Access Guide (How to View the Logs)

If you want to open and view the logs inside the database file directly on your computer:

### Option A: Using VS Code (Visual Extension)
1. Search the VS Code extensions marketplace for **"SQLite Viewer"** (by Florian Klampfer).
2. Install the extension.
3. In your file explorer, simply click on `data/live/bot_state.db`.
4. VS Code will open the database as a clean, interactive excel-style table.

### Option B: Free GUI Tool (DB Browser for SQLite)
1. Download [DB Browser for SQLite](https://sqlitebrowser.org/) (a free, lightweight open-source tool).
2. Install and launch the application.
3. Click **Open Database** and navigate to your `NFL_Exploration/data/live/bot_state.db` file.
4. Go to the **Browse Data** tab to view your log tables, search, or filter plays.
