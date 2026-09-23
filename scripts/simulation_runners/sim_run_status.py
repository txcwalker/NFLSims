"""Run-status plumbing shared by the DFS-week sim writers and the API.

Why (2026-09-23): the Game Explorer / Showdown pages should pick up a new
10K run automatically as soon as it finishes. That needs two things the
writers didn't provide:
  1. A "run in progress" signal, so the site can say "sims running" instead
     of silently serving the previous run with no hint a new one is coming.
  2. Atomic parquet writes, so the API (which re-reads the parquet whenever
     its mtime changes) can never read a half-written file mid-save.

Writers: run_week_sim_2026.simulate_week() and resim_games_2026.resim_games()
wrap their work in `run_marker(week, ...)` and save via `atomic_to_parquet`.
Reader: app.py's GET /api/sim_status calls `read_run_marker(week)`.

Marker file: data/interim/dfs_week_{week}_running.json
    {"week": 3, "started_at": <unix s>, "iterations": 10000, "games": ["2026_03_ATL_GB", ...] | null,
     "pid": 1234}
Removed when the run ends (success or failure). A marker older than
STALE_AFTER_S is treated as abandoned (e.g. a killed process).
"""
import json
import os
import time
from contextlib import contextmanager

STALE_AFTER_S = 6 * 3600


def marker_path(week, base_dir="."):
    """Inputs: week (int), base_dir (repo root). Output: str marker path."""
    return os.path.join(base_dir, "data", "interim", f"dfs_week_{week}_running.json")


@contextmanager
def run_marker(week, iterations=None, games=None, base_dir="."):
    """Context manager: writes the in-progress marker on entry, always removes
    it on exit (normal return or exception).
    Inputs: week (int), iterations (int|None), games (list[str]|None = whole
    week), base_dir (repo root)."""
    path = marker_path(week, base_dir)
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        json.dump({"week": int(week), "started_at": time.time(), "iterations": iterations,
                   "games": games, "pid": os.getpid()}, f)
    try:
        yield
    finally:
        try:
            os.remove(path)
        except OSError:
            pass


def read_run_marker(week, base_dir="."):
    """Inputs: week (int), base_dir (repo root).
    Output: the marker dict if a run is in progress (and not stale), else None."""
    path = marker_path(week, base_dir)
    if not os.path.exists(path):
        return None
    try:
        with open(path, encoding="utf-8") as f:
            m = json.load(f)
    except (OSError, ValueError):
        return None
    if time.time() - float(m.get("started_at", 0)) > STALE_AFTER_S:
        return None
    return m


def atomic_to_parquet(df, path):
    """Inputs: df (pandas DataFrame), path (str destination).
    Output: none. Writes to `<path>.tmp` then os.replace()s it into place, so
    a concurrent reader sees either the old complete file or the new one."""
    tmp = path + ".tmp"
    df.to_parquet(tmp, index=False)
    os.replace(tmp, path)
