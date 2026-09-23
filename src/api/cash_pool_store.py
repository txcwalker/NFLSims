"""File-backed store for the Cash Lineups page's manual player-pool overrides.

Single user, no auth, no database -- just JSON on disk under
``data/cash_lineups/``, same pattern as optimizer_store.py.

Layout::

    data/cash_lineups/<season>/week_<NN>/pool.json

Inputs
    season : int        -- e.g. 2026
    week   : int         -- 1..22
    pool   : dict        -- {"excluded": [{"name", "team"}, ...],
                              "locked":   [{"name", "team"}, ...]}

Outputs
    read_pool  -> the saved {"excluded": [...], "locked": [...]}, or both
                  empty when nothing has been saved yet.
    write_pool -> persists atomically (temp file + os.replace).

Purpose
    _generate_cash_consensus_lineups() (app.py) treats every priced player as
    fair game for a "cash-optimal" build -- there's no notion of "this guy
    isn't actually startable regardless of value" (a rookie/backup the model
    likes on pure salary efficiency but who's a real-world dart throw) or
    "I want this guy in every build regardless of what the solver picks".
    This lets Cam hand-exclude or hand-lock specific players and have the
    choice stick across reloads/week revisits, instead of re-picking them
    every time -- mirrors the Optimizer's per-player lock/exclude, minus
    everything else that page tracks (ownership, exposure, contest settings).
"""
import json
import os
import tempfile
from typing import Any, Dict, List

_REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
_BASE = os.path.join(_REPO_ROOT, "data", "cash_lineups")

_EMPTY_POOL = {"excluded": [], "locked": []}


def _week_dir(season: int, week: int) -> str:
    return os.path.join(_BASE, str(int(season)), f"week_{int(week):02d}")


def _pool_path(season: int, week: int) -> str:
    return os.path.join(_week_dir(season, week), "pool.json")


def _atomic_write_json(path: str, data: Any) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    fd, tmp = tempfile.mkstemp(dir=os.path.dirname(path), suffix=".tmp")
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)
        os.replace(tmp, path)
    except Exception:
        try:
            os.unlink(tmp)
        except OSError:
            pass
        raise


def _clean_list(items: List[Dict[str, str]]) -> List[Dict[str, str]]:
    return [
        {"name": e.get("name"), "team": e.get("team")}
        for e in (items or [])
        if e.get("name") and e.get("team")
    ]


def read_pool(season: int, week: int) -> Dict[str, List[Dict[str, str]]]:
    """Returns the saved {"excluded": [...], "locked": [...]} for a week, or
    both empty if nothing has been saved yet."""
    path = _pool_path(season, week)
    if not os.path.exists(path):
        return {"excluded": [], "locked": []}
    try:
        with open(path, "r", encoding="utf-8") as f:
            data = json.load(f)
            if not isinstance(data, dict):
                return {"excluded": [], "locked": []}
            return {
                "excluded": _clean_list(data.get("excluded")),
                "locked": _clean_list(data.get("locked")),
            }
    except (json.JSONDecodeError, OSError):
        return {"excluded": [], "locked": []}


def write_pool(
    season: int, week: int, excluded: List[Dict[str, str]], locked: List[Dict[str, str]]
) -> Dict[str, List[Dict[str, str]]]:
    """Persists the exclude/lock lists for a week. Returns them cleaned, for
    the endpoint echo."""
    pool = {"excluded": _clean_list(excluded), "locked": _clean_list(locked)}
    _atomic_write_json(_pool_path(season, week), pool)
    return pool
