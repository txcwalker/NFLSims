"""File-backed store for the DFS Optimizer's per-week working state (Phase 2) and,
later, saved lineup "builds" (Phase 3).

Single user, no auth, no database -- just JSON on disk under ``data/optimizer/``.
See ``docs/implementation_plans/optimizer_persistence_plan.md`` for the full design.

Layout::

    data/optimizer/<season>/week_<NN>/state.json      working state
    data/optimizer/<season>/week_<NN>/builds/*.json   (Phase 3) saved builds

Inputs
    season : int   -- e.g. 2026
    week   : int   -- 1..22
    state  : dict  -- frontend-shaped working state (settings + overlay + slate +
                      prefs). Treated as an opaque blob here; the schema lives in
                      the frontend and the plan doc, not in this module.

Outputs
    read_state  -> the saved dict, or ``{}`` when nothing has been saved yet.
    write_state -> persists ``state`` atomically (temp file + ``os.replace``) so a
                   crash mid-write can never leave a half-written ``state.json``.

Purpose
    Lets the Optimizer reload a partly-finished build session later in the week,
    and (Phase 3+) keep every generated lineup set for end-of-season review.
"""
import datetime
import json
import os
import re
import tempfile
from typing import Any, Dict, List, Optional

# ``__file__`` is ``<repo>/src/api/optimizer_store.py`` -> three dirnames = repo root.
_REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
_BASE = os.path.join(_REPO_ROOT, "data", "optimizer")


def _week_dir(season: int, week: int) -> str:
    """Absolute path to a given season/week folder (not created here)."""
    return os.path.join(_BASE, str(int(season)), f"week_{int(week):02d}")


def _state_path(season: int, week: int) -> str:
    return os.path.join(_week_dir(season, week), "state.json")


def _atomic_write_json(path: str, data: Any) -> None:
    """Write ``data`` as pretty JSON to ``path`` atomically.

    Writes to a sibling temp file first, fsync-free (single-user local tool), then
    ``os.replace`` -- which is atomic on both POSIX and Windows -- so readers only
    ever see a complete file.
    """
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


def read_state(season: int, week: int) -> Dict[str, Any]:
    """Return the saved working state for a week, or ``{}`` if there is none."""
    path = _state_path(season, week)
    if not os.path.exists(path):
        return {}
    try:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)
    except (json.JSONDecodeError, OSError):
        # A corrupt state file should not brick the Optimizer -- start fresh.
        return {}


def write_state(season: int, week: int, state: Dict[str, Any]) -> Dict[str, Any]:
    """Persist ``state`` for a week. Returns it unchanged for the endpoint echo."""
    _atomic_write_json(_state_path(season, week), state)
    return state


# -------------------------------------------------------------------------
# Builds (Phase 3) -- one immutable JSON per Optimize run, under builds/
# -------------------------------------------------------------------------
_ID_RE = re.compile(r"[^A-Za-z0-9T_-]")


def _builds_dir(season: int, week: int) -> str:
    return os.path.join(_week_dir(season, week), "builds")


def _safe_id(build_id: str) -> str:
    """Strip anything that isn't a plain id char -- build ids are timestamps like
    ``20260921T144233Z``, so this only ever removes path-traversal attempts."""
    return _ID_RE.sub("", str(build_id or ""))


def _new_build_id() -> str:
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y%m%dT%H%M%SZ")


def _utc_now_iso() -> str:
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def _build_summary(b: Dict[str, Any]) -> Dict[str, Any]:
    """Lightweight row for the Builds panel list (no lineups/players_used)."""
    portfolio = b.get("portfolio") or {}
    contest = (b.get("settings") or {}).get("contest") or {}
    return {
        "build_id": b.get("build_id"),
        "created_at": b.get("created_at"),
        "source": b.get("source"),
        "label": b.get("label"),
        "pinned": bool(b.get("pinned")),
        "submitted": bool(b.get("submitted")),
        "n_lineups": len(b.get("lineups") or []),
        "portfolio_ev": portfolio.get("total_ev_pct"),
        "contest_name": contest.get("name"),
        "inputs_hash": b.get("inputs_hash"),
    }


def _build_path(season: int, week: int, build_id: str) -> str:
    return os.path.join(_builds_dir(season, week), f"{_safe_id(build_id)}.json")


def _load_build_file(path: str) -> Optional[Dict[str, Any]]:
    try:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)
    except (json.JSONDecodeError, OSError):
        return None


def list_builds(season: int, week: int) -> List[Dict[str, Any]]:
    """Build summaries for a week, newest first."""
    d = _builds_dir(season, week)
    if not os.path.isdir(d):
        return []
    out = []
    for fn in os.listdir(d):
        if not fn.endswith(".json"):
            continue
        b = _load_build_file(os.path.join(d, fn))
        if b is not None:
            out.append(_build_summary(b))
    out.sort(key=lambda b: b.get("created_at") or "", reverse=True)
    return out


def read_build(season: int, week: int, build_id: str) -> Optional[Dict[str, Any]]:
    path = _build_path(season, week, build_id)
    return _load_build_file(path) if os.path.exists(path) else None


def write_build(season: int, week: int, build: Dict[str, Any]) -> Dict[str, Any]:
    """Persist a build. Server owns build_id / created_at / season / week."""
    bid = _safe_id(build.get("build_id") or "") or _new_build_id()
    record = {
        **build,
        "build_id": bid,
        "created_at": build.get("created_at") or _utc_now_iso(),
        "season": int(season),
        "week": int(week),
    }
    _atomic_write_json(_build_path(season, week, bid), record)
    return record


def patch_build(season: int, week: int, build_id: str, patch: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    """Update the mutable metadata of a build (label / pinned / submitted / submission)."""
    b = read_build(season, week, build_id)
    if b is None:
        return None
    for k in ("label", "pinned", "submitted", "submission"):
        if k in patch:
            b[k] = patch[k]
    _atomic_write_json(_build_path(season, week, build_id), b)
    return b


def delete_build(season: int, week: int, build_id: str) -> bool:
    path = _build_path(season, week, build_id)
    if os.path.exists(path):
        os.unlink(path)
        return True
    return False


def prune_builds(season: int, week: int) -> List[Dict[str, Any]]:
    """Delete autosave builds that are not pinned, labeled or submitted. Returns
    summaries of what was removed."""
    d = _builds_dir(season, week)
    removed: List[Dict[str, Any]] = []
    if not os.path.isdir(d):
        return removed
    for fn in os.listdir(d):
        if not fn.endswith(".json"):
            continue
        path = os.path.join(d, fn)
        b = _load_build_file(path)
        if b is None:
            continue
        if b.get("source") == "autosave" and not b.get("pinned") and not b.get("label") and not b.get("submitted"):
            os.unlink(path)
            removed.append(_build_summary(b))
    return removed
