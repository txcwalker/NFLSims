"""File-backed store for "paper trade" entries -- a lineup flagged from the
optimizer or Lineup Lab as "I'm actually entering this," persisted so it can
be checked against the real result once the contest settles.

Lives inside the DFS ownership archive (data/dfs_ownership/), one JSON file
per slate, alongside that slate's salaries_prelock.csv / manifest.json /
standings CSVs -- one folder holding everything about a slate, ours included.
See data/dfs_ownership/README.md and scripts/dfs_ownership/score_paper_entries.py
(the offline job that reads these back once a standings CSV shows up).

Layout::

    data/dfs_ownership/<year>/week_<NN>/<slate_id>/paper_entries.json
        {"entries": [ {...}, {...}, ... ]}   -- append-only, oldest first

Entry shape (frontend-owned; this module treats it mostly opaquely, only
stamping entry_id/created_at):
    {
      "entry_id": "20260912T041500Z-a1b2c3d4",
      "created_at": "2026-09-12T04:15:00Z",
      "slate_format": "showdown" | "classic",
      "source": "optimize" | "lab",
      "label": str | None,
      "contest_name": str,      # must match the <name> a later
                                 # <name>_<price>_<xmax>max.csv will use
      "entry_fee": float,
      "max_entries": int,
      "players": [ {"slot": ..., "name": ..., "team": ..., "pos": ...}, ... ],
      "model": { ... the full lineup-result dict from /optimize(_showdown) ... },
    }

Single user, no auth, no database -- same atomic-write pattern as
optimizer_store.py / workspace_store.py.
"""
import datetime
import glob
import json
import os
import re
import secrets
import tempfile
from typing import Any, Dict, List, Optional

_REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
_ARCHIVE = os.path.join(_REPO_ROOT, "data", "dfs_ownership")

_KEY_RE = re.compile(r"[^A-Za-z0-9_-]")


def _safe_slate_id(slate_id: str) -> str:
    cleaned = _KEY_RE.sub("", str(slate_id or ""))[:120]
    return cleaned or "default"


def _entries_path(year: int, week: int, slate_id: str) -> str:
    return os.path.join(_ARCHIVE, str(int(year)), f"week_{int(week):02d}",
                        _safe_slate_id(slate_id), "paper_entries.json")


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


def _read_all(path: str) -> Dict[str, Any]:
    if not os.path.exists(path):
        return {"entries": []}
    try:
        with open(path, "r", encoding="utf-8") as f:
            doc = json.load(f)
        if not isinstance(doc, dict) or not isinstance(doc.get("entries"), list):
            return {"entries": []}
        return doc
    except (json.JSONDecodeError, OSError):
        # A corrupt file should not brick the page -- treat as empty rather
        # than 500ing every save/list call.
        return {"entries": []}


def list_entries(year: int, week: int, slate_id: str) -> List[Dict[str, Any]]:
    return _read_all(_entries_path(year, week, slate_id))["entries"]


def add_entry(year: int, week: int, slate_id: str, entry: Dict[str, Any]) -> Dict[str, Any]:
    """Append one entry, server-stamped with entry_id/created_at (client
    values, if any, are ignored -- keeps ids collision-free and orderable)."""
    path = _entries_path(year, week, slate_id)
    doc = _read_all(path)
    now = datetime.datetime.now(datetime.timezone.utc)
    record = {
        **entry,
        "entry_id": f"{now.strftime('%Y%m%dT%H%M%SZ')}-{secrets.token_hex(4)}",
        "created_at": now.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "year": int(year), "week": int(week), "slate_id": _safe_slate_id(slate_id),
    }
    doc["entries"].append(record)
    _atomic_write_json(path, doc)
    return record


def delete_entry(year: int, week: int, slate_id: str, entry_id: str) -> bool:
    path = _entries_path(year, week, slate_id)
    doc = _read_all(path)
    before = len(doc["entries"])
    doc["entries"] = [e for e in doc["entries"] if e.get("entry_id") != entry_id]
    if len(doc["entries"]) == before:
        return False
    _atomic_write_json(path, doc)
    return True


def update_entry(year: int, week: int, slate_id: str, entry_id: str, patch: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    """Update one entry's mutable annotation fields only -- notes (free text)
    and late_swap (bool, for "the lineup I actually played differs from what
    was optimized/registered because I swapped a player after a late
    inactive/news update"). Everything else about an entry (players/model/
    contest_name/account_id/...) is set once at creation and not editable
    here -- see the Bankroll account-detail view, which is what surfaces
    these two fields for editing."""
    path = _entries_path(year, week, slate_id)
    doc = _read_all(path)
    for e in doc["entries"]:
        if e.get("entry_id") == entry_id:
            for k in ("notes", "late_swap"):
                if k in patch:
                    e[k] = patch[k]
            _atomic_write_json(path, doc)
            return e
    return None


def clear_account_entries(account_id: str) -> int:
    """Deletes every entry tagged to `account_id`, across every
    year/week/slate in the archive -- the paper_entries.json half of the
    Bankroll "Clear account" action (see app.py's clear_bankroll_account,
    which also un-tags the Builds that produced them). Returns the count
    removed."""
    n = 0
    for path in glob.glob(os.path.join(_ARCHIVE, "*", "week_*", "*", "paper_entries.json")):
        doc = _read_all(path)
        before = len(doc["entries"])
        doc["entries"] = [e for e in doc["entries"] if e.get("account_id") != account_id]
        removed = before - len(doc["entries"])
        if removed:
            n += removed
            _atomic_write_json(path, doc)
    return n
