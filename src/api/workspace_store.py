"""File-backed store for DFS "save slots" -- 3 switchable, autosaved workspace
snapshots per (season, week, slate_key), shared by the classic and showdown
optimizers so a pool build / lineup set / Game-Read scenario survives a page
or tab switch, and Cam can keep 2-3 different takes on the same slate side by
side (chalk build / contrarian build / stack build) -- like save files in a
game, not an ever-growing autosave history (that's optimizer_store.py's
"builds", which this is additive to, not a replacement for).

Single user, no auth, no database -- just JSON on disk under
``data/workspace/``, same atomic-write pattern as optimizer_store.py.

Layout::

    data/workspace/<season>/week_<NN>/<slate_key>/active.json   {"active": 1}
    data/workspace/<season>/week_<NN>/<slate_key>/slot_<N>.json  {label, updated_at, data}

``slate_key`` distinguishes contexts sharing a (season, week) -- a showdown
game (``showdown_<game_id>``) or a classic slate (``classic_<platform>_<draft_group_id>``).
Sanitized to a safe path segment here; the actual key semantics live in the
frontend, this module treats it as an opaque string.

``data`` (per slot) is a frontend-shaped opaque blob -- untouched here, same
spirit as optimizer_store.read_state/write_state.
"""
import datetime
import json
import os
import re
import tempfile
from typing import Any, Dict, List, Optional

N_SLOTS = 3

# ``__file__`` is ``<repo>/src/api/workspace_store.py`` -> three dirnames = repo root.
_REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
_BASE = os.path.join(_REPO_ROOT, "data", "workspace")

_KEY_RE = re.compile(r"[^A-Za-z0-9_-]")


def _safe_key(slate_key: str) -> str:
    """Strip anything that isn't a plain path-segment char. A key that comes
    back empty (all-punctuation input) falls back to "default" rather than
    resolving to the week directory itself."""
    cleaned = _KEY_RE.sub("", str(slate_key or ""))[:120]
    return cleaned or "default"


def _slate_dir(season: int, week: int, slate_key: str) -> str:
    return os.path.join(_BASE, str(int(season)), f"week_{int(week):02d}", _safe_key(slate_key))


def _active_path(season: int, week: int, slate_key: str) -> str:
    return os.path.join(_slate_dir(season, week, slate_key), "active.json")


def _slot_path(season: int, week: int, slate_key: str, slot: int) -> str:
    return os.path.join(_slate_dir(season, week, slate_key), f"slot_{int(slot)}.json")


def _atomic_write_json(path: str, data: Any) -> None:
    """Write ``data`` as pretty JSON to ``path`` atomically (temp file +
    ``os.replace``, atomic on both POSIX and Windows) so a reader never sees
    a half-written file."""
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


def _read_json(path: str) -> Optional[Dict[str, Any]]:
    if not os.path.exists(path):
        return None
    try:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)
    except (json.JSONDecodeError, OSError):
        return None


def _utc_now_iso() -> str:
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def _default_label(slot: int) -> str:
    return f"Slot {slot}"


def get_active(season: int, week: int, slate_key: str) -> int:
    """Which slot (1..N_SLOTS) is current for this context. Defaults to 1 --
    a brand-new slate/week starts on slot 1 rather than needing an explicit
    pick."""
    doc = _read_json(_active_path(season, week, slate_key))
    slot = (doc or {}).get("active", 1)
    return slot if isinstance(slot, int) and 1 <= slot <= N_SLOTS else 1


def set_active(season: int, week: int, slate_key: str, slot: int) -> Dict[str, Any]:
    if not (1 <= slot <= N_SLOTS):
        raise ValueError(f"slot must be 1..{N_SLOTS}")
    _atomic_write_json(_active_path(season, week, slate_key), {"active": slot})
    return {"active": slot}


def list_slots(season: int, week: int, slate_key: str) -> Dict[str, Any]:
    """{"active": N, "slots": [{slot, label, updated_at, has_data}, ...]} --
    metadata only (no ``data`` payload), for a lightweight slot-switcher UI
    that doesn't need to fetch all three full blobs just to render tabs."""
    active = get_active(season, week, slate_key)
    slots = []
    for slot in range(1, N_SLOTS + 1):
        doc = _read_json(_slot_path(season, week, slate_key, slot))
        slots.append({
            "slot": slot,
            "label": (doc or {}).get("label") or _default_label(slot),
            "updated_at": (doc or {}).get("updated_at"),
            "has_data": doc is not None and doc.get("data") not in (None, {}),
        })
    return {"active": active, "slots": slots}


def read_slot(season: int, week: int, slate_key: str, slot: int) -> Dict[str, Any]:
    """The full saved blob for one slot, or an empty shell if never saved."""
    doc = _read_json(_slot_path(season, week, slate_key, slot))
    if doc is None:
        return {"slot": slot, "label": _default_label(slot), "updated_at": None, "data": {}}
    return {"slot": slot, "label": doc.get("label") or _default_label(slot),
            "updated_at": doc.get("updated_at"), "data": doc.get("data") or {}}


def write_slot(season: int, week: int, slate_key: str, slot: int,
               data: Any, label: Optional[str] = None) -> Dict[str, Any]:
    """Persist ``data`` into one slot (autosave target). Keeps the slot's
    existing label unless a new one is given, so a plain autosave tick
    doesn't clobber a name Cam typed in."""
    if not (1 <= slot <= N_SLOTS):
        raise ValueError(f"slot must be 1..{N_SLOTS}")
    existing = _read_json(_slot_path(season, week, slate_key, slot)) or {}
    record = {
        "label": label if label is not None else (existing.get("label") or _default_label(slot)),
        "updated_at": _utc_now_iso(),
        "data": data,
    }
    _atomic_write_json(_slot_path(season, week, slate_key, slot), record)
    return record


def rename_slot(season: int, week: int, slate_key: str, slot: int, label: str) -> Dict[str, Any]:
    existing = _read_json(_slot_path(season, week, slate_key, slot)) or {"data": {}}
    existing["label"] = label or _default_label(slot)
    existing["updated_at"] = existing.get("updated_at") or _utc_now_iso()
    _atomic_write_json(_slot_path(season, week, slate_key, slot), existing)
    return existing


def clear_slot(season: int, week: int, slate_key: str, slot: int) -> Dict[str, Any]:
    """"Start fresh": wipe a slot's data but keep its label (so a renamed
    slot stays named after you empty it)."""
    existing = _read_json(_slot_path(season, week, slate_key, slot)) or {}
    record = {"label": existing.get("label") or _default_label(slot),
              "updated_at": _utc_now_iso(), "data": {}}
    _atomic_write_json(_slot_path(season, week, slate_key, slot), record)
    return record
