"""File-backed store for the DFS Shortlist: lineups the user hand-picks
from the Optimizer(s) during the week and later assembles into a final set.

Layout (one file per week per contest format)::

    data/optimizer/<season>/week_<NN>/shortlist_<format>.json
        { "format": "classic", "lineups": [ { entry_id, saved_at, source_build_id,
                                                note, lineup: {...optimizer lineup...} } ] }

Inputs
    season : int   -- e.g. 2026
    week   : int   -- 1..22
    fmt    : str   -- "classic" | "showdown" (showdown wired in a later pass)
    lineups: list  -- optimizer lineup dicts (players[], ev_pct, itm_pct, histogram, ...)

Outputs
    read_shortlist / add_lineups / remove_lineups / clear_shortlist -> the full
    shortlist dict ``{format, lineups[]}`` (add_lineups also reports added/duplicates).

Purpose
    Saved lineups are a human selection, not regenerable output, so they live next to
    the other tracked optimizer state and are written atomically. Ownership is
    deliberately NOT refreshed here: the frontend overlays LIVE ownership at view time
    while each lineup keeps the sim-derived numbers (EV%, ITM%, histogram) it was saved with.
"""
import datetime
import os
import re
import uuid
from typing import Any, Dict, List

from src.api.optimizer_store import _atomic_write_json, _load_build_file, _week_dir

_FORMATS = ("classic", "showdown")
_ID_RE = re.compile(r"[^A-Za-z0-9_\-]")


def _path(season: int, week: int, fmt: str) -> str:
    if fmt not in _FORMATS:
        raise ValueError(f"unknown shortlist format {fmt!r}")
    return os.path.join(_week_dir(season, week), f"shortlist_{fmt}.json")


def _utc_now_iso() -> str:
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def lineup_key(lineup: Dict[str, Any]) -> str:
    """Identity of a lineup = its set of (slot, name, team) -- two saves of the same
    nine players in the same slots are duplicates regardless of which build made them.
    Showdown lineups differ by CPT vs FLEX, which the slot captures."""
    parts = sorted(
        f"{p.get('slot') or p.get('pos')}|{p.get('name')}|{p.get('team')}"
        for p in (lineup.get("players") or [])
    )
    return ";".join(parts)


def read_shortlist(season: int, week: int, fmt: str) -> Dict[str, Any]:
    data = _load_build_file(_path(season, week, fmt)) if os.path.exists(_path(season, week, fmt)) else None
    data = data or {}
    return {"format": fmt, "lineups": data.get("lineups") or []}


def add_lineups(season: int, week: int, fmt: str, lineups: List[Dict[str, Any]],
                source_build_id: str = None) -> Dict[str, Any]:
    """Append lineups, skipping any already in the shortlist (same player/slot set)."""
    port = read_shortlist(season, week, fmt)
    have = {lineup_key(e["lineup"]) for e in port["lineups"]}
    added, dupes = 0, 0
    for lu in lineups:
        k = lineup_key(lu)
        if k in have:
            dupes += 1
            continue
        have.add(k)
        port["lineups"].append({
            "entry_id": uuid.uuid4().hex[:12],
            "saved_at": _utc_now_iso(),
            "source_build_id": source_build_id,
            "note": "",
            "lineup": lu,
        })
        added += 1
    _atomic_write_json(_path(season, week, fmt), port)
    return {**port, "added": added, "duplicates": dupes}


def remove_lineups(season: int, week: int, fmt: str, entry_ids: List[str]) -> Dict[str, Any]:
    drop = {_ID_RE.sub("", str(i)) for i in entry_ids}
    port = read_shortlist(season, week, fmt)
    port["lineups"] = [e for e in port["lineups"] if e.get("entry_id") not in drop]
    _atomic_write_json(_path(season, week, fmt), port)
    return port


def clear_shortlist(season: int, week: int, fmt: str) -> Dict[str, Any]:
    port = {"format": fmt, "lineups": []}
    _atomic_write_json(_path(season, week, fmt), port)
    return port
