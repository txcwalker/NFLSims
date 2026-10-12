"""Week freeze: immutable snapshots of what the optimizer showed for a week.

Inputs:  the JSON payloads served by /api/week_projections (player pool +
         projections + salaries + ownership) and /api/week_sim_results
         (per-game sim results), plus data/external/schedule_{year}.csv for the
         lock time.
Outputs: data/frozen/{year}/week_{NN}/{projections,sim_results}.json and a
         meta.json (frozen_at, source, lock_ts, draft_group_id).
Purpose: before this, a past week's pool was rebuilt from the CURRENT roster
         files every time they changed, so a later signing/injury changed what
         Week 2 looked like. Once the week's main slate kicks off, the payloads
         are written here once and served verbatim ever after.

Lock time = first kickoff of the main slate: the earliest Sunday game at or
after 13:00 ET (falls back to the week's earliest game if none). Thursday /
Monday / international-morning games are not the main slate.

Never overwritten unless force=True. data/frozen/ is meant to be committed --
a snapshot cannot be regenerated once the underlying inputs move on.

PER-GAME FREEZE (2026-10-12): the whole-week snapshot above froze Monday's
and Thursday's games at the Sunday lock too, so a roster change before MNF
never reached the site, while forcing a re-freeze after kickoff overwrote
played games with degraded live data (optimal_pct -> 0). `serve()` replaces it:
each game's entries track the live payload until THAT game's kickoff, then
are never touched again. Only week-wide fields (the cash consensus lineups)
still freeze at the main-slate lock. Snapshots written before this change
carry no `per_game` flag in meta.json and are served verbatim, as before.
"""
import json
import os
import time
from collections import defaultdict
from typing import Any, Callable, Dict, List, Optional, Set

import pandas as pd

BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
KINDS = ("projections", "sim_results")
_SCHEDULE_TZ = "America/New_York"

# (path, mtime) -> parsed payload, so serving a frozen week doesn't re-parse
# a multi-MB JSON on every request.
_READ_CACHE: Dict[str, Any] = {}


def week_dir(year: int, week: int) -> str:
    """Inputs: year, week (int). Output: snapshot folder path (str)."""
    return os.path.join(BASE_DIR, "data", "frozen", str(year), f"week_{int(week):02d}")


def _path(year: int, week: int, kind: str) -> str:
    return os.path.join(week_dir(year, week), f"{kind}.json")


def lock_ts(week: int, year: int, schedule_csv: str) -> Optional[float]:
    """Inputs: week, year, schedule_csv (path).
    Output: unix seconds of the main slate's first kickoff, or None if the
    schedule can't answer (callers then treat the week as NOT locked, so bad
    data can never freeze a week early)."""
    if not os.path.exists(schedule_csv):
        return None
    df = pd.read_csv(schedule_csv)
    df = df[(df["season"] == year) & (df["week"] == week) & (df["game_type"] == "REG")]
    if df.empty:
        return None
    main = df[(df["weekday"] == "Sunday") & (df["gametime"].astype(str) >= "13:00")]
    pool = main if not main.empty else df
    stamps = []
    for _, r in pool.iterrows():
        try:
            stamps.append(pd.Timestamp(f"{r['gameday']} {r['gametime']}").tz_localize(_SCHEDULE_TZ).timestamp())
        except (ValueError, TypeError):
            continue
    return min(stamps) if stamps else None


def is_locked(week: int, year: int, schedule_csv: str, now: Optional[float] = None) -> bool:
    """True once the main slate's first kickoff has passed."""
    ts = lock_ts(week, year, schedule_csv)
    return ts is not None and (time.time() if now is None else now) >= ts


def load(year: int, week: int, kind: str) -> Optional[Dict[str, Any]]:
    """Inputs: year, week, kind ('projections' | 'sim_results').
    Output: the frozen payload (dict) or None if this week/kind isn't frozen."""
    p = _path(year, week, kind)
    if not os.path.exists(p):
        return None
    m = os.path.getmtime(p)
    hit = _READ_CACHE.get(p)
    if hit and hit[0] == m:
        return hit[1]
    with open(p, "r", encoding="utf-8") as f:
        data = json.load(f)
    _READ_CACHE[p] = (m, data)
    return data


def load_meta(year: int, week: int) -> Optional[Dict[str, Any]]:
    """Output: meta.json contents (dict) or None."""
    p = os.path.join(week_dir(year, week), "meta.json")
    if not os.path.exists(p):
        return None
    with open(p, "r", encoding="utf-8") as f:
        return json.load(f)


def save(year: int, week: int, kind: str, payload: Dict[str, Any], *, source: str,
         lock: Optional[float] = None, draft_group_id: Optional[int] = None,
         force: bool = False, per_game: bool = False) -> bool:
    """Inputs: payload (the endpoint's response dict), source ('live' = frozen
    automatically after lock, 'backfill' = reconstructed from current files
    after the fact), lock (unix s), draft_group_id, force (overwrite).
    Output: True if written, False if a snapshot already existed (left alone).
    Purpose: atomic write (tmp + os.replace) so a reader never sees half a
    file; meta.json records per-kind provenance."""
    p = _path(year, week, kind)
    if os.path.exists(p) and not force:
        return False
    os.makedirs(week_dir(year, week), exist_ok=True)
    tmp = p + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        json.dump(payload, f)
    os.replace(tmp, p)
    _READ_CACHE.pop(p, None)

    meta = load_meta(year, week) or {"year": year, "week": week}
    meta.setdefault("kinds", {})[kind] = {"frozen_at": time.time(), "source": source}
    if per_game:
        meta["kinds"][kind]["per_game"] = True
    if lock is not None:
        meta["lock_ts"] = lock
    if draft_group_id is not None:
        meta["draft_group_id"] = draft_group_id
    mp = os.path.join(week_dir(year, week), "meta.json")
    with open(mp + ".tmp", "w", encoding="utf-8") as f:
        json.dump(meta, f, indent=2)
    os.replace(mp + ".tmp", mp)
    return True


# ---------------------------------------------------------------------------
# Per-game freeze
# ---------------------------------------------------------------------------

def kickoff_map(week: int, year: int, schedule_csv: str) -> Dict[str, float]:
    """Inputs: week, year, schedule_csv (path; kickoffs are Eastern).
    Output: {game_id: kickoff unix seconds} for the week's REG games with a
    parseable kickoff ({} if the schedule can't answer)."""
    if not os.path.exists(schedule_csv):
        return {}
    df = pd.read_csv(schedule_csv)
    df = df[(df["season"] == year) & (df["week"] == week) & (df["game_type"] == "REG")]
    out: Dict[str, float] = {}
    for _, r in df.iterrows():
        try:
            out[str(r["game_id"])] = pd.Timestamp(f"{r['gameday']} {r['gametime']}").tz_localize(_SCHEDULE_TZ).timestamp()
        except (ValueError, TypeError):
            continue
    return out


def _is_per_game(year: int, week: int, kind: str) -> bool:
    meta = load_meta(year, week) or {}
    return bool(((meta.get("kinds") or {}).get(kind) or {}).get("per_game"))


def _game_ids_in(kind: str, payload: Optional[Dict[str, Any]]) -> Set[str]:
    """Game ids present in a payload (projections: the games list keyed by
    game_id; sim_results: the games dict's keys)."""
    if not payload:
        return set()
    if kind == "projections":
        return {e.get("game_id") for e in (payload.get("games") or []) if e.get("game_id")}
    return set((payload.get("games") or {}).keys())


def _has_content(kind: str, payload: Optional[Dict[str, Any]]) -> bool:
    if not payload:
        return False
    return bool(payload.get("players" if kind == "projections" else "games"))


def _merge_list(live_list: List[dict], stored_list: List[dict], keep: Set[str]) -> List[dict]:
    """Replace, in place, every live entry of a game in `keep` with that game's
    stored entries (the first live occurrence marks the position); stored
    entries of kept games the live list no longer has at all go on the end."""
    stored_by: Dict[str, List[dict]] = defaultdict(list)
    for e in stored_list:
        stored_by[e.get("game_id")].append(e)
    out: List[dict] = []
    placed: Set[str] = set()
    for e in live_list:
        g = e.get("game_id")
        if g in keep and g in stored_by:
            if g not in placed:
                out.extend(stored_by[g])
                placed.add(g)
            continue
        out.append(e)
    for g in keep:
        if g in stored_by and g not in placed:
            out.extend(stored_by[g])
    return out


def merge_per_game(kind: str, stored: Optional[Dict[str, Any]], live: Optional[Dict[str, Any]],
                   kicked: Set[str], locked: bool) -> Optional[Dict[str, Any]]:
    """Inputs: kind, stored (last persisted payload or None), live (fresh payload),
    kicked (game ids already kicked off), locked (main slate has locked).
    Output: the live payload where every kicked-off game that the stored payload
    holds is replaced by its stored entries (a kicked-off game with no stored
    entries takes the live ones -- nothing better exists). The week-wide
    `cash_consensus_lineups` keeps the stored copy once `locked`.
    Purpose: 'frozen at kickoff' per game instead of per week."""
    if not _has_content(kind, live):
        return stored if stored else live
    if not stored:
        return live
    out = dict(live)
    keep = {g for g in kicked if g in _game_ids_in(kind, stored)}
    if kind == "projections":
        out["players"] = _merge_list(live.get("players") or [], stored.get("players") or [], keep)
        out["games"] = _merge_list(live.get("games") or [], stored.get("games") or [], keep)
    else:
        games = dict(live.get("games") or {})
        for g in keep:
            games[g] = stored["games"][g]
        out["games"] = games
        if locked and "cash_consensus_lineups" in stored:
            out["cash_consensus_lineups"] = stored["cash_consensus_lineups"]
    return out


def serve(year: int, week: int, kind: str, live_fn: Callable[[], Dict[str, Any]], schedule_csv: str, *,
          draft_group_id: Optional[int] = None, now: Optional[float] = None) -> Dict[str, Any]:
    """Inputs: kind ('projections' | 'sim_results'), live_fn (computes the fresh
    payload), schedule_csv, now (unix s, default time.time()).
    Output: the payload to serve.
      - Legacy whole-week snapshot (no per_game flag): returned verbatim.
      - Every game of the week kicked off AND stored: stored, live_fn not called.
      - Otherwise: the live payload with each kicked-off game's entries replaced
        by their stored ones; persisted whenever it changed, so each game's last
        pre-kickoff payload is what stays on disk.
    A failed write never breaks serving."""
    stored = load(year, week, kind)
    if stored is not None and not _is_per_game(year, week, kind):
        return stored
    t = time.time() if now is None else now
    ko = kickoff_map(week, year, schedule_csv)
    kicked = {g for g, ts in ko.items() if t >= ts}
    if stored is not None and ko and set(ko) <= kicked and set(ko) <= _game_ids_in(kind, stored):
        return stored
    live = live_fn()
    merged = merge_per_game(kind, stored, live, kicked, is_locked(week, year, schedule_csv, now=t))
    if _has_content(kind, merged) and merged != stored:
        try:
            save(year, week, kind, merged, source="live", lock=lock_ts(week, year, schedule_csv),
                 draft_group_id=draft_group_id, force=True, per_game=True)
        except Exception as e:  # noqa: BLE001
            print(f"week_freeze: persisting week {week} {kind} failed: {e}")
    return merged
