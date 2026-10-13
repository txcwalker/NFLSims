"""Append-only line ledger for pasted Underdog boards (Pickems > Underdog Lines).

Every paste is diffed against the current board for the week; only NEW legs and
legs whose multiplier CHANGED are appended, so each paste "updates only the
lines that changed". Because nothing is overwritten, the earliest row of a leg
is its opening multiplier and the latest row is the current one -- the data the
open-vs-close evaluation needs. Like data/eval/{year}/line_history.csv, this
file cannot be regenerated once a line moves: commit it.

File: data/pickems/{year}/line_history.csv
Also data/pickems/{year}/last_seen.json: when each leg was last seen in a paste
(changed or not), so the closing line is only trusted when a paste actually
happened shortly before kickoff.

Entry points: record_paste(week, legs) -> counts; current_board(week) -> list[dict];
leg_histories(week) -> full multiplier history per leg (for grading).
"""
from __future__ import annotations

import csv
import json
import os
import time
from typing import Dict, List, Optional

from src.pickems.underdog_parser import PickemLeg

BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
COLUMNS = ["captured_at", "week", "team", "player", "market", "line", "side",
           "multiplier", "opp", "home", "kickoff"]


def ledger_path(year: int = 2026, base_dir: str = BASE_DIR) -> str:
    return os.path.join(base_dir, "data", "pickems", str(year), "line_history.csv")


def leg_key(team: str, player: str, market: str, line: float, side: str) -> tuple:
    """Identity of a leg: the same player/market/line/side is one leg over time."""
    return (team, player, market, float(line), side)


def _read_rows(path: str) -> List[dict]:
    if not os.path.exists(path):
        return []
    with open(path, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def seen_path(year: int = 2026, base_dir: str = BASE_DIR) -> str:
    """Sidecar of when each leg was LAST seen in a paste (changed or not)."""
    return os.path.join(base_dir, "data", "pickems", str(year), "last_seen.json")


def _seen_key(key: tuple) -> str:
    return "|".join(str(k) for k in key)


def _read_seen(year: int, base_dir: str) -> Dict[str, Dict[str, float]]:
    """{str(week): {leg key string: epoch secs last seen}} ({} when absent)."""
    p = seen_path(year, base_dir)
    if not os.path.exists(p):
        return {}
    with open(p, encoding="utf-8") as f:
        return json.load(f)


def leg_histories(week: int, year: int = 2026, base_dir: str = BASE_DIR) -> List[dict]:
    """Every leg seen for a week, with its full multiplier history.

    Inputs: week, year.
    Output: list of dicts (one per leg): the leg's fields (player, team, opp,
        home, kickoff, market, line, side), history = [(captured_at, multiplier)]
        oldest first (one entry per capture that NEW/CHANGED the multiplier), and
        last_seen (epoch secs of the last paste containing the leg -- later than
        the last history entry when the leg was re-pasted unchanged).
    Purpose: the raw material for both the live board and open-vs-close grading.
    """
    hist: Dict[tuple, List[dict]] = {}
    for r in _read_rows(ledger_path(year, base_dir)):
        if int(r["week"]) != week:
            continue
        hist.setdefault(leg_key(r["team"], r["player"], r["market"], r["line"], r["side"]), []).append(r)
    seen = _read_seen(year, base_dir).get(str(week), {})
    out = []
    for key, rows in hist.items():
        rows.sort(key=lambda r: float(r["captured_at"]))
        last = rows[-1]
        out.append({
            "player": last["player"], "team": last["team"], "opp": last["opp"],
            "home": last["home"] == "True", "kickoff": last["kickoff"],
            "market": last["market"], "line": float(last["line"]), "side": last["side"],
            "history": [(float(r["captured_at"]), float(r["multiplier"])) for r in rows],
            "last_seen": max(float(seen.get(_seen_key(key), 0.0)), float(last["captured_at"])),
        })
    return out


def current_board(week: int, year: int = 2026, base_dir: str = BASE_DIR) -> List[dict]:
    """Latest state of every leg seen for a week.

    Inputs: week, year.
    Output: list of dicts (one per leg) with the leg's fields plus
        multiplier (latest), open_multiplier (first capture), prev_multiplier
        (the one before latest, None if never changed), first_seen,
        last_changed (epoch secs), n_changes.
    Purpose: the board the grader prices -- accumulates across pastes.
    """
    board = []
    for h in leg_histories(week, year, base_dir):
        hist = h["history"]
        board.append({
            **{k: h[k] for k in ("player", "team", "opp", "home", "kickoff", "market", "line", "side")},
            "multiplier": hist[-1][1],
            "open_multiplier": hist[0][1],
            "prev_multiplier": hist[-2][1] if len(hist) > 1 else None,
            "first_seen": hist[0][0],
            "last_changed": hist[-1][0],
            "n_changes": len(hist) - 1,
        })
    return board


def record_paste(week: int, legs: List[PickemLeg], year: int = 2026,
                 now: Optional[float] = None, base_dir: str = BASE_DIR) -> Dict[str, int]:
    """Append the new/changed legs of a paste to the ledger.

    Inputs: week; legs (parse_board output); now (epoch secs, default time.time()).
    Output: {'new': n, 'changed': n, 'unchanged': n}.
    Purpose: diff against the current board so repeated pastes of a mostly
        unchanged board add only what moved. Duplicate legs within one paste
        keep the last occurrence.
    """
    now = time.time() if now is None else now
    current = {leg_key(b["team"], b["player"], b["market"], b["line"], b["side"]): b["multiplier"]
               for b in current_board(week, year, base_dir)}
    latest = {}
    for l in legs:
        latest[leg_key(l.team, l.player, l.market, l.line, l.side)] = l
    counts = {"new": 0, "changed": 0, "unchanged": 0}
    out = []
    for key, l in latest.items():
        if key not in current:
            counts["new"] += 1
        elif abs(current[key] - l.multiplier) > 1e-9:
            counts["changed"] += 1
        else:
            counts["unchanged"] += 1
            continue
        out.append({"captured_at": now, "week": week, "team": l.team, "player": l.player,
                    "market": l.market, "line": l.line, "side": l.side, "multiplier": l.multiplier,
                    "opp": l.opp, "home": l.home, "kickoff": l.kickoff})
    # Every leg in this paste -- changed or not -- was seen now (grades the CLOSE honestly).
    seen = _read_seen(year, base_dir)
    wk = seen.setdefault(str(week), {})
    for key in latest:
        wk[_seen_key(key)] = now
    os.makedirs(os.path.dirname(seen_path(year, base_dir)), exist_ok=True)
    with open(seen_path(year, base_dir), "w", encoding="utf-8") as f:
        json.dump(seen, f)
    if out:
        path = ledger_path(year, base_dir)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        new_file = not os.path.exists(path)
        with open(path, "a", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=COLUMNS)
            if new_file:
                w.writeheader()
            w.writerows(out)
    return counts
