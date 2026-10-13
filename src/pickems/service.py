"""Glue for the Pickems page: paste -> ledger -> graded board (src/api/app.py calls these).

submit_paste(text, week)  -- parse, append new/changed legs to the ledger, save the raw
                             paste, return the freshly graded board plus what changed.
graded_board(week)        -- grade the accumulated board without a new paste.
"""
from __future__ import annotations

import os
import time
from typing import Dict, Optional

from src.pickems.grader import BASE_DIR, _match, combo_probability, grade_legs, line_ladder, load_sim_index
from src.pickems.ledger import current_board, leg_key, record_paste
from src.pickems.underdog_parser import PickemLeg, parse_board


def graded_board(week: int, year: int = 2026, base_dir: str = BASE_DIR) -> Optional[Dict]:
    """Grade every leg accumulated for a week against that week's sims.

    Inputs: week, year. Output: grade_legs() dict plus per-row ledger fields
        (open_multiplier, prev_multiplier, last_changed, n_changes) and
        week / board_legs / sims_updated_at; None when the week has no sim parquet.
    """
    index = load_sim_index(week, base_dir)
    if index is None:
        return None
    board = current_board(week, year, base_dir)
    legs = [PickemLeg(b["player"], b["team"], b["opp"], b["home"], b["kickoff"],
                      b["market"], b["line"], b["side"], b["multiplier"]) for b in board]
    out = grade_legs(legs, index)
    by_key = {leg_key(b["team"], b["player"], b["market"], b["line"], b["side"]): b for b in board}
    for r in out["rows"]:
        b = by_key[leg_key(r["team"], r["player"], r["market"], r["line"], r["side"])]
        r.update({k: b[k] for k in ("open_multiplier", "prev_multiplier", "last_changed", "n_changes")})
    p_path = os.path.join(base_dir, "data", "interim", f"dfs_week_{week}_players.parquet")
    out.update({"week": week, "board_legs": len(board), "sims_updated_at": os.path.getmtime(p_path)})
    return out


def leg_ladder(week: int, team: str, player: str, market: str, line: float,
               base_dir: str = BASE_DIR) -> Optional[Dict]:
    """Line ladder for one board leg (see grader.line_ladder). None when the week has no
    sims, the player has no sim match, or the market is unmapped."""
    index = load_sim_index(week, base_dir)
    if index is None:
        return None
    entry, _ = _match(index, team, player)
    if entry is None:
        return None
    return line_ladder(entry, market, line)


def slip_probability(week: int, legs: list, base_dir: str = BASE_DIR) -> Optional[Dict]:
    """Joint hit probability for a slip (see grader.combo_probability). None = no sims or an unpriceable leg."""
    index = load_sim_index(week, base_dir)
    return None if index is None else combo_probability(legs, index)


def submit_paste(text: str, week: int, year: int = 2026, base_dir: str = BASE_DIR) -> Optional[Dict]:
    """Ingest one pasted board.

    Inputs: text (raw paste), week. Output: graded_board() dict plus
        'paste' = {parsed, new, changed, unchanged} (None if the week has no
        sims; raises ValueError when nothing parseable was pasted).
    Purpose: only new/changed legs reach the ledger; the raw text is also kept
        under data/pickems/raw/ so a parser fix can be replayed.
    """
    legs = parse_board(text)
    if not legs:
        raise ValueError("No Underdog legs found in the pasted text.")
    if load_sim_index(week, base_dir) is None:
        return None
    raw_dir = os.path.join(base_dir, "data", "pickems", "raw")
    os.makedirs(raw_dir, exist_ok=True)
    with open(os.path.join(raw_dir, f"{time.strftime('%Y-%m-%d_%H%M%S')}_wk{week}.txt"), "w", encoding="utf-8") as f:
        f.write(text)
    counts = record_paste(week, legs, year, base_dir=base_dir)
    out = graded_board(week, year, base_dir)
    out["paste"] = {"parsed": len(legs), **counts}
    return out
