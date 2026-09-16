"""Post-processing renormalization so projected ownership sums match real
DK roster-construction math. Applied identically after either the
heuristic (heuristic.py) or the trained model (model_inference.py)
computes a first-pass per-player ownership_pct/cpt_ownership_pct -- both
produce numbers that are directionally reasonable but don't sum to
anything in particular (a softmax scaled to a target sum still drifts off
it once per-player noise/soft-caps, or an independent per-player sigmoid
with no sum constraint at all, are layered on top).

Classic DK roster math (why the targets are what they are): every lineup
has exactly 1 QB, 1 DST, 2 RB, 3 WR, 1 TE and 1 FLEX (RB/WR/TE-eligible).
So across the whole field, ownership must sum to exactly 100 for QB and
for DST (one guaranteed slot each, always filled), and RB+WR+TE together
must sum to exactly 700 (2+3+1 guaranteed slots + the 1 FLEX slot, x100
each). Individually RB lands in 200-300, WR in 300-400, TE in 100-200
depending on how that single FLEX slot splits across positions --
`_renorm_flex_group` derives that split from the model's own relative
scores (who it already thinks is a stronger FLEX-type play) rather than
assuming a fixed split, which is why the ranges fall out automatically
instead of needing to be hand-picked.

Showdown DK roster math: 1 CPT + 5 FLEX, any position eligible for either.
CPT ownership sums to exactly 100, FLEX to exactly 500 -- no per-position
split needed, so both are just the simple single-group case.
"""
from __future__ import annotations

import numpy as np


def _proportional_split(weights: list, total: float, floor: float = 0.1, max_iter: int = 12) -> list:
    """Distribute `total` across len(weights) buckets proportional to
    `weights`, each bucket getting at least `floor` via water-filling:
    clip any bucket whose proportional share is below `floor` to exactly
    `floor` and redistribute the rest among the remaining buckets
    (proportional to their weights), repeating until stable. Standard
    technique for sharing a fixed budget without a low-weight player
    getting rounded to ~0.
    """
    n = len(weights)
    if n == 0:
        return []
    if total <= 1e-9:
        return [0.0] * n
    w = np.array([max(wi, 1e-6) for wi in weights], dtype=float)
    result = np.zeros(n)
    remaining_idx = list(range(n))
    remaining_total = total
    for _ in range(max_iter):
        if not remaining_idx:
            break
        if remaining_total < floor * len(remaining_idx):
            # Not enough left to give everyone the floor -- split flat.
            # Only reachable with a pathologically small `total` vs. pool
            # size (e.g. a near-empty position group), not a real slate.
            flat = remaining_total / len(remaining_idx)
            for idx in remaining_idx:
                result[idx] = flat
            remaining_idx = []
            break
        sub_w = w[remaining_idx]
        share = remaining_total * sub_w / sub_w.sum()
        below = [remaining_idx[i] for i, s in enumerate(share) if s < floor]
        if not below:
            for idx, s in zip(remaining_idx, share):
                result[idx] = s
            remaining_idx = []
            break
        for idx in below:
            result[idx] = floor
        remaining_total -= floor * len(below)
        remaining_idx = [i for i in remaining_idx if i not in below]
    return result.tolist()


def _renorm_simple_group(players: list, indices: list, field: str, locked: set, target: float) -> None:
    """Rescale `field` (e.g. 'ownership_pct') across `players[i] for i in
    indices` to sum to exactly `target`, leaving `locked` indices
    untouched and distributing the remaining budget among non-locked
    players proportional to their current value (their relative shape
    from the model/heuristic). Used for QB / DST / showdown CPT / showdown
    FLEX -- any group with a single guaranteed-slot target and no
    cross-position FLEX ambiguity.
    """
    if not indices:
        return
    locked_sum = sum(players[i].get(field) or 0.0 for i in indices if i in locked)
    free = [i for i in indices if i not in locked]
    free_target = max(0.0, target - locked_sum)
    weights = [players[i].get(field) or 0.1 for i in free]
    shares = _proportional_split(weights, free_target)
    for i, share in zip(free, shares):
        players[i][field] = share


def _renorm_flex_group(players: list, pos_indices: dict, locked: set, field: str,
                        floors: dict, flex_budget: float) -> None:
    """Classic RB/WR/TE joint renormalization: each position gets its
    guaranteed-slot floor (`floors`), then the single shared FLEX budget
    (`flex_budget`) is split across the WHOLE combined pool by relative
    shape -- see module docstring for why this guarantees each position's
    sum lands in its structural range without hand-picking a target.

    A locked player's value is left untouched; the portion of it beyond
    its own position's floor is treated as having also claimed some of
    the shared FLEX budget, so the overall total across positions still
    comes out exactly right regardless of how manual overrides land.
    """
    if not any(pos_indices.values()):
        return

    locked_flex_contrib = 0.0
    remaining_floor: dict = {}
    free_by_pos: dict = {}
    for pos, idxs in pos_indices.items():
        locked_sum_pos = sum(players[i].get(field) or 0.0 for i in idxs if i in locked)
        floor = floors.get(pos, 0.0)
        remaining_floor[pos] = max(0.0, floor - min(locked_sum_pos, floor))
        locked_flex_contrib += max(0.0, locked_sum_pos - floor)
        free_by_pos[pos] = [i for i in idxs if i not in locked]

    # Stage 1: each position's guaranteed-slot floor, split within-position.
    for pos, idxs in free_by_pos.items():
        weights = [players[i].get(field) or 0.1 for i in idxs]
        shares = _proportional_split(weights, remaining_floor[pos])
        for i, share in zip(idxs, shares):
            players[i][field] = share

    # Stage 2: the shared FLEX budget, split across the WHOLE combined pool.
    free_all = [i for idxs in free_by_pos.values() for i in idxs]
    remaining_flex = max(0.0, flex_budget - locked_flex_contrib)
    weights_all = [players[i].get(field) or 0.1 for i in free_all]
    flex_shares = _proportional_split(weights_all, remaining_flex)
    for i, share in zip(free_all, flex_shares):
        players[i][field] = players[i].get(field, 0.0) + share


def normalize_classic_ownership(players: list, locked: set) -> list:
    """Mutates `players[i]['ownership_pct']` in place so QB and DST each
    sum to exactly 100, and RB+WR+TE jointly sum to exactly 700 with each
    individual position landing in its structural range (RB 200-300, WR
    300-400, TE 100-200). `locked` = indices (into `players`) whose
    ownership_pct is a genuine hand override that must not be touched.
    Unpriced players (no salary) are left alone.
    """
    by_pos: dict = {}
    for i, p in enumerate(players):
        if p.get('salary') is None:
            continue
        by_pos.setdefault(p.get('pos'), []).append(i)

    _renorm_simple_group(players, by_pos.get('QB', []), 'ownership_pct', locked, 100.0)
    _renorm_simple_group(players, by_pos.get('DST', []), 'ownership_pct', locked, 100.0)
    flex_pos = {pos: by_pos.get(pos, []) for pos in ('RB', 'WR', 'TE')}
    _renorm_flex_group(players, flex_pos, locked, 'ownership_pct',
                        floors={'RB': 200.0, 'WR': 300.0, 'TE': 100.0}, flex_budget=100.0)

    for i, p in enumerate(players):
        if p.get('ownership_pct') is not None:
            p['ownership_pct'] = round(p['ownership_pct'], 1)
    return players


def normalize_showdown_ownership(players: list, locked_flex: set, locked_cpt: set) -> list:
    """Mutates `ownership_pct` (FLEX) to sum to exactly 500 and
    `cpt_ownership_pct` (CPT) to sum to exactly 100 -- showdown FLEX has no
    per-position split (any position is FLEX-eligible), so both are just
    the simple single-group case, run once each. Unpriced players (no
    salary) are left alone.
    """
    priced = [i for i, p in enumerate(players) if p.get('salary')]
    _renorm_simple_group(players, priced, 'ownership_pct', locked_flex, 500.0)
    _renorm_simple_group(players, priced, 'cpt_ownership_pct', locked_cpt, 100.0)

    for i in priced:
        p = players[i]
        if p.get('ownership_pct') is not None:
            p['ownership_pct'] = round(p['ownership_pct'], 1)
        if p.get('cpt_ownership_pct') is not None:
            p['cpt_ownership_pct'] = round(p['cpt_ownership_pct'], 1)
    return players
