"""Showdown per-lineup DST/K caps (2026-10-05): max_dst_k (defenses + kickers
combined), max_dst, and max_k each limit how many of that position ONE lineup
may hold across all 6 slots (captain included), while the result stays the
exact optimum among legal lineups. Covers _solve_showdown_fast (branch and
bound) and request validation. Pool is built so DST/K are the highest-scoring
players -- an uncapped solve loads up on them, so every cap visibly binds."""
from itertools import combinations

import numpy as np
import pytest


@pytest.fixture(scope="module")
def app_mod():
    import src.api.app as app
    return app


def _pool():
    """2 teams x (3 skill + 1 K + 1 DST); DST/K score highest and are cheap."""
    rows = []
    for t, base in (("A", 0), ("H", 1)):
        rows += [
            (f"{t}_QB", t, "QB", 7000, 20.0 - base), (f"{t}_WR", t, "WR", 6000, 14.0 - base),
            (f"{t}_RB", t, "RB", 5000, 11.0 - base),
            (f"{t}_K", t, "K", 4000, 24.0 - base), (f"{t}_DST", t, "DST", 3500, 22.0 - base),
        ]
    players = [{"name": n, "team": t, "pos": p, "salary": s, "projection": sc} for n, t, p, s, sc in rows]
    sal = np.array([p["salary"] for p in players], dtype=float)
    sc = np.array([p["projection"] for p in players], dtype=float)
    return players, sal, sc


def _fast(app, players, sc, **kw):
    n = len(players)
    args = dict(players=players, draw_scores=sc, own_flex=np.zeros(n), own_cpt=np.zeros(n),
                leverage_lambda=0.0, salary_cap=50000, prior_lineups=[], min_unique=2,
                max_exposure=1.0, cpt_max_exposure=1.0, n_total=1, locked_indices=set(),
                locked_cpt_indices=set(), excluded_indices=set())
    args.update(kw)
    return app._solve_showdown_fast(**args)


def _counts(players, sol):
    members = [sol["cpt"], *sol["flex"]]
    pos = [players[i]["pos"] for i in members]
    return pos.count("DST"), pos.count("K")


def _brute_best(players, sal, sc, max_dst_k=None, max_dst=None, max_k=None):
    """Exact best objective among legal lineups (two teams, <= cap, caps)."""
    n = len(players)
    fv, cv = sc + 0.0005 * sal, 1.5 * sc + 0.00075 * sal
    best = None
    for c in range(n):
        for fl in combinations([i for i in range(n) if i != c], 5):
            members = [c, *fl]
            if sal[c] * 1.5 + sal[list(fl)].sum() > 50000:
                continue
            if len({players[i]["team"] for i in members}) < 2:
                continue
            nd = sum(players[i]["pos"] == "DST" for i in members)
            nk = sum(players[i]["pos"] == "K" for i in members)
            if (max_dst_k is not None and nd + nk > max_dst_k) or \
               (max_dst is not None and nd > max_dst) or (max_k is not None and nk > max_k):
                continue
            v = cv[c] + fv[list(fl)].sum()
            best = v if best is None or v > best else best
    return best


def _value(sc, sal, sol):
    return 1.5 * sc[sol["cpt"]] + 0.00075 * sal[sol["cpt"]] + (sc[sol["flex"]] + 0.0005 * sal[sol["flex"]]).sum()


def test_uncapped_lineup_loads_up_on_dst_and_k(app_mod):
    """Sanity: the fixture really makes the caps bind."""
    players, _, sc = _pool()
    nd, nk = _counts(players, _fast(app_mod, players, sc))
    assert nd + nk >= 3


@pytest.mark.parametrize("caps", [
    dict(max_dst_k=0), dict(max_dst_k=1), dict(max_dst_k=2), dict(max_dst_k=3),
    dict(max_dst=0), dict(max_dst=1), dict(max_k=0), dict(max_k=1),
    dict(max_dst=1, max_k=1), dict(max_dst_k=3, max_dst=2, max_k=1),
])
def test_caps_respected_and_optimal(app_mod, caps):
    players, sal, sc = _pool()
    sol = _fast(app_mod, players, sc, **caps)
    assert sol is not None
    nd, nk = _counts(players, sol)
    if "max_dst_k" in caps:
        assert nd + nk <= caps["max_dst_k"]
    if "max_dst" in caps:
        assert nd <= caps["max_dst"]
    if "max_k" in caps:
        assert nk <= caps["max_k"]
    assert _value(sc, sal, sol) == pytest.approx(_brute_best(players, sal, sc, **caps))


def test_no_caps_changes_nothing(app_mod):
    players, _, sc = _pool()
    assert _fast(app_mod, players, sc, max_dst_k=None, max_dst=None, max_k=None) == _fast(app_mod, players, sc)


def test_cap_counts_the_captain_slot(app_mod):
    """max_dst_k=1 with the top DST locked as captain leaves no room for a K."""
    players, _, sc = _pool()
    dst = next(i for i, p in enumerate(players) if p["pos"] == "DST")
    sol = _fast(app_mod, players, sc, max_dst_k=1, locked_indices={dst}, locked_cpt_indices={dst})
    assert sol["cpt"] == dst
    assert _counts(players, sol) == (1, 0)


def test_locks_that_violate_a_cap_are_infeasible(app_mod):
    """Locking both kickers while allowing at most one K can't be satisfied."""
    players, _, sc = _pool()
    ks = {i for i, p in enumerate(players) if p["pos"] == "K"}
    assert _fast(app_mod, players, sc, max_k=1, locked_indices=ks) is None


def test_ilp_fallback_respects_caps(app_mod):
    players, sal, sc = _pool()
    n = len(players)
    sol = app_mod._solve_showdown_ilp(
        players, sc, np.zeros(n), np.zeros(n), 0.0, 50000, [], 2, 1.0, 1.0, 1, set(), set(), set(),
        max_dst_k=1, max_dst=1, max_k=1)
    assert sol is not None
    nd, nk = _counts(players, sol)
    assert nd + nk <= 1


def test_request_model_defaults_to_no_caps(app_mod):
    req = app_mod.ShowdownOptimizeRequest(players=[])
    assert (req.max_dst_k, req.max_dst, req.max_k) == (None, None, None)
