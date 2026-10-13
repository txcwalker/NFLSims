"""Showdown min-salary floor (2026-10-04): every generated lineup must cost at
least `min_salary` (captain at 1.5x), while still being the exact optimum
among lineups that clear the floor. Covers _solve_showdown_fast (branch and
bound with a leaf reject + reachability prune), the _solve_showdown_ilp
fallback's constraint, and the request validation in optimize_showdown."""
from itertools import combinations

import numpy as np
import pytest


@pytest.fixture(scope="module")
def app_mod():
    import src.api.app as app
    return app


def _pool():
    """Cheap players score MORE here, so the unconstrained optimum is a
    bargain-bin lineup well under the cap -- exactly what a floor must fix."""
    names = [f"A{i}" for i in range(6)] + [f"H{i}" for i in range(6)]
    teams = ["A"] * 6 + ["H"] * 6
    sal = np.array([4000, 5000, 6000, 7000, 8000, 9000] * 2, dtype=float)
    sc = np.array([24, 21, 18, 15, 12, 10, 22, 19, 16, 13, 11, 9], dtype=float)
    players = [{"name": n, "team": t, "salary": int(s), "projection": float(p)}
               for n, t, s, p in zip(names, teams, sal, sc)]
    return players, teams, sal, sc


def _fast(app, players, sc, **kw):
    n = len(players)
    args = dict(players=players, draw_scores=sc, own_flex=np.zeros(n), own_cpt=np.zeros(n),
                leverage_lambda=0.0, salary_cap=50000, prior_lineups=[], min_unique=2,
                max_exposure=1.0, cpt_max_exposure=1.0, n_total=1, locked_indices=set(),
                locked_cpt_indices=set(), excluded_indices=set())
    args.update(kw)
    return app._solve_showdown_fast(**args)


def _total(sal, sol):
    return sal[sol["cpt"]] * 1.5 + sal[sol["flex"]].sum()


def _brute_best(teams, sal, sc, floor):
    """Exact best objective among legal (two-team, <= cap, >= floor) lineups."""
    n = len(teams)
    fv, cv = sc + 0.0005 * sal, 1.5 * sc + 0.00075 * sal
    best = None
    for c in range(n):
        for fl in combinations([i for i in range(n) if i != c], 5):
            fl = list(fl)
            tot = sal[c] * 1.5 + sal[fl].sum()
            if tot > 50000 or tot < floor:
                continue
            if len({teams[c], *(teams[i] for i in fl)}) < 2:
                continue
            v = cv[c] + fv[fl].sum()
            best = v if best is None or v > best else best
    return best


def test_unconstrained_lineup_is_under_the_floor(app_mod):
    """Sanity: the fixture really does make the floor bind."""
    players, _, sal, sc = _pool()
    assert _total(sal, _fast(app_mod, players, sc)) < 46000


@pytest.mark.parametrize("floor", [40000, 46000, 48500, 49500])
def test_floor_respected_and_optimal(app_mod, floor):
    players, teams, sal, sc = _pool()
    sol = _fast(app_mod, players, sc, min_salary=floor)
    assert sol is not None
    assert floor <= _total(sal, sol) <= 50000
    got = 1.5 * sc[sol["cpt"]] + 0.00075 * sal[sol["cpt"]] + (sc[sol["flex"]] + 0.0005 * sal[sol["flex"]]).sum()
    assert got == pytest.approx(_brute_best(teams, sal, sc, floor))


def test_zero_floor_changes_nothing(app_mod):
    players, _, _, sc = _pool()
    assert _fast(app_mod, players, sc, min_salary=0) == _fast(app_mod, players, sc)


def test_unreachable_floor_returns_none(app_mod):
    players, _, _, sc = _pool()
    assert _fast(app_mod, players, sc, min_salary=50000 + 1, salary_cap=50000) is None


def test_floor_with_locked_player(app_mod):
    players, _, sal, sc = _pool()
    sol = _fast(app_mod, players, sc, min_salary=47000, locked_indices={0})
    assert sol is not None and 0 in {sol["cpt"], *sol["flex"]}
    assert _total(sal, sol) >= 47000


def test_ilp_fallback_honours_floor(app_mod):
    players, _, sal, sc = _pool()
    n = len(players)
    sol = app_mod._solve_showdown_ilp(
        players, sc, np.zeros(n), np.zeros(n), 0.0, 50000, [], 2, 1.0, 1.0, 1, set(), set(), set(),
        min_salary=47000)
    assert sol is not None and _total(sal, sol) >= 47000


def test_request_defaults_to_no_floor(app_mod):
    assert app_mod.ShowdownOptimizeRequest.model_fields["min_salary"].default == 0
