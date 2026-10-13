"""Showdown build minimums (2026-09-29): "at least X% of lineups must be a
k-(6-k) build", the rest unconstrained.

Covers the three pieces in src/api/app.py:
  - _build_quotas         fractions -> lineup counts (round UP) + validation
  - _pick_forced_build    which build the next lineup is forced into (even
                          spacing, natural hits count, deadline guarantee)
  - _solve_showdown_fast  required_away = exact away-player count (captain
                          included), still the exact optimum for that build
and end to end through optimize_showdown (minimums met, bad input -> 400).
"""
from itertools import combinations

import numpy as np
import pytest


@pytest.fixture(scope="module")
def app_mod():
    import src.api.app as app
    return app


# --- _build_quotas -----------------------------------------------------------

def test_quotas_round_up(app_mod):
    # 15% of 150 = 22.5 -> 23 (a minimum is a floor); 30% of 150 = 45 exactly
    assert app_mod._build_quotas({1: 0.15, 4: 0.30}, 150) == {1: 23, 4: 45}


def test_quotas_float_noise_does_not_add_a_lineup(app_mod):
    # 0.3 * 10 = 3.0000000000000004 in float -- must stay 3, not ceil to 4
    assert app_mod._build_quotas({4: 0.3}, 10) == {4: 3}


def test_quotas_drop_zero_and_none(app_mod):
    assert app_mod._build_quotas({1: 0.0, 2: None, 3: 0.5}, 20) == {3: 10}
    assert app_mod._build_quotas(None, 20) == {}


def test_quotas_too_many_raises(app_mod):
    # 3 x 34% of 10 -> 4 + 4 + 4 = 12 > 10 after rounding up
    with pytest.raises(ValueError, match="need 12 lineups"):
        app_mod._build_quotas({1: 0.34, 3: 0.34, 5: 0.34}, 10)


@pytest.mark.parametrize("bad", [{0: 0.1}, {6: 0.1}, {3: 1.5}])
def test_quotas_bad_input_raises(app_mod, bad):
    with pytest.raises(ValueError):
        app_mod._build_quotas(bad, 20)


# --- _pick_forced_build -------------------------------------------------------

def _simulate(app, quotas, n_total, natural_build=3, infeasible=()):
    """Drive the scheduler like optimize_showdown does. Unforced lineups land
    on `natural_build`; forcing an `infeasible` build fails every time.
    Output: (have counts, forced-build list per generated lineup, given_up)."""
    have, order, given_up, fails = {}, [], set(), {}
    attempts = 0
    while len(order) < n_total and attempts < n_total * 4:
        attempts += 1
        force = app._pick_forced_build(quotas, have, len(order), n_total, given_up)
        if force in infeasible:
            fails[force] = fails.get(force, 0) + 1
            if fails[force] >= app.BUILD_FORCE_MAX_FAILS:
                given_up.add(force)
            continue
        b = natural_build if force is None else force
        have[b] = have.get(b, 0) + 1
        order.append(force)
    return have, order, given_up


def test_scheduler_meets_minimums(app_mod):
    have, order, _ = _simulate(app_mod, {1: 3, 4: 6}, 20)
    assert have.get(1, 0) >= 3 and have.get(4, 0) >= 6
    assert len(order) == 20
    assert order.count(None) == 11  # everything not owed stays unconstrained


def test_scheduler_spreads_forced_lineups(app_mod):
    """Forced lineups are interleaved through the run, not bunched at either end."""
    _, order, _ = _simulate(app_mod, {4: 10}, 40)
    forced_at = [i for i, f in enumerate(order) if f == 4]
    assert len(forced_at) == 10
    assert forced_at[0] < 8            # starts early...
    assert forced_at[-1] >= 32         # ...and continues late
    assert max(np.diff(forced_at)) <= 6  # no long gaps -> no end-loading


def test_scheduler_counts_natural_hits(app_mod):
    """If unconstrained lineups already land on the build, nothing is forced."""
    have, order, _ = _simulate(app_mod, {3: 5}, 20, natural_build=3)
    assert have[3] == 20
    assert all(f is None for f in order)


def test_scheduler_gives_up_on_infeasible_build(app_mod):
    have, order, given_up = _simulate(app_mod, {1: 4, 4: 4}, 20, infeasible=(1,))
    assert given_up == {1}
    assert have.get(4, 0) >= 4          # the feasible minimum is still met
    assert len(order) == 20             # and the run still fills up


# --- _solve_showdown_fast(required_away=...) ----------------------------------

def _pool():
    """6 per team, team A (away) scores more, so unconstrained leans A-heavy."""
    names = [f"A{i}" for i in range(6)] + [f"H{i}" for i in range(6)]
    teams = ["A"] * 6 + ["H"] * 6
    sal = np.array([9000, 8000, 7000, 6000, 5000, 4000] * 2, dtype=float)
    sc = np.array([30, 25, 20, 15, 10, 8, 22, 18, 14, 11, 9, 6], dtype=float)
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


def _brute_best(teams, sal, sc, k):
    """Exact best objective among legal lineups with exactly k 'A' players."""
    n = len(teams)
    fv, cv = sc + 0.0005 * sal, 1.5 * sc + 0.00075 * sal
    best = None
    for c in range(n):
        for fl in combinations([i for i in range(n) if i != c], 5):
            ids = (c, *fl)
            if sal[c] * 1.5 + sal[list(fl)].sum() > 50000:
                continue
            if sum(teams[i] == "A" for i in ids) != k:
                continue
            v = cv[c] + fv[list(fl)].sum()
            best = v if best is None or v > best else best
    return best


@pytest.mark.parametrize("k", [1, 2, 3, 4, 5])
def test_solver_hits_exact_build_and_is_optimal(app_mod, k):
    players, teams, sal, sc = _pool()
    sol = _fast(app_mod, players, sc, required_away=k, away_team="A")
    assert sol is not None
    ids = [sol["cpt"], *sol["flex"]]
    assert sum(teams[i] == "A" for i in ids) == k
    got = 1.5 * sc[sol["cpt"]] + 0.00075 * sal[sol["cpt"]] + (sc[sol["flex"]] + 0.0005 * sal[sol["flex"]]).sum()
    assert got == pytest.approx(_brute_best(teams, sal, sc, k))


def test_solver_infeasible_build_returns_none(app_mod):
    """Captain + 2 FLEX locked, all team A -> a 1-5 build is impossible."""
    players, _, _, sc = _pool()
    sol = _fast(app_mod, players, sc, required_away=1, away_team="A",
                locked_indices={0, 1, 2}, locked_cpt_indices={0})
    assert sol is None


def test_solver_without_build_is_unchanged(app_mod):
    players, _, _, sc = _pool()
    assert _fast(app_mod, players, sc) == _fast(app_mod, players, sc, required_away=None, away_team="A")


# --- optimize_showdown end to end --------------------------------------------

@pytest.fixture(scope="module")
def client(app_mod):
    from fastapi.testclient import TestClient
    return TestClient(app_mod.app)


def _req_players():
    players, _, _, _ = _pool()
    rng = np.random.default_rng(3)
    for p in players:
        p["pos"] = "WR"
        p["ownership_pct"] = float(rng.uniform(5, 60))
        p["cpt_ownership_pct"] = float(rng.uniform(1, 25))
    return players


def test_endpoint_meets_build_minimums(client):
    body = dict(players=_req_players(), n_lineups=20, min_unique_players=1,
                max_exposure=1.0, cpt_max_exposure=1.0,
                build_minimums={1: 0.15, 4: 0.30}, away_team="A")
    r = client.post("/api/optimize_showdown", json=body)
    assert r.status_code == 200, r.text
    res = r.json()
    counts = {}
    for lu in res["lineups"]:
        k = sum(p["team"] == "A" for p in lu["players"])
        counts[k] = counts.get(k, 0) + 1
    assert counts.get(1, 0) >= 3 and counts.get(4, 0) >= 6
    report = {b["away_count"]: b for b in res["portfolio"]["build_minimums"]}
    assert report[1]["required"] == 3 and report[4]["required"] == 6
    assert report[1]["achieved"] == counts.get(1, 0)


def test_endpoint_rejects_unmeetable_minimums(client):
    body = dict(players=_req_players(), n_lineups=10,
                build_minimums={1: 0.6, 5: 0.6}, away_team="A")
    r = client.post("/api/optimize_showdown", json=body)
    assert r.status_code == 400 and "only 10 were requested" in r.json()["detail"]


def test_endpoint_requires_away_team(client):
    body = dict(players=_req_players(), n_lineups=10, build_minimums={1: 0.2})
    r = client.post("/api/optimize_showdown", json=body)
    assert r.status_code == 400 and "away_team" in r.json()["detail"]
