"""DK Showdown roster rule: every lineup's 6 players must include at least one
from EACH team (2026-09-28 -- previously unenforced).

Covers all three places that build showdown lineups:
  - src/nfl_sim/optimizer.py solve_showdown_iteration  (Opt CPT% / FLEX%)
  - src/api/app.py _solve_showdown_fast                 (our generated lineups)
  - src/api/app.py _build_showdown_field                (synthetic opponent field)

Each pool is built so the unconstrained optimum is a one-team lineup (team A
massively outscores team B), which is exactly the case the rule must block.
"""
import numpy as np
import pytest

from src.nfl_sim.optimizer import solve_showdown_iteration


def _pool(b_scores=(1.0, 0.5, 0.2)):
    """8 team-A studs + 3 cheap team-B players. Inputs: b_scores (team B's
    per-player scores). Output: (names, teams, salaries, scores)."""
    names = [f"A{i}" for i in range(8)] + [f"B{i}" for i in range(len(b_scores))]
    teams = ["A"] * 8 + ["B"] * len(b_scores)
    salaries = np.array([6000] * 8 + [2000] * len(b_scores), dtype=float)
    scores = np.array([30.0 - i for i in range(8)] + list(b_scores), dtype=float)
    return names, teams, salaries, scores


def _team_set(lineup, names, teams):
    return {teams[names.index(n)] for n in lineup}


# --- solve_showdown_iteration --------------------------------------------------

def test_iteration_solver_unconstrained_is_one_team():
    """Sanity check on the fixture: without teams, the optimum is all team A."""
    names, teams, sal, sc = _pool()
    lu = solve_showdown_iteration(names, sal, sc)
    assert _team_set(lu, names, teams) == {"A"}


def test_iteration_solver_enforces_two_teams():
    names, teams, sal, sc = _pool()
    lu = solve_showdown_iteration(names, sal, sc, teams=teams)
    assert len(lu) == 6
    assert _team_set(lu, names, teams) == {"A", "B"}
    assert "B0" in lu  # the best team-B filler, not an arbitrary one


def test_iteration_solver_other_team_all_zero_still_legal():
    """A team that scored nothing still has to supply a player."""
    names, teams, sal, sc = _pool(b_scores=(0.0, 0.0, -1.0))
    lu = solve_showdown_iteration(names, sal, sc, teams=teams)
    assert len(lu) == 6
    assert _team_set(lu, names, teams) == {"A", "B"}
    assert "B2" not in lu  # never the negative one when a 0 is available


# --- app.py solvers ----------------------------------------------------------

@pytest.fixture(scope="module")
def app_mod():
    import src.api.app as app
    return app


def _players(names, teams, sal, sc):
    return [{"name": n, "team": t, "salary": int(s), "projection": float(p)}
            for n, t, s, p in zip(names, teams, sal, sc)]


def _fast(app, players, sc, **kw):
    n = len(players)
    args = dict(players=players, draw_scores=sc, own_flex=np.zeros(n), own_cpt=np.zeros(n),
                leverage_lambda=0.0, salary_cap=50000, prior_lineups=[], min_unique=2,
                max_exposure=1.0, cpt_max_exposure=1.0, n_total=1, locked_indices=set(),
                locked_cpt_indices=set(), excluded_indices=set())
    args.update(kw)
    return app._solve_showdown_fast(**args)


def test_fast_solver_enforces_two_teams(app_mod):
    names, teams, sal, sc = _pool()
    sol = _fast(app_mod, _players(names, teams, sal, sc), sc)
    assert sol is not None
    assert {teams[i] for i in [sol["cpt"], *sol["flex"]]} == {"A", "B"}


def test_fast_solver_locked_one_team_core_still_adds_other_team(app_mod):
    """Captain + 3 FLEX locked, all team A: the 2 open slots must bring in B."""
    names, teams, sal, sc = _pool()
    sol = _fast(app_mod, _players(names, teams, sal, sc), sc,
                locked_indices={0, 1, 2, 3}, locked_cpt_indices={0})
    assert sol["cpt"] == 0 and {1, 2, 3} <= set(sol["flex"])
    assert {teams[i] for i in [sol["cpt"], *sol["flex"]]} == {"A", "B"}


def test_fast_solver_prune_handles_negative_values(app_mod):
    """A heavy leverage penalty makes most FLEX values negative; the solve must
    still return the true best legal lineup (the old all-remaining-sum bound
    over-pruned here)."""
    names, teams, sal, sc = _pool()
    players = _players(names, teams, sal, sc)
    n = len(players)
    own = np.full(n, 10.0)
    sol = _fast(app_mod, players, sc, own_flex=own, own_cpt=own, leverage_lambda=2.5)
    assert sol is not None
    # brute force over every legal lineup for the same objective
    from itertools import combinations
    salv = sal
    flex_val = sc - 2.5 * own + 0.0005 * salv
    cpt_val = 1.5 * sc - 2.5 * own + 0.00075 * salv
    best = -1e18
    for c in range(n):
        for fl in combinations([i for i in range(n) if i != c], 5):
            if salv[c] * 1.5 + salv[list(fl)].sum() > 50000:
                continue
            if len({teams[i] for i in (c, *fl)}) < 2:
                continue
            best = max(best, cpt_val[c] + flex_val[list(fl)].sum())
    got = cpt_val[sol["cpt"]] + flex_val[sol["flex"]].sum()
    assert got == pytest.approx(best)


def test_field_is_all_two_team(app_mod):
    names, teams, sal, sc = _pool()
    players = _players(names, teams, sal, sc)
    for p in players:  # ownership heavily on team A, so one-team draws are common
        p["ownership_pct"] = 80.0 if p["team"] == "A" else 1.0
        p["cpt_ownership_pct"] = 80.0 if p["team"] == "A" else 1.0
    field = app_mod._build_showdown_field(players, 50000, n_field=300, seed=7)
    assert len(field) > 0
    for lu in field:
        assert {teams[i] for i in [lu["cpt"], *lu["flex"]]} == {"A", "B"}
