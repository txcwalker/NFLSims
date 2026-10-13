"""_nearest_positive_definite must always terminate with a Cholesky-able matrix
(2026-10-04): an exactly singular covariance (min eigenvalue exactly 0) used to
spin its `while True` loop forever on the event loop, freezing the whole API.
Also guards that /api/optimize stays a plain `def` (threadpool, not event loop)."""
import inspect

import numpy as np
import pytest


@pytest.fixture(scope="module")
def app_mod():
    import src.api.app as app
    return app


def _is_pd(M):
    try:
        np.linalg.cholesky(M)
        return True
    except np.linalg.LinAlgError:
        return False


def test_already_pd_matrix_is_unchanged(app_mod):
    rng = np.random.default_rng(0)
    X = rng.normal(size=(20, 40))
    A = X @ X.T / 40 + np.eye(20)
    assert np.allclose(app_mod._nearest_positive_definite(A), A)


def test_exactly_singular_duplicate_players_terminates(app_mod):
    """Two perfectly-correlated, equal-variance players -> exactly singular."""
    n = 60
    rng = np.random.default_rng(1)
    sd = rng.uniform(3, 12, n)
    F = rng.normal(size=(n, 5)) * 0.3
    R = np.clip(F @ F.T, -0.6, 0.6)
    np.fill_diagonal(R, 1.0)
    R[n - 1, :] = R[n - 2, :]
    R[:, n - 1] = R[:, n - 2]
    R[n - 1, n - 1] = R[n - 2, n - 2] = R[n - 2, n - 1] = R[n - 1, n - 2] = 1.0
    sd[n - 1] = sd[n - 2]
    D = np.diag(sd)
    out = app_mod._nearest_positive_definite(D @ R @ D)
    assert _is_pd(out)


def test_all_ones_rank_one_matrix_terminates(app_mod):
    """Rank-1 matrix at DFS-realistic scale (entries ~100): min eig is exactly 0."""
    out = app_mod._nearest_positive_definite(np.full((40, 40), 100.0))
    assert _is_pd(out)


def test_zero_matrix_terminates(app_mod):
    assert _is_pd(app_mod._nearest_positive_definite(np.zeros((10, 10))))


def test_optimize_endpoint_is_not_async(app_mod):
    """async def would run the CPU-bound optimizer on the event loop."""
    assert not inspect.iscoroutinefunction(app_mod.optimize_lineups)
