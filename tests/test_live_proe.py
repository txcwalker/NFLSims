"""Live team PROE (2026-10-08): refresh_live_proe computes mean nflverse pass_oe per
team; proe_overlay.get_coach_proe blends it with the coach's historical PROE via the
existing k=8 shrinkage, and falls back to pure historical when no live file exists."""
import json
import os
import sys

import pandas as pd

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "scripts", "roster_management"))
from refresh_live_proe_v_0_1_0 import compute_live_proe  # noqa: E402
from src.nfl_sim import proe_overlay_v_0_1_0 as po  # noqa: E402


def test_compute_live_proe_filters_and_averages():
    pbp = pd.DataFrame({
        "play_type": ["pass", "run", "pass", "kickoff", "pass"],
        "week": [1, 1, 2, 1, 5],
        "posteam": ["DET", "DET", "DET", "DET", "DET"],
        "game_id": ["a", "a", "b", "a", "e"],
        "pass_oe": [10.0, -20.0, 4.0, 99.0, 50.0],
    })
    out = compute_live_proe(pbp, completed_week=2)["DET"]
    assert out["n_plays"] == 3 and out["n_games"] == 2 and out["pass_oe"] == round((10 - 20 + 4) / 3, 4)


def test_get_coach_proe_blends_live_and_falls_back(tmp_path, monkeypatch):
    team = next(t for t, c in po._load_team_to_coach(2026).items() if c in po._HISTORICAL_PROE)
    monkeypatch.setattr(po, "_load_live_proe", lambda y: {})   # isolate from the real data/dna file
    hist = po.get_coach_proe(team, 2026)
    monkeypatch.setattr(po, "_load_live_proe", lambda y: {team: {"n_games": 4, "pass_oe": hist + 12.0}})
    assert abs(po.get_coach_proe(team, 2026) - (hist + 12.0 * 4 / 12)) < 1e-9   # w_curr = 4/(4+8)
    monkeypatch.setattr(po, "_load_live_proe", lambda y: {})
    assert po.get_coach_proe(team, 2026) == hist


def test_team_pace_hook_scales_only_live_cells_and_preserves_league_mean():
    """_apply_team_pace: 'pace' multiplies by exp(team delta) by game state, skips the last 2:00 of Q2/Q4,
    and 'off' returns the draws untouched."""
    import numpy as np
    from src.nfl_sim.game_engine import NFLGameEngine
    e = NFLGameEngine.__new__(NFLGameEngine)
    e.away_team, e.home_team, e.pace_mode = "AAA", "HHH", "pace"
    e.team_pace_tab = {"AAA": {"pace": np.array([0.1, 0.0, 0.0]), "nh": np.zeros(3)},
                       "HHH": {"pace": np.array([-0.1, 0.0, 0.0]), "nh": np.zeros(3)}}
    e.league_nh_rate, e.nh_log_effect = np.zeros(3), 0.0
    e.possession_is_away = np.array([True, False, True])
    mask = np.array([True, True, True])
    res = np.array([30.0, 30.0, 30.0])
    rg = np.array(["Q1", "Q1", "Q4 <2:00"], dtype=object)
    out = e._apply_team_pace(res, mask, rg, np.array([0, 3, 0]))
    assert abs(out[0] - 30 * np.exp(0.1)) < 1e-9 and abs(out[1] - 30 * np.exp(-0.1)) < 1e-9 and out[2] == 30.0
    e.pace_mode = "off"
    assert (e._apply_team_pace(res, mask, rg, np.array([0, 3, 0])) == res).all()
