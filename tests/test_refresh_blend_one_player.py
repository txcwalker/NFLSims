"""Regression tests for refresh_weekly_dna_v_0_1_0.blend_one_player (2026-10-08 fixes)."""
import copy
import os
import sys

import pandas as pd
import pytest

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, ROOT)
sys.path.insert(0, os.path.join(ROOT, "scripts", "roster_management"))

import refresh_weekly_dna_v_0_1_0 as refresh  # noqa: E402

EMPTY_PBP = pd.DataFrame(columns=["play_type", "week", "posteam", "defteam", "yardline_100"])


def _traits(pos, **extra):
    t = {"pos": pos, "player_id": "x", "_name": "Test Player", "status": "active",
         "target_share": 0.2, "carry_share": 0.05,
         "preseason_projection": {"target_share": 0.2, "carry_share": 0.05,
                                  "splits": {"primary": {"target_share": 0.2, "carry_share": 0.05},
                                             "redzone": {"target_share": 0.3, "carry_share": 0.04},
                                             "goalline": {"target_share": 0.4, "carry_share": 0.02}}},
         "splits": {"primary": {"target_share": 0.2}, "redzone": {"target_share": 0.3}, "goalline": {"target_share": 0.4}}}
    t.update(extra)
    return t


@pytest.mark.parametrize("game_number", [3, 5, 7])
def test_refresh_never_aliases_or_mutates_frozen_preseason_splits(game_number):
    t = _traits("WR")
    frozen = copy.deepcopy(t["preseason_projection"])
    refresh.blend_one_player(t, EMPTY_PBP, {}, {}, None, None, game_number - 1, game_number, {})
    assert t["splits"] is not t["preseason_projection"]["splits"]
    assert t["preseason_projection"] == frozen


def test_rolling_stats_qb_carry_share_excludes_scrambles():
    """Legacy refresh reads these builders: QB carry_share must be designed runs only."""
    from src.data_pipeline.rolling_stats_v_0_1_0 import build_player_game_log, build_team_week_totals
    rows = []
    for wk in (1, 2):
        for scr in (1, 1, 0):  # 2 scrambles + 1 designed QB run
            rows.append(dict(week=wk, play_type="run", posteam="A", defteam="B", rusher_player_id="qb",
                             receiver_player_id=None, passer_player_id=None, qb_scramble=scr, yardline_100=50,
                             yards_gained=4, complete_pass=0, air_yards=0.0, yards_after_catch=0.0,
                             pass_attempt=0, sack=0, qb_hit=0, cpoe=0.0))
        for _ in range(7):
            rows.append(dict(week=wk, play_type="run", posteam="A", defteam="B", rusher_player_id="rb",
                             receiver_player_id=None, passer_player_id=None, qb_scramble=0, yardline_100=50,
                             yards_gained=4, complete_pass=0, air_yards=0.0, yards_after_catch=0.0,
                             pass_attempt=0, sack=0, qb_hit=0, cpoe=0.0))
    pbp = pd.DataFrame(rows)
    log, vol = build_player_game_log(pbp, "qb", "QB", team_totals=build_team_week_totals(pbp))
    assert log["carry_share"] == {1: pytest.approx(0.1), 2: pytest.approx(0.1)}  # 1 designed / 10 team runs
    assert vol["carry_share"] == {1: 1, 2: 1}
    rb_log, _ = build_player_game_log(pbp, "rb", "RB", team_totals=build_team_week_totals(pbp))
    assert rb_log["carry_share"][1] == pytest.approx(0.7)  # non-QBs unaffected
