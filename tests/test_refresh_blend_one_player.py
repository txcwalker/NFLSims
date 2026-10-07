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


def test_qb_carry_share_stays_at_preseason_in_legacy_mode_too():
    t = _traits("QB", carry_share=0.2)  # a PBP-blended value that includes scrambles
    t["splits"]["goalline"]["carry_share"] = 0.9
    refresh.blend_one_player(t, EMPTY_PBP, {}, {}, None, None, 4, 5, {})
    assert t["carry_share"] == pytest.approx(0.05)
    assert t["splits"]["goalline"]["carry_share"] == pytest.approx(0.02)
    assert t["splits"]["redzone"]["carry_share"] == pytest.approx(0.04)
