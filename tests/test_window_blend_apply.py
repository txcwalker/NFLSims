"""Plumbing checks for src/data_pipeline/window_blend_apply_v_0_1_0.py on a tiny synthetic season."""
import pandas as pd
import pytest

from src.data_pipeline import window_blend_apply_v_0_1_0 as wa
from src.data_pipeline import window_blend_v_0_1_0 as wb
from src.data_pipeline.rolling_stats_v_0_1_0 import build_team_week_totals, build_team_week_zone_totals


def _pbp():
    """Team A (vs B) plays weeks 1-6. Each week: WR 'w1' gets 3 targets (2 caught), WR 'w2' gets 1;
    week 5 w1 sits out. One red-zone target per week goes to w1 (weeks 1-4) / w2 (5-6)."""
    rows = []
    for wk in range(1, 7):
        w1_active = wk != 5
        for i in range(3 if w1_active else 0):
            rows.append(dict(week=wk, play_type="pass", posteam="A", defteam="B", receiver_player_id="w1",
                             rusher_player_id=None, passer_player_id="qb", complete_pass=1 if i < 2 else 0,
                             air_yards=8.0, yards_after_catch=3.0, yards_gained=8, yardline_100=60,
                             pass_attempt=1, sack=0, qb_scramble=0, qb_hit=0, cpoe=0.0, xyac_mean_yardage=3.0))
        rows.append(dict(week=wk, play_type="pass", posteam="A", defteam="B", receiver_player_id="w2",
                         rusher_player_id=None, passer_player_id="qb", complete_pass=1, air_yards=5.0,
                         yards_after_catch=2.0, yards_gained=5, yardline_100=60, pass_attempt=1, sack=0,
                         qb_scramble=0, qb_hit=0, cpoe=0.0, xyac_mean_yardage=2.0))
        rows.append(dict(week=wk, play_type="pass", posteam="A", defteam="B",
                         receiver_player_id="w1" if wk <= 4 else "w2", rusher_player_id=None, passer_player_id="qb",
                         complete_pass=1, air_yards=5.0, yards_after_catch=2.0, yards_gained=5, yardline_100=15,
                         pass_attempt=1, sack=0, qb_scramble=0, qb_hit=0, cpoe=0.0, xyac_mean_yardage=2.0))
    return pd.DataFrame(rows)


def test_zone_share_pools_last_window_games():
    pbp = _pbp()
    ctx = wa.build_context(pbp)
    zt = build_team_week_zone_totals(pbp)
    traits = {"player_id": "w1", "pos": "WR"}
    # last 8 team games = weeks 1-6; w1 had the red-zone target in weeks 1-4 of 6 -> 4/6
    got = wa.player_zone_values(traits, ctx, zt, completed_week=6)
    assert got["redzone"]["target_share"] == pytest.approx(4 / 6)
    # window of 2 = weeks 5-6: he is active in week 6 but got no red-zone target in either -> a real 0.0
    assert wa.player_zone_values(traits, ctx, zt, 6, window=2)["redzone"]["target_share"] == 0.0
    # a player with no plays at all in the window is omitted (legacy/preseason value stays)
    assert wa.player_zone_values({"player_id": "nobody"}, ctx, zt, 6) == {}


def test_flat_values_match_direct_window_blend():
    pbp = _pbp()
    ctx = wa.build_context(pbp)
    tt = build_team_week_totals(pbp)
    traits = {"player_id": "w1", "pos": "WR", "preseason_projection": {"target_share": 0.30, "catch_rate": 0.60}}
    got = wa.player_flat_values(traits, pbp, ctx, tt, completed_week=6)
    # w1 target_share: 4 of 5 team targets (3 + the red-zone one) in weeks 1-4, 3 of 5 in week 6;
    # week 5 he had no plays at all -> a missed game, filled with the mean of his other window games
    log = {1: 0.8, 2: 0.8, 3: 0.8, 4: 0.8, 6: 0.6}
    direct = wb.window_blend(log, {w: 3 for w in log}, set(range(1, 7)), set(log), 6, 0.30, "flat", True, 0, "mean")
    assert got["target_share"] == pytest.approx(direct)  # no preseason anchor (Cam 2026-10-07)
    # catch_rate: weeks 1-4 he caught 3 of 4 targets (incl. the red-zone one), week 6 2 of 3; week 5 missed.
    # ratio anchor = 50% preseason; min volume 3 targets per game
    clog = {1: .75, 2: .75, 3: .75, 4: .75, 6: 2 / 3}
    cvol = {1: 4, 2: 4, 3: 4, 4: 4, 6: 3}
    cdirect = wb.window_blend(clog, cvol, set(range(1, 7)), set(clog), 6, 0.60, "flat", False, 3, "mean")
    assert got["catch_rate"] == pytest.approx(cdirect)  # no preseason anchor (Cam 2026-10-07)


def test_player_without_id_returns_nothing():
    pbp = _pbp()
    assert wa.player_flat_values({"pos": "WR"}, pbp, wa.build_context(pbp), {}, 6) == {}


def _qb_run_pbp():
    """Team A, weeks 1-6. Each week 10 run plays: QB 2 scrambles + 1 designed (sneak at the goal line),
    RB 7 carries (2 of them at the goal line); a scramble also happens at the goal line."""
    rows = []

    def run(wk, rusher, yl, scr):
        rows.append(dict(week=wk, play_type="run", posteam="A", defteam="B", receiver_player_id=None,
                         rusher_player_id=rusher, passer_player_id=None, qb_scramble=scr, yardline_100=yl,
                         yards_gained=3, complete_pass=0, air_yards=0.0, yards_after_catch=0.0, pass_attempt=0,
                         sack=0, qb_hit=0, cpoe=0.0, xyac_mean_yardage=0.0))
    for wk in range(1, 7):
        run(wk, "qb", 60, 1)
        run(wk, "qb", 3, 1)            # goal-line scramble
        run(wk, "qb", 3, 0)            # goal-line sneak (designed)
        for _ in range(2):
            run(wk, "rb", 3, 0)
        for _ in range(5):
            run(wk, "rb", 60, 0)
    return pd.DataFrame(rows)


def test_qb_carry_share_counts_designed_runs_only_not_scrambles():
    """Real 2026 designed QB runs (sneaks etc.) are used; scrambles are scramble_rate, not carry_share."""
    pbp = _qb_run_pbp()
    ctx = wa.build_context(pbp)
    tt = build_team_week_totals(pbp)
    qb = {"player_id": "qb", "pos": "QB", "preseason_projection": {"carry_share": 0.02}}
    got = wa.player_flat_values(qb, pbp, ctx, tt, 6)
    # 1 designed run of the team's 10 run plays (scrambles stay in the denominator) = 0.10 every week
    assert got["carry_share"] == pytest.approx(0.10)  # no preseason anchor (Cam 2026-10-07)
    # goal line: 1 sneak of the team's 4 goal-line run plays per week -> pooled 6/24 = 0.25 (scrambles excluded)
    zt = build_team_week_zone_totals(pbp)
    assert wa.player_zone_values(qb, ctx, zt, 6)["goalline"]["carry_share"] == pytest.approx(0.25)


def test_preseason_has_no_influence_after_four_games():
    """Cam 2026-10-07: once a player has played 4 games, preseason must not move his values --
    a game under the min-volume bar falls back to his OWN pooled 2026 rate, and a share he never
    earned (no carries) is a real 0.0, not his preseason share."""
    pbp = _pbp()
    ctx = wa.build_context(pbp)
    tt = build_team_week_totals(pbp)
    outs = []
    for pre_catch, pre_carry in ((0.10, 0.30), (0.95, 0.01)):  # wildly different preseason inputs
        w2 = {"player_id": "w2", "pos": "WR",
              "preseason_projection": {"catch_rate": pre_catch, "carry_share": pre_carry, "target_share": 0.5}}
        outs.append(wa.player_flat_values(w2, pbp, ctx, tt, 6))
    assert outs[0] == outs[1]
    assert outs[0]["catch_rate"] == pytest.approx(1.0)   # every w2 target was caught; only his own data counts
    assert outs[0]["carry_share"] == pytest.approx(0.0)  # on the field, never carried -> 0, not preseason


def test_preseason_still_fills_for_a_player_with_few_games():
    pbp = _pbp()
    ctx = wa.build_context(pbp)
    tt = build_team_week_totals(pbp)
    # through week 2 = only 2 games played (< 4): empty slots may still use preseason
    w2 = {"player_id": "w2", "pos": "WR", "preseason_projection": {"catch_rate": 0.10}}
    low = wa.player_flat_values(w2, pbp, ctx, tt, 2)["catch_rate"]
    w2["preseason_projection"]["catch_rate"] = 0.90
    high = wa.player_flat_values(w2, pbp, ctx, tt, 2)["catch_rate"]
    assert high > low
