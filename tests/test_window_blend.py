"""Hand-computed checks for src/data_pipeline/window_blend_v_0_1_0.py."""
import pytest

from src.data_pipeline import window_blend_v_0_1_0 as wb

TEAM = [1, 2, 3, 4, 5, 6]


def _blend(vals, volume=None, active=None, through=4, pre=None, scheme="flat",
           share=False, minv=0, fill="mean", team=None):
    team = team or TEAM
    volume = volume if volume is not None else {w: 10 for w in vals}
    active = active if active is not None else set(vals)
    return wb.window_blend(vals, volume, team, active, through, pre, scheme, share, minv, fill)


def test_scheme_weights_sum_to_one():
    for s in wb.SCHEMES.values():
        assert s["season"] + sum(s["games"]) == pytest.approx(1.0)
        assert s["games"] == sorted(s["games"])  # newest game weighted most


def test_flat_equals_plain_mean_when_season_is_window():
    # Four games, season == the same four -> every scheme input averages to 0.5
    v = {1: .2, 2: .4, 3: .6, 4: .8}
    assert _blend(v) == pytest.approx(0.5)


def test_ramp_tilts_toward_recent():
    v = {1: .2, 2: .4, 3: .6, 4: .8}
    # season .5*.20 + .2*.16 + .4*.18 + .6*.22 + .8*.24 = .1+.032+.072+.132+.192
    assert _blend(v, scheme="ramp_a") == pytest.approx(0.528)
    assert _blend(v, scheme="ramp_b") > _blend(v, scheme="ramp_a") > _blend(v)


def test_one_missed_game_filled_with_mean_or_median():
    # week 3 missed (no entry, not active). Window 1-4 -> good = .2,.4,.9 ; mean=.5, median=.4
    v = {1: .2, 2: .4, 4: .9}
    assert wb.select_window(v, {w: 10 for w in v}, TEAM, set(v), 4, fill="mean") == pytest.approx([.2, .4, .5, .9])
    assert wb.select_window(v, {w: 10 for w in v}, TEAM, set(v), 4, fill="median") == pytest.approx([.2, .4, .4, .9])


def test_two_missed_games_reach_back_past_window():
    # through week 6, window = 3..6; he missed 4 and 5 -> uses weeks 1,2,3,6
    v = {1: .1, 2: .2, 3: .3, 6: .6}
    got = wb.select_window(v, {w: 10 for w in v}, TEAM, set(v), 6)
    assert got == pytest.approx([.1, .2, .3, .6])


def test_not_enough_games_falls_back_to_preseason():
    v = {1: .5}
    got = wb.select_window(v, {1: 10}, TEAM, {1}, 1)
    assert got == [None, None, None, .5]
    # preseason .3 fills three game slots; season .5 + one game .5 => 0.2*.5+.24*.5 (flat: .2 each)
    out = _blend(v, through=1, pre=0.3)
    assert out == pytest.approx(0.2 * .5 + 0.2 * .5 + 0.6 * .3)


def test_no_preseason_renormalises():
    assert _blend({1: .5}, through=1, pre=None) == pytest.approx(0.5)


def test_ratio_game_below_min_volume_is_not_qualifying():
    v = {1: .5, 2: .5, 3: 1.0, 4: .5}
    vol = {1: 8, 2: 8, 3: 1, 4: 8}          # week 3: one target, 100% catch rate
    got = wb.select_window(v, vol, TEAM, set(v), 4, min_volume=3, fill="mean")
    assert got == pytest.approx([.5, .5, .5, .5])  # week 3 replaced by mean of the others


def test_share_active_zero_game_counts_as_zero_not_missing():
    v = {1: .2, 2: .2, 4: .2}                 # week 3 active but no targets -> absent from log
    got = wb.select_window(v, {}, TEAM, {1, 2, 3, 4}, 4, is_share=True)
    assert got == pytest.approx([.2, .2, 0.0, .2])


def test_bye_week_not_a_team_game():
    team = [1, 2, 3, 5, 6]                    # bye week 4
    v = {1: .1, 2: .2, 3: .3, 5: .5}
    got = wb.select_window(v, {w: 10 for w in v}, team, set(v), 6, is_share=False)
    # window = 2,3,5,6; week 6 missing -> filled by mean(.2,.3,.5)
    assert got == pytest.approx([.2, .3, .5, (.2 + .3 + .5) / 3])


def test_season_value_volume_pools_ratios():
    v = {1: 1.0, 2: 0.0}
    vol = {1: 1, 2: 9}
    assert wb.season_value(v, vol, [1, 2], {1, 2}, 2) == pytest.approx(0.1)


def test_zone_blend_anchor_and_min_samples():
    assert wb.zone_blend(0.4, 6, 0.2, anchor=0.5, min_samples=5) == pytest.approx(0.3)
    assert wb.zone_blend(0.4, 4, 0.2, anchor=0.5, min_samples=5) == 0.2      # thin sample -> preseason only
    assert wb.zone_blend(0.4, 6, None) == 0.4


def test_usage_shift_flags():
    assert wb.usage_shift_flags({"A": .30, "B": .10}, {"A": .20, "B": .09}) == [("A", pytest.approx(.10))]
