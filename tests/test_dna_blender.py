"""
tests/test_dna_blender.py
==========================
# Status: live | v1.0.0 | 2026-07-22

Unit tests for the 2026 DNA blending pipeline (Phases 2 and 7a):
  - taper_weights() / steady_state_blend() / blend_player_dna() (dna_blender_v_0_1_0)
  - interpolate_curve() / resolve_rookie_curves() (rookie_curves_v_0_1_0)
  - rolling_average() and n<4-games handling (rolling_stats_v_0_1_0)
  - classify_zone() boundary conditions (rolling_stats_v_0_1_0)

Run from repo root:
    python -m pytest tests/test_dna_blender.py -v
"""

import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from src.data_pipeline.dna_blender_v_0_1_0 import (
    taper_weights, steady_state_blend, blend_player_dna, blend_team_dna, pooled_volume_blend,
)
from src.data_pipeline.rookie_curves_v_0_1_0 import interpolate_curve, resolve_rookie_curves
from src.data_pipeline.rolling_stats_v_0_1_0 import (
    rolling_average, compute_season_to_date, compute_l4, classify_zone,
    build_player_game_log, rolling_stats_for_player,
    compute_season_to_date_volume, compute_l4_volume, rolling_volume_for_fields,
    GOALLINE_YARDLINE, REDZONE_YARDLINE,
)

import pandas as pd


class TestTaperWeights(unittest.TestCase):
    def test_game_1_is_full_projection(self):
        self.assertEqual(taper_weights(1), (1.0, 0.0))

    def test_game_5_boundary(self):
        self.assertEqual(taper_weights(5), (0.2, 0.8))

    def test_game_6_is_full_actual_no_projection(self):
        self.assertEqual(taper_weights(6), (0.0, 1.0))

    def test_game_beyond_6_still_full_actual(self):
        self.assertEqual(taper_weights(18), (0.0, 1.0))

    def test_taper_schedule_monotonic_decreasing(self):
        proj_weights = [taper_weights(g)[0] for g in range(1, 7)]
        for a, b in zip(proj_weights, proj_weights[1:]):
            self.assertGreater(a, b)

    def test_invalid_game_number_raises(self):
        with self.assertRaises(ValueError):
            taper_weights(0)


class TestSteadyStateBlend(unittest.TestCase):
    def test_two_thirds_l4_one_third_season(self):
        l4 = {"target_share": 0.30}
        season = {"target_share": 0.18}
        out = steady_state_blend(l4, season)
        expected = (2 / 3) * 0.30 + (1 / 3) * 0.18
        self.assertAlmostEqual(out["target_share"], expected)

    def test_only_shared_fields_blended(self):
        l4 = {"a": 1.0, "b": 2.0}
        season = {"a": 3.0}
        out = steady_state_blend(l4, season)
        self.assertEqual(set(out.keys()), {"a"})

    def test_none_values_skipped(self):
        l4 = {"a": None}
        season = {"a": 5.0}
        out = steady_state_blend(l4, season)
        self.assertNotIn("a", out)


class TestBlendPlayerDnaTaperFallback(unittest.TestCase):
    """No projection_volume/season_volume given -- blend_player_dna() falls
    back to the original fixed-taper mechanism unchanged (rookies,
    zone-split fields, and team defense all call it this way, see
    dna_blender_v_0_1_0.py's module docstring)."""

    def test_game_1_is_pure_projection(self):
        projection = {"target_share": 0.25}
        out = blend_player_dna(projection, l4_actual={}, season_actual={}, game_number=1)
        self.assertAlmostEqual(out["target_share"], 0.25)

    def test_game_3_blends_projection_and_actual(self):
        projection = {"target_share": 0.25}
        season_actual = {"target_share": 0.10}
        out = blend_player_dna(projection, l4_actual={}, season_actual=season_actual, game_number=3)
        expected = 0.6 * 0.25 + 0.4 * 0.10
        self.assertAlmostEqual(out["target_share"], expected)

    def test_game_6_uses_steady_state_ignoring_projection_weight(self):
        projection = {"target_share": 0.25}
        l4_actual = {"target_share": 0.30}
        season_actual = {"target_share": 0.20}
        out = blend_player_dna(projection, l4_actual, season_actual, game_number=6)
        expected = (2 / 3) * 0.30 + (1 / 3) * 0.20
        self.assertAlmostEqual(out["target_share"], expected)

    def test_curve_override_replaces_projection_during_taper(self):
        projection = {"target_share": 0.05}
        out = blend_player_dna(
            projection, l4_actual={}, season_actual={}, game_number=1,
            curve_override={"target_share": 0.11},
        )
        self.assertAlmostEqual(out["target_share"], 0.11)

    def test_field_only_in_projection_passes_through(self):
        projection = {"elusiveness": -0.05}
        out = blend_player_dna(projection, l4_actual={}, season_actual={}, game_number=3)
        self.assertAlmostEqual(out["elusiveness"], -0.05)

    def test_blend_team_dna_matches_blend_player_dna(self):
        projection = {"def_pressure_rate": 0.18}
        season_actual = {"def_pressure_rate": 0.22}
        player_out = blend_player_dna(projection, {}, season_actual, game_number=3)
        team_out = blend_team_dna(projection, {}, season_actual, game_number=3)
        self.assertEqual(player_out, team_out)


class TestPooledVolumeBlend(unittest.TestCase):
    def test_pools_by_real_sample_size_not_equal_weight(self):
        hist_rate = {"cpoe": 3.0}
        hist_volume = {"cpoe": 900}
        season_rate = {"cpoe": -10.0}
        season_volume = {"cpoe": 30}
        out = pooled_volume_blend(hist_rate, hist_volume, season_rate, season_volume)
        expected = (900 * 3.0 + 30 * -10.0) / (900 + 30)
        self.assertAlmostEqual(out["cpoe"], expected)

    def test_prefers_season_when_hist_has_no_volume(self):
        out = pooled_volume_blend({"cpoe": 3.0}, {"cpoe": 0}, {"cpoe": -10.0}, {"cpoe": 30})
        self.assertAlmostEqual(out["cpoe"], -10.0)

    def test_prefers_hist_when_season_has_no_volume(self):
        out = pooled_volume_blend({"cpoe": 3.0}, {"cpoe": 900}, {}, {})
        self.assertAlmostEqual(out["cpoe"], 3.0)

    def test_missing_from_both_omitted(self):
        out = pooled_volume_blend({"a": 1.0}, {"a": 10}, {"b": 2.0}, {"b": 10})
        self.assertNotIn("c", out)
        self.assertIn("a", out)
        self.assertIn("b", out)


class TestBlendPlayerDnaVolumePooled(unittest.TestCase):
    """projection_volume/season_volume both given -- the new mechanism from
    Phase 3 of docs/implementation_plans/volume_weighted_dna_blend_plan.md."""

    def test_reproduces_the_motivating_love_scenario(self):
        # Real 2026 numbers: preseason_projection.cpoe=2.954 over 903
        # historical attempts; week-1 actual cpoe=-9.654 over 32 attempts.
        # The old fixed 80/20 taper produced 0.432 -- a >2.5-point swing off
        # one bad game. Pooled by real sample size, week 1 should pull far
        # less weight (32 of 935 total attempts, not a flat 20%).
        projection = {"cpoe": 2.954}
        projection_volume = {"cpoe": 903}
        season_actual = {"cpoe": -9.654}
        season_volume = {"cpoe": 32}
        out = blend_player_dna(
            projection, l4_actual={}, season_actual=season_actual, game_number=2,
            projection_volume=projection_volume, season_volume=season_volume,
        )
        expected = (903 * 2.954 + 32 * -9.654) / (903 + 32)
        self.assertAlmostEqual(out["cpoe"], expected)
        self.assertGreater(out["cpoe"], 2.0)  # nowhere near the old 80/20 result of 0.432

    def test_game_1_no_real_data_yet_is_pure_historical(self):
        out = blend_player_dna(
            {"cpoe": 3.0}, l4_actual={}, season_actual={}, game_number=1,
            projection_volume={"cpoe": 900}, season_volume={},
        )
        self.assertAlmostEqual(out["cpoe"], 3.0)

    def test_game_4_still_pools_against_historical(self):
        out = blend_player_dna(
            {"cpoe": 3.0}, l4_actual={"cpoe": -2.0}, season_actual={"cpoe": -2.0}, game_number=4,
            projection_volume={"cpoe": 900}, season_volume={"cpoe": 100},
        )
        expected = (900 * 3.0 + 100 * -2.0) / 1000
        self.assertAlmostEqual(out["cpoe"], expected)

    def test_game_5_drops_historical_anchor_uses_l4_only(self):
        # Even with a large historical anchor available, game 5+ ignores it
        # entirely and uses the real trailing-4-game window.
        out = blend_player_dna(
            {"cpoe": 8.0}, l4_actual={"cpoe": -2.0}, season_actual={"cpoe": -1.0}, game_number=5,
            projection_volume={"cpoe": 900}, season_volume={"cpoe": 150},
        )
        self.assertAlmostEqual(out["cpoe"], -2.0)

    def test_steady_state_falls_back_to_season_then_projection_when_l4_missing(self):
        out = blend_player_dna(
            {"cpoe": 8.0, "ypc": 4.0}, l4_actual={"cpoe": None}, season_actual={"cpoe": -1.0}, game_number=6,
            projection_volume={"cpoe": 900}, season_volume={"cpoe": 150},
        )
        self.assertAlmostEqual(out["cpoe"], -1.0)  # l4 missing -> season
        self.assertAlmostEqual(out["ypc"], 4.0)  # missing from both actuals -> projection

    def test_missing_either_volume_falls_back_to_taper(self):
        # Same inputs as the taper-fallback test above, but with only ONE
        # volume dict supplied -- still routes to the legacy mechanism.
        projection = {"target_share": 0.25}
        season_actual = {"target_share": 0.10}
        out = blend_player_dna(
            projection, l4_actual={}, season_actual=season_actual, game_number=3,
            projection_volume={"target_share": 900},  # season_volume omitted (None)
        )
        expected = 0.6 * 0.25 + 0.4 * 0.10  # the fixed-taper result, not a pooled one
        self.assertAlmostEqual(out["target_share"], expected)

    def test_invalid_game_number_raises_in_pooled_path_too(self):
        with self.assertRaises(ValueError):
            blend_player_dna(
                {"cpoe": 3.0}, l4_actual={}, season_actual={}, game_number=0,
                projection_volume={"cpoe": 900}, season_volume={},
            )

    def test_blend_team_dna_forwards_volume_args(self):
        projection = {"def_pressure_rate": 0.18}
        projection_volume = {"def_pressure_rate": 500}
        season_actual = {"def_pressure_rate": 0.22}
        season_volume = {"def_pressure_rate": 50}
        player_out = blend_player_dna(projection, {}, season_actual, game_number=3,
                                       projection_volume=projection_volume, season_volume=season_volume)
        team_out = blend_team_dna(projection, {}, season_actual, game_number=3,
                                   projection_volume=projection_volume, season_volume=season_volume)
        self.assertEqual(player_out, team_out)


class TestInterpolateCurve(unittest.TestCase):
    def test_before_start_week_is_early_value(self):
        self.assertEqual(interpolate_curve(1, 5, 0.04, 0.11, game_number=1), 0.04)

    def test_at_steady_week_is_late_value(self):
        self.assertEqual(interpolate_curve(1, 5, 0.04, 0.11, game_number=5), 0.11)

    def test_past_steady_week_stays_late_value(self):
        self.assertEqual(interpolate_curve(1, 5, 0.04, 0.11, game_number=10), 0.11)

    def test_midpoint_is_linear(self):
        # start=1, steady=5 -> game 3 is the midpoint (frac=0.5)
        val = interpolate_curve(1, 5, 0.0, 1.0, game_number=3)
        self.assertAlmostEqual(val, 0.5)

    def test_degenerate_steady_at_or_before_start_jumps_immediately(self):
        self.assertEqual(interpolate_curve(3, 3, 0.0, 1.0, game_number=1), 1.0)

    def test_resolve_rookie_curves_multiple_fields(self):
        spec = {
            "target_share": {"start_week": 1, "steady_week": 5, "early_value": 0.04, "late_value": 0.11},
            "carry_share": {"start_week": 1, "steady_week": 4, "early_value": 0.35, "late_value": 0.62},
        }
        out = resolve_rookie_curves(spec, game_number=1)
        self.assertAlmostEqual(out["target_share"], 0.04)
        self.assertAlmostEqual(out["carry_share"], 0.35)


class TestRollingAverage(unittest.TestCase):
    def test_season_to_date_averages_all_played_weeks(self):
        game_values = {1: 10, 2: 20, 3: 30}
        self.assertAlmostEqual(compute_season_to_date(game_values, through_week=3), 20.0)

    def test_l4_with_fewer_than_4_games_uses_available(self):
        game_values = {1: 10, 2: 20}
        self.assertAlmostEqual(compute_l4(game_values, through_week=2), 15.0)

    def test_l4_with_5_games_uses_last_4_only(self):
        game_values = {1: 0, 2: 10, 3: 20, 4: 30, 5: 40}
        self.assertAlmostEqual(compute_l4(game_values, through_week=5), 25.0)

    def test_future_weeks_excluded(self):
        game_values = {1: 10, 2: 999}
        self.assertAlmostEqual(compute_season_to_date(game_values, through_week=1), 10.0)

    def test_no_games_returns_none(self):
        self.assertIsNone(compute_season_to_date({}, through_week=1))

    def test_l4_and_season_converge_at_exactly_4_games(self):
        game_values = {1: 5, 2: 10, 3: 15, 4: 20}
        self.assertAlmostEqual(
            compute_l4(game_values, through_week=4),
            compute_season_to_date(game_values, through_week=4),
        )


class TestBuildPlayerGameLogScrambleRate(unittest.TestCase):
    """Regression test: qb_scramble is only ever 1 on play_type == "run" rows
    (a scramble is coded as a run, not a pass), so scramble_rate must be
    pulled from pbp_run, not averaged over the pass-only slice -- that
    previously returned 0.0 for every QB, every week."""

    def _row(self, play_type, week, **overrides):
        row = {
            "play_type": play_type, "week": week,
            "passer_player_id": None, "rusher_player_id": None,
            "receiver_player_id": None, "pass_attempt": 0, "sack": 0,
            "qb_scramble": 0, "cpoe": None, "air_yards": None, "yards_gained": None,
        }
        row.update(overrides)
        return row

    def test_scramble_rate_counts_scrambles_from_run_plays(self):
        rows = [
            self._row("pass", 1, passer_player_id="QB1", pass_attempt=1, cpoe=5.0, air_yards=8.0),
            self._row("pass", 1, passer_player_id="QB1", pass_attempt=1, cpoe=3.0, air_yards=6.0),
            self._row("pass", 1, passer_player_id="QB1", pass_attempt=1, cpoe=-1.0, air_yards=10.0),
            self._row("run", 1, rusher_player_id="QB1", qb_scramble=1, yards_gained=7),
        ]
        log, volume = build_player_game_log(pd.DataFrame(rows), "QB1", "QB")
        self.assertAlmostEqual(log["scramble_rate"][1], 0.25)  # 1 scramble / (1 scramble + 3 attempts)
        self.assertEqual(volume["scramble_rate"][1], 4)  # dropbacks: 3 attempts + 1 scramble

    def test_scramble_rate_zero_when_truly_no_scrambles(self):
        rows = [self._row("pass", 1, passer_player_id="QB1", pass_attempt=1, cpoe=5.0, air_yards=8.0)]
        log, volume = build_player_game_log(pd.DataFrame(rows), "QB1", "QB")
        self.assertAlmostEqual(log["scramble_rate"][1], 0.0)
        self.assertEqual(volume["scramble_rate"][1], 1)


class TestBuildPlayerGameLogVolume(unittest.TestCase):
    """Phase 1 of docs/implementation_plans/volume_weighted_dna_blend_plan.md:
    volume mirrors log's keys exactly, valued at each field's real weekly
    sample size (not just whether data exists)."""

    def _row(self, play_type, week, **overrides):
        row = {
            "play_type": play_type, "week": week,
            "passer_player_id": None, "rusher_player_id": None,
            "receiver_player_id": None, "pass_attempt": 0, "sack": 0,
            "qb_scramble": 0, "cpoe": None, "air_yards": None,
            "yards_gained": None, "yards_after_catch": None, "complete_pass": 0,
        }
        row.update(overrides)
        return row

    def test_qb_cpoe_volume_is_pass_attempts(self):
        rows = [
            self._row("pass", 1, passer_player_id="QB1", pass_attempt=1, cpoe=5.0, air_yards=8.0),
            self._row("pass", 1, passer_player_id="QB1", pass_attempt=1, cpoe=3.0, air_yards=6.0),
        ]
        log, volume = build_player_game_log(pd.DataFrame(rows), "QB1", "QB")
        self.assertEqual(volume["cpoe"][1], 2)

    def test_receiver_catch_rate_volume_is_targets(self):
        rows = [
            self._row("pass", 1, receiver_player_id="WR1", complete_pass=1, air_yards=8.0),
            self._row("pass", 1, receiver_player_id="WR1", complete_pass=0, air_yards=12.0),
            self._row("pass", 1, receiver_player_id="WR1", complete_pass=1, air_yards=5.0),
        ]
        log, volume = build_player_game_log(pd.DataFrame(rows), "WR1", "WR")
        self.assertAlmostEqual(log["catch_rate"][1], 2 / 3)
        self.assertEqual(volume["catch_rate"][1], 3)

    def test_rusher_ypc_volume_is_carries(self):
        rows = [
            self._row("run", 1, rusher_player_id="RB1", yards_gained=4),
            self._row("run", 1, rusher_player_id="RB1", yards_gained=6),
            self._row("run", 1, rusher_player_id="RB1", yards_gained=2),
        ]
        log, volume = build_player_game_log(pd.DataFrame(rows), "RB1", "RB")
        self.assertAlmostEqual(log["ypc"][1], 4.0)
        self.assertEqual(volume["ypc"][1], 3)

    def test_volume_keys_match_log_keys(self):
        rows = [
            self._row("pass", 1, receiver_player_id="WR1", complete_pass=1, air_yards=8.0),
            self._row("run", 1, rusher_player_id="WR1", yards_gained=3),
        ]
        log, volume = build_player_game_log(pd.DataFrame(rows), "WR1", "WR")
        self.assertEqual(set(log.keys()), set(volume.keys()))


class TestRollingSum(unittest.TestCase):
    def test_season_volume_sums_all_played_weeks(self):
        game_values = {1: 10, 2: 20, 3: 30}
        self.assertEqual(compute_season_to_date_volume(game_values, through_week=3), 60)

    def test_l4_volume_uses_last_4_only(self):
        game_values = {1: 5, 2: 10, 3: 15, 4: 20, 5: 25}
        self.assertEqual(compute_l4_volume(game_values, through_week=5), 70)  # weeks 2-5

    def test_future_weeks_excluded(self):
        game_values = {1: 10, 2: 999}
        self.assertEqual(compute_season_to_date_volume(game_values, through_week=1), 10)

    def test_no_games_returns_zero_not_none(self):
        self.assertEqual(compute_season_to_date_volume({}, through_week=1), 0)

    def test_rolling_volume_for_fields_mirrors_rolling_stats_for_fields(self):
        per_field = {"cpoe": {1: 30, 2: 35}, "catch_rate": {1: 5}}
        season, l4 = rolling_volume_for_fields(per_field, through_week=2)
        self.assertEqual(season["cpoe"], 65)
        self.assertEqual(season["catch_rate"], 5)
        self.assertEqual(l4["cpoe"], 65)


class TestRollingStatsForPlayerVolume(unittest.TestCase):
    """rolling_stats_for_player() now returns a 4-tuple (season, l4,
    season_volume, l4_volume) -- volume is additive, the rate calculation
    itself (still an unweighted per-game mean) is unchanged."""

    def _row(self, play_type, week, **overrides):
        row = {
            "play_type": play_type, "week": week,
            "passer_player_id": None, "rusher_player_id": None,
            "receiver_player_id": None, "pass_attempt": 0, "sack": 0,
            "qb_scramble": 0, "cpoe": None, "air_yards": None,
            "yards_gained": None, "complete_pass": 0,
        }
        row.update(overrides)
        return row

    def test_returns_four_tuple_with_pooled_volume(self):
        rows = [
            self._row("pass", 1, passer_player_id="QB1", pass_attempt=1, cpoe=10.0, air_yards=8.0),
            self._row("pass", 1, passer_player_id="QB1", pass_attempt=1, cpoe=8.0, air_yards=6.0),
            self._row("pass", 2, passer_player_id="QB1", pass_attempt=1, cpoe=-4.0, air_yards=5.0),
        ]
        season, l4, season_volume, l4_volume = rolling_stats_for_player(
            pd.DataFrame(rows), "QB1", "QB", through_week=2,
        )
        # season["cpoe"] is still a mean-of-weekly-means (week 1: (10+8)/2=9,
        # week 2: -4 -> (9 + -4)/2 = 2.5), unchanged by this phase -- volume
        # is purely additive, doesn't touch the existing rate calculation.
        self.assertAlmostEqual(season["cpoe"], 2.5)
        self.assertEqual(season_volume["cpoe"], 3)  # real pass-attempt count behind it
        self.assertEqual(l4_volume["cpoe"], 3)


class TestClassifyZone(unittest.TestCase):
    def test_goalline_boundary(self):
        self.assertEqual(classify_zone(GOALLINE_YARDLINE), "goalline")
        self.assertEqual(classify_zone(GOALLINE_YARDLINE + 1), "redzone")

    def test_redzone_boundary(self):
        self.assertEqual(classify_zone(REDZONE_YARDLINE), "redzone")
        self.assertEqual(classify_zone(REDZONE_YARDLINE + 1), "primary")

    def test_deep_primary(self):
        self.assertEqual(classify_zone(75), "primary")


if __name__ == "__main__":
    unittest.main()
