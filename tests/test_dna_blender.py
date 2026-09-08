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
    taper_weights, steady_state_blend, blend_player_dna, blend_team_dna,
)
from src.data_pipeline.rookie_curves_v_0_1_0 import interpolate_curve, resolve_rookie_curves
from src.data_pipeline.rolling_stats_v_0_1_0 import (
    rolling_average, compute_season_to_date, compute_l4, classify_zone,
    GOALLINE_YARDLINE, REDZONE_YARDLINE,
)


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


class TestBlendPlayerDna(unittest.TestCase):
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
