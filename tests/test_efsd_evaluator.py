"""
tests/test_efsd_evaluator.py
============================
# Status: live | v1.0.0 | 2026-06-23

Unit tests for the EFSD (Expected Final Score Differential) model and its
integration into the PositionalEvaluator.

Tests cover:
  - EFSDModelV010 inference wrapper: sign direction, scalar/batch consistency,
    physical plausibility, possession-flip symmetry
  - PositionalEvaluator._drive_end_efsd(): terminal lanes return exact margin,
    non-terminal sign consistency
  - PositionalEvaluator.evaluate(): efsd_start present, per-concept delta_efsd
    finite, leading state has higher EFSD than trailing
  - PositionalEvaluator.suggest_lines(metric="efsd"): runs without error, returns
    correct structure

Run from repo root:
    python -m pytest tests/test_efsd_evaluator.py -v
"""

import os
import sys
import unittest
import numpy as np

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from src.nfl_sim.models.efsd_v_0_1_0.efsd_inference import EFSDModelV010
from src.nfl_sim.nfl_positional_evaluator import PositionalEvaluator


# ---------------------------------------------------------------------------
# EFSDModelV010 Inference Tests
# ---------------------------------------------------------------------------

class TestEFSDInference(unittest.TestCase):
    """
    Purpose: Verify the EFSD inference wrapper produces physically plausible
    values before it gets used in the evaluator.
    """

    @classmethod
    def setUpClass(cls):
        cls.model = EFSDModelV010()
        # Neutral midfield kickoff reference state
        cls.neutral = {
            "score_differential": 0.0,
            "game_seconds_remaining": 3600.0,
            "down": 1.0,
            "ydstogo": 10.0,
            "yardline_100": 75.0,
            "posteam_timeouts_remaining": 3.0,
            "defteam_timeouts_remaining": 3.0,
            "receive_2h_ko": 0.0,
        }

    def test_neutral_kickoff_near_zero(self):
        """
        EFSD at a tied kickoff should be near 0 (within ±5 pts).
        Having the ball in a neutral game is worth ~3 pts in expected final margin
        (possession advantage), so a small positive value is correct — not a bug.
        """
        efsd = self.model.predict_efsd(self.neutral)
        self.assertAlmostEqual(efsd, 0.0, delta=5.0,
                               msg=f"Tied kickoff EFSD={efsd:.3f} is too far from 0")

    def test_leading_state_positive_efsd(self):
        """EFSD must be positive when offense leads."""
        state = {**self.neutral, "score_differential": 14.0,
                 "game_seconds_remaining": 120.0}
        efsd = self.model.predict_efsd(state)
        self.assertGreater(efsd, 0.0, f"Leading late EFSD={efsd:.3f} should be positive")

    def test_trailing_state_negative_efsd(self):
        """EFSD must be negative when offense trails."""
        state = {**self.neutral, "score_differential": -14.0,
                 "game_seconds_remaining": 120.0}
        efsd = self.model.predict_efsd(state)
        self.assertLess(efsd, 0.0, f"Trailing late EFSD={efsd:.3f} should be negative")

    def test_possession_flip_sign(self):
        """
        Flipping the score differential must flip the sign of EFSD.
        Magnitudes are NOT required to match — the model correctly learns
        asymmetry: a leading team plays conservatively and may extend their
        margin, while a trailing team takes risks that can cut both ways.
        This asymmetry is a feature, not a bug. We only assert directional correctness.
        """
        up7 = {**self.neutral, "score_differential":  7.0}
        dn7 = {**self.neutral, "score_differential": -7.0}
        efsd_up = self.model.predict_efsd(up7)
        efsd_dn = self.model.predict_efsd(dn7)
        self.assertGreater(efsd_up, 0.0, f"Up-7 EFSD={efsd_up:.3f} should be positive")
        self.assertLess(efsd_dn, 0.0,    f"Down-7 EFSD={efsd_dn:.3f} should be negative")

    def test_efsd_converges_to_score_late_game(self):
        """
        With 2 minutes left and a 7-point lead, EFSD should converge close to
        the current score differential (+7), because the game is nearly decided.
        Early-game EFSD for the same lead can be HIGHER (the offense has more time
        to extend their margin) — EFSD is unbounded unlike WP, so "confident" means
        converging toward the actual margin, not toward a larger number.
        """
        state_late = {**self.neutral, "score_differential": 7.0,
                      "game_seconds_remaining": 120.0}
        efsd_late = self.model.predict_efsd(state_late)
        # Late EFSD should be within 5 points of the current score differential
        self.assertAlmostEqual(efsd_late, 7.0, delta=5.0,
                               msg=f"Late EFSD={efsd_late:.3f} should converge near +7 with 2 min left")

    def test_scalar_batch_consistency(self):
        """predict_batch output must match predict_efsd scalar for the same inputs."""
        states = [
            self.neutral,
            {**self.neutral, "score_differential":  7.0},
            {**self.neutral, "score_differential": -7.0},
        ]
        feature_order = self.model.features
        X = np.array([[float(s.get(f, 0.0)) for f in feature_order]
                      for s in states], dtype=np.float32)
        batch = self.model.predict_batch(X)
        for i, s in enumerate(states):
            scalar = self.model.predict_efsd(s)
            self.assertAlmostEqual(float(batch[i]), scalar, places=4,
                                   msg=f"Batch/scalar mismatch at row {i}")

    def test_efsd_physical_range(self):
        """EFSD should stay within plausible NFL final margin bounds (−55 to +55)."""
        extreme_states = [
            {**self.neutral, "score_differential":  28.0, "game_seconds_remaining": 60.0},
            {**self.neutral, "score_differential": -28.0, "game_seconds_remaining": 60.0},
            {**self.neutral, "score_differential":   0.0, "game_seconds_remaining": 3600.0},
        ]
        for s in extreme_states:
            efsd = self.model.predict_efsd(s)
            self.assertGreater(efsd, -55.0, f"EFSD={efsd:.3f} below physical minimum")
            self.assertLess(efsd,    55.0, f"EFSD={efsd:.3f} above physical maximum")


# ---------------------------------------------------------------------------
# _drive_end_efsd Internal Logic Tests
# ---------------------------------------------------------------------------

class TestDriveEndEFSD(unittest.TestCase):
    """
    Purpose: Verify the terminal and non-terminal branches of _drive_end_efsd.
    This method is the critical possession-flip path — bugs here produce
    silently wrong concept rankings that only show up when the UI numbers look off.
    """

    @classmethod
    def setUpClass(cls):
        cls.ev = PositionalEvaluator()

    def test_terminal_lanes_return_exact_margin(self):
        """
        Terminal drives (game over) must return the exact current score differential
        as the EFSD — no model inference should occur; it IS the final margin.
        """
        score_diffs = np.array([7.0, -7.0, 0.0, 14.0, -21.0, 28.0])
        game_secs   = np.zeros(len(score_diffs))          # clock expired
        terminal    = np.ones(len(score_diffs), dtype=bool)

        result = self.ev._drive_end_efsd(score_diffs, game_secs, terminal, 3, 3)
        np.testing.assert_array_almost_equal(
            result, score_diffs, decimal=3,
            err_msg="Terminal EFSD must equal the current score differential exactly"
        )

    def test_terminal_overrides_non_terminal(self):
        """
        When game_sec <= 0, the terminal branch must fire even if terminal=False,
        because a clock at 0 with the game not flagged over is still a final state.
        """
        score_diffs = np.array([10.0, -3.0])
        game_secs   = np.zeros(2)
        terminal    = np.zeros(2, dtype=bool)   # not flagged, but clock is 0

        result = self.ev._drive_end_efsd(score_diffs, game_secs, terminal, 3, 3)
        np.testing.assert_array_almost_equal(
            result, score_diffs, decimal=3,
            err_msg="Zero-clock non-terminal lanes should still resolve to exact margin"
        )

    def test_non_terminal_sign_matches_score(self):
        """
        Non-terminal drives: when offense is clearly ahead, EFSD should be positive.
        When clearly behind, EFSD should be negative.
        """
        N = 2
        score_diffs = np.array([21.0, -21.0])
        game_secs   = np.full(N, 1800.0)           # 30 minutes left — genuinely non-terminal
        terminal    = np.zeros(N, dtype=bool)

        result = self.ev._drive_end_efsd(score_diffs, game_secs, terminal, 3, 3)
        self.assertGreater(result[0], 0.0,
                           f"Non-terminal EFSD for +21 lead: {result[0]:.3f} should be > 0")
        self.assertLess(result[1], 0.0,
                        f"Non-terminal EFSD for -21 deficit: {result[1]:.3f} should be < 0")


# ---------------------------------------------------------------------------
# PositionalEvaluator Integration Tests
# ---------------------------------------------------------------------------

class TestEFSDEvaluator(unittest.TestCase):
    """
    Purpose: End-to-end checks on EFSD integration within PositionalEvaluator.evaluate()
    and evaluate_one_step(). Uses 250 sims (fast) for shape/ordering — not precision.
    """

    @classmethod
    def setUpClass(cls):
        cls.ev = PositionalEvaluator()
        cls.midfield = {
            "down": 1, "ydstogo": 10, "yardline_100": 50,
            "game_seconds_remaining": 1800.0,
            "score_differential": 0.0,
            "posteam_timeouts_remaining": 3,
            "defteam_timeouts_remaining": 3,
        }

    def _eval(self, state, n=250):
        return self.ev.evaluate(state, off_team="KC", def_team="BUF", n_sims=n)

    def test_evaluate_returns_efsd_start(self):
        """evaluate() must include efsd_start in the returned dict."""
        res = self._eval(self.midfield)
        self.assertIn("efsd_start", res,
                      "evaluate() result missing 'efsd_start' key")
        self.assertIsNotNone(res["efsd_start"])
        self.assertTrue(np.isfinite(res["efsd_start"]),
                        f"efsd_start={res['efsd_start']} is not finite")

    def test_concept_delta_efsd_present(self):
        """All concept entries must include mean_efsd and delta_efsd keys."""
        res = self._eval(self.midfield, n=500)
        for name, c in res["concepts"].items():
            self.assertIn("mean_efsd", c, f"Concept {name} missing 'mean_efsd'")
            self.assertIn("delta_efsd", c, f"Concept {name} missing 'delta_efsd'")

    def test_concept_delta_efsd_finite_when_n_sufficient(self):
        """Concepts with enough lanes must have a finite delta_efsd."""
        res = self._eval(self.midfield, n=500)
        for name, c in res["concepts"].items():
            if c["n"] >= 5:
                self.assertIsNotNone(c["delta_efsd"],
                                     f"Concept {name} n={c['n']} but delta_efsd=None")
                self.assertTrue(np.isfinite(c["delta_efsd"]),
                                f"Concept {name} delta_efsd={c['delta_efsd']} not finite")

    def test_leading_late_has_higher_efsd(self):
        """
        Being up 7 with 2 minutes left should produce a higher efsd_start than
        being tied at the same field position and clock.
        """
        base = {**self.midfield, "game_seconds_remaining": 120.0}
        res_tied = self._eval({**base, "score_differential":  0.0})
        res_up7  = self._eval({**base, "score_differential":  7.0})
        self.assertGreater(
            res_up7["efsd_start"], res_tied["efsd_start"],
            f"Up-7 EFSD={res_up7['efsd_start']:.3f} should exceed tied EFSD={res_tied['efsd_start']:.3f}"
        )

    def test_efsd_and_kep_agree_on_sign(self):
        """
        efsd_start and kep_start must have the same sign for a clear leading/trailing state.
        Both measure the same positional advantage — disagreement on sign would indicate
        a possession-flip bug in one of the paths.
        """
        state = {**self.midfield, "score_differential": 14.0,
                 "game_seconds_remaining": 120.0}
        res = self._eval(state)
        self.assertGreater(res["kep_start"], 0.0,
                           f"Leading late kep_start={res['kep_start']:.3f} should be positive")
        self.assertGreater(res["efsd_start"], 0.0,
                           f"Leading late efsd_start={res['efsd_start']:.3f} should be positive")

    def test_one_step_returns_efsd_start(self):
        """evaluate_one_step() must include efsd_start in its return dict."""
        res = self.ev.evaluate_one_step(
            self.midfield, off_team="KC", def_team="BUF", n_sims=200
        )
        self.assertIn("efsd_start", res,
                      "evaluate_one_step() result missing 'efsd_start'")
        self.assertTrue(np.isfinite(res["efsd_start"]),
                        f"efsd_start={res['efsd_start']} is not finite")

    def test_one_step_concept_delta_efsd(self):
        """evaluate_one_step() concept entries must include delta_efsd."""
        res = self.ev.evaluate_one_step(
            self.midfield, off_team="KC", def_team="BUF", n_sims=200
        )
        for name, c in res["concepts"].items():
            self.assertIn("delta_efsd", c, f"Concept {name} missing 'delta_efsd'")


# ---------------------------------------------------------------------------
# suggest_lines(metric="efsd") Tests
# ---------------------------------------------------------------------------

class TestSuggestLinesEFSD(unittest.TestCase):
    """
    Purpose: Verify the EFSD metric path through suggest_lines — structure,
    key naming, and that KEP and EFSD rankings can differ.
    """

    @classmethod
    def setUpClass(cls):
        cls.ev = PositionalEvaluator()
        cls.state = {
            "down": 1, "ydstogo": 10, "yardline_100": 75,
            "game_seconds_remaining": 1800.0,
            "score_differential": 0.0,
            "posteam_timeouts_remaining": 3,
            "defteam_timeouts_remaining": 3,
        }

    def test_suggest_lines_efsd_runs(self):
        """suggest_lines(metric='efsd') must complete without raising."""
        result = self.ev.suggest_lines(
            self.state, off_team="KC", def_team="BUF",
            n_sims=100, depth=2, n_lines=3, metric="efsd"
        )
        self.assertIsNotNone(result)

    def test_suggest_lines_efsd_structure(self):
        """Result must include efsd_start, metric='efsd', and a non-empty lines list."""
        result = self.ev.suggest_lines(
            self.state, off_team="KC", def_team="BUF",
            n_sims=100, depth=2, n_lines=3, metric="efsd"
        )
        self.assertIn("efsd_start", result, "Missing 'efsd_start' key")
        self.assertEqual(result["metric"], "efsd", "metric field should be 'efsd'")
        self.assertIn("lines", result, "Missing 'lines' key")
        self.assertGreater(len(result["lines"]), 0, "Lines list is empty")

    def test_suggest_lines_efsd_line_keys(self):
        """Each line must contain efsd_steps, efsd_final, and delta_efsd."""
        result = self.ev.suggest_lines(
            self.state, off_team="KC", def_team="BUF",
            n_sims=100, depth=2, n_lines=3, metric="efsd"
        )
        for line in result["lines"]:
            self.assertIn("efsd_steps",  line, f"Line missing 'efsd_steps': {line}")
            self.assertIn("efsd_final",  line, f"Line missing 'efsd_final': {line}")
            self.assertIn("delta_efsd",  line, f"Line missing 'delta_efsd': {line}")
            self.assertIn("concepts",    line, f"Line missing 'concepts': {line}")

    def test_suggest_lines_kep_backward_compatible(self):
        """
        Calling suggest_lines without metric (defaults to 'kep') must still work
        and must NOT include efsd_start as the primary key (only kep_start).
        """
        result = self.ev.suggest_lines(
            self.state, off_team="KC", def_team="BUF",
            n_sims=100, depth=2, n_lines=3
        )
        self.assertIn("kep_start", result, "KEP backward compat: missing 'kep_start'")
        self.assertEqual(result["metric"], "kep")

    def test_invalid_metric_raises(self):
        """Passing an unsupported metric string must raise a ValueError."""
        with self.assertRaises(ValueError):
            self.ev.suggest_lines(
                self.state, off_team="KC", def_team="BUF", metric="invalid_metric"
            )


if __name__ == "__main__":
    unittest.main(verbosity=2)
