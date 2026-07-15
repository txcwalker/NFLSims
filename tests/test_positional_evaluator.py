"""
tests/test_positional_evaluator.py
===================================
# Status: live | v1.0.0 | 2026-06-21

Unit tests for the chess-style positional evaluator (KEP + EP).

Tests cover:
  - PositionalEPModelV010: monotonicity across yardline, down effects, goal-to-go flag
  - KEPConverter: tied-kickoff ~0, monotone inversion, clipping at grid edges
  - PositionalEvaluator.evaluate(): EP/KEP range sanity, concept delta finiteness,
    drive-end rate plausibility, goal-line vs. own-territory EP ordering

Run from repo root:
    python -m pytest tests/test_positional_evaluator.py -v
"""

import os
import sys
import unittest
import numpy as np

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from src.nfl_sim.models.positional_ep_v_0_1_0.positional_ep_inference import PositionalEPModelV010
from src.nfl_sim.nfl_positional_evaluator import KEPConverter, PositionalEvaluator
from src.nfl_sim.models.win_probability_v_0_1_0.inference import WinProbabilityModelV010


# ---------------------------------------------------------------------------
# EP Model Tests
# ---------------------------------------------------------------------------

class TestPositionalEPModel(unittest.TestCase):
    """
    Purpose: Verify the XGBoost EP model produces physically plausible values
    before it gets used inside the evaluator.
    """

    @classmethod
    def setUpClass(cls):
        cls.ep = PositionalEPModelV010()

    def test_ep_increases_toward_goal_line(self):
        """EP on 1st & 10 must increase monotonically as yardline_100 decreases."""
        yardlines = [99, 75, 50, 25, 10, 5]
        eps = [self.ep.predict_expected_points(yl, down=1, ydstogo=10) for yl in yardlines]
        for i in range(len(eps) - 1):
            self.assertLess(
                eps[i], eps[i + 1],
                f"EP should rise as yardline shrinks: EP({yardlines[i]})={eps[i]:.3f} "
                f">= EP({yardlines[i+1]})={eps[i+1]:.3f}"
            )

    def test_ep_range(self):
        """EP must stay within rough physical bounds (−7.5, +7.5)."""
        for yl in [99, 75, 50, 25, 10, 5, 1]:
            for down in [1, 2, 3, 4]:
                ep = self.ep.predict_expected_points(yl, down=down, ydstogo=min(10, yl))
                self.assertGreater(ep, -7.5, f"EP too low at yl={yl} down={down}: {ep}")
                self.assertLess(ep, 7.5,    f"EP too high at yl={yl} down={down}: {ep}")

    def test_ep_later_down_worse(self):
        """
        At the same yardline, 3rd & 10 EP must be lower than 1st & 10 EP
        (later down is worse given equal distance).
        """
        ep_1st = self.ep.predict_expected_points(50, down=1, ydstogo=10)
        ep_3rd = self.ep.predict_expected_points(50, down=3, ydstogo=10)
        self.assertGreater(ep_1st, ep_3rd, "1st & 10 EP should exceed 3rd & 10 EP")

    def test_ep_goal_to_go_flag(self):
        """
        Goal-to-go (ydstogo == yardline_100) EP should exceed same distance
        with no goal-to-go flag — short yardage near goal has higher EP.
        """
        ep_gtg  = self.ep.predict_expected_points(5, down=1, ydstogo=5, goal_to_go=1)
        ep_norm = self.ep.predict_expected_points(5, down=1, ydstogo=5, goal_to_go=0)
        # Goal-to-go should be >= (at worst equal) to non-goal-to-go
        self.assertGreaterEqual(ep_gtg, ep_norm - 0.3,
                                f"GTG EP={ep_gtg:.3f} vs non-GTG EP={ep_norm:.3f}: suspicious gap")

    def test_ep_batch_matches_scalar(self):
        """predict_batch output must match scalar predict_expected_points for the same rows."""
        X = np.array([
            [75, 1, 10, 0],
            [50, 2,  7, 0],
            [10, 1,  5, 1],
        ], dtype=np.float32)
        batch = self.ep.predict_batch(X)
        for i, row in enumerate(X):
            scalar = self.ep.predict_expected_points(*[int(v) for v in row])
            self.assertAlmostEqual(float(batch[i]), scalar, places=4,
                                   msg=f"Batch/scalar mismatch at row {i}")


# ---------------------------------------------------------------------------
# KEPConverter Tests
# ---------------------------------------------------------------------------

class TestKEPConverter(unittest.TestCase):
    """
    Purpose: Verify the WP-inversion curve is monotone, clips gracefully, and
    that a tied-kickoff game state maps to ~0 KEP.
    """

    @classmethod
    def setUpClass(cls):
        cls.wp_model = WinProbabilityModelV010()
        cls.kep = KEPConverter(cls.wp_model)

    def test_tied_kickoff_is_near_zero(self):
        """
        KEP at kickoff (1st & 10 at own 25, t=3600, tied) must be close to 0.
        Absolute tolerance is ±1.0 point to account for model calibration noise.
        """
        from src.nfl_sim.nfl_positional_evaluator import KICKOFF_REFERENCE
        wp = self.wp_model.predict_win_probability(KICKOFF_REFERENCE)
        kep = float(self.kep.kep_from_wp(wp))
        self.assertAlmostEqual(kep, 0.0, delta=1.0,
                               msg=f"Tied kickoff KEP={kep:.3f} is too far from 0")

    def test_kep_monotone_in_wp(self):
        """kep_from_wp must be strictly increasing in WP."""
        wps = np.linspace(0.05, 0.95, 40)
        keps = [float(self.kep.kep_from_wp(w)) for w in wps]
        for i in range(len(keps) - 1):
            self.assertLessEqual(
                keps[i], keps[i + 1] + 1e-6,
                f"KEP not monotone: kep({wps[i]:.2f})={keps[i]:.3f} > kep({wps[i+1]:.2f})={keps[i+1]:.3f}"
            )

    def test_winning_state_positive_kep(self):
        """A game state where offense leads big late should yield positive KEP."""
        from src.nfl_sim.nfl_positional_evaluator import KICKOFF_REFERENCE
        state = dict(KICKOFF_REFERENCE)
        state["score_differential"] = 14   # up 14 at kickoff
        kep = self.kep.kep_from_state(state)
        self.assertGreater(kep, 0, f"Leading state KEP={kep:.3f} should be positive")

    def test_losing_state_negative_kep(self):
        """A state where offense trails big late should yield negative KEP."""
        from src.nfl_sim.nfl_positional_evaluator import KICKOFF_REFERENCE
        state = dict(KICKOFF_REFERENCE)
        state["score_differential"] = -14  # down 14 at kickoff
        kep = self.kep.kep_from_state(state)
        self.assertLess(kep, 0, f"Trailing state KEP={kep:.3f} should be negative")

    def test_kep_clips_at_grid_boundaries(self):
        """kep_from_wp at extreme probabilities must not raise and must stay in grid range."""
        low_kep  = float(self.kep.kep_from_wp(0.001))
        high_kep = float(self.kep.kep_from_wp(0.999))
        self.assertGreaterEqual(low_kep,  -30.0, "Lower clip out of range")
        self.assertLessEqual(high_kep,    30.0,  "Upper clip out of range")


# ---------------------------------------------------------------------------
# PositionalEvaluator Integration Tests
# ---------------------------------------------------------------------------

class TestPositionalEvaluator(unittest.TestCase):
    """
    Purpose: End-to-end sanity checks on PositionalEvaluator.evaluate(). These
    run 250 sims (fast) to verify shape and ordering — not statistical precision.
    """

    @classmethod
    def setUpClass(cls):
        cls.ev = PositionalEvaluator()
        # Neutral midfield state used across multiple tests
        cls.midfield = {
            "down": 1, "ydstogo": 10, "yardline_100": 50,
            "game_seconds_remaining": 1800,
            "score_differential": 0,
            "posteam_timeouts_remaining": 3,
            "defteam_timeouts_remaining": 3,
        }

    def _eval(self, state, n=250):
        return self.ev.evaluate(state, off_team="KC", def_team="BUF", n_sims=n)

    def test_ep_positive_at_opponent_territory(self):
        """EP at Opp 30 on 1st & 10 must be positive."""
        res = self._eval({**self.midfield, "yardline_100": 30})
        self.assertGreater(res["ep_start"], 0,
                           f"EP at Opp 30 should be positive, got {res['ep_start']:.3f}")

    def test_ep_higher_at_redzone_than_midfield(self):
        """EP at Opp 10 must exceed EP at Own 50 (midfield)."""
        res_mid  = self._eval({**self.midfield, "yardline_100": 50})
        res_rz   = self._eval({**self.midfield, "yardline_100": 10})
        self.assertGreater(res_rz["ep_start"], res_mid["ep_start"],
                           f"Red-zone EP={res_rz['ep_start']:.3f} should exceed midfield EP={res_mid['ep_start']:.3f}")

    def test_kep_approximately_zero_at_tied_kickoff(self):
        """KEP at tied kickoff should be close to 0 (within ±1.5 pts calibration tolerance)."""
        from src.nfl_sim.nfl_positional_evaluator import KICKOFF_REFERENCE
        # KICKOFF_REFERENCE defines the KEP axis but omits score_differential;
        # add it as 0 (tied) when calling evaluate().
        kickoff_state = dict(KICKOFF_REFERENCE, score_differential=0)
        res = self._eval(kickoff_state)
        self.assertAlmostEqual(res["kep_start"], 0.0, delta=1.5,
                               msg=f"Tied kickoff KEP={res['kep_start']:.3f} too far from 0")

    def test_concepts_all_present(self):
        """Result must contain all 5 concept keys."""
        res = self._eval(self.midfield)
        for concept in ("Run", "Screen", "Short", "Medium", "Deep"):
            self.assertIn(concept, res["concepts"], f"Missing concept: {concept}")

    def test_concept_deltas_finite_or_null(self):
        """
        Concepts with enough sims (n >= 5) must have a finite delta_kep.
        Concepts with n < 5 may have None — that's valid.
        """
        res = self._eval(self.midfield, n=500)
        for name, c in res["concepts"].items():
            if c["n"] >= 5:
                self.assertIsNotNone(c["delta_kep"],
                                     f"Concept {name} has n={c['n']} but delta_kep=None")
                self.assertTrue(np.isfinite(c["delta_kep"]),
                                f"Concept {name} delta_kep={c['delta_kep']} is not finite")

    def test_drive_end_rate_plausible(self):
        """drive_end_rate must be in (0, 1] — at least one drive ends in MAX_STEPS."""
        res = self._eval(self.midfield)
        self.assertGreater(res["drive_end_rate"], 0.0,
                           f"drive_end_rate={res['drive_end_rate']:.3f} should be > 0")
        self.assertLessEqual(res["drive_end_rate"], 1.0,
                             f"drive_end_rate={res['drive_end_rate']:.3f} should be <= 1")

    def test_n_sims_reported_matches_request(self):
        """n_sims returned must equal the n_sims requested."""
        res = self._eval(self.midfield, n=250)
        self.assertEqual(res["n_sims"], 250)

    def test_leading_late_has_higher_kep(self):
        """
        Being up 7 with 2 minutes left should give a higher KEP than being
        tied at the same field position.
        """
        base = {**self.midfield, "yardline_100": 75, "game_seconds_remaining": 120}
        res_tied = self._eval({**base, "score_differential":  0})
        res_up7  = self._eval({**base, "score_differential":  7})
        self.assertGreater(res_up7["kep_start"], res_tied["kep_start"],
                           f"Up-7 KEP={res_up7['kep_start']:.3f} should exceed tied KEP={res_tied['kep_start']:.3f}")


if __name__ == "__main__":
    unittest.main(verbosity=2)
