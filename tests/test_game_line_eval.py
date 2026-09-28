"""
tests/test_game_line_eval.py
============================
# Status: live | 2026-09-25

Unit tests for the Evaluation tab's Game Lines grading:
  - src/evaluation/game_line_eval.py -- payout / cover / push math, PIT,
    vig removal, moneyline value bets, agreement tiers, per-game grading,
    season summary + tier breakdown
  - src/evaluation/line_history.py   -- ledger dedupe, open/close resolution,
    hand-entered overrides

Every expected value is hand-computed in the comment beside it, so a failure
points at the math rather than at a fixture. Conventions under test (same as
nflverse): margin = home - away; spread_line > 0 = home favored.

Run from repo root:
    python -m pytest tests/test_game_line_eval.py -v
"""
import math
import os
import sys
import tempfile
import unittest

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from src.evaluation.game_line_eval import (  # noqa: E402
    payout, grade_side, pit, implied_prob, novig_home_prob, ml_value_bet, pick_at_line,
    tier, TIER_CUTS_PTS, TIER_CUTS_ML, grade_game, summarize, tier_breakdown, running_by_week,
)
from src.evaluation.line_history import (  # noqa: E402
    append_snapshot, load_ledger, resolve_open_close, LEDGER_COLS, OVERRIDE_COLS,
)

# 2026-09-24 20:15 ET (EDT, UTC-4) = 2026-09-25 00:15 UTC
KICKOFF = pd.Timestamp("2026-09-25 00:15", tz="UTC").timestamp()


class TestPayout(unittest.TestCase):
    def test_minus_110_win(self):
        self.assertAlmostEqual(payout("W"), 100 / 110)          # 0.9091u

    def test_plus_money_win_pays_real_odds(self):
        self.assertAlmostEqual(payout("W", 210), 2.10)          # ATL +210 winner

    def test_minus_money_win(self):
        self.assertAlmostEqual(payout("W", -155), 100 / 155)    # CAR -155 winner = 0.645u

    def test_loss_push_ungraded(self):
        self.assertEqual(payout("L", 210), -1.0)
        self.assertEqual(payout("P"), 0.0)
        self.assertTrue(math.isnan(payout(None)))


class TestGradeSide(unittest.TestCase):
    def test_home_pick(self):
        # BUF won by 10 (margin +10) vs BUF -3 (line 3): home covers
        self.assertEqual(grade_side(10, 3, pick_over=True), "W")
        self.assertEqual(grade_side(10, 3, pick_over=False), "L")   # DET +3 loses

    def test_away_pick_covers_by_losing_small(self):
        # ATL +4.5 at GB (line 4.5), GB wins by 3 (margin +3): away covers
        self.assertEqual(grade_side(3, 4.5, pick_over=False), "W")

    def test_push_and_unplayed(self):
        self.assertEqual(grade_side(3, 3, pick_over=True), "P")
        self.assertIsNone(grade_side(np.nan, 3, True))
        self.assertIsNone(grade_side(3, None, True))

    def test_totals(self):
        self.assertEqual(grade_side(72, 52.5, pick_over=False), "L")  # under 52.5, went 72
        self.assertEqual(grade_side(37, 43.5, pick_over=False), "W")


class TestPit(unittest.TestCase):
    def test_mid_rank(self):
        # P(<3) = 2/4, P(==3) = 1/4 -> 0.5 + 0.5*0.25 = 0.625
        self.assertAlmostEqual(pit(np.array([1, 2, 3, 4]), 3), 0.625)

    def test_extremes_and_unplayed(self):
        s = np.array([10, 20, 30])
        self.assertEqual(pit(s, 99), 1.0)
        self.assertEqual(pit(s, 0), 0.0)
        self.assertTrue(math.isnan(pit(s, np.nan)))


class TestVig(unittest.TestCase):
    def test_implied_prob(self):
        self.assertAlmostEqual(implied_prob(-110), 110 / 210)
        self.assertAlmostEqual(implied_prob(210), 100 / 310)     # 32.26%
        self.assertAlmostEqual(implied_prob(-258), 258 / 358)    # 72.07%
        self.assertTrue(math.isnan(implied_prob(None)))

    def test_novig_removes_overround(self):
        # (258/358) / (258/358 + 100/310) = 0.69079 -- the "69.1%" on the ATL@GB row
        self.assertAlmostEqual(novig_home_prob(-258, 210), 0.69079, places=4)
        self.assertAlmostEqual(novig_home_prob(-110, -110), 0.5)
        self.assertTrue(math.isnan(novig_home_prob(None, 210)))


class TestMoneylineValueBet(unittest.TestCase):
    def test_atl_plus_210(self):
        # sim ATL 42.55% vs +210 break-even 32.26% -> bet ATL, edge 10.29 pts
        side, edge = ml_value_bet(1 - 0.42545, -258, 210)
        self.assertEqual(side, "away")
        self.assertAlmostEqual(edge, 0.42545 - 100 / 310, places=6)

    def test_no_bet_when_neither_side_beats_the_vig(self):
        # sim home 70%: home 0.70 < 0.7207, away 0.30 < 0.3226 -> no bet
        side, edge = ml_value_bet(0.70, -258, 210)
        self.assertIsNone(side)
        self.assertLess(edge, 0)

    def test_exact_break_even_is_not_a_bet(self):
        # -400 break-even is exactly 80%; a sim at exactly 80% has zero edge
        side, _ = ml_value_bet(0.80, -400, 320)
        self.assertIsNone(side)

    def test_no_line(self):
        side, edge = ml_value_bet(0.6, None, None)
        self.assertIsNone(side)
        self.assertTrue(math.isnan(edge))


class TestPickAtLine(unittest.TestCase):
    def test_pushes_excluded_from_pick_prob(self):
        # line 3: over = {5,7} (0.4), under = {1} (0.2), push = {3,3} (0.4)
        pick_over, prob, push = pick_at_line(np.array([1, 3, 3, 5, 7]), 3)
        self.assertTrue(pick_over)
        self.assertAlmostEqual(prob, 0.4 / 0.6)
        self.assertAlmostEqual(push, 0.4)

    def test_skewed_distribution_mean_above_line_but_pick_under(self):
        # mean 45 > 44, but 75% of sims land under 44 -> under (the NO@BAL case)
        s = np.array([40, 40, 40, 60])
        self.assertGreater(s.mean(), 44)
        pick_over, prob, _ = pick_at_line(s, 44)
        self.assertFalse(pick_over)
        self.assertAlmostEqual(prob, 0.75)


class TestTiers(unittest.TestCase):
    def test_point_boundaries_inclusive(self):
        self.assertEqual(tier(0.0, TIER_CUTS_PTS), "agree")
        self.assertEqual(tier(1.0, TIER_CUTS_PTS), "agree")          # exactly 1 = agree
        self.assertEqual(tier(1.01, TIER_CUTS_PTS), "disagree")
        self.assertEqual(tier(2.5, TIER_CUTS_PTS), "disagree")       # exactly 2.5 = disagree
        self.assertEqual(tier(2.51, TIER_CUTS_PTS), "strong")

    def test_sign_does_not_matter(self):
        self.assertEqual(tier(-1.95, TIER_CUTS_PTS), "disagree")     # sim GB -2.55 vs -4.5
        self.assertEqual(tier(-4.8, TIER_CUTS_PTS), "strong")

    def test_moneyline_boundaries(self):
        self.assertEqual(tier(0.03, TIER_CUTS_ML), "agree")
        self.assertEqual(tier(-0.05, TIER_CUTS_ML), "disagree")
        self.assertEqual(tier(0.07, TIER_CUTS_ML), "disagree")
        self.assertEqual(tier(-0.1162, TIER_CUTS_ML), "strong")      # ATL@GB close

    def test_missing(self):
        self.assertIsNone(tier(np.nan, TIER_CUTS_PTS))
        self.assertIsNone(tier(None, TIER_CUTS_PTS))


def _sched(result=-3.0, total=50.0, **kw):
    """ATL @ GB, Thu 2026-09-24 8:15 ET. result = home - away (default: ATL by 3)."""
    row = {"game_id": "2026_03_ATL_GB", "week": 3, "away_team": "ATL", "home_team": "GB",
           "gameday": "2026-09-24", "gametime": "20:15", "roof": "outdoors", "div_game": 0,
           "result": result, "total": total}
    row.update(kw)
    return pd.Series(row)


# Sim: GB wins 6 of 10 (+3), ATL 4 of 10 (-3) -> mean margin 0.6, P(home) 0.60.
# Totals: 3 x 44, 7 x 50 -> mean 48.2.
MARGINS = np.array([-3.0] * 4 + [3.0] * 6)
TOTALS = np.array([44.0] * 3 + [50.0] * 7)
LINES = {"open_spread": 7.5, "open_total": 46.5, "open_spread_src": "ledger", "open_total_src": "ledger",
         "close_spread": 4.5, "close_total": 43.5, "close_spread_src": "override", "close_total_src": "override",
         "open_home_ml": -360.0, "open_away_ml": 285.0, "close_home_ml": -258.0, "close_away_ml": 210.0}


class TestGradeGame(unittest.TestCase):
    def setUp(self):
        self.r = grade_game(MARGINS, TOTALS, _sched(), LINES, sim_run_at=KICKOFF - 3600)

    def test_sim_summary(self):
        self.assertAlmostEqual(self.r["sim_margin"], 0.6)
        self.assertAlmostEqual(self.r["sim_total"], 48.2)
        self.assertAlmostEqual(self.r["sim_home_win"], 0.6)
        self.assertEqual(self.r["sim_timing"], "pregame")

    def test_spread_pick_and_result(self):
        # every sim margin < 7.5 -> ATL +7.5 at 100%; ATL won by 3 -> W at -110
        self.assertEqual(self.r["open_spread_pick"], "ATL")
        self.assertAlmostEqual(self.r["open_spread_pick_prob"], 1.0)
        self.assertEqual(self.r["open_spread_result"], "W")
        self.assertAlmostEqual(self.r["open_spread_units"], 100 / 110)

    def test_total_pick_and_result(self):
        # 70% of sims > 46.5 -> over; actual 50 -> W
        self.assertEqual(self.r["open_total_pick"], "over")
        self.assertAlmostEqual(self.r["open_total_pick_prob"], 0.7)
        self.assertEqual(self.r["open_total_result"], "W")

    def test_errors_and_closer(self):
        self.assertAlmostEqual(self.r["sim_margin_err"], -3.6)          # -3 - 0.6
        self.assertAlmostEqual(self.r["open_spread_line_err"], -10.5)   # Vegas's own miss
        self.assertEqual(self.r["open_spread_sim_closer"], 1.0)         # 3.6 < 10.5
        self.assertEqual(self.r["open_total_sim_closer"], 1.0)          # |50-48.2| < |50-46.5|

    def test_tiers(self):
        self.assertEqual(self.r["open_spread_tier"], "strong")          # |0.6 - 7.5| = 6.9
        self.assertEqual(self.r["open_total_tier"], "disagree")         # |48.2 - 46.5| = 1.7
        self.assertEqual(self.r["close_total_tier"], "strong")          # |48.2 - 43.5| = 4.7

    def test_clv_from_open_side(self):
        # took ATL +7.5, closed +4.5: market moved our way 3 pts
        self.assertAlmostEqual(self.r["spread_clv"], 3.0)
        # took over 46.5, closed 43.5: moved against us 3 pts
        self.assertAlmostEqual(self.r["total_clv"], -3.0)

    def test_moneyline_close(self):
        # sim ATL 40% vs +210 break-even 32.26% -> bet ATL; ATL won -> +2.10u
        self.assertEqual(self.r["close_ml_pick"], "ATL")
        self.assertTrue(self.r["close_ml_pick_dog"])
        self.assertAlmostEqual(self.r["close_ml_edge"], 0.40 - 100 / 310)
        self.assertEqual(self.r["close_ml_result"], "W")
        self.assertAlmostEqual(self.r["close_ml_units"], 2.10)
        self.assertEqual(self.r["close_ml_tier"], "strong")             # 0.60 - 0.6908 = -9.1 pts

    def test_moneyline_open_uses_opening_price(self):
        # +285 -> break-even 25.97%; sim ATL 40% -> bet at +285, pays 2.85
        self.assertEqual(self.r["open_ml_pick"], "ATL")
        self.assertAlmostEqual(self.r["open_ml_units"], 2.85)

    def test_tie_game_pushes_moneyline(self):
        r = grade_game(MARGINS, TOTALS, _sched(result=0.0, total=40.0), LINES)
        self.assertEqual(r["close_ml_result"], "P")
        self.assertEqual(r["close_ml_units"], 0.0)

    def test_unplayed_game_not_graded(self):
        r = grade_game(MARGINS, TOTALS, _sched(result=np.nan, total=np.nan), LINES)
        self.assertFalse(r["played"])
        self.assertIsNone(r["open_spread_result"])
        self.assertTrue(math.isnan(r["open_spread_units"]))
        self.assertIsNone(r["close_ml_result"])
        self.assertTrue(math.isnan(r["open_spread_sim_closer"]))
        self.assertEqual(r["close_ml_pick"], "ATL")                     # pick still shown pre-game

    def test_timing_flags(self):
        self.assertEqual(grade_game(MARGINS, TOTALS, _sched(), LINES, KICKOFF + 60)["sim_timing"], "post_kickoff")
        self.assertEqual(grade_game(MARGINS, TOTALS, _sched(), LINES, None)["sim_timing"], "unknown")

    def test_missing_lines(self):
        r = grade_game(MARGINS, TOTALS, _sched(), {})
        self.assertIsNone(r["open_spread_pick"])
        self.assertIsNone(r["open_spread_tier"])
        self.assertIsNone(r["close_ml_pick"])
        self.assertTrue(math.isnan(r["spread_clv"]))


class TestSummaries(unittest.TestCase):
    def setUp(self):
        # g1: ATL@GB as above (ATL by 3). g2: same sims, GB by 14 (ATL +7.5 loses,
        # ATL ML loses). g3: unplayed -- must never be graded.
        g1 = grade_game(MARGINS, TOTALS, _sched(), LINES)
        g2 = grade_game(MARGINS, TOTALS, _sched(game_id="g2", result=14.0, total=30.0), LINES)
        g3 = grade_game(MARGINS, TOTALS, _sched(game_id="g3", week=4, result=np.nan, total=np.nan), LINES)
        self.df = pd.DataFrame([g1, g2, g3])

    def test_records(self):
        s = summarize(self.df)
        self.assertEqual((s["n_games"], s["n_played"]), (3, 2))
        self.assertEqual((s["records"]["open_spread"]["w"], s["records"]["open_spread"]["l"]), (1, 1))
        # ML at close: +2.10 then -1 = +1.10
        self.assertAlmostEqual(s["records"]["close_ml"]["units"], 1.10)

    def test_tier_breakdown_counts_played_only(self):
        t = tier_breakdown(self.df[self.df["played"]], "open")
        spread = {r["tier"]: r for r in t["spread"]}
        self.assertEqual([r["tier"] for r in t["spread"]], ["agree", "disagree", "strong"])
        self.assertEqual(spread["strong"]["games"], 2)
        self.assertEqual(spread["agree"]["games"], 0)
        # g1: sim closer (1.0); g2: actual 14 vs sim 0.6 (13.4) / line 7.5 (6.5) -> Vegas (0.0)
        self.assertAlmostEqual(spread["strong"]["sim_closer_pct"], 0.5)
        self.assertIn("sim_brier", {r["tier"]: r for r in t["ml"]}["strong"])

    def test_running_is_cumulative(self):
        run = running_by_week(self.df)
        self.assertEqual([r["week"] for r in run], [3])      # week 4 unplayed -> no point
        self.assertAlmostEqual(run[0]["units_close_ml"], 1.10)

    def test_empty(self):
        s = summarize(self.df[~self.df["played"]])
        self.assertEqual(s["n_played"], 0)
        self.assertNotIn("records", s)


class TestLineHistory(unittest.TestCase):
    def _sched_df(self, spread, total, ml_home=-258.0, ml_away=210.0):
        return pd.DataFrame([{"game_id": "2026_03_ATL_GB", "week": 3, "spread_line": spread, "total_line": total,
                              "home_moneyline": ml_home, "away_moneyline": ml_away,
                              "home_spread_odds": -110.0, "away_spread_odds": -110.0,
                              "over_odds": -110.0, "under_odds": -110.0}])

    def test_append_dedupes_on_change(self):
        with tempfile.TemporaryDirectory() as d:
            self.assertEqual(append_snapshot(self._sched_df(7.5, 46.5), 2026, captured_at=1, base_dir=d), 1)
            self.assertEqual(append_snapshot(self._sched_df(7.5, 46.5), 2026, captured_at=2, base_dir=d), 0)
            self.assertEqual(append_snapshot(self._sched_df(6.5, 46.5), 2026, captured_at=3, base_dir=d), 1)
            self.assertEqual(len(load_ledger(2026, d)), 2)

    def test_games_without_lines_skipped(self):
        with tempfile.TemporaryDirectory() as d:
            self.assertEqual(append_snapshot(self._sched_df(np.nan, np.nan), 2026, captured_at=1, base_dir=d), 0)

    def _ledger(self):
        rows = [(KICKOFF - 900000, 7.5, 46.5, -360, 285), (KICKOFF - 50000, 6.5, 46.5, -298, 240),
                (KICKOFF + 20000, 5.5, 42.5, -258, 210)]          # last one captured after kickoff
        return pd.DataFrame([{"game_id": "2026_03_ATL_GB", "week": 3, "captured_at": t, "source": "nflverse",
                              "spread_line": s, "total_line": tot, "home_moneyline": h, "away_moneyline": a,
                              "home_spread_odds": -110, "away_spread_odds": -110, "over_odds": -110,
                              "under_odds": -110} for t, s, tot, h, a in rows], columns=LEDGER_COLS)

    def test_open_is_earliest_pregame_close_is_latest(self):
        r = resolve_open_close(self._ledger(), pd.DataFrame(columns=OVERRIDE_COLS),
                               {"2026_03_ATL_GB": KICKOFF}).loc["2026_03_ATL_GB"]
        self.assertEqual((r["open_spread"], r["open_total"]), (7.5, 46.5))
        self.assertEqual((r["close_spread"], r["close_total"]), (5.5, 42.5))   # post-kickoff pull = final
        self.assertEqual((r["open_home_ml"], r["open_away_ml"]), (-360, 285))
        self.assertEqual((r["close_home_ml"], r["close_away_ml"]), (-258, 210))

    def test_no_pregame_capture_means_no_open(self):
        led = self._ledger().iloc[[2]]                     # only the post-kickoff row
        r = resolve_open_close(led, pd.DataFrame(columns=OVERRIDE_COLS),
                               {"2026_03_ATL_GB": KICKOFF}).loc["2026_03_ATL_GB"]
        self.assertIsNone(r["open_spread"])
        self.assertEqual(r["close_spread"], 5.5)

    def test_override_wins_and_can_fix_one_field(self):
        ov = pd.DataFrame([{"game_id": "2026_03_ATL_GB", "tag": "close", "spread_line": 4.5,
                            "total_line": np.nan, "note": "book"}], columns=OVERRIDE_COLS)
        r = resolve_open_close(self._ledger(), ov, {"2026_03_ATL_GB": KICKOFF}).loc["2026_03_ATL_GB"]
        self.assertEqual((r["close_spread"], r["close_spread_src"]), (4.5, "override"))
        self.assertEqual((r["close_total"], r["close_total_src"]), (42.5, "ledger"))   # blank -> ledger


if __name__ == "__main__":
    unittest.main()
