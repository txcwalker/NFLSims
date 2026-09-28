"""
tests/test_player_proj_eval.py
==============================
# Status: live | 2026-09-25

Unit tests for the Evaluation tab's Player Projections grading:
  - src/evaluation/player_actuals.py   -- nflverse -> sim-shaped stat lines
    (column map, LAR->LA, fumble total, DK via the sim's own scoring, REG only)
  - src/evaluation/player_proj_eval.py -- percentile finish (mid-rank PIT),
    projection quantiles, id join + name fallback, the "no stat line" /
    "unprojected" rules, unplayed games excluded, coverage/bias summaries,
    min-projection filter, repeat-offender list

A synthetic 10-iteration "sim" with hand-computable answers stands in for the
real 10K-run parquet (build_player_eval takes sims_by_week= / actuals=), and a
temp roster traits file supplies the GSIS ids.

Run from repo root:
    python -m pytest tests/test_player_proj_eval.py -v
"""
import json
import os
import sys
import tempfile
import unittest

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from src.evaluation.player_actuals import normalize_actuals  # noqa: E402
from src.evaluation.player_proj_eval import (  # noqa: E402
    norm_name, pit_by_group, build_player_eval, summarize_players, proj_bucket, ALL_STATS,
)
from src.nfl_sim.scoring import calculate_fantasy_points  # noqa: E402

G1, G2 = "2026_03_ATL_GB", "2026_03_NE_BUF"     # G1 played, G2 not yet


def _sim_rows(game, team, player, pos, dk_values, **stat_values):
    """One player's per-iteration sim rows; unspecified stats are 0."""
    n = len(dk_values)
    d = {"game_id": [game] * n, "Team": [team] * n, "Player": [player] * n, "Pos": [pos] * n,
         "dk_score": dk_values}
    for s in ALL_STATS:
        if s != "dk_score":
            d[s] = stat_values.get(s, [0] * n)
    return pd.DataFrame(d)


def _actual(game, team, pid, name, pos, dk, **stats):
    row = {"game_id": game, "week": 3, "team": team, "player_id": pid, "player_name": name,
           "position": pos, "dk_score": dk, "fumbles": 0}
    for s in ALL_STATS:
        if s != "dk_score":
            row[s] = stats.get(s, 0)
    return row


class TestNormName(unittest.TestCase):
    def test_suffixes_and_punctuation(self):
        self.assertEqual(norm_name("Michael Penix Jr."), "michaelpenix")
        self.assertEqual(norm_name("Amon-Ra St. Brown"), "amonrastbrown")
        self.assertEqual(norm_name("Marvin Harrison II"), "marvinharrison")
        self.assertEqual(norm_name("De'Zhaun Stribling"), "dezhaunstribling")


class TestNormalizeActuals(unittest.TestCase):
    def setUp(self):
        raw = pd.DataFrame([
            {"season_type": "REG", "game_id": G1, "week": 3, "team": "ATL", "player_id": "00-1",
             "player_display_name": "Bijan Robinson", "position": "RB", "carries": 20, "rushing_yards": 194,
             "rushing_tds": 0, "receptions": 2, "receiving_yards": 19, "targets": 3,
             "rushing_fumbles": 1, "receiving_fumbles": np.nan, "sack_fumbles": 0},
            {"season_type": "REG", "game_id": "2026_03_LA_X", "week": 3, "team": "LAR", "player_id": "00-2",
             "player_display_name": "Rams Guy", "position": "WR"},
            {"season_type": "POST", "game_id": "2026_19_A_B", "week": 19, "team": "KC", "player_id": "00-3",
             "player_display_name": "Playoff Guy", "position": "QB"},
        ])
        self.df = normalize_actuals(raw)

    def test_reg_only_and_team_fix(self):
        self.assertEqual(len(self.df), 2)
        self.assertEqual(self.df.loc[1, "team"], "LA")

    def test_columns_mapped_to_sim_names(self):
        r = self.df.loc[0]
        self.assertEqual((r["rAtt"], r["rYds"], r["rec"], r["recYds"], r["targets"]), (20, 194, 2, 19, 3))
        self.assertEqual(r["fumbles"], 1)                                   # NaN receiving fumbles -> 0

    def test_dk_uses_sim_scoring(self):
        # 194*0.1 + 3 (100+ bonus) + 2 rec + 1.9 - 1 fumble = 25.3 (sim scoring: every fumble -1)
        r = self.df.loc[0]
        self.assertAlmostEqual(r["dk_score"], 25.3)
        self.assertAlmostEqual(r["dk_score"], calculate_fantasy_points(r.to_dict(), "DK"))


class TestPitByGroup(unittest.TestCase):
    def test_mid_rank_per_group(self):
        sim = pd.DataFrame({"k": ["a"] * 4 + ["b"] * 4, "x": [1, 2, 3, 4, 0, 0, 0, 0], "act": [3] * 4 + [0] * 4})
        p = pit_by_group(sim, ["k"], "x", "act")
        self.assertAlmostEqual(p["a"], 0.5 + 0.5 * 0.25)                    # 2 below, 1 tie of 4
        self.assertAlmostEqual(p["b"], 0.5)                                 # all ties -> dead center


class TestBuildPlayerEval(unittest.TestCase):
    """G1 cast:
      Alpha  (ATL WR) -- id match; DK sims 0..9, actual 7 -> PIT 0.75, mean 4.5
      Beta   (ATL RB) -- roster has NO id; actual listed as 'Beta Guy Jr.' -> name fallback
      Gamma  (GB WR)  -- projected, no actual -> no_stat_line
      Defense (GB DST) -- never graded
      Epsilon (GB QB, actual 12 DK) + Zeta (GB TE, actual 3 DK) -- not in sims:
        Epsilon -> unprojected (>= 5 DK), Zeta too small to list
    G2 (NE@BUF) has sims but no actuals -> must not appear anywhere."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        rdir = os.path.join(self.tmp.name, "data", "current_rosters")
        os.makedirs(rdir)
        json.dump({"team": "ATL", "traits": {"Alpha": {"player_id": "00-A"}, "Beta Guy": {"pos": "RB"}}},
                  open(os.path.join(rdir, "ATL_traits_2026.json"), "w"))
        json.dump({"team": "GB", "traits": {"Gamma": {"player_id": "00-G"}}},
                  open(os.path.join(rdir, "GB_traits_2026.json"), "w"))
        rng = list(range(10))
        sim = pd.concat([
            _sim_rows(G1, "ATL", "Alpha", "WR", rng, recYds=[v * 10 for v in rng]),
            _sim_rows(G1, "ATL", "Beta Guy", "RB", [10] * 10),
            _sim_rows(G1, "GB", "Gamma", "WR", [8] * 10),
            _sim_rows(G1, "GB", "Defense", "DST", [6] * 10),
            _sim_rows(G2, "BUF", "Omega", "WR", [15] * 10),
        ], ignore_index=True)
        actuals = pd.DataFrame([
            _actual(G1, "ATL", "00-A", "Alpha", "WR", 7, recYds=95),
            _actual(G1, "ATL", "00-B", "Beta Guy Jr.", "RB", 10),
            _actual(G1, "GB", "00-E", "Epsilon", "QB", 12),
            _actual(G1, "GB", "00-Z", "Zeta", "TE", 3),
        ])
        self.graded, self.missing, self.unproj = build_player_eval(
            2026, base_dir=self.tmp.name, actuals=actuals, sims_by_week={3: sim})
        self.g = self.graded.set_index("Player")

    def tearDown(self):
        self.tmp.cleanup()

    def test_who_is_graded(self):
        self.assertEqual(sorted(self.g.index), ["Alpha", "Beta Guy"])       # no DST, no G2
        self.assertEqual(self.g.loc["Alpha", "matched_by"], "id")
        self.assertEqual(self.g.loc["Beta Guy", "matched_by"], "name")

    def test_projection_summary(self):
        a = self.g.loc["Alpha"]
        self.assertAlmostEqual(a["dk_score_mean"], 4.5)
        self.assertAlmostEqual(a["dk_score_q10"], 0.9)                      # linear quantile of 0..9
        self.assertAlmostEqual(a["dk_score_q90"], 8.1)
        self.assertAlmostEqual(a["proj_dk"], 4.5)

    def test_percentile_and_miss(self):
        a = self.g.loc["Alpha"]
        self.assertAlmostEqual(a["dk_score_pit"], 0.75)                     # 7 below + half of 1 tie
        self.assertAlmostEqual(a["dk_score_miss"], 2.5)
        self.assertAlmostEqual(a["recYds_pit"], 1.0)                        # 95 beats every sim (max 90)
        self.assertAlmostEqual(a["rAtt_pit"], 0.5)                          # 0 vs all-zero sims = center
        self.assertAlmostEqual(self.g.loc["Beta Guy", "dk_score_pit"], 0.5) # 10 vs all-10 sims

    def test_no_stat_line_and_unprojected(self):
        self.assertEqual(self.missing["Player"].tolist(), ["Gamma"])
        self.assertEqual(self.unproj["player_name"].tolist(), ["Epsilon"])  # Zeta < 5 DK not listed

    def test_unplayed_game_excluded_everywhere(self):
        for df in (self.graded, self.missing):
            self.assertNotIn(G2, set(df["game_id"]))


def _graded(rows):
    """Minimal graded frame: rows of (player, pos, week, proj_dk, dk_pit, dk_miss)."""
    out = []
    for player, pos, week, proj, p, miss in rows:
        r = {"Player": player, "Team": "T", "Pos": pos, "week": week, "game_id": f"g{week}",
             "proj_dk": proj, "dk_score_mean": proj, "dk_score_actual": proj + miss}
        for s in ALL_STATS:
            r[f"{s}_pit"], r[f"{s}_miss"] = p, miss
        out.append(r)
    return pd.DataFrame(out)


class TestSummarizePlayers(unittest.TestCase):
    def setUp(self):
        self.df = _graded([
            ("P1", "WR", 1, 12.0, 0.05, -6.0),
            ("P1", "WR", 2, 12.0, 0.10, -4.0),     # P1: 2 games, avg pct 0.075 -> over-projected
            ("P2", "RB", 1, 18.0, 0.50, 0.0),
            ("P3", "QB", 1, 22.0, 0.90, 5.0),
            ("P4", "TE", 2, 8.0, 0.95, 7.0),
            ("P5", "WR", 1, 3.0, 0.99, 9.0),       # below the 5-DK default filter
        ])

    def test_coverage_inclusive_bounds(self):
        s = summarize_players(self.df)                                      # P5 filtered out
        o = s["overall"]["dk_score"]
        self.assertEqual(o["n"], 5)
        self.assertAlmostEqual(o["cov80"], 3 / 5)                           # 0.10, 0.50, 0.90 inside [0.1, 0.9]
        self.assertAlmostEqual(o["cov50"], 1 / 5)                           # only 0.50 inside [0.25, 0.75]
        self.assertAlmostEqual(o["bias"], (-6 - 4 + 0 + 5 + 7) / 5)
        self.assertAlmostEqual(o["mae"], (6 + 4 + 0 + 5 + 7) / 5)
        self.assertEqual(sum(o["pit_hist"]), 5)

    def test_min_dk_filter(self):
        self.assertEqual(summarize_players(self.df, min_dk=0)["n"], 6)
        self.assertEqual(summarize_players(self.df, min_dk=20)["n"], 1)
        self.assertEqual(summarize_players(self.df, min_dk=99), {"n": 0})

    def test_repeat_requires_two_games(self):
        rep = summarize_players(self.df)["repeat"]
        self.assertEqual([r["Player"] for r in rep], ["P1"])
        self.assertAlmostEqual(rep[0]["mean_pit"], 0.075)

    def test_by_position_uses_position_stats(self):
        s = summarize_players(self.df)
        self.assertIn("pYds", s["by_pos"]["QB"])
        self.assertNotIn("recYds", s["by_pos"]["QB"])                      # a QB's receiving line isn't graded
        self.assertNotIn("pYds", s["by_pos"]["WR"])

    def test_buckets(self):
        self.assertEqual([proj_bucket(v) for v in (25, 20, 19.9, 15, 10, 5, 4.9)],
                         ["20+", "20+", "15-20", "15-20", "10-15", "5-10", "<5"])
        buckets = [b["bucket"] for b in summarize_players(self.df)["by_bucket"]]
        self.assertEqual(buckets, ["20+", "15-20", "10-15", "5-10"])       # ordered big -> small


if __name__ == "__main__":
    unittest.main()
