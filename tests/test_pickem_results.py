"""Tests for src/pickems/grade_results.py on a synthetic ledger + stat lines (temp dir; no repo files)."""
import numpy as np
import pandas as pd
import pytest

from src.evaluation.player_proj_eval import norm_name
from src.pickems.grade_results import grade_results, settle, summarize, _close_info
from src.pickems.grader import STORED_STATS
from src.pickems.ledger import record_paste
from src.pickems.underdog_parser import PickemLeg

K = 1791133200.0   # 2026-10-04 13:00 ET kickoff of the synthetic game
DAY = 86400.0
SCHED = pd.DataFrame([
    {"week": 4, "game_id": "2026_04_DET_CAR", "gameday": "2026-10-04", "gametime": "13:00", "away_team": "DET", "home_team": "CAR"},
    {"week": 4, "game_id": "2026_04_KC_LV", "gameday": "2026-10-04", "gametime": "16:25", "away_team": "KC", "home_team": "LV"},
])


def leg(player, side, mult, team="DET", opp="CAR", market="Rush + Rec TDs", line=0.5):
    return PickemLeg(player, team, opp, False, "Sun 12:00PM CDT", market, line, side, mult)


def entry(name, team, game_id, tds, usage=10.0, pos="RB"):
    n = len(tds)
    stats = {s: np.zeros(n, dtype=np.int16) for s in STORED_STATS}
    stats["tds"] = np.array(tds, dtype=np.int16)
    return {"name": name, "team": team, "pos": pos, "n": n, "usage": usage, "stats": stats, "game_id": game_id}


def make_index(*entries):
    idx = {(e["team"], norm_name(e["name"])): e for e in entries}
    return idx, {}


def actual(game_id, team, name, tds):
    return {"game_id": game_id, "week": 4, "team": team, "player_id": f"id_{name}", "player_name": name, "position": "RB",
            "pAtt": 0, "pCmp": 0, "pYds": 0, "pTD": 0, "int": 0, "sacks_taken": 0, "rAtt": 0, "rYds": 0,
            "rTD": tds, "targets": 0, "rec": 0, "recYds": 0, "recTD": 0, "fumbles": 0, "dk_score": 0}


def run(tmp_path, **kw):
    b = str(tmp_path)
    # open paste 2 days out; close paste 1 hour before kickoff (A moves 3.0 -> 2.5; B unchanged)
    record_paste(4, [leg("Alpha", "Higher", 3.0), leg("Alpha", "Lower", 1.4), leg("Bravo", "Higher", 2.0),
                     leg("Gone", "Higher", 4.0), leg("Delta", "Higher", 5.0, team="KC", opp="LV")], now=K - 2 * DAY, base_dir=b)
    record_paste(4, [leg("Alpha", "Higher", 2.5), leg("Alpha", "Lower", 1.5), leg("Gone", "Higher", 4.0),
                     leg("Delta", "Higher", 5.0, team="KC", opp="LV")], now=K - 3600, base_dir=b)   # Bravo NOT re-pasted
    idx = make_index(entry("Alpha", "DET", "2026_04_DET_CAR", [0] * 50 + [1] * 50),
                     entry("Bravo", "DET", "2026_04_DET_CAR", [0] * 80 + [1] * 20),
                     entry("Gone", "DET", "2026_04_DET_CAR", [0] * 90 + [1] * 10, usage=1.0),
                     entry("Delta", "KC", "2026_04_KC_LV", [0] * 50 + [1] * 50))
    act = pd.DataFrame([actual("2026_04_DET_CAR", "DET", "Alpha", 1), actual("2026_04_DET_CAR", "DET", "Bravo", 0)])
    return grade_results([4], base_dir=b, actuals=act, sched_by_week={4: SCHED}, index_by_week={4: idx},
                         roster_ids={}, **kw)


def test_settle_hit_miss_push():
    assert settle("Higher", 0.5, 1) == "hit" and settle("Lower", 0.5, 1) == "miss"
    assert settle("Lower", 49.5, 40) == "hit" and settle("Higher", 50.0, 50) == "push"


def test_open_and_close_returns_and_edges(tmp_path):
    res = run(tmp_path)
    rows = {(r["player"], r["side"]): r for r in res["rows"]}
    a = rows[("Alpha", "Higher")]          # hit: 1 TD, sim 50%
    assert a["hit"] and a["open_multiplier"] == 3.0 and a["close_multiplier"] == 2.5
    assert a["ret_open"] == pytest.approx(2.0) and a["ret_close"] == pytest.approx(1.5)
    assert a["ev_open"] == pytest.approx(50.0) and a["ev_close"] == pytest.approx(25.0)
    lo = rows[("Alpha", "Lower")]          # miss
    assert not lo["hit"] and lo["ret_open"] == -1.0
    # de-margined market prob at open: Higher 3.0 vs Lower 1.4 -> (1/3)/(1/3+1/1.4)
    assert a["market_prob_open"] == pytest.approx((1 / 3) / (1 / 3 + 1 / 1.4))
    assert a["market_prob_close"] == pytest.approx((1 / 2.5) / (1 / 2.5 + 1 / 1.5))


def test_stale_close_is_excluded_not_treated_as_open(tmp_path):
    res = run(tmp_path)
    b = [r for r in res["rows"] if r["player"] == "Bravo"][0]   # only pasted 2 days out
    assert b["close_multiplier"] is None and b["ret_close"] is None and b["close_age_h"] > 40
    assert b["ret_open"] == -1.0


def test_pending_no_stat_line_and_status_buckets(tmp_path):
    res = run(tmp_path)
    assert res["pending"] == 1                                          # Delta: KC@LV not final
    assert [x["player"] for x in res["no_stat_line"]] == ["Gone"]       # matched sim player, no real line
    assert res["unmatched"] == [] and res["late"] == 0


def test_close_info_late_capture():
    assert _close_info([(K + 10, 2.0)], K + 10, K) is None              # first capture after kickoff
    open_m, close_m, age = _close_info([(K - DAY, 2.0), (K - 7200, 1.8)], K - 7200, K)
    assert (open_m, close_m) == (2.0, 1.8) and age == pytest.approx(2.0)


def test_summary_buckets_and_brier(tmp_path):
    res = run(tmp_path, min_usage=3.0)       # Gone (usage 1) is thin anyway
    s = res["summary"]
    assert s["n"] == 3                        # Alpha H, Alpha L, Bravo H
    assert s["plus_ev"]["open"]["n"] == 1     # only Alpha Higher (+50%); Alpha Lower and Bravo are -EV
    assert s["plus_ev"]["open"]["roi"] == pytest.approx(2.0)
    assert s["brier"]["open"]["n"] == 2       # the two-sided Alpha legs
    assert s["brier"]["open"]["sim"] == pytest.approx(np.mean([(0.5 - 1) ** 2, (0.5 - 0) ** 2]))
    assert summarize([]) == {"n": 0}
