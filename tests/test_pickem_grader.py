"""Tests for src/pickems/grader.py -- EV math, line pricing and matching on a synthetic sim index."""
import numpy as np
import pytest

from src.evaluation.player_proj_eval import norm_name
from src.pickems.grader import grade_legs, market_totals, p_higher, _match, STORED_STATS
from src.pickems.underdog_parser import PickemLeg


def leg(player, team, side, mult, market="Rush + Rec TDs", line=0.5):
    return PickemLeg(player, team, "OPP", False, "Sun", market, line, side, mult)


def entry(name, team, pos="RB", usage=10.0, **stats):
    """Sim entry from per-iteration arrays, e.g. entry('A','DET', tds=[0,1,1,0])."""
    n = len(next(iter(stats.values())))
    full = {s: np.zeros(n, dtype=np.int16) for s in STORED_STATS}
    for s, v in stats.items():
        full[s] = np.array(v, dtype=np.int16)
    return {"name": name, "team": team, "pos": pos, "n": n, "usage": usage, "stats": full}


def make_index(*entries):
    idx, fb = {}, {}
    for e in entries:
        idx[(e["team"], norm_name(e["name"]))] = e
        parts = e["name"].replace(".", "").split()
        fb.setdefault((e["team"], parts[-1].lower(), parts[0][0].lower()), []).append(e)
    return idx, fb


def tds(p_score, n=100):
    """n iterations, round(p_score*n) of which have exactly one TD."""
    k = round(p_score * n)
    return [1] * k + [0] * (n - k)


def test_p_higher_at_half_is_at_least_one_td_and_any_line():
    t = np.array([0] * 60 + [1] * 30 + [2] * 10)
    assert p_higher(t, 0.5) == pytest.approx(0.40)
    assert p_higher(t, 1.5) == pytest.approx(0.10)
    yds = np.array([40, 60, 80, 100, 120])
    assert p_higher(yds, 79.5) == pytest.approx(0.6)  # the slider prices any line


def test_multiplier_not_half_is_the_threshold():
    # Sim says 70% to score. At 1.28x that's NEGATIVE EV (needs >78.1%);
    # a 30% player at 4.0x is POSITIVE EV (needs >25%).
    index = make_index(entry("Star Back", "DET", tds=tds(0.7)), entry("Long Shot", "NE", tds=tds(0.3)))
    out = grade_legs([leg("Star Back", "DET", "Higher", 1.28), leg("Long Shot", "NE", "Higher", 4.0)], index)
    r = {x["player"]: x for x in out["rows"]}
    assert r["Star Back"]["ev_pct"] == pytest.approx(-10.4, abs=0.05)
    assert r["Star Back"]["min_multiplier"] == pytest.approx(1 / 0.7)
    assert r["Long Shot"]["ev_pct"] == pytest.approx(20.0, abs=0.05)
    assert out["rows"][0]["player"] == "Long Shot"  # ranked by EV


def test_lower_side_and_market_prob():
    index = make_index(entry("Star Back", "DET", tds=tds(0.7)))
    out = grade_legs([leg("Star Back", "DET", "Higher", 1.28), leg("Star Back", "DET", "Lower", 2.88)], index)
    lo = [x for x in out["rows"] if x["side"] == "Lower"][0]
    assert lo["sim_prob"] == pytest.approx(0.30)
    assert lo["market_prob"] == pytest.approx(0.3077, abs=1e-3)  # (1/2.88)/(1/1.28+1/2.88)
    assert grade_legs([leg("Star Back", "DET", "Higher", 1.28)], index)["rows"][0]["market_prob"] is None


def test_yardage_market_sums_stats_and_carries_usage():
    e = entry("Combo Back", "KC", usage=14.2, rYds=[50, 80, 10, 100], recYds=[40, 30, 20, 10])
    assert list(market_totals(e, "Rush + Rec Yards")) == [90, 110, 30, 110]
    out = grade_legs([leg("Combo Back", "KC", "Higher", 2.0, market="Rushing Yards", line=49.5)], make_index(e))
    row = out["rows"][0]
    assert row["sim_prob"] == pytest.approx(0.75) and row["usage"] == 14.2 and row["pos"] == "RB"


def test_matching_aliases_suffixes_and_failures():
    index = make_index(entry("Kenneth Walker III", "KC", tds=tds(0.5)), entry("Puka Nacua", "LA", tds=tds(0.5)),
                       entry("Kenneth Gainwell", "TB", tds=tds(0.5)))
    assert _match(index, "KC", "Kenneth Walker")[1] == "name"           # suffix dropped by norm_name
    assert _match(index, "LAR", "Puka Nacua")[0]["team"] == "LA"         # Underdog LAR -> our LA
    assert _match(index, "TB", "Kenny Gainwell")[1] == "last+initial"    # nickname via fallback
    out = grade_legs([leg("Nobody Here", "KC", "Higher", 2.0), leg("Foo", "KC", "Higher", 2.0, market="Mystery Prop"),
                      leg("Foo", "KC", "Yes", 9.0, market="First TD Scorer")], index)
    assert out["unmatched"] == [{"player": "Nobody Here", "team": "KC"}]
    assert out["unsupported"] == {"Mystery Prop": 1}  # first/last TD silently ignored


def test_line_ladder_prices_nearby_lines_and_min_mult_is_inverse_prob():
    from src.pickems.grader import line_ladder
    e = entry("Combo Back", "KC", recYds=[40, 60, 80, 100, 120])
    out = line_ladder(e, "Receiving Yards", 79.5, span=2)
    by = {l["line"]: l for l in out["lines"]}
    assert sorted(by) == [77.5, 78.5, 79.5, 80.5, 81.5]
    assert by[79.5]["p_higher"] == pytest.approx(0.6) and by[79.5]["min_mult_higher"] == pytest.approx(1 / 0.6)
    assert by[79.5]["p_lower"] == pytest.approx(0.4)
    assert by[80.5]["p_higher"] == pytest.approx(0.4)  # line moved up -> Higher harder, needs bigger multiplier
    assert out["mean"] == pytest.approx(80.0)
    assert line_ladder(e, "Not A Market", 1.5) is None
    assert [l["line"] for l in line_ladder(e, "Receiving Yards", 0.5, span=2)["lines"]][0] == 0.5  # floored at 0


def test_combo_probability_same_game_uses_iteration_alignment_and_cross_game_multiplies():
    from src.pickems.grader import combo_probability
    # A and B always hit together (perfect correlation, same game); C is in another game.
    a = entry("A Back", "DET", tds=[1, 1, 0, 0]); a["game_id"] = "g1"
    b = entry("B Wr", "DET", rec=[5, 5, 0, 0]); b["game_id"] = "g1"
    c = entry("C Qb", "NE", pAtt=[30, 40, 30, 40]); c["game_id"] = "g2"
    idx = make_index(a, b, c)
    legs = [dict(team="DET", player="A Back", market="Rush + Rec TDs", line=0.5, side="Higher"),
            dict(team="DET", player="B Wr", market="Receptions", line=2.5, side="Higher")]
    out = combo_probability(legs, idx)
    assert out["joint"] == pytest.approx(0.5) and out["independent"] == pytest.approx(0.25) and out["games"] == 1
    legs.append(dict(team="NE", player="C Qb", market="Pass Attempts", line=34.5, side="Lower"))
    assert combo_probability(legs, idx)["joint"] == pytest.approx(0.25)  # 0.5 (game 1) x 0.5 (game 2)
    assert combo_probability([dict(team="NE", player="Nobody", market="Receptions", line=1.5, side="Higher")], idx) is None
