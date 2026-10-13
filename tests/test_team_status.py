"""Tests for the injury/depth-chart monitor: scripts/roster_management/
scrape_team_status.py (HTML parsers) and diff_team_status.py (out-probability
rules, change detection, manual overrides). Pure functions, no network."""
import os
import sys

sys.path.insert(0, os.path.join("scripts", "roster_management"))

from scrape_team_status import parse_depth, parse_injuries, _clean_depth_name  # noqa: E402
from diff_team_status import out_probability, team_changes, apply_overrides  # noqa: E402


def snap(depth_rt="Lane Johnson", injuries=(), active=("A QB",), source="team_site"):
    """Minimal snapshot: RT starter, injury rows (player,pos,status,practice), Active names."""
    return {
        "team": "PHI", "depth_source": source,
        "roster": [{"player": n, "pos": "T", "section": "Active"} for n in active],
        "depth": {"Offense": {"RT": [depth_rt, "Fred Johnson"], "QB": ["Hurts"]}, "Defense": {}},
        "injuries": {"team": [{"player": p, "pos": pos, "injury": "Knee", "practice": pr, "status": st}
                              for p, pos, st, pr in injuries], "opponent_team": None, "opponent": []},
    }


# ---- out-probability rules (Cam, 2026-10-07) ----
def test_out_and_doubtful_are_certain():
    assert out_probability("Out", ["DNP", "DNP", "DNP"]) == 1.0
    assert out_probability("DOUBTFUL", []) == 1.0


def test_questionable_dnp_friday_is_75():
    assert out_probability("Questionable", ["LP", "LP", "DNP"]) == 0.75


def test_questionable_practiced_friday_is_50():
    assert out_probability("Questionable", ["DNP", "LP", "LP"]) == 0.5


def test_unpublished_status_has_no_number():
    assert out_probability("UNSPECIFIED", ["DNP", "DNP", ""]) is None
    assert out_probability("(-)", []) is None


# ---- change detection ----
def test_no_change_is_empty():
    assert team_changes(snap(), snap()) == []


def test_modeled_player_status_change_is_material():
    ch = team_changes(snap(injuries=[("X Y", "WR", "UNSPECIFIED", ["DNP", "", ""])]),
                      snap(injuries=[("X Y", "WR", "Out", ["DNP", "DNP", "DNP"])]))
    assert any(c["kind"] == "injury" and c["material"] for c in ch)


def test_unmodeled_position_injury_is_info_only():
    ch = team_changes(snap(), snap(injuries=[("C B", "CB", "Out", ["DNP", "DNP", "DNP"])]))
    assert ch and not any(c["material"] for c in ch)


def test_practice_change_without_probability_change_not_material():
    ch = team_changes(snap(injuries=[("X Y", "WR", "UNSPECIFIED", ["DNP", "", ""])]),
                      snap(injuries=[("X Y", "WR", "UNSPECIFIED", ["DNP", "LP", ""])]))
    assert ch and not any(c["material"] for c in ch)


def test_starter_change_is_material():
    ch = team_changes(snap(depth_rt="Lane Johnson"), snap(depth_rt="Markel Bell"))
    assert any(c["kind"] == "depth" and c["material"] and c["player"] == "Markel Bell" for c in ch)


def test_depth_source_switch_skips_depth_diff():
    ch = team_changes(snap(source="team_site"), snap(depth_rt="Someone Else", source="espn"))
    assert not any(c["kind"] == "depth" for c in ch)


def test_active_roster_departure_is_material_for_modeled_pos():
    ch = team_changes(snap(active=("A QB", "L J")), snap(active=("A QB",)))
    assert any(c["kind"] == "roster" and c["material"] and c["player"] == "L J" for c in ch)


# ---- manual overrides (Lane Johnson retired, Markel Bell starts at RT) ----
def test_override_retires_and_replaces_starter():
    ov = {"PHI": {"retired": ["Lane Johnson"], "depth": {"RT": ["Markel Bell"]}}}
    s = apply_overrides(snap(active=("Lane Johnson", "A QB")), ov)
    assert s["depth"]["Offense"]["RT"] == ["Markel Bell"]
    assert [r["player"] for r in s["roster"]] == ["A QB"]


def test_override_masks_site_lag():
    """Site still lists Johnson before AND after -> no spurious change once overridden."""
    ov = {"PHI": {"retired": ["Lane Johnson"], "depth": {"RT": ["Markel Bell"]}}}
    a, b = apply_overrides(snap(), ov), apply_overrides(snap(), ov)
    assert team_changes(a, b) == []


# ---- HTML parsers ----
def test_parse_depth_summary_markup_and_repeated_slots():
    html = ('<table summary="Offense"><tr><th>Position</th></tr>'
            '<tr><td>WR</td><td><a>A One</a></td><td></td></tr>'
            '<tr><td>WR</td><td><a>B Two</a></td><td><a>C Three</a></td></tr></table>')
    assert parse_depth(html) == {"Offense": {"WR": ["A One"], "WR2": ["B Two", "C Three"]}}


def test_parse_depth_summaryless_markup_with_jersey_prefix():
    html = ('<div>OFFENSE</div><table><tr><th>Position</th></tr>'
            '<tr><td>LT</td><td>76 Rasheed Walker</td></tr></table>')
    assert parse_depth(html) == {"Offense": {"LT": ["Rasheed Walker"]}}
    assert _clean_depth_name("9 Christian Watson") == "Christian Watson"


def test_parse_depth_no_tables_returns_empty():
    assert parse_depth("<html>client rendered</html>") == {}


def test_parse_injuries_assigns_team_by_preceding_name():
    row = ("<tr><td>Z Z</td><td>QB</td><td>Thumb</td><td>DNP</td><td>DNP</td><td></td>"
           "<td>UNSPECIFIED</td></tr>")
    tbl = "<table><tr><th>Game Status</th></tr>" + row + "</table>"
    _, out = parse_injuries("<h2>Tampa Bay Buccaneers</h2>" + tbl + "<h2>Dallas Cowboys</h2>" + tbl)
    assert [t["team"] for t in out] == ["TB", "DAL"]
    assert out[0]["rows"][0]["status"] == "UNSPECIFIED"
