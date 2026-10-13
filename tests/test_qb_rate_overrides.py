"""qb_rate_overrides.csv (2026-10-08): hand values for QB cpoe / air yards /
sack / scramble rate, applied at the DFS week compile because week sheets can
only carry shares. Checks the flat + splits writes, the week window, and that
unknown players/fields are skipped."""
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "scripts", "roster_management"))
from apply_team_week_overrides_v_0_1_0 import apply_qb_rate_overrides  # noqa: E402

HEADER = "team,player_name,field,value,effective_week,end_week,note\n"


def _traits():
    return {"Tyson Bagent": {"pos": "QB", "cpoe": 14.3,
                             "splits": {"primary": {"cpoe": 14.3}, "redzone": {"cpoe": 7.0}, "goalline": {"cpoe": -1.8}}}}


def _write(tmp_path, body):
    p = tmp_path / "o.csv"
    p.write_text(HEADER + body)
    return str(p)


def test_cpoe_override_hits_flat_primary_and_zones(tmp_path):
    t = _traits()
    apply_qb_rate_overrides(t, "CHI", 5, _write(tmp_path, "CHI,Tyson Bagent,cpoe,0.0,5,,x\n"))
    p = t["Tyson Bagent"]
    assert p["cpoe"] == 0.0 and all(z["cpoe"] == 0.0 for z in p["splits"].values())


def test_window_and_team_and_unknowns_are_respected(tmp_path):
    body = ("CHI,Tyson Bagent,cpoe,0.0,6,,not yet\n"          # starts next week
            "CHI,Tyson Bagent,cpoe,1.0,1,4,expired\n"        # ended
            "GB,Tyson Bagent,cpoe,2.0,1,,wrong team\n"
            "CHI,Nobody Real,cpoe,3.0,1,,no such player\n"
            "CHI,Tyson Bagent,target_share,0.5,1,,field not allowed\n")
    t = _traits()
    assert apply_qb_rate_overrides(t, "CHI", 5, _write(tmp_path, body)) == []
    assert t["Tyson Bagent"]["cpoe"] == 14.3


def test_missing_file_is_a_noop(tmp_path):
    assert apply_qb_rate_overrides(_traits(), "CHI", 5, str(tmp_path / "nope.csv")) == []
