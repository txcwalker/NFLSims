"""Tests for apply_preseason_overrides_v_0_1_0.py's splits-sync behaviour.

Focus: the 2026-09-06 "A1" change -- a hand-tuned flat `catch_rate` in the
override CSV must reach game_engine.py's completion model, which reads
`splits[zone]['catch_rate']`, not the flat field. Covers:
  - splits.primary.catch_rate is set to the flat CSV value
  - splits.redzone / splits.goalline shift by the SAME delta (zone shape kept)
  - the shifted zone values are clipped to [0.05, 0.99]
  - preseason_projection['splits'] is synced too (so an in-season taper
    doesn't wash the override out over weeks 1-5)
  - a blank CSV cell is a no-op
Also a regression guard on the pre-existing target_share/carry_share sync.
"""
import csv
import importlib
import json

import pytest

MOD = importlib.import_module(
    "scripts.roster_management.apply_preseason_overrides_v_0_1_0"
)


def _write_roster(path, team, players):
    path.write_text(json.dumps({"team": team, "year": 2026, "traits": players}))


def _write_csv(path, rows):
    # union of keys, player_name/team first
    cols = ["player_name", "team"]
    for r in rows:
        for k in r:
            if k not in cols:
                cols.append(k)
    with open(path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=cols)
        w.writeheader()
        for r in rows:
            w.writerow(r)


@pytest.fixture
def env(tmp_path, monkeypatch):
    rosters = tmp_path / "rosters"
    dna = tmp_path / "dna"
    rosters.mkdir()
    dna.mkdir()
    monkeypatch.setattr(MOD, "ROSTERS_DIR", str(rosters))
    monkeypatch.setattr(MOD, "DNA_DIR", str(dna))
    return rosters, dna


def _run(dna, rows):
    _write_csv(dna / "preseason_overrides_2026.csv", rows)
    MOD.apply(2026)


def test_catch_rate_propagates_to_zone_splits(env):
    rosters, dna = env
    # A receiver with real, differentiated zone catch rates (primary != rz != gl)
    player = {
        "Olave": {
            "pos": "WR", "catch_rate": 0.6441, "target_share": 0.27,
            "splits": {
                "primary": {"catch_rate": 0.6441, "target_share": 0.27},
                "redzone": {"catch_rate": 0.5789, "target_share": 0.26},
                "goalline": {"catch_rate": 0.3750, "target_share": 0.235},
            },
            "preseason_projection": {
                "catch_rate": 0.6441,
                "splits": {
                    "primary": {"catch_rate": 0.6441},
                    "redzone": {"catch_rate": 0.5789},
                    "goalline": {"catch_rate": 0.3750},
                },
            },
        }
    }
    _write_roster(rosters / "NO_traits_2026.json", "NO", player)
    _run(dna, [{"player_name": "Olave", "team": "NO", "catch_rate": "0.6986"}])

    out = json.load(open(rosters / "NO_traits_2026.json"))["traits"]["Olave"]
    s = out["splits"]
    delta = 0.6986 - 0.6441
    assert out["catch_rate"] == pytest.approx(0.6986)
    assert s["primary"]["catch_rate"] == pytest.approx(0.6986)          # primary <- flat
    assert s["redzone"]["catch_rate"] == pytest.approx(0.5789 + delta)  # shifted, shape kept
    assert s["goalline"]["catch_rate"] == pytest.approx(0.3750 + delta)
    # frozen projection synced the same way
    ps = out["preseason_projection"]["splits"]
    assert ps["primary"]["catch_rate"] == pytest.approx(0.6986)
    assert ps["goalline"]["catch_rate"] == pytest.approx(0.3750 + delta)


def test_catch_rate_shift_is_clipped(env):
    rosters, dna = env
    player = {
        "DeepGuy": {
            "pos": "WR", "catch_rate": 0.50, "target_share": 0.15,
            "splits": {
                "primary": {"catch_rate": 0.50},
                "redzone": {"catch_rate": 0.95},   # already high; +delta must clip at 0.99
                "goalline": {"catch_rate": 0.08},  # low; a downward move must clip at 0.05
            },
        }
    }
    _write_roster(rosters / "KC_traits_2026.json", "KC", player)
    # flat up to 0.60 => delta +0.10 ; redzone 0.95 + 0.10 -> clip 0.99
    _run(dna, [{"player_name": "DeepGuy", "team": "KC", "catch_rate": "0.60"}])
    s = json.load(open(rosters / "KC_traits_2026.json"))["traits"]["DeepGuy"]["splits"]
    assert s["primary"]["catch_rate"] == pytest.approx(0.60)
    assert s["redzone"]["catch_rate"] == pytest.approx(0.99)


def test_blank_catch_rate_cell_is_noop(env):
    rosters, dna = env
    player = {
        "X": {"pos": "WR", "catch_rate": 0.65, "target_share": 0.20,
              "splits": {"primary": {"catch_rate": 0.61},
                         "redzone": {"catch_rate": 0.55},
                         "goalline": {"catch_rate": 0.40}}}
    }
    _write_roster(rosters / "SF_traits_2026.json", "SF", player)
    _run(dna, [{"player_name": "X", "team": "SF", "catch_rate": ""}])
    s = json.load(open(rosters / "SF_traits_2026.json"))["traits"]["X"]["splits"]
    assert s["primary"]["catch_rate"] == pytest.approx(0.61)   # untouched
    assert s["redzone"]["catch_rate"] == pytest.approx(0.55)


def test_low_target_share_player_catch_rate_not_synced(env):
    """A deep-bench player's tiny-sample flat catch_rate must NOT be forced
    into the zone splits -- the build-time default is left alone."""
    rosters, dna = env
    player = {
        "Scrub": {"pos": "RB", "catch_rate": 1.0, "target_share": 0.01,
                  "splits": {"primary": {"catch_rate": 0.62},
                             "redzone": {"catch_rate": 0.62},
                             "goalline": {"catch_rate": 0.62}}}
    }
    _write_roster(rosters / "GB_traits_2026.json", "GB", player)
    _run(dna, [{"player_name": "Scrub", "team": "GB",
                "catch_rate": "1.0", "target_share": "0.01"}])
    out = json.load(open(rosters / "GB_traits_2026.json"))["traits"]["Scrub"]
    assert out["catch_rate"] == pytest.approx(1.0)             # flat field still set
    s = out["splits"]
    assert s["primary"]["catch_rate"] == pytest.approx(0.62)   # splits untouched
    assert s["goalline"]["catch_rate"] == pytest.approx(0.62)


def test_target_share_sync_still_works(env):
    """Regression guard on the pre-existing zone-share sync."""
    rosters, dna = env
    player = {
        "RB1": {"pos": "RB", "carry_share": 0.40,
                "splits": {"primary": {"carry_share": 0.40},
                           "redzone": {"carry_share": 0.40},     # in lockstep -> follows
                           "goalline": {"carry_share": 0.15}}}   # differentiated -> left alone
    }
    _write_roster(rosters / "CHI_traits_2026.json", "CHI", player)
    _run(dna, [{"player_name": "RB1", "team": "CHI", "carry_share": "0.55"}])
    s = json.load(open(rosters / "CHI_traits_2026.json"))["traits"]["RB1"]["splits"]
    assert s["primary"]["carry_share"] == pytest.approx(0.55)
    assert s["redzone"]["carry_share"] == pytest.approx(0.55)
    assert s["goalline"]["carry_share"] == pytest.approx(0.15)
