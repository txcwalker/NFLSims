"""DK Showdown ignore list (2026-10-05): players named in
data/overrides/2026/dk_showdown_ignored_players.json are dropped from the pool
get_dk_showdown_salaries returns (e.g. Charlie Smyth, the Saints' second
kicker). Matched on normalized name + team, so punctuation/case don't matter
and a same-named player on another team is kept."""
import json

import pytest

from src.scrapers import dk_scraper


def _entry():
    mk = lambda n, t, pos, s: {"name": n, "team": t, "pos": pos, "salary": s}
    return {
        "fetched_at": 1.0, "teams": ["ATL", "NO"],
        "players": {
            1: mk("Daniel Carlson", "NO", "K", 4800), 2: mk("Charlie Smyth", "NO", "K", 4800),
            3: mk("Nick Folk", "ATL", "K", 5400), 4: mk("Charlie Smyth", "ATL", "K", 4000),
        },
        "defense": {9: mk("Saints", "NO", "DST", 3600)},
    }


@pytest.fixture
def pool(monkeypatch, tmp_path):
    monkeypatch.setattr(dk_scraper, "_maybe_refresh_lobby", lambda force=False: None)
    monkeypatch.setattr(dk_scraper, "_showdown_slate_entry", lambda dg, force=False: _entry())
    path = tmp_path / "ignored.json"
    monkeypatch.setattr(dk_scraper, "SHOWDOWN_IGNORE_PATH", str(path))
    return path


def _names(res):
    return {(p["name"], p["team"]) for p in res["players"]}


def test_ignored_player_is_dropped_and_others_kept(pool):
    pool.write_text(json.dumps({"players": [{"name": "Charlie Smyth", "team": "NO"}]}))
    res = dk_scraper.get_dk_showdown_salaries(draft_group_id=1)
    assert ("Charlie Smyth", "NO") not in _names(res)
    assert {("Daniel Carlson", "NO"), ("Nick Folk", "ATL"), ("Charlie Smyth", "ATL")} <= _names(res)


def test_match_ignores_case_and_punctuation(pool):
    pool.write_text(json.dumps({"players": [{"name": "charlie  SMYTH.", "team": "no"}]}))
    assert ("Charlie Smyth", "NO") not in _names(dk_scraper.get_dk_showdown_salaries(draft_group_id=1))


def test_defense_can_be_ignored_too(pool):
    pool.write_text(json.dumps({"players": [{"name": "Saints", "team": "NO"}]}))
    assert dk_scraper.get_dk_showdown_salaries(draft_group_id=1)["defense"] == []


@pytest.mark.parametrize("content", [None, "not json", '{"players": "oops"}', '{"players": [{"name": "x"}]}'])
def test_missing_or_bad_file_ignores_no_one(pool, content):
    if content is not None:
        pool.write_text(content)
    assert len(dk_scraper.get_dk_showdown_salaries(draft_group_id=1)["players"]) == 4
