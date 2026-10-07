"""Regression tests for the bye-week guard in dk_scraper.resolve_main_slate_draft_group_id.

Bug (2026-10-06): week 5 was pinned to week 4's slate (draft group 154078, contains
KC who are on bye in week 5). Pins only grow, so the real slate (fewer contests)
could never replace it. The guard drops/refuses any non-manual pin or candidate
whose draft group contains a team with no game that week.
"""
import pytest

from src.scrapers import dk_scraper as d

BAD, GOOD = 111, 222


@pytest.fixture
def env(monkeypatch):
    """In-memory pin store + fake lobby/slates; week open, KC on bye."""
    store = {}
    monkeypatch.setattr(d, "_load_main_slate_pins", lambda: store)
    monkeypatch.setattr(d, "_save_main_slate_pins", lambda p: None)
    monkeypatch.setattr(d, "_maybe_refresh_lobby", lambda force=False: None)
    monkeypatch.setattr(d, "_lobby_cache", {
        "default_draft_group_id": GOOD,
        "slates": [{"draft_group_id": GOOD, "contest_count": 1500}],
    })
    monkeypatch.setattr(d, "_bye_teams_for_week", lambda y, w: ({"KC"}, False))
    teams = {BAD: {"KC", "DAL"}, GOOD: {"DAL", "TB"}}
    monkeypatch.setattr(d, "get_dk_salaries",
                        lambda draft_group_id=None, **k: {"main_slate_teams": teams[draft_group_id]})
    return store


def _pin(dg, count, **extra):
    return {"draft_group_id": dg, "contest_count": count, "pinned_at": 0, **extra}


def test_stale_pin_with_bye_team_is_replaced(env):
    env["2026"] = {"5": _pin(BAD, 1774)}  # bigger count would normally win forever
    assert d.resolve_main_slate_draft_group_id(2026, 5) == GOOD
    assert env["2026"]["5"]["draft_group_id"] == GOOD


def test_valid_pin_is_kept(env):
    env["2026"] = {"5": _pin(GOOD, 1774)}
    assert d.resolve_main_slate_draft_group_id(2026, 5) == GOOD


def test_manual_pin_is_respected(env):
    env["2026"] = {"5": _pin(BAD, 10**9, manual=True)}
    assert d.resolve_main_slate_draft_group_id(2026, 5) == BAD


def test_finished_week_skips_guard(env, monkeypatch):
    monkeypatch.setattr(d, "_bye_teams_for_week", lambda y, w: ({"KC"}, True))
    env["2026"] = {"4": _pin(BAD, 2011)}
    assert d.resolve_main_slate_draft_group_id(2026, 4) == BAD


def test_bad_live_default_is_not_pinned(env, monkeypatch):
    monkeypatch.setitem(d._lobby_cache, "default_draft_group_id", BAD)
    monkeypatch.setitem(d._lobby_cache, "slates", [{"draft_group_id": BAD, "contest_count": 9}])
    d.resolve_main_slate_draft_group_id(2026, 5)
    assert "5" not in env.get("2026", {})
