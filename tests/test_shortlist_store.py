"""Shortlist store: add / dedupe / remove / clear, isolated in a temp dir."""
import pytest

from src.api import optimizer_store, shortlist_store


@pytest.fixture(autouse=True)
def _tmp_base(tmp_path, monkeypatch):
    monkeypatch.setattr(optimizer_store, "_BASE", str(tmp_path))


def _lu(*names, flex="Z"):
    players = [{"name": n, "team": "KC", "pos": "WR", "slot": "WR"} for n in names]
    players.append({"name": flex, "team": "KC", "pos": "RB", "slot": "FLEX"})
    return {"players": players, "ev_pct": 1.0}


def test_add_and_read_roundtrip():
    out = shortlist_store.add_lineups(2026, 5, "classic", [_lu("A", "B")], "b1")
    assert out["added"] == 1 and out["duplicates"] == 0
    port = shortlist_store.read_shortlist(2026, 5, "classic")
    assert len(port["lineups"]) == 1
    assert port["lineups"][0]["source_build_id"] == "b1"


def test_duplicate_player_slot_set_is_skipped_even_when_order_differs():
    shortlist_store.add_lineups(2026, 5, "classic", [_lu("A", "B")])
    out = shortlist_store.add_lineups(2026, 5, "classic", [_lu("B", "A"), _lu("A", "C")])
    assert out["added"] == 1 and out["duplicates"] == 1
    assert len(out["lineups"]) == 2


def test_remove_and_clear():
    out = shortlist_store.add_lineups(2026, 5, "classic", [_lu("A"), _lu("B")])
    first = out["lineups"][0]["entry_id"]
    port = shortlist_store.remove_lineups(2026, 5, "classic", [first])
    assert len(port["lineups"]) == 1
    assert shortlist_store.clear_shortlist(2026, 5, "classic")["lineups"] == []


def test_weeks_and_formats_are_isolated():
    shortlist_store.add_lineups(2026, 5, "classic", [_lu("A")])
    assert shortlist_store.read_shortlist(2026, 6, "classic")["lineups"] == []
    assert shortlist_store.read_shortlist(2026, 5, "showdown")["lineups"] == []


def test_unknown_format_rejected():
    with pytest.raises(ValueError):
        shortlist_store.read_shortlist(2026, 5, "bogus")
