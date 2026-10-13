"""Tests for src/pickems/ledger.py -- change-only appends and open/previous multipliers (temp dir, no repo files)."""
from src.pickems.ledger import current_board, record_paste
from src.pickems.underdog_parser import PickemLeg


def leg(player, side, mult, line=0.5, market="Rush + Rec TDs", team="DET"):
    return PickemLeg(player, team, "CAR", False, "Sun 7:20PM CDT", market, line, side, mult)


def test_only_new_and_changed_legs_are_appended(tmp_path):
    b = str(tmp_path)
    first = [leg("A", "Higher", 1.5), leg("A", "Lower", 2.5), leg("B", "Higher", 3.0)]
    assert record_paste(4, first, now=100.0, base_dir=b) == {"new": 3, "changed": 0, "unchanged": 0}

    # A.Higher moves, A.Lower and B unchanged, C is new.
    second = [leg("A", "Higher", 1.4), leg("A", "Lower", 2.5), leg("B", "Higher", 3.0), leg("C", "Higher", 5.0)]
    assert record_paste(4, second, now=200.0, base_dir=b) == {"new": 1, "changed": 1, "unchanged": 2}

    rows = {(r["player"], r["side"]): r for r in current_board(4, base_dir=b)}
    a = rows[("A", "Higher")]
    assert a["multiplier"] == 1.4 and a["open_multiplier"] == 1.5 and a["prev_multiplier"] == 1.5
    assert a["n_changes"] == 1 and a["first_seen"] == 100.0 and a["last_changed"] == 200.0
    assert rows[("A", "Lower")]["prev_multiplier"] is None and rows[("A", "Lower")]["n_changes"] == 0
    assert len(rows) == 4


def test_last_seen_updates_even_when_leg_unchanged(tmp_path):
    from src.pickems.ledger import leg_histories
    b = str(tmp_path)
    record_paste(4, [leg("A", "Higher", 1.5), leg("B", "Higher", 3.0)], now=100.0, base_dir=b)
    record_paste(4, [leg("A", "Higher", 1.5)], now=900.0, base_dir=b)           # A unchanged, B absent
    h = {x["player"]: x for x in leg_histories(4, base_dir=b)}
    assert h["A"]["history"] == [(100.0, 1.5)] and h["A"]["last_seen"] == 900.0  # re-pasted -> a real close
    assert h["B"]["last_seen"] == 100.0                                         # never re-seen


def test_board_accumulates_across_pastes_and_separates_weeks(tmp_path):
    b = str(tmp_path)
    record_paste(4, [leg("A", "Higher", 1.5)], now=1.0, base_dir=b)
    record_paste(4, [leg("B", "Higher", 2.0, market="Receiving Yards", line=54.5)], now=2.0, base_dir=b)  # A absent
    record_paste(5, [leg("Z", "Higher", 9.0)], now=3.0, base_dir=b)
    assert sorted(r["player"] for r in current_board(4, base_dir=b)) == ["A", "B"]  # A stays on the board
    assert [r["player"] for r in current_board(5, base_dir=b)] == ["Z"]
    assert current_board(6, base_dir=b) == []
