"""Tests for src/pickems/underdog_parser.py, using excerpts of a real pasted board
(2026-10-03) including its quirks: irregular blank lines, one-sided blocks, a
"First TD Scorer" section header between blocks, and a block with no valid body.
"""
from src.pickems.underdog_parser import parse_board

BOARD = """
athlete or team avatar
Jahmyr Gibbs
DET @ CAR - Sun 7:20PM CDT


0.5
Rush + Rec TDs

Higher
1.28x

Lower
2.88x
athlete or team avatar
Aaron Jones
MIN vs MIA - Sun 3:05PM CDT


0.5
Rush + Rec TDs

Higher
1.62x
athlete or team avatar
Terry McLaurin
WAS vs IND - Sun 8:30AM CDT

0.5
Rush + Rec TDs

Higher
3.4x
First TD Scorer
athlete or team avatar
Jonathan Taylor
IND @ WAS - Sun 8:30AM CDT

0.5
First TD Scorer

Yes
3.47x
athlete or team avatar
Broken Block
not a matchup
"""


def test_two_sided_block():
    legs = parse_board(BOARD)
    g = [l for l in legs if l.player == "Jahmyr Gibbs"]
    assert [(l.side, l.multiplier) for l in g] == [("Higher", 1.28), ("Lower", 2.88)]
    assert g[0].team == "DET" and g[0].opp == "CAR" and g[0].home is False
    assert g[0].line == 0.5 and g[0].market == "Rush + Rec TDs"
    assert g[0].kickoff == "Sun 7:20PM CDT"


def test_one_sided_block_and_home_flag():
    jones = [l for l in parse_board(BOARD) if l.player == "Aaron Jones"]
    assert len(jones) == 1 and jones[0].side == "Higher" and jones[0].home is True


def test_section_header_does_not_leak_into_next_block():
    legs = parse_board(BOARD)
    mc = [l for l in legs if l.player == "Terry McLaurin"]
    assert [(l.market, l.multiplier) for l in mc] == [("Rush + Rec TDs", 3.4)]
    jt = [l for l in legs if l.player == "Jonathan Taylor"]
    assert [(l.market, l.side, l.multiplier) for l in jt] == [("First TD Scorer", "Yes", 3.47)]


def test_garbage_block_skipped_and_total_count():
    legs = parse_board(BOARD)
    assert len(legs) == 5
    assert all(l.player != "Broken Block" for l in legs)


def test_empty_and_windows_newlines():
    assert parse_board("") == []
    assert len(parse_board(BOARD.replace("\n", "\r\n"))) == 5
