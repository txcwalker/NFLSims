"""Parser for text copied from the Underdog Pick'em board (Pickems > Underdog Lines).

Underdog's Terms of Use (6.ix) allow only manual browser access, so the input
is whatever the user selects-all/copies from the board and pastes -- never a
scrape. This module only turns that pasted text into structured rows; matching
players to our rosters and grading against the sims live elsewhere.

Entry point: parse_board(text) -> list[PickemLeg].

Pasted format (blank lines vary, section headers like "First TD Scorer" can
appear between blocks and are ignored -- the market name is read from inside
each block):

    athlete or team avatar
    Jahmyr Gibbs
    DET @ CAR - Sun 7:20PM CDT
    0.5
    Rush + Rec TDs
    Higher
    1.28x
    Lower
    2.88x
"""
from __future__ import annotations

import re
from dataclasses import dataclass, asdict
from typing import Dict, List, Optional

BLOCK_START = "athlete or team avatar"
# "DET @ CAR - Sun 7:20PM CDT" (away) or "BUF vs NE - Sun 12:00PM CDT" (home).
MATCHUP_RE = re.compile(r"^([A-Z]{2,4}) (@|vs) ([A-Z]{2,4}) - (.+)$")
LINE_RE = re.compile(r"^-?\d+(\.\d+)?$")
MULT_RE = re.compile(r"^(\d+(?:\.\d+)?)x$")
SIDES = {"Higher", "Lower", "Yes", "No", "Over", "Under"}


@dataclass
class PickemLeg:
    """One selectable side of one prop: a single (player, market, side, multiplier)."""
    player: str          # as shown by Underdog (needs name-matching to our rosters)
    team: str            # the player's own team (first team in the matchup text)
    opp: str             # opponent
    home: bool           # True when the matchup reads "TEAM vs OPP"
    kickoff: str         # raw text after the dash, e.g. "Sun 7:20PM CDT"
    market: str          # e.g. "Rush + Rec TDs", "First TD Scorer"
    line: float          # e.g. 0.5
    side: str            # Higher / Lower / Yes / ...
    multiplier: float    # payout multiplier for this leg, e.g. 1.28

    def to_dict(self) -> Dict:
        return asdict(self)


def _parse_block(lines: List[str]) -> List[PickemLeg]:
    """Turn one player block (blank lines already removed) into 0+ legs.

    Inputs: lines -- block text lines AFTER the avatar marker: name, matchup,
        line value, market, then (side, multiplier) pairs; trailing section
        headers or junk are ignored.
    Output: one PickemLeg per (side, multiplier) pair. A block that does not
        have name/matchup/line/market in that order returns [] (skipped, not
        an error -- pasted pages carry unrelated text).
    Purpose: positional parse, because the paste has no delimiters.
    """
    if len(lines) < 4:
        return []
    name, matchup = lines[0], MATCHUP_RE.match(lines[1])
    if not matchup or not LINE_RE.match(lines[2]):
        return []
    team, sep, opp, kickoff = matchup.groups()
    line, market = float(lines[2]), lines[3]

    legs: List[PickemLeg] = []
    i = 4
    while i < len(lines) - 1:
        m = MULT_RE.match(lines[i + 1])
        if lines[i] in SIDES and m:
            legs.append(PickemLeg(name, team, opp, sep == "vs", kickoff, market,
                                  line, lines[i], float(m.group(1))))
            i += 2
        else:
            i += 1  # section header or unknown text
    return legs


def parse_board(text: str) -> List[PickemLeg]:
    """Parse a whole pasted board into legs.

    Inputs: text (str) -- raw pasted text, any mix of markets.
    Output: list[PickemLeg], in paste order. Empty list if nothing parseable.
    Purpose: split on the avatar marker that starts every player block, drop
        blank lines, hand each block to _parse_block.
    """
    lines = [ln.strip() for ln in text.replace("\r", "").split("\n")]
    blocks: List[List[str]] = []
    cur: Optional[List[str]] = None
    for ln in lines:
        if ln == BLOCK_START:
            cur = []
            blocks.append(cur)
        elif ln and cur is not None:
            cur.append(ln)
    out: List[PickemLeg] = []
    for b in blocks:
        out.extend(_parse_block(b))
    return out
