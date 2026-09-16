"""Shared DK "Export Full Standings" CSV parsing -- the guts of
build_ownership_dataset.py, factored out so score_paper_entries.py and
eval_field.py (which both also need to read a settled contest's field) don't
duplicate the regex/parsing logic. See data/dfs_ownership/README.md for the
full pipeline story; this module is just the "read one standings CSV" step.

Standings CSV naming: <name>_<price>_<xmax>.csv   e.g. hardcount_20_5max.csv
    name   free text, the contest's identity ("hardcount", "milly", "3pointstance")
    price  entry fee in dollars, fractional ok ("20", ".5", "3.33"); trailing
           "c" = cents ("25c")
    xmax   max entries per user ("5max", "150max", "1max" for single-entry)
Field size is NOT in the filename -- it's read straight off the CSV as max(Rank).

DK's standings export packs two things into one CSV, side by side:
  cols A-F   one row per entry:  Rank, EntryId, EntryName, TimeRemaining, Points, Lineup
  cols H-J   one row per player:  Player, Roster Position, %Drafted, FPTS
Showdown lists every player TWICE in the summary (Roster Position = CPT / FLEX,
each with its own %Drafted and FPTS -- the CPT row's FPTS is already the 1.5x
score), so the CPT/FLEX split AND each player's real result are both published,
not just a combined ownership number. Classic exports omit Roster Position.
"""
from __future__ import annotations

import re
from collections import Counter

import pandas as pd

# DK writes a team defense in a Lineup string by city/nickname, not abbrev.
DST_NICKNAMES = {
    "cardinals": "ARI", "falcons": "ATL", "ravens": "BAL", "bills": "BUF",
    "panthers": "CAR", "bears": "CHI", "bengals": "CIN", "browns": "CLE",
    "cowboys": "DAL", "broncos": "DEN", "lions": "DET", "packers": "GB",
    "texans": "HOU", "colts": "IND", "jaguars": "JAX", "chiefs": "KC",
    "raiders": "LV", "chargers": "LAC", "rams": "LA", "dolphins": "MIA",
    "vikings": "MIN", "patriots": "NE", "saints": "NO", "giants": "NYG",
    "jets": "NYJ", "eagles": "PHI", "steelers": "PIT", "49ers": "SF",
    "seahawks": "SEA", "buccaneers": "TB", "titans": "TEN", "commanders": "WAS",
}

# <name>_<price>_<xmax>max  e.g. hard_count_20_5max, minimax_.5_150max, milly_.25_20max
FNAME_RE = re.compile(r"^(?P<name>.+?)_(?P<price>\d*\.?\d+c?)_(?P<xmax>\d+)max$", re.IGNORECASE)
# "CPT <name> FLEX <name> FLEX <name> ..." -> [(slot, name), ...]
SLOT_RE = re.compile(r"\b(CPT|FLEX)\s+(.+?)(?=\s+(?:CPT|FLEX)\b|$)")
# Classic's Lineup string instead reads "DST <name> FLEX <name> QB <name> RB
# <name> RB <name> TE <name> WR <name> WR <name> WR <name>" -- same shape,
# different slot vocabulary, so it needs its own regex.
CLASSIC_SLOT_RE = re.compile(r"\b(QB|RB|WR|TE|FLEX|DST)\s+(.+?)(?=\s+(?:QB|RB|WR|TE|FLEX|DST)\b|$)")


def slot_re_for(slate_format: str) -> re.Pattern:
    """CLASSIC_SLOT_RE for 'classic', SLOT_RE (CPT/FLEX) for everything
    else (showdown) -- the one format-dependent choice needed to parse a
    Lineup string; every other parsing step in this module works the same
    once you have (slot, name) pairs."""
    return CLASSIC_SLOT_RE if slate_format == "classic" else SLOT_RE


def parse_filename(stem: str) -> dict | None:
    """`<name>_<price>_<xmax>max` -> {contest_name, entry_fee, max_entries}, or
    None if the stem doesn't match (not a standings export we recognize)."""
    m = FNAME_RE.match(stem)
    if not m:
        return None
    price_tok = m.group("price").lower()
    entry_fee = float(price_tok[:-1]) / 100.0 if price_tok.endswith("c") else float(price_tok)
    return {"contest_name": m.group("name"), "entry_fee": entry_fee,
            "max_entries": int(m.group("xmax"))}


def stakes_tier(entry_fee: float) -> str:
    """Two-tier sharpness proxy -- casual and sharp money build very
    differently even at the same field size."""
    if entry_fee <= 5:
        return "casual"
    if entry_fee >= 20:
        return "sharp"
    return "mid"


def field_bucket(field_size: int) -> str:
    """Primary ownership-model axis -- the target's scale AND shape change
    with it (chalk tops ~45% in a Milly, ~25% in a 150-max, lumpier still
    single-entry)."""
    if field_size >= 50_000:
        return "large"
    if field_size >= 1_000:
        return "mid"
    return "small"


def player_key(name: str, team: str, pos: str, normalize_fn) -> tuple[str, str | None]:
    """Canonical join key for a player, used consistently across the salary
    snapshot, the standings' Lineup strings, and OUR lineups (from the
    optimizer's pool) so all three can be matched against each other.
    `("__dst__", TEAM)` for a defense, else `(normalized_name, None)`.
    `normalize_fn` is `src.scrapers.dk_scraper.normalize_player_name` --
    passed in rather than imported here to keep this module free of the
    sys.path/src-import dance every caller already does its own way."""
    if str(pos or "").upper() == "DST":
        return "__dst__", team
    return normalize_fn(str(name)), None


def resolve_lineup_name(raw: str, normalize_fn) -> tuple[str, str | None]:
    """A `Lineup` column token -> the same key scheme as `player_key`. A DST
    nickname ("Seahawks") resolves via DST_NICKNAMES; anyone else is a normal
    player name."""
    key = normalize_fn(raw)
    abbr = DST_NICKNAMES.get(key)
    if abbr:
        return "__dst__", abbr
    return key, None


def read_standings(path: str) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Split DK's dual-purpose export into (entries, dk_player_summary).

    entries  columns: rank, lineup, points (points is the entrant's real
        score for that lineup -- the field-score distribution used for
        percentile cutoffs and "where would our score have ranked")
    summary  columns: player, roster_position, drafted_pct, fpts
        `roster_position` is "" when the export doesn't carry the CPT/FLEX
        split (classic exports, or an older showdown format).
    """
    df = pd.read_csv(path, dtype=str, keep_default_na=False, encoding="utf-8-sig")
    cols = {c.lower().strip().lstrip("﻿"): c for c in df.columns}

    lineup_col = cols.get("lineup")
    rank_col = cols.get("rank")
    points_col = cols.get("points")
    if not lineup_col or not rank_col:
        raise ValueError("no Rank/Lineup columns -- not a standings export?")
    keep = [rank_col, lineup_col] + ([points_col] if points_col else [])
    names = ["rank", "lineup"] + (["points"] if points_col else [])
    entries = df[keep].copy()
    entries.columns = names
    entries = entries[entries["lineup"].str.strip() != ""]
    entries["rank"] = pd.to_numeric(entries["rank"], errors="coerce")
    if "points" in entries.columns:
        entries["points"] = pd.to_numeric(entries["points"], errors="coerce")

    player_col = cols.get("player")
    drafted_col = next((cols[k] for k in cols if "drafted" in k or "owned" in k), None)
    rp_col = cols.get("roster position")
    fpts_col = cols.get("fpts")
    summ = pd.DataFrame(columns=["player", "roster_position", "drafted_pct", "fpts"])
    if player_col and drafted_col:
        keep = [player_col, drafted_col] + ([rp_col] if rp_col else []) + ([fpts_col] if fpts_col else [])
        names = ["player", "drafted_pct"] + (["roster_position"] if rp_col else []) + (["fpts"] if fpts_col else [])
        s = df[keep].copy()
        s.columns = names
        s = s[s["player"].str.strip() != ""].copy()
        s["drafted_pct"] = pd.to_numeric(
            s["drafted_pct"].str.replace("%", "", regex=False), errors="coerce")
        if "fpts" in s:
            s["fpts"] = pd.to_numeric(s["fpts"], errors="coerce")
        if "roster_position" not in s:
            s["roster_position"] = ""
        summ = s
    return entries, summ


def ownership_from_entries(entries: pd.DataFrame, normalize_fn) -> tuple[dict, int, int]:
    """Recompute CPT / FLEX counts per player from every Lineup string.
    Returns ({key: {cpt, flex}}, field_size, top_lineup_dupes)."""
    field_size = int(entries["rank"].max()) if entries["rank"].notna().any() else len(entries)
    cpt: Counter = Counter()
    flex: Counter = Counter()
    lineup_sigs: Counter = Counter()
    for lu in entries["lineup"]:
        slots = SLOT_RE.findall(lu)
        if not slots:
            continue
        sig_parts = []
        for slot, raw in slots:
            key, dst_abbr = resolve_lineup_name(raw.strip(), normalize_fn)
            pkey = (key, dst_abbr) if dst_abbr else (key, None)
            if slot.upper() == "CPT":
                cpt[pkey] += 1
                sig_parts.append(("CPT", pkey))
            else:
                flex[pkey] += 1
                sig_parts.append(("FLEX", pkey))
        lineup_sigs[frozenset(sig_parts)] += 1
    top_dupes = max(lineup_sigs.values()) if lineup_sigs else 0
    keys = set(cpt) | set(flex)
    counts = {k: {"cpt": cpt.get(k, 0), "flex": flex.get(k, 0)} for k in keys}
    return counts, field_size, top_dupes


def actual_scores_from_summary(dk_summary: pd.DataFrame, normalize_fn) -> dict:
    """{player_key: base/FLEX FPTS} from the summary block's published
    per-player results -- the ground truth for "what did this player actually
    score" used by paper-entry settling and field-optimal reconstruction.
    Showdown's CPT row FPTS is already x1.5'd, so this only keeps the FLEX
    (or roster-position-less) row -- callers apply the 1.5x themselves for a
    captain slot, same convention as the live app."""
    scores: dict = {}
    if "fpts" not in dk_summary.columns:
        return scores
    for _, sr in dk_summary.iterrows():
        rp = str(sr.get("roster_position", "")).upper().strip()
        if rp == "CPT":
            continue
        if pd.isna(sr.get("fpts")):
            continue
        nk = normalize_fn(str(sr["player"]))
        abbr = DST_NICKNAMES.get(nk)
        key = ("__dst__", abbr) if abbr else (nk, None)
        scores[key] = float(sr["fpts"])
    return scores
