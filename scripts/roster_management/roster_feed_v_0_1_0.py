"""Shared roster-status helpers for the per-team override sheet pipeline
(export_team_season_overrides / build_week_overrides / build_roster_md).

Roster status now comes from the official nfl.com team pages, cached to
data/overrides/2026/_nfl_com_roster.csv by scrape_nfl_rosters_v_0_1_0.py --
far more accurate than nfl_data_py's 2026 feed (exactly 53 ACT per team;
correctly tags EXE / SUS / RSR / PUP / DEV). nfl_data_py is still consulted,
best-effort, only to bridge a gsis player_id by name.

Callers trust the roster feed for `roster_slot` on players already in a
curated sheet, and for adding players it lists as active/reserve on a team.

I/O: reads the cached CSV; one nfl.import_seasonal_rosters([2026]) call for
the id bridge (failure -> blank ids, non-fatal).
"""
import csv
import os

SEASON = 2026
# positions eligible to be ADDED to a sheet as a new player. FB folded to RB.
SKILL_POSITIONS = {"QB", "RB", "WR", "TE", "FB"}
POS_ORDER = {"QB": 0, "RB": 1, "WR": 2, "TE": 3}
NFL_ROSTER_CSV = os.path.join("data", "overrides", "2026", "_nfl_com_roster.csv")

# nfl.com status code -> roster_slot written into the sheets.
# ACT=active, DEV=practice squad, RSR=Reserve/Injured, RES=reserve (generic --
# usually injured; default ir, hand-refine in the sheet), RSN=Reserve/NFI,
# PUP, SUS=suspended, EXE=commissioner's exempt, CUT/RLS/UFA/etc.=off the team.
NFL_STATUS_TO_SLOT = {
    "ACT": "active", "INA": "active",
    "DEV": "practice_squad",
    "RSR": "ir", "RES": "ir", "R01": "ir",
    "PUP": "pup",
    "RSN": "nfi", "NFI": "nfi",
    "SUS": "suspended",
    "EXE": "exempt",
    "CUT": "left_team", "RLS": "left_team", "UFA": "left_team",
    "RET": "left_team", "TRD": "left_team", "WAI": "left_team",
}

ACTIVE_SLOTS = {"active"}
RESERVE_SLOTS = {"ir", "pup", "nfi", "suspended", "exempt"}
DROP_SLOTS = {"left_team"}          # off the team -> row dropped on seed

# single-number return-week seed per reserve slot (99 = out for the season).
# Only seeds the sheet's `return_week` column; hand-tune there afterward.
RETURN_WEEK_SEED = {
    "ir": 8, "pup": 9, "nfi": 9, "suspended": 7, "exempt": 99,
}


def read_rows(path):
    """Read a CSV to a list of dicts, tolerant of whatever Excel saved it as:
    tries utf-8 (with BOM), falls back to latin-1 (never errors on Windows
    cp1252 output). Returns (fieldnames, rows)."""
    for enc in ("utf-8-sig", "latin-1"):
        try:
            with open(path, newline="", encoding=enc) as f:
                r = csv.DictReader(f)
                rows = list(r)
                return r.fieldnames, rows
        except UnicodeDecodeError:
            continue
    raise UnicodeDecodeError("csv", b"", 0, 1, f"could not decode {path}")


def write_rows(path, fieldnames, rows):
    """Write rows as latin-1 (matches the existing preseason_overrides
    pipeline; a stray smart-quote from Excel is replaced rather than crashing)."""
    with open(path, "w", newline="", encoding="latin-1", errors="replace") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames)
        w.writeheader()
        w.writerows(rows)


def clean_name(name):
    """Suffix-/period-stripped display name -- matches
    build_2026_rosters_v_0_1_0.clean_name and preseason_overrides_2026.csv."""
    if not isinstance(name, str):
        return ""
    name = name.strip()
    for s in [" Jr.", " Sr.", " III", " II", " IV", " V"]:
        if name.endswith(s):
            name = name[: -len(s)]
    return name.replace(".", "")


_SUFFIXES = (" jr", " sr", " ii", " iii", " iv", " v")


def match_key(name):
    """Aggressive cross-reference key: suffix-stripped, lowercase,
    alphanumerics only. Bridges 'JMichael Sturdivant' vs 'J. Michael
    Sturdivant', 'Keeney-James' vs 'Keeney James', 'Marvin Harrison Jr.' vs
    'Marvin Harrison'. Display always uses clean_name, never this."""
    s = "".join(c if c.isalnum() else " " for c in str(name).lower()).strip()
    s = " ".join(s.split())
    for suf in _SUFFIXES:
        if s.endswith(suf):
            s = s[: -len(suf)].strip()
    return s.replace(" ", "")


def _gsis_id_bridge():
    """{(team, match_key): gsis_id} from nfl_data_py, best-effort. Any failure
    (offline, feed down, schema drift) -> {} and the pipeline runs id-less."""
    try:
        import nfl_data_py as nfl
        df = nfl.import_seasonal_rosters([SEASON])
        out = {}
        for _, r in df.iterrows():
            pid = r.get("player_id")
            if isinstance(pid, str) and pid:
                out[(r.get("team"), match_key(r.get("player_name")))] = pid
        return out
    except Exception:                                # noqa: BLE001
        return {}


def _lastfirst(name):
    """(last-word, first-initial) from a match_key -- e.g. 'kennethgainwell'
    can't give this, so work off the spaced form."""
    s = "".join(c if c.isalnum() else " " for c in str(name).lower()).split()
    for suf in ("jr", "sr", "ii", "iii", "iv", "v"):
        if s and s[-1] == suf:
            s = s[:-1]
    if len(s) < 2:
        return None
    return (s[-1], s[0][:1])


def resolve_status(team, name, by_name, lastfirst_index):
    """Look up a player's feed record: exact match_key/clean-name first, then a
    unique (team, last-name, first-initial) fallback (catches Kenny/Kenneth,
    Cam/Cameron). Returns (rec_or_None, how)."""
    rec = by_name.get((team, match_key(name))) or by_name.get((team, clean_name(name)))
    if rec is not None:
        return rec, "exact"
    lf = _lastfirst(name)
    if lf is not None:
        cands = lastfirst_index.get((team, *lf), [])
        if len(cands) == 1:
            return cands[0], "lastname+initial"
    return None, "none"


def build_feed_index():
    """-> (by_name, team_feed, lastfirst_index)
      by_name:         (team, match_key) -> feed record (also keyed by clean name)
      team_feed:       team -> [feed record ...]  (skill-position players)
      lastfirst_index: (team, last, first_initial) -> [feed record ...]
    feed record: {name, clean, key, pos, player_id, nfl_status, team,
                  experience, slot}.  Use resolve_status() to look players up."""
    if not os.path.exists(NFL_ROSTER_CSV):
        raise SystemExit(
            f"{NFL_ROSTER_CSV} not found -- run scrape_nfl_rosters_v_0_1_0.py first.")
    _, rows = read_rows(NFL_ROSTER_CSV)
    ids = _gsis_id_bridge()

    by_name, team_feed, lastfirst_index = {}, {}, {}
    for row in rows:
        pos = (row.get("pos") or "").strip().upper()
        team = (row.get("team") or "").strip()
        name = (row.get("player_name") or "").strip()
        if not name or not team:
            continue
        key = match_key(name)
        status = (row.get("nfl_status") or "").strip().upper()
        rec = {
            "name": name,
            "clean": clean_name(name),
            "key": key,
            "pos": "RB" if pos == "FB" else pos,
            "player_id": ids.get((team, key), ""),
            "nfl_status": status,
            "team": team,
            "experience": (row.get("experience") or "").strip(),
            "slot": NFL_STATUS_TO_SLOT.get(status, "active"),
        }
        # by_name: EVERY player, any position -- so a curated sheet's
        # "Travis Hunter" matches even though nfl.com lists him at CB.
        if rec["clean"]:
            by_name[(team, rec["clean"])] = rec
        if key:
            by_name[(team, key)] = rec
        # team_feed + the fuzzy last-name index: skill positions only, so a
        # sheet's "Charlie Jones" (WR) can't fuzzy-match nfl.com's "Christian
        # Jones" (OT).
        if pos in SKILL_POSITIONS:
            team_feed.setdefault(team, []).append(rec)
            lf = _lastfirst(name)
            if lf is not None:
                lastfirst_index.setdefault((team, *lf), []).append(rec)
    return by_name, team_feed, lastfirst_index
