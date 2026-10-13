"""Scrape each NFL team's official site (roster, depth chart, injury report) into
per-team JSON snapshots -> data/overrides/2026/_team_status/latest/{TEAM}.json.

Roadmap item: injury/depth-chart monitoring (Phase 1 of 5 -- fetch + parse only).
Later phases: (2) diff snapshots -> games to re-sim, (3) feed dfs_status_ledger,
(4) OL/DL starter-out adjustment, (5) auto-trigger resims. Nothing here touches
the sim or any roster file.

All 32 sites share the NFL Digital platform, so one parser covers them:
    https://<domain>/team/players-roster/   (tables captioned Active / Reserve/... / Practice Squad)
    https://<domain>/team/depth-chart/      (tables summary=Offense|Defense|Special Teams; tiers First..Fourth)
    https://<domain>/team/injury-report/    (TWO tables: this team's, then this week's opponent's;
                                             the team name sits just before each table)
Plain server-rendered HTML -- stdlib only, no browser automation.

Injury-report semantics (verified 2026-10-07): practice columns hold DNP/LP/FP,
and `Game Status` is UNSPECIFIED until the Friday report, then Out/Doubtful/
Questionable. Each game's two reports appear on BOTH teams' pages, so
cross_check() compares them as a free integrity test.

Output per team:
    {team, fetched_at, week, roster:[{player,pos,jersey,section}],
     depth:{Offense:{LT:[names...]},...}, injuries:{team:[...], opponent_team, opponent:[...]}}

Usage:
    venv\\Scripts\\python.exe scripts/roster_management/scrape_team_status.py [TEAM ...]
    (no args = all 32; ~96 GETs, ~2 min with the polite delay)
"""
import os
import re
import sys
import json
import shutil
import html
import time
import datetime
import urllib.request

STATUS_DIR = os.path.join("data", "overrides", "2026", "_team_status")
OUT_DIR = os.path.join(STATUS_DIR, "latest")
PREV_DIR = os.path.join(STATUS_DIR, "previous")      # last run's snapshot, rotated in main(); diffed by diff_team_status.py
UA = {"User-Agent": "Mozilla/5.0 (research; nflsims status refresh)"}
DELAY_S = 1.0

# repo team code -> (official site domain, full team name as printed above injury tables)
TEAMS = {
    "ARI": ("azcardinals.com", "Arizona Cardinals"),
    "ATL": ("atlantafalcons.com", "Atlanta Falcons"),
    "BAL": ("baltimoreravens.com", "Baltimore Ravens"),
    "BUF": ("buffalobills.com", "Buffalo Bills"),
    "CAR": ("panthers.com", "Carolina Panthers"),
    "CHI": ("chicagobears.com", "Chicago Bears"),
    "CIN": ("bengals.com", "Cincinnati Bengals"),
    "CLE": ("clevelandbrowns.com", "Cleveland Browns"),
    "DAL": ("dallascowboys.com", "Dallas Cowboys"),
    "DEN": ("denverbroncos.com", "Denver Broncos"),
    "DET": ("detroitlions.com", "Detroit Lions"),
    "GB": ("packers.com", "Green Bay Packers"),
    "HOU": ("houstontexans.com", "Houston Texans"),
    "IND": ("colts.com", "Indianapolis Colts"),
    "JAX": ("jaguars.com", "Jacksonville Jaguars"),
    "KC": ("chiefs.com", "Kansas City Chiefs"),
    "LV": ("raiders.com", "Las Vegas Raiders"),
    "LAC": ("chargers.com", "Los Angeles Chargers"),
    "LA": ("therams.com", "Los Angeles Rams"),
    "MIA": ("miamidolphins.com", "Miami Dolphins"),
    "MIN": ("vikings.com", "Minnesota Vikings"),
    "NE": ("patriots.com", "New England Patriots"),
    "NO": ("neworleanssaints.com", "New Orleans Saints"),
    "NYG": ("giants.com", "New York Giants"),
    "NYJ": ("newyorkjets.com", "New York Jets"),
    "PHI": ("philadelphiaeagles.com", "Philadelphia Eagles"),
    "PIT": ("steelers.com", "Pittsburgh Steelers"),
    "SF": ("49ers.com", "San Francisco 49ers"),
    "SEA": ("seahawks.com", "Seattle Seahawks"),
    "TB": ("buccaneers.com", "Tampa Bay Buccaneers"),
    "TEN": ("titansonline.com", "Tennessee Titans"),
    "WAS": ("commanders.com", "Washington Commanders"),
}
# repo team code -> ESPN team id (ESPN depth-chart JSON fallback, used only when a team site lacks one)
ESPN_IDS = {
    "ARI": 22, "ATL": 1, "BAL": 33, "BUF": 2, "CAR": 29, "CHI": 3, "CIN": 4, "CLE": 5, "DAL": 6,
    "DEN": 7, "DET": 8, "GB": 9, "HOU": 34, "IND": 11, "JAX": 30, "KC": 12, "LV": 13, "LAC": 24,
    "LA": 14, "MIA": 15, "MIN": 16, "NE": 17, "NO": 18, "NYG": 19, "NYJ": 20, "PHI": 21, "PIT": 23,
    "SF": 25, "SEA": 26, "TB": 27, "TEN": 10, "WAS": 28,
}
NAME_TO_TEAM = {full: code for code, (_, full) in TEAMS.items()}
OL_SLOTS = ("LT", "LG", "C", "RG", "RT")
SLOT_ALIASES = {"LOT": "LT", "ROT": "RT"}        # CIN-style naming -> canonical OL slot


def _text(s):
    """Strip tags/entities/whitespace from an HTML fragment -> plain str."""
    return re.sub(r"\s+", " ", html.unescape(re.sub(r"<[^>]+>", "", s))).strip()


def _cells(tr):
    """Cell texts of one <tr> (th + td) -> list[str]."""
    return [_text(c) for c in re.findall(r"<t[dh][^>]*>(.*?)</t[dh]>", tr, re.S)]


def _tables(page):
    """All <table> blocks of a page with their start offset -> list[(offset, html)]."""
    return [(m.start(), m.group(0)) for m in re.finditer(r"<table.*?</table>", page, re.S)]


def fetch(domain, path):
    """GET https://<domain>/team/<path>/ with one retry -> str (decoded HTML)."""
    url = f"https://www.{domain}/team/{path}/"
    for attempt in (1, 2):
        try:
            return urllib.request.urlopen(
                urllib.request.Request(url, headers=UA), timeout=30
            ).read().decode("utf-8", "replace")
        except Exception:                                    # noqa: BLE001
            if attempt == 2:
                raise
            time.sleep(3)


def parse_roster(page):
    """Roster page -> list[dict(player,pos,jersey,section)]; section = table caption
    (Active / Reserve/Injured / Reserve/Non-Football Injury / Practice Squad ...)."""
    rows = []
    for _, t in _tables(page):
        cap = re.search(r"<caption[^>]*>(.*?)</caption>", t, re.S)
        section = _text(cap.group(1)) if cap else ""
        for tr in re.findall(r"<tr[^>]*>(.*?)</tr>", t, re.S):
            c = _cells(tr)
            if len(c) >= 3 and c[0] and c[0] != "Player":
                rows.append({"player": c[0], "jersey": c[1], "pos": c[2], "section": section})
    return rows


def _clean_depth_name(x):
    """'9 Christian Watson' (GB-style jersey prefix) -> 'Christian Watson'."""
    return re.sub(r"^\d+\s+", "", x)


def parse_depth(page):
    """Depth-chart page -> {unit: {pos_slot: [names by tier, blanks dropped]}}.
    Two markups exist: `summary="Offense"` tables (most teams) and summary-less
    tables whose unit is the OFFENSE/DEFENSE/SPECIAL TEAMS header just above (GB);
    some prefix names with a jersey number.  A slot that repeats (WR, DL, CB...)
    becomes WR, WR2, WR3 so no row is lost.  Returns {} if the page has no tables
    (e.g. HOU renders its chart client-side)."""
    depth = {}
    for off, t in _tables(page):
        unit = re.search(r'summary="([^"]+)"', t)
        if unit:
            unit = unit.group(1)
        else:
            hdr = re.findall(r"(OFFENSE|DEFENSE|SPECIAL TEAMS)", _text(page[max(0, off - 600):off]))
            if not hdr:
                continue
            unit = hdr[-1].title()
        slots = {}
        for tr in re.findall(r"<tr[^>]*>(.*?)</tr>", t, re.S):
            c = _cells(tr)
            if len(c) < 2 or c[0] == "Position":
                continue
            key, n = c[0], 1
            while key in slots:
                n += 1
                key = f"{c[0]}{n}"
            slots[key] = [_clean_depth_name(x) for x in c[1:] if x]
        depth[unit] = slots
    return depth


def fetch_espn_depth(code):
    """ESPN depth-chart JSON -> same shape as parse_depth(): {unit: {SLOT: [names]}}.
    ESPN groups by formation (offense e.g. '3WR 1TE', defense 'Base 4-3 D', 'Special
    Teams'); each slot's athletes are already in depth order. Slot keys are upper-
    cased ESPN abbreviations (LT, LG, C, RG, RT, LDE, ...)."""
    url = f"https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams/{ESPN_IDS[code]}/depthcharts"
    data = json.loads(urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=30).read())
    depth = {}
    for chart in data.get("depthchart", []):
        name = chart.get("name", "")
        unit = "Special Teams" if "Special" in name else "Defense" if name.endswith(" D") else "Offense"
        slots = depth.setdefault(unit, {})
        for key, v in chart.get("positions", {}).items():
            slots[key.upper()] = [a["displayName"] for a in v.get("athletes", []) if "displayName" in a]
    return depth


def parse_injuries(page):
    """Injury page -> (week, [ {team, rows:[...]} per table ]).  Team = nearest full
    team name in the text just before each table; row = player,pos,injury,practice[],status."""
    wk = re.search(r"WEEK (\d+)\s*</?[^>]*>?[^<]{0,40}selected", page)  # best effort, may be None
    out = []
    for off, t in _tables(page):
        if "Game Status" not in t:
            continue
        before = _text(page[max(0, off - 2500):off])
        name = max(NAME_TO_TEAM, key=before.rfind)           # latest-printed team name wins
        team = NAME_TO_TEAM[name] if before.rfind(name) >= 0 else None
        rows = []
        for tr in re.findall(r"<tr[^>]*>(.*?)</tr>", t, re.S):
            c = _cells(tr)
            if len(c) >= 7 and c[0] != "Player":
                rows.append({"player": c[0], "pos": c[1], "injury": c[2],
                             "practice": c[3:6], "status": c[6]})
        out.append({"team": team, "rows": rows})
    return (wk.group(1) if wk else None), out


def _has_ol(depth):
    """True if the offense chart names all five OL slots (after CIN-style aliasing)."""
    have = {SLOT_ALIASES.get(k, k) for k in depth.get("Offense", {})}
    return all(x in have for x in OL_SLOTS)


def scrape_team(code):
    """Fetch+parse the 3 pages for one team -> snapshot dict (see module docstring)."""
    domain = TEAMS[code][0]
    roster = parse_roster(fetch(domain, "players-roster")); time.sleep(DELAY_S)
    depth = parse_depth(fetch(domain, "depth-chart")); time.sleep(DELAY_S)
    depth_source = "team_site"
    if not _has_ol(depth):                       # e.g. HOU renders its chart client-side
        try:
            depth, depth_source = fetch_espn_depth(code), "espn"
        except Exception:                        # noqa: BLE001 -- keep whatever the site gave; sanity_flags will say so
            pass
    week, inj = parse_injuries(fetch(domain, "injury-report")); time.sleep(DELAY_S)
    own = next((t for t in inj if t["team"] == code), None)
    opp = next((t for t in inj if t["team"] != code), None)
    return {
        "team": code,
        "fetched_at": datetime.datetime.now(datetime.timezone.utc).isoformat(timespec="seconds"),
        "week": week,
        "roster": roster,
        "depth": depth,
        "depth_source": depth_source,
        "injuries": {
            "team": own["rows"] if own else [],
            "opponent_team": opp["team"] if opp else None,   # None on a bye week
            "opponent": opp["rows"] if opp else [],
        },
    }


def sanity_flags(snap):
    """Cheap structural checks so a layout change fails loudly -> list[str].
    An empty injury table is NOT a failure early in the week (reports post
    Wed-Fri, Thursday-night teams first); it is reported as 'no report yet'."""
    flags = []
    active = sum(1 for r in snap["roster"] if r["section"] == "Active")
    if not 45 <= active <= 60:
        flags.append(f"active roster rows={active}")
    off = snap["depth"].get("Offense")
    if off is None:
        flags.append("no depth chart from team site or ESPN")
    else:
        have = {SLOT_ALIASES.get(k, k) for k in off}
        missing = [x for x in OL_SLOTS if x not in have]
        if missing:
            flags.append(f"depth chart missing OL slots {missing}")
    if snap["injuries"]["opponent_team"] is None and not snap["injuries"]["team"]:
        flags.append("no injury report yet")
    return flags


def _norm_inj(r):
    """Injury row -> comparable (name, practice, status): strips a trailing ', POS'
    and treats the '(-)' / UNSPECIFIED placeholders as the same 'unset' status."""
    status = "" if r["status"] in ("(-)", "UNSPECIFIED") else r["status"]
    return (re.sub(r",\s*[A-Z]{1,3}$", "", r["player"]), tuple(x for x in r["practice"] if x not in ("(-)", "")), status)


def cross_check(snaps):
    """Each game's reports appear on both teams' pages -> compare them.
    Returns list[str] of disagreements (empty = consistent)."""
    bad = []
    for code, s in snaps.items():
        opp = s["injuries"]["opponent_team"]
        if opp in snaps:
            mine = {_norm_inj(r) for r in s["injuries"]["opponent"]}
            theirs = {_norm_inj(r) for r in snaps[opp]["injuries"]["team"]}
            if mine != theirs:
                diff = sorted(x[0] for x in mine ^ theirs)
                bad.append(f"{code} page's view of {opp} differs from {opp}'s own page: {diff}")
    return bad


def main():
    teams = [a.upper() for a in sys.argv[1:]] or sorted(TEAMS)
    os.makedirs(OUT_DIR, exist_ok=True)
    snaps, failed = {}, []
    for code in teams:
        try:
            snap = scrape_team(code)
        except Exception as e:                               # noqa: BLE001
            print(f"{code:4} FAILED: {e}")
            failed.append(code)
            continue
        snaps[code] = snap
        latest = os.path.join(OUT_DIR, f"{code}.json")
        if os.path.exists(latest):               # keep last run for the Phase 2 diff
            os.makedirs(PREV_DIR, exist_ok=True)
            shutil.copyfile(latest, os.path.join(PREV_DIR, f"{code}.json"))
        with open(latest, "w", encoding="utf-8") as f:
            json.dump(snap, f, indent=1, ensure_ascii=False)
        n_inj = len(snap["injuries"]["team"])
        flags = sanity_flags(snap)
        print(f"{code:4} depth={snap['depth_source']:9} roster={len(snap['roster']):3} inj={n_inj:2} opp={snap['injuries']['opponent_team']}"
              + (f"  !! {'; '.join(flags)}" if flags else ""))
    for line in cross_check(snaps):
        print("CROSS-CHECK:", line)
    print(f"\n{len(snaps)} ok, {len(failed)} failed {failed or ''}")


if __name__ == "__main__":
    main()
