"""Scrape the official nfl.com team roster pages -> data/overrides/2026/
_nfl_com_roster.csv, the roster-status source of truth for the per-team
override sheet pipeline (roster_feed_v_0_1_0 reads this cache).

nfl.com is far more accurate than nfl_data_py's 2026 feed: exactly 53 ACT
per team, and it correctly tags EXE (commissioner's exempt), SUS (suspended),
RSR/RES (injured reserve), PUP, DEV (practice squad).

The roster table is plain server-rendered HTML -- stdlib urllib + regex, no
extra dependencies. ~32 GETs; run this by hand to refresh (weekly during the
season, or whenever roster news breaks). Cached output is committed so a
fresh clone / offline run still works off the last scrape.

Output columns: team, player_name, jersey, pos, nfl_status, height, weight,
experience, college.  nfl_status is the raw code (ACT/DEV/RSR/RES/PUP/NFI/
SUS/EXE/CUT/...); roster_feed_v_0_1_0.NFL_STATUS_TO_SLOT maps it to a
roster_slot.

Usage:
    venv\\Scripts\\python.exe scripts/roster_management/scrape_nfl_rosters_v_0_1_0.py [TEAM ...]
    (no args = all 32)
"""
import os
import re
import sys
import csv
import html
import time
import urllib.request

OUT = os.path.join("data", "overrides", "2026", "_nfl_com_roster.csv")
COLS = ["team", "player_name", "jersey", "pos", "nfl_status",
        "height", "weight", "experience", "college"]

# nfl.com URL slug -> repo team code
SLUG_TO_TEAM = {
    "arizona-cardinals": "ARI", "atlanta-falcons": "ATL", "baltimore-ravens": "BAL",
    "buffalo-bills": "BUF", "carolina-panthers": "CAR", "chicago-bears": "CHI",
    "cincinnati-bengals": "CIN", "cleveland-browns": "CLE", "dallas-cowboys": "DAL",
    "denver-broncos": "DEN", "detroit-lions": "DET", "green-bay-packers": "GB",
    "houston-texans": "HOU", "indianapolis-colts": "IND", "jacksonville-jaguars": "JAX",
    "kansas-city-chiefs": "KC", "las-vegas-raiders": "LV", "los-angeles-chargers": "LAC",
    "los-angeles-rams": "LA", "miami-dolphins": "MIA", "minnesota-vikings": "MIN",
    "new-england-patriots": "NE", "new-orleans-saints": "NO", "new-york-giants": "NYG",
    "new-york-jets": "NYJ", "philadelphia-eagles": "PHI", "pittsburgh-steelers": "PIT",
    "san-francisco-49ers": "SF", "seattle-seahawks": "SEA", "tampa-bay-buccaneers": "TB",
    "tennessee-titans": "TEN", "washington-commanders": "WAS",
}
TEAM_TO_SLUG = {v: k for k, v in SLUG_TO_TEAM.items()}
UA = {"User-Agent": "Mozilla/5.0 (research; nflsims roster refresh)"}


def _strip_tags(s):
    return re.sub(r"\s+", " ", html.unescape(re.sub(r"<[^>]+>", "", s))).strip()


def scrape_team(slug):
    url = f"https://www.nfl.com/teams/{slug}/roster"
    html = urllib.request.urlopen(
        urllib.request.Request(url, headers=UA), timeout=30
    ).read().decode("utf-8", "replace")
    m = re.search(r"<table[^>]*>.*?</table>", html, re.S)
    if not m:
        raise RuntimeError(f"no roster table found at {url}")
    rows = []
    for tr in re.findall(r"<tr[^>]*>(.*?)</tr>", m.group(0), re.S):
        cells = [_strip_tags(c) for c in re.findall(r"<t[dh][^>]*>(.*?)</t[dh]>", tr, re.S)]
        if len(cells) >= 8 and cells[0] and cells[0] != "Player":
            rows.append(cells[:8])
    return rows


def main():
    args = [a.upper() for a in sys.argv[1:]]
    teams = args or sorted(SLUG_TO_TEAM.values())

    existing = {}
    if os.path.exists(OUT):
        _, ext_rows = _read(OUT)
        for r in ext_rows:
            existing.setdefault(r["team"], []).append(r)

    all_rows, failed = [], []
    for team in sorted(SLUG_TO_TEAM.values()):
        if team not in teams and team in existing:
            all_rows.extend(existing[team])          # keep prior scrape for teams not requested
            continue
        slug = TEAM_TO_SLUG[team]
        try:
            parsed = scrape_team(slug)
            for name, jersey, pos, status, h, w, exp, college in parsed:
                all_rows.append({
                    "team": team, "player_name": name, "jersey": jersey, "pos": pos,
                    "nfl_status": status, "height": h, "weight": w,
                    "experience": exp, "college": college,
                })
            acts = sum(1 for r in parsed if r[3] == "ACT")
            print(f"{team:4} {len(parsed):3} players  ({acts} ACT)")
            time.sleep(1.0)
        except Exception as e:                        # noqa: BLE001
            print(f"{team:4} FAILED: {e}")
            failed.append(team)
            if team in existing:
                all_rows.extend(existing[team])

    all_rows.sort(key=lambda r: (r["team"], r["player_name"]))
    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    with open(OUT, "w", newline="", encoding="latin-1", errors="replace") as f:
        w = csv.DictWriter(f, fieldnames=COLS)
        w.writeheader()
        w.writerows(all_rows)
    print(f"\nWrote {len(all_rows)} rows to {OUT}"
          + (f"  ({len(failed)} team(s) failed: {failed})" if failed else ""))


def _read(path):
    for enc in ("utf-8-sig", "latin-1"):
        try:
            with open(path, newline="", encoding=enc) as f:
                r = csv.DictReader(f)
                return r.fieldnames, list(r)
        except UnicodeDecodeError:
            continue
    return [], []


if __name__ == "__main__":
    main()
