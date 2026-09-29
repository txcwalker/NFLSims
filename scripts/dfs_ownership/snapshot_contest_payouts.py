"""Save the REAL DK payout tables for a week's live contests, before they
disappear from the lobby.

Why (2026-09-29): DK's "Export Full Standings" CSV has no payout column, and
once a contest settles it's gone from the lobby -- so its rank-by-rank
payout table can only be fetched while the contest is still live. Week 3
had to be graded against Week 1's tables as a proxy; prize pools and field
sizes DO move week to week (First Down: $125K/148,632 in Wk3 vs.
$100K/118,906 in Wk4), so the proxy is only approximate. Run this once per
week any time between contests posting and lock:

    venv\\Scripts\\python.exe scripts/dfs_ownership/snapshot_contest_payouts.py --week 4
    ... --no-showdown        # main slate only
    ... --no-main            # showdown slates only (e.g. SNF/MNF once they post)

Showdown slates post on DK's own schedule (the Thursday game early in the
week, Sunday/Monday night later) -- only slates live at run time are
captured, so rerun later in the week to pick up the rest (merges, never
drops an earlier table).

Writes data/dfs_ownership/<year>/week_<NN>/<slate_id>/payouts.json, where
slate_id is "main_slate" or "showdown_<AWAY>_<HOME>" (same folders as
snapshot_slate_salaries.py):

    {"year", "week", "slate_id", "draft_group_id", "updated_at",
     "contests": [{contest_id, name, stem, entry_fee, prize_pool,
                   max_entries, max_entries_per_user, paying_positions,
                   guaranteed, start_time, fetched_at, tiers:[{rank_start,
                   rank_end, payout}]}, ...]}

`stem` is the <name>_<price>_<xmax>max key the standings CSV for that
contest should be saved under (e.g. "firstdown_1_20max"), which is how
score_paper_entries.py finds the table at grading time. Two contests can
share a stem (e.g. two $150 3-max Power Sweeps with different prize pools);
the grader disambiguates by field size, and falls back to price+xmax when a
CSV's name part is spelled differently ("hard_count" vs "hardcount").

Rerunning merges by contest_id (a failed refetch keeps the earlier table),
so it's safe to run more than once. Satellites / super-satellites are
skipped (ticket payouts, not cash).
"""
from __future__ import annotations

import argparse
import json
import os
import re
import sys
import time
from datetime import datetime, timezone

import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.scrapers.dk_scraper import (  # noqa: E402
    get_dk_contests, get_dk_contest_payout, resolve_main_slate_draft_group_id,
    get_dk_showdown_slates,
)

BASE = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
ARCHIVE = os.path.join(BASE, "data", "dfs_ownership")
SCHEDULE = os.path.join(BASE, "data", "external", "schedule_2026.csv")

_SKIP_RE = re.compile(r"satellite|supersat", re.IGNORECASE)

# The recurring contest families we build for / enter (and the archive's
# standings CSVs come from). A main slate lists ~1,400 contests and a single
# showdown ~900, mostly small leagues/satellites -- fetching every one is one
# request each, so only these are pulled by default (--all overrides). Add a
# family here when a new contest joins the rotation.
MAIN_FAMILIES = ("First Down", "Screen Pass", "mini-MAX", "Slant", "Power Sweep",
                 "Play Action", "Millionaire", "Spy")
SHOWDOWN_FAMILIES = ("mini-MAX", "Night Showdown", "Millionaire", "Huddle", "Play-Action",
                     "Play Action", "Hard Count", "Bubble Screen", "Wildcat", "First Down",
                     "Single Back", "Singleback", "Power Sweep", "Spy")

# DK names each primetime showdown's flagship after its night ("$1.5M
# Thursday Night Showdown"); Cam's CSVs call that contest "millionaire".
_SLUG_ALIASES = {"thursdaynight": "millionaire", "sundaynight": "millionaire",
                 "mondaynight": "millionaire", "saturdaynight": "millionaire"}


def contest_slug(name: str) -> str:
    """DK lobby name -> the short <name> part of a standings-CSV filename.

    Input:  e.g. "NFL $100K First Down [20 Entry Max]" or "NFL Showdown $80K
            mini-MAX [150 Entry Max] (PIT @ CLE)" (str, from the lobby).
    Output: e.g. "firstdown" / "minimax" (str). Drops the "NFL"/"Showdown"
            words, the "(AWAY @ HOME)" suffix, $-amount tokens ("$100K",
            "$2.75M", "$4,444"), bracketed notes, "Casual", and the "Fantasy
            Football" filler, then lowercases and strips non-alphanumerics --
            matching the names Cam already uses for the CSVs ("minimax",
            "slant", "powersweep", "millionaire"). _SLUG_ALIASES covers
            names that differ outright."""
    s = re.sub(r"\[.*?\]|\(.*?@.*?\)", " ", name)
    s = re.sub(r"\$[\d,.]+[KkMm]?", " ", s)
    s = re.sub(r"\b(NFL|Showdown|Casual|Fantasy Football)\b", " ", s, flags=re.IGNORECASE)
    slug = re.sub(r"[^a-z0-9]", "", s.lower())
    return _SLUG_ALIASES.get(slug, slug)


def fee_token(fee: float) -> str:
    """Entry fee -> filename price token: 0.5 -> ".5", 1.0 -> "1", 0.25 -> ".25"."""
    txt = f"{fee:g}"
    return txt[1:] if txt.startswith("0.") else txt


def contest_stem(c: dict) -> str:
    """Lobby contest dict -> "<slug>_<price>_<xmax>max" (the CSV naming key)."""
    return f"{contest_slug(c['name'])}_{fee_token(float(c['entry_fee']))}_{int(c['max_entries_per_user'])}max"


def snapshot_slate(year: int, week: int, slate_id: str, dg: int, families: tuple,
                   include: str | None, fetch_all: bool) -> str:
    """Fetch + save payout tables for one slate's cash contests.

    Inputs:  year/week, slate_id ("main_slate" / "showdown_AWAY_HOME" -- the
             archive folder), dg (DK draft group id), families (default name
             filter), include (optional name filter overriding families),
             fetch_all (no filter at all -- slow).
    Output:  path of the written payouts.json. One DK request per contest,
             throttled. Merges with any existing file by contest_id, so a
             failed refetch keeps the earlier table."""
    lobby = get_dk_contests(dg)
    contests = [c for c in lobby["contests"] if not _SKIP_RE.search(c["name"])]
    if include:
        contests = [c for c in contests if include.lower() in c["name"].lower()]
    elif not fetch_all:
        contests = [c for c in contests if any(f.lower() in c["name"].lower() for f in families)]

    folder = os.path.join(ARCHIVE, str(year), f"week_{week:02d}", slate_id)
    os.makedirs(folder, exist_ok=True)
    path = os.path.join(folder, "payouts.json")
    doc = json.load(open(path)) if os.path.exists(path) else {"contests": []}
    by_id = {c["contest_id"]: c for c in doc.get("contests", [])}

    print(f"{slate_id} (draft group {dg}): {len(contests)} cash contest(s) to fetch")
    ok = 0
    for c in contests:
        res = get_dk_contest_payout(c["contest_id"])
        now = datetime.now(timezone.utc).isoformat()
        if not res["tiers"]:
            print(f"  !! {c['name']} ({c['contest_id']}): {res['error']} -- keeping any earlier table")
            continue
        by_id[c["contest_id"]] = {
            "contest_id": c["contest_id"], "name": c["name"], "stem": contest_stem(c),
            "entry_fee": c["entry_fee"], "prize_pool": c["prize_pool"],
            "max_entries": c["max_entries"], "max_entries_per_user": c["max_entries_per_user"],
            "paying_positions": max(t["rank_end"] for t in res["tiers"]),
            "guaranteed": c.get("guaranteed"), "start_time": c.get("start_time"),
            "fetched_at": now, "tiers": res["tiers"],
        }
        ok += 1
        time.sleep(0.3)

    doc.update({"year": year, "week": week, "slate_id": slate_id, "draft_group_id": dg,
                "updated_at": datetime.now(timezone.utc).isoformat(),
                "contests": sorted(by_id.values(), key=lambda x: (x["stem"], -x["prize_pool"]))})
    with open(path, "w") as f:
        json.dump(doc, f, indent=2)
    print(f"  saved {ok} payout table(s) ({len(by_id)} total on file) -> {os.path.relpath(path, BASE)}")
    return path


def snapshot_showdowns(year: int, week: int, include: str | None, fetch_all: bool) -> int:
    """Snapshot every LIVE showdown slate that belongs to `week`.

    DK's lobby labels a showdown only by start time, so each slate's two
    teams (from get_dk_showdown_slates) are matched to the week's schedule
    to get the showdown_<AWAY>_<HOME> folder -- same mapping as
    snapshot_slate_salaries.snapshot_showdown. A live slate whose teams
    aren't a week-N game (e.g. next week's TNF already posted) is skipped.
    Output: number of slates snapshotted."""
    sched = pd.read_csv(SCHEDULE)
    wk = sched[(sched["week"] == week) & (sched["game_type"] == "REG")]
    games = {tuple(sorted([r.away_team, r.home_team])): (r.away_team, r.home_team) for _, r in wk.iterrows()}
    n = 0
    for sl in get_dk_showdown_slates(force_refresh=True).get("slates", []):
        game = games.get(tuple(sorted(sl.get("teams") or [])))
        if not game:
            print(f"  showdown dg {sl['draft_group_id']} {sl.get('teams')}: not a week-{week} game -- skipped")
            continue
        snapshot_slate(year, week, f"showdown_{game[0]}_{game[1]}", sl["draft_group_id"],
                       SHOWDOWN_FAMILIES, include, fetch_all)
        n += 1
    return n


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--week", type=int, required=True)
    ap.add_argument("--year", type=int, default=2026)
    ap.add_argument("--only", help="only contests whose name contains this text (e.g. 'First Down')")
    ap.add_argument("--all", action="store_true", help="every cash contest (one request each -- slow)")
    ap.add_argument("--no-main", action="store_true", help="skip the classic Main Slate")
    ap.add_argument("--no-showdown", action="store_true", help="skip showdown slates")
    args = ap.parse_args()
    if not args.no_main:
        dg = resolve_main_slate_draft_group_id(args.year, args.week, force_refresh=True)
        get_dk_contests(dg, force_refresh=True)
        snapshot_slate(args.year, args.week, "main_slate", dg, MAIN_FAMILIES, args.only, args.all)
    if not args.no_showdown:
        n = snapshot_showdowns(args.year, args.week, args.only, args.all)
        print(f"Showdown: {n} live slate(s) snapshotted -- rerun later in the week for slates not posted yet")


if __name__ == "__main__":
    main()
