"""Pre-lock salary + Vegas snapshot for the DFS ownership archive.

DraftKings drops a slate from its feed the moment the game kicks off, so the
salary pool (the #1 ownership driver -- value = proj/salary) has to be written
to disk while the slate is still live. Run this in the hours before lock.

    venv\\Scripts\\python.exe scripts/dfs_ownership/snapshot_slate_salaries.py --week 1
    ... --week 1 --game SF_LA           # just one game
    ... --week 1 --main                 # also snapshot the classic Main Slate

For every week-N game that currently has a live DK "Showdown Captain Mode"
slate it writes:

    data/dfs_ownership/2026/week_01/showdown_SF_LA/
        salaries_prelock.csv   name,team,pos,salary,cpt_salary,dk_flex_id,dk_cpt_id,snapshot_ts
        manifest.json          teams, draft group, Vegas lines, snapshot log

and appends every run to data/dfs_ownership/_processed/salary_history.parquet
(one row per player per snapshot -- keeps intra-week salary movement).

Downstream: after the contests settle, drop each contest's DK "Export Full
Standings" CSV into the matching slate folder (naming: name_price_xmax.csv,
e.g. hardcount_20_5max.csv) and run build_ownership_dataset.py.
"""
from __future__ import annotations

import argparse
import json
import os
import sys
from datetime import datetime, timezone

import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from src.scrapers.dk_scraper import (  # noqa: E402
    get_dk_showdown_slates, get_dk_showdown_salaries, get_dk_salaries,
    resolve_main_slate_draft_group_id,
)

BASE = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
SCHEDULE = os.path.join(BASE, "data", "external", "schedule_2026.csv")
ARCHIVE = os.path.join(BASE, "data", "dfs_ownership")


def _vegas_for_game(row) -> dict:
    """Implied team totals from the schedule's home-favored spread_line
    (matches src/api/app.py's convention)."""
    total = float(row["total_line"])
    spread = float(row["spread_line"])  # home favored by
    return {
        "total_line": total,
        "spread_line_home": spread,
        "home_implied": round((total + spread) / 2.0, 2),
        "away_implied": round((total - spread) / 2.0, 2),
    }


def _write_manifest(folder: str, base: dict, snap_entry: dict) -> None:
    path = os.path.join(folder, "manifest.json")
    if os.path.exists(path):
        with open(path) as f:
            m = json.load(f)
    else:
        m = {**base, "salary_snapshots": [], "contests": []}
    m.update({k: v for k, v in base.items() if k != "salary_snapshots"})
    m.setdefault("salary_snapshots", []).append(snap_entry)
    m.setdefault("contests", [])
    with open(path, "w") as f:
        json.dump(m, f, indent=2)


def _append_history(rows: list) -> None:
    if not rows:
        return
    out_dir = os.path.join(ARCHIVE, "_processed")
    os.makedirs(out_dir, exist_ok=True)
    path = os.path.join(out_dir, "salary_history.parquet")
    df = pd.DataFrame(rows)
    if os.path.exists(path):
        df = pd.concat([pd.read_parquet(path), df], ignore_index=True)
    df.to_parquet(path, index=False)


def _write_showdown_slate(folder: str, away: str, home: str, week: int, year: int,
                          dg: int, players: list, row, ts: str, late: bool, hist: list) -> None:
    """Write salaries_prelock.csv + merge manifest for one showdown slate."""
    os.makedirs(folder, exist_ok=True)
    pd.DataFrame([{
        "name": p["name"], "team": p["team"], "pos": p["pos"],
        "salary": p["salary"], "cpt_salary": p.get("cpt_salary"),
        "dk_flex_id": p.get("flex_id"), "dk_cpt_id": p.get("cpt_id"),
        "snapshot_ts": ts,
    } for p in players]).to_csv(os.path.join(folder, "salaries_prelock.csv"), index=False)

    base = {"year": year, "week": week, "slate_format": "showdown",
            "away_team": away, "home_team": home, "dk_draft_group_id": dg}
    if row is not None:
        base.update({"gameday": str(row.get("gameday", "")),
                     "gametime": str(row.get("gametime", "")),
                     "vegas": _vegas_for_game(row)})
    _write_manifest(folder, base, {
        "ts": ts, "n_players": len(players), "dk_draft_group_id": dg,
        "late_snapshot": late,  # True = pulled after kickoff; DK freezes salaries at final, ~= lock
    })
    for p in players:
        hist.append({"year": year, "week": week, "slate": f"showdown_{away}_{home}",
                     "name": p["name"], "team": p["team"], "pos": p["pos"],
                     "salary": p["salary"], "cpt_salary": p.get("cpt_salary"), "snapshot_ts": ts})


def snapshot_showdown(week: int, year: int, only_game: str | None, backfill: bool) -> int:
    """Snapshot every week-N showdown slate that's live in the DK lobby.

    backfill=True also (re)fetches slates that have already dropped from the
    lobby, by the dk_draft_group_id already recorded in that slate's
    manifest.json -- DK's *draftables* endpoint keeps serving a locked slate's
    pool long after the *lobby* listing is gone, so a game we missed can still
    be reconstructed (flagged late_snapshot=true).
    """
    sched = pd.read_csv(SCHEDULE)
    wk = sched[(sched["week"] == week) & (sched["game_type"] == "REG")]
    games = {f"{r.away_team}_{r.home_team}": r for _, r in wk.iterrows()}

    live = {tuple(sorted(s["teams"])): s for s in get_dk_showdown_slates().get("slates", []) if s.get("teams")}
    ts = datetime.now(timezone.utc).isoformat()
    n_done, hist = 0, []

    for key, row in games.items():
        if only_game and key.upper() != only_game.upper():
            continue
        away, home = row["away_team"], row["home_team"]
        folder = os.path.join(ARCHIVE, str(year), f"week_{week:02d}", f"showdown_{away}_{home}")
        slate = live.get(tuple(sorted([away, home])))
        late = False
        dg = slate["draft_group_id"] if slate else None

        if dg is None and backfill:
            man = os.path.join(folder, "manifest.json")
            if os.path.exists(man):
                dg = json.load(open(man)).get("dk_draft_group_id")
                late = True

        if dg is None:
            print(f"  {key}: no live DK Showdown slate"
                  f"{' and no draft group in manifest to backfill' if backfill else ' (already locked, or not posted yet)'}"
                  " -- skipped")
            continue

        sd = get_dk_showdown_salaries(draft_group_id=dg)
        if not sd.get("found"):
            print(f"  {key}: showdown salary fetch (draft group {dg}) returned nothing -- skipped")
            continue

        players = list(sd["players"]) + list(sd["defense"])
        _write_showdown_slate(folder, away, home, week, year, dg, players, row, ts, late, hist)
        tag = " (LATE / post-kickoff backfill)" if late else ""
        print(f"  {key}: {len(players)} players -> {os.path.relpath(folder, BASE)}{tag}")
        n_done += 1

    _append_history(hist)
    return n_done


def snapshot_main(week: int, year: int) -> None:
    sched = pd.read_csv(SCHEDULE)
    wk = sched[(sched["week"] == week) & (sched["game_type"] == "REG")]
    # Sticky per-week pin (see dk_scraper.resolve_main_slate_draft_group_id),
    # NOT an unpinned get_dk_salaries() -- that falls back to DK's raw live
    # "most open contests" default, which silently flips to a small leftover
    # slate once the real main slate's contests lock (confirmed live
    # 2026-09-14: it briefly pointed at a 2-team Monday-night slate instead
    # of the real 700+-player week-1 main slate). The pinned draft group's
    # draftables data stays fetchable long after it drops out of the lobby.
    dg = resolve_main_slate_draft_group_id(year, week)
    dk = get_dk_salaries(draft_group_id=dg)
    if not dk.get("is_live"):
        print("  Main Slate: DK salary feed not live -- skipped")
        return
    ts = datetime.now(timezone.utc).isoformat()
    folder = os.path.join(ARCHIVE, str(year), f"week_{week:02d}", "main_slate")
    os.makedirs(folder, exist_ok=True)

    rows, hist = [], []
    for _, r in wk.iterrows():
        for team in (r.away_team, r.home_team):
            if team not in dk["main_slate_teams"]:
                continue
            for (nname, tm), sal in dk["players"].items():
                if tm != team:
                    continue
                did = dk["player_ids"].get((nname, tm))
                rows.append({"name": nname, "team": tm, "pos": "", "salary": sal,
                             "dk_id": did, "snapshot_ts": ts})
            if team in dk["defense"]:
                rows.append({"name": f"{team} DST", "team": team, "pos": "DST",
                             "salary": dk["defense"][team],
                             "dk_id": dk["defense_ids"].get(team), "snapshot_ts": ts})
    if not rows:
        print("  Main Slate: no priced players for this week's teams -- skipped")
        return
    pd.DataFrame(rows).drop_duplicates(subset=["name", "team"]).to_csv(
        os.path.join(folder, "salaries_prelock.csv"), index=False)
    _write_manifest(folder, {
        "year": year, "week": week, "slate_format": "classic",
        "dk_draft_group_id": dk.get("draft_group_id"),
    }, {"ts": ts, "n_players": len(rows)})
    for x in rows:
        hist.append({"year": year, "week": week, "slate": "main_slate", **x, "cpt_salary": None,
                     "snapshot_ts": ts})
    _append_history(hist)
    print(f"  Main Slate: {len(rows)} priced players -> {folder}")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--week", type=int, required=True)
    ap.add_argument("--year", type=int, default=2026)
    ap.add_argument("--game", help="limit to one showdown game, e.g. SF_LA")
    ap.add_argument("--main", action="store_true", help="also snapshot the classic Main Slate")
    ap.add_argument("--no-showdown", action="store_true", help="skip showdown slates")
    ap.add_argument("--backfill", action="store_true",
                    help="also refetch slates already gone from the lobby, via the draft "
                         "group id in each slate's manifest.json (flagged late_snapshot)")
    args = ap.parse_args()

    print(f"Snapshotting week {args.week} ({datetime.now():%Y-%m-%d %H:%M})")
    if not args.no_showdown:
        n = snapshot_showdown(args.week, args.year, args.game, args.backfill)
        print(f"Showdown: {n} slate(s) snapshotted")
    if args.main:
        snapshot_main(args.week, args.year)
    print("Done. After the contests settle, drop each 'Export Full Standings' CSV into the "
          "matching slate folder (name_price_xmax.csv) and run build_ownership_dataset.py.")


if __name__ == "__main__":
    main()
