"""Import YOUR real DK entries from a settled contest's standings CSV into the
"real" Bankroll account, so score_paper_entries.py grades them alongside the
paper (baseline / catered) builds.

Why this exists (2026-09-29): real entries are made on DK directly, not via
the optimizer's 📝 button, so they never land in paper_entries.json on their
own. Week 1's were imported by a one-off; this makes it repeatable. The
standings export already carries every one of your lineups (EntryName
"<username> (n/20)"), so no DK entry-history download is needed.

Usage (after dropping the settled standings CSVs into the slate folder):

    venv\\Scripts\\python.exe scripts/dfs_ownership/import_real_entries.py --year 2026 --week 3
    ... --contests firstdown screenpass     # default: the two contests Cam actually enters
    ... --slate main_slate                  # default

Then rerun score_paper_entries.py --year 2026 (NO --week: it rewrites the
whole results parquet from scratch).

Idempotent: an entry whose dk_entry_id is already in the slate's
paper_entries.json is skipped, so rerunning is safe.

Payout table: DK's standings export has no payout column, so the real
rank-by-rank `payout_tiers` is copied from the nearest other week's entry
for the same contest stem + fee (these are recurring contests with the same
prize pool / field size week to week -- same approach used to grade Week 3's
paper entries). If none exists, the entry is imported without one and
score_paper_entries.py falls back to its generic curve (flagged in output).
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import sys

import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.scrapers.dk_scraper import normalize_player_name  # noqa: E402
from src.api import paper_store  # noqa: E402
from src.api.sim_replay_store import load_my_usernames, _is_mine  # noqa: E402
from scripts.dfs_ownership.standings_parser import (  # noqa: E402
    parse_filename, slot_re_for, resolve_lineup_name,
)

BASE = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
ARCHIVE = os.path.join(BASE, "data", "dfs_ownership")


def _salary_lookup(folder: str) -> dict:
    """salaries_prelock.csv -> {normalized_name: (team, pos)}.

    Inputs:  folder (str) -- slate folder holding salaries_prelock.csv.
    Outputs: dict keyed by normalize_player_name(name); used to attach
             team/pos to the bare names in a DK Lineup string (which gives
             neither). Last snapshot wins if a name repeats."""
    df = pd.read_csv(os.path.join(folder, "salaries_prelock.csv"))
    return {normalize_player_name(r["name"]): (r["team"], r["pos"]) for _, r in df.iterrows()}


def _reference_payout(year: int, contest_name: str, entry_fee: float, skip_week: int) -> dict | None:
    """Find the real DK payout metadata for a recurring contest from another
    week's paper entries.

    Inputs:  year, contest stem (e.g. "screenpass"), entry fee, and the week
             being imported (skipped -- it's the one missing the table).
    Outputs: {"payout_tiers", "prize_pool", "paying_positions",
             "contest_type", "_from_week"} or None if no week has one.
    Prefers the closest week (by |week - skip_week|)."""
    best = None
    for path in glob.glob(os.path.join(ARCHIVE, str(year), "week_*", "*", "paper_entries.json")):
        wk = int(os.path.basename(os.path.dirname(os.path.dirname(path))).split("_")[1])
        if wk == skip_week:
            continue
        for e in (json.load(open(path)) or {}).get("entries", []):
            if (e.get("contest_name") == contest_name and e.get("payout_tiers")
                    and abs(float(e.get("entry_fee") or 0) - entry_fee) < 1e-9):
                if best is None or abs(wk - skip_week) < abs(best["_from_week"] - skip_week):
                    best = {"payout_tiers": e["payout_tiers"], "prize_pool": e.get("prize_pool"),
                            "paying_positions": e.get("paying_positions"),
                            "contest_type": e.get("contest_type") or "top_heavy", "_from_week": wk}
                break
    return best


def _patch_entry(year: int, week: int, slate_id: str, entry_id: str, patch: dict) -> None:
    """Merge `patch` into one stored entry. paper_store.update_entry only
    allows the notes/late_swap annotation fields, so linking a DK entry id
    (and back-filling a payout table) writes through paper_store's own
    read/atomic-write helpers instead."""
    path = paper_store._entries_path(year, week, slate_id)
    doc = paper_store._read_all(path)
    for e in doc["entries"]:
        if e.get("entry_id") == entry_id:
            e.update(patch)
    paper_store._atomic_write_json(path, doc)


def _lineup_key(players: list) -> frozenset:
    """Order/slot-independent identity of a lineup: normalized names, DSTs by
    team. Input: an entry's players list. Output: frozenset for equality
    (tolerates "James Cook III" vs "James Cook" and "Defense" vs nickname)."""
    return frozenset(f"dst:{p.get('team')}" if str(p.get("pos", "")).upper() == "DST"
                     else normalize_player_name(p.get("name", "")) for p in players)


def import_contest(year: int, week: int, slate_id: str, csv_path: str, existing_ids: set) -> int:
    """Import every one of your lineups from one standings CSV.

    Inputs:  year/week/slate_id (destination paper_entries.json), csv_path
             (a <name>_<price>_<xmax>max.csv standings export), existing_ids
             (dk_entry_ids already stored -- mutated as entries are added).
    Outputs: number of entries written (via paper_store.add_entry).
    Tricky bit: players are keyed by the same resolve_lineup_name scheme the
    grader uses, so a DST comes out as name "Defense" + team abbrev (DK
    writes it by nickname, e.g. "DST Vikings") -- exactly what the grader's
    player_key expects."""
    folder = os.path.dirname(csv_path)
    meta = parse_filename(os.path.splitext(os.path.basename(csv_path))[0])
    usernames = {u.lower() for u in load_my_usernames()}
    sal = _salary_lookup(folder)
    slot_re = slot_re_for("classic" if slate_id == "main_slate" else "showdown")

    df = pd.read_csv(csv_path, dtype=str, keep_default_na=False, encoding="utf-8-sig")
    cols = {c.strip().lstrip("﻿").lower(): c for c in df.columns}
    mine = df[df[cols["entryname"]].apply(lambda n: _is_mine(n, usernames))]

    ref = _reference_payout(year, meta["contest_name"], meta["entry_fee"], week)
    if ref is None:
        print(f"  {meta['contest_name']}: no real payout table in any other week -- generic curve will be used")
    else:
        print(f"  {meta['contest_name']}: payout table copied from week {ref['_from_week']}")

    # Real-account entries for this fee that aren't yet tied to a DK entry id
    # (see the link-instead-of-duplicate step below).
    unclaimed_real = [e for e in paper_store.list_entries(year, week, slate_id)
                      if e.get("account_id") == "real" and not e.get("dk_entry_id")
                      and abs(float(e.get("entry_fee") or 0) - meta["entry_fee"]) < 1e-9]

    added = linked = 0
    for _, r in mine.iterrows():
        dk_id = str(r[cols["entryid"]])
        if dk_id in existing_ids:
            continue
        players, unresolved = [], []
        for slot, raw in slot_re.findall(r[cols["lineup"]]):
            key, dst_team = resolve_lineup_name(raw.strip(), normalize_player_name)
            if dst_team:
                players.append({"slot": slot, "name": "Defense", "team": dst_team, "pos": "DST"})
                continue
            team, pos = sal.get(key, ("", ""))
            if not team:
                unresolved.append(raw.strip())
            players.append({"slot": slot, "name": raw.strip(), "team": team, "pos": pos})
        if unresolved:
            print(f"    entry {dk_id}: no salary match for {', '.join(unresolved)} (team/pos left blank)")

        # A lineup already 📝-flagged into the "real" account from the
        # optimizer (no dk_entry_id, but carries the model's predictions) is
        # the same entry -- stamp the DK id onto it instead of duplicating
        # (Week 2 had exactly this: 20 pre-flagged + 20 in the standings).
        existing = next((e for e in unclaimed_real if _lineup_key(e["players"]) == _lineup_key(players)), None)
        if existing is not None:
            unclaimed_real.remove(existing)
            patch = {"dk_entry_id": dk_id, "contest_name": meta["contest_name"]}
            if ref and not existing.get("payout_tiers"):
                patch.update({k: ref[k] for k in ("payout_tiers", "prize_pool", "paying_positions", "contest_type")})
            _patch_entry(year, week, slate_id, existing["entry_id"], patch)
            existing_ids.add(dk_id)
            linked += 1
            continue

        entry = {
            "slate_format": "classic" if slate_id == "main_slate" else "showdown",
            "source": "real_import",
            "label": f"Real {meta['contest_name']} (DK entry {dk_id})",
            "contest_name": meta["contest_name"],
            "entry_fee": meta["entry_fee"],
            "max_entries": meta["max_entries"],
            "players": players,
            "model": {},
            "build_id": None,
            "account_id": "real",
            "dk_entry_id": dk_id,
        }
        if ref:
            entry.update({k: ref[k] for k in ("payout_tiers", "prize_pool", "paying_positions", "contest_type")})
        paper_store.add_entry(year, week, slate_id, entry)
        existing_ids.add(dk_id)
        added += 1
    print(f"  {meta['contest_name']}: {len(mine)} of your entries in standings, {added} newly imported, "
          f"{linked} linked to an already-flagged real entry")
    return added + linked


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--year", type=int, required=True)
    ap.add_argument("--week", type=int, required=True)
    ap.add_argument("--slate", default="main_slate")
    ap.add_argument("--contests", nargs="+", default=["firstdown", "screenpass"],
                    help="contest stems to import (default: the ones actually entered)")
    args = ap.parse_args()

    folder = os.path.join(ARCHIVE, str(args.year), f"week_{args.week:02d}", args.slate)
    existing_ids = {str(e.get("dk_entry_id")) for e in paper_store.list_entries(args.year, args.week, args.slate)
                    if e.get("dk_entry_id")}
    total = 0
    for csv_path in sorted(glob.glob(os.path.join(folder, "*.csv"))):
        meta = parse_filename(os.path.splitext(os.path.basename(csv_path))[0])
        if meta and meta["contest_name"] in args.contests:
            total += import_contest(args.year, args.week, args.slate, csv_path, existing_ids)
    print(f"Imported {total} real entr{'y' if total == 1 else 'ies'} into {os.path.relpath(folder, BASE)}/paper_entries.json")


if __name__ == "__main__":
    main()
