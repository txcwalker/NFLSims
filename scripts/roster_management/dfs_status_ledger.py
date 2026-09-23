"""Sticky gameday Active/Inactive toggles for the Game Explorer page.

Problem (2026-09-22): a UI toggle used to live ONLY in that week's
data/overrides/2026/week_NN/{TEAM}.csv `dfs_status` column, so it vanished
the moment the next week's sheet was built from season_long (week 2 had 16
real gameday outs; week 3 had none). This ledger is the persistent source of
truth for those toggles; week sheets are just where they get materialized.

File: data/overrides/{year}/dfs_status_ledger.json
    {
      "KC": {
        "<match_key(player_name)>": {
          "player_name": "Patrick Mahomes",
          "entries": [
            {"week": 3, "status": "out"},
            {"week": 5, "status": "active", "reserve_sig": "ir|8"}
          ]
        }
      }
    }

Semantics (see week_roster_v_0_1_0.effective_dfs_status): the latest entry
at or before the week being built wins, so a toggle carries forward until
you flip it again. An "active" made while the player sat on a reserve slot
(IR/PUP/...) overrides that slot -- but only for that same injury stint.

Entry points:
    load_ledger / save_ledger                -- JSON I/O
    record_toggle                            -- called by POST /api/dfs/roster_status
    statuses_for_week                        -- called by every week-sheet build
    python dfs_status_ledger.py seed <week>  -- one-time import of an existing
                                                week's sheet toggles (opt-in)
"""
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.week_roster_v_0_1_0 import (  # noqa: E402
    effective_dfs_status, reserve_signature, DFS_OUT,
)
from roster_feed_v_0_1_0 import match_key, read_rows  # noqa: E402


def ledger_path(year=2026, base_dir="."):
    """Inputs: year (int), base_dir (str, repo root -- app.py passes its
    absolute BASE_DIR, scripts run from the repo root use ".").
    Output: str path to that season's ledger JSON."""
    return os.path.join(base_dir, "data", "overrides", str(year), "dfs_status_ledger.json")


def load_ledger(year=2026, base_dir="."):
    """Inputs: year, base_dir (see ledger_path).
    Output: dict {team: {match_key: {"player_name", "entries"}}}; empty dict
    if the file doesn't exist yet (first toggle creates it)."""
    path = ledger_path(year, base_dir)
    if not os.path.exists(path):
        return {}
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def save_ledger(ledger, year=2026, base_dir="."):
    """Inputs: ledger (dict, as load_ledger), year, base_dir.
    Output: none -- writes the JSON (sorted keys, stable diffs in git).
    Written via a temp file + os.replace so a crash mid-write can't leave a
    truncated ledger behind (it's the only copy of these decisions)."""
    path = ledger_path(year, base_dir)
    os.makedirs(os.path.dirname(path), exist_ok=True)
    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        json.dump(ledger, f, indent=2, sort_keys=True)
    os.replace(tmp, path)


def record_toggle(ledger, team, player_name, week, status, season_row=None):
    """Inputs: ledger (dict, mutated in place), team (str abbr), player_name
    (str, display name from the UI), week (int, the week the toggle was made
    in), status ("out" | "active"), season_row (dict | None, the player's
    season_long row -- needed to stamp reserve_sig on an IR override).
    Output: none (ledger mutated).
    Purpose: one entry per (player, week) -- re-toggling within the same
    week replaces that week's entry instead of stacking. Entries for LATER
    weeks are left alone: they were explicit decisions made for those weeks."""
    key = match_key(player_name)
    team_book = ledger.setdefault(team, {})
    rec = team_book.setdefault(key, {"player_name": player_name, "entries": []})
    rec["player_name"] = player_name
    entry = {"week": int(week), "status": status}
    if status != DFS_OUT and season_row is not None:
        sig = reserve_signature(season_row, int(week))
        if sig:
            entry["reserve_sig"] = sig
    rec["entries"] = sorted([e for e in rec["entries"] if int(e["week"]) != int(week)] + [entry],
                            key=lambda e: int(e["week"]))


def statuses_for_week(ledger, team, week, season_rows):
    """Inputs: ledger (dict), team (str), week (int), season_rows (list of
    dict, the team's season_long rows).
    Output: {match_key: dfs_status} for every player with an applicable
    ledger entry this week. Players absent from the result have no sticky
    toggle -- callers keep the week sheet's existing value for them."""
    team_book = (ledger or {}).get(team, {})
    out = {}
    for r in season_rows:
        name = r.get("player_name")
        if not name:
            continue
        key = match_key(name)
        if key not in team_book:
            continue
        status = effective_dfs_status(team_book[key]["entries"], int(week), r)
        if status is not None:
            out[key] = status
    return out


def seed_from_week(week, year=2026, base_dir="."):
    """Inputs: week (int), year, base_dir.
    Output: count of entries added.
    Purpose: one-time, opt-in import of gameday toggles that predate this
    ledger (they only exist in week_NN/{TEAM}.csv). Every dfs_status=="out"
    row becomes a sticky "out" at `week`. Skips *_live.csv sanity exports.
    Never overwrites an entry the ledger already holds for that week."""
    week_dir = os.path.join(base_dir, "data", "overrides", str(year), f"week_{week:02d}")
    ledger = load_ledger(year, base_dir)
    added = 0
    for fname in sorted(os.listdir(week_dir)):
        if not fname.endswith(".csv") or fname.endswith("_live.csv"):
            continue
        team = fname[:-4]
        _, rows = read_rows(os.path.join(week_dir, fname))
        for r in rows:
            if (r.get("dfs_status") or "").strip().lower() != DFS_OUT or not r.get("player_name"):
                continue
            existing = ledger.get(team, {}).get(match_key(r["player_name"]), {}).get("entries", [])
            if any(int(e["week"]) == week for e in existing):
                continue
            record_toggle(ledger, team, r["player_name"], week, DFS_OUT)
            print(f"  {team}: {r['player_name']} -> out from week {week}")
            added += 1
    save_ledger(ledger, year, base_dir)
    return added


if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1] == "seed" and sys.argv[2].isdigit():
        n = seed_from_week(int(sys.argv[2]))
        print(f"Seeded {n} sticky 'out' entries from week {sys.argv[2]}.")
    else:
        print(__doc__)
        sys.exit(1)
