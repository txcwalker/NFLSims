"""One-time backfill of data/eval/{year}/season_rankings_snapshots.csv from
the git history of docs/reports/season_leaders_{year}.json (2026-09-26).

Why: the season-rankings ledger (src/evaluation/season_rankings.py) only
started recording on 2026-09-26, but git holds earlier versions of the
leaders JSON -- including the last PRESEASON one (committed 2026-09-09,
before the Week 1 opener). Each is stamped with its COMMIT time (an upper
bound on when it was generated). These JSON copies only carry each
position's top 32 (the file's own slice); live snapshots store full tables.
current_week is left blank (unknown for git copies).

Safe to rerun: append_snapshot() skips a snapshot identical to the latest,
and snapshots are replayed oldest-first.

Usage:
    venv\\Scripts\\python.exe scripts/evaluation/seed_season_rankings.py [year]
"""
import json
import os
import subprocess
import sys

sys.path.insert(0, os.getcwd())
from src.evaluation.season_rankings import snapshot_frame, append_snapshot  # noqa: E402


def records(doc):
    """All 'overall' leader records from a leaders JSON document."""
    return [r for pos_rows in doc.get("overall", {}).values() for r in pos_rows]


def main(year):
    rel = f"docs/reports/season_leaders_{year}.json"
    log = subprocess.run(["git", "log", "--format=%H %ct", "--", rel],
                         capture_output=True, text=True, check=True).stdout.split()
    for sha, ts in list(zip(log[0::2], log[1::2]))[::-1]:
        doc = json.loads(subprocess.run(["git", "show", f"{sha}:{rel}"], capture_output=True,
                                        text=True, check=True, encoding="utf-8").stdout)
        n = append_snapshot(snapshot_frame(records(doc), float(ts), f"git:{sha[:7]}"), year)
        print(f"git {sha[:7]} @ {ts}: +{n} rows")
    if os.path.exists(rel):
        doc = json.load(open(rel, encoding="utf-8"))
        n = append_snapshot(snapshot_frame(records(doc), os.path.getmtime(rel), "working-copy"), year)
        print(f"working copy: +{n} rows")


if __name__ == "__main__":
    main(int(sys.argv[1]) if len(sys.argv) > 1 else 2026)
