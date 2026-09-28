"""One-time backfill of data/eval/{year}/line_history.csv from the schedule
file's git history + the current working copy (2026-09-25).

Why: the line ledger (src/evaluation/line_history.py) only started recording
on 2026-09-25, but git already holds older snapshots of
data/external/schedule_{year}.csv -- the only record of Weeks 1-3's earlier
(opening-ish) lines. Each snapshot is stamped with its COMMIT time, which is
an upper bound on when those lines were pulled (conservative for the
"captured before kickoff" test that decides the opener).

Also creates data/eval/{year}/line_overrides.csv (header + any seed rows) if
it doesn't exist -- the hand-edited file for true open/close numbers from
outside nflverse.

Safe to rerun: append_snapshot() dedupes on change, so replaying the same
snapshots adds nothing.

Usage:
    venv\\Scripts\\python.exe scripts/evaluation/seed_line_history.py [year]
"""
import io
import os
import subprocess
import sys

import pandas as pd

sys.path.insert(0, os.getcwd())
from src.evaluation.line_history import (append_snapshot, overrides_path,  # noqa: E402
                                         OVERRIDE_COLS)

# True opener/closer numbers Cam reported (book lines, not nflverse's).
# spread_line is home-favored-positive (GB -7.5 -> 7.5).
SEED_OVERRIDES = [
    {"game_id": "2026_03_ATL_GB", "tag": "open", "spread_line": 7.5, "total_line": 46.5,
     "note": "per Cam 2026-09-25"},
    {"game_id": "2026_03_ATL_GB", "tag": "close", "spread_line": 4.5, "total_line": 43.5,
     "note": "per Cam 2026-09-25"},
]


def git_snapshots(rel_path):
    """Inputs: rel_path (repo-relative file). Output: list of (commit_unix_ts,
    DataFrame) oldest-first for every commit that touched the file."""
    log = subprocess.run(["git", "log", "--format=%H %ct", "--", rel_path],
                         capture_output=True, text=True, check=True).stdout.split()
    pairs = list(zip(log[0::2], log[1::2]))[::-1]            # oldest first
    out = []
    for sha, ts in pairs:
        blob = subprocess.run(["git", "show", f"{sha}:{rel_path}"],
                              capture_output=True, text=True, check=True).stdout
        out.append((float(ts), pd.read_csv(io.StringIO(blob))))
    return out


def main(year):
    rel = f"data/external/schedule_{year}.csv"
    for ts, df in git_snapshots(rel):
        n = append_snapshot(df, year, source="nflverse_git", captured_at=ts)
        print(f"git snapshot @ {pd.Timestamp(ts, unit='s', tz='UTC')}: +{n} rows")
    n = append_snapshot(pd.read_csv(rel), year, source="nflverse", captured_at=os.path.getmtime(rel))
    print(f"working copy @ {pd.Timestamp(os.path.getmtime(rel), unit='s', tz='UTC')}: +{n} rows")

    op = overrides_path(year)
    if not os.path.exists(op):
        os.makedirs(os.path.dirname(op), exist_ok=True)
        with open(op, "w", encoding="utf-8", newline="") as f:
            f.write("# Hand-edited true opening/closing lines (win over line_history.csv).\n"
                    "# tag = open | close. spread_line > 0 = HOME favored (GB -4.5 at home = 4.5).\n"
                    "# Leave spread_line or total_line blank to override only the other one.\n")
            pd.DataFrame(SEED_OVERRIDES, columns=OVERRIDE_COLS).to_csv(f, index=False)
        print(f"Created {op} with {len(SEED_OVERRIDES)} seed rows")


if __name__ == "__main__":
    main(int(sys.argv[1]) if len(sys.argv) > 1 else 2026)
