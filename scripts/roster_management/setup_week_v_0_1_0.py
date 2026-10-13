"""Set up a DFS week end to end: refresh DNA -> build sheets -> compile rosters -> sim -> verify site.

Inputs : argv week <W> (the week being PREPARED, e.g. 6). The DNA refresh uses <W-1>.
         Flags: --skip-refresh (DNA already refreshed), --skip-sim (stop after compile),
                --api http://localhost:8002 (DFS API to verify against).
Outputs: everything the individual steps write (see RUNBOOK.md A1, C1, C2, C3), plus a
         PASS/FAIL line from the final check that the DFS API now lists week <W>.
Purpose: one command for the Tuesday "put week W on the site" chain, so the Ops Desk and
         RUNBOOK point at a single step. Stops at the first failing step (non-zero exit).
         /api/weeks derives its list from data/interim/dfs_week_N_*.parquet, so a finished
         sim IS what publishes the week -- there is no code edit or restart step.
Usage  : venv\\Scripts\\python.exe scripts/roster_management/setup_week_v_0_1_0.py 6
Not covered (slow / judgment calls, see RUNBOOK): A2/A3 season reports, ownership retrain
(B), injury toggles (D1).
"""
import argparse
import json
import subprocess
import sys
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PY = sys.executable


def run_step(label: str, args: list) -> None:
    """Inputs: label (str, for the log), args (script path + its argv, list[str]).
    Outputs: none; exits the whole process with the step's code if it fails.
    Purpose: run one pipeline script from the repo root with the current interpreter."""
    print(f"\n=== {label} ===\n  {' '.join(args)}", flush=True)
    rc = subprocess.call([PY, *args], cwd=ROOT)
    if rc != 0:
        print(f"FAIL: {label} exited {rc}; stopping.")
        sys.exit(rc)


def verify(week: int, api: str) -> bool:
    """Inputs: week (int), api (base URL of the DFS API).
    Outputs: bool, True when the sim files exist and (if the API is up) it lists the week.
    Purpose: final check that the site can see the new week."""
    interim = ROOT / "data" / "interim"
    files_ok = all((interim / f"dfs_week_{week}_{k}.parquet").exists() for k in ("games", "players"))
    print(f"\nsim parquets for week {week}: {'present' if files_ok else 'MISSING'}")
    try:
        with urllib.request.urlopen(f"{api}/api/weeks", timeout=10) as r:
            weeks = json.load(r).get("weeks", [])
        print(f"{api}/api/weeks -> {weeks}")
        return files_ok and week in weeks
    except Exception as e:  # API not running is not a pipeline failure
        print(f"DFS API not reachable ({e}); start it with start_backend_api.bat, then reload the site.")
        return files_ok


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("week", type=int, help="week being prepared")
    ap.add_argument("--skip-refresh", action="store_true")
    ap.add_argument("--skip-sim", action="store_true")
    ap.add_argument("--api", default="http://localhost:8002")
    a = ap.parse_args()
    w, w1 = a.week, a.week - 1
    rm = "scripts/roster_management"
    if not a.skip_refresh:
        run_step(f"A1 refresh DNA through week {w1}", [f"{rm}/refresh_weekly_dna_v_0_1_0.py", "2026", str(w1)])
    run_step(f"C1 build week {w} override sheets", [f"{rm}/build_week_overrides_v_0_1_0.py", str(w)])
    run_step(f"C2 compile week {w} DFS rosters", [f"{rm}/apply_team_week_overrides_v_0_1_0.py", str(w)])
    if not a.skip_sim:
        run_step(f"C3 run week {w} sim", ["scripts/simulation_runners/run_week_sim_2026.py", str(w)])
    ok = verify(w, a.api)
    print("PASS" if ok else "FAIL (or sim skipped)")
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
