"""Live in-season team PROE (pass rate over expected), from nflverse's own `pass_oe`.

Why: game_engine.py's play selection used each coach's 2015-24 PROE for the
whole season (proe_overlay.get_coach_proe was never given any current-season
data), so e.g. a new play-caller or a changed offense never moved. nflverse
already publishes the per-play expected pass probability (`xpass`) and
`pass_oe` (= 100 * (pass - xpass)), so nothing needs modelling here.

Output: data/dna/proe_live_{year}.json
    {"_metadata": {...}, "teams": {"DET": {"n_games": 4, "n_plays": 265, "pass_oe": 1.33}, ...}}
Consumed by src/nfl_sim/proe_overlay_v_0_1_0.get_coach_proe, which blends it with
the coach's historical PROE via the existing k=8 Bayesian shrinkage
(w_current = n_games / (n_games + 8)). Rerun weekly (also called from
refresh_weekly_dna_v_0_1_0.py).

Usage:
    venv\\Scripts\\python.exe scripts/roster_management/refresh_live_proe_v_0_1_0.py <year> <completed_week>
"""
import json
import os
import sys

import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

DNA_DIR = os.path.join("data", "dna")


def compute_live_proe(pbp, completed_week):
    """Inputs: pbp (DataFrame of nflverse play-by-play with play_type, week,
    posteam, game_id, pass_oe), completed_week (int, last finished week).
    Output: {team: {"n_games": int, "n_plays": int, "pass_oe": float}} -- mean
    pass_oe over that team's scrimmage pass/run plays through completed_week,
    in percentage points (same unit as coach_dna.json's "proe").
    Purpose: team-level (play-caller + roster) pass tendency vs the situation-
    expected rate. Plays without a pass_oe value (kneels, spikes, etc.) are
    skipped. Teams with no qualifying plays are omitted."""
    d = pbp[pbp["play_type"].isin(["pass", "run"]) & (pbp["week"] <= completed_week)]
    d = d[d["posteam"].notna() & d["pass_oe"].notna()]
    out = {}
    for team, g in d.groupby("posteam"):
        out[team] = {"n_games": int(g["game_id"].nunique()), "n_plays": int(len(g)),
                     "pass_oe": round(float(g["pass_oe"].mean()), 4)}
    return out


def write_live_proe(year, completed_week, pbp):
    """Writes data/dna/proe_live_{year}.json; returns the path."""
    teams = compute_live_proe(pbp, completed_week)
    path = os.path.join(DNA_DIR, f"proe_live_{year}.json")
    payload = {"_metadata": {"year": int(year), "completed_week": int(completed_week),
                             "source": "nflverse pass_oe (mean over pass/run plays)"},
               "teams": teams}
    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        json.dump(payload, f, indent=1, sort_keys=True)
    os.replace(tmp, path)
    return path


if __name__ == "__main__":
    import nfl_data_py as nfl
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(1)
    yr, wk = int(sys.argv[1]), int(sys.argv[2])
    df = nfl.import_pbp_data([yr], include_participation=False)
    p = write_live_proe(yr, wk, df)
    print(f"wrote {p}")
