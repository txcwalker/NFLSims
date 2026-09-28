"""Append-only ledger of our SEASON-LONG positional rankings (2026-09-26).

Why: docs/reports/season_leaders_{year}.json (the full-season sim's
projected season totals -- our de facto draft / rest-of-season rankings) is
overwritten every time generate_season_leaders_2026.py runs, so the
preseason version was already gone by Week 3 (recovered from git for 2026).
Same problem the game sims had before the kickoff lock. Every regeneration
now also appends a timestamped snapshot here, so later we can grade
"preseason rankings vs. final finish" and "rest-of-season rankings vs.
rest-of-season finish". Nothing is graded from this yet -- 3 weeks is too
early for season-long.

File (COMMIT it -- not regeneratable): data/eval/{year}/season_rankings_snapshots.csv
  one row per player per snapshot: snapshot_at (unix s), source ('live' |
  'git:<sha>'), current_week (weeks < this were already REAL in the totals --
  the leaders script runs in additive mode after Week 1), Player, Team, Pos,
  pts_{fmt} and rank_{fmt} for every rankings_eval.FORMATS format.
Season points use the same season-long formula as the weekly rankings.
"""
import os

import pandas as pd

from src.evaluation.line_history import BASE_DIR
from src.evaluation.rankings_eval import FORMATS, GRADED_POS, season_score


def snapshots_path(year, base_dir=BASE_DIR):
    """Inputs: year, base_dir. Output: str path of the snapshot ledger CSV."""
    return os.path.join(base_dir, "data", "eval", str(year), "season_rankings_snapshots.csv")


def snapshot_frame(leaders, snapshot_at, source, current_week=None):
    """Turn a leaders table into ranked snapshot rows.

    Inputs:
      leaders      -- DataFrame (or list of records) with Player, Team, Pos and
                      season-total *_avg stat columns (pYds_avg, rec_avg, ...),
                      i.e. generate_season_leaders' tables or the JSON's records
      snapshot_at  -- unix seconds; source -- provenance label
      current_week -- the leaders script's current week (None if unknown)
    Output: DataFrame of LEDGER columns, ranked within Pos per format.
    """
    df = pd.DataFrame(leaders).copy()
    df = df[df["Pos"].isin(GRADED_POS)]
    stats = df.rename(columns={c: c[:-4] for c in df.columns if c.endswith("_avg")})
    out = df[["Player", "Team", "Pos"]].copy()
    for fmt in FORMATS:
        out[f"pts_{fmt}"] = season_score(stats, fmt).round(2).to_numpy()
        out[f"rank_{fmt}"] = out.groupby("Pos")[f"pts_{fmt}"].rank(ascending=False, method="min").astype(int)
    out.insert(0, "current_week", current_week)
    out.insert(0, "source", source)
    out.insert(0, "snapshot_at", float(snapshot_at))
    return out.reset_index(drop=True)


def load_snapshots(year, base_dir=BASE_DIR):
    """Output: the ledger DataFrame (empty if none yet)."""
    p = snapshots_path(year, base_dir)
    return pd.read_csv(p) if os.path.exists(p) else pd.DataFrame()


def append_snapshot(snap, year, base_dir=BASE_DIR):
    """Append one snapshot unless (a) a snapshot with the same snapshot_at +
    source is already stored (replaying history, e.g. rerunning the git seed),
    or (b) it's identical (same players + points) to the latest stored one
    (a regeneration that changed nothing). Output: rows appended (0 if skipped).
    Written atomically (tmp + replace)."""
    led = load_snapshots(year, base_dir)
    key_cols = ["Player", "Team", "Pos"] + [f"pts_{f}" for f in FORMATS]
    if len(led):
        at, src = float(snap["snapshot_at"].iloc[0]), snap["source"].iloc[0]
        if ((led["snapshot_at"] == at) & (led["source"] == src)).any():
            return 0
        last = led[led["snapshot_at"] == led["snapshot_at"].max()]
        a = last[key_cols].sort_values(key_cols[:3]).reset_index(drop=True)
        b = snap[key_cols].sort_values(key_cols[:3]).reset_index(drop=True)
        if a.shape == b.shape and a.round(2).equals(b.round(2)):
            return 0
    out = pd.concat([led, snap], ignore_index=True) if len(led) else snap
    out = out.sort_values(["snapshot_at", "Pos", f"rank_{next(iter(FORMATS))}"]).reset_index(drop=True)
    p = snapshots_path(year, base_dir)
    os.makedirs(os.path.dirname(p), exist_ok=True)
    out.to_csv(p + ".tmp", index=False)
    os.replace(p + ".tmp", p)
    return len(snap)


def snapshot_summary(year, first_kickoff=None, base_dir=BASE_DIR):
    """Small status block for the UI. Inputs: year, first_kickoff (unix s of the
    season's first REG kickoff, to identify the preseason snapshot).
    Output: {count, latest_at, preseason_at, players_latest} (None-filled if empty)."""
    led = load_snapshots(year, base_dir)
    if led.empty:
        return {"count": 0, "latest_at": None, "preseason_at": None, "players_latest": 0}
    times = sorted(led["snapshot_at"].unique())
    pre = [t for t in times if first_kickoff is not None and t < first_kickoff]
    return {"count": len(times), "latest_at": float(times[-1]), "preseason_at": float(pre[-1]) if pre else None,
            "players_latest": int((led["snapshot_at"] == times[-1]).sum())}
