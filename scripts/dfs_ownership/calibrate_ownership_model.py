"""Backtest the current V1 ownership heuristic (src/ownership/heuristic.py)
against real archived ownership -- "if we'd used today's model on this
already-settled slate, how far off would it have been, and in which
direction?" Answers this BEFORE a trained model exists: it's evaluating the
existing hand-tuned formula, not a GBM.

For every slate under data/dfs_ownership/<year>/week_NN/ that has both a
salaries_prelock.csv (pre-lock pricing) and rows in
_processed/ownership_actuals.parquet (settled contest(s)), this:

  1. Rebuilds the exact player-pool inputs the heuristic would have seen at
     lock time (salary from the snapshot, projection/optimal-CPT-FLEX% from
     that week's cached sim results, Vegas implied team total from the
     schedule/manifest) -- the SAME inputs available pre-lock, not anything
     that leaked from the result.
  2. Runs _compute_ownership (classic) or _compute_showdown_ownership
     (showdown) to get what the model would have predicted.
  3. Joins against the real actual ownership (averaged across that slate's
     settled contests) and reports signed error (bias) and absolute error
     (MAE), overall and by position -- plus the single worst individual
     misses, since a few big chalk/contrarian surprises usually matter more
     than the aggregate.

Run:
    venv\\Scripts\\python.exe scripts/dfs_ownership/calibrate_ownership_model.py
    ... --year 2026 --week 1        # limit the walk
    ... --out report.csv            # also write the per-player comparison rows

Writes nothing by default (pure report) unless --out is given, and never
touches ownership_actuals.parquet / features.parquet.
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import sys
import zlib

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.ownership.heuristic import _compute_ownership, _compute_showdown_ownership  # noqa: E402
from src.scrapers.dk_scraper import normalize_player_name  # noqa: E402
from scripts.dfs_ownership._shared import (  # noqa: E402
    ARCHIVE, load_sim_projections as _load_sim_projections_by_week,
    vegas_implied as _vegas_implied_by_week, slate_folders, load_manifest,
)


def _stable_seed(s: str) -> int:
    """Deterministic seed from a string -- Python's builtin hash() is
    randomized per-process (PYTHONHASHSEED), so it can't be used here despite
    looking deterministic; crc32 always is."""
    return zlib.crc32(s.encode())


def _load_sim_projections(year: int, week: int) -> dict:
    return _load_sim_projections_by_week(week)


def _vegas_implied(year: int, week: int) -> dict:
    return _vegas_implied_by_week(week)


def _build_pool_classic(sal_df: pd.DataFrame, sim_by_key: dict, vegas: dict) -> list:
    pool = []
    for _, r in sal_df.iterrows():
        name, team = r["name"], r["team"]
        sim = sim_by_key.get((normalize_player_name(name), team), {})
        pos = str(r.get("pos") or sim.get("pos") or "").upper() or None
        if pos is None or r.get("salary") is None or pd.isna(r.get("salary")) or sim.get("dk_points") is None:
            continue
        pool.append({
            "name": sim.get("name", name), "team": team, "pos": pos,
            "salary": float(r["salary"]),
            "projection": sim.get("dk_points"),
            "implied_total": vegas.get(team),
        })
    return pool


def _build_pool_showdown(sal_df: pd.DataFrame, sim_by_key: dict, vegas: dict) -> list:
    pool = []
    for _, r in sal_df.iterrows():
        name, team = r["name"], r["team"]
        sim = sim_by_key.get((normalize_player_name(name), team), {})
        pos = str(r.get("pos") or sim.get("pos") or "").upper() or None
        if pos is None or r.get("salary") is None or pd.isna(r.get("salary")) or sim.get("dk_points") is None:
            continue
        pool.append({
            "name": sim.get("name", name), "team": team, "pos": pos,
            "salary": float(r["salary"]),
            "projection": sim.get("dk_points"),
            "optimal_cpt_pct": sim.get("optimal_cpt_pct"),
            "optimal_flex_pct": sim.get("optimal_flex_pct"),
            "implied_total": vegas.get(team),
        })
    return pool


def _error_report(rows: pd.DataFrame, pred_col: str, actual_col: str, label: str) -> None:
    d = rows.dropna(subset=[pred_col, actual_col])
    if d.empty:
        print(f"  {label}: no comparable rows")
        return
    err = d[pred_col] - d[actual_col]
    print(f"  {label} (n={len(d)}): MAE={err.abs().mean():.1f}pp  bias={err.mean():+.1f}pp "
          f"(+ = over-predicts)  corr={d[pred_col].corr(d[actual_col]):.2f}")
    by_pos = d.assign(_err=err).groupby("pos")["_err"].agg(["mean", lambda s: s.abs().mean(), "count"])
    by_pos.columns = ["bias", "mae", "n"]
    print(by_pos.round(1).to_string())


def calibrate(year: int, week: int | None, out: str | None) -> None:
    actuals = pd.read_parquet(os.path.join(ARCHIVE, "_processed", "ownership_actuals.parquet"))
    week_glob = f"week_{week:02d}" if week else "week_*"
    folders = sorted(glob.glob(os.path.join(ARCHIVE, str(year), week_glob, "*")))
    folders = [f for f in folders if os.path.isdir(f) and os.path.basename(f) != "_processed"]

    all_rows = []
    for folder in folders:
        slate_id = os.path.basename(folder)
        sal_path = os.path.join(folder, "salaries_prelock.csv")
        if not os.path.exists(sal_path):
            continue
        man_path = os.path.join(folder, "manifest.json")
        if not os.path.exists(man_path):
            continue
        manifest = json.load(open(man_path))
        wk = manifest.get("week", week)
        slate_actuals = actuals[(actuals["year"] == year) & (actuals["slate_id"] == slate_id)]
        if slate_actuals.empty:
            print(f"{slate_id}: no settled contests in ownership_actuals.parquet yet -- skipped")
            continue

        sim_by_gid = _load_sim_projections(year, wk)
        vegas = _vegas_implied(year, wk)
        sal_df = pd.read_csv(sal_path)
        fmt = manifest.get("slate_format", "classic")

        if fmt == "showdown":
            gid = f"{year}_{wk:02d}_{manifest['away_team']}_{manifest['home_team']}"
            sim_by_key = sim_by_gid.get(gid, {})
            pool = _build_pool_showdown(sal_df, sim_by_key, vegas)
            _compute_showdown_ownership(pool, seed=_stable_seed(slate_id))
            pred = pd.DataFrame(pool)[["name", "team", "pos", "ownership_pct", "cpt_ownership_pct"]]
            pred["_key"] = pred["name"].map(normalize_player_name)
            actual = slate_actuals.groupby(["player", "team"], as_index=False)[
                ["flex_own_pct", "cpt_own_pct"]].mean()
            actual["_key"] = actual["player"].map(normalize_player_name)
            merged = pred.merge(actual, on=["_key", "team"], how="inner")
            print(f"\n{slate_id} ({fmt}, {len(merged)}/{len(pred)} players matched to actuals):")
            _error_report(merged, "ownership_pct", "flex_own_pct", "FLEX")
            _error_report(merged, "cpt_ownership_pct", "cpt_own_pct", "CPT")
            merged["slate_id"] = slate_id
            all_rows.append(merged)
        else:
            sim_by_key = {}
            for g in sim_by_gid.values():
                sim_by_key.update(g)
            pool = _build_pool_classic(sal_df, sim_by_key, vegas)
            _compute_ownership(pool, seed=_stable_seed(slate_id))
            pred = pd.DataFrame(pool)[["name", "team", "pos", "ownership_pct"]]
            pred["_key"] = pred["name"].map(normalize_player_name)
            actual = slate_actuals.groupby(["player", "team"], as_index=False)[["total_own_pct"]].mean()
            actual["_key"] = actual["player"].map(normalize_player_name)
            merged = pred.merge(actual, on=["_key", "team"], how="inner")
            print(f"\n{slate_id} ({fmt}, {len(merged)}/{len(pred)} players matched to actuals):")
            _error_report(merged, "ownership_pct", "total_own_pct", "Total")
            merged["slate_id"] = slate_id
            all_rows.append(merged)

    if not all_rows:
        print("\nNothing to calibrate -- no slate has both a salary snapshot and settled actuals.")
        return

    combined = pd.concat(all_rows, ignore_index=True)
    # Worst individual misses across everything -- usually more actionable
    # than the aggregate stats for spotting a systematic blind spot.
    combined["_err"] = combined.get("ownership_pct", np.nan) - combined.get(
        "flex_own_pct", combined.get("total_own_pct"))
    worst = combined.reindex(combined["_err"].abs().sort_values(ascending=False).index).head(10)
    print("\n=== 10 largest individual misses (predicted vs real) ===")
    cols = [c for c in ["slate_id", "name", "pos", "ownership_pct", "flex_own_pct", "total_own_pct", "_err"]
            if c in worst.columns]
    print(worst[cols].round(1).to_string(index=False))

    if out:
        combined.to_csv(out, index=False)
        print(f"\nWrote per-player comparison rows -> {out}")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--year", type=int, default=2026)
    ap.add_argument("--week", type=int, default=None, help="limit to one week")
    ap.add_argument("--out", help="also write per-player comparison rows to this CSV")
    args = ap.parse_args()
    calibrate(args.year, args.week, args.out)


if __name__ == "__main__":
    main()
