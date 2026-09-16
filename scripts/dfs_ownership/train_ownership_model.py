"""Train the real ownership models to replace src/ownership/heuristic.py's
hand-tuned formula -- TWO independent models (classic main-slate, showdown),
not one shared model with a format flag: the two contest types have
genuinely different rosters, pricing (CPT premium vs none), and pools, and
Cam wants them kept structurally separate rather than pooled.

Known data limitation (explicit call from Cam, 2026-09-14): as of writing
there is 1 settled classic slate and 2-3 settled showdown slates archived --
short of the ~4-6/bucket data/dfs_ownership/README.md originally targeted.
Cam's call: train and ship on what exists now anyway ("results won't be good
for most of the year but it is better than what we were doing"), not gate on
a coverage minimum. This script ALWAYS trains + saves (no --force/--save
gate) but prints the coverage caveat prominently every run so nobody mistakes
an early model for a mature one -- re-run after each new settled slate is
archived; the model should visibly improve as coverage grows.

Feature set (Cam's explicit list + two flagged optional additions):
  salary, projection_median, projection_ceiling (both FROM OUR SIM -- p50/p95
  of the per-player simulated distribution, not a flat multiplier), Vegas
  (spread signed from the player's own team's perspective, game total, team
  implied total), position, and (classic only) how many games are on the
  slate.
  + cash_consensus_frac (classic: fraction of this week's ~10 cash-optimal
    lineups a player appears in) / optimal_cpt_pct+optimal_flex_pct
    (showdown's analogue -- already computed by the sim, no cash-lineup
    concept exists for single-game slates)
  + salary_dispersion_pos: coefficient of variation (std/mean) of salary
    within this player's position on this slate -- "tight" pricing (low CV,
    DK hasn't differentiated much) vs "loose" (high CV, clear tiers) changes
    how concentrated chalk gets.
  + salary_rank_pctile (Cam did not ask for this -- flagged, on by default,
    drop via --no-salary-rank): the player's salary rank across the WHOLE
    slate, not just their position. Added because calibrate_ownership_model.py
    found the heuristic crushes top-salary studs (QBs and otherwise) to the
    ownership floor in showdown FLEX -- it only ever looks at points-per-
    dollar, never "this guy is just a slate-wide stud, which is exactly what
    the real public chases regardless of value." This feature gives a model
    a direct shot at learning that effect instead of needing it implied
    through non-linear salary/projection interactions alone.

Run:
    venv\\Scripts\\python.exe scripts/dfs_ownership/train_ownership_model.py
    ... --year 2026 --week 1        # limit the walk
    ... --no-salary-rank            # drop the flagged extra feature
    ... --dry-run                   # print CV/coverage, don't save models
"""
from __future__ import annotations

import argparse
import json
import os
import sys
from datetime import datetime, timezone

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.ownership.heuristic import _compute_ownership, _compute_showdown_ownership  # noqa: E402
from src.scrapers.dk_scraper import normalize_player_name  # noqa: E402
from scripts.dfs_ownership._shared import (  # noqa: E402
    ARCHIVE, load_sim_projections, vegas_implied, vegas_spread,
    cash_consensus_frac, n_games_on_slate,
)
from scripts.dfs_ownership.calibrate_ownership_model import _stable_seed  # noqa: E402

MODEL_DIR = os.path.join(ARCHIVE, "_processed", "models")
TARGET_SLATES_PER_BUCKET = 4  # data/dfs_ownership/README.md's original (not-yet-met) target -- informational only now

CLASSIC_FEATURES = [
    "salary", "projection_median", "projection_ceiling", "game_total", "team_implied_total",
    "team_spread", "n_games_on_slate", "cash_consensus_frac", "salary_dispersion_pos",
]
SHOWDOWN_FEATURES = [
    "salary", "cpt_salary", "projection_median", "projection_ceiling", "game_total", "team_implied_total",
    "team_spread", "optimal_cpt_pct", "optimal_flex_pct", "salary_dispersion_pos",
]
EXTRA_FEATURE = "salary_rank_pctile"
CATEGORICAL_COLS = ["pos"]


def _logit(p: pd.Series) -> pd.Series:
    p = p.clip(0.1, 99.9) / 100.0
    return np.log(p / (1 - p))


def _sigmoid_pct(x: np.ndarray) -> np.ndarray:
    return 100.0 / (1.0 + np.exp(-x))


def _enrich(df: pd.DataFrame) -> pd.DataFrame:
    """Adds every derived v2 column used by either format: projection
    median/ceiling from the sim's own percentile array (not a flat
    heuristic multiplier), signed Vegas spread, the sim's optimal-CPT/FLEX%
    (showdown), cash-consensus fraction (classic), games-on-slate (classic),
    and the two dispersion/rank features computed within each slate."""
    df = df.copy()
    sim_cache: dict[int, dict] = {}
    vegas_i_cache: dict[int, dict] = {}
    vegas_s_cache: dict[int, dict] = {}
    cash_cache: dict[int, dict] = {}
    ngames_cache: dict[int, int] = {}

    proj_med, proj_ceil, team_spread, team_implied = [], [], [], []
    opt_cpt, opt_flex, cash_frac, ngames = [], [], [], []
    for _, r in df.iterrows():
        wk = int(r["week"])
        if wk not in sim_cache:
            sim_by_gid = load_sim_projections(wk)
            sim_cache[wk] = {k: v for g in sim_by_gid.values() for k, v in g.items()}
        if wk not in vegas_i_cache:
            vegas_i_cache[wk] = vegas_implied(wk)
        if wk not in vegas_s_cache:
            vegas_s_cache[wk] = vegas_spread(wk)
        if wk not in cash_cache:
            cash_cache[wk] = cash_consensus_frac(wk)
        if wk not in ngames_cache:
            ngames_cache[wk] = n_games_on_slate(wk)
        key = (r["_key"], r["team"])
        sim = sim_cache[wk].get(key, {})
        pcts = sim.get("dk_pcts_all")
        proj_med.append(pcts[50] if pcts else sim.get("dk_points"))
        proj_ceil.append(pcts[95] if pcts else sim.get("ceiling_dk_points"))
        team_implied.append(vegas_i_cache[wk].get(r["team"]))
        team_spread.append(vegas_s_cache[wk].get(r["team"]))
        opt_cpt.append(sim.get("optimal_cpt_pct"))
        opt_flex.append(sim.get("optimal_flex_pct"))
        cash_frac.append(cash_cache[wk].get(key, 0.0))
        ngames.append(ngames_cache[wk])
    df["projection_median"] = proj_med
    df["projection_ceiling"] = proj_ceil
    df["team_spread"] = team_spread
    df["team_implied_total"] = team_implied  # overwrite features.parquet's copy with this week's fresh value
    df["optimal_cpt_pct"] = opt_cpt
    df["optimal_flex_pct"] = opt_flex
    df["cash_consensus_frac"] = cash_frac
    df["n_games_on_slate"] = ngames

    # Slate-relative dispersion / rank -- computed per slate_id, not per row.
    df["salary_dispersion_pos"] = df.groupby(["slate_id", "pos"])["salary"].transform(
        lambda s: s.std() / s.mean() if s.mean() else 0.0).fillna(0.0)
    df["salary_rank_pctile"] = df.groupby("slate_id")["salary"].rank(pct=True)
    return df


def build_dataset(year: int, week: int | None) -> pd.DataFrame:
    feat = pd.read_parquet(os.path.join(ARCHIVE, "_processed", "features.parquet"))
    own = pd.read_parquet(os.path.join(ARCHIVE, "_processed", "ownership_actuals.parquet"))
    if year:
        feat, own = feat[feat["year"] == year], own[own["year"] == year]
    if week:
        feat, own = feat[feat["week"] == week], own[own["week"] == week]

    keys = ["year", "week", "slate_id", "contest_name", "player", "team", "pos"]
    df = feat.merge(
        own[keys + ["slate_format", "flex_own_pct", "cpt_own_pct", "total_own_pct"]],
        on=keys, how="inner",
    )
    df["_key"] = df["player"].map(normalize_player_name)
    df = _enrich(df)
    return df.dropna(subset=["projection_median"])


def coverage_report(df: pd.DataFrame, slate_format: str) -> None:
    sub = df[df["slate_format"] == slate_format]
    cov = sub.groupby("field_bucket")["slate_id"].nunique().reindex(["large", "mid", "small"]).fillna(0).astype(int)
    n_slates = sub["slate_id"].nunique()
    print(f"  Coverage: {n_slates} distinct {slate_format} slate(s) total "
          f"(README's original target was ~{TARGET_SLATES_PER_BUCKET}-6 PER bucket -- informational, not a gate):")
    print("  " + cov.to_string().replace("\n", "\n  "))


def _fit_one(train_df: pd.DataFrame, target_col: str, feature_cols: list[str]):
    import xgboost as xgb

    X = pd.get_dummies(train_df[feature_cols + CATEGORICAL_COLS], columns=CATEGORICAL_COLS)
    y = _logit(train_df[target_col])
    model = xgb.XGBRegressor(
        n_estimators=100, max_depth=3, learning_rate=0.1,
        subsample=0.9, colsample_bytree=0.9, reg_lambda=1.0,
    )
    model.fit(X, y)
    return model, list(X.columns)


def _predict(model, columns: list[str], df: pd.DataFrame, feature_cols: list[str]) -> np.ndarray:
    X = pd.get_dummies(df[feature_cols + CATEGORICAL_COLS], columns=CATEGORICAL_COLS)
    X = X.reindex(columns=columns, fill_value=0)
    return _sigmoid_pct(model.predict(X))


def _heuristic_baseline(df: pd.DataFrame, is_showdown: bool) -> pd.Series:
    """Dedupe to one row per (slate, player) before scoring -- df has one row
    per (contest, player), and the heuristic scores a slate's distinct pool
    once, not once per contest (see the identical fix in
    calibrate_ownership_model.py's _heuristic_baseline -- running it on
    duplicated rows inflates the softmax normalization and silently shrinks
    every prediction)."""
    preds = pd.Series(index=df.index, dtype=float)
    for slate_id, idx in df.groupby("slate_id").groups.items():
        sub = df.loc[idx]
        distinct = sub.drop_duplicates(subset=["player", "team"])
        pool = distinct.rename(columns={"projection_median": "projection"}).to_dict("records")
        if is_showdown:
            _compute_showdown_ownership(pool, seed=_stable_seed(str(slate_id)))
        else:
            _compute_ownership(pool, seed=_stable_seed(str(slate_id)))
        pred_by_player = {(p["player"], p["team"]): p["ownership_pct"] for p in pool}
        preds.loc[idx] = [pred_by_player.get((r["player"], r["team"])) for _, r in sub.iterrows()]
    return preds


def leave_one_slate_out_cv(df: pd.DataFrame, target_col: str, feature_cols: list[str],
                            label: str, is_showdown: bool) -> None:
    d = df.dropna(subset=[target_col])
    slates = d["slate_id"].unique()
    if len(slates) < 2:
        print(f"  {label}: only {len(slates)} slate(s) -- can't hold one out and still train on the rest. Skipped.")
        return
    heuristic_pred = _heuristic_baseline(d, is_showdown)
    rows = []
    for held_out in slates:
        train_df, test_df = d[d["slate_id"] != held_out], d[d["slate_id"] == held_out]
        if train_df.empty or test_df.empty:
            continue
        model, cols = _fit_one(train_df, target_col, feature_cols)
        model_pred = _predict(model, cols, test_df, feature_cols)
        model_mae = float(np.abs(model_pred - test_df[target_col].values).mean())
        heur_mae = float(np.abs(heuristic_pred.loc[test_df.index].values - test_df[target_col].values).mean())
        rows.append({"held_out_slate": held_out, "n": len(test_df), "model_mae": model_mae, "heuristic_mae": heur_mae})
    if not rows:
        print(f"  {label}: no valid CV folds")
        return
    cv = pd.DataFrame(rows)
    wins = int((cv["model_mae"] < cv["heuristic_mae"]).sum())
    print(f"\n  {label} -- leave-one-slate-out CV ({len(cv)} folds):")
    print("  " + cv.round(1).to_string(index=False).replace("\n", "\n  "))
    print(f"  Model beat the heuristic on {wins}/{len(cv)} held-out slates "
          f"(mean model MAE {cv['model_mae'].mean():.1f}pp vs heuristic {cv['heuristic_mae'].mean():.1f}pp)")


def train_classic(df: pd.DataFrame, feature_cols: list[str], dry_run: bool) -> None:
    print("\n" + "=" * 60)
    print("CLASSIC MAIN-SLATE MODEL (total ownership)")
    print("=" * 60)
    sub = df[df["slate_format"] == "classic"].copy()
    if sub.empty:
        print("  No classic rows -- skipped.")
        return
    coverage_report(df, "classic")
    leave_one_slate_out_cv(sub, "total_own_pct", feature_cols, "Total ownership", is_showdown=False)
    if dry_run:
        return
    model, cols = _fit_one(sub.dropna(subset=["total_own_pct"]), "total_own_pct", feature_cols)
    os.makedirs(MODEL_DIR, exist_ok=True)
    model.save_model(os.path.join(MODEL_DIR, "ownership_classic_model.json"))
    _save_meta("ownership_classic_meta.json", sub, cols, feature_cols, {"target": "total_own_pct"})
    print(f"  Saved -> {os.path.join(MODEL_DIR, 'ownership_classic_model.json')}")


def train_showdown(df: pd.DataFrame, feature_cols: list[str], dry_run: bool) -> None:
    print("\n" + "=" * 60)
    print("SHOWDOWN MODEL (FLEX + CPT ownership, separate targets)")
    print("=" * 60)
    sub = df[df["slate_format"] == "showdown"].copy()
    if sub.empty:
        print("  No showdown rows -- skipped.")
        return
    coverage_report(df, "showdown")

    leave_one_slate_out_cv(sub, "flex_own_pct", feature_cols, "FLEX ownership", is_showdown=True)
    leave_one_slate_out_cv(sub, "cpt_own_pct", feature_cols, "CPT ownership", is_showdown=True)
    if dry_run:
        return

    os.makedirs(MODEL_DIR, exist_ok=True)
    flex_df = sub.dropna(subset=["flex_own_pct"])
    flex_model, flex_cols = _fit_one(flex_df, "flex_own_pct", feature_cols)
    flex_model.save_model(os.path.join(MODEL_DIR, "ownership_showdown_flex_model.json"))
    _save_meta("ownership_showdown_flex_meta.json", flex_df, flex_cols, feature_cols, {"target": "flex_own_pct"})

    cpt_df = sub.dropna(subset=["cpt_own_pct"])
    cpt_model, cpt_cols = _fit_one(cpt_df, "cpt_own_pct", feature_cols)
    cpt_model.save_model(os.path.join(MODEL_DIR, "ownership_showdown_cpt_model.json"))
    _save_meta("ownership_showdown_cpt_meta.json", cpt_df, cpt_cols, feature_cols, {"target": "cpt_own_pct"})
    print(f"  Saved -> {MODEL_DIR}/ownership_showdown_{{flex,cpt}}_model.json")


def _save_meta(filename: str, df: pd.DataFrame, feature_columns: list[str],
                raw_feature_cols: list[str], extra: dict) -> None:
    meta = {
        "trained_at": datetime.now(timezone.utc).isoformat(),
        "n_rows": int(len(df)), "n_slates": int(df["slate_id"].nunique()),
        "slate_ids": sorted(df["slate_id"].unique().tolist()),
        "feature_columns": feature_columns,  # post-dummy columns the model actually expects, in order
        "raw_feature_columns": raw_feature_cols,  # pre-dummy fields (+ categorical "pos") live inference must supply
        "categorical_columns": CATEGORICAL_COLS,
        "data_maturity_warning": "Trained on very limited data (see README's ~4-6-slates-per-bucket target). "
                                  "Re-train as more slates are archived.",
        **extra,
    }
    with open(os.path.join(MODEL_DIR, filename), "w") as f:
        json.dump(meta, f, indent=2)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--year", type=int, default=2026)
    ap.add_argument("--week", type=int, default=None, help="limit to one week")
    ap.add_argument("--no-salary-rank", action="store_true", help="drop the flagged salary_rank_pctile feature")
    ap.add_argument("--dry-run", action="store_true", help="print CV/coverage only, don't save models")
    args = ap.parse_args()

    df = build_dataset(args.year, args.week)
    if df.empty:
        print("No joined feature/actuals rows -- nothing to train on.")
        return

    classic_features = list(CLASSIC_FEATURES)
    showdown_features = list(SHOWDOWN_FEATURES)
    if not args.no_salary_rank:
        classic_features.append(EXTRA_FEATURE)
        showdown_features.append(EXTRA_FEATURE)

    print("NOTE: data is thin right now (see coverage below) -- shipping anyway per explicit "
          "instruction; treat these models as a starting point to re-train weekly, not a finished product.")
    train_classic(df, classic_features, args.dry_run)
    train_showdown(df, showdown_features, args.dry_run)
    if args.dry_run:
        print("\nDry run -- nothing saved. Drop --dry-run to persist the models.")


if __name__ == "__main__":
    main()
