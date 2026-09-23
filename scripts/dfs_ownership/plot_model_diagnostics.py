"""Feature importance + calibration diagnostics for the shipped ownership
models (classic total%, showdown FLEX%, showdown CPT%) -- generates the data
behind the diagnostics dashboard Cam asked to see 2026-09-22.

Calibration uses leave-one-slate-out OUT-OF-FOLD predictions (re-running the
same CV as train_ownership_model.py, not reading it back from that script's
console output), not the final model's in-sample fit -- an in-sample fit on
this little data would look artificially good and hide exactly the
overfitting risk Cam has been worried about all along. Feature importance
comes from the actual shipped model files on disk (data/dfs_ownership/
_processed/models/), so this always reflects whatever was last trained, not
a fresh, possibly-different fit.

Run:
    venv\\Scripts\\python.exe scripts/dfs_ownership/plot_model_diagnostics.py

Writes data/dfs_ownership/_processed/model_diagnostics.json, consumed by the
diagnostics dashboard artifact (not itself part of this repo -- a Claude
Artifact). Re-run any time after a retrain to refresh the dashboard's data.
"""
from __future__ import annotations

import json
import os
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from scripts.dfs_ownership.train_ownership_model import (  # noqa: E402
    ARCHIVE, MODEL_DIR, CLASSIC_FEATURES, SHOWDOWN_FEATURES, EXTRA_FEATURE, CATEGORICAL_COLS,
    build_dataset, _fit_one, _predict, _sigmoid_pct,
)

OUT_PATH = os.path.join(ARCHIVE, "_processed", "model_diagnostics.json")
N_BINS = 8  # fewer than the usual 10 -- classic only has ~3-4k OOF rows across 2 slates, showdown far less


def _slate_key(df: pd.DataFrame) -> pd.Series:
    return df["year"].astype(str) + "_wk" + df["week"].astype(int).astype(str).str.zfill(2) + "_" + df["slate_id"]


def _oof_predictions(df: pd.DataFrame, target_col: str, feature_cols: list[str]) -> pd.DataFrame:
    """Leave-one-slate-out out-of-fold predictions -- same fold logic as
    train_ownership_model.py's leave_one_slate_out_cv, but returns every
    row's (predicted, actual) instead of just aggregate MAE per fold."""
    d = df.dropna(subset=[target_col]).copy()
    d["_slate_key"] = _slate_key(d)
    slates = d["_slate_key"].unique()
    if len(slates) < 2:
        return pd.DataFrame(columns=["predicted", "actual", "slate", "pos"])
    rows = []
    for held_out in slates:
        train_df, test_df = d[d["_slate_key"] != held_out], d[d["_slate_key"] == held_out]
        if train_df.empty or test_df.empty:
            continue
        model, cols = _fit_one(train_df, target_col, feature_cols)
        preds = _predict(model, cols, test_df, feature_cols)
        for p, a, s, pos in zip(preds, test_df[target_col].values, test_df["_slate_key"], test_df["pos"]):
            rows.append({"predicted": round(float(p), 2), "actual": round(float(a), 2), "slate": s, "pos": pos})
    return pd.DataFrame(rows)


def _calibration_bins(oof: pd.DataFrame) -> list[dict]:
    """Equal-count bins by predicted value -- mean predicted vs mean actual
    per bin, the standard reliability-diagram shape, adapted for a
    continuous regression target instead of a classifier's probability."""
    if oof.empty:
        return []
    n_bins = min(N_BINS, max(2, len(oof) // 30))  # don't create bins with a handful of rows each
    oof = oof.copy()
    oof["_bin"] = pd.qcut(oof["predicted"], q=n_bins, duplicates="drop")
    out = []
    for interval, g in oof.groupby("_bin", observed=True):
        out.append({
            "bin_lo": round(float(interval.left), 2), "bin_hi": round(float(interval.right), 2),
            "mean_predicted": round(float(g["predicted"].mean()), 2),
            "mean_actual": round(float(g["actual"].mean()), 2),
            "n": int(len(g)),
        })
    return sorted(out, key=lambda r: r["mean_predicted"])


def _feature_importance(model_name: str) -> list[dict]:
    """Loads the shipped model + meta, pairs XGBoost's gain-based
    feature_importances_ with the post-dummy column names, and aggregates
    any one-hot dummy group (pos_QB, pos_RB, ... / stakes_tier_casual, ...)
    back to its base feature name so the chart reads as "position matters
    this much" rather than 20 separate tiny bars."""
    import xgboost as xgb

    model_path = os.path.join(MODEL_DIR, f"ownership_{model_name}_model.json")
    meta_path = os.path.join(MODEL_DIR, f"ownership_{model_name}_meta.json")
    if not (os.path.exists(model_path) and os.path.exists(meta_path)):
        return []
    model = xgb.XGBRegressor()
    model.load_model(model_path)
    meta = json.load(open(meta_path))
    cols = meta["feature_columns"]
    importances = model.feature_importances_

    agg: dict = {}
    for col, imp in zip(cols, importances):
        base = col
        for cat in CATEGORICAL_COLS:
            if col.startswith(cat + "_"):
                base = cat
                break
        agg[base] = agg.get(base, 0.0) + float(imp)
    total = sum(agg.values()) or 1.0
    return sorted(
        [{"feature": k, "importance_pct": round(100 * v / total, 2)} for k, v in agg.items()],
        key=lambda r: -r["importance_pct"],
    )


def main() -> None:
    df = build_dataset(2026, None)
    classic_features = CLASSIC_FEATURES + [EXTRA_FEATURE]
    showdown_features = SHOWDOWN_FEATURES + [EXTRA_FEATURE]

    out: dict = {}

    classic = df[df["slate_format"] == "classic"].copy()
    oof = _oof_predictions(classic, "total_own_pct", classic_features)
    out["classic"] = {
        "label": "Classic — Total Ownership %",
        "n_oof_rows": len(oof),
        # bare slate_id collides across weeks (same fix as leave_one_slate_out_cv) --
        # count real slates via the composite key, not this column directly.
        "n_slates": _slate_key(classic).nunique() if not classic.empty else 0,
        "feature_importance": _feature_importance("classic"),
        "calibration_bins": _calibration_bins(oof),
        "calibration_points": oof.to_dict("records"),
    }

    showdown = df[df["slate_format"] == "showdown"].copy()
    for target, name, label in [
        ("flex_own_pct", "showdown_flex", "Showdown — FLEX Ownership %"),
        ("cpt_own_pct", "showdown_cpt", "Showdown — CPT Ownership %"),
    ]:
        oof = _oof_predictions(showdown, target, showdown_features)
        out[name] = {
            "label": label,
            "n_oof_rows": len(oof),
            "n_slates": _slate_key(showdown).nunique() if not showdown.empty else 0,
            "feature_importance": _feature_importance(name),
            "calibration_bins": _calibration_bins(oof),
            "calibration_points": oof.to_dict("records"),
        }

    os.makedirs(os.path.dirname(OUT_PATH), exist_ok=True)
    with open(OUT_PATH, "w") as f:
        json.dump(out, f, indent=2)
    print(f"Wrote {OUT_PATH}")
    for k, v in out.items():
        print(f"  {k}: {v['n_oof_rows']} OOF rows across {v['n_slates']} slates, "
              f"top feature = {v['feature_importance'][0]['feature'] if v['feature_importance'] else 'n/a'}")


if __name__ == "__main__":
    main()
