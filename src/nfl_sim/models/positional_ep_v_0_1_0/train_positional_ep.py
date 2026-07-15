"""
train_positional_ep.py
======================
# Status: live | v0.1.0 | 2026-06-21

Trains the Expected Points (EP) regression model used by the chess-style
positional evaluator. EP is a purely situational metric: the expected point
value of a drive starting at a given (down, distance, yardline) state,
independent of score, clock, or roster.

Features (4):
    yardline_100  -- yards to the end zone (1 = goal line, 99 = own 1-yd line)
    down          -- 1, 2, 3, or 4
    ydstogo       -- yards needed for a first down
    goal_to_go    -- 1 if ydstogo == yards_to_goal, else 0

Target:
    ep            -- expected points (pre-computed in nflfastR PBP data)

Serializes:
    positional_ep_model.json  -- XGBoost booster in native JSON format
    metadata.json             -- features, target, training params, val metrics

Run from repo root:
    python src/nfl_sim/models/positional_ep_v_0_1_0/train_positional_ep.py
"""

import os
import json
import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, mean_absolute_error

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
DATA_PATH = "data/processed/hardened_pass_training_master_v2_5.csv"
MODEL_DIR = "src/nfl_sim/models/positional_ep_v_0_1_0"
MODEL_PATH = os.path.join(MODEL_DIR, "positional_ep_model.json")
META_PATH = os.path.join(MODEL_DIR, "metadata.json")

FEATURES = ["yardline_100", "down", "ydstogo", "goal_to_go"]
TARGET = "ep"

# XGBoost hyperparameters — shallow tree to capture the smooth EP surface
# without overfitting the noise in individual play EP labels.
XGB_PARAMS = {
    "n_estimators": 300,
    "max_depth": 4,
    "learning_rate": 0.05,
    "subsample": 0.8,
    "colsample_bytree": 1.0,
    "min_child_weight": 50,
    "objective": "reg:squarederror",
    "tree_method": "hist",
    "device": "cpu",
    "seed": 42,
}

VALID_PLAY_TYPES = {"pass", "run"}

os.makedirs(MODEL_DIR, exist_ok=True)


def load_and_filter(path):
    """
    Inputs:  Raw PBP CSV at `path`.
    Outputs: DataFrame of clean scrimmage plays with non-null EP and valid features.
    Purpose: Remove non-scrimmage rows (timeouts, end-of-halfs, kickoffs, spikes)
             that carry meaningless or extreme EP values. Drops any row where a
             required feature or the target is null.
    """
    usecols = FEATURES + [TARGET, "play_type", "qb_spike", "qb_kneel",
                          "penalty", "aborted_play"]
    df = pd.read_csv(path, usecols=usecols, low_memory=False)

    n_raw = len(df)
    # Scrimmage plays only
    df = df[df["play_type"].isin(VALID_PLAY_TYPES)].copy()
    # No spikes, kneels, aborted plays (they have anomalous EP)
    df = df[
        (df["qb_spike"].fillna(0) == 0) &
        (df["qb_kneel"].fillna(0) == 0) &
        (df["aborted_play"].fillna(0) == 0)
    ]
    # Drop rows where any required column is null
    df = df.dropna(subset=FEATURES + [TARGET])
    # EP physical bounds: range is roughly −7 (own end zone, opponent scores)
    # to +7 (opponent 1-yd line, offense about to score). Filter extremes that
    # appear from data artefacts.
    df = df[(df[TARGET] >= -7.5) & (df[TARGET] <= 7.5)]
    # Down must be 1-4
    df = df[df["down"].isin([1, 2, 3, 4])]

    n_clean = len(df)
    print(f"Loaded {n_raw:,} rows -> {n_clean:,} clean scrimmage plays after filtering")
    return df[FEATURES + [TARGET]]


def train(df):
    """
    Inputs:  Clean DataFrame with FEATURES + TARGET columns.
    Outputs: (model, val_metrics dict)
    Purpose: 70/15/15 train/val/test split -> XGBRegressor fit -> eval on val.
    """
    X = df[FEATURES].values.astype(np.float32)
    y = df[TARGET].values.astype(np.float32)

    X_tr, X_tmp, y_tr, y_tmp = train_test_split(X, y, test_size=0.30, random_state=42)
    X_val, X_te, y_val, y_te = train_test_split(X_tmp, y_tmp, test_size=0.50, random_state=42)

    model = xgb.XGBRegressor(**XGB_PARAMS, early_stopping_rounds=20, eval_metric="rmse")
    model.fit(
        X_tr, y_tr,
        eval_set=[(X_val, y_val)],
        verbose=50,
    )

    val_preds = model.predict(X_val)
    test_preds = model.predict(X_te)

    metrics = {
        "val_rmse": float(np.sqrt(mean_squared_error(y_val, val_preds))),
        "val_mae": float(mean_absolute_error(y_val, val_preds)),
        "test_rmse": float(np.sqrt(mean_squared_error(y_te, test_preds))),
        "test_mae": float(mean_absolute_error(y_te, test_preds)),
        "n_train": int(len(X_tr)),
        "n_val": int(len(X_val)),
        "n_test": int(len(X_te)),
        "best_iteration": int(model.best_iteration),
    }
    return model, metrics


def main():
    print("=" * 60)
    print("Positional EP Model — Training (V.0.1.0)")
    print("=" * 60)

    df = load_and_filter(DATA_PATH)
    model, metrics = train(df)

    print("\n[Metrics]")
    for k, v in metrics.items():
        print(f"  {k}: {v}")

    # Serialize booster to XGBoost native JSON
    model.save_model(MODEL_PATH)
    print(f"\nModel saved -> {MODEL_PATH}")

    metadata = {
        "version": "V.0.1.0",
        "model_type": "xgb.XGBRegressor",
        "features": FEATURES,
        "target": TARGET,
        "xgb_params": XGB_PARAMS,
        "metrics": metrics,
        "data_source": DATA_PATH,
    }
    with open(META_PATH, "w") as f:
        json.dump(metadata, f, indent=2)
    print(f"Metadata saved -> {META_PATH}")

    # Quick sanity: EP should increase as yardline approaches goal
    print("\n[Sanity: EP by yardline on 1st & 10]")
    checks = [99, 75, 50, 25, 10, 5, 1]
    booster = model.get_booster()
    for yl in checks:
        X_chk = xgb.DMatrix(
            np.array([[yl, 1, 10, 0]], dtype=np.float32),
            feature_names=FEATURES
        )
        ep = float(booster.predict(X_chk)[0])
        print(f"  yardline_100={yl:3d}  EP={ep:+.3f}")


if __name__ == "__main__":
    main()
