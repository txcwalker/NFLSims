"""
Chaos model Gate 4 — interception classifier.

No training script existed for chaos_v_0_1_0 before this one. Written from
scratch for Gate 4 specifically (a single flat XGBoost classifier, not
zone-split like play-selection/air-yards — matches the original architecture).

Drops `off_int_rate_l4` from the original 7-feature set: at inference
(game_engine.py) this was fed as a hardcoded flat constant (0.023) identical
for every play regardless of QB — not even a stale duplicate of a real stat,
a literal with zero per-play variation, so it contributed nothing. No
real per-QB "last-4-games interception rate" data source exists yet (same gap
as Gate 2's off_sack_rate_l4/def_sack_rate_l4 — trench_dna.json and friends
are season-level only, no week-granularity data to build a rolling window
from). See docs/models/README.md §4 "Known Gaps."

Features (6): air_yards, cpoe_qb, down, ydstogo, yardline_100,
score_differential. game_engine.py's X_g4 construction must match this list
and order exactly.

Data: live nfl_data_py PBP pull + data/dna/qb_dna.json join (flat cpoe, not
zone-split — matches game_engine.py's qb_cpoe array, which is also flat),
GroupShuffleSplit by game_id, 70/15/15 train/val/test, 2020+ window. Uses the
same to_short_name() player-name-format fix as the air-yards/play-selection
scripts (nflfastR's "P.Mahomes" vs. qb_dna.json's "Patrick Mahomes").
"""
import os
import re
import json
import joblib
import numpy as np
import pandas as pd
import xgboost as xgb
import nfl_data_py as nfl
from sklearn.model_selection import GroupShuffleSplit
from sklearn.metrics import brier_score_loss, log_loss, average_precision_score

SEASONS = list(range(2020, 2026))
MODEL_DIR = "src/nfl_sim/models/chaos_v_0_1_0"
DNA_DIR = "data/dna"

FEATURES = ["air_yards", "cpoe_qb", "down", "ydstogo", "yardline_100", "score_differential"]

CACHE_PATH = os.path.join(MODEL_DIR, "_gate4_train_cache.parquet")

_SUFFIX_RE = re.compile(r"\s+(Jr\.?|Sr\.?|I{2,3}|IV)$")


def to_short_name(full_name):
    full_name = _SUFFIX_RE.sub("", full_name.strip())
    parts = full_name.split(" ")
    if len(parts) < 2:
        return full_name
    return f"{parts[0][0]}.{parts[-1]}"


def load_and_prepare(use_cache=True):
    if use_cache and os.path.exists(CACHE_PATH):
        print(f"Loading cached prepared dataset from {CACHE_PATH}...")
        return pd.read_parquet(CACHE_PATH)

    print(f"Pulling real PBP for seasons {SEASONS}...")
    df_raw = nfl.import_pbp_data(SEASONS)
    df = df_raw[df_raw["season_type"] == "REG"].copy() if "season_type" in df_raw.columns else df_raw[df_raw["game_type"] == "REG"].copy()

    # Real (non-sack) pass attempts only — matches game_engine.py's Gate 4
    # call site, which only runs on `no_sack_pass` plays.
    df = df[(df["play_type"] == "pass") & (df["sack"] == 0) & (df["qb_spike"] == 0) & (df["aborted_play"] == 0)].copy()
    df = df.dropna(subset=["air_yards", "yardline_100", "game_id", "interception"])
    print(f"Non-sack pass attempts after filtering: {len(df)}")

    df["score_differential"] = df["score_differential"].fillna(0)
    df["ydstogo"] = df["ydstogo"].fillna(10)
    df["down"] = df["down"].fillna(1)

    with open(os.path.join(DNA_DIR, "qb_dna.json")) as f:
        qb_dna = json.load(f)
    qb_dna.pop("_metadata", None)
    cpoe_lookup = {to_short_name(name): data.get("cpoe", 0.0) for name, data in qb_dna.items()}

    df["cpoe_qb"] = df["passer_player_name"].map(cpoe_lookup).fillna(0.0)

    out = df[["game_id", "interception"] + FEATURES].copy()
    out.to_parquet(CACHE_PATH)
    return out


def grouped_split(df):
    gss1 = GroupShuffleSplit(n_splits=1, train_size=0.70, random_state=42)
    train_idx, temp_idx = next(gss1.split(df, groups=df["game_id"]))
    train_df = df.iloc[train_idx]
    temp_df = df.iloc[temp_idx]

    gss2 = GroupShuffleSplit(n_splits=1, train_size=0.50, random_state=42)
    val_idx, test_idx = next(gss2.split(temp_df, groups=temp_df["game_id"]))
    val_df = temp_df.iloc[val_idx]
    test_df = temp_df.iloc[test_idx]

    return train_df, val_df, test_df


def run(save=True, use_cache=True):
    df = load_and_prepare(use_cache=use_cache)
    train_df, val_df, test_df = grouped_split(df)
    print(f"Split sizes (rows): train={len(train_df)} val={len(val_df)} test={len(test_df)}")

    X_tr, y_tr = train_df[FEATURES], train_df["interception"]
    X_va, y_va = val_df[FEATURES], val_df["interception"]
    X_te, y_te = test_df[FEATURES], test_df["interception"]

    # No scale_pos_weight/rebalancing: this model's raw probability output is
    # used directly by game_engine.py as a Bernoulli draw probability, not
    # just a classification decision — rebalancing for the rare positive
    # class (interceptions, ~2.2% base rate) inflates predicted probability
    # (a first attempt at this hit 37% predicted vs. 2.2% real). Let the
    # natural class imbalance train normally so the output stays calibrated.
    clf = xgb.XGBClassifier(
        objective="binary:logistic", n_estimators=500, max_depth=4,
        learning_rate=0.05, random_state=42, tree_method="hist",
        early_stopping_rounds=30,
    )
    clf.fit(X_tr, y_tr, eval_set=[(X_va, y_va)], verbose=False)
    preds_proba = clf.predict_proba(X_te)[:, 1]

    brier = brier_score_loss(y_te, preds_proba)
    ll = log_loss(y_te, preds_proba, labels=[0, 1])
    prauc = average_precision_score(y_te, preds_proba)
    print(f"[gate_4] n_train={len(X_tr)} n_test={len(X_te)}  brier={brier:.4f} logloss={ll:.4f} pr_auc={prauc:.4f}  "
          f"pred_int_rate={preds_proba.mean():.4f} real_int_rate={y_te.mean():.4f}")

    if save:
        # inference.py loads this via joblib.load() then .get_booster() — a
        # real joblib pickle of the XGBClassifier, not native XGBoost JSON
        # format (unlike play_selection_v_0_1_0's convention — each model
        # folder in this repo has its own, inconsistent save format).
        joblib.dump(clf, os.path.join(MODEL_DIR, "gate_4_interception.joblib"))

        with open(os.path.join(MODEL_DIR, "metadata.json")) as f:
            metadata = json.load(f)
        metadata["gate_4"] = {
            "features": FEATURES,
            "brier": float(brier),
            "logloss": float(ll),
            "prauc": float(prauc),
            "trained_by": "train_gate4.py",
            "seasons": SEASONS,
        }
        with open(os.path.join(MODEL_DIR, "metadata.json"), "w") as f:
            json.dump(metadata, f, indent=2)
        print("Updated metadata.json's gate_4 entry and saved gate_4_interception.joblib")

    return {"brier": brier, "logloss": ll, "prauc": prauc}


if __name__ == "__main__":
    run()
