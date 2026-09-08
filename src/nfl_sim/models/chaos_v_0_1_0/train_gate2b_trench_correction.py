"""
Chaos model Gate 2b — sack-probability CORRECTION layer using the newer,
richer pass-blocking/pass-rush trench composites (pass_block_off_z,
pass_def_z — built by scripts/eda/build_trench_dna_pass_composites.py).

This does NOT retrain Gate 2 (gate_2_sack.joblib) — its weights and its
already-verified calibration (2.410 net sacks/game vs. real 2.41) stay
completely untouched. Gate 2b sits alongside it: takes Gate 2's own
(calibrated) probability for a play, plus the two new composites Gate 2 was
never trained on, and learns a correction toward the real sack outcome. A
small stacking/meta-model, not a replacement.

Follows train_gate4.py's structure and conventions exactly (same repo, same
pull/split/eval/save pattern) — see that file for the established house
style this mirrors.

Target: real sack occurrence (binary) — sacks are INCLUDED in the play
filter here, unlike Gate 4's script, which excludes them (Gate 4 predicts
interceptions on the no-sack subset; here a sack is exactly what's being
predicted).

Historical Gate 2 features are reconstructed to match game_engine.py's live
X_g2 construction exactly (avg_time_to_throw_sec_qb, cpoe_qb,
def_pressure_rate, def_sack_rate, sack_rate_allowed, off_sack_rate_l4,
def_sack_rate_l4, down, ydstogo, yardline_100, score_differential) so Gate
2's reconstructed prediction here matches what it actually outputs in
production:
  - avg_time_to_throw_sec_qb uses the QB's own season average from
    qb_dna.json — deterministic, NOT the +/-noise draw game_engine.py adds
    at simulation time (no real-data equivalent to train that noise against).
  - def_pressure_rate/def_sack_rate/sack_rate_allowed come from that row's
    OWN season via trench_dna.json (not a fixed year — avoids look-ahead/
    era-drift issues, same principle used throughout this project's trench
    composite work).
  - off_sack_rate_l4/def_sack_rate_l4 are fed as exact duplicates of
    sack_rate_allowed/def_sack_rate, matching game_engine.py's current
    (documented, deliberately deferred) simplification — no real last-4-
    games data exists yet.

Run from repo root:
  python src/nfl_sim/models/chaos_v_0_1_0/train_gate2b_trench_correction.py
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

G2_FEATURES = [
    "avg_time_to_throw_sec_qb", "cpoe_qb", "def_pressure_rate", "def_sack_rate",
    "sack_rate_allowed", "off_sack_rate_l4", "def_sack_rate_l4",
    "down", "ydstogo", "yardline_100", "score_differential",
]
SACK_PROB_CALIBRATION_MULT = 1.24  # must match game_engine.py exactly

FEATURES = ["gate2_prob", "pass_block_off_z", "pass_def_z"]

CACHE_PATH = os.path.join(MODEL_DIR, "_gate2b_train_cache.parquet")

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

    # All real dropbacks -- sacks INCLUDED, that's exactly the target here.
    # Matches game_engine.py's Gate 2 call site (runs on every is_pass play).
    df = df[df["qb_dropback"] == 1].copy()
    df = df.dropna(subset=["yardline_100", "game_id", "sack", "posteam", "defteam", "passer_player_name"])
    print(f"Dropbacks after filtering: {len(df)}  (sack rate: {df['sack'].mean():.4f})")

    df["score_differential"] = df["score_differential"].fillna(0)
    df["ydstogo"] = df["ydstogo"].fillna(10)
    df["down"] = df["down"].fillna(1)

    with open(os.path.join(DNA_DIR, "qb_dna.json")) as f:
        qb_dna = json.load(f)
    qb_dna.pop("_metadata", None)
    ttt_lookup = {to_short_name(name): data.get("avg_time_to_throw_sec", 2.7) for name, data in qb_dna.items()}
    cpoe_lookup = {to_short_name(name): data.get("cpoe", 0.0) for name, data in qb_dna.items()}

    df["avg_time_to_throw_sec_qb"] = df["passer_player_name"].map(ttt_lookup).fillna(2.7)
    df["cpoe_qb"] = df["passer_player_name"].map(cpoe_lookup).fillna(0.0)

    with open(os.path.join(DNA_DIR, "trench_dna.json")) as f:
        trench = json.load(f)

    def trench_field(season_series, team_series, field, default):
        return [
            trench.get(str(int(s)), {}).get(t, {}).get(field, default)
            for s, t in zip(season_series, team_series)
        ]

    df["def_pressure_rate"] = trench_field(df["season"], df["defteam"], "def_pressure_rate", 0.15)
    df["def_sack_rate"] = trench_field(df["season"], df["defteam"], "def_sack_rate", 0.06)
    df["sack_rate_allowed"] = trench_field(df["season"], df["posteam"], "sack_rate_allowed", 0.06)
    df["off_sack_rate_l4"] = df["sack_rate_allowed"]
    df["def_sack_rate_l4"] = df["def_sack_rate"]

    df["pass_block_off_z"] = trench_field(df["season"], df["posteam"], "pass_block_off_z", 0.0)
    df["pass_def_z"] = trench_field(df["season"], df["defteam"], "pass_def_z", 0.0)

    out = df[["game_id", "sack"] + G2_FEATURES + ["pass_block_off_z", "pass_def_z"]].copy()
    out.to_parquet(CACHE_PATH)
    return out


def add_gate2_prob(df):
    """Run the ALREADY-DEPLOYED Gate 2 booster over the historical rows to
    get each row's real Gate 2 probability -- this is a feature for Gate 2b,
    not something being retrained."""
    g2_path = os.path.join(MODEL_DIR, "gate_2_sack.joblib")
    g2_sklearn = joblib.load(g2_path)
    booster = g2_sklearn.get_booster()

    X_g2 = df[G2_FEATURES].to_numpy(dtype=np.float32)
    raw = booster.inplace_predict(X_g2)
    df["gate2_prob"] = np.clip(raw * SACK_PROB_CALIBRATION_MULT, 0.0, 1.0)
    return df


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
    df = add_gate2_prob(df)
    train_df, val_df, test_df = grouped_split(df)
    print(f"Split sizes (rows): train={len(train_df)} val={len(val_df)} test={len(test_df)}")

    X_tr, y_tr = train_df[FEATURES], train_df["sack"]
    X_va, y_va = val_df[FEATURES], val_df["sack"]
    X_te, y_te = test_df[FEATURES], test_df["sack"]

    # No scale_pos_weight/rebalancing: same reasoning as Gate 4 -- this
    # model's raw probability output feeds a Bernoulli draw directly, not
    # just a classification decision. Rebalancing for the positive class
    # would inflate the predicted rate away from the real ~6% sack rate.
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
    baseline_brier = brier_score_loss(y_te, X_te["gate2_prob"])
    importances = dict(zip(FEATURES, clf.feature_importances_.round(4).tolist()))
    print(f"[gate_2b] n_train={len(X_tr)} n_test={len(X_te)}  brier={brier:.4f} (Gate2-alone baseline: {baseline_brier:.4f})  "
          f"logloss={ll:.4f} pr_auc={prauc:.4f}  pred_sack_rate={preds_proba.mean():.4f} real_sack_rate={y_te.mean():.4f}")
    print(f"Feature importances: {importances}")

    if save:
        joblib.dump(clf, os.path.join(MODEL_DIR, "gate_2b_trench_correction.joblib"))

        with open(os.path.join(MODEL_DIR, "metadata.json")) as f:
            metadata = json.load(f)
        metadata["gate_2b"] = {
            "features": FEATURES,
            "brier": float(brier),
            "gate2_alone_brier": float(baseline_brier),
            "logloss": float(ll),
            "prauc": float(prauc),
            "feature_importances": importances,
            "trained_by": "train_gate2b_trench_correction.py",
            "seasons": SEASONS,
        }
        with open(os.path.join(MODEL_DIR, "metadata.json"), "w") as f:
            json.dump(metadata, f, indent=2)
        print("Updated metadata.json's gate_2b entry and saved gate_2b_trench_correction.joblib")

    return {"brier": brier, "logloss": ll, "prauc": prauc, "importances": importances}


if __name__ == "__main__":
    run()
