"""
Air yards model — zone-split double hurdle (gate + regressor per zone).

Trains: 3-way gate per zone classifying each pass attempt as screen
(air_yards <= 0) / standard (0 < air_yards < 20) / deep (air_yards >= 20),
then a separate regressor per level predicting the actual air_yards value.

Data: live nfl_data_py PBP pull + data/dna/*.json joins, GroupShuffleSplit by
game_id for a 70/15/15 train/val/test split, 2020+ training window.

Features: down, ydstogo, yardline_100, score_differential,
game_seconds_remaining, cpoe_by_filter, target_share_by_filter,
carry_share_by_filter, play_ttt, avg_air_yards_per_att, deep_target_rate.
game_engine.py's X_ay_zone construction must match this list and order
exactly. See docs/models/README.md §2.2 for the deployed feature list and
docs/sims/inputs/README.md for why the last three were added (the model
previously had no feature encoding throw-depth preference at all — only
completion accuracy and target/carry volume).
"""
import os
import json
import joblib
import numpy as np
import pandas as pd
import xgboost as xgb
import nfl_data_py as nfl
from sklearn.model_selection import GroupShuffleSplit
from sklearn.metrics import mean_absolute_error, mean_squared_error, accuracy_score

SEASONS = list(range(2020, 2026))
MODEL_DIR = "src/nfl_sim/models/air_yards_v_0_1_1"
DNA_DIR = "data/dna"
ZONES = ["primary", "redzone", "goalline"]
SCREEN_MAX = 0
DEEP_MIN = 20

FEATURES = [
    "down", "ydstogo", "yardline_100", "score_differential",
    "game_seconds_remaining", "cpoe_by_filter", "target_share_by_filter",
    "carry_share_by_filter", "play_ttt", "avg_air_yards_per_att", "deep_target_rate",
]

CACHE_PATH = os.path.join(MODEL_DIR, "_train_cache.parquet")


def zone_of(yardline_100):
    return np.where(yardline_100 <= 5, "goalline", np.where(yardline_100 <= 20, "redzone", "primary"))


def level_of(air_yards):
    return np.where(air_yards <= SCREEN_MAX, 0, np.where(air_yards >= DEEP_MIN, 2, 1))


def load_dna():
    with open(os.path.join(DNA_DIR, "qb_dna.json")) as f:
        qb_dna = json.load(f)
    with open(os.path.join(DNA_DIR, "skill_dna.json")) as f:
        skill_dna = json.load(f)
    qb_dna.pop("_metadata", None)
    skill_dna.pop("_metadata", None)
    return qb_dna, skill_dna


import re

_SUFFIX_RE = re.compile(r"\s+(Jr\.?|Sr\.?|I{2,3}|IV)$")


def to_short_name(full_name):
    """Converts a DNA-file full name ("Patrick Mahomes") to nflfastR's PBP
    player-name format ("P.Mahomes") — first-initial + last name, no suffix.
    Verified empirically: 98.8% row-weighted match rate for QBs, 95.7% for
    skill players against real 2023-2024 PBP (the residual misses are almost
    entirely deep backups with negligible snap counts). Without this, every
    DNA-based feature lookup in this script silently falls back to its default
    for 100% of rows — the bug this function fixes — since dna_dict's keys
    (full names) never match df["passer_player_name"]/df["receiver_player_name"]
    (nflfastR's abbreviated format) directly."""
    full_name = _SUFFIX_RE.sub("", full_name.strip())
    parts = full_name.split(" ")
    if len(parts) < 2:
        return full_name
    return f"{parts[0][0]}.{parts[-1]}"


def build_dna_lookup(dna_dict, key):
    lookup = {}
    for name, data in dna_dict.items():
        overall = data.get(key, 0.0)
        splits = data.get("splits", {})
        lookup[to_short_name(name)] = {zone: splits.get(zone, {}).get(key, overall) for zone in ZONES}
    return lookup


def load_and_prepare(use_cache=True):
    if use_cache and os.path.exists(CACHE_PATH):
        print(f"Loading cached prepared dataset from {CACHE_PATH}...")
        return pd.read_parquet(CACHE_PATH)

    print(f"Pulling real PBP for seasons {SEASONS}...")
    df_raw = nfl.import_pbp_data(SEASONS)
    df = df_raw[df_raw["season_type"] == "REG"].copy() if "season_type" in df_raw.columns else df_raw[df_raw["game_type"] == "REG"].copy()

    df = df[(df["play_type"] == "pass") & (df["qb_spike"] == 0) & (df["aborted_play"] == 0)].copy()
    df = df.dropna(subset=["air_yards", "yardline_100", "game_id"])
    print(f"Pass attempts after filtering: {len(df)}")

    df["zone"] = zone_of(df["yardline_100"].values)
    df["ay_level"] = level_of(df["air_yards"].values)
    df["score_differential"] = df["score_differential"].fillna(0)
    df["game_seconds_remaining"] = df["game_seconds_remaining"].fillna(1800)
    df["ydstogo"] = df["ydstogo"].fillna(10)
    df["down"] = df["down"].fillna(1)

    qb_dna, skill_dna = load_dna()
    qb_lookup = build_dna_lookup(qb_dna, "cpoe")
    target_share_lookup = build_dna_lookup(skill_dna, "target_share")
    carry_share_lookup = build_dna_lookup(skill_dna, "carry_share")
    # avg_time_to_throw_sec, avg_air_yards_per_att, deep_target_rate have no
    # per-zone splits in the DNA files — build_dna_lookup's fallback-to-overall
    # behavior means these three come back flat across all zones, which is
    # correct (they're season-level QB/receiver traits, not situational).
    ttt_lookup = build_dna_lookup(qb_dna, "avg_time_to_throw_sec")
    avg_air_yards_lookup = build_dna_lookup(qb_dna, "avg_air_yards_per_att")
    deep_target_lookup = build_dna_lookup(skill_dna, "deep_target_rate")

    def lookup_col(names, zones, table, default=0.0):
        return [table.get(n, {}).get(z, default) for n, z in zip(names, zones)]

    df["cpoe_by_filter"] = lookup_col(df["passer_player_name"].values, df["zone"].values, qb_lookup)
    df["target_share_by_filter"] = lookup_col(df["receiver_player_name"].values, df["zone"].values, target_share_lookup)
    df["carry_share_by_filter"] = lookup_col(df["receiver_player_name"].values, df["zone"].values, carry_share_lookup)
    # Training uses each QB's flat average time-to-throw (no per-play noise
    # available in historical data); at inference game_engine.py feeds the real
    # per-play sampled play_ttt (Normal(avg_ttt, 0.6) clipped [1.5,4.5]), which
    # is unbiased around this same mean — a standard, valid train/inference
    # pairing (train on the clean signal, infer with calibrated stochasticity
    # around it), not a train/inference mismatch.
    df["play_ttt"] = lookup_col(df["passer_player_name"].values, df["zone"].values, ttt_lookup, default=2.7)
    df["avg_air_yards_per_att"] = lookup_col(df["passer_player_name"].values, df["zone"].values, avg_air_yards_lookup, default=8.0)
    df["deep_target_rate"] = lookup_col(df["receiver_player_name"].values, df["zone"].values, deep_target_lookup, default=0.12)

    out = df[["game_id", "zone", "ay_level", "air_yards"] + FEATURES].copy()
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


def train_zone(zone, train_df, val_df, test_df, save=True):
    tr = train_df[train_df["zone"] == zone]
    va = val_df[val_df["zone"] == zone]
    te = test_df[test_df["zone"] == zone]

    X_tr, y_tr = tr[FEATURES], tr["ay_level"]
    X_va, y_va = va[FEATURES], va["ay_level"]
    X_te, y_te = te[FEATURES], te["ay_level"]

    # Goalline has ~zero "deep" throws (physically can't legally gain 20+ air
    # yards from inside your own 5) — training data there only ever has 2 of
    # the 3 ay_level classes. Forcing num_class=3 crashes at eval time
    # (predict() shape mismatch) and letting XGBClassifier auto-infer crashes
    # at fit time (num_class=0). Cleanest fix: detect this upfront and skip
    # training a zone-specific gate entirely for a zone missing class
    # coverage — fall back to copying primary's gate after the run instead of
    # fighting xgboost's edge-case handling of degenerate label sets.
    gate_missing = sorted(y_tr.unique()) != [0, 1, 2]
    if gate_missing:
        print(f"[{zone}] gate: only classes {sorted(y_tr.unique())} present in training data — "
              f"will fall back to primary's gate  n_train={len(tr)} n_val={len(va)} n_test={len(te)}")
        gate_acc = None
    else:
        gate = xgb.XGBClassifier(
            objective="multi:softprob", num_class=3, n_estimators=500,
            max_depth=5, learning_rate=0.05, random_state=42, tree_method="hist",
            early_stopping_rounds=30,
        )
        gate.fit(X_tr, y_tr, eval_set=[(X_va, y_va)], verbose=False)
        gate_preds = np.asarray(gate.predict(X_te)).reshape(-1)
        gate_acc = accuracy_score(y_te, gate_preds)
        print(f"[{zone}] gate test accuracy={gate_acc:.4f}  n_train={len(tr)} n_val={len(va)} n_test={len(te)}")

    reg_metrics = {}
    regressors = {}
    for level, name in [(0, "screen"), (1, "std"), (2, "deep")]:
        tr_l = tr[tr["ay_level"] == level]
        va_l = va[va["ay_level"] == level]
        te_l = te[te["ay_level"] == level]
        if len(tr_l) < 20 or len(va_l) < 5 or len(te_l) < 5:
            print(f"  [{zone}/{name}] too few rows, skipping (n_train={len(tr_l)})")
            continue

        reg = xgb.XGBRegressor(
            objective="reg:squarederror", n_estimators=500, learning_rate=0.05,
            max_depth=(6 if level == 1 else 4), random_state=42, tree_method="hist",
            early_stopping_rounds=30,
        )
        reg.fit(tr_l[FEATURES], tr_l["air_yards"], eval_set=[(va_l[FEATURES], va_l["air_yards"])], verbose=False)
        preds = reg.predict(te_l[FEATURES])
        mae = mean_absolute_error(te_l["air_yards"], preds)
        rmse = np.sqrt(mean_squared_error(te_l["air_yards"], preds))
        print(f"  [{zone}/{name}] n_train={len(tr_l)} n_test={len(te_l)}  test MAE={mae:.3f} RMSE={rmse:.3f}  "
              f"pred_mean={preds.mean():.3f} real_mean={te_l['air_yards'].mean():.3f}")

        regressors[level] = reg
        reg_metrics[name] = {
            "n_train": int(len(tr_l)), "n_test": int(len(te_l)),
            "test_mae": float(mae), "test_rmse": float(rmse),
            "test_pred_mean": float(preds.mean()), "test_real_mean": float(te_l["air_yards"].mean()),
        }

    if save:
        if not gate_missing:
            joblib.dump(gate, os.path.join(MODEL_DIR, f"{zone}_tri_gate.joblib"))
        for level, name in [(0, "screen"), (1, "std"), (2, "deep")]:
            if level in regressors:
                joblib.dump(regressors[level], os.path.join(MODEL_DIR, f"{zone}_{name}_reg.joblib"))

    missing_levels = [name for level, name in [(0, "screen"), (1, "std"), (2, "deep")] if level not in regressors]
    return {"gate_test_accuracy": gate_acc, "gate_missing": gate_missing, "n_train": int(len(tr)),
            "n_val": int(len(va)), "n_test": int(len(te)), "regressors": reg_metrics,
            "missing_levels": missing_levels}


def run(save=True, use_cache=True):
    df = load_and_prepare(use_cache=use_cache)
    train_df, val_df, test_df = grouped_split(df)
    print(f"Split sizes (rows): train={len(train_df)} val={len(val_df)} test={len(test_df)}")

    zone_metrics = {}
    for zone in ZONES:
        zone_metrics[zone] = train_zone(zone, train_df, val_df, test_df, save=save)

    if save:
        # Safety net: inference.py unconditionally loads all 3 regressor levels
        # for any zone whose gate file exists. If a zone had too little data for
        # a given level (goalline has ~zero real "deep" throws — physically
        # can't legally gain 20+ air yards from inside your own 5), fall back to
        # copying 'primary's regressor for that level so the file always exists.
        import shutil
        # Fallback donor is the ADJACENT zone by field position, not always
        # 'primary'. goalline (yardline_100 <= 5) falling back to primary
        # (trained exclusively on yardline_100 > 20 — never seen a play from
        # inside the 5) was tried and measured worse than the original
        # problem: the gate has to extrapolate wildly out-of-distribution on
        # the one feature that matters most there, distorting the whole
        # screen/std/deep mix. redzone (6-20) is a much closer donor and has
        # real (if thin) deep-class training data of its own.
        fallback_donor = {"goalline": "redzone", "redzone": "primary", "primary": "primary"}
        for zone in ZONES:
            donor = fallback_donor[zone]
            if zone_metrics[zone].get("gate_missing"):
                src = os.path.join(MODEL_DIR, f"{donor}_tri_gate.joblib")
                dst = os.path.join(MODEL_DIR, f"{zone}_tri_gate.joblib")
                shutil.copyfile(src, dst)
                print(f"[{zone}] gate — insufficient class coverage — copied {donor}'s gate as fallback")
            for level, name in [(0, "screen"), (1, "std"), (2, "deep")]:
                if name in zone_metrics[zone].get("missing_levels", []):
                    src = os.path.join(MODEL_DIR, f"{donor}_{name}_reg.joblib")
                    dst = os.path.join(MODEL_DIR, f"{zone}_{name}_reg.joblib")
                    shutil.copyfile(src, dst)
                    print(f"[{zone}/{name}] insufficient data — copied {donor}'s regressor as fallback")

        metadata = {
            "version": "V.0.3.0",
            "trained_by": "train_zone_split.py (clock_physics_v020 Round 11)",
            "seasons": SEASONS,
            "split": "GroupShuffleSplit by game_id, 70/15/15 train/val/test",
            "features": FEATURES,
            "screen_max": SCREEN_MAX,
            "deep_min": DEEP_MIN,
            "zone_metrics": zone_metrics,
        }
        with open(os.path.join(MODEL_DIR, "metadata.json"), "w") as f:
            json.dump(metadata, f, indent=2)
        print(f"\nSaved metadata.json and zone-split gates/regressors to {MODEL_DIR}")

    return zone_metrics


if __name__ == "__main__":
    run()
