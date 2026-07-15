"""
YAC model retrain — clock_physics_v020 Round 8/9.

Supersedes `train.py` (the orphaned single-model, non-zone-split V.0.1.1
script — it trains a different architecture than what's actually deployed
and should not be run expecting it to reproduce or refresh the live model;
see AGENTS.md §11.7 and Round 8 in docs/audit/clock_physics_v020/README.md).

This script trains the three zone-specific regressors
(`{primary,redzone,goalline}_yac_reg.joblib`) that `inference.py` actually
loads, using the same live nfl_data_py pull + data/dna/*.json join pattern
as `air_yards_v_0_1_1/train.py` (rather than a static training CSV), so
training features can never drift from what game_engine.py feeds the model
at inference.

Feature set (Cam's decision, see clock_physics_v020 Round 8 discussion):
situational features only — no player "skill" traits (elusiveness,
broken_tackle_rate, top_speed_mph, avg_separation_yds) this round. Checked
NGS receiving coverage via nfl_data_py.import_ngs_data('receiving'):
avg_separation has been populated back to 2016 for ~200-220 qualifying
pass-catchers/season (better coverage than remembered), but top_speed_mph
is not part of that dataset at all (no 'speed' column in NGS rushing or
receiving) and its source in skill_dna.json is unclear/likely sparse.
Decision: skip all four for now, revisit avg_separation_yds specifically
in a future incremental pass. One experimental addition this round:
`room_after_catch = yardline_100 - air_yards`, added because the deployed
model over-predicts YAC near the goal line without it (Round 8 Finding 3) —
Cam is ready to cut this feature if post-retrain validation shows it isn't
earning its keep.

Player-name join fix: this script previously keyed DNA lookups on full names
("Patrick Mahomes") against nflfastR's abbreviated PBP names ("P.Mahomes") —
a 100% mismatch that silently zeroed out cpoe_by_filter/target_share_by_filter
/carry_share_by_filter in every prior retrain. Fixed with the same
to_short_name() normalization used in air_yards_v_0_1_1/train_zone_split.py.
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
from sklearn.metrics import mean_absolute_error, mean_squared_error

SEASONS = list(range(2020, 2026))
MODEL_DIR = "src/nfl_sim/models/yac_model_v_0_1_1"
DNA_DIR = "data/dna"
ZONES = ["primary", "redzone", "goalline"]

FEATURES = [
    "air_yards", "yardline_100", "ydstogo", "score_differential",
    "game_seconds_remaining", "cpoe_by_filter", "target_share_by_filter",
    "carry_share_by_filter", "room_after_catch",
]

_SUFFIX_RE = re.compile(r"\s+(Jr\.?|Sr\.?|I{2,3}|IV)$")


def to_short_name(full_name):
    """DNA full name ("Patrick Mahomes") -> nflfastR PBP format ("P.Mahomes")."""
    full_name = _SUFFIX_RE.sub("", full_name.strip())
    parts = full_name.split(" ")
    if len(parts) < 2:
        return full_name
    return f"{parts[0][0]}.{parts[-1]}"


def zone_of(yardline_100):
    return np.where(yardline_100 <= 5, "goalline", np.where(yardline_100 <= 20, "redzone", "primary"))


def load_dna():
    with open(os.path.join(DNA_DIR, "qb_dna.json")) as f:
        qb_dna = json.load(f)
    with open(os.path.join(DNA_DIR, "skill_dna.json")) as f:
        skill_dna = json.load(f)
    qb_dna.pop("_metadata", None)
    skill_dna.pop("_metadata", None)
    return qb_dna, skill_dna


def build_dna_lookup(dna_dict, key):
    """Per (name, zone) lookup mirroring game_engine.py's precomputed_* dicts:
    zone-specific value if present, else the player's overall value, else 0.0."""
    lookup = {}
    for name, data in dna_dict.items():
        overall = data.get(key, 0.0)
        splits = data.get("splits", {})
        lookup[to_short_name(name)] = {
            zone: splits.get(zone, {}).get(key, overall) for zone in ZONES
        }
    return lookup


CACHE_PATH = os.path.join(MODEL_DIR, "_train_cache.parquet")


def load_and_prepare(use_cache=True):
    if use_cache and os.path.exists(CACHE_PATH):
        print(f"Loading cached prepared dataset from {CACHE_PATH}...")
        return pd.read_parquet(CACHE_PATH)

    print(f"Pulling real PBP for seasons {SEASONS}...")
    df_raw = nfl.import_pbp_data(SEASONS)
    df = df_raw[df_raw["season_type"] == "REG"].copy() if "season_type" in df_raw.columns else df_raw[df_raw["game_type"] == "REG"].copy()

    df = df[(df["play_type"] == "pass") & (df["complete_pass"] == 1)].copy()
    df = df.dropna(subset=["air_yards", "yards_after_catch", "yardline_100", "game_id"])
    print(f"Completed passes after filtering: {len(df)}")

    df["zone"] = zone_of(df["yardline_100"].values)
    df["room_after_catch"] = df["yardline_100"] - df["air_yards"]
    df["score_differential"] = df["score_differential"].fillna(0)
    df["game_seconds_remaining"] = df["game_seconds_remaining"].fillna(1800)
    df["ydstogo"] = df["ydstogo"].fillna(10)

    qb_dna, skill_dna = load_dna()
    qb_lookup = build_dna_lookup(qb_dna, "cpoe")
    target_share_lookup = build_dna_lookup(skill_dna, "target_share")
    carry_share_lookup = build_dna_lookup(skill_dna, "carry_share")

    def lookup_col(names, zones, table):
        return [table.get(n, {}).get(z, 0.0) for n, z in zip(names, zones)]

    df["cpoe_by_filter"] = lookup_col(df["passer_player_name"].values, df["zone"].values, qb_lookup)
    df["target_share_by_filter"] = lookup_col(df["receiver_player_name"].values, df["zone"].values, target_share_lookup)
    df["carry_share_by_filter"] = lookup_col(df["receiver_player_name"].values, df["zone"].values, carry_share_lookup)

    df["yac_actual"] = df["yards_after_catch"]

    out = df[["game_id", "zone", "yac_actual"] + FEATURES].copy()
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


def train_zone(zone, train_df, val_df, test_df, objective="reg:squarederror", save=True):
    tr = train_df[train_df["zone"] == zone]
    va = val_df[val_df["zone"] == zone]
    te = test_df[test_df["zone"] == zone]

    X_tr, y_tr = tr[FEATURES], tr["yac_actual"]
    X_va, y_va = va[FEATURES], va["yac_actual"]
    X_te, y_te = te[FEATURES], te["yac_actual"]

    model = xgb.XGBRegressor(
        objective=objective,
        n_estimators=600,
        max_depth=6,
        learning_rate=0.05,
        subsample=0.8,
        random_state=42,
        tree_method="hist",
        early_stopping_rounds=30,
    )
    model.fit(X_tr, y_tr, eval_set=[(X_va, y_va)], verbose=False)

    preds_test = model.predict(X_te)
    mae = mean_absolute_error(y_te, preds_test)
    rmse = np.sqrt(mean_squared_error(y_te, preds_test))
    baseline_mae = mean_absolute_error(y_te, np.full_like(y_te, y_tr.mean()))
    fi = dict(zip(FEATURES, [float(x) for x in model.feature_importances_]))

    print(f"[{zone}] obj={objective}  n_train={len(tr)} n_val={len(va)} n_test={len(te)}  "
          f"test MAE={mae:.3f} (baseline {baseline_mae:.3f})  test RMSE={rmse:.3f}  "
          f"pred_mean={preds_test.mean():.3f} real_mean={y_te.mean():.3f}  "
          f"room_after_catch_importance={fi['room_after_catch']:.4f}")

    if save:
        joblib.dump(model, os.path.join(MODEL_DIR, f"{zone}_yac_reg.joblib"))

    return {
        "n_train": int(len(tr)), "n_val": int(len(va)), "n_test": int(len(te)),
        "test_mae": float(mae), "test_rmse": float(rmse), "baseline_test_mae": float(baseline_mae),
        "test_pred_mean": float(preds_test.mean()), "test_real_mean": float(y_te.mean()),
        "objective": objective, "feature_importance": fi,
    }


def run(objective="reg:squarederror", save=True, use_cache=True):
    df = load_and_prepare(use_cache=use_cache)
    train_df, val_df, test_df = grouped_split(df)
    print(f"Split sizes (rows): train={len(train_df)} val={len(val_df)} test={len(test_df)}")

    zone_metrics = {}
    for zone in ZONES:
        zone_metrics[zone] = train_zone(zone, train_df, val_df, test_df, objective=objective, save=save)

    if save:
        metadata = {
            "version": "V.0.3.0",
            "trained_by": "train_zone_split.py (clock_physics_v020 Round 8/9)",
            "seasons": SEASONS,
            "split": "GroupShuffleSplit by game_id, 70/15/15 train/val/test",
            "objective": objective,
            "features": FEATURES,
            "zone_metrics": zone_metrics,
            "notes": "room_after_catch is experimental — see Round 8/9 validation before keeping.",
        }
        with open(os.path.join(MODEL_DIR, "metadata.json"), "w") as f:
            json.dump(metadata, f, indent=2)
        print(f"\nSaved metadata.json and {len(ZONES)} zone boosters to {MODEL_DIR}")

    return zone_metrics


if __name__ == "__main__":
    import sys
    objective = sys.argv[1] if len(sys.argv) > 1 else "reg:squarederror"
    run(objective=objective)
