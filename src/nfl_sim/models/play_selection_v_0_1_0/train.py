"""
Play selection model — pass/run choice, per (down/distance bucket x zone).

No training script existed for this model before this one (confirmed: zero
.py files in this folder). Written from scratch following the same
methodology as the air-yards/YAC zone-split retrains: live nfl_data_py pull +
data/dna/*.json joins, GroupShuffleSplit by game_id for a 70/15/15
train/val/test split, 2020+ training window.

Architecture: 11 down/distance buckets (1_10, 1_long, 1_short, 2_short,
2_med, 2_long, 3_short, 3_med, 3_long, 4_short, 4_med_long) x 3 zones
(primary/redzone/goalline) = up to 33 independent binary XGBoost classifiers,
falling back to the primary-zone classifier for any zone/bucket combo with
too little data. Bucket and zone boundaries match game_engine.py's inference
grouping exactly (game_engine.py:677-700 for buckets, the yardline_100 <= 5 /
<= 20 / else convention used everywhere else for zones).

Features: yardline_100, game_seconds_remaining, score_differential,
timeouts_pos (offense), timeouts_def (defense), leverage (=
score_differential * game_seconds_remaining), cpoe_by_filter,
target_share_by_filter, carry_share_by_filter. `proe_by_filter` (the 10th
feature in the original model) was deliberately dropped — redundant with the
separate post-model PROE overlay (proe_overlay_v_0_1_0.py) that's the only
user-adjustable PROE mechanism; see docs/models/README.md §2.13 and
docs/sims/inputs/README.md for the full discussion.

Player-name join note: nflfastR's *_player_name columns use abbreviated
format ("P.Mahomes"), not the full names DNA files use as keys ("Patrick
Mahomes") — see air_yards_v_0_1_1/train_zone_split.py's to_short_name() for
the empirically-verified fix (98.8%/95.7% row-weighted match rates for
QBs/skill players) applied identically here.

target_share_by_filter/carry_share_by_filter are RB-specific at inference
(game_engine.py uses self.rb_starters[team], a single designated starter for
the whole game) — not the specific receiver/rusher on any given play, since
play type hasn't been decided yet when this model runs. Reconstructed here as
each team's season-long leading rusher (most rush attempts), matching the
simulation's fixed-starter-per-game convention, and joined to every play
(pass and run alike) for that team that season.
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
from sklearn.metrics import accuracy_score, log_loss

SEASONS = list(range(2020, 2026))
MODEL_DIR = "src/nfl_sim/models/play_selection_v_0_1_0"
DNA_DIR = "data/dna"
ZONES = ["primary", "redzone", "goalline"]
BUCKETS = ["1_10", "1_long", "1_short", "2_short", "2_med", "2_long",
           "3_short", "3_med", "3_long", "4_short", "4_med_long"]

FEATURES = [
    "yardline_100", "game_seconds_remaining", "score_differential",
    "timeouts_pos", "timeouts_def", "leverage",
    "cpoe_by_filter", "target_share_by_filter", "carry_share_by_filter",
]

CACHE_PATH = os.path.join(MODEL_DIR, "_train_cache.parquet")

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


def bucket_of(down, distance):
    down = np.asarray(down)
    distance = np.asarray(distance)
    out = np.empty(len(down), dtype=object)
    m1 = down == 1
    out[m1] = np.where(distance[m1] == 10, "1_10", np.where(distance[m1] > 10, "1_long", "1_short"))
    for d in (2, 3):
        md = down == d
        suffix = np.where(distance[md] <= 3, "short", np.where(distance[md] <= 7, "med", "long"))
        out[md] = [f"{d}_{s}" for s in suffix]
    m4 = down == 4
    suffix4 = np.where(distance[m4] <= 2, "short", "med_long")
    out[m4] = [f"4_{s}" for s in suffix4]
    return out


def load_dna():
    with open(os.path.join(DNA_DIR, "qb_dna.json")) as f:
        qb_dna = json.load(f)
    with open(os.path.join(DNA_DIR, "skill_dna.json")) as f:
        skill_dna = json.load(f)
    qb_dna.pop("_metadata", None)
    skill_dna.pop("_metadata", None)
    return qb_dna, skill_dna


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

    # Pass or run only — exclude special teams, penalties-only, no-plays, etc.
    df = df[df["play_type"].isin(["pass", "run"])].copy()
    df = df[(df["qb_spike"] == 0) & (df["qb_kneel"] == 0) & (df["aborted_play"] == 0)].copy()
    df = df.dropna(subset=["yardline_100", "down", "ydstogo", "game_id"])
    df = df[df["down"].isin([1, 2, 3, 4])].copy()
    print(f"Pass/run plays after filtering: {len(df)}")

    df["is_pass"] = (df["play_type"] == "pass").astype(int)
    df["zone"] = zone_of(df["yardline_100"].values)
    df["bucket"] = bucket_of(df["down"].values, df["ydstogo"].values)
    df["score_differential"] = df["score_differential"].fillna(0)
    df["game_seconds_remaining"] = df["game_seconds_remaining"].fillna(1800)
    df["timeouts_pos"] = df["posteam_timeouts_remaining"].fillna(3)
    df["timeouts_def"] = df["defteam_timeouts_remaining"].fillna(3)
    df["leverage"] = df["score_differential"] * df["game_seconds_remaining"]

    qb_dna, skill_dna = load_dna()
    qb_lookup = build_dna_lookup(qb_dna, "cpoe")
    target_share_lookup = build_dna_lookup(skill_dna, "target_share")
    carry_share_lookup = build_dna_lookup(skill_dna, "carry_share")

    # Starting QB per team-season: most pass attempts.
    qb_attempts = df[df["is_pass"] == 1].groupby(["season", "posteam", "passer_player_name"]).size()
    starter_qb = qb_attempts.groupby(["season", "posteam"]).idxmax().apply(lambda x: x[2])
    # Starting RB per team-season: most rush attempts (matches game_engine.py's
    # fixed-starter-per-game convention — see module docstring).
    rb_attempts = df[df["is_pass"] == 0].groupby(["season", "posteam", "rusher_player_name"]).size()
    starter_rb = rb_attempts.groupby(["season", "posteam"]).idxmax().apply(lambda x: x[2])

    df["starter_qb"] = list(zip(df["season"], df["posteam"]))
    df["starter_qb"] = df["starter_qb"].map(starter_qb)
    df["starter_rb"] = list(zip(df["season"], df["posteam"]))
    df["starter_rb"] = df["starter_rb"].map(starter_rb)

    def lookup_col(names, zones, table, default=0.0):
        return [table.get(n, {}).get(z, default) if isinstance(n, str) else default for n, z in zip(names, zones)]

    df["cpoe_by_filter"] = lookup_col(df["starter_qb"].values, df["zone"].values, qb_lookup)
    df["target_share_by_filter"] = lookup_col(df["starter_rb"].values, df["zone"].values, target_share_lookup)
    df["carry_share_by_filter"] = lookup_col(df["starter_rb"].values, df["zone"].values, carry_share_lookup)

    out = df[["game_id", "zone", "bucket", "is_pass"] + FEATURES].copy()
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


def train_one(zone, bucket, train_df, val_df, test_df, save=True):
    tr = train_df[(train_df["zone"] == zone) & (train_df["bucket"] == bucket)]
    va = val_df[(val_df["zone"] == zone) & (val_df["bucket"] == bucket)]
    te = test_df[(test_df["zone"] == zone) & (test_df["bucket"] == bucket)]

    if len(tr) < 50 or len(va) < 10 or len(te) < 10 or tr["is_pass"].nunique() < 2:
        print(f"[{zone}_{bucket}] too little data (n_train={len(tr)}) — will fall back to primary's classifier")
        return None, None

    X_tr, y_tr = tr[FEATURES], tr["is_pass"]
    X_va, y_va = va[FEATURES], va["is_pass"]
    X_te, y_te = te[FEATURES], te["is_pass"]

    clf = xgb.XGBClassifier(
        objective="binary:logistic", n_estimators=300, max_depth=4,
        learning_rate=0.05, random_state=42, tree_method="hist",
        early_stopping_rounds=30,
    )
    clf.fit(X_tr, y_tr, eval_set=[(X_va, y_va)], verbose=False)
    preds_proba = clf.predict_proba(X_te)[:, 1]
    preds = (preds_proba >= 0.5).astype(int)
    acc = accuracy_score(y_te, preds)
    ll = log_loss(y_te, preds_proba, labels=[0, 1])
    print(f"[{zone}_{bucket}] n_train={len(tr)} n_test={len(te)}  test acc={acc:.4f} log_loss={ll:.4f}  "
          f"pred_pass_rate={preds_proba.mean():.3f} real_pass_rate={y_te.mean():.3f}")

    if save:
        # Native XGBoost JSON format, not joblib — matches the existing loader
        # in model_registry.py (`m.load_model(...)` on an `.json` file, then
        # `.get_booster()`), which only picks up `.json` files in this folder.
        clf.save_model(os.path.join(MODEL_DIR, f"{zone}_{bucket}.json"))

    return clf, {"n_train": int(len(tr)), "n_test": int(len(te)), "test_accuracy": float(acc),
                 "test_log_loss": float(ll), "pred_pass_rate": float(preds_proba.mean()),
                 "real_pass_rate": float(y_te.mean())}


def run(save=True, use_cache=True):
    df = load_and_prepare(use_cache=use_cache)
    train_df, val_df, test_df = grouped_split(df)
    print(f"Split sizes (rows): train={len(train_df)} val={len(val_df)} test={len(test_df)}")

    metrics = {}
    for zone in ZONES:
        for bucket in BUCKETS:
            _, m = train_one(zone, bucket, train_df, val_df, test_df, save=save)
            metrics[f"{zone}_{bucket}"] = m

    if save:
        # Fall back any missing zone/bucket combo to primary's classifier for
        # the same bucket, matching the missing-data fallback pattern already
        # used by the air-yards/YAC zone-split scripts.
        for zone in ZONES:
            for bucket in BUCKETS:
                key = f"{zone}_{bucket}"
                if metrics[key] is None:
                    src = os.path.join(MODEL_DIR, f"primary_{bucket}.json")
                    dst = os.path.join(MODEL_DIR, f"{key}.json")
                    if os.path.exists(src):
                        import shutil
                        shutil.copyfile(src, dst)
                        print(f"[{key}] copied primary_{bucket}'s classifier as fallback")

        metadata = {
            "version": "V.0.3.0",
            "trained_by": "train.py",
            "seasons": SEASONS,
            "split": "GroupShuffleSplit by game_id, 70/15/15 train/val/test",
            "features": FEATURES,
            "zones": ZONES,
            "buckets": BUCKETS,
            "bucket_metrics": metrics,
        }
        with open(os.path.join(MODEL_DIR, "metadata.json"), "w") as f:
            json.dump(metadata, f, indent=2)
        print(f"\nSaved metadata.json and {len(ZONES) * len(BUCKETS)} classifiers to {MODEL_DIR}")

    return metrics


if __name__ == "__main__":
    run()
