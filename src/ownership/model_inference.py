"""Live inference wrapper around the trained ownership models (see
scripts/dfs_ownership/train_ownership_model.py, artifacts in
data/dfs_ownership/_processed/models/) -- drop-in replacements for
src/ownership/heuristic.py's _compute_ownership / _compute_showdown_ownership
with the SAME mutate-in-place signature, so existing call sites in
src/api/app.py don't need to change shape, only which function they call.

Fails soft, same contract as the rest of this app's optional-data paths:
if a model file doesn't exist yet, xgboost isn't installed, or a player is
missing an input the model needs, that player (or the whole pool, if the
model itself is unavailable) falls back to the hand-tuned heuristic instead
of raising. Every player gets `ownership_source` set to "model" or
"heuristic" so it's visible, per player, which one actually produced their
number -- useful for a post-game review of how the early model performed.

Known limitation (updated 2026-09-22): still thin data (2 classic slates, 6
showdown slates as of this update -- see docs/implementation_plans/
ownership_model_v2_plan.md), shipped anyway per the same standing
instruction as 2026-09-14 ("better than what we were doing"). Re-run
train_ownership_model.py as more slates get archived; this module always
loads whatever's on disk, no code change needed to pick up a re-trained
model.
"""
from __future__ import annotations

import json
import os
from typing import Optional

import numpy as np
import pandas as pd

from src.ownership.heuristic import _compute_ownership, _compute_showdown_ownership
from src.ownership.normalize import normalize_classic_ownership, normalize_showdown_ownership
from src.ownership.prior_week import prior_week_feature

BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
MODEL_DIR = os.path.join(BASE_DIR, "data", "dfs_ownership", "_processed", "models")
SCHEDULE_CSV_PATH = os.path.join(BASE_DIR, "data", "external", "schedule_2026.csv")
YEAR = 2026  # matches the hardcoded schedule filename above -- no multi-season support yet anywhere in this module

_model_cache: dict = {}   # name -> (model, meta) or None (tried, unavailable)
_vegas_cache: dict = {}   # week -> (implied_by_team, spread_by_team)


def _load_model(name: str):
    """name in {'classic', 'showdown_flex', 'showdown_cpt'}. Cached; a
    missing/broken model is cached as None so a bad file doesn't get
    re-attempted (and re-logged) on every request."""
    if name in _model_cache:
        return _model_cache[name]
    model_path = os.path.join(MODEL_DIR, f"ownership_{name}_model.json")
    meta_path = os.path.join(MODEL_DIR, f"ownership_{name}_meta.json")
    if not (os.path.exists(model_path) and os.path.exists(meta_path)):
        _model_cache[name] = None
        return None
    try:
        import xgboost as xgb
        model = xgb.XGBRegressor()
        model.load_model(model_path)
        with open(meta_path) as f:
            meta = json.load(f)
        _model_cache[name] = (model, meta)
    except Exception as e:
        print(f"ownership model_inference: failed to load '{name}' model, falling back to heuristic: {e}")
        _model_cache[name] = None
    return _model_cache[name]


def _vegas_lookup(week: Optional[int]) -> tuple[dict, dict, dict]:
    """{team: implied_total}, {team: signed spread (+ = favored)},
    {team: that game's total_line} for a week, independent of whatever
    Vegas fields (if any) the caller already attached to individual players
    -- the request models don't always carry team_spread or game_total, and
    this needs to be right even when they don't."""
    if week in _vegas_cache:
        return _vegas_cache[week]
    implied, spread, total_by_team = {}, {}, {}
    if week is not None and os.path.exists(SCHEDULE_CSV_PATH):
        sched = pd.read_csv(SCHEDULE_CSV_PATH)
        wk = sched[(sched["week"] == week) & (sched["game_type"] == "REG")]
        for _, r in wk.iterrows():
            if pd.isna(r.get("spread_line")) or pd.isna(r.get("total_line")):
                continue
            total, sp = float(r["total_line"]), float(r["spread_line"])
            implied[r["home_team"]] = round((total + sp) / 2.0, 2)
            implied[r["away_team"]] = round((total - sp) / 2.0, 2)
            spread[r["home_team"]] = sp
            spread[r["away_team"]] = -sp
            total_by_team[r["home_team"]] = total
            total_by_team[r["away_team"]] = total
    _vegas_cache[week] = (implied, spread, total_by_team)
    return implied, spread, total_by_team


def _n_games_on_slate(week: Optional[int]) -> Optional[int]:
    if week is None or not os.path.exists(SCHEDULE_CSV_PATH):
        return None
    sched = pd.read_csv(SCHEDULE_CSV_PATH)
    return int(len(sched[(sched["week"] == week) & (sched["game_type"] == "REG")]))


def _sigmoid_pct(x: np.ndarray) -> np.ndarray:
    return 100.0 / (1.0 + np.exp(-x))


def _stakes_tier(entry_fee: Optional[float]) -> Optional[str]:
    """Mirrors scripts/dfs_ownership/standings_parser.py's stakes_tier()
    exactly (casual <=$5, sharp >=$20, else mid) -- duplicated rather than
    imported so this always-loaded live-inference module never depends on
    the scripts/ package tree (a broken import there would take the whole
    API process down with it). Keep in sync if the thresholds ever change."""
    if entry_fee is None:
        return None
    if entry_fee <= 5:
        return "casual"
    if entry_fee >= 20:
        return "sharp"
    return "mid"


def _contest_fields(contest: Optional[dict]) -> dict:
    """{field_size, stakes_tier, max_entries} for the segmentation features
    added 2026-09-22 -- None/missing for any key the caller doesn't supply,
    which XGBoost (numeric -> NaN, coerced) and the dummy-column reindex
    (categorical -> all-zero) both already treat as "no signal", so omitting
    `contest` entirely reproduces the exact pre-segmentation behavior."""
    contest = contest or {}
    return {
        "field_size": contest.get("field_size"),
        "stakes_tier": _stakes_tier(contest.get("entry_fee")),
        "max_entries": contest.get("max_entries"),
    }


def _predict(model, meta: dict, rows: list) -> np.ndarray:
    df = pd.DataFrame(rows)
    raw_cols, cat_cols = meta["raw_feature_columns"], meta["categorical_columns"]
    numeric_cols = [c for c in raw_cols if c not in cat_cols]
    # A column that's None for every row in this particular call (e.g. no
    # week given, so game_total/team_spread can't be looked up) infers as
    # pandas dtype "object", which XGBoost rejects outright -- force numeric
    # so it becomes a NaN column instead (XGBoost handles NaN natively).
    df[numeric_cols] = df[numeric_cols].apply(pd.to_numeric, errors="coerce")
    X = pd.get_dummies(df[raw_cols + cat_cols], columns=cat_cols)
    X = X.reindex(columns=meta["feature_columns"], fill_value=0)
    return _sigmoid_pct(model.predict(X))


def _dispersion_and_rank(players: list) -> tuple[dict, dict]:
    """salary_dispersion_pos (CV of salary within this pool's position
    group) and salary_rank_pctile (salary rank across the WHOLE pool) --
    both computed from the pool itself, same as at training time, so no
    external data is needed for these two."""
    df = pd.DataFrame([{"i": i, "pos": p.get("pos"), "salary": p.get("salary")} for i, p in enumerate(players)])
    df["salary_dispersion_pos"] = df.groupby("pos")["salary"].transform(
        lambda s: s.std() / s.mean() if s.mean() else 0.0).fillna(0.0)
    df["salary_rank_pctile"] = df["salary"].rank(pct=True)
    disp = dict(zip(df["i"], df["salary_dispersion_pos"]))
    rank = dict(zip(df["i"], df["salary_rank_pctile"]))
    return disp, rank


def predict_classic_ownership(players: list, week: Optional[int] = None,
                               cash_consensus: Optional[dict] = None, seed: Optional[int] = None,
                               contest: Optional[dict] = None) -> list:
    """Drop-in for _compute_ownership(players, seed=...): mutates each
    priced player, adding ownership_pct + ownership_source ("model" or
    "heuristic"). `cash_consensus` is an optional {(name, team): frac}
    map (see app.py's existing _generate_cash_consensus_lineups usage) --
    omit it and the model just sees 0 for that feature, a graceful (not
    fatal) degradation.

    `contest` (added 2026-09-22, segmentation features) is an optional
    {"field_size": float, "entry_fee": float, "max_entries": float} dict --
    the SAME numbers get predicted for two different contests that happen to
    share these characteristics, since the model never sees contest identity,
    only field_size/stakes_tier(from entry_fee)/max_entries. Omit it (or any
    key within it) for today's pre-segmentation behavior: this stays the
    single "one set of numbers per week" call every existing caller makes,
    unchanged -- see _contest_fields().

    Falls back to the heuristic for the WHOLE pool if no classic model is on
    disk yet; a player missing dk_pcts_all (so no real median/ceiling) still
    gets a model prediction, just with projection standing in for both.
    """
    loaded = _load_model("classic")
    if loaded is None:
        _compute_ownership(players, seed=seed)
        for p in players:
            p.setdefault("ownership_source", "heuristic")
        return players

    model, meta = loaded
    implied_by_team, spread_by_team, total_by_team = _vegas_lookup(week)
    n_games = _n_games_on_slate(week)
    cash_consensus = cash_consensus or {}
    contest_fields = _contest_fields(contest)
    disp, rank = _dispersion_and_rank(players)

    # "Locked" here means the SAME thing it means to the heuristic (and to
    # to_score's own filter below): any value already non-None, whether a
    # genuine hand override or an upstream-prefilled prior -- left alone by
    # the scoring loop, so it must also be left alone by the renorm below.
    locked = {i for i, p in enumerate(players)
              if p.get("salary") is not None and p.get("ownership_pct") is not None}
    to_score = [(i, p) for i, p in enumerate(players)
                if p.get("salary") is not None and p.get("ownership_pct") is None]
    rows = []
    for i, p in to_score:
        pcts = p.get("dk_pcts_all")
        median = pcts[50] if pcts else p.get("projection")
        ceiling = pcts[95] if pcts else p.get("projection")
        rows.append({
            "salary": p["salary"], "projection_median": median, "projection_ceiling": ceiling,
            "game_total": total_by_team.get(p.get("team")),
            "team_implied_total": p.get("implied_total") or implied_by_team.get(p.get("team")),
            "team_spread": spread_by_team.get(p.get("team")),
            "n_games_on_slate": n_games,
            "prior_week_score": prior_week_feature(p.get("name"), p.get("team"), YEAR, week),
            # Prefer a value already on the player dict (e.g. the bulk
            # weekly-sim bootstrap already attaches this per-player) over the
            # external map, so a caller with it embedded doesn't also have
            # to build and pass the lookup separately.
            "cash_consensus_frac": (p.get("cash_consensus_frac") if p.get("cash_consensus_frac") is not None
                                     else cash_consensus.get((p.get("name"), p.get("team")), 0.0)),
            "salary_dispersion_pos": disp.get(i, 0.0), "salary_rank_pctile": rank.get(i, 0.5),
            "pos": p.get("pos"), **contest_fields,
        })
    if rows:
        preds = _predict(model, meta, rows)
        for (i, p), val in zip(to_score, preds):
            p["ownership_pct"] = round(float(val), 1)
            p["ownership_source"] = "model"
    for p in players:
        # A value that arrived already set (skipped above, same "leave a
        # manual override alone" convention the heuristic itself follows)
        # could be a genuine hand-typed override OR a value computed
        # upstream by this same model at an earlier stage of the pipeline
        # (e.g. the classic bootstrap prior) -- "prefilled" rather than
        # asserting a specific (possibly wrong) provenance.
        p.setdefault("ownership_source", "prefilled" if p.get("ownership_pct") is not None else None)

    # Each player's sigmoid prediction above is independent -- no sum
    # constraint at all -- so renormalize to the real DK roster-math
    # targets, same treatment as the heuristic path.
    normalize_classic_ownership(players, locked)
    return players


def predict_showdown_ownership(players: list, week: Optional[int] = None, seed: Optional[int] = None,
                                contest: Optional[dict] = None) -> list:
    """Drop-in for _compute_showdown_ownership(players, seed=...): mutates
    each priced player, adding ownership_pct (FLEX) + cpt_ownership_pct +
    ownership_source. Falls back to the heuristic (for BOTH targets
    together) if either the flex or CPT model is missing, so a pool never
    ends up with one target from the model and the other from the
    heuristic -- inconsistent premises would make them hard to compare.

    `contest` -- see predict_classic_ownership's docstring; same
    {"field_size", "entry_fee", "max_entries"} shape, same graceful-omit
    behavior via _contest_fields()."""
    flex_loaded, cpt_loaded = _load_model("showdown_flex"), _load_model("showdown_cpt")
    if flex_loaded is None or cpt_loaded is None:
        _compute_showdown_ownership(players, seed=seed)
        for p in players:
            p.setdefault("ownership_source", "heuristic")
        return players

    flex_model, flex_meta = flex_loaded
    cpt_model, cpt_meta = cpt_loaded
    implied_by_team, spread_by_team, total_by_team = _vegas_lookup(week)
    contest_fields = _contest_fields(contest)
    disp, rank = _dispersion_and_rank(players)

    # Captured before any computation -- same "leave a pre-existing value
    # alone, whatever its provenance" convention as predict_classic_ownership.
    locked_flex = {i for i, p in enumerate(players) if p.get("salary") and p.get("ownership_pct") is not None}
    locked_cpt = {i for i, p in enumerate(players) if p.get("salary") and p.get("cpt_ownership_pct") is not None}

    to_score = [(i, p) for i, p in enumerate(players) if p.get("salary")]
    rows = []
    for i, p in to_score:
        pcts = p.get("dk_pcts_all")
        median = pcts[50] if pcts else p.get("projection")
        ceiling = pcts[95] if pcts else p.get("projection")
        rows.append({
            "salary": p["salary"], "cpt_salary": p.get("cpt_salary") or round(p["salary"] * 1.5),
            "projection_median": median, "projection_ceiling": ceiling,
            "game_total": total_by_team.get(p.get("team")),
            "team_implied_total": p.get("implied_total") or implied_by_team.get(p.get("team")),
            "team_spread": spread_by_team.get(p.get("team")),
            "optimal_cpt_pct": p.get("optimal_cpt_pct") or 0.0, "optimal_flex_pct": p.get("optimal_flex_pct") or 0.0,
            "prior_week_score": prior_week_feature(p.get("name"), p.get("team"), YEAR, week),
            "salary_dispersion_pos": disp.get(i, 0.0), "salary_rank_pctile": rank.get(i, 0.5),
            "pos": p.get("pos"), **contest_fields,
        })
    if rows:
        flex_preds = _predict(flex_model, flex_meta, rows)
        cpt_preds = _predict(cpt_model, cpt_meta, rows)
        for (i, p), fv, cv in zip(to_score, flex_preds, cpt_preds):
            if p.get("ownership_pct") is None:
                p["ownership_pct"] = round(float(min(62.0, max(0.5, fv))), 1)
            if p.get("cpt_ownership_pct") is None:
                p["cpt_ownership_pct"] = round(float(min(45.0, max(0.3, cv))), 1)
            p["ownership_source"] = "model"
    for p in players:
        # A value that arrived already set (skipped above, same "leave a
        # manual override alone" convention the heuristic itself follows)
        # could be a genuine hand-typed override OR a value computed
        # upstream by this same model at an earlier stage of the pipeline
        # (e.g. the classic bootstrap prior) -- "prefilled" rather than
        # asserting a specific (possibly wrong) provenance.
        p.setdefault("ownership_source", "prefilled" if p.get("ownership_pct") is not None else None)

    # Each player's sigmoid predictions above are independent -- no sum
    # constraint at all -- so renormalize to the real DK roster-math
    # targets, same treatment as the heuristic path.
    normalize_showdown_ownership(players, locked_flex, locked_cpt)
    return players
