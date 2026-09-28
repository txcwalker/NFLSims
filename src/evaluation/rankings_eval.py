"""Grade our de facto weekly positional rankings (the Slate Leaders page).

Roadmap: Evaluation tab, "Rankings" section (2026-09-26). Season-long scoring,
exactly the Slate Leaders formula (frontend/src/pages/SlateLeaders.jsx
calculateMeanScore / app.py's rank-probability block):
    pYds*0.04 + pTD*{4|6} - int*2 + rYds*0.1 + rTD*6 + rec*{1|0.5|0}
    + recYds*0.1 + recTD*6 - fumbles*1          (no 100/300-yard bonuses)
Formats: 4_ppr / 4_half / 4_std / 6_ppr / 6_half / 6_std (6-pt passing TD
only moves QBs).

What gets graded, per completed week x format x position:
  - Ranking, by BOTH bases (Cam, 2026-09-26: grade both, show which ranks
    better -- decides the page's default sort): our projected MEAN and our
    projected MEDIAN of the player's sim runs.
      * rank correlation (Spearman) between our order and the actual finish
        order, inside a relevance pool (our top POOL[pos]) -- otherwise
        hundreds of backups projected ~0 who score ~0 inflate it for free
      * mean |our rank - actual rank| by tier (1-12, 13-24, 25+)
      * start/sit hit rate: of our top-N, the share who finished top-N
  - The top-12 / #1 finish probabilities the page shows (basis-free):
    Brier score + reliability bins (did our "60% top-12" guys hit ~60%?)
  - Biggest rank misses and repeat offenders.

Rules (same spirit as player_proj_eval.py):
  - Actual positional finish is ranked among EVERYONE at the position that
    week (incl. players we never projected) -- a surprise backup really does
    push our guys down, as in a real league.
  - A projected player with no stat line (inactive etc.) is listed, not
    graded -- roster-status misses stay separate. Projected ranks are kept as
    published (the absent player still occupied his slot on the page).
  - Only COMPLETED weeks (every REG game of the week has stat lines) count;
    a half-played week would make top-12 meaningless.
  - QB/RB/WR/TE only (DST/K later).
"""
import os

import numpy as np
import pandas as pd

from src.evaluation.line_history import BASE_DIR
from src.evaluation.player_actuals import load_player_actuals
from src.evaluation.player_proj_eval import match_to_actuals, roster_id_map

FORMATS = {"4_ppr": (4.0, 1.0), "4_half": (4.0, 0.5), "4_std": (4.0, 0.0),
           "6_ppr": (6.0, 1.0), "6_half": (6.0, 0.5), "6_std": (6.0, 0.0)}
FORMAT_LABELS = {"4_ppr": "PPR", "4_half": "Half-PPR", "4_std": "Standard",
                 "6_ppr": "PPR · 6-pt pass TD", "6_half": "Half-PPR · 6-pt pass TD", "6_std": "Standard · 6-pt pass TD"}
GRADED_POS = ("QB", "RB", "WR", "TE")
BASES = ("mean", "median")
POOL = {"QB": 24, "RB": 48, "WR": 72, "TE": 24}          # relevance pool by OUR rank
HIT_TIERS = {"QB": [12], "RB": [12, 24], "WR": [12, 24, 36], "TE": [12]}
SCORE_COLS = ["pYds", "pTD", "int", "rYds", "rTD", "rec", "recYds", "recTD", "fumbles"]
PROB_BINS = [0.0, 0.05, 0.25, 0.5, 0.75, 1.0001]


def season_score(df, fmt):
    """Season-long fantasy points for every row of `df` (sim iterations or
    real stat lines -- same column names). Inputs: df with SCORE_COLS
    (missing column or blank cell -> 0, e.g. a QB row has no rec column in a
    mixed QB+skill table), fmt (FORMATS key). Output: float Series."""
    pass_td, ppr = FORMATS[fmt]
    c = {k: (df[k].astype(float).fillna(0.0) if k in df else 0.0) for k in SCORE_COLS}
    return (c["pYds"] * 0.04 + c["pTD"] * pass_td - c["int"] * 2.0 + c["rYds"] * 0.1 + c["rTD"] * 6.0
            + c["rec"] * ppr + c["recYds"] * 0.1 + c["recTD"] * 6.0 - c["fumbles"] * 1.0)


def finish_probs(keys, iters, scores):
    """P(top-12) and P(#1) at the position, vectorized.

    Inputs: keys (array of player keys, one per sim row), iters (iteration
    index per row), scores (format score per row) -- all rows ONE position.
    Output: DataFrame indexed by key with p_top12, p_top1.
    Same definition as app.py's week_projections: rank within (position,
    iteration) with method='min'; rank <= 12  <=>  score >= the 12th-largest
    score of that iteration; rank == 1  <=>  score == that iteration's max.
    """
    kcat = pd.Categorical(keys)
    icat = pd.Categorical(iters)
    m = np.full((len(icat.categories), len(kcat.categories)), -np.inf)
    m[icat.codes, kcat.codes] = scores
    k = min(12, m.shape[1])
    kth = -np.partition(-m, k - 1, axis=1)[:, k - 1:k]                  # 12th largest per iteration
    top12 = (m >= kth).mean(axis=0)
    top1 = (m >= m.max(axis=1, keepdims=True)).mean(axis=0)
    return pd.DataFrame({"p_top12": top12, "p_top1": top1}, index=kcat.categories)


def project_week(sim, fmt):
    """Our published ranking for one week + format.
    Inputs: sim (players parquet rows for the week), fmt.
    Output: DataFrame per (game_id, Team, Player, Pos): proj_mean, proj_median,
    rank_mean, rank_median (1 = best, method='min', within Pos), p_top12, p_top1."""
    sim = sim[sim["Pos"].isin(GRADED_POS)].copy()
    sim["_s"] = season_score(sim, fmt).to_numpy()
    sim["_k"] = sim["game_id"] + "|" + sim["Team"] + "|" + sim["Player"]
    out = []
    for pos, g in sim.groupby("Pos"):
        agg = g.groupby("_k").agg(game_id=("game_id", "first"), Team=("Team", "first"),
                                  Player=("Player", "first"), proj_mean=("_s", "mean"), proj_median=("_s", "median"))
        agg = agg.join(finish_probs(g["_k"].to_numpy(), g["iteration"].to_numpy(), g["_s"].to_numpy()))
        agg["Pos"] = pos
        for b in BASES:
            agg[f"rank_{b}"] = agg[f"proj_{b}"].rank(ascending=False, method="min").astype(int)
        out.append(agg.reset_index(drop=True))
    return pd.concat(out, ignore_index=True)


def actual_ranks(act_week, fmt):
    """Actual positional finish for one week: every real QB/RB/WR/TE line.
    Output: act rows + actual_score, actual_rank (method='min' within position)."""
    a = act_week[act_week["position"].isin(GRADED_POS)].copy()
    a["actual_score"] = season_score(a, fmt)
    a["actual_rank"] = a.groupby("position")["actual_score"].rank(ascending=False, method="min").astype(int)
    return a


def completed_weeks(year, actuals, base_dir=BASE_DIR):
    """Weeks whose every REG game has real stat lines. Output: sorted list."""
    sched = pd.read_csv(os.path.join(base_dir, "data", "external", f"schedule_{year}.csv"))
    sched = sched[sched["game_type"] == "REG"]
    have = set(actuals["game_id"]) if len(actuals) else set()
    return sorted(int(w) for w, g in sched.groupby("week") if set(g["game_id"]) <= have)


# ── Per-week disk cache ──────────────────────────────────────────────────────
# A completed week is frozen (kickoff lock + final stats), but grading it costs
# ~30s (6 formats x 10K sims x ~400 players). Cache each week's graded rows in
# data/eval/{year}/_cache/ (gitignored), keyed on the week's sim-file mtime +
# a content hash of that week's actual stat lines -- a resim or a stat
# correction invalidates just that week.

def _cache_dir(year, base_dir):
    return os.path.join(base_dir, "data", "eval", str(year), "_cache")


def _week_token(sim_path, act_week):
    """Inputs: sim parquet path, the week's actual rows. Output: str cache key."""
    h = int(pd.util.hash_pandas_object(act_week.sort_values(["game_id", "player_id"]), index=False).sum())
    return f"{os.path.getmtime(sim_path):.3f}|{h}|v1"


def _read_week_cache(year, wk, token, base_dir):
    d = _cache_dir(year, base_dir)
    try:
        if open(os.path.join(d, f"rankings_w{wk}.token"), encoding="utf-8").read() != token:
            return None
        return (pd.read_parquet(os.path.join(d, f"rankings_w{wk}_rows.parquet")),
                pd.read_parquet(os.path.join(d, f"rankings_w{wk}_absent.parquet")))
    except OSError:
        return None


def _write_week_cache(year, wk, token, rows, absent, base_dir):
    d = _cache_dir(year, base_dir)
    os.makedirs(d, exist_ok=True)
    rows.to_parquet(os.path.join(d, f"rankings_w{wk}_rows.parquet"), index=False)
    absent.to_parquet(os.path.join(d, f"rankings_w{wk}_absent.parquet"), index=False)
    with open(os.path.join(d, f"rankings_w{wk}.token"), "w", encoding="utf-8") as f:   # token last = commit marker
        f.write(token)


def build_rankings_eval(year=2026, base_dir=BASE_DIR, actuals=None, sims_by_week=None, weeks=None):
    """Per player-week-format ranking rows for every completed week.

    Inputs: year, base_dir; actuals / sims_by_week / weeks overrides (tests).
    Output: (rows, absent)
      rows   -- one row per projected QB/RB/WR/TE per week per format, played
                ones only: week, fmt, Player, Team, Pos, proj_mean, proj_median,
                rank_mean, rank_median, p_top12, p_top1, actual_score,
                actual_rank, matched_by
      absent -- projected inside the relevance pool (by either basis) but no
                stat line: week, fmt, Player, Team, Pos, rank_mean, rank_median
    """
    if actuals is None:
        actuals = load_player_actuals(year, base_dir)
    if weeks is None:
        weeks = completed_weeks(year, actuals, base_dir)
    idmap = roster_id_map(year, base_dir)
    cols = ["game_id", "Team", "Player", "Pos", "iteration"] + SCORE_COLS
    rows, absent = [], []
    for wk in weeks:
        act_week = actuals[actuals["week"] == wk]
        sim_path = os.path.join(base_dir, "data", "interim", f"dfs_week_{wk}_players.parquet")
        use_cache = sims_by_week is None
        if use_cache:
            if not os.path.exists(sim_path):
                continue
            token = _week_token(sim_path, act_week)
            hit = _read_week_cache(year, wk, token, base_dir)
            if hit is not None:
                rows.append(hit[0]); absent.append(hit[1])
                continue
            sim = pd.read_parquet(sim_path, columns=cols)
        else:
            sim = sims_by_week.get(wk)
        if sim is None or sim.empty:
            continue
        wk_rows, wk_absent = [], []
        for fmt in FORMATS:
            proj = project_week(sim, fmt)
            proj["player_id"] = [idmap.get((t, n)) for t, n in zip(proj["Team"], proj["Player"])]
            act = actual_ranks(act_week, fmt)
            matched, how = match_to_actuals(proj, act)
            proj["matched_by"] = how
            proj["actual_score"] = [r["actual_score"] if r is not None else np.nan for r in matched]
            proj["actual_rank"] = [r["actual_rank"] if r is not None else np.nan for r in matched]
            proj["week"], proj["fmt"] = wk, fmt
            played = proj["actual_rank"].notna()
            wk_rows.append(proj[played].drop(columns=["player_id"]))
            in_pool = (proj["rank_mean"] <= proj["Pos"].map(POOL)) | (proj["rank_median"] <= proj["Pos"].map(POOL))
            wk_absent.append(proj[~played & in_pool][["week", "fmt", "Player", "Team", "Pos", "rank_mean", "rank_median"]])
        wr, wa = pd.concat(wk_rows, ignore_index=True), pd.concat(wk_absent, ignore_index=True)
        if use_cache:
            _write_week_cache(year, wk, token, wr, wa, base_dir)
        rows.append(wr); absent.append(wa)
    cat = lambda xs: pd.concat(xs, ignore_index=True) if xs else pd.DataFrame()  # noqa: E731
    out = cat(rows)
    if len(out):
        out["actual_rank"] = out["actual_rank"].astype(int)
    return out, cat(absent)


# ── Aggregates ───────────────────────────────────────────────────────────────

def _r(v, d=4):
    return None if v is None or (isinstance(v, float) and np.isnan(v)) else round(float(v), d)


def _tier(rank):
    return "1-12" if rank <= 12 else "13-24" if rank <= 24 else "25+"


def basis_block(df, pos, basis):
    """Ranking accuracy for one position + basis over the given rows (any
    number of weeks). Spearman is computed per week inside the pool, then
    averaged weighted by pool size (ranks aren't comparable across weeks).
    Output: {spearman, n, tiers: {tier: mean_abs_rank_err}, hits: {N: {hit, n}}}"""
    rk = f"rank_{basis}"
    d = df[df["Pos"] == pos]
    pool = d[d[rk] <= POOL[pos]]
    cors, ws = [], []
    for _, g in pool.groupby("week"):
        if len(g) >= 3 and g[rk].nunique() > 1 and g["actual_rank"].nunique() > 1:
            cors.append(g[rk].corr(g["actual_rank"], method="spearman"))
            ws.append(len(g))
    err = (pool[rk] - pool["actual_rank"]).abs()
    tiers = {t: _r(err[pool[rk].map(_tier) == t].mean(), 2)
             for t in ("1-12", "13-24", "25+") if (pool[rk].map(_tier) == t).any()}
    hits = {}
    for n in HIT_TIERS[pos]:
        top = d[d[rk] <= n]
        hits[str(n)] = {"hit": _r((top["actual_rank"] <= n).mean()) if len(top) else None, "n": int(len(top))}
    return {"spearman": _r(np.average(cors, weights=ws)) if cors else None, "n": int(len(pool)),
            "mae_rank": _r(err.mean(), 2) if len(err) else None, "tiers": tiers, "hits": hits}


def prob_block(df, pos=None):
    """Top-12 / #1 probability calibration (basis-free) over the given rows."""
    d = df if pos is None else df[df["Pos"] == pos]
    if d.empty:
        return None
    hit12 = (d["actual_rank"] <= 12).astype(float)
    hit1 = (d["actual_rank"] == 1).astype(float)
    bins = []
    for lo, hi in zip(PROB_BINS[:-1], PROB_BINS[1:]):
        m = (d["p_top12"] >= lo) & (d["p_top12"] < hi)
        bins.append({"lo": lo, "hi": min(hi, 1.0), "n": int(m.sum()),
                     "mean_pred": _r(d.loc[m, "p_top12"].mean()) if m.any() else None,
                     "hit_rate": _r(hit12[m].mean()) if m.any() else None})
    # Skill vs. a naive forecast that gives every projected player the same
    # base-rate chance. (Deliberately NOT "expected vs. actual top-12 count":
    # our probabilities sum to ~12 per position per week by construction --
    # every sim run has exactly 12 top-12 finishers -- so that comparison
    # always looks perfect and says nothing.)
    brier = ((d["p_top12"] - hit12) ** 2).mean()
    naive = ((hit12.mean() - hit12) ** 2).mean()
    return {"n": int(len(d)), "brier_top12": _r(brier),
            "brier_top12_naive": _r(naive),
            "skill_top12": _r(1 - brier / naive) if naive > 0 else None,
            "brier_top1": _r(((d["p_top1"] - hit1) ** 2).mean()), "bins": bins}


def summarize_rankings(rows, fmt):
    """Everything the UI needs for one format. Output: JSON-able dict:
      weeks, by_pos: {pos: {mean: basis_block, median: basis_block, prob}},
      by_week: [{week, pos, mean/median spearman + top-12 hit}],
      winner: {pos: 'mean'|'median'|'tie'} -- which basis ranked better
        (Spearman first, top-12 hit rate as tiebreak),
      misses: biggest single-week rank misses (by mean basis, in pool),
      repeat: players in pool 2+ weeks with average (our rank - actual rank)."""
    df = rows[rows["fmt"] == fmt] if len(rows) else rows
    if df is None or df.empty:
        return {"weeks": [], "by_pos": {}}
    out = {"weeks": sorted(int(w) for w in df["week"].unique()), "by_pos": {}, "winner": {}, "by_week": []}
    for pos in GRADED_POS:
        if not (df["Pos"] == pos).any():
            continue
        blk = {b: basis_block(df, pos, b) for b in BASES}
        blk["prob"] = prob_block(df, pos)
        out["by_pos"][pos] = blk
        sm, sd = blk["mean"]["spearman"], blk["median"]["spearman"]
        if sm is None or sd is None:
            out["winner"][pos] = None
        elif abs(sm - sd) >= 0.005:
            out["winner"][pos] = "mean" if sm > sd else "median"
        else:
            hm, hd = blk["mean"]["hits"]["12"]["hit"], blk["median"]["hits"]["12"]["hit"]
            out["winner"][pos] = "tie" if hm == hd else ("mean" if (hm or 0) > (hd or 0) else "median")
        for wk in out["weeks"]:
            w = df[df["week"] == wk]
            out["by_week"].append({"week": wk, "pos": pos,
                                   **{f"{b}_{k}": v for b in BASES
                                      for k, v in (("spearman", basis_block(w, pos, b)["spearman"]),
                                                   ("hit12", basis_block(w, pos, b)["hits"]["12"]["hit"]))}})
    out["prob"] = prob_block(df)
    pool = df[df["rank_mean"] <= df["Pos"].map(POOL)].copy()
    pool["diff"] = pool["rank_mean"] - pool["actual_rank"]            # + = finished BETTER than we ranked
    keep = ["week", "Player", "Team", "Pos", "rank_mean", "rank_median", "actual_rank", "proj_mean", "actual_score", "diff"]
    out["misses"] = {
        "underrated": pool.sort_values("diff", ascending=False).head(12)[keep].to_dict(orient="records"),
        "overrated": pool.sort_values("diff").head(12)[keep].to_dict(orient="records"),
    }
    rep = (pool.groupby(["Player", "Team", "Pos"])
               .agg(weeks=("week", "nunique"), avg_rank=("rank_mean", "mean"),
                    avg_finish=("actual_rank", "mean"), avg_diff=("diff", "mean"))
               .reset_index())
    rep = rep[rep["weeks"] >= 2].sort_values("avg_diff", ascending=False)
    out["repeat"] = [{k: (_r(v, 2) if isinstance(v, (float, np.floating)) else v) for k, v in r.items()}
                     for r in rep.to_dict(orient="records")]
    return out
