"""Backtest "smash rate" (hitting a salary-multiple pace) against REAL
settled DK Classic contests.

Why (2026-09-23, Cam): the idea is to build around players whose sims give
them real odds of hitting tournament-winning pace (~4.5-5x salary/1000
points), since 200-250 total points on a $50K cap = 4-5x. Cam's worry is
that sim-based smash odds are too optimistic. This script answers that with
real results, in three steps:

  1. CALIBRATION -- for every priced player our pre-lock week sim covers,
     P(sim score >= k x salary/1000) vs. whether he actually did, binned by
     predicted probability and by salary tier, for k = 4.5 and 5.0.
  2. BUILD -- 150-lineup Classic portfolios per strategy via /api/optimize
     (in-process TestClient), Optimizer-page default settings:
       gaussian   current default (gpp_projection ceiling blend, hand-set corr)
       sim_corr   same, sim-measured correlations
       smash45 / smash50
                  gpp_projection replaced by pace + E[max(0, X - pace)] from
                  the sims (pace = k x salary/1000): the expected points ABOVE
                  pace. The pace term only keeps the objective in points
                  units for the width x gaussian perturbation -- with the cap
                  (nearly) binding, sum(pace) is ~constant, so the solver is
                  effectively maximising summed smash surplus. No API change
                  needed: it's just a different gpp_projection.
  3. SCORE -- each lineup's actual score from the contest CSVs' own FPTS
     (the same numbers the field's Points were computed from, so our lineup
     is placed consistently), then its rank in every real settled contest
     that week: finish percentile, top-20% ("cash-ish" -- DK GPPs pay
     ~20-25%), top-1%, top-0.1%. Cam's own paper entries are scored the same
     way as a reference row.

Inputs (per week): data/interim/dfs_week_{w}_players.parquet (pre-lock
sim), data/dfs_ownership/2026/week_{ww}/main_slate/salaries_prelock.csv +
the settled standings CSVs + paper_entries.json.
Output: printed tables + docs/eda_outputs/smash_backtest/week_{w}.json

CAVEATS (read before trusting any number here): two slates is a tiny
sample -- one slate-breaking game can decide a whole week; the contests
within a week share the same player outcomes, so they are NOT independent
samples, only different field sizes/sharpness. Re-run as weeks accumulate.

Usage:
    venv\\Scripts\\python.exe scripts/eda/backtest_smash_rate.py <week> [<week> ...] [--lineups 150] [--reps 2]
"""
import argparse
import glob
import json
import os
import sys
import time

sys.path.insert(0, os.getcwd())

import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from fastapi.testclient import TestClient  # noqa: E402

import src.api.app as api  # noqa: E402
from src.scrapers.dk_scraper import normalize_player_name as N  # noqa: E402
from scripts.dfs_ownership.standings_parser import (  # noqa: E402
    parse_filename, read_standings, actual_scores_from_summary, player_key,
)

YEAR = 2026
MULTIPLES = (4.5, 5.0)
GPP_WEIGHTS = {25: 0.05, 50: 0.20, 75: 0.35, 95: 0.40}   # Optimizer.jsx top_heavy
SETTINGS = {
    "contest_type": "top_heavy", "contest_size": 11000, "total_entries": 11000,
    "entry_fee": 18, "paying_positions": 2200, "min_unique_players": 2,
    "include_dst_in_unique": False,
    "max_exposure_by_pos": {"QB": 0.4, "RB": 0.4, "WR": 0.4, "TE": 0.4, "DST": 0.4},
}
ALIASES = json.load(open(os.path.join("data", "dna", "dk_name_aliases.json"), encoding="utf-8"))


def dk_key(name, team, pos):
    """player_key after mapping an internal/sim name to its DK spelling via
    the live site's persisted alias file (e.g. "Josh Palmer" -> "Joshua
    Palmer") -- the backtest must see the same pool the real optimizer did."""
    dk = ALIASES.get(f"{N(name)}|{team}", {}).get("dk_name")
    return player_key(dk or name, team, pos, N)


PROB_BINS = [0, 0.01, 0.03, 0.06, 0.10, 0.15, 0.25, 1.0]
SAL_BINS = [0, 4000, 5500, 7000, 20000]
SAL_LABELS = ["<4K", "4-5.4K", "5.5-6.9K", "7K+"]


def size_bucket(field):
    """Cam's field-size classes (2026-09-24): small <= 2,000 entries,
    large >= 5,000, medium in between (plays closer to large than small).
    Provisional -- revisit with real research on where GPP dynamics shift."""
    return "small" if field <= 2000 else ("large" if field >= 5000 else "medium")


def slate_dir(week):
    return os.path.join("data", "dfs_ownership", str(YEAR), f"week_{week:02d}", "main_slate")


def load_week(week):
    """Inputs: week (int). Outputs: dict with
        sim     {key: np.ndarray of per-iteration dk_score}
        meta    {key: {name, team, pos}} (the SIM's own name/pos, which is
                what /api/optimize's trial-score lookup keys on)
        salary  {key: int}
        actual  {key: float} (union of every contest CSV's FPTS)
        contests [(meta, field_points ndarray)]
    `key` is standings_parser.player_key -- ('__dst__', TEAM) for defenses,
    (normalized_name, None) for everyone else -- the one scheme all three
    sources (sim, salary file, DK standings) can be joined on."""
    sim_df = pd.read_parquet(f"data/interim/dfs_week_{week}_players.parquet",
                             columns=["Player", "Team", "Pos", "iteration", "dk_score"])
    sim, meta = {}, {}
    for (p, t, pos), g in sim_df.groupby(["Player", "Team", "Pos"]):
        k = dk_key(p, t, pos)   # alias-aware: without it "Josh Palmer" never met DK's "Joshua Palmer"
        arr = np.zeros(int(sim_df["iteration"].max()) + 1)
        arr[g["iteration"].values] = g["dk_score"].values
        sim[k], meta[k] = arr, {"name": p, "team": t, "pos": pos}

    sal = pd.read_csv(os.path.join(slate_dir(week), "salaries_prelock.csv"))
    sal = sal.sort_values("snapshot_ts").drop_duplicates(["name", "team", "pos"], keep="last")
    salary = {player_key(r.name, r.team, r.pos, N): int(r.salary) for r in sal.itertuples()}

    actual, contests = {}, []
    for f in sorted(glob.glob(os.path.join(slate_dir(week), "*max.csv"))):
        m = parse_filename(os.path.splitext(os.path.basename(f))[0])
        entries, summ = read_standings(f)
        for k, v in actual_scores_from_summary(summ, N).items():
            actual[k] = max(actual.get(k, v), v)
        pts = np.sort(entries["points"].dropna().values)
        contests.append(({**m, "field_size": len(pts)}, pts))
    return {"sim": sim, "meta": meta, "salary": salary, "actual": actual, "contests": contests}


# ── Step 1: calibration ──────────────────────────────────────────────────
def calibration(d, k_mult):
    """Inputs: d (load_week output), k_mult (pace multiple).
    Output: (per-player DataFrame, by-probability-bin table, by-salary table).
    Only players in all three sources (priced, simmed, scored) -- anyone we
    simmed as out isn't a smash candidate we'd have built with."""
    rows = []
    for k, arr in d["sim"].items():
        if k not in d["salary"] or k not in d["actual"]:
            continue
        pace = k_mult * d["salary"][k] / 1000.0
        rows.append({"key": str(k), "pos": d["meta"][k]["pos"], "salary": d["salary"][k],
                     "p_hit": float((arr >= pace).mean()), "hit": d["actual"][k] >= pace,
                     "sim_mean": float(arr.mean()), "actual": d["actual"][k]})
    df = pd.DataFrame(rows)
    df["p_bin"] = pd.cut(df["p_hit"], PROB_BINS, include_lowest=True)
    df["sal_bin"] = pd.cut(df["salary"], SAL_BINS, labels=SAL_LABELS, right=False)
    agg = lambda g: pd.Series({"n": len(g), "predicted": g["p_hit"].mean(),  # noqa: E731
                               "actual": g["hit"].mean(), "exp_hits": g["p_hit"].sum(),
                               "real_hits": int(g["hit"].sum())})
    return (df, df.groupby("p_bin", observed=True).apply(agg),
            df.groupby("sal_bin", observed=True).apply(agg))


# ── Step 2: build ────────────────────────────────────────────────────────
def build_pool(d, strategy):
    """Inputs: d, strategy name. Output: /api/optimize players payload.
    projection = sim p50 (what the Optimizer sends); gpp_projection = the
    top_heavy percentile blend, or for smashXX the pace + expected surplus."""
    pool = []
    for k, arr in d["sim"].items():
        if k not in d["salary"]:
            continue
        m, sal = d["meta"][k], d["salary"][k]
        pcts = np.percentile(arr, np.arange(101)).round(2).tolist()
        if strategy.startswith("smash"):
            pace = int(strategy[5:]) / 10.0 * sal / 1000.0
            gpp = pace + float(np.maximum(0.0, arr - pace).mean())
        else:
            gpp = sum(w * pcts[q] for q, w in GPP_WEIGHTS.items())
        pool.append({"name": m["name"], "team": m["team"], "pos": m["pos"], "salary": sal,
                     "projection": pcts[50], "gpp_projection": round(gpp, 2), "dk_pcts_all": pcts})
    return pool


def lineup_key_list(lu):
    return [dk_key(p["name"], p["team"], p["pos"]) for p in lu["players"]]


# ── Step 3: score vs. real fields ────────────────────────────────────────
def place(score, pts):
    """Inputs: our score, the contest's sorted field points. Output: finish
    percentile (0 = won, 1 = last) -- share of the field that beat us."""
    beat_us = len(pts) - np.searchsorted(pts, score, side="right")
    return beat_us / len(pts)


def score_lineups(keys_list, d):
    """Actual score per lineup (missing players count 0 and are reported)."""
    scores, missing = [], 0
    for keys in keys_list:
        s = 0.0
        for k in keys:
            if k in d["actual"]:
                s += d["actual"][k]
            else:
                missing += 1
        scores.append(s)
    return np.array(scores), missing


def contest_metrics(scores, contests):
    out = {}
    for meta, pts in contests:
        pct = np.array([place(s, pts) for s in scores])
        out[meta["contest_name"]] = {
            "field": meta["field_size"], "fee": meta["entry_fee"],
            "mean_pctile": float(pct.mean()), "top20": float((pct <= 0.20).mean()),
            "top1": float((pct <= 0.01).mean()), "top01": float((pct <= 0.001).mean()),
            "best_rank": int(round(pct.min() * meta["field_size"])) + 1,
        }
    return out


def paper_keys(week):
    path = os.path.join(slate_dir(week), "paper_entries.json")
    if not os.path.exists(path):
        return []
    ents = [e for e in json.load(open(path)).get("entries", []) if e.get("slate_format") == "classic"]
    return [[dk_key(p["name"], p["team"], p["pos"]) for p in e["players"]] for e in ents]


def run_week(client, week, n_lineups, reps):
    print(f"\n{'=' * 70}\nWEEK {week}\n{'=' * 70}")
    d = load_week(week)
    print("contests: " + ", ".join(f"{m['contest_name']} ({m['field_size']:,})" for m, _ in d["contests"]))

    # Side check: priced players our pre-lock sim had OUT who actually scored.
    outs = sorted(((d["actual"][k], k[0] if k[0] != "__dst__" else k[1]) for k in d["salary"]
                   if k in d["actual"] and k not in d["sim"] and d["actual"][k] >= 5), reverse=True)
    print(f"priced players we simmed OUT who scored >= 5 DK pts: {len(outs)} "
          + ", ".join(f"{n} {v:.1f}" for v, n in outs[:10]))

    result = {"week": week, "calibration": {}, "strategies": {}}
    for km in MULTIPLES:
        df, by_p, by_sal = calibration(d, km)
        tot = df["p_hit"].sum(), int(df["hit"].sum())
        print(f"\n-- Calibration @ {km}x salary: {len(df)} players, expected hits {tot[0]:.1f}, "
              f"real hits {tot[1]} (ratio real/expected {tot[1] / max(tot[0], 1e-9):.2f})")
        print(by_p.round(3).to_string())
        print(by_sal.round(3).to_string())
        result["calibration"][str(km)] = {
            "n": len(df), "expected_hits": float(tot[0]), "real_hits": tot[1],
            "by_prob": by_p.reset_index().astype({"p_bin": str}).to_dict("records"),
            "by_salary": by_sal.reset_index().astype({"sal_bin": str}).to_dict("records"),
        }

    strategies = {"gaussian": ("gaussian", "gaussian"), "sim_corr": ("gaussian", "sim_corr"),
                  "smash45": ("smash45", "gaussian"), "smash50": ("smash50", "gaussian")}
    rows = []
    for name, (pool_kind, draw) in strategies.items():
        pool = build_pool(d, pool_kind)
        for rep in range(reps):
            t = time.time()
            r = client.post("/api/optimize", json={**SETTINGS, "players": pool, "week": week,
                                                   "n_lineups": n_lineups, "draw_source": draw})
            r.raise_for_status()
            keys_list = [lineup_key_list(lu) for lu in r.json()["lineups"]]
            scores, missing = score_lineups(keys_list, d)
            m = contest_metrics(scores, d["contests"])
            rows.append({"strategy": name, "rep": rep, "n": len(scores), "mean_score": float(scores.mean()),
                         "max_score": float(scores.max()), "missing_players": missing, "contests": m})
            print(f"  built {name} rep{rep}: {len(scores)} lineups, mean actual {scores.mean():.1f}, "
                  f"max {scores.max():.1f} ({time.time() - t:.0f}s)")
    pk = paper_keys(week)
    if pk:
        scores, missing = score_lineups(pk, d)
        rows.append({"strategy": "your_paper_entries", "rep": 0, "n": len(scores), "mean_score": float(scores.mean()),
                     "max_score": float(scores.max()), "missing_players": missing,
                     "contests": contest_metrics(scores, d["contests"])})
    result["strategies"] = rows

    # Summary by field-size class (size_bucket), averaged over that week's contests.
    print(f"\n-- Real-contest results, week {week} (percentile: 0 = 1st place; lower is better)")
    hdr = f"{'strategy':20s}{'rep':>4s}{'n':>5s}{'mean':>7s}{'max':>7s}"
    for bucket in ("small", "medium", "large"):
        hdr += f"  {bucket}: {'pctile':>7s}{'top20':>7s}{'top1':>7s}{'top.1':>7s}"
    print(hdr)
    for r in rows:
        line = f"{r['strategy']:20s}{r['rep']:>4d}{r['n']:>5d}{r['mean_score']:7.1f}{r['max_score']:7.1f}"
        for bucket in ("small", "medium", "large"):
            cs = [c for c in r["contests"].values() if size_bucket(c["field"]) == bucket]
            line += ("         " + "".join(f"{np.mean([c[f] for c in cs]):7.3f}"
                                           for f in ("mean_pctile", "top20", "top1", "top01"))
                     if cs else "  " + " " * (len(bucket) + 1) + f"{'--':>28s}")
        print(line)
    return result


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("weeks", type=int, nargs="+")
    ap.add_argument("--lineups", type=int, default=150)
    ap.add_argument("--reps", type=int, default=2)
    a = ap.parse_args()
    client = TestClient(api.app)
    out_dir = os.path.join("docs", "eda_outputs", "smash_backtest")
    os.makedirs(out_dir, exist_ok=True)
    for w in a.weeks:
        res = run_week(client, w, a.lineups, a.reps)
        with open(os.path.join(out_dir, f"week_{w}.json"), "w") as f:
            json.dump(res, f, indent=2, default=float)


if __name__ == "__main__":
    main()
