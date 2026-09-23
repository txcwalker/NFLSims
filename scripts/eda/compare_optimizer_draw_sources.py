"""A/B the classic optimizer's ILP draw source: real sim iterations ('sim')
vs. the original correlated-normal draw ('gaussian').

Why: 2026-09-23 the optimizer gained OptimizeRequest.draw_source='sim' --
each lineup is built for one real slate-wide sim iteration instead of a
made-up bell-curve week with hand-set correlations. This script measures
whether that actually produces better GPP portfolios, graded by the same
(now week-correct) field simulation the site shows.

Method: one in-process TestClient, one week's real player pool, the
Optimizer page's default settings (top_heavy, 11,000 entries, $18,
2,200 paid, min_unique 2, 40% max exposure per position), gpp_projection
computed exactly like Optimizer.jsx's computeGppProj. Both sources are
scored against the SAME cached field sample (built once per process). Each
source runs `--reps` times so run-to-run noise is visible next to the
between-source difference -- a gap smaller than the within-source spread
is not a real difference.

Metrics per run: portfolio EV%, mean Top1%/Top0.1%/ITM%, mean lineup
p50/p95, distinct players used, max single-player exposure, mean pairwise
overlap, and stack structure (QB + >=1 / >=2 same-team pass catchers,
bring-back from the opponent, DST facing a rostered offensive player).

Usage:
    venv\\Scripts\\python.exe scripts/eda/compare_optimizer_draw_sources.py <week> [n_lineups] [reps]
Output: prints a table; writes docs/eda_outputs/optimizer_draw_source/week_{week}.json
"""
import json
import os
import sys
import time
from collections import Counter
from itertools import combinations

sys.path.insert(0, os.getcwd())

import numpy as np  # noqa: E402
from fastapi.testclient import TestClient  # noqa: E402

import src.api.app as api  # noqa: E402

# Mirrors frontend/src/pages/Optimizer.jsx GPP_WEIGHTS_BY_TYPE.top_heavy.
GPP_WEIGHTS = {"p25": 0.05, "p50": 0.20, "p75": 0.35, "p95": 0.40}
DEFAULTS = {
    "contest_type": "top_heavy", "contest_size": 11000, "total_entries": 11000,
    "entry_fee": 18, "paying_positions": 2200, "min_unique_players": 2,
    "include_dst_in_unique": False,
    "max_exposure_by_pos": {"QB": 0.4, "RB": 0.4, "WR": 0.4, "TE": 0.4, "DST": 0.4},
}


def build_pool(client, week):
    """Inputs: client (TestClient), week (int).
    Outputs: (players payload list for /api/optimize, {(name, team): opponent}).
    Purpose: the same priced-player payload Optimizer.jsx sends, with no
    hand overrides -- projection = sim p50, gpp_projection = weighted
    p25/p50/p75/p95, ownership left for the backend model to fill."""
    proj = client.get("/api/week_projections", params={"week": week}).json()
    players, opp = [], {}
    for p in proj["players"]:
        if not p.get("salary"):
            continue
        pcts = p.get("dk_pcts_all")
        if pcts and len(pcts) == 101:
            gpp = sum(w * pcts[int(k[1:])] for k, w in GPP_WEIGHTS.items())
            p50 = pcts[50]
        else:
            gpp = p50 = p["dk_score"]
        players.append({"name": p["name"], "team": p["team"], "pos": p["pos"],
                        "salary": p["salary"], "projection": p50,
                        "gpp_projection": round(gpp, 1), "dk_pcts_all": pcts})
        opp[(p["name"], p["team"])] = p.get("opponent")
    return players, opp


def lineup_structure(lu, opp):
    """Inputs: lu (one /api/optimize lineup dict), opp ({(name, team): opp team}).
    Output: dict of stack flags for that lineup.
    Tricky bit: DST is matched by team, so 'DST vs own offense' means the
    DST's team is the OPPONENT of some rostered offensive player."""
    ps = lu["players"]
    qb = next(p for p in ps if p["pos"] == "QB")
    qb_opp = opp.get((qb["name"], qb["team"]))
    mates = sum(1 for p in ps if p["team"] == qb["team"] and p["pos"] in ("WR", "TE", "RB"))
    catchers = sum(1 for p in ps if p["team"] == qb["team"] and p["pos"] in ("WR", "TE"))
    bring_back = any(p["team"] == qb_opp and p["pos"] != "DST" for p in ps)
    dst = next(p for p in ps if p["pos"] == "DST")
    dst_conflict = any(opp.get((p["name"], p["team"])) == dst["team"] for p in ps if p["pos"] != "DST")
    return {"qb_stack1": catchers >= 1, "qb_stack2": catchers >= 2,
            "qb_mates": mates, "bring_back": bring_back, "dst_conflict": dst_conflict}


def summarize(resp, opp):
    """Inputs: resp (/api/optimize JSON), opp. Output: flat metrics dict."""
    lus = resp["lineups"]
    n = len(lus)
    keys = [frozenset((p["name"], p["team"]) for p in lu["players"]) for lu in lus]
    expo = Counter(k for ks in keys for k in ks)
    overlaps = [len(a & b) / 9 for a, b in combinations(keys, 2)]
    st = [lineup_structure(lu, opp) for lu in lus]
    m = lambda f: float(np.mean([lu[f] for lu in lus]))  # noqa: E731
    return {
        "n": n,
        "draw_source": resp["portfolio"].get("draw_source"),
        "portfolio_ev_pct": resp["portfolio"]["total_ev_pct"],
        "top1_pct": m("top1_pct"), "top01_pct": m("top01_pct"), "itm_pct": m("itm_pct"),
        "lineup_p50": m("lineup_p50"), "lineup_p95": m("lineup_p95"),
        "median_proj": m("projected_score"),
        "distinct_players": len(expo),
        "max_exposure": max(expo.values()) / n,
        "avg_overlap": float(np.mean(overlaps)) if overlaps else 0.0,
        "qb_stack1": np.mean([s["qb_stack1"] for s in st]),
        "qb_stack2": np.mean([s["qb_stack2"] for s in st]),
        "bring_back": np.mean([s["bring_back"] for s in st]),
        "dst_conflict": np.mean([s["dst_conflict"] for s in st]),
    }


def main(week, n_lineups=150, reps=2):
    client = TestClient(api.app)
    players, opp = build_pool(client, week)
    print(f"Week {week}: {len(players)} priced players, {n_lineups} lineups x {reps} reps per source")
    rows = []
    for rep in range(reps):
        for source in ("gaussian", "sim"):
            t = time.time()
            r = client.post("/api/optimize", json={**DEFAULTS, "players": players, "week": week,
                                                   "n_lineups": n_lineups, "draw_source": source})
            r.raise_for_status()
            row = {"source": source, "rep": rep, "secs": round(time.time() - t, 1), **summarize(r.json(), opp)}
            rows.append(row)
            print(f"  {source:8s} rep{rep}: EV {row['portfolio_ev_pct']:7.1f}%  top1 {row['top1_pct']:.2f}%  "
                  f"({row['secs']}s, ran as {row['draw_source']})")

    cols = ["portfolio_ev_pct", "top1_pct", "top01_pct", "itm_pct", "median_proj", "lineup_p50", "lineup_p95",
            "distinct_players", "max_exposure", "avg_overlap", "qb_stack1", "qb_stack2", "bring_back", "dst_conflict"]
    print(f"\n{'metric':18s}" + "".join(f"{s + ' r' + str(k):>13s}" for k in range(reps) for s in ("gauss", "sim")))
    for c in cols:
        vals = [next(x[c] for x in rows if x["source"] == s and x["rep"] == k) for k in range(reps) for s in ("gaussian", "sim")]
        print(f"{c:18s}" + "".join(f"{v:13.3f}" for v in vals))

    out_dir = os.path.join("docs", "eda_outputs", "optimizer_draw_source")
    os.makedirs(out_dir, exist_ok=True)
    with open(os.path.join(out_dir, f"week_{week}.json"), "w") as f:
        json.dump({"week": week, "settings": DEFAULTS, "n_lineups": n_lineups, "runs": rows}, f, indent=2, default=float)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    main(int(sys.argv[1]),
         int(sys.argv[2]) if len(sys.argv) > 2 else 150,
         int(sys.argv[3]) if len(sys.argv) > 3 else 2)
