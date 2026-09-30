"""Cash Lineups evaluation (2026-09-30): how did each week's cash-optimal
builds score against their projection, and against a benchmark cash lineup
(Adam Levitan's, hand-entered in data/eval/{year}/benchmark_cash_lineups.json)?

Actual points -- real DK scoring, from the main-slate standings CSVs already
archived under data/dfs_ownership/{year}/week_NN/main_slate/ (each CSV's
Player/FPTS summary block). A CSV's FPTS is frozen at whenever it was
downloaded, and some get grabbed mid-slate (week 1 2026: most files at 4:46pm
ET, with the 4:25 games still live), so each player's score is taken from the
MOST RECENTLY DOWNLOADED file that lists them. Cross-checked against ESPN
final box scores for weeks 1-3 (2026-09-30): identical apart from 2-pt
conversions, which DK counts and our ESPN formula doesn't. A player nobody in
any archived contest rostered falls back to nflverse (player_actuals.py) and is
flagged `nflverse`; found in neither -> `missing`, scored 0.
Caveat: "latest file" uses file mtimes, which a fresh git clone resets -- on a
new machine, re-download or touch the post-slate files if week totals look low.

Projection -- the sim's per-player mean DK points (the same `dk_points` the
cash solver maximizes), so a lineup's projected total is also its sim mean.
Sim percentile -- where the actual total landed inside that exact lineup's own
per-iteration sim scores (correlated within a game, since a player's scores
come from the same simulated game iteration), via sim_replay_field's
_load_sim_scores/_lineup_score_vector.
"""
import glob
import json
import os

import numpy as np
import pandas as pd

from src.evaluation.line_history import BASE_DIR
from src.scrapers.dk_scraper import normalize_player_name
from scripts.dfs_ownership.standings_parser import player_key, resolve_lineup_name
from scripts.dfs_ownership.sim_replay_field import _load_sim_scores, _lineup_score_vector

SLOT_ORDER = ["QB", "RB", "RB", "WR", "WR", "WR", "TE", "FLEX", "DST"]


def main_slate_dir(year, week, base_dir=BASE_DIR):
    """Inputs: year, week, base_dir. Output: str path of that week's archived
    classic main-slate folder (may not exist)."""
    return os.path.join(base_dir, "data", "dfs_ownership", str(year), f"week_{int(week):02d}", "main_slate")


def _standings_csvs(folder):
    """Every standings export in `folder` (skips salaries_prelock.csv and any
    CSV without a Player/FPTS summary block), oldest download first."""
    out = []
    for f in glob.glob(os.path.join(folder, "*.csv")):
        if os.path.basename(f).startswith("salaries"):
            continue
        out.append(f)
    return sorted(out, key=os.path.getmtime)


def load_dk_actuals(year, week, base_dir=BASE_DIR):
    """Real DK points per player for a week's main slate.

    Inputs: year, week (int), base_dir.
    Output: {player_key: float} -- player_key is standings_parser's scheme
      ((normalized_name, None) or ("__dst__", TEAM)). Empty dict if no
      standings CSVs are archived for the week (= week not gradable yet).
    Later-downloaded files overwrite earlier ones (see module docstring for
    why that's the staleness rule).
    """
    scores = {}
    for path in _standings_csvs(main_slate_dir(year, week, base_dir)):
        df = pd.read_csv(path, dtype=str, keep_default_na=False, encoding="utf-8-sig")
        cols = {c.strip().lstrip("﻿").lower(): c for c in df.columns}
        pcol, fcol = cols.get("player"), cols.get("fpts")
        if not pcol or not fcol:
            continue
        summ = df[[pcol, fcol]]
        summ = summ[summ[pcol].str.strip() != ""]
        for name, fpts in summ.itertuples(index=False):
            try:
                scores[resolve_lineup_name(name.strip(), normalize_player_name)] = float(fpts)
            except ValueError:
                continue
    return scores


def load_fallback_actuals(year, week, base_dir=BASE_DIR):
    """nflverse DK points (player_actuals cache, no download) for players no
    archived contest rostered. Output: {player_key: float} (skill players only)."""
    from src.evaluation.player_actuals import load_player_actuals
    act = load_player_actuals(year, base_dir, fetch_if_missing=False)
    if act.empty:
        return {}
    wk = act[act["week"] == int(week)]
    return {(normalize_player_name(str(n)), None): float(s) for n, s in zip(wk["player_name"], wk["dk_score"])}


def load_salaries(year, week, base_dir=BASE_DIR):
    """Pre-lock DK salaries. Output: {player_key: {"name","team","pos","salary"}}."""
    path = os.path.join(main_slate_dir(year, week, base_dir), "salaries_prelock.csv")
    if not os.path.exists(path):
        return {}
    df = pd.read_csv(path)
    df = df.drop_duplicates(subset=["name", "team", "pos"], keep="last")
    return {player_key(r.name, r.team, r.pos, normalize_player_name):
            {"name": r.name, "team": r.team, "pos": r.pos, "salary": int(r.salary)}
            for r in df.itertuples(index=False)}


def benchmark_path(year, base_dir=BASE_DIR):
    """Inputs: year, base_dir. Output: str path of the hand-edited benchmark file."""
    return os.path.join(base_dir, "data", "eval", str(year), "benchmark_cash_lineups.json")


def load_benchmarks(year, week, base_dir=BASE_DIR):
    """Hand-entered benchmark lineups for one week.
    Inputs: year, week, base_dir. Output: list of {"label", "source", "players":
    [{"slot","name","team","pos"}]} (empty if the file or week is absent)."""
    p = benchmark_path(year, base_dir)
    if not os.path.exists(p):
        return []
    with open(p, encoding="utf-8") as f:
        data = json.load(f)
    return [b for b in data.get("lineups", []) if int(b.get("week", -1)) == int(week)]


def _pct(vec, q):
    return round(float(np.percentile(vec, q)), 2)


def grade_lineup(players, proj_lookup, actuals, fallback, sim_scores, n_iterations):
    """Grade one 9-player classic lineup.

    Inputs:
      players      -- [{"slot","name","team","pos", optional "projection","salary"}]
      proj_lookup  -- {player_key: projected DK mean} (used when a player has no
                      "projection" of its own, e.g. benchmark lineups)
      actuals      -- {player_key: real DK points} from load_dk_actuals
      fallback     -- {player_key: nflverse DK points} from load_fallback_actuals
      sim_scores, n_iterations -- from sim_replay_field._load_sim_scores
    Output: dict -- "players" (each with projection, actual, diff, source),
      projected/actual totals, diff, n_beat (players who beat projection),
      and "sim" (p10/p50/p90 of the lineup's own sim scores + where the actual
      landed, as a percentile) when sims exist. A player with no real score
      counts 0 toward the actual total and is listed in "missing".
    """
    graded, keys, missing = [], [], []
    for p in players:
        key = player_key(p["name"], p["team"], p["pos"], normalize_player_name)
        keys.append((key, 1.0))
        proj = p.get("projection")
        if proj is None:
            proj = proj_lookup.get(key)
        if key in actuals:
            actual, source = actuals[key], "dk"
        elif key in fallback:
            actual, source = fallback[key], "nflverse"
        else:
            actual, source = None, "missing"
            missing.append(p["name"] if p["pos"] != "DST" else f"{p['team']} DST")
        graded.append({
            "slot": p["slot"], "name": p["name"], "team": p["team"], "pos": p["pos"],
            "salary": p.get("salary"),
            "projection": round(proj, 2) if proj is not None else None,
            "actual": round(actual, 2) if actual is not None else None,
            "diff": round(actual - proj, 2) if actual is not None and proj is not None else None,
            "source": source,
        })

    proj_total = sum(g["projection"] or 0 for g in graded)
    actual_total = sum(g["actual"] or 0 for g in graded)
    out = {
        "players": graded,
        "salary": sum(g["salary"] or 0 for g in graded) or None,
        "projected": round(proj_total, 2),
        "actual": round(actual_total, 2),
        "diff": round(actual_total - proj_total, 2),
        "n_beat": sum(1 for g in graded if g["diff"] is not None and g["diff"] > 0),
        "missing": missing,
        "sim": None,
    }
    if n_iterations:
        vec, sim_missing = _lineup_score_vector(keys, sim_scores, n_iterations)
        out["sim"] = {
            "p10": _pct(vec, 10), "p50": _pct(vec, 50), "p90": _pct(vec, 90),
            # share of this lineup's sim runs scoring at or below what it actually scored
            "actual_percentile": round(float((vec <= actual_total).mean() * 100), 1),
            "unsimmed": sim_missing,
        }
    return out


def _with_salaries(players, salaries):
    """Fill salary (and canonical display name) from the pre-lock salary file."""
    out = []
    for p in players:
        key = player_key(p["name"], p["team"], p["pos"], normalize_player_name)
        s = salaries.get(key, {})
        out.append({**p, "salary": p.get("salary") or s.get("salary")})
    return out


def summarize(ours, benchmarks):
    """Week-level roll-up.

    Inputs: ours (graded cash lineups, solver order = best projection first),
      benchmarks (graded benchmark lineups, each with "label").
    Output: dict -- our average projected/actual/diff, how many of our builds
      beat projection, the top build, and per benchmark: its totals, how many
      of ours outscored it, and the player overlap vs. our top build
      (shared / only-ours / only-theirs, each with actual points).
    """
    if not ours:
        return {}
    acts = [l["actual"] for l in ours]
    s = {
        "n": len(ours),
        "avg_projected": round(float(np.mean([l["projected"] for l in ours])), 2),
        "avg_actual": round(float(np.mean(acts)), 2),
        "avg_diff": round(float(np.mean([l["diff"] for l in ours])), 2),
        "n_beat_projection": sum(1 for l in ours if l["diff"] > 0),
        "best_actual": round(max(acts), 2),
        "top_build": {"projected": ours[0]["projected"], "actual": ours[0]["actual"]},
        "benchmarks": [],
    }
    top = _by_overlap_key(ours[0]["players"])
    for b in benchmarks:
        theirs = _by_overlap_key(b["players"])
        s["benchmarks"].append({
            "label": b["label"], "projected": b["projected"], "actual": b["actual"],
            "n_ours_beat_it": sum(1 for a in acts if a > b["actual"]),
            "top_build_minus_benchmark": round(ours[0]["actual"] - b["actual"], 2),
            "shared": _pick(top, top.keys() & theirs.keys()),
            "only_ours": _pick(top, top.keys() - theirs.keys()),
            "only_theirs": _pick(theirs, theirs.keys() - top.keys()),
        })
    return s


def _by_overlap_key(players):
    """{(normalized name or "dst", team): player} -- so "Jahmyr Gibbs" matches
    "jahmyr gibbs" and our "Defense"/NYJ matches a benchmark's "Jets"/NYJ."""
    return {("dst" if p["pos"] == "DST" else normalize_player_name(p["name"]), p["team"]): p for p in players}


def _pick(by_key, keys):
    """The players at `keys` as {name, team, pos, actual}, highest actual first."""
    return sorted(({"name": by_key[k]["name"], "team": by_key[k]["team"], "pos": by_key[k]["pos"],
                    "actual": by_key[k]["actual"]} for k in keys), key=lambda x: -(x["actual"] or 0))


def build_cash_eval(year, week, our_lineups, pool, base_dir=BASE_DIR):
    """Grade a week's cash lineups + benchmark lineups.

    Inputs:
      year, week   -- the slate
      our_lineups  -- GET /api/week_cash_lineups' "lineups" (each {"slots": [...],
                      "projected_score", "total_salary"})
      pool         -- app._priced_pool_for_week's priced pool ([{"name","team","pos",
                      "salary","projection"}]) for benchmark projections
    Output: {"gradable": bool, "ours": [...], "benchmarks": [...], "summary": {...}}.
      gradable is False (and the lists empty) until a main-slate standings CSV
      is archived for the week.
    """
    actuals = load_dk_actuals(year, week, base_dir)
    if not actuals:
        return {"gradable": False, "ours": [], "benchmarks": [], "summary": {}}
    fallback = load_fallback_actuals(year, week, base_dir)
    salaries = load_salaries(year, week, base_dir)
    proj_lookup = {player_key(p["name"], p["team"], p["pos"], normalize_player_name): p["projection"] for p in pool}
    bench_raw = load_benchmarks(year, week, base_dir)

    needed = set()
    for l in our_lineups:
        needed |= {player_key(p["name"], p["team"], p["pos"], normalize_player_name) for p in l["slots"]}
    for b in bench_raw:
        needed |= {player_key(p["name"], p["team"], p["pos"], normalize_player_name) for p in b["players"]}
    sim_scores, n_iter = _load_sim_scores(int(week), needed)

    ours = []
    for i, l in enumerate(our_lineups):
        g = grade_lineup(l["slots"], proj_lookup, actuals, fallback, sim_scores, n_iter)
        ours.append({"label": f"Build {i + 1}", **g})
    benches = []
    for b in bench_raw:
        g = grade_lineup(_with_salaries(b["players"], salaries), proj_lookup, actuals, fallback, sim_scores, n_iter)
        benches.append({"label": b.get("label", "Benchmark"), "source": b.get("source"), **g})
    return {"gradable": True, "ours": ours, "benchmarks": benches, "summary": summarize(ours, benches)}
