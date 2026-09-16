"""Field analysis for a settled contest -- Phase 4a of the evaluation tab.

Independent of paper trading (score_paper_entries.py): this looks at the
CONTEST ITSELF, not our own picks. For every standings CSV in the archive it
computes the winner, percentile score cutoffs, the ownership profile of the
top finishers (chalky or contrarian?), and the hindsight-optimal lineup --
the single best score achievable within the salary cap using everyone's real
result, an exact ILP solve (PuLP; this is an occasional offline batch job,
not a hot path, so full-ILP correctness beats the live app's custom
branch-and-bound speed hacks).

Run any time after a standings CSV is dropped in (no salaries_prelock.csv
needed for the percentile/winner/ownership numbers; needed for the
hindsight-optimal lineup, which needs salaries to respect the cap):

    venv\\Scripts\\python.exe scripts/dfs_ownership/eval_field.py
    ... --year 2026 --week 1

Writes data/dfs_ownership/_processed/field_eval.parquet -- one row per
settled contest.

KNOWN GAP: no "cash line" / cashers count -- the standings export doesn't
carry the contest's payout structure (paying_positions), only entrant scores.
Reported here as percentile cutoffs (top 1% / top 0.1%) instead, which ARE
exactly computable. A real cash line would need the live payout table
snapshotted pre-lock (get_dk_contest_payout, while the contest is still
open) -- not built yet.
"""
from __future__ import annotations

import argparse
import glob
import json
import math
import os
import sys

import pandas as pd
import pulp

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.scrapers.dk_scraper import normalize_player_name  # noqa: E402
from scripts.dfs_ownership.standings_parser import (  # noqa: E402
    parse_filename, read_standings, actual_scores_from_summary, ownership_from_entries,
    resolve_lineup_name, SLOT_RE, player_key,
)

BASE = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
ARCHIVE = os.path.join(BASE, "data", "dfs_ownership")
OUT_DIR = os.path.join(ARCHIVE, "_processed")
SALARY_CAP = 50000


def _solve_showdown_hindsight(pool: list[dict]) -> dict | None:
    """Exact best CPT+5FLEX under the cap, maximizing real score. `pool`:
    [{'name','team','pos','salary','score'}, ...]."""
    n = len(pool)
    if n < 6:
        return None
    prob = pulp.LpProblem("hindsight_showdown", pulp.LpMaximize)
    c = [pulp.LpVariable(f"c{i}", cat="Binary") for i in range(n)]
    f = [pulp.LpVariable(f"f{i}", cat="Binary") for i in range(n)]
    prob += pulp.lpSum(1.5 * pool[i]["score"] * c[i] + pool[i]["score"] * f[i] for i in range(n))
    prob += pulp.lpSum(c) == 1
    prob += pulp.lpSum(f) == 5
    for i in range(n):
        prob += c[i] + f[i] <= 1
    prob += pulp.lpSum(1.5 * pool[i]["salary"] * c[i] + pool[i]["salary"] * f[i] for i in range(n)) <= SALARY_CAP
    try:
        prob.solve(pulp.PULP_CBC_CMD(msg=0))
    except pulp.PulpSolverError:
        return None
    if prob.status != 1:
        return None
    cpt_i = next(i for i in range(n) if c[i].value() and c[i].value() > 0.5)
    flex_i = [i for i in range(n) if f[i].value() and f[i].value() > 0.5]
    slots = [{"slot": "CPT", **pool[cpt_i]}] + [{"slot": "FLEX", **pool[i]} for i in flex_i]
    score = 1.5 * pool[cpt_i]["score"] + sum(pool[i]["score"] for i in flex_i)
    salary = 1.5 * pool[cpt_i]["salary"] + sum(pool[i]["salary"] for i in flex_i)
    return {"score": round(score, 2), "salary": int(salary), "players": slots}


def _solve_classic_hindsight(pool: list[dict]) -> dict | None:
    """Exact best QB/RB/RB/WR/WR/WR/TE/FLEX/DST under the cap. `pool`:
    [{'name','team','pos','salary','score'}, ...], pos in QB/RB/WR/TE/DST."""
    elig = {"QB": ["QB"], "RB": ["RB", "FLEX"], "WR": ["WR", "FLEX"], "TE": ["TE", "FLEX"], "DST": ["DST"]}
    need = {"QB": 1, "RB": 2, "WR": 3, "TE": 1, "FLEX": 1, "DST": 1}
    x = {}
    prob = pulp.LpProblem("hindsight_classic", pulp.LpMaximize)
    for i, p in enumerate(pool):
        for slot in elig.get(p["pos"], []):
            x[(i, slot)] = pulp.LpVariable(f"x{i}_{slot}", cat="Binary")
    if not x:
        return None
    prob += pulp.lpSum(pool[i]["score"] * v for (i, _), v in x.items())
    for slot, cnt in need.items():
        prob += pulp.lpSum(v for (_, s), v in x.items() if s == slot) == cnt
    for i in range(len(pool)):
        vs = [v for (pi, _), v in x.items() if pi == i]
        if vs:
            prob += pulp.lpSum(vs) <= 1
    prob += pulp.lpSum(pool[i]["salary"] * v for (i, _), v in x.items()) <= SALARY_CAP
    try:
        prob.solve(pulp.PULP_CBC_CMD(msg=0))
    except pulp.PulpSolverError:
        return None
    if prob.status != 1:
        return None
    chosen = [(i, slot) for (i, slot), v in x.items() if v.value() and v.value() > 0.5]
    slots = [{"slot": slot, **pool[i]} for i, slot in chosen]
    score = sum(pool[i]["score"] for i, _ in chosen)
    salary = sum(pool[i]["salary"] for i, _ in chosen)
    return {"score": round(score, 2), "salary": int(salary), "players": slots}


def _load_priced_pool(folder: str, actual_scores: dict) -> list[dict]:
    """salaries_prelock.csv joined to actual_scores -- missing scores (a
    priced player nobody rostered, so DK's summary never mentions them)
    default to 0. That's a real caveat for the hindsight-optimal number: a
    zero-owned player could in principle have scored well and we'd never
    know it from this data source, so treat "hindsight optimal" as a lower
    bound on the true optimal, not an exact one."""
    path = os.path.join(folder, "salaries_prelock.csv")
    if not os.path.exists(path):
        return []
    sal = pd.read_csv(path)
    pool = []
    for _, r in sal.iterrows():
        key = player_key(r["name"], r["team"], str(r.get("pos", "")), normalize_player_name)
        pool.append({"name": r["name"], "team": r["team"], "pos": str(r.get("pos", "")).upper(),
                     "salary": float(r["salary"]), "score": actual_scores.get(key, 0.0)})
    return pool


def eval_contest(folder: str, csv_path: str, slate_format: str, slate_actual_scores: dict) -> dict | None:
    stem = os.path.splitext(os.path.basename(csv_path))[0]
    meta = parse_filename(stem)
    if not meta:
        return None
    try:
        entries_df, dk_summary = read_standings(csv_path)
    except ValueError as e:
        print(f"  {os.path.basename(folder)}/{stem}.csv: {e}")
        return None

    field_size = int(entries_df["rank"].max()) if entries_df["rank"].notna().any() else len(entries_df)
    has_points = "points" in entries_df.columns and entries_df["points"].notna().any()
    if not has_points:
        print(f"  {stem}.csv: no Points column -- can't compute score cutoffs, skipping")
        return None
    pts = entries_df["points"].dropna().sort_values(ascending=False).values

    def _cutoff(frac):
        idx = max(0, min(len(pts) - 1, math.ceil(field_size * frac) - 1))
        return round(float(pts[idx]), 2)

    winner_row = entries_df.loc[entries_df["points"].idxmax()]
    winner_players = [{"slot": s.upper(), "name": n.strip()} for s, n in SLOT_RE.findall(winner_row["lineup"])]

    counts, _, top_dupes = ownership_from_entries(entries_df, normalize_player_name)
    flex_own = {k: 100 * v["flex"] / field_size for k, v in counts.items()}
    cpt_own = {k: 100 * v["cpt"] / field_size for k, v in counts.items()}

    top_n = max(1, min(20, field_size))
    top_rows = entries_df.sort_values("points", ascending=False).head(top_n)
    top_ownerships = []
    for _, row in top_rows.iterrows():
        total = 0.0
        for slot, raw in SLOT_RE.findall(row["lineup"]):
            key, dst = resolve_lineup_name(raw.strip(), normalize_player_name)
            pkey = (key, dst) if dst else (key, None)
            total += cpt_own.get(pkey, 0.0) if slot.upper() == "CPT" else flex_own.get(pkey, 0.0)
        top_ownerships.append(total)

    # Hindsight-optimal uses scores merged across every settled contest for
    # this slate (slate_actual_scores), not just this one contest's own
    # summary -- DK only publishes a player's real score in a contest's
    # export if someone in THAT field actually rostered them, so a small
    # field can miss a player a bigger field on the same slate captured.
    pool = _load_priced_pool(folder, slate_actual_scores)
    hindsight = None
    if pool:
        hindsight = (_solve_showdown_hindsight(pool) if slate_format == "showdown"
                     else _solve_classic_hindsight(pool))

    return {
        "slate_id": os.path.basename(folder), "slate_format": slate_format,
        "contest_name": meta["contest_name"], "entry_fee": meta["entry_fee"], "max_entries": meta["max_entries"],
        "field_size": field_size,
        "winner_score": round(float(winner_row["points"]), 2),
        "winner_players": json.dumps([p["name"] for p in winner_players]),
        "mean_score": round(float(pts.mean()), 2), "median_score": round(float(pd.Series(pts).median()), 2),
        "p90_score": _cutoff(0.10), "p99_score": _cutoff(0.01),
        "top1pct_cutoff_score": _cutoff(0.01), "top01pct_cutoff_score": _cutoff(0.001),
        "top_n_avg_ownership": round(sum(top_ownerships) / len(top_ownerships), 1) if top_ownerships else None,
        "top_lineup_dupe_count": top_dupes,
        "hindsight_optimal_score": hindsight["score"] if hindsight else None,
        "hindsight_optimal_salary": hindsight["salary"] if hindsight else None,
        "hindsight_optimal_players": json.dumps([p["name"] for p in hindsight["players"]]) if hindsight else None,
        "hindsight_vs_winner": round(hindsight["score"] - float(winner_row["points"]), 2) if hindsight else None,
    }


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--year", type=int, default=None)
    ap.add_argument("--week", type=int, default=None)
    args = ap.parse_args()

    year_glob = str(args.year) if args.year else "*"
    week_glob = f"week_{args.week:02d}" if args.week else "week_*"
    folders = sorted(glob.glob(os.path.join(ARCHIVE, year_glob, week_glob, "*")))
    folders = [f for f in folders if os.path.isdir(f) and os.path.basename(f) != "_processed"]

    print(f"Scanning {len(folders)} slate folder(s)...")
    rows = []
    for folder in folders:
        man_path = os.path.join(folder, "manifest.json")
        manifest = json.load(open(man_path)) if os.path.exists(man_path) else {}
        slate_format = manifest.get("slate_format", "showdown" if "showdown" in os.path.basename(folder) else "classic")
        standings = [p for p in glob.glob(os.path.join(folder, "*.csv"))
                     if os.path.basename(p) != "salaries_prelock.csv"]
        if not standings:
            continue

        # Merge real per-player scores across every settled contest for this
        # slate first -- see eval_contest's comment on why one contest's
        # summary alone can be missing a player a bigger field captured.
        slate_actual_scores: dict = {}
        for csv_path in standings:
            try:
                _, dk_summary = read_standings(csv_path)
            except ValueError:
                continue
            slate_actual_scores.update(actual_scores_from_summary(dk_summary, normalize_player_name))

        for csv_path in standings:
            row = eval_contest(folder, csv_path, slate_format, slate_actual_scores)
            if row:
                rows.append(row)

    if not rows:
        print("\nNo settled contests found.")
        return

    df = pd.DataFrame(rows)
    os.makedirs(OUT_DIR, exist_ok=True)
    out_path = os.path.join(OUT_DIR, "field_eval.parquet")
    df.to_parquet(out_path, index=False)
    print(f"\nWrote {len(df)} contest(s) -> {os.path.relpath(out_path, BASE)}")
    print(df[["slate_id", "contest_name", "field_size", "winner_score",
              "hindsight_optimal_score", "top1pct_cutoff_score", "top_n_avg_ownership"]].to_string(index=False))


if __name__ == "__main__":
    main()
