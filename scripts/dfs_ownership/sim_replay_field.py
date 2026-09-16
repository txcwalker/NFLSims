"""Post-lock sim replay -- "given OUR week-N simulations, how would this
already-settled contest have played out?"

This is deliberately NOT score_paper_entries.py (which grades a paper entry
against what actually happened) and NOT eval_field.py (which analyzes the
real contest on its own real results). Both of those answer "how did we do
against reality." This answers a different question: replace every real
field entrant's score with OUR sim's score for that same lineup, redraw
many times, and see where our own paper-entered lineup(s) land against that
real field's actual roster construction. That isolates our own
model/optimizer/ownership process from a single week's real-world variance
-- if our sim is a good model of reality, our paper lineups should rank
well against the real field even under our own (not the real) scoring.

Why this is possible at all: `run_week_sim_2026.py` writes one row per
(player x iteration) to data/interim/dfs_week_{N}_players.parquet -- a full
simulated GAME per iteration, so teammates' scores stay correlated within
an iteration (a real QB/WR stack moves together), not independent draws
per player. The real field's lineups (who's actually rostered, from the
settled contest's standings CSV) are exactly what a real field looked
like; only the SCORING is swapped out for ours.

Run after (a) paper entries exist for a contest (see score_paper_entries.py's
docstring for how those get saved) and (b) that contest's standings CSV has
been dropped in (same folder convention as every other script here), and
(c) that week's DFS sim has been run (scripts/simulation_runners/run_week_sim_2026.py):

    venv\\Scripts\\python.exe scripts/dfs_ownership/sim_replay_field.py --year 2026 --week 1

Writes data/dfs_ownership/_processed/sim_replay.parquet -- one row per
(paper entry x settled contest), same shape/convention as paper_results.parquet.

KNOWN GAP (same one eval_field.py documents): no real payout table is
archived, so results are reported as percentile/rank distributions rather
than $EV. Snapshotting DK's payout table pre-lock (get_dk_contest_payout)
would unlock real $EV here too -- not built yet.
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import sys
from collections import Counter, defaultdict
from functools import lru_cache

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.scrapers.dk_scraper import normalize_player_name  # noqa: E402
from scripts.dfs_ownership.standings_parser import (  # noqa: E402
    parse_filename, read_standings, resolve_lineup_name, player_key, slot_re_for,
)

BASE = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
ARCHIVE = os.path.join(BASE, "data", "dfs_ownership")
OUT_DIR = os.path.join(ARCHIVE, "_processed")
INTERIM_DIR = os.path.join(BASE, "data", "interim")

_WEEK_PARQUET_CACHE: dict = {}   # week -> raw players df (or None if missing), avoids re-reading a 3-4M-row parquet once per settled contest on the same slate


@lru_cache(maxsize=4096)
def _resolve_cached(raw: str) -> tuple:
    """resolve_lineup_name is called millions of times on a large-field
    export (126k entries x 6 players is routine for a real "millionaire"
    contest), but the whole slate only has a few dozen distinct player name
    strings -- memoize on the raw token so the regex/normalize work behind
    resolve_lineup_name happens once per player, not once per appearance."""
    return resolve_lineup_name(raw, normalize_player_name)


def _lineup_keys(lineup: str, slate_format: str) -> tuple[tuple[tuple, float], ...]:
    """Lineup string -> ((player_key, score_multiplier), ...) -- a tuple
    (not list) so the whole thing is hashable, letting callers dedupe
    identical lineups (real large fields are massively duplicated) instead
    of scoring the same composition thousands of times over."""
    out = []
    for slot, raw in slot_re_for(slate_format).findall(lineup):
        key = _resolve_cached(raw.strip())
        mult = 1.5 if slot.upper() == "CPT" else 1.0
        out.append((key, mult))
    return tuple(out)


def _load_sim_scores(week: int, needed_keys: set) -> tuple[dict, int]:
    """{player_key: np.array of per-iteration dk_score} for every key in
    `needed_keys` found in this week's sim, plus the iteration count.
    A key not simulated (e.g. a bye-week/inactive player who nonetheless
    got rostered in the real field) is simply absent from the returned
    dict -- callers treat a missing player as 0 for that lineup, same
    "surfaced as missing, not a crash" convention as score_paper_entries.py.
    """
    if week not in _WEEK_PARQUET_CACHE:
        path = os.path.join(INTERIM_DIR, f"dfs_week_{week}_players.parquet")
        _WEEK_PARQUET_CACHE[week] = (
            pd.read_parquet(path, columns=["Player", "Team", "Pos", "iteration", "dk_score"])
            if os.path.exists(path) else None)
    cached = _WEEK_PARQUET_CACHE[week]
    if cached is None:
        return {}, 0
    df = cached
    n_iterations = int(df["iteration"].max()) + 1 if len(df) else 0

    # Key every (Player, Team, Pos) combo once (a few hundred, not 3.84M
    # rows) rather than row-wise .apply over the whole parquet.
    combos = df[["Player", "Team", "Pos"]].drop_duplicates()
    combos["_key"] = combos.apply(
        lambda r: player_key(r["Player"], r["Team"], r["Pos"], normalize_player_name), axis=1)
    combos = combos[combos["_key"].isin(needed_keys)]
    if combos.empty:
        return {}, n_iterations

    df = df.merge(combos, on=["Player", "Team", "Pos"], how="inner")
    scores: dict = {}
    for key, sub in df.groupby("_key"):
        arr = np.zeros(n_iterations)
        arr[sub["iteration"].values] = sub["dk_score"].values
        scores[key] = arr
    return scores, n_iterations


def _lineup_score_vector(lineup_keys: list[tuple[tuple, float]], sim_scores: dict, n_iterations: int) -> tuple[np.ndarray, list[str]]:
    """Sum a lineup's per-iteration score across all its players (CPT x1.5
    already folded into `lineup_keys`'s multiplier). Returns (n_iterations,)
    total-score array and the list of keys with no sim data (scored 0)."""
    total = np.zeros(n_iterations)
    missing = []
    for key, mult in lineup_keys:
        arr = sim_scores.get(key)
        if arr is None:
            missing.append(f"{key[0]}" if key[1] is None else f"{key[1]} DST")
            continue
        total += mult * arr
    return total, missing


def process_slate(folder: str) -> list[dict]:
    entries_path = os.path.join(folder, "paper_entries.json")
    if not os.path.exists(entries_path):
        return []
    paper = (json.load(open(entries_path)) or {}).get("entries", [])
    if not paper:
        return []

    man_path = os.path.join(folder, "manifest.json")
    manifest = json.load(open(man_path)) if os.path.exists(man_path) else {}
    slate_id = os.path.basename(folder)
    slate_format = manifest.get("slate_format", "showdown" if "showdown" in slate_id else "classic")

    by_contest: dict = defaultdict(list)
    for e in paper:
        by_contest[e.get("contest_name")].append(e)

    standings = [p for p in glob.glob(os.path.join(folder, "*.csv"))
                 if os.path.basename(p) != "salaries_prelock.csv"]

    rows: list[dict] = []
    for csv_path in standings:
        stem = os.path.splitext(os.path.basename(csv_path))[0]
        meta = parse_filename(stem)
        if not meta or meta["contest_name"] not in by_contest:
            continue
        our_entries = by_contest[meta["contest_name"]]
        week = our_entries[0].get("week")
        if week is None:
            print(f"  {slate_id}/{stem}.csv: paper entry has no week -- skipping")
            continue
        try:
            entries_df, _ = read_standings(csv_path)
        except ValueError as e:
            print(f"  {slate_id}/{stem}.csv: {e}")
            continue

        field_size = int(entries_df["rank"].max()) if entries_df["rank"].notna().any() else len(entries_df)
        # A real large-field contest (a "millionaire" GPP routinely has
        # 100k+ entries) is massively duplicated -- dedupe by composition
        # and weight by count, so a 126k-entry field might only need a few
        # hundred to a few thousand lineups actually scored, not 126k.
        lineup_counts: Counter = Counter(_lineup_keys(lu, slate_format) for lu in entries_df["lineup"])
        lineup_counts.pop((), None)   # an unparseable Lineup string -> empty tuple
        if not lineup_counts:
            print(f"  {slate_id}/{stem}.csv: couldn't parse any real lineups -- skipping")
            continue
        unique_lineups = list(lineup_counts.keys())
        lineup_weights = np.array([lineup_counts[lu] for lu in unique_lineups], dtype=float)

        our_lineup_keys = {
            e.get("entry_id"): tuple(
                (player_key(p.get("name", ""), p.get("team", ""), p.get("pos", ""), normalize_player_name),
                 1.5 if str(p.get("slot", "")).upper() == "CPT" else 1.0)
                for p in e.get("players", [])
            )
            for e in our_entries
        }

        needed_keys = {k for lu in unique_lineups for k, _ in lu}
        for lu in our_lineup_keys.values():
            needed_keys.update(k for k, _ in lu)

        sim_scores, n_iterations = _load_sim_scores(int(week), needed_keys)
        if n_iterations == 0:
            print(f"  {slate_id}/{stem}.csv: no week-{week} sim found "
                  f"(run scripts/simulation_runners/run_week_sim_2026.py {week}) -- skipping")
            continue

        # Every DISTINCT real field lineup's per-iteration score ->
        # (n_unique_lineups, n_iterations); `lineup_weights` (how many real
        # entries shared that exact composition) does the rest of the work
        # when counting how many field entries a given score beats, below.
        field_matrix = np.zeros((len(unique_lineups), n_iterations))
        field_missing_total = 0
        for i, lu in enumerate(unique_lineups):
            vec, missing = _lineup_score_vector(lu, sim_scores, n_iterations)
            field_matrix[i] = vec
            field_missing_total += len(missing)
        if field_missing_total:
            print(f"  {slate_id}/{stem}.csv: {field_missing_total} field-lineup player slot(s) "
                  f"had no sim data (treated as 0 for those iterations)")

        for e in our_entries:
            lu = our_lineup_keys[e.get("entry_id")]
            our_vec, missing = _lineup_score_vector(lu, sim_scores, n_iterations)
            if missing:
                print(f"  {slate_id}/{meta['contest_name']}: entry {e.get('entry_id')} -- "
                      f"no sim data for {', '.join(missing)} (treated as 0)")

            # Weighted by lineup_weights so a duplicated real lineup counts
            # once per real entry that actually ran it, not once total.
            beat = lineup_weights @ (field_matrix < our_vec[None, :])     # per-iteration count beaten
            tied = lineup_weights @ (field_matrix == our_vec[None, :])
            rank = field_size - beat - tied + 1                       # same convention as score_paper_entries.py
            percentile = 100.0 * rank / field_size                    # lower = better

            model = e.get("model") or {}
            rows.append({
                "entry_id": e.get("entry_id"), "created_at": e.get("created_at"),
                "year": e.get("year"), "week": e.get("week"), "slate_id": slate_id,
                "slate_format": e.get("slate_format") or slate_format,
                "source": e.get("source"), "label": e.get("label"),
                "contest_name": meta["contest_name"], "entry_fee": meta["entry_fee"],
                "max_entries": meta["max_entries"], "field_size": field_size,
                "n_sim_iterations": n_iterations,
                "predicted_score": model.get("projected_score"),
                "sim_mean_score": round(float(our_vec.mean()), 2),
                "sim_p10_score": round(float(np.percentile(our_vec, 10)), 2),
                "sim_p50_score": round(float(np.percentile(our_vec, 50)), 2),
                "sim_p90_score": round(float(np.percentile(our_vec, 90)), 2),
                "mean_rank": round(float(rank.mean()), 1),
                "median_rank": round(float(np.median(rank)), 1),
                "best_rank": int(rank.min()), "worst_rank": int(rank.max()),
                "mean_percentile": round(float(percentile.mean()), 2),
                "median_percentile": round(float(np.median(percentile)), 2),
                "prob_top1pct": round(float((percentile <= 1.0).mean() * 100), 2),
                "prob_top10pct": round(float((percentile <= 10.0).mean() * 100), 2),
                "prob_top20pct": round(float((percentile <= 20.0).mean() * 100), 2),
                "prob_beat_field_median": round(float((percentile <= 50.0).mean() * 100), 2),
                "players": json.dumps([p.get("name") for p in e.get("players", [])]),
            })

    return rows


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--year", type=int, default=None)
    ap.add_argument("--week", type=int, default=None)
    args = ap.parse_args()

    year_glob = str(args.year) if args.year else "*"
    week_glob = f"week_{args.week:02d}" if args.week else "week_*"
    folders = sorted(glob.glob(os.path.join(ARCHIVE, year_glob, week_glob, "*")))
    folders = [f for f in folders if os.path.isdir(f) and os.path.basename(f) != "_processed"]

    print(f"Scanning {len(folders)} slate folder(s) for paper_entries.json...")
    all_rows: list[dict] = []
    for folder in folders:
        all_rows.extend(process_slate(folder))

    if not all_rows:
        print("\nNo settled contests with matching paper entries + a week sim found.")
        return

    df = pd.DataFrame(all_rows)
    os.makedirs(OUT_DIR, exist_ok=True)
    out_path = os.path.join(OUT_DIR, "sim_replay.parquet")
    df.to_parquet(out_path, index=False)

    print(f"\nWrote {len(df)} sim-replay result(s) -> {os.path.relpath(out_path, BASE)}")
    print(df[["contest_name", "label", "field_size", "n_sim_iterations", "sim_mean_score",
              "mean_rank", "mean_percentile", "prob_top10pct"]].to_string(index=False))


if __name__ == "__main__":
    main()
