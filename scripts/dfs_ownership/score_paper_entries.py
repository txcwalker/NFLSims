"""Settle paper-trade entries against a dropped-in DK standings CSV.

A "paper entry" is a lineup flagged from the optimizer or Lineup Lab as "I'm
actually entering this" (the 📝 button), saved via POST /api/paper/entries
into data/dfs_ownership/<year>/week_<NN>/<slate_id>/paper_entries.json. This
script is the other half: once that contest settles and you drop its
standings CSV into the same folder (same <name>_<price>_<xmax>max.csv
convention as build_ownership_dataset.py -- matched here by <name>), it
computes what your lineup would ACTUALLY have scored and where it would
have ranked in that real field.

Run after both (a) saving paper entries, and (b) dropping the settled
contest's standings CSV in:

    venv\\Scripts\\python.exe scripts/dfs_ownership/score_paper_entries.py
    ... --year 2026 --week 1

Writes data/dfs_ownership/_processed/paper_results.parquet -- one row per
(paper entry x matching settled contest). An entry with no settled contest
yet is silently skipped (not an error -- most of the week it just isn't
settled yet).

Ownership backcheck (predicted vs actual total ownership of your exact
lineup) is joined from _processed/ownership_actuals.parquet when present --
run build_ownership_dataset.py first for the fullest picture; without it,
the actual-ownership columns come back null rather than blocking the run.

Real scores (2026-09-15): the dropped-in CSV's own Points/FPTS columns are
frozen at whatever moment DK finalized that download -- per Cam, DK's own
rules (draftkings.com/help/rules/1/226) tie that moment to ownership/roster
percentages locking, not to every game in the slate having finished, so a
CSV pulled once ownership was final can predate a Sunday/Monday night game's
result entirely. Skill-position (QB/RB/WR/TE) scores are therefore computed
fresh from ESPN's box score instead (sim_replay_store.get_real_dk_scores_for_
week, the same mechanism Sim Replays uses) and only fall back to the CSV's
FPTS on a name-match miss; DST still comes from the CSV (ESPN's box score
isn't reconstructed into a DST score here -- see that function's docstring).
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import sys
from collections import defaultdict

import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.scrapers.dk_scraper import normalize_player_name  # noqa: E402
from scripts.dfs_ownership.standings_parser import (  # noqa: E402
    parse_filename, read_standings, actual_scores_from_summary, player_key,
)
from src.api.lineup_stats import get_default_payout_structure  # noqa: E402
from src.api.sim_replay_store import get_real_dk_scores_for_week  # noqa: E402

BASE = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
ARCHIVE = os.path.join(BASE, "data", "dfs_ownership")
OUT_DIR = os.path.join(ARCHIVE, "_processed")


def _lineup_actual_score(players: list, real_scores: dict, csv_scores: dict) -> tuple[float, list[str]]:
    """Sum a paper lineup's real per-player scores (CPT slot x1.5, same
    convention as the live app). `real_scores` (get_real_dk_scores_for_week,
    ESPN box-score-derived) wins over `csv_scores` (the standings CSV's own
    published summary-block FPTS) for skill positions -- see this module's
    docstring update (2026-09-15): a standings CSV can be exported at
    whatever moment DK finalizes ownership/roster percentages, which is NOT
    necessarily after every game in the slate has finished, so its FPTS can
    be frozen mid-game. csv_scores is still the ONLY source for DST (ESPN's
    box score isn't reconstructed into a DST score here -- see
    sim_replay_store.get_real_dk_scores_for_week's docstring for why) and
    the fallback for any skill player ESPN's box score doesn't have (a
    name-match miss, or a bye/inactive edge case). Returns (total, [names
    found in NEITHER source -- scored 0 in the total, surfaced so a missing
    match doesn't silently look like "played and busted"])."""
    total, missing = 0.0, []
    for p in players:
        key = player_key(p.get("name", ""), p.get("team", ""), p.get("pos", ""), normalize_player_name)
        base = real_scores.get(key)
        if base is None:
            base = csv_scores.get(key)
        if base is None:
            missing.append(p.get("name", "?"))
            base = 0.0
        mult = 1.5 if str(p.get("slot", "")).upper() == "CPT" else 1.0
        total += base * mult
    return round(total, 2), missing


def _estimated_payout(actual_rank: int | None, entry: dict, field_size: int) -> float | None:
    """A paper entry never actually sat in the real contest's field, so there
    is no real "Winnings" column to read (DK's standings export doesn't
    publish one anyway -- see read_standings' docstring). Two ways to price
    it, preferring the more exact one when available:

    1. entry["payout_tiers"] -- DK's OWN real rank-by-rank payout table (via
       dk_scraper.get_dk_contest_payout), when a real DK contest could be
       matched and attached -- e.g. Cam adapting a still-live Week 2 contest
       with the same entry fee/type for a settled Week 1 one DK's lobby no
       longer lists (2026-09-16): same top-money tiers, only the bottom
       min-cash tier's rank_end rescaled to the real (bigger) Week 1 field.
       Exact, not an approximation.
    2. Otherwise, get_default_payout_structure (src/api/lineup_stats.py) --
       the same generic contest-type-shaped curve the live app's own EV%/
       ITM% math falls back to. An estimate against a generic curve, not
       DK's real one.

    None (not 0) when the entry has neither -- e.g. an individually
    📝-flagged entry from before the Bankroll accounts work, which never
    captured prize_pool/paying_positions at all."""
    if actual_rank is None:
        return None
    real_tiers = entry.get("payout_tiers")
    if real_tiers:
        for tier in real_tiers:
            if tier["rank_start"] <= actual_rank <= tier["rank_end"]:
                return tier["payout"]
        return 0.0
    prize_pool = entry.get("prize_pool")
    paying_positions = entry.get("paying_positions")
    if not prize_pool or not paying_positions:
        return None
    structure = get_default_payout_structure(
        entry.get("contest_type") or "top_heavy", float(prize_pool), int(paying_positions), field_size)
    for tier in structure:
        if tier["rank_start"] <= actual_rank <= tier["rank_end"]:
            return tier["payout"]
    return 0.0


def _load_ownership_actuals() -> pd.DataFrame | None:
    path = os.path.join(OUT_DIR, "ownership_actuals.parquet")
    return pd.read_parquet(path) if os.path.exists(path) else None


def process_slate(folder: str, own_actuals: pd.DataFrame | None) -> list[dict]:
    entries_path = os.path.join(folder, "paper_entries.json")
    if not os.path.exists(entries_path):
        return []
    paper = (json.load(open(entries_path)) or {}).get("entries", [])
    if not paper:
        return []

    by_contest: dict = defaultdict(list)
    for e in paper:
        by_contest[e.get("contest_name")].append(e)

    slate_id = os.path.basename(folder)
    standings = [p for p in glob.glob(os.path.join(folder, "*.csv"))
                 if os.path.basename(p) != "salaries_prelock.csv"]

    # Real, freshly-computed skill-position scores for this slate's week --
    # see _lineup_actual_score's docstring for why these win over the
    # standings CSV's own (possibly ownership-lock-time-frozen) FPTS. Every
    # entry in one slate folder shares the same year/week, so this is
    # computed once per slate rather than per contest CSV. A showdown slate
    # scopes to its one game (parsed from the "showdown_<AWAY>_<HOME>"
    # folder-name convention, same as sim_replay_store._rescore_csv); a
    # classic slate (main_slate) spans the whole week.
    year = paper[0].get("year")
    week = paper[0].get("week")
    real_scores: dict = {}
    if year and week:
        game_ids = None
        if slate_id.startswith("showdown_"):
            parts = slate_id.split("_")
            if len(parts) == 3:
                game_ids = {f"{year}_{int(week):02d}_{parts[1]}_{parts[2]}"}
        real_scores = get_real_dk_scores_for_week(int(year), int(week), game_ids)

    rows: list[dict] = []
    settled_contests = set()
    for csv_path in standings:
        stem = os.path.splitext(os.path.basename(csv_path))[0]
        meta = parse_filename(stem)
        if not meta or meta["contest_name"] not in by_contest:
            continue
        our_entries = by_contest[meta["contest_name"]]
        try:
            entries_df, dk_summary = read_standings(csv_path)
        except ValueError as e:
            print(f"  {slate_id}/{stem}.csv: {e}")
            continue
        settled_contests.add(meta["contest_name"])
        field_size = int(entries_df["rank"].max()) if entries_df["rank"].notna().any() else len(entries_df)
        csv_scores = actual_scores_from_summary(dk_summary, normalize_player_name)
        has_points = "points" in entries_df.columns and entries_df["points"].notna().any()
        field_points = entries_df["points"].dropna().values if has_points else None

        own_row_by_player = {}
        if own_actuals is not None:
            sub = own_actuals[(own_actuals["slate_id"] == slate_id)
                              & (own_actuals["contest_name"] == meta["contest_name"])]
            for _, r in sub.iterrows():
                # By (name,team,pos)-derived key, not a raw name-normalize --
                # a DST in our lineup is stored as the generic sim name
                # "Defense" (disambiguated only by team), while the ownership
                # archive uses DK's real nickname ("Patriots"); player_key's
                # DST branch keys on team for exactly this reason (see
                # _lineup_actual_score, which already relies on it).
                own_row_by_player[player_key(r["player"], r["team"], r.get("pos", ""), normalize_player_name)] = r

        for e in our_entries:
            score, missing = _lineup_actual_score(e.get("players", []), real_scores, csv_scores)
            if missing:
                print(f"  {slate_id}/{meta['contest_name']}: entry {e.get('entry_id')} -- "
                      f"no settled score for {', '.join(missing)} (treated as 0)")

            if has_points:
                beat_count = int((field_points < score).sum())
                tied_count = int((field_points == score).sum())
                actual_rank = field_size - beat_count - tied_count + 1
                beat_field_pct = round(100 * beat_count / field_size, 2)  # 100 = beat everyone
                finish_percentile = round(100 * actual_rank / field_size, 2)  # lower = better ("3" = top 3%)
            else:
                actual_rank, beat_field_pct, finish_percentile = None, None, None

            model = e.get("model") or {}
            predicted_own = model.get("total_ownership")
            actual_own = None
            if own_row_by_player:
                p_keys = [player_key(p["name"], p.get("team", ""), p.get("pos", ""), normalize_player_name)
                          for p in e.get("players", [])]
                vals = [own_row_by_player[k]["total_own_pct"] for k in p_keys if k in own_row_by_player]
                if len(vals) == len(p_keys):
                    actual_own = round(sum(vals), 1)

            rows.append({
                "entry_id": e.get("entry_id"), "created_at": e.get("created_at"),
                "year": e.get("year"), "week": e.get("week"), "slate_id": slate_id,
                "slate_format": e.get("slate_format"), "source": e.get("source"), "label": e.get("label"),
                # build_id/account_id are only present on entries the Bankroll
                # accounts work bulk-registered (see app.py's
                # _bulk_register_paper_entries) -- None for an older/
                # individually 📝-flagged entry, which is fine, the Bankroll
                # rollup just won't attribute it to an account.
                "build_id": e.get("build_id"), "account_id": e.get("account_id"),
                "contest_name": meta["contest_name"], "entry_fee": meta["entry_fee"], "max_entries": meta["max_entries"],
                "field_size": field_size,
                "predicted_score": model.get("projected_score"), "actual_score": score,
                "score_diff": round(score - model.get("projected_score", 0), 2) if model.get("projected_score") is not None else None,
                "predicted_total_ownership": predicted_own, "actual_total_ownership": actual_own,
                "ownership_diff": round(actual_own - predicted_own, 1) if (actual_own is not None and predicted_own is not None) else None,
                "predicted_ev_pct": model.get("ev_pct"), "predicted_itm_pct": model.get("itm_pct"),
                "predicted_top1_pct": model.get("top1_pct"), "predicted_top01_pct": model.get("top01_pct"),
                "predicted_first_pct": model.get("first_pct"),
                "actual_rank": actual_rank, "beat_field_pct": beat_field_pct, "finish_percentile": finish_percentile,
                "estimated_payout": _estimated_payout(actual_rank, e, field_size),
                "total_salary": model.get("total_salary"), "over_salary_cap": model.get("over_salary_cap"),
                "players": json.dumps([p.get("name") for p in e.get("players", [])]),
            })

    unsettled = set(by_contest) - settled_contests
    for name in unsettled:
        print(f"  {slate_id}/{name}: {len(by_contest[name])} paper entr{'y' if len(by_contest[name])==1 else 'ies'} -- not settled yet")
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

    own_actuals = _load_ownership_actuals()
    if own_actuals is None:
        print("(no _processed/ownership_actuals.parquet yet -- run build_ownership_dataset.py "
              "first for the ownership backcheck; continuing without it)\n")

    print(f"Scanning {len(folders)} slate folder(s) for paper_entries.json...")
    all_rows: list[dict] = []
    for folder in folders:
        all_rows.extend(process_slate(folder, own_actuals))

    if not all_rows:
        print("\nNo settled paper entries found.")
        return

    df = pd.DataFrame(all_rows)
    os.makedirs(OUT_DIR, exist_ok=True)
    out_path = os.path.join(OUT_DIR, "paper_results.parquet")
    df.to_parquet(out_path, index=False)

    print(f"\nWrote {len(df)} settled paper-entry result(s) -> {os.path.relpath(out_path, BASE)}")
    print(df[["contest_name", "label", "predicted_score", "actual_score", "actual_rank",
              "field_size", "beat_field_pct"]].to_string(index=False))


if __name__ == "__main__":
    main()
