"""Build the DFS ownership training set from archived contest standings.

Walks every slate folder under data/dfs_ownership/<year>/week_*/, pairs each
DK "Export Full Standings" CSV with that slate's pre-lock salary snapshot, and
emits two tidy long-format tables:

    data/dfs_ownership/_processed/ownership_actuals.parquet   <- the target (y)
    data/dfs_ownership/_processed/features.parquet            <- the matrix (X), joined 1:1

Run it after dropping new standings CSVs in:

    venv\\Scripts\\python.exe scripts/dfs_ownership/build_ownership_dataset.py
    ... --year 2026 --week 1        # limit the walk

--------------------------------------------------------------------------
Standings CSV naming: <name>_<price>_<xmax>.csv   e.g. hardcount_20_5max.csv
    name   free text, the contest's identity ("hardcount", "milly", "3pointstance")
    price  entry fee in whole dollars ("20"); sub-$1 -> cents token "25c" / "50c"
    xmax   max entries per user ("5max", "150max", "1max" for single-entry)
Field size is NOT in the filename -- it's read straight off the CSV as max(Rank).

DK's standings export packs two things into one CSV, side by side:
  cols A-F   one row per entry:  Rank, EntryId, EntryName, TimeRemaining, Points, Lineup
  cols H-J   one row per player:  Player, Roster Owned %, FPTS   (DK's own totals)
We recompute ownership ourselves from the Lineup strings so we get the
CPT / FLEX split (DK only publishes the combined number for showdown) and the
realized top-lineup duplication count.
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import sys

import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.scrapers.dk_scraper import normalize_player_name  # noqa: E402
from scripts.dfs_ownership.standings_parser import (  # noqa: E402
    DST_NICKNAMES, parse_filename as _parse_filename, stakes_tier as _stakes_tier,
    field_bucket as _field_bucket, read_standings as _read_standings_raw,
    ownership_from_entries as _ownership_from_entries_raw,
)

BASE = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
ARCHIVE = os.path.join(BASE, "data", "dfs_ownership")
OUT_DIR = os.path.join(ARCHIVE, "_processed")


def _read_standings(path: str) -> tuple[pd.DataFrame, pd.DataFrame]:
    try:
        return _read_standings_raw(path)
    except ValueError as e:
        raise ValueError(f"{os.path.basename(path)}: {e}") from e


def _ownership_from_entries(entries: pd.DataFrame) -> tuple[dict, int, int]:
    return _ownership_from_entries_raw(entries, normalize_player_name)


def _load_salary_snapshot(folder: str) -> pd.DataFrame | None:
    path = os.path.join(folder, "salaries_prelock.csv")
    if not os.path.exists(path):
        return None
    sal = pd.read_csv(path)
    sal["key"] = sal.apply(
        lambda r: ("__dst__", r["team"]) if str(r.get("pos", "")).upper() == "DST"
        else (normalize_player_name(str(r["name"])), None), axis=1)
    return sal


def process_slate(folder: str) -> list[dict]:
    """One slate folder -> a list of {target + feature} rows, one per
    (contest, player). Skips silently if the folder isn't ready."""
    man_path = os.path.join(folder, "manifest.json")
    manifest = json.load(open(man_path)) if os.path.exists(man_path) else {}
    sal = _load_salary_snapshot(folder)
    if sal is None:
        print(f"  {os.path.relpath(folder, ARCHIVE)}: no salaries_prelock.csv -- skipped")
        return []

    slate_id = os.path.basename(folder)
    away, home = manifest.get("away_team"), manifest.get("home_team")
    vegas = manifest.get("vegas", {})
    implied = {home: vegas.get("home_implied"), away: vegas.get("away_implied")}

    sal_by_key = {}
    for _, r in sal.iterrows():
        sal_by_key[r["key"]] = r

    standings = glob.glob(os.path.join(folder, "*.csv"))
    standings = [p for p in standings if os.path.basename(p) != "salaries_prelock.csv"]
    if not standings:
        print(f"  {slate_id}: salary snapshot present, no standings CSV yet")
        return []

    rows: list[dict] = []
    for csv_path in standings:
        stem = os.path.splitext(os.path.basename(csv_path))[0]
        meta = _parse_filename(stem)
        if not meta:
            print(f"  {slate_id}/{stem}.csv: filename not <name>_<price>_<xmax>max -- skipped")
            continue
        try:
            entries, dk_summary = _read_standings(csv_path)
        except ValueError as e:
            print(f"  {e}")
            continue
        counts, field_size, top_dupes = _ownership_from_entries(entries)

        # DK's published split (CPT / FLEX %Drafted rows), keyed like the pool
        has_rp = dk_summary["roster_position"].astype(str).str.strip().ne("").any()
        dk_flex_own, dk_cpt_own, dk_raw_score = {}, {}, {}
        for _, sr in dk_summary.iterrows():
            nk = normalize_player_name(str(sr["player"]))
            abbr = DST_NICKNAMES.get(nk)
            k = ("__dst__", abbr) if abbr else (nk, None)
            rp = str(sr.get("roster_position", "")).upper().strip()
            pct = sr["drafted_pct"]
            if rp == "CPT":
                dk_cpt_own[k] = pct
            else:  # FLEX, or no roster-position column at all
                dk_flex_own[k] = pct
                if "fpts" in dk_summary and pd.notna(sr.get("fpts")):
                    dk_raw_score[k] = float(sr["fpts"])

        fld_bucket = _field_bucket(field_size)
        tier = _stakes_tier(meta["entry_fee"])
        own_source = "dk_published" if has_rp else "lineup_recompute"

        max_implied = max([v for v in implied.values() if v] or [0]) or 1.0
        for key, srow in sal_by_key.items():
            name, team = srow["name"], srow["team"]
            pos = str(srow.get("pos", "")).upper() or ("DST" if key[0] == "__dst__" else "")
            c = counts.get(key, {"cpt": 0, "flex": 0})
            recomp_flex = 100 * c["flex"] / field_size if field_size else 0.0
            recomp_cpt = 100 * c["cpt"] / field_size if field_size else 0.0
            if has_rp:
                flex_own = dk_flex_own.get(key, 0.0)
                cpt_own = dk_cpt_own.get(key, 0.0)
            else:
                # combined %Drafted only -> take the split from the lineups
                flex_own, cpt_own = recomp_flex, recomp_cpt
            team_implied = implied.get(team)
            rows.append({
                # ---- identity ----
                "year": manifest.get("year"), "week": manifest.get("week"),
                "slate_format": manifest.get("slate_format", "showdown"),
                "slate_id": slate_id, "away_team": away, "home_team": home,
                "contest_name": meta["contest_name"], "entry_fee": meta["entry_fee"],
                "max_entries": meta["max_entries"],
                "field_size": field_size, "field_bucket": fld_bucket, "stakes_tier": tier,
                "player": name, "team": team, "pos": pos,
                # ---- target (y) ----
                "flex_own_pct": round(flex_own, 3),
                "cpt_own_pct": round(cpt_own, 3),
                "total_own_pct": round(flex_own + cpt_own, 3),
                "own_source": own_source,
                "flex_own_pct_recomputed": round(recomp_flex, 3),
                "cpt_own_pct_recomputed": round(recomp_cpt, 3),
                "actual_dk_score": dk_raw_score.get(key),
                "top_lineup_dupe_count": top_dupes,
                # ---- features (X) — v1 value + Vegas; role/narrative joined later ----
                "salary": float(srow["salary"]), "cpt_salary": srow.get("cpt_salary"),
                "team_implied_total": team_implied,
                "game_total": vegas.get("total_line"),
                "spread_home": vegas.get("spread_line_home"),
                "is_home": team == home,
                "implied_total_rank": (
                    1 if team_implied and team_implied >= max_implied else 2) if team_implied else None,
            })
    return rows


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--year", type=int, default=None, help="limit to one season")
    ap.add_argument("--week", type=int, default=None, help="limit to one week")
    args = ap.parse_args()

    year_glob = str(args.year) if args.year else "*"
    week_glob = f"week_{args.week:02d}" if args.week else "week_*"
    folders = sorted(glob.glob(os.path.join(ARCHIVE, year_glob, week_glob, "*")))
    folders = [f for f in folders if os.path.isdir(f) and os.path.basename(f) != "_processed"]

    if not folders:
        print(f"No slate folders under {os.path.relpath(os.path.join(ARCHIVE, year_glob, week_glob), BASE)}")
        return

    print(f"Scanning {len(folders)} slate folder(s)...")
    all_rows: list[dict] = []
    for folder in folders:
        all_rows.extend(process_slate(folder))

    if not all_rows:
        print("No (salary snapshot + standings CSV) pairs found yet -- nothing written.")
        return

    df = pd.DataFrame(all_rows)
    os.makedirs(OUT_DIR, exist_ok=True)

    target_cols = ["year", "week", "slate_format", "slate_id", "away_team", "home_team",
                   "contest_name", "entry_fee", "max_entries", "field_size", "field_bucket",
                   "stakes_tier", "player", "team", "pos", "flex_own_pct", "cpt_own_pct",
                   "total_own_pct", "own_source", "flex_own_pct_recomputed",
                   "cpt_own_pct_recomputed", "actual_dk_score", "top_lineup_dupe_count"]
    feat_cols = ["year", "week", "slate_id", "contest_name", "player", "team", "pos",
                 "field_size", "field_bucket", "stakes_tier", "entry_fee", "max_entries",
                 "salary", "cpt_salary", "team_implied_total", "game_total", "spread_home",
                 "is_home", "implied_total_rank"]

    df[target_cols].to_parquet(os.path.join(OUT_DIR, "ownership_actuals.parquet"), index=False)
    df[feat_cols].to_parquet(os.path.join(OUT_DIR, "features.parquet"), index=False)

    n_contests = df.groupby(["slate_id", "contest_name"]).ngroups
    print(f"\nWrote {len(df)} rows  ({n_contests} contest(s), {df['slate_id'].nunique()} slate(s))")
    print(f"  {os.path.relpath(os.path.join(OUT_DIR, 'ownership_actuals.parquet'), BASE)}")
    print(f"  {os.path.relpath(os.path.join(OUT_DIR, 'features.parquet'), BASE)}")
    by_bucket = df.groupby(["field_bucket", "stakes_tier"]).size().reset_index(name="rows")
    print("\nCoverage (target = ~4-6 slates per bucket before training a GBM):")
    print(by_bucket.to_string(index=False))


if __name__ == "__main__":
    main()
