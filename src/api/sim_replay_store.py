"""Sim Replays -- "how did MY submitted lineups do against what our sim
projected for them," using the DK standings CSVs already archived under
data/dfs_ownership/ (see that folder's README) as the source of both your
own entries (filtered by EntryName) and their real scored Points -- no
separate "my entries" export or real-stats pipeline needed, since DK's
standings export already carries both.

Deliberately NOT a full field/ownership analysis (see eval_field.py /
sim_replay_field.py for that) -- this only ever looks at rows whose
EntryName matches your own configured username(s), and compares each one
against our sim's own projected score distribution for that exact lineup
(scripts/dfs_ownership/sim_replay_field.py's per-player sim-score machinery,
reused here rather than duplicated).

Config: data/dfs_ownership/config.json -- {"my_usernames": ["handle", ...]}.
DK's EntryName is "<username>" (single-entry) or "<username> (n/m)"
(multi-entry contest); matching strips the "(n/m)" suffix before comparing.
"""
from __future__ import annotations

import glob
import json
import os
import re
import time
from typing import Any, Dict, List, Optional

import numpy as np
import pandas as pd

from src.scrapers.dk_scraper import normalize_player_name
from scripts.dfs_ownership.standings_parser import parse_filename, slot_re_for, resolve_lineup_name, player_key
from scripts.dfs_ownership.sim_replay_field import _load_sim_scores, _lineup_score_vector  # noqa: F401 (reused sim-scoring machinery)
from src.api.lineup_stats import get_default_payout_structure, compute_lineup_field_stats_batch

# Rescoring an entire real field means building one (field_size, n_sims) score
# matrix -- at n_sims=10,000 that's field_size*40KB (float32), plus this app's
# ranking math (lineup_stats.compute_lineup_field_stats_batch) holds a couple
# more arrays of the same shape at once. Fine up to a few thousand entries
# (Cam: "screenpass ... under 6K entries" as the intended starting point);
# a real Millionaire-sized field (100K+) needs the dedup-by-composition +
# weighting approach sim_replay_field.py already uses before this would be
# safe to lift. Until that's built, refuse rather than silently hang/OOM.
MAX_FIELD_SIZE_FOR_RESCORE = 10_000

BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
ARCHIVE_DIR = os.path.join(BASE_DIR, "data", "dfs_ownership")
CONFIG_PATH = os.path.join(ARCHIVE_DIR, "config.json")
SCHEDULE_PATH = os.path.join(BASE_DIR, "data", "external", "schedule_2026.csv")

_ENTRY_NAME_SUFFIX_RE = re.compile(r"\s*\(\d+/\d+\)\s*$")

# {(year, week): (fetched_at, {player_key: real_dk_score})} -- a settled
# contest's real final score is what Cam actually wants here (2026-09-15:
# "calculate the final scores from the API endpoints for the games we are
# using on the other site" -- i.e. reuse the same ESPN-backed box-score path
# /api/games/{game_id}/player-stats already uses, instead of trusting the
# standings CSV's baked-in Points/FPTS, which is frozen at whatever moment
# the CSV was downloaded and can be stale if that was before the slate
# closed). Short TTL rather than "forever" so a reload after a still-live
# game finishes picks up the final number without a backend restart.
_REAL_SCORES_CACHE: Dict[tuple, tuple] = {}
_REAL_SCORES_CACHE_TTL_S = 30.0

# Categories ESPN's boxscore.players publishes that feed calculate_fantasy_points
# (src/nfl_sim/scoring.py) -- passing/rushing/receiving/fumbles cover every
# offensive skill-position field that formula reads. DST scoring (sacks,
# INTs, fumble recoveries, defensive/ST TDs, points allowed) is NOT
# reconstructed here -- DK's own real DST FPTS (from the standings CSV's
# summary block, already parsed for ownership) is used instead, since
# getting a from-scratch DST formula subtly wrong (return-TD double
# counting, safeties, blocked kicks) is a worse failure mode than a number
# that's occasionally stale by a few hours.
_ESPN_SKILL_CATEGORIES = ("passing", "rushing", "receiving", "fumbles")


def _espn_num(v) -> float:
    if v is None:
        return 0.0
    try:
        return float(str(v).replace(",", "").strip())
    except ValueError:
        return 0.0


def _espn_stat(stats: list, labels: list, label: str) -> Optional[str]:
    try:
        return stats[labels.index(label)]
    except (ValueError, IndexError):
        return None


def _fetch_real_dk_scores_for_game(espn_event_id: str) -> Dict[tuple, float]:
    """{(normalized_name, None): real_dk_score} for every skill player (QB/
    RB/WR/TE) in one game, computed from ESPN's live/final box score via the
    same calculate_fantasy_points formula the sim itself uses -- so this is
    "what DK actually paid," not our own sim's projection, for whichever
    game has actually finished (or is in progress; ESPN's box score updates
    live, so a reload mid-game just reflects the current running total)."""
    from src.live.main import get_game_pbp
    from src.nfl_sim.scoring import calculate_fantasy_points

    pbp = get_game_pbp(str(espn_event_id))
    if not pbp:
        return {}
    boxscore = pbp.get("boxscore", {}) or {}
    per_player: Dict[str, Dict[str, float]] = {}
    for team_block in boxscore.get("players", []) or []:
        for cat in team_block.get("statistics", []) or []:
            name = cat.get("name")
            if name not in _ESPN_SKILL_CATEGORIES:
                continue
            labels = cat.get("labels", []) or []
            for a in cat.get("athletes", []) or []:
                athlete_name = ((a.get("athlete") or {}).get("displayName") or "").strip()
                if not athlete_name:
                    continue
                stats = a.get("stats", []) or []
                d = per_player.setdefault(athlete_name, {})
                if name == "passing":
                    d["pYds"] = _espn_num(_espn_stat(stats, labels, "YDS"))
                    d["pTD"] = _espn_num(_espn_stat(stats, labels, "TD"))
                    d["int"] = _espn_num(_espn_stat(stats, labels, "INT"))
                elif name == "rushing":
                    d["rYds"] = _espn_num(_espn_stat(stats, labels, "YDS"))
                    d["rTD"] = _espn_num(_espn_stat(stats, labels, "TD"))
                elif name == "receiving":
                    d["rec"] = _espn_num(_espn_stat(stats, labels, "REC"))
                    d["recYds"] = _espn_num(_espn_stat(stats, labels, "YDS"))
                    d["recTD"] = _espn_num(_espn_stat(stats, labels, "TD"))
                elif name == "fumbles":
                    d["fumbles"] = _espn_num(_espn_stat(stats, labels, "LOST"))

    return {(normalize_player_name(name), None): calculate_fantasy_points(stat_dict, "DK")
            for name, stat_dict in per_player.items()}


def get_real_dk_scores_for_week(year: int, week: int, game_ids: Optional[set] = None) -> Dict[tuple, float]:
    """Merged {(normalized_name, None): real_dk_score} across every REG game
    in `game_ids` (or the whole week if None) that week's schedule has an
    ESPN event id for. Cached briefly per (year, week, game_ids) so building
    a classic slate's full-week lookup (up to 16 ESPN calls) isn't repeated
    on every request."""
    key = (year, week, tuple(sorted(game_ids)) if game_ids else None)
    now = time.time()
    cached = _REAL_SCORES_CACHE.get(key)
    if cached and (now - cached[0]) < _REAL_SCORES_CACHE_TTL_S:
        return cached[1]

    if not os.path.exists(SCHEDULE_PATH):
        return {}
    sched = pd.read_csv(SCHEDULE_PATH)
    games = sched[(sched["week"] == week) & (sched["game_type"] == "REG")]
    if game_ids:
        games = games[games["game_id"].isin(game_ids)]

    merged: Dict[tuple, float] = {}
    for _, row in games.iterrows():
        espn_id = row.get("espn")
        if pd.isna(espn_id):
            continue
        merged.update(_fetch_real_dk_scores_for_game(int(espn_id)))

    _REAL_SCORES_CACHE[key] = (now, merged)
    return merged

# Scan-result cache: signature (sorted (path, mtime) tuples of every CSV
# considered) -> contest list. A file being added/removed/rewritten changes
# the signature, so this self-invalidates without a background watcher.
_CONTESTS_CACHE: Dict[tuple, List[Dict[str, Any]]] = {}


def load_my_usernames() -> List[str]:
    if not os.path.exists(CONFIG_PATH):
        return []
    try:
        with open(CONFIG_PATH, "r", encoding="utf-8") as f:
            cfg = json.load(f)
    except (json.JSONDecodeError, OSError):
        return []
    names = cfg.get("my_usernames") or []
    return [str(n) for n in names if str(n).strip()]


def _base_username(entry_name: str) -> str:
    return _ENTRY_NAME_SUFFIX_RE.sub("", str(entry_name or "")).strip()


def _is_mine(entry_name: str, usernames_lower: set) -> bool:
    return _base_username(entry_name).lower() in usernames_lower


def _standings_csvs() -> List[str]:
    """Every standings CSV under the archive, excluding salary snapshots and
    the offline-pipeline's own _processed/ output."""
    paths = glob.glob(os.path.join(ARCHIVE_DIR, "*", "week_*", "*", "*.csv"))
    return [p for p in paths if os.path.basename(p) != "salaries_prelock.csv"]


def _slate_meta(slate_dir: str) -> Dict[str, Any]:
    man_path = os.path.join(slate_dir, "manifest.json")
    if os.path.exists(man_path):
        try:
            with open(man_path, "r", encoding="utf-8") as f:
                return json.load(f)
        except (json.JSONDecodeError, OSError):
            pass
    return {}


def list_my_contests(year: Optional[int] = None, week: Optional[int] = None) -> List[Dict[str, Any]]:
    """Every contest with at least one of your entries, newest slate first.
    Reads only the columns needed to check EntryName + count rows -- still an
    O(file size) parse per CSV (some are 80MB+), but cached by a (path,
    mtime) signature so a page sitting on the picker doesn't re-scan."""
    usernames = load_my_usernames()
    if not usernames:
        return []
    usernames_lower = {u.lower() for u in usernames}

    csv_paths = _standings_csvs()
    if year is not None:
        csv_paths = [p for p in csv_paths if f"{os.sep}{year}{os.sep}" in p]
    if week is not None:
        csv_paths = [p for p in csv_paths if f"{os.sep}week_{int(week):02d}{os.sep}" in p]

    sig = tuple(sorted((p, os.path.getmtime(p)) for p in csv_paths))
    if sig in _CONTESTS_CACHE:
        return _CONTESTS_CACHE[sig]

    results: List[Dict[str, Any]] = []
    for csv_path in csv_paths:
        stem = os.path.splitext(os.path.basename(csv_path))[0]
        meta = parse_filename(stem)
        if not meta:
            continue
        try:
            df = pd.read_csv(csv_path, dtype=str, keep_default_na=False, encoding="utf-8-sig",
                              usecols=lambda c: c.strip().lstrip("﻿").lower() in ("rank", "entryname"))
        except (ValueError, pd.errors.ParserError):
            continue
        cols = {c.strip().lstrip("﻿").lower(): c for c in df.columns}
        name_col, rank_col = cols.get("entryname"), cols.get("rank")
        if not name_col or not rank_col:
            continue
        mine = df[df[name_col].apply(lambda n: _is_mine(n, usernames_lower))]
        if mine.empty:
            continue

        slate_dir = os.path.dirname(csv_path)
        slate_id = os.path.basename(slate_dir)
        week_dir = os.path.basename(os.path.dirname(slate_dir))
        year_dir = os.path.basename(os.path.dirname(os.path.dirname(slate_dir)))
        manifest = _slate_meta(slate_dir)
        ranks = pd.to_numeric(df[rank_col], errors="coerce")

        results.append({
            "year": manifest.get("year", int(year_dir) if year_dir.isdigit() else None),
            "week": manifest.get("week", int(week_dir.replace("week_", "")) if week_dir.startswith("week_") else None),
            "slate_id": slate_id,
            "slate_format": manifest.get("slate_format", "showdown" if slate_id.startswith("showdown") else "classic"),
            "away_team": manifest.get("away_team"), "home_team": manifest.get("home_team"),
            "contest_name": meta["contest_name"], "entry_fee": meta["entry_fee"], "max_entries": meta["max_entries"],
            "my_entry_count": int(len(mine)),
            "field_size": int(ranks.max()) if ranks.notna().any() else len(df),
        })

    results.sort(key=lambda r: (r.get("year") or 0, r.get("week") or 0, r["slate_id"], r["contest_name"]), reverse=True)
    _CONTESTS_CACHE[sig] = results
    return results


def _find_csv(year: int, week: int, slate_id: str, contest_name: str) -> Optional[str]:
    slate_dir = os.path.join(ARCHIVE_DIR, str(year), f"week_{int(week):02d}", slate_id)
    if not os.path.isdir(slate_dir):
        return None
    for p in glob.glob(os.path.join(slate_dir, "*.csv")):
        if os.path.basename(p) == "salaries_prelock.csv":
            continue
        meta = parse_filename(os.path.splitext(os.path.basename(p))[0])
        if meta and meta["contest_name"] == contest_name:
            return p
    return None


def get_contest_entries(year: int, week: int, slate_id: str, contest_name: str) -> Dict[str, Any]:
    """Your entries in one contest: real lineup + actual Points/rank (straight
    off the standings CSV) plus our sim's own projected score distribution
    for that exact lineup, and where the actual score landed inside it."""
    usernames = load_my_usernames()
    usernames_lower = {u.lower() for u in usernames}
    csv_path = _find_csv(year, week, slate_id, contest_name)
    if not csv_path:
        return {"entries": [], "error": "contest not found"}

    slate_dir = os.path.dirname(csv_path)
    manifest = _slate_meta(slate_dir)
    slate_format = manifest.get("slate_format", "showdown" if slate_id.startswith("showdown") else "classic")

    df = pd.read_csv(csv_path, dtype=str, keep_default_na=False, encoding="utf-8-sig")
    cols = {c.strip().lstrip("﻿").lower(): c for c in df.columns}
    name_col, rank_col, points_col, lineup_col, id_col = (
        cols.get("entryname"), cols.get("rank"), cols.get("points"), cols.get("lineup"), cols.get("entryid"))
    if not all([name_col, rank_col, lineup_col]):
        return {"entries": [], "error": "not a standings export (missing Rank/EntryName/Lineup)"}

    field_size = int(pd.to_numeric(df[rank_col], errors="coerce").max())
    mine = df[df[name_col].apply(lambda n: _is_mine(n, usernames_lower))].copy()
    if mine.empty:
        return {"entries": [], "field_size": field_size}

    slot_re = slot_re_for(slate_format)

    # Resolve every player key this contest's standings mentions to a
    # display (name, team, pos) via the week's own sim parquet -- the
    # Lineup string only ever gives a bare name or DST nickname.
    parsed_rows = []
    needed_keys = set()
    for _, r in mine.iterrows():
        slots = []
        for slot, raw in slot_re.findall(r[lineup_col]):
            key = resolve_lineup_name(raw.strip(), normalize_player_name)
            needed_keys.add(key)
            slots.append({"slot": slot.upper(), "raw_name": raw.strip(), "key": key})
        parsed_rows.append({
            "entry_id": r[id_col] if id_col else None,
            "rank": int(float(r[rank_col])) if r[rank_col] else None,
            "actual_points": float(r[points_col]) if points_col and r[points_col] else None,
            "slots": slots,
        })

    sim_scores, n_iterations = _load_sim_scores(int(week), needed_keys)

    # name/team/pos display lookup, same parquet, cheap (a few hundred distinct combos)
    display: Dict[tuple, Dict[str, str]] = {}
    if n_iterations:
        players_path = os.path.join(BASE_DIR, "data", "interim", f"dfs_week_{int(week)}_players.parquet")
        if os.path.exists(players_path):
            combos = pd.read_parquet(players_path, columns=["Player", "Team", "Pos"]).drop_duplicates()
            for _, c in combos.iterrows():
                k = player_key(c["Player"], c["Team"], c["Pos"], normalize_player_name)
                display[k] = {"name": c["Player"], "team": c["Team"], "pos": c["Pos"]}

    entries = []
    for row in parsed_rows:
        lineup_keys = [(s["key"], 1.5 if s["slot"] == "CPT" else 1.0) for s in row["slots"]]
        sim_vec, missing = _lineup_score_vector(lineup_keys, sim_scores, n_iterations) if n_iterations else (np.array([]), [])

        players = []
        for s in row["slots"]:
            d = display.get(s["key"], {})
            players.append({
                "slot": s["slot"],
                "name": d.get("name", s["raw_name"]),
                "team": d.get("team"), "pos": d.get("pos"),
            })

        sim_summary = None
        if len(sim_vec):
            mean = float(sim_vec.mean())
            sim_summary = {
                "mean": round(mean, 2),
                "p10": round(float(np.percentile(sim_vec, 10)), 2),
                "p25": round(float(np.percentile(sim_vec, 25)), 2),
                "p50": round(float(np.percentile(sim_vec, 50)), 2),
                "p75": round(float(np.percentile(sim_vec, 75)), 2),
                "p90": round(float(np.percentile(sim_vec, 90)), 2),
                "floor": round(float(sim_vec.min()), 2),
                "ceiling": round(float(sim_vec.max()), 2),
                "n_iterations": n_iterations,
                "missing_players": missing,
            }
            if row["actual_points"] is not None:
                sim_summary["actual_vs_sim_percentile"] = round(
                    float((sim_vec <= row["actual_points"]).mean() * 100), 1)

        entries.append({
            "entry_id": row["entry_id"], "rank": row["rank"], "field_size": field_size,
            "field_percentile": round(100.0 * row["rank"] / field_size, 2) if row["rank"] else None,
            "actual_points": row["actual_points"],
            "players": players,
            "sim": sim_summary,
        })

    entries.sort(key=lambda e: (e["rank"] is None, e["rank"]))
    return {
        "entries": entries, "field_size": field_size,
        "contest_name": contest_name, "slate_format": slate_format,
        "away_team": manifest.get("away_team"), "home_team": manifest.get("home_team"),
    }


# {(csv_path, mtime, contest_type, paying_positions, top_pct): (fetched_at, result)}
# -- scoring the whole field (not just mine + top 1%) is real work at a few
# thousand entries (~20-25s for ~5,900), so a plain reload/settings-panel
# open shouldn't pay that again. Short TTL (not "forever") so it still picks
# up a re-grabbed/overwritten CSV (new mtime -> new key, self-invalidates)
# and get_real_dk_scores_for_week's own still-live-game updates.
_FIELD_STATS_CACHE: Dict[tuple, tuple] = {}
_FIELD_STATS_CACHE_TTL_S = 60.0


def get_contest_field_stats(
    year: int, week: int, slate_id: str, contest_name: str,
    contest_type: str = "top_heavy", paying_positions: Optional[int] = None,
    top_pct: float = 1.0,
) -> Dict[str, Any]:
    """Cached wrapper around _compute_contest_field_stats -- see that
    docstring for what this actually does."""
    csv_path = _find_csv(year, week, slate_id, contest_name)
    if not csv_path:
        return {"entries": [], "error": "contest not found"}
    cache_key = (csv_path, os.path.getmtime(csv_path), contest_type, paying_positions, top_pct)
    now = time.time()
    cached = _FIELD_STATS_CACHE.get(cache_key)
    if cached and (now - cached[0]) < _FIELD_STATS_CACHE_TTL_S:
        return cached[1]
    result = _compute_contest_field_stats(year, week, slate_id, contest_name, csv_path, contest_type, paying_positions, top_pct)
    _FIELD_STATS_CACHE[cache_key] = (now, result)
    return result


def _compute_contest_field_stats(
    year: int, week: int, slate_id: str, contest_name: str, csv_path: str,
    contest_type: str, paying_positions: Optional[int], top_pct: float,
) -> Dict[str, Any]:
    """Solver-style "Range of Outcomes" stats (Sim ROI/Cash Rate/Ceiling/
    Floor/Top1%/1st Place/histogram, see lineup_stats.compute_lineup_field_stats_batch)
    for your own entries plus the real top `top_pct`% of the field, computed by
    rescoring EVERY real entrant's REAL roster with our sim and ranking against
    each other -- not a synthetic field, the field this contest actually had.

    No real DK payout table is archived for a settled contest (same known gap
    sim_replay_field.py documents), so `contest_type`/`paying_positions` pick a
    default payout curve (lineup_stats.get_default_payout_structure) the same
    way a fresh /api/optimize call would if you didn't override it -- pass
    real values once you've looked up the actual contest structure.
    """
    usernames = load_my_usernames()
    usernames_lower = {u.lower() for u in usernames}

    slate_dir = os.path.dirname(csv_path)
    manifest = _slate_meta(slate_dir)
    slate_format = manifest.get("slate_format", "showdown" if slate_id.startswith("showdown") else "classic")

    # Which REG game(s) this slate's real final scores should come from --
    # a showdown slate is exactly the one game in its manifest; classic
    # spans the whole week (get_real_dk_scores_for_week(..., game_ids=None)
    # below pulls every REG game that week).
    game_ids = None
    if slate_format == "showdown" and manifest.get("away_team") and manifest.get("home_team"):
        game_ids = {f"{year}_{int(week):02d}_{manifest['away_team']}_{manifest['home_team']}"}
    real_scores = get_real_dk_scores_for_week(year, week, game_ids)

    df = pd.read_csv(csv_path, dtype=str, keep_default_na=False, encoding="utf-8-sig")
    cols = {c.strip().lstrip("﻿").lower(): c for c in df.columns}
    name_col, rank_col, points_col, lineup_col, id_col = (
        cols.get("entryname"), cols.get("rank"), cols.get("points"), cols.get("lineup"), cols.get("entryid"))
    if not all([name_col, rank_col, lineup_col]):
        return {"entries": [], "error": "not a standings export (missing Rank/EntryName/Lineup)"}

    # Real published ownership, from the same CSV's side-by-side summary
    # block (cols H-J: Player, Roster Position, %Drafted, FPTS -- see
    # standings_parser.read_standings' docstring). BOTH slate formats split
    # ownership by slot for a player eligible in more than one -- showdown's
    # Roster Position holds "CPT"/"FLEX", classic's holds the player's
    # natural slot ("RB"/"WR"/...) OR "FLEX" when he's flexed in, each with
    # its own %Drafted (e.g. real 2026-09-15 data: Jahmyr Gibbs "RB" 52.81%
    # vs "FLEX" 3.30% -- treating those as one number silently picks
    # whichever row happens to load last, undercounting total ownership).
    # So ownership is always looked up by (player, the exact slot label the
    # real Lineup string used) -- conveniently the same vocabulary
    # (QB/RB/WR/TE/FLEX/DST or CPT/FLEX) row_slots already parses lineups
    # into, so no per-format branching is needed at lookup time either.
    ownership_lookup: Dict[tuple, Dict[str, float]] = {}
    # DK's own real per-player fantasy score, published in the same summary
    # block -- used as the actual_points source for DST (not reconstructed
    # from ESPN below, see get_real_dk_scores_for_week's docstring) and as a
    # fallback for any skill player ESPN's box score doesn't have. FPTS
    # doesn't vary by slot the way ownership does (same real stats either
    # way) EXCEPT showdown's CPT row, which is pre-multiplied by 1.5 -- that
    # row is skipped so the 1.5x is applied exactly once, consistently, when
    # a lineup's actual_points is summed below.
    fpts_lookup: Dict[tuple, float] = {}
    player_col = cols.get("player")
    drafted_col = next((cols[k] for k in cols if "drafted" in k or "owned" in k), None)
    fpts_col = cols.get("fpts")
    rp_col = cols.get("roster position")
    if player_col and drafted_col:
        summ_cols = [player_col, drafted_col] + ([rp_col] if rp_col else []) + ([fpts_col] if fpts_col else [])
        summ = df[summ_cols].copy()
        summ = summ[summ[player_col].str.strip() != ""]
        for _, sr in summ.iterrows():
            k = resolve_lineup_name(sr[player_col].strip(), normalize_player_name)
            rp = str(sr[rp_col]).strip().upper() if rp_col else ""
            try:
                pct = float(str(sr[drafted_col]).replace("%", "").strip())
                ownership_lookup.setdefault(k, {})[rp] = pct
            except ValueError:
                pass
            if fpts_col and rp != "CPT" and k not in fpts_lookup:
                try:
                    fpts_lookup[k] = float(sr[fpts_col])
                except ValueError:
                    pass

    df = df[df[lineup_col].str.strip() != ""].copy()
    df["_rank"] = pd.to_numeric(df[rank_col], errors="coerce")
    field_size = int(df["_rank"].max())
    if field_size > MAX_FIELD_SIZE_FOR_RESCORE:
        return {"entries": [], "field_size": field_size,
                "error": f"field has {field_size:,} entries -- rescoring the full real field is only "
                         f"supported up to {MAX_FIELD_SIZE_FOR_RESCORE:,} for now (needs dedup-by-composition "
                         f"+ weighting for larger fields, see sim_replay_field.py for that approach)."}

    slot_re = slot_re_for(slate_format)
    # Parse every real Lineup string once into (slot, raw_name, key) triples --
    # reused below for both sim scoring and the display player list, rather
    # than re-parsing the same string for each.
    row_slots: List[List[tuple]] = []
    needed_keys = set()
    for lu in df[lineup_col]:
        parsed = [(slot.upper(), raw.strip(), resolve_lineup_name(raw.strip(), normalize_player_name))
                  for slot, raw in slot_re.findall(lu)]
        row_slots.append(parsed)
        needed_keys.update(k for _, _, k in parsed)

    sim_scores, n_iterations = _load_sim_scores(int(week), needed_keys)
    if not n_iterations:
        return {"entries": [], "field_size": field_size,
                "error": f"no week-{week} sim found -- run scripts/simulation_runners/run_week_sim_2026.py {week} first"}

    display: Dict[tuple, Dict[str, str]] = {}
    players_path = os.path.join(BASE_DIR, "data", "interim", f"dfs_week_{int(week)}_players.parquet")
    if os.path.exists(players_path):
        combos = pd.read_parquet(players_path, columns=["Player", "Team", "Pos"]).drop_duplicates()
        for _, c in combos.iterrows():
            k = player_key(c["Player"], c["Team"], c["Pos"], normalize_player_name)
            display[k] = {"name": c["Player"], "team": c["Team"], "pos": c["Pos"]}

    # Every real entry's per-iteration score -- CPT is the only 1.5x slot.
    field_matrix = np.zeros((len(df), n_iterations), dtype=np.float64)
    for i, parsed in enumerate(row_slots):
        keyed = [(k, 1.5 if slot == "CPT" else 1.0) for slot, _raw, k in parsed]
        field_matrix[i], _missing = _lineup_score_vector(keyed, sim_scores, n_iterations)

    is_mine = df[name_col].apply(lambda n: _is_mine(n, usernames_lower)).to_numpy()
    top_cutoff = max(1, int(np.ceil(field_size * top_pct / 100.0)))
    is_top = (df["_rank"].to_numpy() <= top_cutoff)
    selected = np.where(is_mine | is_top)[0]
    if len(selected) == 0:
        return {"entries": [], "field_size": field_size}

    if paying_positions is None:
        paying_positions = max(1, round(field_size * 0.20))
    entry_fee = parse_filename(os.path.splitext(os.path.basename(csv_path))[0])["entry_fee"]
    prize_pool = entry_fee * field_size * 0.85
    payout_structure = get_default_payout_structure(contest_type, prize_pool, paying_positions, field_size)

    lineup_draws_list = [field_matrix[i] for i in selected]
    stats_list = compute_lineup_field_stats_batch(
        lineup_draws_list, field_matrix, payout_structure, entry_fee, field_size, paying_positions)

    entries = []
    for idx, stats in zip(selected, stats_list):
        row = df.iloc[idx]
        players = [{"slot": slot, "name": display.get(k, {}).get("name", raw),
                    "team": display.get(k, {}).get("team"), "pos": display.get(k, {}).get("pos")}
                   for slot, raw, k in row_slots[idx]]

        total_ownership = None
        if ownership_lookup:
            total_ownership = 0.0
            for slot, _raw, k in row_slots[idx]:
                total_ownership += ownership_lookup.get(k, {}).get(slot, 0.0)
            total_ownership = round(total_ownership, 1)

        # Real final points, computed fresh from ESPN's box score (skill
        # positions) + DK's own published DST FPTS -- NOT the CSV's frozen
        # Points column, which is only as fresh as whenever that CSV was
        # downloaded (see WORKLOG 2026-09-15). A player found in neither
        # source (inactive, or a name-match miss) scores 0 for that slot,
        # same "missing treated as 0" convention _lineup_score_vector uses.
        actual_points = 0.0
        any_real_source = False
        for slot, _raw, k in row_slots[idx]:
            mult = 1.5 if slot == "CPT" else 1.0
            pts = real_scores.get(k)
            if pts is None:
                pts = fpts_lookup.get(k)
            if pts is not None:
                any_real_source = True
            actual_points += mult * (pts or 0.0)
        if not any_real_source and points_col and row[points_col]:
            # Neither source had anyone in this lineup -- fall back to the
            # CSV's own (possibly stale) total rather than reporting a false 0.
            actual_points = float(row[points_col])
        actual_points = round(actual_points, 2)

        real_rank = int(row["_rank"]) if pd.notna(row["_rank"]) else None

        entries.append({
            **stats,
            "entry_id": row[id_col] if id_col else None,
            "rank": real_rank,
            "actual_points": actual_points,
            "is_mine": bool(is_mine[idx]),
            "total_ownership": total_ownership,
            "players": players,
        })

    entries.sort(key=lambda e: (e["rank"] is None, e["rank"]))
    return {
        "entries": entries, "field_size": field_size, "n_iterations": n_iterations,
        "paying_positions": paying_positions, "contest_type": contest_type,
        "contest_name": contest_name, "slate_format": slate_format,
        "away_team": manifest.get("away_team"), "home_team": manifest.get("home_team"),
    }
