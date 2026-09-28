"""Grade the sim's player projections against what actually happened.

Roadmap: Evaluation tab, "Player Projections" section (2026-09-25) -- the
player-level companion to game_line_eval.py. Evaluated relative to our OWN
sim distribution for now; prop lines (Vegas / prediction markets) are a later
column on the same rows (see PROP_READY note in build_player_eval).

Per player-game, per stat (DK points + the counting stats in POS_STATS):
  projection  -- mean, median and the 10/25/75/90th percentiles of the
                 player's 10K sim runs
  actual      -- real stat line (player_actuals.py, nflverse)
  percentile  -- mid-rank PIT: where the actual landed inside the player's own
                 sim runs (0..1). "32.4 DK = 0.94" = better than 94% of sims.
  miss        -- actual minus sim mean
Calibration reads exactly like the game-lines PIT histogram: flat = calibrated,
pile on the right = we under-project, U-shape = our ranges are too narrow.
Range coverage: share of actuals with percentile in [0.10, 0.90] (should be
~80%) and [0.25, 0.75] (~50%) -- PIT-based so discrete stats with lots of
ties (TDs, INTs) are handled correctly.

Scope decisions (Cam, 2026-09-25):
  - Graded: QB/RB/WR/TE with a stat line. Team DST and kickers skipped for v1.
  - Projected but NO stat line -> "no stat line" list (inactive, or active
    with zero touches/targets -- nflverse lists only players who recorded a
    stat), never graded as zeros: roster-status misses stay separate from
    projection accuracy.
  - Default view hides players projected under MIN_DK_DEFAULT DK points
    (deep backups whose near-zero projections would swamp the charts).
  - Real stat lines for players we never simmed (backup QB who came in, a
    player missing from our roster) -> "unprojected" list.

Join: sim Player name -> GSIS player_id via the roster traits files (the DFS
week tree + season-long tree), exact on (game_id, player_id); normalized
name within (game_id, team) is the fallback.
"""
import glob
import json
import os
import re

import numpy as np
import pandas as pd

from src.evaluation.line_history import BASE_DIR
from src.evaluation.player_actuals import load_player_actuals

GRADED_POS = ("QB", "RB", "WR", "TE")
MIN_DK_DEFAULT = 5.0
UNPROJECTED_MIN_DK = 5.0
QUANTILES = (0.10, 0.25, 0.50, 0.75, 0.90)

# Stats worth grading per position (a QB's receiving line is noise).
POS_STATS = {
    "QB": ["dk_score", "pAtt", "pYds", "pTD", "int", "rAtt", "rYds", "rTD"],
    "RB": ["dk_score", "rAtt", "rYds", "rTD", "targets", "rec", "recYds", "recTD"],
    "WR": ["dk_score", "targets", "rec", "recYds", "recTD"],
    "TE": ["dk_score", "targets", "rec", "recYds", "recTD"],
}
ALL_STATS = ["dk_score", "pAtt", "pYds", "pTD", "int", "rAtt", "rYds", "rTD", "targets", "rec", "recYds", "recTD"]
STAT_LABELS = {"dk_score": "DK pts", "pAtt": "Pass att", "pYds": "Pass yds", "pTD": "Pass TD", "int": "INT",
               "rAtt": "Rush att", "rYds": "Rush yds", "rTD": "Rush TD", "targets": "Targets",
               "rec": "Receptions", "recYds": "Rec yds", "recTD": "Rec TD"}


def norm_name(name):
    """Inputs: player name (str). Output: lowercase alnum key with suffixes
    (Jr/Sr/II/III/IV/V) dropped -- 'Michael Penix Jr.' -> 'michaelpenix'."""
    s = re.sub(r"[^a-z ]", "", str(name).lower().replace(".", ""))
    s = re.sub(r"\b(jr|sr|ii|iii|iv|v)\b", "", s)
    return s.replace(" ", "")


def roster_id_map(year=2026, base_dir=BASE_DIR):
    """Sim name -> GSIS player_id from every roster traits file for the year.
    Inputs: year, base_dir. Output: dict {(team, name): player_id}.
    DFS tree read last so its (current-week) ids win on conflicts."""
    ids = {}
    pats = [os.path.join(base_dir, "data", "current_rosters", f"*_traits_{year}.json"),
            os.path.join(base_dir, "data", "current_rosters", "dfs", f"*_traits_{year}.json")]
    for pat in pats:
        for path in glob.glob(pat):
            try:
                doc = json.load(open(path, encoding="utf-8"))
            except (OSError, ValueError):
                continue
            team = doc.get("team") or os.path.basename(path).split("_")[0]
            for name, t in (doc.get("traits") or {}).items():
                if isinstance(t, dict) and t.get("player_id"):
                    ids[(team, name)] = t["player_id"]
    return ids


def match_to_actuals(summ, act):
    """Match sim player rows to real stat lines.

    Inputs:
      summ -- DataFrame with game_id, Team, Player, player_id (GSIS or None)
      act  -- actuals rows (player_actuals.py shape) for the same games
    Output: (matched_rows, methods) -- two lists aligned with summ's rows:
      the matched actual row (Series) or None, and 'id' | 'name' | None.
    Exact (game_id, player_id) first; else normalized name within
    (game_id, team) -- catches players missing an id in the roster files and
    suffix differences ('Michael Penix' vs 'Michael Penix Jr.').
    Shared by build_player_eval and rankings_eval."""
    act = act.copy()
    act["name_key"] = act["player_name"].map(norm_name)
    # drop=False: the matched row must still carry player_id/team as columns
    by_id = act.set_index(["game_id", "player_id"], drop=False)
    by_name = act.drop_duplicates(["game_id", "team", "name_key"]).set_index(
        ["game_id", "team", "name_key"], drop=False)
    rows, methods = [], []
    for gid, team, player, pid in zip(summ["game_id"], summ["Team"], summ["Player"], summ["player_id"]):
        if pid and (gid, pid) in by_id.index:
            src, key, how = by_id, (gid, pid), "id"
        elif (gid, team, norm_name(player)) in by_name.index:
            src, key, how = by_name, (gid, team, norm_name(player)), "name"
        else:
            rows.append(None); methods.append(None)
            continue
        hit = src.loc[key]
        rows.append(hit.iloc[0] if isinstance(hit, pd.DataFrame) else hit)
        methods.append(how)
    return rows, methods


def pit_by_group(sim, keys, stat, actual_col):
    """Mid-rank PIT per group, vectorized.
    Inputs: sim (DataFrame of per-iteration rows already merged with the
    group's actual in `actual_col`), keys (group columns), stat (sim column).
    Output: Series indexed by keys: P(sim < actual) + 0.5 * P(sim == actual)."""
    below = (sim[stat] < sim[actual_col]).astype(float)
    equal = (sim[stat] == sim[actual_col]).astype(float)
    tmp = sim[keys].copy()
    tmp["_v"] = below + 0.5 * equal
    return tmp.groupby(keys)["_v"].mean()


def summarize_week_sims(sim):
    """Per player-game projection summary from the 10K sim rows.
    Inputs: sim (players parquet rows: game_id, Team, Player, Pos, ALL_STATS).
    Output: DataFrame keyed (game_id, Team, Player, Pos) with {stat}_mean and
    {stat}_q10/_q25/_q50/_q75/_q90 for every stat in ALL_STATS."""
    keys = ["game_id", "Team", "Player", "Pos"]
    g = sim.groupby(keys)[ALL_STATS]
    out = g.mean().add_suffix("_mean")
    qs = g.quantile(list(QUANTILES))                       # index: keys + quantile level
    for q in QUANTILES:
        out = out.join(qs.xs(q, level=-1).add_suffix(f"_q{int(q * 100)}"))
    return out.reset_index()


def build_player_eval(year=2026, weeks=None, base_dir=BASE_DIR, actuals=None, sims_by_week=None):
    """Build the per player-game evaluation table.

    Inputs:
      year, weeks (None = every week with a sim file), base_dir
      actuals      -- optional pre-loaded player_actuals frame (tests)
      sims_by_week -- optional {week: players DataFrame} (tests); default reads
                      data/interim/dfs_week_{week}_players.parquet
    Output: (graded, no_stat_line, unprojected)
      graded       -- one row per QB/RB/WR/TE player-game with a real stat line:
                      ids/names/pos/team/week, proj_dk (sim mean DK), and per
                      stat: {s}_mean, {s}_q10..q90, {s}_actual, {s}_pit, {s}_miss
      no_stat_line -- projected players with no real line (proj_dk etc.)
      unprojected  -- real lines (dk >= UNPROJECTED_MIN_DK) with no sim player
    PROP_READY: a future prop line is one more {s}_line column per row; the
    grading helpers (over/under hit, edge vs. line) can reuse game_line_eval.
    """
    if actuals is None:
        actuals = load_player_actuals(year, base_dir)
    if weeks is None:
        pat = os.path.join(base_dir, "data", "interim", "dfs_week_*_players.parquet")
        weeks = sorted(int(re.search(r"week_(\d+)_", p).group(1)) for p in glob.glob(pat))
        if sims_by_week:
            weeks = sorted(sims_by_week)
    idmap = roster_id_map(year, base_dir)
    cols = ["game_id", "Team", "Player", "Pos"] + ALL_STATS

    graded, missing, unproj = [], [], []
    for wk in weeks:
        if sims_by_week is not None:
            sim = sims_by_week.get(wk)
        else:
            p = os.path.join(base_dir, "data", "interim", f"dfs_week_{wk}_players.parquet")
            sim = pd.read_parquet(p, columns=cols) if os.path.exists(p) else None
        if sim is None or sim.empty:
            continue
        sim = sim[sim["Pos"].isin(GRADED_POS)]
        summ = summarize_week_sims(sim)
        summ["week"] = wk
        summ["player_id"] = [idmap.get((t, n)) for t, n in zip(summ["Team"], summ["Player"])]

        act = actuals[actuals["game_id"].isin(summ["game_id"].unique())].copy() if len(actuals) else pd.DataFrame()
        if act.empty:
            # games not played yet (or actuals not refreshed): nothing to grade
            continue
        # Only games with real stat lines count -- an unplayed game's players
        # aren't "missing", they just haven't played yet.
        summ = summ[summ["game_id"].isin(set(act["game_id"]))].reset_index(drop=True)
        sim = sim[sim["game_id"].isin(set(act["game_id"]))]

        matched_rows, methods = match_to_actuals(summ, act)
        summ["matched_by"] = methods

        has = [r is not None for r in matched_rows]
        summ["actual_player_id"] = [r["player_id"] if r is not None else None for r in matched_rows]
        for s in ALL_STATS:
            summ[f"{s}_actual"] = [float(r[s]) if r is not None else np.nan for r in matched_rows]
        summ["proj_dk"] = summ["dk_score_mean"]

        # PIT per stat: broadcast each group's actual onto its sim rows
        g_ok = summ[has]
        keys = ["game_id", "Team", "Player", "Pos"]
        sim_ok = sim.merge(g_ok[keys + [f"{s}_actual" for s in ALL_STATS]], on=keys, how="inner")
        for s in ALL_STATS:
            pit = pit_by_group(sim_ok, keys, s, f"{s}_actual").rename(f"{s}_pit")
            g_ok = g_ok.merge(pit.reset_index(), on=keys, how="left")
            g_ok[f"{s}_miss"] = g_ok[f"{s}_actual"] - g_ok[f"{s}_mean"]
        graded.append(g_ok)
        missing.append(summ[[not h for h in has]][["week", "game_id", "Team", "Player", "Pos", "proj_dk", "player_id"]])

        # real lines nobody in our sim matched
        used = {pid for pid in summ["actual_player_id"] if pid}
        extra = act[~act["player_id"].isin(used) & act["position"].isin(GRADED_POS)
                    & (act["dk_score"] >= UNPROJECTED_MIN_DK)]
        unproj.append(extra[["week", "game_id", "team", "player_name", "position", "dk_score"]])

    cat = lambda xs: pd.concat(xs, ignore_index=True) if xs else pd.DataFrame()  # noqa: E731
    return cat(graded), cat(missing), cat(unproj)


# ── Aggregates ───────────────────────────────────────────────────────────────

def _cov(pits, lo, hi):
    p = pits.dropna()
    return round(float(((p >= lo) & (p <= hi)).mean()), 4) if len(p) else None


def _stat_block(df, s):
    """Accuracy/calibration numbers for one stat over a set of player-games."""
    pits = df[f"{s}_pit"].dropna()
    miss = df[f"{s}_miss"].dropna()
    counts, _ = np.histogram(pits, bins=np.linspace(0, 1, 11))
    return {"n": int(len(pits)),
            "cov80": _cov(pits, 0.10, 0.90), "cov50": _cov(pits, 0.25, 0.75),
            "mae": round(float(miss.abs().mean()), 2) if len(miss) else None,
            "bias": round(float(miss.mean()), 2) if len(miss) else None,
            "mean_pit": round(float(pits.mean()), 4) if len(pits) else None,
            "pit_hist": [int(c) for c in counts]}


def proj_bucket(dk):
    """Projection-size bucket (stars vs. depth) by sim mean DK points."""
    return "20+" if dk >= 20 else "15-20" if dk >= 15 else "10-15" if dk >= 10 else "5-10" if dk >= 5 else "<5"


BUCKET_ORDER = ["20+", "15-20", "10-15", "5-10", "<5"]


def summarize_players(graded, min_dk=MIN_DK_DEFAULT):
    """Season/filtered summary over graded player-games.

    Inputs: graded (build_player_eval()[0]), min_dk (hide players projected
    below this many DK points; 0 = everyone).
    Output: JSON-able dict:
      n, n_players
      by_pos   -- {pos: {stat: _stat_block}} for that position's POS_STATS
      overall  -- {dk_score: _stat_block} across all graded positions
      by_bucket -- DK stat block per projection-size bucket
      by_week  -- DK stat block per week (+ how many beat/fell short of the mean)
      repeat   -- players with >= 2 graded games: mean DK percentile + games,
                  sorted most-underprojected first (for override tuning)
    """
    df = graded[graded["proj_dk"] >= min_dk] if len(graded) else graded
    if df is None or df.empty:
        return {"n": 0}
    out = {"n": int(len(df)), "n_players": int(df["Player"].nunique()), "min_dk": min_dk,
           "overall": {"dk_score": _stat_block(df, "dk_score")},
           "by_pos": {pos: {s: _stat_block(df[df["Pos"] == pos], s) for s in POS_STATS[pos]}
                      for pos in GRADED_POS if (df["Pos"] == pos).any()}}
    b = df.assign(_b=df["proj_dk"].map(proj_bucket))
    out["by_bucket"] = [{"bucket": k, **_stat_block(b[b["_b"] == k], "dk_score")}
                        for k in BUCKET_ORDER if (b["_b"] == k).any()]
    out["by_week"] = [{"week": int(w), **_stat_block(g, "dk_score")} for w, g in df.groupby("week")]
    rep = (df.groupby(["Player", "Team", "Pos"])
             .agg(games=("dk_score_pit", "size"), mean_pit=("dk_score_pit", "mean"),
                  avg_proj=("dk_score_mean", "mean"), avg_actual=("dk_score_actual", "mean"))
             .reset_index())
    rep = rep[rep["games"] >= 2].sort_values("mean_pit", ascending=False)
    out["repeat"] = [{k: (round(float(v), 4) if isinstance(v, (float, np.floating)) else v)
                      for k, v in r.items()} for r in rep.to_dict(orient="records")]
    return out
