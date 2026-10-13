"""Team-specific pace (snap-to-snap time) and no-huddle rate: is it a persistent team trait?

Purpose: the sim's clock model draws snap-to-snap runoff from LEAGUE-wide pools keyed by
(quarter/time window x score-margin tier) -- no team ever plays faster or slower than any
other. Before wiring team pace in, test whether it exists as a stable signal:
  1. For every running-clock offensive snap (same cohort definition as
     analyze_clock_pace_grid.py), compute ratio = elapsed / league median of the play's own
     (time window x margin tier) cell, so a team isn't penalised for being behind more often.
  2. Aggregate per (team, season, situation): mean log-ratio, in three situations --
     neutral (within one score), trailing 2+ scores, leading 2+ scores -- excluding the last
     2 minutes of each half (clock strategy, already modelled explicitly).
  3. Stability: corr of a team's value in season t vs t+1, and split-half (odd/even game)
     reliability. Also the same for no-huddle rate, and no-huddle rate vs pace.
Outputs: docs/eda_outputs/team_pace/{stability.json, team_pace_2026_live.csv}
Usage: venv\\Scripts\\python.exe scripts/eda/analyze_team_pace.py
"""
import json
import os

import nfl_data_py as nfl
import numpy as np
import pandas as pd

OUT = os.path.join("docs", "eda_outputs", "team_pace")
CACHE = os.path.join("data", "interim", "_team_pace_pbp_cache.parquet")
SEASONS = list(range(2021, 2027))
COLS = ["game_id", "play_id", "season", "season_type", "week", "qtr", "game_seconds_remaining",
        "game_half", "play_type", "incomplete_pass", "out_of_bounds", "penalty", "timeout",
        "posteam", "score_differential", "no_huddle", "shotgun", "down"]


def load(refresh_year=None, completed_week=None):
    """Inputs: refresh_year/completed_week (optional) -- if given and the cached parquet has no
    plays for refresh_year at/after completed_week, that season is re-pulled and the cache rewritten
    (the weekly job passes these so the cache can't go stale). Output: slim regular-season pbp."""
    cached = pd.read_parquet(CACHE) if os.path.exists(CACHE) else None
    if cached is not None:
        stale = (refresh_year is not None and completed_week is not None
                 and cached[cached["season"] == refresh_year]["week"].max() < completed_week)
        if not stale:
            return cached
        base = cached[cached["season"] != refresh_year]
        new = nfl.import_pbp_data([refresh_year], columns=COLS, include_participation=False)
        df = pd.concat([base, new[new["season_type"] == "REG"]], ignore_index=True)
    else:
        df = nfl.import_pbp_data(SEASONS, columns=COLS, include_participation=False)
        df = df[df["season_type"] == "REG"].copy()
    os.makedirs(os.path.dirname(CACHE), exist_ok=True)
    df.to_parquet(CACHE)
    return df


def row_group(qtr, tr):
    """Same time windows as game_engine._sample_pace_runoff. tr = seconds left in the QUARTER."""
    g = np.full(len(qtr), "", dtype=object)
    g[qtr == 1] = "Q1"
    g[(qtr == 2) & (tr > 240)] = "Q2a"
    g[(qtr == 2) & (tr <= 240) & (tr > 120)] = "Q2b"
    g[(qtr == 2) & (tr <= 120)] = "Q2c"
    g[(qtr == 3) & (tr > 600)] = "Q3a"
    g[(qtr == 3) & (tr <= 600) & (tr > 300)] = "Q3b"
    g[(qtr == 3) & (tr <= 300)] = "Q3c"
    g[(qtr >= 4) & (tr > 600)] = "Q4a"
    g[(qtr >= 4) & (tr <= 600) & (tr > 300)] = "Q4b"
    g[(qtr >= 4) & (tr <= 300) & (tr > 120)] = "Q4c"
    g[(qtr >= 4) & (tr <= 120)] = "Q4d"
    return g


def margin_tier(d):
    t = np.full(len(d), "", dtype=object)
    t[d == 0] = "T"
    t[(d >= 1) & (d <= 8)] = "L1"
    t[(d >= 9) & (d <= 16)] = "L2"
    t[d >= 17] = "L3"
    t[(d <= -1) & (d >= -8)] = "T1"
    t[(d <= -9) & (d >= -16)] = "T2"
    t[d <= -17] = "T3"
    return t


def build_samples(df):
    """Running-clock offensive snaps with log(elapsed / league cell median), a situation label,
    and the no-huddle flag. Cohort = same exclusions as analyze_clock_pace_grid.py."""
    df = df.sort_values(["game_id", "play_id"]).reset_index(drop=True)
    same = (df["game_id"] == df["game_id"].shift(1)) & (df["game_half"] == df["game_half"].shift(1))
    df["elapsed"] = np.where(same, df["game_seconds_remaining"].shift(1) - df["game_seconds_remaining"], np.nan)
    stopped = ((df["incomplete_pass"].shift(1).fillna(0) == 1) | (df["out_of_bounds"].shift(1).fillna(0) == 1)
               | (df["timeout"].shift(1).fillna(0) == 1) | (df["penalty"].shift(1).fillna(0) == 1))
    poss_change = same & (df["posteam"] != df["posteam"].shift(1))
    ok = (df["play_type"].isin(["pass", "run"]) & df["posteam"].notna() & df["elapsed"].between(15, 45)   # <15s: short-gap outliers; 2025+ pbp has an unexplained spike at exactly 5s between pass/run snaps
          & ~stopped & ~poss_change & df["score_differential"].notna())
    s = df[ok].copy()
    tr = (s["game_seconds_remaining"] % 900).to_numpy()
    s["cell"] = pd.Series(row_group(s["qtr"].to_numpy(), tr), index=s.index) + "|" + pd.Series(
        margin_tier(s["score_differential"].to_numpy()), index=s.index)
    s = s[~s["cell"].str.startswith(("Q2c", "Q4d"))]           # last 2:00 of each half: modelled separately
    s = s[~s["cell"].str.contains(r"\|$")]
    med = s.groupby("cell")["elapsed"].transform("median")
    s["lr"] = np.log(s["elapsed"] / med)
    sd = s["score_differential"]
    s["sit"] = np.select([sd.abs() <= 8, sd <= -9, sd >= 9], ["neutral", "trail2", "lead2"], "")
    s = s[s["sit"] != ""]
    s["nh"] = s["no_huddle"].fillna(0)
    return s


def stability(s, col, min_n=60):
    """corr of team-season mean(col) in season t vs t+1 (teams with >= min_n plays in both), and
    split-half (odd vs even game) reliability within a season, per situation."""
    out = {}
    for sit in ["neutral", "trail2", "lead2"]:
        d = s[s["sit"] == sit]
        g = d.groupby(["posteam", "season"])[col].agg(["mean", "size"]).reset_index()
        a = g.assign(season=g["season"] + 1)
        m = g.merge(a, on=["posteam", "season"], suffixes=("", "_prev"))
        m = m[(m["size"] >= min_n) & (m["size_prev"] >= min_n)]
        d = d.assign(half=d["game_id"].map(lambda x: hash(x) % 2))
        h = d.groupby(["posteam", "season", "half"])[col].mean().unstack().dropna()
        out[sit] = {"yoy_corr": round(float(m["mean"].corr(m["mean_prev"])), 3), "yoy_n": int(len(m)),
                    "split_half_corr": round(float(h[0].corr(h[1])), 3),
                    "between_team_sd": round(float(g[g["size"] >= min_n]["mean"].std()), 4)}
    return out


def main():
    os.makedirs(OUT, exist_ok=True)
    df = load()
    s = build_samples(df)
    print(f"{len(s)} running-clock snaps, seasons {s['season'].min()}-{s['season'].max()}")
    res = {"pace_log_ratio": stability(s, "lr"), "no_huddle_rate": stability(s, "nh"),
           "corr_nohuddle_vs_pace": round(float(s.groupby(["posteam", "season"])[["lr", "nh"]].mean().corr().iloc[0, 1]), 3),
           "league_no_huddle_rate": round(float(s["nh"].mean()), 4)}
    json.dump(res, open(os.path.join(OUT, "stability.json"), "w"), indent=1)
    print(json.dumps(res, indent=1))
    live = s[s["season"] == 2026].groupby(["posteam", "sit"])[["lr", "nh"]].agg(["mean", "size"]).round(4)
    live.to_csv(os.path.join(OUT, "team_pace_2026_live.csv"))


if __name__ == "__main__":
    main()
