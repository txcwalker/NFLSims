"""Season-to-date / last-4-games versions of the four trench composite
z-scores (run_block_off_z, run_def_z, pass_block_off_z, pass_def_z) that
data/dna/trench_dna.json carries per team.

Those composites were originally built ONCE, offline, from full-season
multi-year history (scripts/eda/build_trench_dna_composites.py +
build_trench_dna_pass_composites.py, fed by analyze_run_block_metric_
correlations.py / analyze_run_defense_metrics.py / analyze_pass_block_
metrics.py) -- this module re-derives the SAME metrics, with the SAME
formulas and the SAME z-score-within-population methodology, but scoped to
just the real weeks played so far this season, so refresh_weekly_dna_v_0_1_0.py
can taper-blend them against the frozen preseason (PFF-ranking-derived)
values exactly the way it already does for every other field -- see
dna_blender_v_0_1_0.py's TAPER_SCHEDULE.

Only 4 of each EDA script's candidate metrics were actually selected into the
composites (not every metric analyze_*.py explored) -- this module computes
only those, matching build_trench_dna_composites.py's RUN_BLOCK_OFF_METRICS /
RUN_DEF_METRICS and build_trench_dna_pass_composites.py's
PASS_BLOCK_OFF_METRICS / PASS_DEF_METRICS exactly.

Data source differences from the original offline build, both intentional:
  - ybc_per_att / ybc_allowed_per_att used to come from a static external CSV
    (data/external/pfr_adv_rushing_stats.csv, capped at historical seasons,
    joined to schedules_2015_2024.csv for opponent). The LIVE
    nfl_data_py.import_weekly_pfr(s_type='rush') feed used here already
    carries rushing_yards_before_contact AND a real 'opponent' column per
    row, so no external file or schedule join is needed for that piece.
  - rush_pct_over_expected_allowed still needs an opponent join (NGS rushing
    data has no opponent column) -- done here against the live 2026 schedule
    (data/external/schedule_2026.csv), not the stale 2015-2024 file.

z-scoring needs a same-week cross-section of teams, not multiple weeks of
history -- 32 teams is a perfectly valid z-score population even at week 1.
A bye-week (or otherwise missing-data) team just gets NaN, excluded from the
mean/std like any other missing value; downstream, dna_blender treats a None
season_actual field as "use the projection value as-is," same as everywhere
else in this pipeline.
"""
from __future__ import annotations

import numpy as np
import pandas as pd

SCHEDULE_PATH = "data/external/schedule_2026.csv"

# Next Gen Stats' team_abbr uses a few codes that differ from PBP/PFR/the
# schedule (confirmed live 2026: NGS rushing says "LAR", everything else
# says "LA") -- same alias class dk_scraper.py's DK_TEAM_ALIASES handles for
# a different source. Applied to every incoming frame's team-ish columns so
# a join or a groupby never silently splits one team into two keys.
TEAM_ALIASES = {"LAR": "LA", "JAC": "JAX", "LVR": "LV", "WSH": "WAS"}


def _normalize_teams(df: pd.DataFrame, *cols: str) -> pd.DataFrame:
    df = df.copy()
    for c in cols:
        if c in df.columns:
            df[c] = df[c].replace(TEAM_ALIASES)
    return df

RUN_BLOCK_OFF_METRICS = {"ybc_per_att": 1, "rush_pct_over_expected": 1, "stuff_rate": -1, "avg_time_to_los": -1}
RUN_DEF_METRICS = {"stuff_rate_forced": 1, "aly_allowed": -1, "ybc_allowed_per_att": -1, "rush_pct_over_expected_allowed": -1}
PASS_BLOCK_OFF_METRICS = {"pressured_pct_allowed": -1, "hurry_rate_allowed": -1, "hit_rate_allowed": -1}
PASS_DEF_METRICS = {"pressure_rate_forced": 1, "hurry_rate_forced": 1, "hit_rate_forced": 1}

COMPOSITE_FIELDS = ("run_block_off_z", "run_def_z", "pass_block_off_z", "pass_def_z")
# Every field compute_trench_composites() can produce -- the 4 composites
# plus every raw metric feeding them. The weekly refresh needs this full set
# (not just the composites) to snapshot a preseason_projection baseline for
# ALL of them, so the raw inputs taper the same way the composites do rather
# than jumping straight to 100% week-1 value with no blend.
ALL_TRENCH_FIELDS = tuple(COMPOSITE_FIELDS) + tuple(RUN_BLOCK_OFF_METRICS) + tuple(RUN_DEF_METRICS) \
    + tuple(PASS_BLOCK_OFF_METRICS) + tuple(PASS_DEF_METRICS)


def _aly_value(yards: float) -> float:
    """Football Outsiders Adjusted Line Yards weighting, per play -- identical
    to analyze_run_block_metric_correlations.py's aly_value()."""
    if yards < 0:
        return yards * 1.2
    elif yards <= 4:
        return yards
    elif yards <= 10:
        return 4 + (yards - 4) * 0.5
    else:
        return 4 + 6 * 0.5


def _zscore_and_average(df: pd.DataFrame, metrics_signs: dict, out_col: str) -> pd.Series:
    """Z-score each metric across the team population in `df` (this IS the
    "within season" population the offline scripts used, just usually a
    32-row weekly cross-section here instead of a multi-year set of rows),
    sign-flip per metrics_signs so higher always means better, then average
    -- exactly build_trench_dna_composites.py's build_composite_table()."""
    z_cols = []
    for metric, sign in metrics_signs.items():
        z_col = f"_z_{metric}"
        std = df[metric].std(ddof=0)
        df[z_col] = ((df[metric] - df[metric].mean()) / std if std else 0.0) * sign
        z_cols.append(z_col)
    return df[z_cols].mean(axis=1)


def _opponent_map_2026(weeks: list[int]) -> pd.DataFrame:
    sched = pd.read_csv(SCHEDULE_PATH)
    sched = sched[(sched["game_type"] == "REG") & (sched["week"].isin(weeks))]
    a = sched[["week", "away_team", "home_team"]].rename(columns={"away_team": "team", "home_team": "opponent"})
    h = sched[["week", "home_team", "away_team"]].rename(columns={"home_team": "team", "away_team": "opponent"})
    return pd.concat([a, h], ignore_index=True)


def _run_block_off(pbp: pd.DataFrame, pfr_rush: pd.DataFrame) -> pd.DataFrame:
    runs = pbp[
        (pbp["rush_attempt"] == 1) & (pbp["qb_scramble"].fillna(0) == 0)
        & (pbp["qb_kneel"].fillna(0) == 0) & pbp["posteam"].notna()
    ].copy()
    runs["is_stuffed"] = (runs["yards_gained"] <= 0).astype(int)
    stuff = runs.groupby("posteam")["is_stuffed"].mean().rename("stuff_rate")

    ybc = pfr_rush.groupby("team").agg(
        ybc_sum=("rushing_yards_before_contact", "sum"), carries=("carries", "sum")).reset_index()
    ybc["ybc_per_att"] = ybc["ybc_sum"] / ybc["carries"].replace(0, np.nan)
    ybc = ybc.set_index("team")["ybc_per_att"]

    df = pd.DataFrame({"stuff_rate": stuff, "ybc_per_att": ybc}).reset_index().rename(columns={"index": "team"})
    return df


def _run_ngs_off(ngs_rush: pd.DataFrame) -> pd.DataFrame:
    def wavg(g, col, w="rush_attempts"):
        wsum = g[w].sum()
        return (g[col] * g[w]).sum() / wsum if wsum > 0 else np.nan

    rows = []
    for team, g in ngs_rush.groupby("team_abbr"):
        rows.append({
            "team": team,
            "rush_pct_over_expected": wavg(g, "rush_pct_over_expected"),
            "avg_time_to_los": wavg(g, "avg_time_to_los"),
        })
    return pd.DataFrame(rows)


def compute_run_block_off(pbp: pd.DataFrame, pfr_rush: pd.DataFrame, ngs_rush: pd.DataFrame) -> dict:
    """{team: {ybc_per_att, rush_pct_over_expected, stuff_rate, avg_time_to_los, run_block_off_z}}"""
    df = _run_block_off(pbp, pfr_rush).merge(_run_ngs_off(ngs_rush), on="team", how="outer")
    if df.empty:
        return {}
    df["run_block_off_z"] = _zscore_and_average(df, RUN_BLOCK_OFF_METRICS, "run_block_off_z")
    keep = ["team"] + list(RUN_BLOCK_OFF_METRICS) + ["run_block_off_z"]
    return {r["team"]: {k: (None if pd.isna(r[k]) else round(float(r[k]), 4)) for k in keep[1:]}
            for _, r in df[keep].iterrows()}


def compute_run_defense(pbp: pd.DataFrame, pfr_rush: pd.DataFrame, ngs_rush: pd.DataFrame,
                         opponent_map: pd.DataFrame) -> dict:
    """{team: {stuff_rate_forced, aly_allowed, ybc_allowed_per_att,
    rush_pct_over_expected_allowed, run_def_z}}"""
    runs = pbp[
        (pbp["rush_attempt"] == 1) & (pbp["qb_scramble"].fillna(0) == 0)
        & (pbp["qb_kneel"].fillna(0) == 0) & pbp["defteam"].notna()
    ].copy()
    runs["is_stuffed"] = (runs["yards_gained"] <= 0).astype(int)
    runs["aly_val"] = runs["yards_gained"].apply(_aly_value)
    pbp_def = runs.groupby("defteam").agg(
        stuff_rate_forced=("is_stuffed", "mean"), aly_allowed=("aly_val", "mean")
    ).reset_index().rename(columns={"defteam": "team"})

    # ybc_allowed_per_att: pfr_rush already carries a real 'opponent' column.
    ybc_def = pfr_rush.groupby("opponent").agg(
        ybc_sum=("rushing_yards_before_contact", "sum"), carries=("carries", "sum")
    ).reset_index().rename(columns={"opponent": "team"})
    ybc_def["ybc_allowed_per_att"] = ybc_def["ybc_sum"] / ybc_def["carries"].replace(0, np.nan)
    ybc_def = ybc_def[["team", "ybc_allowed_per_att"]]

    # rush_pct_over_expected_allowed: NGS has no opponent column -> join live schedule.
    ngs = ngs_rush.rename(columns={"team_abbr": "team"}).merge(opponent_map, on=["team", "week"], how="inner")

    def wavg(g, col, w="rush_attempts"):
        wsum = g[w].sum()
        return (g[col] * g[w]).sum() / wsum if wsum > 0 else np.nan

    rows = [{"team": opp, "rush_pct_over_expected_allowed": wavg(g, "rush_pct_over_expected")}
            for opp, g in ngs.groupby("opponent")]
    ngs_def = pd.DataFrame(rows)

    df = pbp_def.merge(ybc_def, on="team", how="outer").merge(ngs_def, on="team", how="outer")
    if df.empty:
        return {}
    df["run_def_z"] = _zscore_and_average(df, RUN_DEF_METRICS, "run_def_z")
    keep = ["team"] + list(RUN_DEF_METRICS) + ["run_def_z"]
    return {r["team"]: {k: (None if pd.isna(r[k]) else round(float(r[k]), 4)) for k in keep[1:]}
            for _, r in df[keep].iterrows()}


def compute_pass_block_off(pfr_pass: pd.DataFrame) -> dict:
    """{team: {pressured_pct_allowed, hurry_rate_allowed, hit_rate_allowed, pass_block_off_z}}"""
    df = pfr_pass.copy()
    if df.empty:
        return {}
    df["implied_dropbacks"] = df["times_pressured"] / df["times_pressured_pct"].replace(0, np.nan)

    def wavg(g, col):
        w = g["implied_dropbacks"].sum()
        return (g[col] * g["implied_dropbacks"]).sum() / w if w > 0 else np.nan

    rows = []
    for team, g in df.groupby("team"):
        dropbacks = g["implied_dropbacks"].sum()
        rows.append({
            "team": team,
            "pressured_pct_allowed": wavg(g, "times_pressured_pct"),
            "hurry_rate_allowed": g["times_hurried"].sum() / dropbacks if dropbacks > 0 else np.nan,
            "hit_rate_allowed": g["times_hit"].sum() / dropbacks if dropbacks > 0 else np.nan,
        })
    out = pd.DataFrame(rows)
    if out.empty:
        return {}
    out["pass_block_off_z"] = _zscore_and_average(out, PASS_BLOCK_OFF_METRICS, "pass_block_off_z")
    keep = ["team"] + list(PASS_BLOCK_OFF_METRICS) + ["pass_block_off_z"]
    return {r["team"]: {k: (None if pd.isna(r[k]) else round(float(r[k]), 4)) for k in keep[1:]}
            for _, r in out[keep].iterrows()}


def compute_pass_rush_def(pfr_pass: pd.DataFrame) -> dict:
    """{team: {pressure_rate_forced, hurry_rate_forced, hit_rate_forced, pass_def_z}}
    Opponent-join: this row's times_pressured/hurried/hit were inflicted BY
    the 'opponent' team's defense ON the 'team' passer -- group by opponent,
    same technique as the run side."""
    df = pfr_pass.copy()
    if df.empty:
        return {}
    df["implied_dropbacks"] = df["times_pressured"] / df["times_pressured_pct"].replace(0, np.nan)

    rows = []
    for opp, g in df.groupby("opponent"):
        dropbacks = g["implied_dropbacks"].sum()
        rows.append({
            "team": opp,
            "pressure_rate_forced": g["times_pressured"].sum() / dropbacks if dropbacks > 0 else np.nan,
            "hurry_rate_forced": g["times_hurried"].sum() / dropbacks if dropbacks > 0 else np.nan,
            "hit_rate_forced": g["times_hit"].sum() / dropbacks if dropbacks > 0 else np.nan,
        })
    out = pd.DataFrame(rows)
    if out.empty:
        return {}
    out["pass_def_z"] = _zscore_and_average(out, PASS_DEF_METRICS, "pass_def_z")
    keep = ["team"] + list(PASS_DEF_METRICS) + ["pass_def_z"]
    return {r["team"]: {k: (None if pd.isna(r[k]) else round(float(r[k]), 4)) for k in keep[1:]}
            for _, r in out[keep].iterrows()}


def compute_trench_composites(pbp: pd.DataFrame, pfr_pass: pd.DataFrame, pfr_rush: pd.DataFrame,
                               ngs_rush: pd.DataFrame, weeks: list[int]) -> dict:
    """All 4 composites (+ their raw inputs) for every team with data in
    `weeks`. `pbp`/`pfr_pass`/`pfr_rush`/`ngs_rush` should already be
    filtered to the desired week window (season-to-date or last-4) by the
    caller -- this just requires a consistent 'week' column for the
    opponent join. Returns {team: {...merged fields from all 4 composites...}}."""
    if not weeks or pbp.empty:
        return {}
    pbp = _normalize_teams(pbp, "posteam", "defteam")
    pfr_pass = _normalize_teams(pfr_pass, "team", "opponent")
    pfr_rush = _normalize_teams(pfr_rush, "team", "opponent")
    ngs_rush = _normalize_teams(ngs_rush, "team_abbr")
    opp_map = _opponent_map_2026(weeks)
    parts = [
        compute_run_block_off(pbp, pfr_rush, ngs_rush),
        compute_run_defense(pbp, pfr_rush, ngs_rush, opp_map),
        compute_pass_block_off(pfr_pass),
        compute_pass_rush_def(pfr_pass),
    ]
    teams = set()
    for p in parts:
        teams |= set(p)
    merged = {}
    for team in teams:
        d = {}
        for p in parts:
            d.update(p.get(team, {}))
        merged[team] = d
    return merged
