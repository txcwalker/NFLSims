"""Glue between window_blend_v_0_1_0 (pure math) and the weekly refresh
(scripts/roster_management/refresh_weekly_dna_v_0_1_0.py, `--blend-mode window`).

Everything here only RETURNS values; the refresh script decides whether to use
them, so the default (legacy) path is untouched. Settings come from the
2024+2025 backtest (docs/eda_outputs/window_blend_backtest/):

  - flat fields  : window_blend(scheme=FLAT_SCHEME) then keep PRESEASON_ANCHOR[class]
                   of the preseason value -- shares 15%, efficiency ratios 50%
                   (the anchor hurts shares, helps ratios).
  - zone shares  : red-zone / goal-line target_share + carry_share pooled over the
                   last ZONE_WINDOW team games, no gate, no anchor.
  - team rates   : def_pressure_rate / def_sack_rate / sack_rate_allowed, same
                   window blend with a 50% preseason anchor.
Not covered (stay on the legacy blend): rookies, NGS fields, zone catch_rate/cpoe,
the trench composite z-scores. Teammate opportunity shifts are NOT modelled --
Cam hand-tunes those in data/overrides/2026/week_NN.
"""
from collections import defaultdict

import pandas as pd

from src.data_pipeline import window_blend_v_0_1_0 as wb
from src.data_pipeline.rolling_stats_v_0_1_0 import (
    PLAYER_RATE_FIELDS, TEAM_RATE_FIELDS, build_player_game_log, build_team_game_log, classify_zone,
)

FLAT_SCHEME = "flat"
FILL = "mean"
PRESEASON_ANCHOR = {"share": 0.15, "ratio": 0.5, "team": 0.5}
ZONE_WINDOW = 8
ZONE_FIELDS = ("target_share", "carry_share")
ID_COLS = ("receiver_player_id", "rusher_player_id", "passer_player_id")


def build_context(pbp):
    """Inputs: the season's pass/run PBP through the completed week.
    Output: dict with team_weeks {team: {weeks}}, active {pid: {weeks}},
    team_of {pid: modal team}, zone_counts {(field, pid, week, zone): plays}.
    Purpose: computed once per refresh and shared by every player."""
    team_weeks = defaultdict(set)
    for col in ("posteam", "defteam"):
        for team, wk in pbp[[col, "week"]].dropna().drop_duplicates().itertuples(index=False):
            team_weeks[team].add(int(wk))
    active = defaultdict(set)
    team_of = {}
    for col in ID_COLS:
        sub = pbp.dropna(subset=[col])
        for pid, wk in sub[[col, "week"]].drop_duplicates().itertuples(index=False):
            active[pid].add(int(wk))
        for pid, tm in sub.groupby(col)["posteam"].agg(lambda s: s.mode().iat[0]).items():
            team_of.setdefault(pid, tm)
    zone_counts = {}
    z = pbp[pbp["yardline_100"].notna()].copy()
    z["zone"] = z["yardline_100"].apply(classify_zone)
    z = z[z["zone"].isin(["redzone", "goalline"])]
    for field, idcol, ptype in (("target_share", "receiver_player_id", "pass"), ("carry_share", "rusher_player_id", "run")):
        zz = z[z["play_type"] == ptype].dropna(subset=[idcol])
        if field == "carry_share" and "qb_scramble" in zz.columns:
            zz = zz[zz["qb_scramble"].fillna(0) != 1]  # QB carry_share = designed runs only (scrambles are scramble_rate)
        g = zz.groupby([idcol, "week", "zone"]).size()
        for (pid, wk, zone), n in g.items():
            zone_counts[(field, pid, int(wk), zone)] = int(n)
    return {"team_weeks": team_weeks, "active": active, "team_of": team_of, "zone_counts": zone_counts}


def _clean(d):
    return {int(k): float(v) for k, v in d.items() if v is not None and v == v}


def _anchored(window_value, preseason, anchor):
    if window_value is None:
        return None
    if preseason is None:
        return window_value
    return (1 - anchor) * window_value + anchor * preseason


def player_flat_values(traits, pbp, ctx, team_totals, completed_week, scheme=FLAT_SCHEME, fill=FILL):
    """Inputs: a veteran's traits dict (needs player_id, pos, preseason_projection),
    PBP, build_context() output, team_totals, completed_week.
    Output: {field: value} for every PLAYER_RATE_FIELD the player has a log for
    (and only those -- others are left to the legacy blend).
    Purpose: the 5-input weighted blend, anchored to preseason per field class."""
    pid = traits.get("player_id")
    if not pid:
        return {}
    log, vol = build_player_game_log(pbp, pid, traits["pos"], team_totals=team_totals)
    team_weeks = ctx["team_weeks"].get(ctx["team_of"].get(pid), set())
    pre_all = traits.get("preseason_projection", {})
    out = {}
    for field in PLAYER_RATE_FIELDS:
        if field not in log:
            continue
        is_share = field in wb.SHARE_FIELDS
        pre = pre_all.get(field)
        v = wb.window_blend(_clean(log[field]), _clean(vol.get(field, {})), team_weeks, ctx["active"][pid],
                            completed_week, pre, scheme, is_share, wb.MIN_VOLUME.get(field, 0), fill)
        v = _anchored(v, pre, PRESEASON_ANCHOR["share" if is_share else "ratio"])
        if v is not None:
            out[field] = v
    return out


def player_zone_values(traits, ctx, team_zone_totals, completed_week, window=ZONE_WINDOW):
    """Output: {zone: {field: value}} for redzone/goalline target_share/carry_share
    pooled over the last `window` team games (player zone plays / team zone
    plays). A zone/field is omitted (legacy value stays) when he wasn't active
    in any of those games or the team had no plays there."""
    pid = traits.get("player_id")
    team = ctx["team_of"].get(pid)
    if not pid or team is None:
        return {}
    weeks = sorted(w for w in ctx["team_weeks"][team] if w <= completed_week)[-window:]
    if not any(w in ctx["active"][pid] for w in weeks):
        return {}
    out = {}
    for zone in ("redzone", "goalline"):
        for field in ZONE_FIELDS:
            den_key = "team_targets" if field == "target_share" else "team_carries"
            num = sum(ctx["zone_counts"].get((field, pid, w, zone), 0) for w in weeks)
            den = sum((team_zone_totals.get((team, w, zone)) or {}).get(den_key, 0) for w in weeks)
            if den > 0:
                out.setdefault(zone, {})[field] = num / den
    return out


def team_values(team, pbp, ctx, projection, completed_week, scheme=FLAT_SCHEME):
    """Output: {field: value} for TEAM_RATE_FIELDS -- window blend with a 50%
    preseason anchor; volume = that side's pass dropbacks per week."""
    log = build_team_game_log(pbp, team)
    pp = pbp[pbp["play_type"] == "pass"]
    off_db = pp[pp["posteam"] == team].groupby("week").size().to_dict()
    def_db = pp[pp["defteam"] == team].groupby("week").size().to_dict()
    weeks = ctx["team_weeks"].get(team, set())
    out = {}
    for field in TEAM_RATE_FIELDS:
        if field not in log:
            continue
        vol = off_db if field == "sack_rate_allowed" else def_db
        pre = projection.get(field)
        v = wb.window_blend(_clean(log[field]), _clean(vol), weeks, weeks, completed_week, pre, scheme,
                            False, wb.MIN_VOLUME.get(field, 0), FILL)
        v = _anchored(v, pre, PRESEASON_ANCHOR["team"])
        if v is not None:
            out[field] = v
    return out
