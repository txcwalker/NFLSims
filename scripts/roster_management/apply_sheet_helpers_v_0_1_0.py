"""Shared logic for turning one team's override-sheet rows (season_long or
week_NN) into a traits.json-shaped {name: player_dict} -- used by both
apply_team_season_overrides_v_0_1_0.py (season-long) and
apply_team_week_overrides_v_0_1_0.py (DFS weekly).

A sheet row only ever carries 15 flat + 4 red-zone/goal-line fields -- never
the static career-DNA fields (pressure_rate, route_profile, top_speed_mph,
etc.) a traits.json entry also needs. So a brand-new player is built via
enrich_player() (career-DNA lookup + position defaults, the exact same path
export_team_season_overrides_v_0_1_0.py uses when it first adds a feed
player to a sheet) and the sheet's field values are then overlaid on top.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from src.data_pipeline.rolling_stats_v_0_1_0 import PLAYER_RATE_FIELDS, PLAYER_NGS_FIELDS  # noqa: E402
from src.data_pipeline.week_roster_v_0_1_0 import (  # noqa: E402
    GONE_SLOTS, RESERVE_SLOTS, parse_return_week,
)
from roster_feed_v_0_1_0 import match_key  # noqa: E402
from build_2026_rosters_v_0_1_0 import enrich_player  # noqa: E402

TUNABLE = PLAYER_RATE_FIELDS + PLAYER_NGS_FIELDS
SHARE_FIELDS = ("target_share", "carry_share")
POS_ORDER = {"QB": 0, "RB": 1, "WR": 2, "TE": 3}
# sheet zone column -> (traits.json zone, field)
ZONE_COL_MAP = {
    "rz_target_share": ("redzone", "target_share"), "rz_carry_share": ("redzone", "carry_share"),
    "gl_target_share": ("goalline", "target_share"), "gl_carry_share": ("goalline", "carry_share"),
}


def _f(v, default=None):
    try:
        return float(v)
    except (TypeError, ValueError):
        return default


def zero_player_shares(player):
    """In place: target_share/carry_share -> 0 everywhere (top level, every
    splits zone, preseason_projection + its splits)."""
    for field in SHARE_FIELDS:
        if field in player:
            player[field] = 0.0
        for zone in player.get("splits", {}).values():
            if field in zone:
                zone[field] = 0.0
        pp = player.get("preseason_projection", {})
        if field in pp:
            pp[field] = 0.0
        for zone in pp.get("splits", {}).values():
            if field in zone:
                zone[field] = 0.0


def _overlay_row(player, row):
    """In place: write row's 15 flat + 4 rz/gl fields into player (top-level
    + splits.primary for the flat fields, splits.redzone/goalline for the
    zone fields, and preseason_projection mirrors both)."""
    for f in TUNABLE:
        v = _f(row.get(f))
        if v is None:
            continue
        player[f] = round(v, 6)
        player.setdefault("splits", {}).setdefault("primary", {})[f] = round(v, 6)
        pp = player.setdefault("preseason_projection", {})
        pp[f] = round(v, 6)
        if f in SHARE_FIELDS:
            pp.setdefault("splits", {}).setdefault("primary", {})[f] = round(v, 6)
    for sheet_col, (zone, field) in ZONE_COL_MAP.items():
        v = _f(row.get(sheet_col))
        if v is None:
            continue
        player.setdefault("splits", {}).setdefault(zone, {})[field] = round(v, 6)
        pp = player.setdefault("preseason_projection", {})
        pp.setdefault("splits", {}).setdefault(zone, {})[field] = round(v, 6)


def _overlay_status(player, row, week=None, touch_starter=True):
    """In place: dfs_status/starter_override (week-sheet-only columns, added
    2026-09-19 for the UI gameday toggle -- absent from season-long sheets,
    where row.get() below just falls through to the defaults) -> the
    traits.json status/starter_override fields game_engine.py already reads
    (_get_starter_static's starter_override check, the status=='active'
    pool filters). Mirrors set_qb_starter_overrides_v_0_1_0.py's
    clear-then-set convention for starter_override.

    `week` (only ever passed on the weekly compile path -- season-long has
    no week context) also folds in the PRE-EXISTING roster_slot/return_week
    IR-style unavailability, using the exact same predicate
    resolve_week_rows uses to decide who's "hurt"/"gone". Found 2026-09-19:
    without this, a player excluded via roster_slot=ir (rather than the new
    dfs_status column) had their shares correctly zeroed by
    resolve_week_rows but still compiled to status="active" here -- right
    answer for the simulation math (0 share ~= 0 selection probability) but
    wrong for the UI's Active checkbox and any consumer that actually reads
    status instead of shares.

    `touch_starter` (set by apply_sheet_to_traits, team-wide -- see there):
    False means no row in this compile has an explicit starter_override
    this week, so starter_override is left alone entirely rather than
    cleared. Needed because a week sheet's own starter_override column is
    blank by default (only ever populated by a real gameday toggle or a
    hand-edit) -- a genuine, code-set live override (e.g.
    set_qb_starter_overrides_v_0_1_0.py's `TEN: Cam Ward`) has nothing to do
    with that column, so a blank cell must never be read as "clear it".
    Found 2026-09-19: TEN/SF/NYG/NO/SEA's real, hand-confirmed starters were
    silently losing their live starter_override on every DFS week compile
    for exactly this reason -- see WORKLOG.md."""
    dfs_status = (row.get("dfs_status") or "active").strip().lower()
    unavailable = dfs_status == "out"
    if week is not None:
        slot = (row.get("roster_slot") or "active").strip().lower()
        if slot in GONE_SLOTS:
            unavailable = True
        elif (slot in RESERVE_SLOTS and parse_return_week(row.get("return_week")) > week
              and dfs_status != "force_active"):   # gameday override of a reserve slot
            unavailable = True
    player["status"] = "out" if unavailable else "active"
    if not touch_starter:
        return
    so = str(row.get("starter_override") or "").strip().upper()
    if so in ("TRUE", "1", "YES"):
        player["starter_override"] = True
    elif "starter_override" in player:
        del player["starter_override"]


def apply_sheet_to_traits(traits, rows, skill_dna, week=None):
    """Mutates `traits` ({player_name: player_dict}, one team) in place so
    it reflects `rows` (that team's real player rows from a sheet, already
    filtered to non-gone via is_player_row + roster_slot):
      - a traits player not present in `rows` (by name) with nonzero share
        -> zeroed (kept in traits, not deleted -- consistent with how a
        reserve/injured player is represented)
      - a `rows` player already in traits -> its 15 flat + 4 rz/gl field
        values are overlaid
      - a `rows` player NOT yet in traits -> a full entry is created via
        enrich_player(), then the same overlay is applied
    Returns (updated, created, zeroed) name lists, for reporting."""
    row_by_key = {match_key(r["player_name"]): r for r in rows if r.get("player_name")}
    traits_keys = {match_key(n): n for n in traits}

    # Team-wide, not per-row: a blank starter_override cell is this sheet's
    # default/unedited state, not an instruction to clear anyone's flag --
    # only treat it as a real toggle when SOME row this compile explicitly
    # claims the starter slot (see _overlay_status's touch_starter docs).
    touch_starter = any(
        str(r.get("starter_override") or "").strip().upper() in ("TRUE", "1", "YES")
        for r in row_by_key.values()
    )

    zeroed = []
    for key, name in list(traits_keys.items()):
        p = traits[name]
        if key not in row_by_key and (_f(p.get("target_share"), 0) > 0
                                      or _f(p.get("carry_share"), 0) > 0):
            zero_player_shares(p)
            zeroed.append(name)

    updated, created = [], []
    for key, row in row_by_key.items():
        name = traits_keys.get(key)
        if name is None:
            name = row["player_name"]
            pid = (row.get("player_id") or "").strip() or None
            traits[name] = enrich_player(name, row["pos"], _f(row.get("target_share"), 0.0),
                                         _f(row.get("carry_share"), 0.0), skill_dna,
                                         False, None, player_id=pid)
            created.append(name)
        else:
            updated.append(name)
        _overlay_row(traits[name], row)
        _overlay_status(traits[name], row, week, touch_starter)

    return updated, created, zeroed
