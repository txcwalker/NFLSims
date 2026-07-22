"""Computes per-week rolling stats (season-to-date, last-4-games) for players
and teams from raw play-by-play, feeding dna_blender_v_0_1_0's taper/steady-
state blend.

Two layers:
  - rolling_average() / compute_season_to_date() / compute_l4(): generic,
    field-agnostic windowed-mean primitives over a {week: value} log. Used
    directly by tests and by anything that already has a per-week log.
  - build_player_game_log() / build_team_game_log(): PBP -> per-week log for
    the specific fields current_rosters/trench_dna consume live (see
    docs/sims/inputs/README.md for which fields are actually read by the
    engine -- dead/cosmetic fields are intentionally not built here).

Entry points: rolling_stats_for_player(), rolling_stats_for_team().
"""

# Fields with a real live consumer in game_engine.py per docs/sims/inputs/README.md.
# Names match current_rosters' TOP-LEVEL player fields exactly (yac_per_rec/
# adot, NOT the DNA-file/nested-splits names yac_per_reception/
# avg_target_depth_yds -- current_rosters uses different names at the top
# level vs. inside its own `splits` sub-object, confirmed against a real
# roster file; this module only ever writes top-level fields).
PLAYER_RATE_FIELDS = [
    "target_share", "carry_share", "catch_rate", "ypc",
    "yac_per_rec", "cpoe", "sack_rate", "scramble_rate",
    "adot",
]
TEAM_RATE_FIELDS = ["def_pressure_rate", "def_sack_rate", "sack_rate_allowed"]


def rolling_average(game_values, through_week, window=None):
    """game_values: {week: value}. Only weeks <= through_week are eligible.
    window=None -> season-to-date (all eligible weeks). window=N -> last N
    of those weeks (naturally degrades to fewer than N if fewer exist).
    Returns None if no eligible weeks have a value yet."""
    weeks = sorted(w for w in game_values if w <= through_week)
    if window is not None:
        weeks = weeks[-window:]
    values = [game_values[w] for w in weeks if game_values[w] is not None]
    if not values:
        return None
    return sum(values) / len(values)


def compute_season_to_date(game_values, through_week):
    return rolling_average(game_values, through_week, window=None)


def compute_l4(game_values, through_week):
    return rolling_average(game_values, through_week, window=4)


def rolling_stats_for_fields(per_field_game_values, through_week):
    """per_field_game_values: {field: {week: value}}.
    Returns (season_to_date, l4), each {field: value_or_None}."""
    season = {f: compute_season_to_date(gv, through_week) for f, gv in per_field_game_values.items()}
    l4 = {f: compute_l4(gv, through_week) for f, gv in per_field_game_values.items()}
    return season, l4


def build_player_game_log(pbp, player_id, position, team_totals=None):
    """pbp: single-season PBP DataFrame (pass_attempt/rush_attempt rows).
    player_id: gsis_id, matched against the *_player_id columns -- NOT
    *_player_name, which nfl_data_py's PBP stores in short-name form
    ("J.Chase"), not the full display names current_rosters/DNA files key
    by. Matching on ID (as build_full_name_dna.py already does) sidesteps
    that mismatch and is more robust besides (handles name changes, avoids
    short-name collisions).
    team_totals: optional pre-grouped (team, week) -> {team_targets, team_carries}
    for target_share/carry_share; if omitted those two fields are skipped
    (caller can supply them when team context is available -- see
    build_team_week_totals()).
    Returns {field: {week: value}} for whichever of PLAYER_RATE_FIELDS apply."""
    pbp_pass = pbp[pbp["play_type"] == "pass"]
    pbp_run = pbp[pbp["play_type"] == "run"]

    log = {}

    if position != "QB":
        p_pass = pbp_pass[pbp_pass["receiver_player_id"] == player_id]
        if len(p_pass):
            by_week = p_pass.groupby("week")
            log["catch_rate"] = by_week["complete_pass"].mean().to_dict()
            log["adot"] = by_week["air_yards"].mean().to_dict()
            complete = p_pass[p_pass["complete_pass"] == 1].groupby("week")
            log["yac_per_rec"] = complete["yards_after_catch"].mean().to_dict()
            if team_totals is not None:
                team = p_pass["posteam"].mode().iat[0] if len(p_pass["posteam"].mode()) else None
                targets_by_week = by_week.size()
                log["target_share"] = {
                    wk: n / team_totals.get((team, wk), {}).get("team_targets", n) if team_totals.get((team, wk)) else None
                    for wk, n in targets_by_week.items()
                }

        p_run = pbp_run[pbp_run["rusher_player_id"] == player_id]
        if len(p_run):
            by_week = p_run.groupby("week")
            log["ypc"] = by_week["yards_gained"].mean().to_dict()
            if team_totals is not None:
                team = p_run["posteam"].mode().iat[0] if len(p_run["posteam"].mode()) else None
                carries_by_week = by_week.size()
                log["carry_share"] = {
                    wk: n / team_totals.get((team, wk), {}).get("team_carries", n) if team_totals.get((team, wk)) else None
                    for wk, n in carries_by_week.items()
                }
    else:
        p_pass = pbp_pass[pbp_pass["passer_player_id"] == player_id]
        if len(p_pass):
            by_week = p_pass.groupby("week")
            log["cpoe"] = by_week["cpoe"].mean().to_dict()
            n_att = by_week["pass_attempt"].sum()
            n_sack = by_week["sack"].sum()
            log["sack_rate"] = (n_sack / (n_sack + n_att)).to_dict()
            log["scramble_rate"] = by_week["qb_scramble"].mean().to_dict()

    return log


def build_team_week_totals(pbp):
    """(team, week) -> {team_targets, team_carries}, for target_share/carry_share."""
    pbp_pass = pbp[pbp["play_type"] == "pass"]
    pbp_run = pbp[pbp["play_type"] == "run"]
    targets = pbp_pass.groupby(["posteam", "week"]).size()
    carries = pbp_run.groupby(["posteam", "week"]).size()
    out = {}
    for (team, wk), n in targets.items():
        out.setdefault((team, wk), {})["team_targets"] = n
    for (team, wk), n in carries.items():
        out.setdefault((team, wk), {})["team_carries"] = n
    return out


def build_team_game_log(pbp, team):
    """Defensive team-level per-week log for TEAM_RATE_FIELDS."""
    off = pbp[pbp["posteam"] == team]
    deff = pbp[pbp["defteam"] == team]

    log = {}
    off_pass = off[off["play_type"] == "pass"]
    if len(off_pass):
        by_week = off_pass.groupby("week")
        n_att = by_week["pass_attempt"].sum()
        n_sack = by_week["sack"].sum()
        log["sack_rate_allowed"] = (n_sack / (n_sack + n_att)).to_dict()

    def_pass = deff[deff["play_type"] == "pass"]
    if len(def_pass):
        by_week = def_pass.groupby("week")
        log["def_pressure_rate"] = by_week["qb_hit"].mean().to_dict()
        n_att = by_week["pass_attempt"].sum()
        n_sack = by_week["sack"].sum()
        log["def_sack_rate"] = (n_sack / (n_sack + n_att)).to_dict()

    return log


def rolling_stats_for_player(pbp, player_id, position, through_week, team_totals=None):
    log = build_player_game_log(pbp, player_id, position, team_totals=team_totals)
    return rolling_stats_for_fields(log, through_week)


def rolling_stats_for_team(pbp, team, through_week):
    log = build_team_game_log(pbp, team)
    return rolling_stats_for_fields(log, through_week)
