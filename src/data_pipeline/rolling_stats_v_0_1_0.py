"""Computes per-week rolling stats (season-to-date, last-4-games) for players
and teams from raw play-by-play + NGS data, feeding dna_blender_v_0_1_0's
taper/steady-state blend.

Three layers:
  - rolling_average() / compute_season_to_date() / compute_l4(): generic,
    field-agnostic windowed-mean primitives over a {week: value} log. Used
    directly by tests and by anything that already has a per-week log.
  - build_player_game_log() / build_team_game_log(): PBP -> per-week log,
    flat (whole-game) fields.
  - build_player_zone_game_log(): PBP -> per-week-per-zone log, for the
    fields that feed game_engine.py's zone-conditioned ("_by_filter") model
    features -- target_share, carry_share, cpoe, catch_rate.

Field coverage matches what's confirmed LIVE in game_engine.py (Phase 7
audit, see docs/sims/inputs/README.md) -- one confirmed exception:
contested_catch_rate has no real signal in nfl_data_py (checked PBP and all
three NGS categories, 2026-07-22) -- NFL's real Next Gen Stats tracks it,
but this package doesn't expose it, so it stays a one-time synthetic value
at roster-build time, not part of this module. Revisit if a source is ever
found.

Entry points: rolling_stats_for_player(), rolling_stats_for_team(),
rolling_stats_for_player_zones().
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
    "adot", "avg_air_yards_per_att", "deep_target_rate",
    "elusiveness", "broken_tackle_rate",
]
# NGS-sourced (import_ngs_data), not derivable from plain PBP.
PLAYER_NGS_FIELDS = ["avg_time_to_throw_sec", "avg_separation_yds"]
TEAM_RATE_FIELDS = ["def_pressure_rate", "def_sack_rate", "sack_rate_allowed"]
ZONE_SPLIT_FIELDS = ["target_share", "carry_share", "cpoe", "catch_rate"]

GOALLINE_YARDLINE = 5  # matches game_engine.py's constant of the same name
REDZONE_YARDLINE = 20
ZONES = ("primary", "redzone", "goalline")
# Below this many real plays in a zone/window, the rolling value for that
# zone is unreliable -- caller falls back to the flat (all-zone) value.
MIN_ZONE_SAMPLES = 5


def classify_zone(yardline_100):
    if yardline_100 <= GOALLINE_YARDLINE:
        return "goalline"
    if yardline_100 <= REDZONE_YARDLINE:
        return "redzone"
    return "primary"


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


def rolling_sum(game_values, through_week, window=None):
    """Same eligibility rules as rolling_average() (only weeks <=
    through_week, last `window` of those if given), but sums instead of
    averaging -- for VOLUME (attempts/targets/carries/dropbacks/
    completions), where what matters is the total sample size behind a
    window, not a per-game mean of counts. Unlike rolling_average(), an
    empty/no-data window returns 0, not None -- zero real volume is a valid,
    poolable value (feeds a weighted-average denominator downstream), not
    "no rate exists yet"."""
    weeks = sorted(w for w in game_values if w <= through_week)
    if window is not None:
        weeks = weeks[-window:]
    return sum(game_values[w] for w in weeks if game_values[w] is not None)


def compute_season_to_date_volume(game_values, through_week):
    return rolling_sum(game_values, through_week, window=None)


def compute_l4_volume(game_values, through_week):
    return rolling_sum(game_values, through_week, window=4)


def rolling_volume_for_fields(per_field_volume_values, through_week):
    """Volume sibling of rolling_stats_for_fields() -- sums instead of
    averaging. per_field_volume_values: {field: {week: volume}}.
    Returns (season_volume, l4_volume), each {field: total_volume}."""
    season = {f: compute_season_to_date_volume(gv, through_week) for f, gv in per_field_volume_values.items()}
    l4 = {f: compute_l4_volume(gv, through_week) for f, gv in per_field_volume_values.items()}
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
    Returns (log, volume) -- both {field: {week: value}} for whichever of
    PLAYER_RATE_FIELDS apply. `volume` mirrors `log`'s keys exactly: each
    field's real per-week sample size (pass attempts, targets, carries,
    dropbacks, or completions -- whichever denominator actually produced
    that week's rate), for a caller that wants to pool weeks by real sample
    size instead of averaging per-game values unweighted (see
    docs/implementation_plans/volume_weighted_dna_blend_plan.md)."""
    pbp_pass = pbp[pbp["play_type"] == "pass"]
    pbp_run = pbp[pbp["play_type"] == "run"]

    log = {}
    volume = {}

    if position != "QB":
        p_pass = pbp_pass[pbp_pass["receiver_player_id"] == player_id]
        if len(p_pass):
            by_week = p_pass.groupby("week")
            targets_by_week = by_week.size()
            log["catch_rate"] = by_week["complete_pass"].mean().to_dict()
            log["adot"] = by_week["air_yards"].mean().to_dict()
            log["deep_target_rate"] = by_week["air_yards"].apply(lambda s: (s >= 20).mean()).to_dict()
            volume["catch_rate"] = targets_by_week.to_dict()
            volume["adot"] = targets_by_week.to_dict()
            volume["deep_target_rate"] = targets_by_week.to_dict()
            complete = p_pass[p_pass["complete_pass"] == 1].groupby("week")
            log["yac_per_rec"] = complete["yards_after_catch"].mean().to_dict()
            completions_by_week = complete.size()
            volume["yac_per_rec"] = completions_by_week.to_dict()
            if "xyac_mean_yardage" in p_pass.columns:
                completions = p_pass[p_pass["complete_pass"] == 1].copy()
                completions["yac_over_expected"] = completions["yards_after_catch"] - completions["xyac_mean_yardage"]
                by_week_completions = completions.groupby("week")
                log["elusiveness"] = by_week_completions["yac_over_expected"].mean().to_dict()
                log["broken_tackle_rate"] = by_week_completions["yac_over_expected"].apply(lambda s: (s > 3.0).mean()).to_dict()
                volume["elusiveness"] = completions_by_week.to_dict()
                volume["broken_tackle_rate"] = completions_by_week.to_dict()
            if team_totals is not None:
                team = p_pass["posteam"].mode().iat[0] if len(p_pass["posteam"].mode()) else None
                log["target_share"] = {
                    wk: n / team_totals.get((team, wk), {}).get("team_targets", n) if team_totals.get((team, wk)) else None
                    for wk, n in targets_by_week.items()
                }
                volume["target_share"] = targets_by_week.to_dict()
    else:
        p_pass = pbp_pass[pbp_pass["passer_player_id"] == player_id]
        if len(p_pass):
            by_week = p_pass.groupby("week")
            log["cpoe"] = by_week["cpoe"].mean().to_dict()
            log["avg_air_yards_per_att"] = by_week["air_yards"].mean().to_dict()
            n_att = by_week["pass_attempt"].sum()
            volume["cpoe"] = n_att.to_dict()
            volume["avg_air_yards_per_att"] = n_att.to_dict()

            n_sack = by_week["sack"].sum()
            sack_dropbacks = n_sack + n_att
            log["sack_rate"] = (n_sack / sack_dropbacks).to_dict()
            volume["sack_rate"] = sack_dropbacks.to_dict()

            # qb_scramble is only ever 1 on play_type == "run" rows (a
            # scramble means the QB kept it instead of throwing, so it's
            # coded as a run, not a pass -- see docs/eda_outputs/
            # qb_scramble_rate_2025.md). Computing it from p_pass alone
            # silently returns 0.0 for every QB every week; scrambles have
            # to be pulled from pbp_run and combined with pass attempts into
            # a real dropback rate.
            n_scramble = pbp_run[
                (pbp_run["rusher_player_id"] == player_id) & (pbp_run["qb_scramble"] == 1)
            ].groupby("week").size().reindex(n_att.index, fill_value=0)
            scramble_dropbacks = n_scramble + n_att
            log["scramble_rate"] = (n_scramble / scramble_dropbacks).to_dict()
            volume["scramble_rate"] = scramble_dropbacks.to_dict()

    # Rushing stats apply to whoever actually carries the ball -- RB
    # primarily, but also QB scrambles/sneaks/designed runs and occasional
    # WR jet sweeps. Previously gated behind "position != QB", which silently
    # dropped every QB's real rushing production from the weekly blend.
    p_run = pbp_run[pbp_run["rusher_player_id"] == player_id]
    if len(p_run):
        by_week = p_run.groupby("week")
        carries_by_week = by_week.size()
        log["ypc"] = by_week["yards_gained"].mean().to_dict()
        volume["ypc"] = carries_by_week.to_dict()
        if team_totals is not None:
            team = p_run["posteam"].mode().iat[0] if len(p_run["posteam"].mode()) else None
            log["carry_share"] = {
                wk: n / team_totals.get((team, wk), {}).get("team_carries", n) if team_totals.get((team, wk)) else None
                for wk, n in carries_by_week.items()
            }
            volume["carry_share"] = carries_by_week.to_dict()

    return log, volume


def build_player_ngs_game_log(ngs_pass_df, ngs_recv_df, player_id, position):
    """NGS-sourced fields (not in PBP): avg_time_to_throw_sec (QB, from
    import_ngs_data('passing', ...)), avg_separation_yds (receivers, from
    import_ngs_data('receiving', ...)). Both frames have a real per-week
    `week` column (0 = season aggregate, excluded here) keyed by
    player_gsis_id. Pass ngs_pass_df/ngs_recv_df=None to skip either.
    Returns (log, volume) -- volume uses each frame's own real weekly
    sample-size column (`attempts` for passing, `targets` for receiving),
    same convention as build_player_game_log()."""
    log = {}
    volume = {}
    if position == "QB" and ngs_pass_df is not None:
        p = ngs_pass_df[(ngs_pass_df["player_gsis_id"] == player_id) & (ngs_pass_df["week"] > 0)]
        if len(p):
            log["avg_time_to_throw_sec"] = dict(zip(p["week"], p["avg_time_to_throw"]))
            volume["avg_time_to_throw_sec"] = dict(zip(p["week"], p["attempts"]))
    elif position != "QB" and ngs_recv_df is not None:
        p = ngs_recv_df[(ngs_recv_df["player_gsis_id"] == player_id) & (ngs_recv_df["week"] > 0)]
        if len(p):
            log["avg_separation_yds"] = dict(zip(p["week"], p["avg_separation"]))
            volume["avg_separation_yds"] = dict(zip(p["week"], p["targets"]))
    return log, volume


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


def build_team_week_zone_totals(pbp):
    """(team, week, zone) -> {team_targets, team_carries} -- zone-aware
    version of build_team_week_totals(), for zone-split target_share/
    carry_share."""
    pbp = pbp.copy()
    pbp["zone"] = pbp["yardline_100"].apply(classify_zone)
    pbp_pass = pbp[pbp["play_type"] == "pass"]
    pbp_run = pbp[pbp["play_type"] == "run"]
    targets = pbp_pass.groupby(["posteam", "week", "zone"]).size()
    carries = pbp_run.groupby(["posteam", "week", "zone"]).size()
    out = {}
    for (team, wk, zone), n in targets.items():
        out.setdefault((team, wk, zone), {})["team_targets"] = n
    for (team, wk, zone), n in carries.items():
        out.setdefault((team, wk, zone), {})["team_carries"] = n
    return out


def build_player_zone_game_log(pbp, player_id, position, team_zone_totals=None):
    """Zone-bucketed version of build_player_game_log(), for
    ZONE_SPLIT_FIELDS only (target_share, carry_share, cpoe, catch_rate --
    the four that feed game_engine.py's "_by_filter" model features).
    Returns {field: {zone: {week: value}}}."""
    pbp = pbp.copy()
    pbp["zone"] = pbp["yardline_100"].apply(classify_zone)
    pbp_pass = pbp[pbp["play_type"] == "pass"]

    log = {f: {z: {} for z in ZONES} for f in ZONE_SPLIT_FIELDS}

    if position != "QB":
        p_pass = pbp_pass[pbp_pass["receiver_player_id"] == player_id]
        if len(p_pass):
            for zone, zdf in p_pass.groupby("zone"):
                by_week = zdf.groupby("week")
                log["catch_rate"][zone] = by_week["complete_pass"].mean().to_dict()
                if team_zone_totals is not None:
                    team = zdf["posteam"].mode().iat[0] if len(zdf["posteam"].mode()) else None
                    targets_by_week = by_week.size()
                    log["target_share"][zone] = {
                        wk: n / team_zone_totals.get((team, wk, zone), {}).get("team_targets", n)
                        if team_zone_totals.get((team, wk, zone)) else None
                        for wk, n in targets_by_week.items()
                    }
    else:
        p_pass = pbp_pass[pbp_pass["passer_player_id"] == player_id]
        if len(p_pass):
            for zone, zdf in p_pass.groupby("zone"):
                log["cpoe"][zone] = zdf.groupby("week")["cpoe"].mean().to_dict()

    # Rushing zone shares apply to any position that runs the ball -- see
    # build_player_game_log's identical carve-out for why QB isn't excluded
    # (QB sneaks/scrambles are rusher_player_id rows just like RB carries).
    pbp_run = pbp[pbp["play_type"] == "run"]
    p_run = pbp_run[pbp_run["rusher_player_id"] == player_id]
    if len(p_run) and team_zone_totals is not None:
        for zone, zdf in p_run.groupby("zone"):
            by_week = zdf.groupby("week")
            team = zdf["posteam"].mode().iat[0] if len(zdf["posteam"].mode()) else None
            carries_by_week = by_week.size()
            log["carry_share"][zone] = {
                wk: n / team_zone_totals.get((team, wk, zone), {}).get("team_carries", n)
                if team_zone_totals.get((team, wk, zone)) else None
                for wk, n in carries_by_week.items()
            }

    return log


def rolling_stats_for_player_zones(pbp, player_id, position, through_week, team_zone_totals=None,
                                    min_samples=MIN_ZONE_SAMPLES):
    """Season/L4 rolling stats per zone for ZONE_SPLIT_FIELDS. A zone's
    value is set to None (caller falls back to the flat blended value) when
    fewer than `min_samples` real plays exist in that zone/window --
    red-zone and especially goal-line sample sizes are thin, and a rolling
    average over 2-3 plays is noise, not signal."""
    zone_log = build_player_zone_game_log(pbp, player_id, position, team_zone_totals=team_zone_totals)
    pbp_z = pbp.copy()
    pbp_z["zone"] = pbp_z["yardline_100"].apply(classify_zone)

    def _sample_count(field, zone, window_weeks):
        if field in ("target_share", "catch_rate"):
            mask = (pbp_z["play_type"] == "pass") & (pbp_z["zone"] == zone) & (pbp_z["week"].isin(window_weeks))
            id_col = "receiver_player_id"
        elif field == "carry_share":
            mask = (pbp_z["play_type"] == "run") & (pbp_z["zone"] == zone) & (pbp_z["week"].isin(window_weeks))
            id_col = "rusher_player_id"
        else:  # cpoe
            mask = (pbp_z["play_type"] == "pass") & (pbp_z["zone"] == zone) & (pbp_z["week"].isin(window_weeks))
            id_col = "passer_player_id"
        return int((pbp_z.loc[mask, id_col] == player_id).sum())

    all_weeks = sorted(w for w in range(1, through_week + 1))
    l4_weeks = all_weeks[-4:]

    season = {f: {} for f in ZONE_SPLIT_FIELDS}
    l4 = {f: {} for f in ZONE_SPLIT_FIELDS}
    for field in ZONE_SPLIT_FIELDS:
        for zone in ZONES:
            gv = zone_log[field][zone]
            season_val = compute_season_to_date(gv, through_week)
            l4_val = compute_l4(gv, through_week)
            season[field][zone] = season_val if _sample_count(field, zone, all_weeks) >= min_samples else None
            l4[field][zone] = l4_val if _sample_count(field, zone, l4_weeks) >= min_samples else None
    return season, l4


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


def rolling_stats_for_player(pbp, player_id, position, through_week, team_totals=None,
                              ngs_pass_df=None, ngs_recv_df=None):
    """Returns (season, l4, season_volume, l4_volume) -- the last two are
    each field's real total sample size (attempts/targets/carries/
    dropbacks/completions) summed over the same window as season/l4's
    rates, not yet consumed by dna_blender_v_0_1_0 (still the fixed taper
    as of this writing) -- see
    docs/implementation_plans/volume_weighted_dna_blend_plan.md Phase 1."""
    log, volume = build_player_game_log(pbp, player_id, position, team_totals=team_totals)
    ngs_log, ngs_volume = build_player_ngs_game_log(ngs_pass_df, ngs_recv_df, player_id, position)
    log.update(ngs_log)
    volume.update(ngs_volume)
    season, l4 = rolling_stats_for_fields(log, through_week)
    season_volume, l4_volume = rolling_volume_for_fields(volume, through_week)
    return season, l4, season_volume, l4_volume


def rolling_stats_for_team(pbp, team, through_week):
    log = build_team_game_log(pbp, team)
    return rolling_stats_for_fields(log, through_week)
