"""Blends preseason projection, last-4-games actual, and season-to-date
actual into a single DNA snapshot for a given upcoming game number.

Two blend mechanisms live here now (Phase 3 of
docs/implementation_plans/volume_weighted_dna_blend_plan.md):

  - VOLUME-POOLED (new, default whenever real volume is supplied): games
    1-4, `blended = (hist_volume*hist_rate + season_volume*season_rate) /
    (hist_volume+season_volume)` -- a real weighted pool of historical vs.
    real-2026 sample size, so a 3-attempt cameo doesn't swing the number as
    hard as a 40-attempt start. Game 5+: drop the historical anchor
    entirely, blend exclusively from the real trailing-4-game window (L4).
    Requires `projection_volume`/`season_volume` (Phase 1/2's output --
    real per-field attempt/target/carry/dropback/completion counts). Only
    ever available for the flat (non-zone) fields of a veteran with a
    `preseason_projection_volume` block (rebuild_veteran_baseline_v_0_1_0.py
    --volume-only) -- i.e. exactly refresh_weekly_dna_v_0_1_0.py's
    non-rookie blend_one_player() call.

  - TAPER (original, unchanged -- see TAPER_SCHEDULE below): used whenever
    `projection_volume`/`season_volume` aren't supplied. Covers three
    populations Phase 1/2 deliberately never built volume for: rookies (on
    the curve track, not rebuild_veteran_baseline's population -- their
    curve_override already provides its own usage ramp, and giving them
    zero historical volume would make one real game instantly override that
    ramp instead of smoothing it), zone-split fields (target_share/
    carry_share/cpoe/catch_rate per primary/redzone/goalline -- gated by
    MIN_ZONE_SAMPLES instead, a different, already-existing solution to the
    same small-sample problem), and team defense (TEAM_RATE_FIELDS -- out
    of scope for the volume-weighted redesign per the plan doc).
    Confirmed design (docs/sims/inputs/README.md 2026 rollover plan):
      - Games 1-5 (taper): weight on projection vs. actual-so-far is
        100/0, 80/20, 60/40, 40/60, 20/80 before games 1-5 respectively.
      - Game 6+ (steady state): stat = (2/3)*L4 + (1/3)*season_to_date,
        zero projection weight.

Applies identically to players and teams -- blend_team_dna is a thin alias,
kept separate for call-site clarity.
"""

TAPER_SCHEDULE = {1: 1.0, 2: 0.8, 3: 0.6, 4: 0.4, 5: 0.2}  # projection weight before game N
STEADY_L4_WEIGHT = 2 / 3
STEADY_SEASON_WEIGHT = 1 / 3

# Volume-pooled path only: once game_number reaches this, real 2026 data
# already covers WINDOW games -- drop the historical anchor, blend
# exclusively from the trailing WINDOW-game window. WINDOW=4 (Cam's call,
# 2026-09-17) reuses rolling_stats_v_0_1_0.compute_l4()'s existing window
# rather than a new parameter.
WINDOW = 4
STEADY_STATE_START_GAME = WINDOW + 1


def taper_weights(game_number):
    """Returns (projection_weight, actual_weight) for games 1-5. Game 6+
    returns (0.0, 1.0) -- steady_state_blend takes over entirely there."""
    if game_number < 1:
        raise ValueError(f"game_number must be >= 1, got {game_number}")
    if game_number in TAPER_SCHEDULE:
        proj_w = TAPER_SCHEDULE[game_number]
        return proj_w, 1.0 - proj_w
    return 0.0, 1.0


def steady_state_blend(l4, season):
    """Field-by-field 2/3 L4 + 1/3 season blend, fields present in both only."""
    out = {}
    for field in set(l4) & set(season):
        if l4[field] is None or season[field] is None:
            continue
        out[field] = STEADY_L4_WEIGHT * l4[field] + STEADY_SEASON_WEIGHT * season[field]
    return out


def pooled_volume_blend(hist_rate, hist_volume, season_rate, season_volume):
    """Field-by-field pooled weighted average -- combines a historical
    sample (hist_volume real plays behind hist_rate) with the real
    current-season sample (season_volume real plays behind season_rate) as
    if they were one combined sample, rather than a fixed per-game-number
    split. Pools only when BOTH sides have a real (positive-volume) value;
    otherwise prefers season (real, current data) over hist, and hist over
    nothing. hist_volume/season_volume: {field: volume}, missing/None/0
    all treated as "no real sample on that side"."""
    out = {}
    for field in set(hist_rate) | set(season_rate):
        hr, sr = hist_rate.get(field), season_rate.get(field)
        hv = hist_volume.get(field) or 0
        sv = season_volume.get(field) or 0
        if hr is not None and hv > 0 and sr is not None and sv > 0:
            out[field] = (hv * hr + sv * sr) / (hv + sv)
        elif sr is not None:
            out[field] = sr
        elif hr is not None:
            out[field] = hr
    return out


def _taper_blend(projection, l4_actual, season_actual, game_number, curve_override=None):
    """The original fixed-taper/steady-state mechanism, unchanged -- see
    module docstring for which callers still use this."""
    if game_number >= 6:
        blended = steady_state_blend(l4_actual, season_actual)
        for field, value in projection.items():
            blended.setdefault(field, value)
        return blended

    proj_weight, actual_weight = taper_weights(game_number)

    effective_projection = dict(projection)
    if curve_override:
        effective_projection.update(curve_override)

    blended = {}
    for field in set(effective_projection) | set(season_actual):
        proj_val = effective_projection.get(field)
        actual_val = season_actual.get(field)
        if proj_val is None:
            blended[field] = actual_val
        elif actual_val is None:
            blended[field] = proj_val
        else:
            blended[field] = proj_weight * proj_val + actual_weight * actual_val
    return blended


def blend_player_dna(projection, l4_actual, season_actual, game_number, curve_override=None,
                      projection_volume=None, season_volume=None):
    """projection: preseason-projected values (veteran career-DNA fallback,
    or a rookie's static_projection fields).
    l4_actual/season_actual: real rolling stats through the most recently
    completed game (from rolling_stats_v_0_1_0), possibly empty pre-season.
    game_number: the upcoming game (1-indexed) being blended FOR.
    curve_override: optional {field: value}, a rookie's week-specific
    projection (from rookie_curves_v_0_1_0) -- overrides projection[field]
    before blending, taper period only.
    projection_volume/season_volume: optional {field: volume} (Phase 1/2's
    output) -- when BOTH are given, uses the volume-pooled mechanism
    (see module docstring); when either is None (the default), falls back
    to the original fixed-taper mechanism unchanged, so every caller that
    doesn't have real volume to supply keeps working exactly as before.
    Returns a single blended dict; fields present in only one source pass
    through unchanged rather than being dropped."""
    if game_number < 1:
        raise ValueError(f"game_number must be >= 1, got {game_number}")

    if projection_volume is None or season_volume is None:
        return _taper_blend(projection, l4_actual, season_actual, game_number, curve_override)

    if game_number >= STEADY_STATE_START_GAME:
        # Real 2026 data now covers >= WINDOW games -- drop the historical
        # anchor entirely. Prefer L4 (trailing WINDOW games), fall back to
        # season-to-date if a field's L4 window happens to be empty (e.g. a
        # player who sat out the last WINDOW weeks but has earlier-season
        # data), then to the frozen projection if neither has anything.
        blended = {}
        for field in set(l4_actual) | set(season_actual) | set(projection):
            if l4_actual.get(field) is not None:
                blended[field] = l4_actual[field]
            elif season_actual.get(field) is not None:
                blended[field] = season_actual[field]
            elif field in projection:
                blended[field] = projection[field]
        return blended

    effective_projection = dict(projection)
    if curve_override:
        effective_projection.update(curve_override)

    return pooled_volume_blend(effective_projection, projection_volume, season_actual, season_volume)


def blend_team_dna(projection, l4_actual, season_actual, game_number, curve_override=None,
                    projection_volume=None, season_volume=None):
    """Team-level sibling of blend_player_dna -- identical mechanics."""
    return blend_player_dna(projection, l4_actual, season_actual, game_number, curve_override,
                             projection_volume=projection_volume, season_volume=season_volume)
