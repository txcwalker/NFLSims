"""Blends preseason projection, last-4-games actual, and season-to-date
actual into a single DNA snapshot for a given upcoming game number.

Confirmed design (docs/sims/inputs/README.md 2026 rollover plan):
  - Games 1-5 (taper): weight on projection vs. actual-so-far is
    100/0, 80/20, 60/40, 40/60, 20/80 before games 1-5 respectively.
    "Actual-so-far" is season-to-date, which naturally equals L4 once 4
    games exist (by game 5) -- no special-casing needed.
  - Game 6+ (steady state): stat = (2/3)*L4 + (1/3)*season_to_date, zero
    projection weight.

Applies identically to players and teams -- blend_team_dna is a thin alias,
kept separate for call-site clarity.
"""

TAPER_SCHEDULE = {1: 1.0, 2: 0.8, 3: 0.6, 4: 0.4, 5: 0.2}  # projection weight before game N
STEADY_L4_WEIGHT = 2 / 3
STEADY_SEASON_WEIGHT = 1 / 3


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


def blend_player_dna(projection, l4_actual, season_actual, game_number, curve_override=None):
    """projection: preseason-projected values (veteran career-DNA fallback,
    or a rookie's static_projection fields).
    l4_actual/season_actual: real rolling stats through the most recently
    completed game (from rolling_stats_v_0_1_0), possibly empty pre-season.
    game_number: the upcoming game (1-indexed) being blended FOR.
    curve_override: optional {field: value}, a rookie's week-specific
    projection (from rookie_curves_v_0_1_0) -- overrides projection[field]
    before blending, taper period only (irrelevant once game_number >= 6,
    since projection weight is already zero there).
    Returns a single blended dict; fields present in only one source pass
    through unchanged rather than being dropped."""
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


def blend_team_dna(projection, l4_actual, season_actual, game_number, curve_override=None):
    """Team-level sibling of blend_player_dna -- identical mechanics."""
    return blend_player_dna(projection, l4_actual, season_actual, game_number, curve_override)
