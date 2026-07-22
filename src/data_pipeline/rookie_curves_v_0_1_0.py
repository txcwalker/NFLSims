"""Parametric week-by-week curve for a rookie's projection input itself
(separate from dna_blender's taper, which controls trust in that
projection vs. real actuals -- see rookie_projections_2026.json's schema).

Deliberately a single simple linear interpolation between two knots, kept
isolated so the curve shape can be redesigned later (Cam expects to iterate
on this) without touching dna_blender_v_0_1_0.
"""


def interpolate_curve(start_week, steady_week, early_value, late_value, game_number):
    """Linear ramp from early_value at start_week to late_value at
    steady_week. Before start_week -> early_value. At/after steady_week ->
    late_value. steady_week <= start_week is treated as an immediate jump
    to late_value (no ramp)."""
    if steady_week <= start_week:
        return late_value
    if game_number <= start_week:
        return early_value
    if game_number >= steady_week:
        return late_value
    frac = (game_number - start_week) / (steady_week - start_week)
    return early_value + frac * (late_value - early_value)


def resolve_rookie_curves(curve_spec, game_number):
    """curve_spec: {field: {start_week, steady_week, early_value, late_value}}
    (a rookie_projections_2026.json entry's usage_curve or efficiency_curve).
    Returns {field: interpolated_value} for the given game_number, suitable
    as dna_blender_v_0_1_0.blend_player_dna's curve_override."""
    return {
        field: interpolate_curve(
            params["start_week"], params["steady_week"],
            params["early_value"], params["late_value"],
            game_number,
        )
        for field, params in curve_spec.items()
    }
