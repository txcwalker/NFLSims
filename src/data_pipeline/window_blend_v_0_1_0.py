"""Weighted last-4-games + season-long input blend (Cam, 2026-10-07).

Replaces (behind a mode flag -- nothing calls this yet) the taper/steady-state
blend in dna_blender_v_0_1_0.py once four real games exist. Each rate field
becomes a weighted sum of FIVE inputs: the season-long value plus the player's
last four *qualifying* games, oldest game smallest weight, newest largest.

    SCHEMES["flat"]    20 | 20 20 20 20      (season | 4-ago .. most recent)
    SCHEMES["ramp_a"]  20 | 16 18 22 24
    SCHEMES["ramp_b"]  20 | 14 17 23 26

Which games count (Cam's rules, 2026-10-07):
  - Window = the team's last 4 games up to `through_week` (byes don't count).
  - A game QUALIFIES if the player was active and, for ratio stats, had at least
    `min_volume` real plays behind the rate (a 1-target 100% catch rate is noise).
    Share stats (target_share / carry_share) qualify on activity alone -- an
    active game with no targets is a real 0.
  - 0 non-qualifying games in the window: use it as is.
  - Exactly 1: fill that slot with the mean (or median) of the player's other
    qualifying window games ("fill" arg -- the backtest compares both).
  - 2 or more: drop them and reach back past the window for the player's most
    recent qualifying games instead.
  - Slots still empty (not enough qualifying games exist) fall back to the
    PRESEASON value, as does the season slot when it has no qualifying games.
    With no preseason value either, the weights renormalise over what exists.

Zone fields (redzone / goalline) are too thin for per-game slots; zone_blend()
pools the last `window` (default 8) team games and keeps a fixed `anchor`
(default 0.5) of the preseason value -- the "half speed" version.

Team-mate opportunity shifts (A.J. Brown returning shrinks his receivers'
shares) are deliberately NOT automated -- Cam hand-tunes those via
data/overrides/2026/week_NN sheets. See `usage_shift_flags` for the report.

Pure functions, no I/O. Entry points: window_blend(), zone_blend().
"""
from statistics import mean, median

WINDOW = 4

SCHEMES = {
    "flat": {"season": 0.20, "games": [0.20, 0.20, 0.20, 0.20]},
    "ramp_a": {"season": 0.20, "games": [0.16, 0.18, 0.22, 0.24]},
    "ramp_b": {"season": 0.20, "games": [0.14, 0.17, 0.23, 0.26]},
}

# Share stats qualify on being active (a 0-target game is a true 0).
SHARE_FIELDS = {"target_share", "carry_share"}

# Minimum real plays behind a per-game ratio for that game to qualify.
# Starting values -- the backtest includes a no-minimum control to check they help.
MIN_VOLUME = {
    "catch_rate": 3, "adot": 3, "deep_target_rate": 3,
    "yac_per_rec": 2, "elusiveness": 2, "broken_tackle_rate": 2,
    "ypc": 3,
    "cpoe": 10, "avg_air_yards_per_att": 10, "sack_rate": 10, "scramble_rate": 10,
    "def_pressure_rate": 15, "def_sack_rate": 15, "sack_rate_allowed": 15,
}


def _qualifying_value(week, game_values, volume, active_weeks, is_share, min_volume):
    """Inputs: week (int); game_values/volume {week: float}; active_weeks set of
    weeks the player was active; is_share bool; min_volume number.
    Output: the game's value if it qualifies, else None.
    Purpose: single definition of "this game counts" used for window + season."""
    if is_share:
        if week not in active_weeks:
            return None
        v = game_values.get(week)
        return 0.0 if v is None else v
    v = game_values.get(week)
    if v is None or (volume.get(week) or 0) < min_volume:
        return None
    return v


def select_window(game_values, volume, team_weeks, active_weeks, through_week,
                  is_share=False, min_volume=0, fill="mean"):
    """Inputs: per-week values/volume for one field; team_weeks = weeks the
    player's team played (any order); active_weeks; through_week; fill 'mean'|'median'.
    Output: list of exactly WINDOW values, oldest -> newest, None where nothing
    qualifies (caller substitutes preseason).
    Purpose: applies the missed-game rules in the module docstring."""
    weeks = sorted(w for w in set(team_weeks) if w <= through_week)
    window = weeks[-WINDOW:]
    vals = {w: _qualifying_value(w, game_values, volume, active_weeks, is_share, min_volume)
            for w in weeks}
    bad = [w for w in window if vals[w] is None]
    good = [w for w in window if vals[w] is not None]

    if len(bad) == 0:
        slots = [vals[w] for w in window]
    elif len(bad) == 1 and len(good) >= 1:
        agg = (median if fill == "median" else mean)([vals[w] for w in good])
        slots = [agg if w == bad[0] else vals[w] for w in window]
    else:
        # 2+ missed (or the single miss has nothing to average): use the most
        # recent games he actually qualified in, reaching back past the window.
        picked = [w for w in reversed(weeks) if vals[w] is not None][:WINDOW]
        slots = [vals[w] for w in sorted(picked)]

    slots = slots[-WINDOW:]
    return [None] * (WINDOW - len(slots)) + slots


def season_value(game_values, volume, team_weeks, active_weeks, through_week,
                 is_share=False, min_volume=0):
    """Season-to-date value over qualifying games only. Shares: plain per-game
    mean. Ratios: volume-pooled (sum(v*n)/sum(n)) so a 3-target game doesn't
    count like a 12-target one. None if no game qualifies."""
    weeks = [w for w in set(team_weeks) if w <= through_week]
    vs = [(w, _qualifying_value(w, game_values, volume, active_weeks, is_share, min_volume))
          for w in weeks]
    vs = [(w, v) for w, v in vs if v is not None]
    if not vs:
        return None
    if is_share:
        return mean(v for _, v in vs)
    tot = sum(volume.get(w) or 0 for w, _ in vs)
    if tot <= 0:
        return mean(v for _, v in vs)
    return sum(v * (volume.get(w) or 0) for w, v in vs) / tot


def window_blend(game_values, volume, team_weeks, active_weeks, through_week, preseason,
                 scheme="flat", is_share=False, min_volume=0, fill="mean", fill_value=None):
    """Inputs: one field's per-week log (+volume), team/active weeks, the
    preseason value (may be None), scheme name or dict, fill rule.
    Output: blended float, or None if there is nothing at all to blend.
    fill_value (optional): what an EMPTY slot / empty season uses instead of `preseason`
    -- window_blend_apply passes the player's own pooled 2026 value here once he has
    played 4 games, so preseason only fills for players who have not (Cam 2026-10-07).
    Purpose: the 5-input weighted sum from the module docstring."""
    sch = SCHEMES[scheme] if isinstance(scheme, str) else scheme
    slots = select_window(game_values, volume, team_weeks, active_weeks, through_week,
                          is_share, min_volume, fill)
    season = season_value(game_values, volume, team_weeks, active_weeks, through_week,
                          is_share, min_volume)
    inputs = [(sch["season"], season)] + list(zip(sch["games"], slots))
    num = den = 0.0
    fallback = fill_value if fill_value is not None else preseason
    for w, v in inputs:
        if v is None:
            v = fallback
        if v is None:
            continue
        num += w * v
        den += w
    return None if den == 0 else num / den


def zone_blend(pooled_value, sample_n, preseason, anchor=0.5, min_samples=5):
    """Inputs: pooled_value = the player's zone stat pooled over the last N team
    games (e.g. his red-zone targets / team red-zone targets), sample_n = how
    many real plays of his are behind it, the preseason value, `anchor` = share
    of preseason kept.
    Output: anchor*preseason + (1-anchor)*pooled_value, or preseason alone when
    sample_n < min_samples (or None if there is no preseason either and the
    sample is thin; pooled_value alone when there is no preseason but enough sample).
    Purpose: slow, preseason-anchored zone update (Cam: "half speed")."""
    if pooled_value is None or sample_n is None or sample_n < min_samples:
        return preseason
    return pooled_value if preseason is None else anchor * preseason + (1 - anchor) * pooled_value


def usage_shift_flags(shares_by_player, prior_shares_by_player, threshold=0.05):
    """Inputs/outputs: {player: share} now vs. last week's/preseason. Returns
    [(player, delta)] for |delta| >= threshold, largest first -- a review list
    for Cam's manual week-sheet edits, never auto-applied."""
    flags = [(p, shares_by_player[p] - prior_shares_by_player[p])
             for p in shares_by_player if p in prior_shares_by_player
             and abs(shares_by_player[p] - prior_shares_by_player[p]) >= threshold]
    return sorted(flags, key=lambda x: -abs(x[1]))
