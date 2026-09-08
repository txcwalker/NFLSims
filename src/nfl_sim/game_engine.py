"""Vectorized Monte Carlo NFL game simulation engine.

Simulates N games in parallel using NumPy arrays (yardline, down, distance,
clock, score, possession as length-N vectors), driving the full play loop —
play-type selection, air yards / YAC / rush gains, sacks, turnovers, penalties,
field goals, 4th-down decisions, clock physics, possession changes — via the ML
submodels in ModelRegistry. True vectorization (boolean masking over N lanes)
runs thousands of concurrent sims in seconds instead of looping game objects.

Entry point: NFLGameEngine(away, home, year, N).simulate_play_step().
Full design rationale (SME notes, metric trade-offs): see game_engine.md.
"""

import numpy as np
import pandas as pd
import os
import json
import scipy.stats as stats
import xgboost as xgb
from .model_registry import ModelRegistry
from .proe_overlay_v_0_1_0 import apply_proe_overlay, get_coach_proe, _proe_to_logit_offset, _sigmoid, _logit
from .scoring import get_player_summary

ZONES = ('goalline', 'redzone', 'primary')

# Field-position boundaries (yards from the opponent's goal line) for the
# goalline/redzone/primary zone split -- shared by every zone-split model
# (air yards, YAC, rush yards, catch probability) and Phase 3 play-selection.
# These aren't arbitrary: each deployed model was trained as 3 separate
# zone-specific artifacts using exactly this split (see e.g.
# air_yards_v_0_1_1/train_zone_split.py), so changing these values means
# retraining those models, not just relabeling a threshold here.
GOALLINE_YARDLINE = 5
REDZONE_YARDLINE = 20


def _classify_zone(yardline_100):
    """goalline/redzone/primary classification by field position, shared by
    every call site that needs to bucket plays for a zone-split model (was
    independently duplicated 5x: Phase 3 play-selection, air-yards, catch
    probability, YAC, run-resolution)."""
    return np.where(
        yardline_100 <= GOALLINE_YARDLINE, 'goalline',
        np.where(yardline_100 <= REDZONE_YARDLINE, 'redzone', 'primary')
    )


# Mirrors src/data_pipeline/rolling_stats_v_0_1_0.py's ZONE_SPLIT_FIELDS --
# duplicated as a plain tuple rather than imported so the live engine
# doesn't depend on the offline data-pipeline/build-tooling package.
HANDCUFF_ZONE_FIELDS = ('target_share', 'carry_share', 'catch_rate', 'cpoe')


def resolve_handcuff_overrides(rosters):
    """Applies a designated handcuff's `if_starting` projection (carry_share,
    efficiency fields, etc.) in place of their normal committee-role numbers,
    whenever the specific starter they're designated to replace
    (`backup_to`) is inactive. Exists because "next man up" isn't always
    "whoever currently has the next-highest share" -- a passing-down back
    can out-touch the true bell-cow-capable backup in a committee, but has a
    specialized skill set (pass-block/pass-catch, smaller/quicker) that
    doesn't scale to a full lead-back workload the way the real handcuff's
    does. The passing-down back needs no entry here at all -- his own
    carry_share is untouched, and _precompute_matchup_tables' renormalization
    (dividing every active rusher's carry_share by the active-pool total)
    already gives him a proportionally bigger role automatically once the
    starter drops out of that pool.

    A player needs both `backup_to` (a teammate's exact name) and
    `if_starting` ({field: value}, any subset of the tunable fields) set to
    participate -- both come from data/dna/handcuff_overrides_{year}.csv via
    apply_handcuff_overrides_v_0_1_0.py. No effect on anyone without both,
    and no effect at all while the named starter is still active.

    Copy-on-write: never mutates the input `rosters` dict or any nested
    player dict -- `rosters` may be a shared object reused across other
    concurrent games/instances (e.g. a batch run simulating many matchups
    off one loaded roster registry). Only the specific team/player entries
    that actually change get fresh dicts; everything else is passed through
    by reference."""
    resolved = {}
    for team, roster in rosters.items():
        new_roster = None  # lazy copy-on-write -- stays None if nothing changes
        for name, traits in roster.items():
            backup_to = traits.get('backup_to')
            if_starting = traits.get('if_starting')
            if not backup_to or not if_starting:
                continue
            starter = roster.get(backup_to)
            if starter is None or starter.get('status', 'active') == 'active':
                continue  # named starter is still active -- no override in effect

            merged = dict(traits)
            merged.update(if_starting)
            if any(f in if_starting for f in HANDCUFF_ZONE_FIELDS):
                # Keep the precomputed zone-split tables (which read `splits`
                # before falling back to the flat value) consistent with the
                # override -- same "flat value as every zone's baseline"
                # convention used for rookies with no real per-zone data yet.
                merged['splits'] = {zone: dict(traits.get('splits', {}).get(zone, {})) for zone in ZONES}
                for zone in ZONES:
                    for field in HANDCUFF_ZONE_FIELDS:
                        if field in if_starting:
                            merged['splits'][zone][field] = if_starting[field]

            if new_roster is None:
                new_roster = dict(roster)
            new_roster[name] = merged

        resolved[team] = new_roster if new_roster is not None else roster
    return resolved


def get_safe_float(d, key, default):
    """Coerce a DNA dict field to float, falling back to `default` for
    missing/None/nested (dict or list) values -- receiver DNA occasionally
    has None or split sub-dicts under a key a caller expects to be scalar."""
    val = d.get(key)
    if val is None or isinstance(val, (dict, list)):
        return default
    try:
        return float(val)
    except (ValueError, TypeError):
        return default


def _sigmoid_arr(x):
    return 1.0 / (1.0 + np.exp(-x))


def _logit_arr(p):
    # clip p to prevent log(0) or division by zero
    p_clipped = np.clip(p, 0.01, 0.99)
    return np.log(p_clipped / (1.0 - p_clipped))


# Completion-model depth curve (A2, 2026-09-06). The completion model tapers
# catch probability with target depth. Rather than fit a line (Phase 2 tried a
# two-slope; the real curve is steep 3-8, flat 8-18, steep 18-25, flat 25+ --
# no simple line fits it), this IS the real 2021-2025 completion-vs-air-yards
# curve, in logit space, linearly interpolated. Source:
# scripts/eda/analyze_completion_by_depth.py (real TARGETS only -- receiver_id
# not null, excludes throwaways). `_DEPTH_AY` = bucket-mean air yards (+ endpoint
# anchors at ~1 and ~55 yds); `_DEPTH_CMP` = real completion at each.
# Only the SHAPE matters here -- delta_wr's `curve(ay) - curve(adot)`
# construction cancels any level. Backtrack: replace with a flat line.
_DEPTH_AY = np.array([1.0, 3.4, 7.6, 12.9, 17.8, 24.7, 39.0, 55.0], dtype=np.float32)
_DEPTH_CMP = np.array([0.800, 0.764, 0.659, 0.588, 0.528, 0.392, 0.301, 0.210], dtype=np.float32)
_DEPTH_LOGIT_PTS = np.log(_DEPTH_CMP / (1.0 - _DEPTH_CMP)).astype(np.float32)


def _depth_logit(air_yards):
    """Logit-space completion level for a throw of `air_yards` depth -- the real
    completion-vs-depth curve, interpolated. Array (or scalar) in, same out."""
    return np.interp(np.asarray(air_yards, dtype=np.float32), _DEPTH_AY, _DEPTH_LOGIT_PTS).astype(np.float32)


# Receiver-skill shrink (A2 Phase 3, 2026-09-06). Real completion at a given
# throw depth is ~flat across receiver ADOT (analyze_completion_by_receiver_adot
# .py), so the per-receiver adjustment `logit(zone_baseline) - curve(adot)`
# should be trusted only partially -- a receiver whose catch rate implies "+8pp
# vs league at their depth" is really more like +4-5pp. 1.0 = full (old
# behaviour), 0.0 = pure population curve. Backtrack: SKILL_SHRINK = 1.0.
SKILL_SHRINK = 0.7


# Completion-model ADOT anchor soft-cap (A2 Phase 2a, 2026-09-06). The model
# anchors each receiver's catch rate at their ADOT and adjusts along the depth
# curve for the actual throw. Real 2021-2025 data (scripts/eda/
# analyze_completion_by_receiver_adot.py) shows completion at a given throw
# depth is ~flat across receiver ADOT -- so a deep specialist thrown a short
# pass does NOT catch it much better than a possession receiver. The raw
# `g(air_yards) - g(ADOT)` construction badly over-credits them (a 26-ADOT
# receiver was modeled at ~80% on a 5-10 yd throw vs ~62% real). Fix: compress
# the ADOT used for anchoring above a knee -- untouched up to ADOT_ANCHOR_KNEE
# (every real possession WR is below it), a fraction ADOT_ANCHOR_COMPRESS of the
# excess removed above it. Backtrack: set ADOT_ANCHOR_COMPRESS = 0.
ADOT_ANCHOR_KNEE = 16.0
ADOT_ANCHOR_COMPRESS = 0.75


def _anchor_adot(adot):
    """Soft-capped ADOT for the completion-model anchor. See constants above."""
    a = np.asarray(adot, dtype=np.float32)
    return np.minimum(a, ADOT_ANCHOR_KNEE) + np.maximum(a - ADOT_ANCHOR_KNEE, 0.0) * (1.0 - ADOT_ANCHOR_COMPRESS)


# Open-field completion adjustments (A2 Phase 4, 2026-09-06 -- joint re-cal after
# correcting the real baseline: it had been measured against completions / ALL
# pass attempts incl. throwaways/batted balls, but the sim's completion model
# only ever fires on real targets, whose real 2021-2025 completion is 67.5%, not
# the 64.4% box-score number. See completion_rate_calibration_plan.md Phase 4).
#
# SEP_BONUS_SCALE: the play-to-play separation roll feeds a logit adjustment.
#   It is now ZERO-MEAN per receiver -- `scale * (sep_roll - receiver_avg_sep)`.
#   Previously `0.15 * (sep_roll - 1.0)`, which added ~+0.31 logit (~+6pp) to
#   EVERY open throw on top of a `zone_baseline` that already reflects the
#   receiver's real separation -- double-counting it. Now only the deviation
#   from the receiver's norm matters.
# OPEN_FIELD_CALIBRATION_OFFSET: flat prob-space offset on the final catch prob.
#   Re-fit against the corrected curve alongside the sep_bonus change and the
#   Phase 2 deep slope. Backtrack: 0.075 + `sep_bonus = 0.15*(sep_roll - 1.0)`.
SEP_BONUS_SCALE = 0.13
OPEN_FIELD_CALIBRATION_OFFSET = 0.0


# Throwaway rate (2026-09-06). Real QBs throw the ball away ~5% of dropbacks
# (PFF charting; AGENTS.md §11.6 target). The engine only ever produced ~0.4%
# (a 6% diversion from would-be-sacks, PRESSURE_THROWAWAY_DIVERT below), so QB
# completion % ran ~3-4pp hot vs the real box-score number -- which DOES count
# throwaways as incomplete attempts. This adds a clean-pocket / "nothing open"
# throwaway path on otherwise-normal dropbacks. Diverted plays become incomplete
# pass attempts (count as pAtt, not pCmp, no target, 0 air yards) and never
# reach the completion model, matching how real target/catch-rate stats are
# computed. The sack model is left untouched -- Gate 2 is trained on real
# RECORDED sacks, which already exclude plays where the QB threw it away.
# Backtrack: CLEAN_POCKET_THROWAWAY_RATE = 0.0.
CLEAN_POCKET_THROWAWAY_RATE = 0.055
PRESSURE_THROWAWAY_DIVERT = 0.06   # fraction of would-be-sacks that become throwaways


# Trench tail-widening (2026-09-06, Cam's call). The composite z-scores in
# trench_dna.json (run_block_off_z / pass_block_off_z / run_def_z / pass_def_z)
# are ~standard-normal, so the top/bottom teams sit only ~1.5-2 sd from average
# -- not enough separation for a genuinely elite or genuinely bad line to
# dominate/get-dominated the way it should. This applies a progressive gain:
# a z near 0 is nearly unchanged, a z at the tail is stretched by up to
# (1 + TRENCH_TAIL_GAIN * TRENCH_TAIL_CAP)x. So a bad line still gets punished
# even against an average front, and an elite line pulls away. Backtrack:
# TRENCH_TAIL_GAIN = 0.
TRENCH_TAIL_GAIN = 0.35
TRENCH_TAIL_CAP = 2.5
TRENCH_SPREAD_CLAMP = 3.5   # keep the transformed z inside the gates' training range


def _trench_spread(z):
    """Progressive tail-widening of a trench composite z-score. See constants."""
    stretched = z * (1.0 + TRENCH_TAIL_GAIN * min(abs(z), TRENCH_TAIL_CAP))
    return max(-TRENCH_SPREAD_CLAMP, min(TRENCH_SPREAD_CLAMP, stretched))


# PROE game-script fade (2026-09-06, Cam's call). Coach PROE is an ALL-TIME
# neutral-script tendency -- how a coach deviates from expected pass rate in a
# typical situation. Applying it flat across every game state makes a run-heavy
# coach (e.g. Monken/CLE -5.25) keep running when down multiple scores late,
# which real teams don't. This fades the PROE offset toward 0 as the score gap
# widens beyond one score, faster after halftime, so the (well-calibrated) base
# play-selection model's own script response takes over in blowouts. Symmetric
# in lead/trail (a team up 20 late also has its scheme tendency overridden by
# clock management). Backtrack: PROE_FADE_SLOPE = 0.
PROE_FADE_SLOPE = 0.04       # weight lost per point beyond one score, 1st half
PROE_FADE_MAX = 0.85         # cap on total weight lost (floor weight = 0.15)
PROE_FADE_2H_FACTOR = 2.0    # fade this many times faster once game_sec <= 1800


def _proe_script_weight(score_diff, game_sec):
    """Per-play multiplier on the PROE logit offset. 1.0 in one-score games,
    fading toward (1 - PROE_FADE_MAX) as the margin blows out. Vectorised."""
    excess = np.maximum(np.abs(score_diff) - 8.0, 0.0)
    half_factor = np.where(game_sec <= 1800, PROE_FADE_2H_FACTOR, 1.0)
    return 1.0 - np.minimum(PROE_FADE_MAX, PROE_FADE_SLOPE * excess * half_factor)


# Q4 trailing pass-rate target (2026-09-06, Cam's call). The base play-selection
# model is well-calibrated everywhere EXCEPT it under-reacts to being down 2+
# scores in the 4th quarter -- real teams there pass ~73-92% (down 2 scores,
# rising as the clock dies) / ~72-77% (down 3+), and the model only gets to
# ~72-85%. Rather than retrain, blend the model's Q4-trailing output toward the
# real empirical curve (scripts/eda/analyze_pass_rate_by_script.py). Only fires
# in Q4, only down 9+; one-score and pre-Q4 situations are untouched.
# Backtrack: Q4_TRAIL_BLEND = 0.
Q4_TRAIL_BLEND = 0.65


def _q4_trailing_pass_target(score_diff, game_sec):
    """Empirical real pass rate for a Q4 trailing situation; NaN where the
    overlay doesn't apply (one score, or not Q4). Vectorised. `frac` = fraction
    of Q4 still to play (1.0 at 15:00, 0.0 at 0:00)."""
    d = -np.asarray(score_diff, dtype=np.float64)
    gs = np.asarray(game_sec, dtype=np.float64)
    frac = np.clip(gs / 900.0, 0.0, 1.0)
    late = 1.0 - frac
    two_score = 0.72 + 0.20 * late ** 2          # 0.72 early Q4 -> 0.92 at 0:00
    three_plus = 0.77 - 0.06 * late ** 3         # 0.77 -> 0.71 (teams ease off once it's out of reach)
    tgt = np.where(d >= 17, three_plus, two_score)
    applies = (gs < 900) & (d >= 9)
    return np.where(applies, tgt, np.nan)


def _zone_scalar_vector(zone_dict, zones):
    """Map a single entity's {zone: scalar} dict across a `zones` string
    array. Used where the entity (e.g. a team's starting QB) is fixed for the
    whole call but the zone varies per play -- avoids the per-play dict
    lookups this collapses (was duplicated across Phase 3 play-selection,
    sack-gate, air-yards, YAC, and run-resolution)."""
    out = np.empty(zones.shape, dtype=np.float32)
    for zone in ZONES:
        out[zones == zone] = zone_dict.get(zone, 0.0)
    return out


def _zone_split_vector(precomputed_dict, names, zones):
    """Vectorized replacement for 'for each unique name in `names`: for each
    zone: fill matching slots', keyed off a precomputed {name: {zone: scalar}}
    dict. Used for air-yards/YAC receiver target_share/carry_share, where the
    set of names varies per play (unlike `_zone_scalar_vector`'s fixed-entity
    case)."""
    out = np.zeros(len(names), dtype=np.float32)
    for name in np.unique(names):
        if name is None or name == "Unknown":
            continue
        entry = precomputed_dict.get(name)
        if not entry:
            continue
        name_mask = (names == name)
        for zone in ZONES:
            z_mask = name_mask & (zones == zone)
            if np.any(z_mask):
                out[z_mask] = entry.get(zone, 0.0)
    return out


class NFLGameEngine:
    def __init__(self, away_team, home_team, year=2025, dna=None, team_coaches=None, rosters=None,
                trench_tiers=None, N=10000, is_playoff=False, rosters_dir="data/current_rosters"):
        self.away_team = away_team
        self.home_team = home_team
        self.year = year
        # Lets a caller point at a parallel roster tree (e.g.
        # data/current_rosters/dfs) instead of the season-long
        # current_rosters/{team}_traits_{year}.json -- e.g. the DFS weekly
        # pipeline compiles its own {team}_traits_{year}.json under
        # current_rosters/dfs/ so a week's injury-adjusted usage never
        # overwrites the season-long file. Ignored when `rosters` is passed
        # directly. Default reproduces the exact prior hardcoded path.
        self.rosters_dir = rosters_dir
        self.N = N
        self.is_playoff = is_playoff
        self.registry = ModelRegistry()
        
        # Load DNA Registry
        self.dna = dna if dna is not None else {
            'qb': self._load_json('data/dna/qb_dna.json'),
            'skill': self._load_skill_dna(),
            'coach': self._load_json('data/dna/coach_dna.json'),
            'trench': self._load_json('data/dna/trench_dna.json')
        }
        
        self.team_coaches = team_coaches if team_coaches is not None else self._load_json(f'data/dna/team_to_coach_{self.year}.json')
        self.rosters = rosters if rosters is not None else {
            away_team: self._load_json(f"{self.rosters_dir}/{away_team}_traits_{self.year}.json").get('traits', {}),
            home_team: self._load_json(f"{self.rosters_dir}/{home_team}_traits_{self.year}.json").get('traits', {})
        }
        self.rosters = resolve_handcuff_overrides(self.rosters)

        # Trench Tiers
        self.trench_tiers = trench_tiers if trench_tiers is not None else self._load_json(f'data/dna/trench_tiers_{self.year}.json')
        
        # Precompute static coach aggression adjustments
        self.coach_aggression = {}
        for team in [away_team, home_team]:
            coach_name = self.team_coaches.get(team, "Unknown")
            coach_dna = self.dna['coach'].get(coach_name, {})
            self.coach_aggression[team] = (coach_dna.get('deep_shot_rate', 0.12) - 0.12) * 2.0
            
        # Precompute static roster information to bypass lookups in game loop
        self.qb_starters = {
            away_team: self._get_starter_static(away_team, 'QB'),
            home_team: self._get_starter_static(home_team, 'QB')
        }
        
        self.rb_starters = {
            away_team: self._get_starter_static(away_team, 'RB'),
            home_team: self._get_starter_static(home_team, 'RB')
        }

        # Matchup strength multipliers to widen spreads and increase totals
        away_t = self.trench_tiers.get(away_team, {})
        home_t = self.trench_tiers.get(home_team, {})

        # QB cpoe z difference (to further separate passing efficiency).
        # NOTE: this used to also fold in a trench_tiers-based
        # pass_rush_tier/pass_block_tier difference (`pass_diff_*_off`) --
        # that piece is dropped here, now superseded by the Gate 2b
        # sack-probability correction below (real composite data, not the
        # old 1-5 tier grades). The QB CPOE differential is a genuinely
        # separate signal (quarterback skill, not O-line/D-line matchup) and
        # is kept as-is -- it was never part of the trench work.
        qb_diff_away_off = away_t.get('qb_cpoe_z', 0.0) - home_t.get('qb_cpoe_z', 0.0)
        qb_diff_home_off = home_t.get('qb_cpoe_z', 0.0) - away_t.get('qb_cpoe_z', 0.0)

        self.pass_mult_away_off = 1.00 + qb_diff_away_off * 0.04
        self.pass_mult_home_off = 1.00 + qb_diff_home_off * 0.04

        # Gate 2b (v0.4.0): sack-probability correction using real
        # pass-block/pass-rush composites (pass_block_off_z, pass_def_z --
        # built by scripts/eda/build_trench_dna_pass_composites.py), sitting
        # alongside Gate 2 rather than replacing it. See
        # src/nfl_sim/models/chaos_v_0_1_0/train_gate2b_trench_correction.py
        # for the full rationale and how it was trained/validated.
        trench_pass_away = self.dna['trench'].get(str(self.year), {}).get(away_team, {})
        trench_pass_home = self.dna['trench'].get(str(self.year), {}).get(home_team, {})
        self.pass_block_off_z_away = _trench_spread(trench_pass_away.get('pass_block_off_z', 0.0))
        self.pass_block_off_z_home = _trench_spread(trench_pass_home.get('pass_block_off_z', 0.0))
        self.pass_def_z_away = _trench_spread(trench_pass_away.get('pass_def_z', 0.0))
        self.pass_def_z_home = _trench_spread(trench_pass_home.get('pass_def_z', 0.0))

        # Rush trench matchup gate (v0.4.0): replaces the old trench_tiers-based
        # run_mult_*_off flat per-game scalar. Composite z-scores
        # (run_block_off_z, run_def_z) live in trench_dna.json, built by
        # scripts/eda/build_trench_dna_composites.py from real per-team-season
        # blocking/run-defense metrics. The gate decides, per PLAY, whether to
        # route to a dedicated negative- or explosive-yardage draw instead of
        # the normal rush model -- EDA showed matchup quality mostly
        # reallocates how often a play lands in each mode, not what a normal
        # play looks like. Gate probabilities are calibrated (via
        # scripts/model_training/measure_rush_organic_tail_rates.py) to only
        # make up the gap between the base model's own organic negative/
        # explosive rate and the real target rate at that matchup strength --
        # not stacked on top of it -- so plays aren't double-counted between
        # the organic model and the new gate. See docs/eda_outputs/trench/.
        trench_away = self.dna['trench'].get(str(self.year), {}).get(away_team, {})
        trench_home = self.dna['trench'].get(str(self.year), {}).get(home_team, {})
        self.rush_matchup_away = _trench_spread(trench_away.get('run_block_off_z', 0.0)) - _trench_spread(trench_home.get('run_def_z', 0.0))
        self.rush_matchup_home = _trench_spread(trench_home.get('run_block_off_z', 0.0)) - _trench_spread(trench_away.get('run_def_z', 0.0))

        self.rush_gate_calib = self._load_json('docs/eda_outputs/trench/rush_gate_calibration.json')
        organic_neg = self.rush_gate_calib.get('organic_neg_rate', 0.0)
        organic_exp = self.rush_gate_calib.get('organic_exp_rate', 0.0)
        neg_a = self.rush_gate_calib.get('target_neg_intercept', 0.0)
        neg_b = self.rush_gate_calib.get('target_neg_slope', 0.0)
        exp_a = self.rush_gate_calib.get('target_exp_intercept', 0.0)
        exp_b = self.rush_gate_calib.get('target_exp_slope', 0.0)

        def _gate_prob(matchup_z, target_a, target_b, organic_rate):
            # p_gate = (target(z) - organic) / (1 - organic), clipped to [0,1] --
            # the calibration step that keeps this from double-counting with
            # the base model's own organic negative/explosive tail.
            target = target_a + target_b * matchup_z
            denom = max(1e-6, 1.0 - organic_rate)
            return float(np.clip((target - organic_rate) / denom, 0.0, 1.0))

        self.p_neg_gate_away = _gate_prob(self.rush_matchup_away, neg_a, neg_b, organic_neg)
        self.p_neg_gate_home = _gate_prob(self.rush_matchup_home, neg_a, neg_b, organic_neg)
        self.p_exp_gate_away = _gate_prob(self.rush_matchup_away, exp_a, exp_b, organic_exp)
        self.p_exp_gate_home = _gate_prob(self.rush_matchup_home, exp_a, exp_b, organic_exp)

        def _rescue_prob(matchup_z, target_a, target_b, organic_rate):
            # Symmetric to _gate_prob, opposite direction: p_neg_gate floors at 0
            # whenever target(z) < organic (the base model already overshoots the
            # real negative rate on its own -- an additive-only gate can't reduce
            # below that). This closes that gap: for a "normal"-mode play that
            # lands <=0, roll this probability to redraw it as a non-negative
            # outcome instead. Zero everywhere target(z) >= organic (i.e.
            # everywhere the plain gate already handles things correctly) --
            # only activates at the matchup extremes where organic overshoots.
            target = target_a + target_b * matchup_z
            denom = max(1e-6, organic_rate)
            return float(np.clip((organic_rate - target) / denom, 0.0, 1.0))

        self.p_rescue_gate_away = _rescue_prob(self.rush_matchup_away, neg_a, neg_b, organic_neg)
        self.p_rescue_gate_home = _rescue_prob(self.rush_matchup_home, neg_a, neg_b, organic_neg)

        self.rush_negative_pool = np.array(self.rush_gate_calib.get('negative_pool', [0.0]), dtype=np.float32)
        self.rush_explosive_pool = np.array(self.rush_gate_calib.get('explosive_pool', [10.0]), dtype=np.float32)
        self.rush_positive_pool = np.array(self.rush_gate_calib.get('positive_pool', [4.0]), dtype=np.float32)

        # Vectorized Game States (size N)
        # We represent possession as boolean: True for away_team, False for home_team
        self.possession_is_away = np.ones(self.N, dtype=bool)
        self.yardline_100 = np.full(self.N, 70, dtype=np.int32)
        self.down = np.ones(self.N, dtype=np.int32)
        self.distance = np.full(self.N, 10, dtype=np.int32)
        self.score_away = np.zeros(self.N, dtype=np.int32)
        self.score_home = np.zeros(self.N, dtype=np.int32)
        self.quarter = np.ones(self.N, dtype=np.int32)
        self.time_remaining = np.full(self.N, 900, dtype=np.int32) # Seconds
        self.game_over = np.zeros(self.N, dtype=bool)
        self.needs_kickoff = np.ones(self.N, dtype=bool)
        self.clock_stopped = np.ones(self.N, dtype=bool)
        self.timeouts_away = np.full(self.N, 3, dtype=np.int32)
        self.timeouts_home = np.full(self.N, 3, dtype=np.int32)

        # Overtime: both teams get a guaranteed possession before a score
        # differential can end the game (sudden death only kicks in once both
        # flags are True -- see the OT resolution check near the end of
        # simulate_play_step). Reset to False every time a new OT period
        # starts (regulation->OT, and each playoff period after that). Not
        # meaningful until self.quarter >= 5; harmless before then.
        self.ot_away_possessed = np.zeros(self.N, dtype=bool)
        self.ot_home_possessed = np.zeros(self.N, dtype=bool)
        self.play_count = np.zeros(self.N, dtype=np.int32)
        
        # Metrics Tracking (aggregated vectorially)
        self.plays_over_20_yds = np.zeros(self.N, dtype=np.int32)
        self.punts_run = np.zeros(self.N, dtype=np.int32)
        self.fg_attempts_away = np.zeros(self.N, dtype=np.int32)
        self.fg_attempts_home = np.zeros(self.N, dtype=np.int32)
        self.kickoffs_run = np.zeros(self.N, dtype=np.int32)
        self.presnap_penalty_snaps = np.zeros(self.N, dtype=np.int32)
        self.penalties_accepted = np.zeros(self.N, dtype=np.int32)

        # Completion-by-target-depth instrumentation (A2 Phase 0, 2026-09-06 --
        # docs/implementation_plans/completion_rate_calibration_plan.md). Off by
        # default: zero cost and zero behaviour change on a normal run. A
        # calibration script sets `engine.track_cmp_by_depth = True` before
        # run_game() and reads the two counters after. Bucket edges match
        # scripts/eda/analyze_completion_by_depth.py's real-side buckets exactly
        # (air_yards in (-inf,0],(0,5],(5,10],(10,15],(15,20],(20,30],(30,inf)).
        self.track_cmp_by_depth = False
        self._cmp_depth_edges = np.array([0, 5, 10, 15, 20, 30], dtype=np.float32)
        self._cmp_depth_att = np.zeros(len(self._cmp_depth_edges) + 1, dtype=np.int64)
        self._cmp_depth_cmp = np.zeros(len(self._cmp_depth_edges) + 1, dtype=np.int64)
        # dropback outcome tallies (same flag): [dropbacks, sacks, scrambles, throwaways]
        self._dropback_tally = np.zeros(4, dtype=np.int64)
        # play-call-by-script tally (self.track_playcall): rows [plays, passes,
        # sum base_pass_prob, sum adjusted_pass_prob], 28 cells (7 score bands x 4 time bands)
        self.track_playcall = False
        self._playcall_tally = np.zeros((4, 28), dtype=np.float64)

        # A2 Phase 2: capture the completion-model inputs per non-screen throw
        # (air_yards, adot, zone/contested anchor, separation roll, qb cpoe) so
        # scripts/eda/calibrate_completion_curve.py can refit the depth curve
        # offline. Off by default, zero cost.
        self.capture_completion = False
        self._completion_cap = []


        # B3 (clock_physics_v020): trailing-defense timeout strategy, decided
        # once per simulated game (not re-rolled each play). "Early" burns
        # timeouts starting at 4:00 remaining in Q4; "Late" holds until after
        # the two-minute warning. 50/50 split — real coach-specific tendency
        # is a deferred, later refinement (see docs/audit/clock_physics_v020).
        self.trailing_defense_early_strategy = np.random.rand(self.N) < 0.5

        # Two-minute warning (clock_physics_v020): tracks whether the automatic
        # stoppage has already fired this half (Q2/Q4). Reset on quarter transition.
        self.two_minute_warning_used = np.zeros(self.N, dtype=bool)

        # -------------------------------------------------------------
        # Positional Evaluator Hook (additive — does NOT affect game logic)
        # -------------------------------------------------------------
        # Exposes the most recent scrimmage play's classification vectors so an
        # external drive-rollout harness (nfl_positional_evaluator.py) can snapshot
        # the FIRST executed play per lane for chess-style concept evaluation.
        # Updated at the end of every simulate_play_step that reaches play execution.
        #   last_play_scrimmage_mask : bool[N]  lanes that ran a real scrimmage down this step
        #   last_play_is_pass        : bool[N]  designed pass (incl. sacks/scrambles)
        #   last_play_is_run         : bool[N]  designed run
        #   last_play_air_yards      : int[N]   intended air yards (0 for runs/sacks)
        #   last_play_is_sack        : bool[N]  dropback ended in a sack
        #   last_play_is_scramble    : bool[N]  designed pass where QB scrambled
        #   last_play_pre_yardline_100 : int[N] yardline_100 BEFORE this step's play resolved
        #   last_play_is_complete_pass : bool[N] pass completion resolved this step
        #   last_play_yac              : int[N] yards after catch on a completion (0 otherwise)
        #   last_play_gain             : int[N] final yards gained this step (any play type)
        #   last_play_target_name      : object[N] receiver targeted on a pass (None otherwise)
        #   last_play_rusher_name      : object[N] ball carrier on a run (None otherwise)
        #   last_play_is_normal_play   : bool[N] active & not sack/interception/lost-fumble/accepted-penalty
        #                                (the mask that gates the yardline_100/down/distance update this step)
        self.last_play_scrimmage_mask = np.zeros(self.N, dtype=bool)
        self.last_play_is_pass = np.zeros(self.N, dtype=bool)
        self.last_play_is_run = np.zeros(self.N, dtype=bool)
        self.last_play_air_yards = np.zeros(self.N, dtype=np.int32)
        self.last_play_is_sack = np.zeros(self.N, dtype=bool)
        self.last_play_is_scramble = np.zeros(self.N, dtype=bool)
        self.last_play_pre_yardline_100 = np.zeros(self.N, dtype=np.int32)
        self.last_play_is_complete_pass = np.zeros(self.N, dtype=bool)
        self.last_play_yac = np.zeros(self.N, dtype=np.int32)
        self.last_play_gain = np.zeros(self.N, dtype=np.int32)
        self.last_play_target_name = np.empty(self.N, dtype=object)
        self.last_play_rusher_name = np.empty(self.N, dtype=object)
        self.last_play_is_normal_play = np.zeros(self.N, dtype=bool)

        # Initialize player stats vectorially: dict of {team: {player: {stat: np.zeros(N)}}}
        self.player_stats = {away_team: {}, home_team: {}}
        for team in [away_team, home_team]:
            for player, traits in self.rosters[team].items():
                self.player_stats[team][player] = {
                    'Pos': traits.get('pos', 'WR/TE'),
                    'rAtt': np.zeros(self.N, dtype=np.int32),
                    'rYds': np.zeros(self.N, dtype=np.int32),
                    'rTD': np.zeros(self.N, dtype=np.int32),
                    'pAtt': np.zeros(self.N, dtype=np.int32),
                    'pCmp': np.zeros(self.N, dtype=np.int32),
                    'pYds': np.zeros(self.N, dtype=np.int32),
                    'pTD': np.zeros(self.N, dtype=np.int32),
                    'int': np.zeros(self.N, dtype=np.int32),
                    'rec': np.zeros(self.N, dtype=np.int32),
                    'recYds': np.zeros(self.N, dtype=np.int32),
                    'recTD': np.zeros(self.N, dtype=np.int32),
                    'targets': np.zeros(self.N, dtype=np.int32),
                    'fumbles': np.zeros(self.N, dtype=np.int32),
                    'sacks_taken': np.zeros(self.N, dtype=np.int32),
                    'fumbles_lost': np.zeros(self.N, dtype=np.int32),
                    'air_yards': np.zeros(self.N, dtype=np.int32),
                    'def_sack': np.zeros(self.N, dtype=np.int32),
                    'def_int': np.zeros(self.N, dtype=np.int32),
                    'def_fumble_rec': np.zeros(self.N, dtype=np.int32),
                    'def_td': np.zeros(self.N, dtype=np.int32),
                    'pts_allowed': np.zeros(self.N, dtype=np.int32)
                }
            # Initialize defense player representing team D/ST
            self.player_stats[team]["Defense"] = {
                'Pos': 'DST',
                'rAtt': np.zeros(self.N, dtype=np.int32),
                'rYds': np.zeros(self.N, dtype=np.int32),
                'rTD': np.zeros(self.N, dtype=np.int32),
                'pAtt': np.zeros(self.N, dtype=np.int32),
                'pCmp': np.zeros(self.N, dtype=np.int32),
                'pYds': np.zeros(self.N, dtype=np.int32),
                'pTD': np.zeros(self.N, dtype=np.int32),
                'int': np.zeros(self.N, dtype=np.int32),
                'rec': np.zeros(self.N, dtype=np.int32),
                'recYds': np.zeros(self.N, dtype=np.int32),
                'recTD': np.zeros(self.N, dtype=np.int32),
                'targets': np.zeros(self.N, dtype=np.int32),
                'fumbles': np.zeros(self.N, dtype=np.int32),
                'sacks_taken': np.zeros(self.N, dtype=np.int32),
                'fumbles_lost': np.zeros(self.N, dtype=np.int32),
                'air_yards': np.zeros(self.N, dtype=np.int32),
                'def_sack': np.zeros(self.N, dtype=np.int32),
                'def_int': np.zeros(self.N, dtype=np.int32),
                'def_fumble_rec': np.zeros(self.N, dtype=np.int32),
                'def_td': np.zeros(self.N, dtype=np.int32),
                'pts_allowed': np.zeros(self.N, dtype=np.int32)
            }

        # Cache receivers and carry share cumulative distributions for fast
        # sampling -- one per zone (primary/redzone/goalline), not one flat
        # game-long distribution. A zone's per-player share falls back to the
        # player's flat season-long share whenever splits[zone] is missing a
        # field (either the player never had enough real zone volume to earn
        # a real value -- see build_redzone_goalline_shares_v_0_1_0.py -- or
        # the zone is "primary", which was never given its own split at all).
        # Before this (2026-08-13), a real goalline bruiser and a real
        # receiving-down/passing-situation back had identical odds of getting
        # the ball on 4th-and-goal from the 1 -- this is what actually reads
        # splits.redzone/goalline, closing that gap.
        self.receivers_cache = {}
        self.rusher_cache = {}
        for team in [away_team, home_team]:
            team_roster = self.rosters[team]
            self.receivers_cache[team] = {}
            self.rusher_cache[team] = {}

            recs = [p for p, t in team_roster.items() if t.get('pos') != 'QB' and t.get('status', 'active') == 'active']
            # QBs are only eligible for this shared designed-run pool if they're
            # the actual starter -- otherwise a backup QB's carry_share (meant
            # for sneaks/design runs if/when they're under center) leaks into
            # every game's rush distribution regardless of who's starting.
            # Scrambles are unaffected (always attributed to qb_starters[team]
            # directly, see play_is_scramble handling below).
            rushers = [
                p for p, t in team_roster.items()
                if t.get('carry_share', 0) > 0 and t.get('status', 'active') == 'active'
                and (t.get('pos') != 'QB' or p == self.qb_starters[team])
            ]

            for zone in ('primary', 'redzone', 'goalline'):
                if recs:
                    shares = np.array([
                        team_roster[r].get('splits', {}).get(zone, {}).get('target_share', team_roster[r].get('target_share', 0.0))
                        for r in recs
                    ], dtype=np.float32)
                    total_shares = shares.sum()
                    if total_shares <= 0:
                        shares = np.ones_like(shares) / len(shares)
                    else:
                        shares /= total_shares
                    self.receivers_cache[team][zone] = (np.array(recs, dtype=object), np.cumsum(shares))
                else:
                    self.receivers_cache[team][zone] = (np.array(["Unknown"], dtype=object), np.array([1.0], dtype=np.float32))

                if rushers:
                    shares = np.array([
                        team_roster[r].get('splits', {}).get(zone, {}).get('carry_share', team_roster[r].get('carry_share', 0.0))
                        for r in rushers
                    ], dtype=np.float32)
                    total_shares = shares.sum()
                    if total_shares <= 0:
                        shares = np.ones_like(shares) / len(shares)
                    else:
                        shares /= total_shares
                    self.rusher_cache[team][zone] = (np.array(rushers, dtype=object), np.cumsum(shares))
                else:
                    self.rusher_cache[team][zone] = (np.array([self.rb_starters[team]], dtype=object), np.array([1.0], dtype=np.float32))

        # Precompute play selection logit offsets to avoid doing it inside the loops.
        # Phase 7 fix (2026-07-22): get_coach_proe() used to always join
        # against team_to_coach_2025.json regardless of self.year -- passing
        # self.year here now, same bug class as the original Phase 0 fix.
        self.proe_offsets = {
            away_team: _proe_to_logit_offset(get_coach_proe(away_team, self.year)),
            home_team: _proe_to_logit_offset(get_coach_proe(home_team, self.year))
        }

        # Precompute DNA splits lookup maps for the loaded teams to avoid deep
        # splits lookups inside the high-frequency loops.
        #
        # Phase 7 fix (2026-07-22): this used to iterate self.registry.qb_dna/
        # skill_dna -- the ModelRegistry singleton's copy of the STATIC career-
        # average qb_dna.json/skill_dna.json, loaded once per process and
        # shared across every concurrent game, never reflecting the weekly
        # refresh job's blended output. Now sourced from self.rosters (the
        # real per-week snapshot refresh_weekly_dna_v_0_1_0.py writes), and
        # scoped to just this game's two rosters instead of the whole league
        # (cheaper, and the registry's league-wide scope was never needed --
        # only players on these two teams can ever be qb_starters/rb_starters
        # or a targeted receiver in this game). self.dna['qb']/self.dna['skill']
        # (per-instance, also static) are kept as a defensive fallback only
        # for a player somehow missing from self.rosters or a specific field.
        self.precomputed_qb_cpoe = {}
        self.precomputed_skill_target_share = {}
        self.precomputed_skill_carry_share = {}
        for team in [away_team, home_team]:
            for player_name, player_data in self.rosters[team].items():
                if player_data.get('pos') == 'QB':
                    dna_fallback = self.dna['qb'].get(player_name, {})
                    flat_cpoe = player_data.get('cpoe', dna_fallback.get('cpoe', 0.0))
                    splits = player_data.get('splits', {})
                    self.precomputed_qb_cpoe[player_name] = {
                        'goalline': splits.get('goalline', {}).get('cpoe', flat_cpoe),
                        'redzone': splits.get('redzone', {}).get('cpoe', flat_cpoe),
                        'primary': splits.get('primary', {}).get('cpoe', flat_cpoe),
                    }
                else:
                    dna_fallback = self.dna['skill'].get(player_name, {})
                    flat_target_share = player_data.get('target_share', dna_fallback.get('target_share', 0.0))
                    flat_carry_share = player_data.get('carry_share', dna_fallback.get('carry_share', 0.0))
                    splits = player_data.get('splits', {})
                    self.precomputed_skill_target_share[player_name] = {
                        'goalline': splits.get('goalline', {}).get('target_share', flat_target_share),
                        'redzone': splits.get('redzone', {}).get('target_share', flat_target_share),
                        'primary': splits.get('primary', {}).get('target_share', flat_target_share),
                    }
                    self.precomputed_skill_carry_share[player_name] = {
                        'goalline': splits.get('goalline', {}).get('carry_share', flat_carry_share),
                        'redzone': splits.get('redzone', {}).get('carry_share', flat_carry_share),
                        'primary': splits.get('primary', {}).get('carry_share', flat_carry_share),
                    }
            
        self.precomputed_coach_proe = {}
        for coach_name, coach_data in self.registry.coach_proe_splits.items():
            self.precomputed_coach_proe[coach_name] = {
                'goalline': coach_data.get('goalline', 0.0),
                'redzone': coach_data.get('redzone', 0.0),
                'primary': coach_data.get('primary', 0.0),
            }

        self._precompute_matchup_tables()

    def _precompute_matchup_tables(self):
        """Per-team/per-zone scalars derived from qb_starters/rb_starters/dna
        -- fixed for the life of the game (starters don't change mid-game),
        so this only needs to run once instead of being re-derived every play
        (it previously was, independently, in Phase 3 play-selection, the
        sack-gate block, air-yards, YAC, and run-resolution). Split out as a
        re-callable method rather than inlined __init__ code so a future
        season-sim wrapper can call it again after mutating self.dna/
        self.rosters between simulated weeks (rolling last-4-games stats) --
        it only reads from already-mutable instance state.
        """
        self.zone_cpoe = {}
        self.zone_target_share = {}
        self.zone_carry_share = {}
        self.team_sack_features = {}
        default_zone_dict = {z: 0.0 for z in ZONES}

        for team in [self.away_team, self.home_team]:
            qb = self.qb_starters[team]
            rb = self.rb_starters[team]
            self.zone_cpoe[team] = self.precomputed_qb_cpoe.get(qb, default_zone_dict)
            self.zone_target_share[team] = self.precomputed_skill_target_share.get(rb, default_zone_dict)
            self.zone_carry_share[team] = self.precomputed_skill_carry_share.get(rb, default_zone_dict)

            t = self.dna['trench'].get(str(self.year), {}).get(team, {})
            # qb_cpoe: prefer the live per-week roster value (Phase 7 fix),
            # fall back to the static career-average DNA file only if the
            # starter is somehow missing from self.rosters or lacks the field.
            qb_roster = self.rosters[team].get(qb, {})
            self.team_sack_features[team] = {
                'def_pressure_rate': t.get('def_pressure_rate', 0.15),
                'def_sack_rate': t.get('def_sack_rate', 0.06),
                'sack_rate_allowed': t.get('sack_rate_allowed', 0.06),
                'qb_cpoe': qb_roster.get('cpoe', self.dna['qb'].get(qb, {}).get('cpoe', 0.0)),
                # Phase 7e fix: genuine last-4-games rate, persisted raw
                # (unblended) by refresh_weekly_dna_v_0_1_0.py's
                # refresh_team_defense(). Falls back to the season-level
                # blended value when no real L4 window exists yet (e.g.
                # preseason) -- same fallback shape the rest of this dict
                # already uses, and matches what Gate 2 saw before this fix
                # (a duplicate of the season value) for the weeks where no
                # real L4 signal exists yet.
                'def_sack_rate_l4': t.get('def_sack_rate_l4', t.get('def_sack_rate', 0.06)),
                'sack_rate_allowed_l4': t.get('sack_rate_allowed_l4', t.get('sack_rate_allowed', 0.06)),
            }

        # Receiver DNA traits used by pass resolution (air-yards/YAC) -- none
        # of this changes mid-game, so precompute once per roster instead of
        # re-parsing per-player data + defensive get_safe_float coercion for
        # every unique target on every play. Covers every player who can ever
        # be targeted (receivers_cache's full roster list, not just players
        # targeted so far) so play-time lookups are a plain dict `.get()`.
        #
        # Phase 7 fix (2026-07-22): prefer self.rosters (live, weekly-blended)
        # over self.dna['skill'] (static career average) -- same reasoning as
        # the precomputed_qb_cpoe/skill_target_share/carry_share fix above.
        # current_rosters' field is named `adot`, not `avg_target_depth_yds`
        # (that's the DNA-file/nested-splits name) -- confirmed via
        # docs/sims/inputs/README.md, read explicitly by name here.
        self.default_receiver_traits = {
            'catch_rate': 0.65, 'avg_target_depth_yds': 8.0, 'deep_target_rate': 0.12,
            'avg_separation_yds': 2.9, 'primary_catch_rate': 0.65, 'redzone_catch_rate': 0.65,
            'goalline_catch_rate': 0.65, 'contested_catch_rate': 0.45, 'elusiveness': 0.0,
            'broken_tackle_rate': 0.10, 'pos': 'WR',
        }
        self.receiver_traits = {}
        for team in [self.away_team, self.home_team]:
            team_traits = {}
            for r_name in self.receivers_cache[team]['primary'][0]:  # recs list is identical across zones, only weights differ
                roster_data = self.rosters[team].get(r_name, {})
                r_dna = self.dna['skill'].get(r_name, {})
                catch_rate = get_safe_float(roster_data, 'catch_rate', get_safe_float(r_dna, 'catch_rate', 0.65))
                splits = roster_data.get('splits', r_dna.get('splits', {}))
                team_traits[r_name] = {
                    'catch_rate': catch_rate,
                    'avg_target_depth_yds': get_safe_float(roster_data, 'adot', get_safe_float(r_dna, 'avg_target_depth_yds', 8.0)),
                    'deep_target_rate': get_safe_float(roster_data, 'deep_target_rate', get_safe_float(r_dna, 'deep_target_rate', 0.12)),
                    'avg_separation_yds': get_safe_float(roster_data, 'avg_separation_yds', get_safe_float(r_dna, 'avg_separation_yds', 2.9)),
                    'primary_catch_rate': get_safe_float(splits.get('primary', {}), 'catch_rate', catch_rate),
                    'redzone_catch_rate': get_safe_float(splits.get('redzone', {}), 'catch_rate', catch_rate),
                    'goalline_catch_rate': get_safe_float(splits.get('goalline', {}), 'catch_rate', catch_rate),
                    'contested_catch_rate': get_safe_float(roster_data, 'contested_catch_rate', get_safe_float(r_dna, 'contested_catch_rate', 0.45)),
                    'elusiveness': get_safe_float(roster_data, 'elusiveness', get_safe_float(r_dna, 'elusiveness', 0.0)),
                    'broken_tackle_rate': get_safe_float(roster_data, 'broken_tackle_rate', get_safe_float(r_dna, 'broken_tackle_rate', 0.10)),
                    'pos': roster_data.get('pos', r_dna.get('position', 'WR')),
                }
            self.receiver_traits[team] = team_traits

    def _load_json(self, path):
        if os.path.exists(path):
            with open(path, 'r') as f: return json.load(f)
        return {}

    def _load_skill_dna(self):
        # Fallback to the old skill_dna.json (pre-rb/wr/te split) was removed
        # here -- confirmed unreachable: rb_dna.json/wr_dna.json/te_dna.json
        # are always populated (270/260/123 entries as of this check), so the
        # merged dict can never come back empty in practice.
        merged = {}
        merged.update(self._load_json('data/dna/rb_dna.json'))
        merged.update(self._load_json('data/dna/wr_dna.json'))
        merged.update(self._load_json('data/dna/te_dna.json'))
        return merged

    def _get_starter_static(self, team, pos):
        team_roster = self.rosters[team]
        players = [p for p, t in team_roster.items() if t.get('pos') == pos and t.get('status', 'active') == 'active']
        if not players: return "Unknown"
        # Hand-verified override for a real current starter the default
        # heuristic below gets wrong (see set_qb_starter_overrides_v_0_1_0.py --
        # career total_attempts silently favors a veteran backup over a
        # shorter-track-record real starter). Checked before falling back.
        if pos == 'QB':
            overridden = [p for p in players if team_roster[p].get('starter_override')]
            if overridden:
                return overridden[0]
        dna_key = 'qb' if pos == 'QB' else 'skill'
        return max(players, key=lambda p: self.dna[dna_key].get(p, {}).get('total_attempts' if pos == 'QB' else 'total_targets', 0))

    def run_game(self):
        """Simulates all N games concurrently until all games are over."""
        while not np.all(self.game_over):
            self.simulate_play_step()
        
        # Post-game defensive stat assignments: points allowed
        self.player_stats[self.away_team]['Defense']['pts_allowed'] = self.score_home.copy()
        self.player_stats[self.home_team]['Defense']['pts_allowed'] = self.score_away.copy()

    def simulate_play_step(self):
        """Executes a single play step concurrently for all active games."""
        active = ~self.game_over
        if not np.any(active):
            return

        # Diagnostic-only snapshot (additive, does not affect game logic): line of
        # scrimmage before this step's play resolves, used by the positional-evaluator
        # hook below to classify completed-pass yardage by zone after the fact.
        pre_yardline_100 = self.yardline_100.copy()

        # Snapshot used by the Q2->Q3 kickoff logic inside `_run_clock` (a separate
        # method, so this needs to be an instance attribute, not a local) to tell
        # whether a possession change already happened earlier THIS SAME step
        # (score, turnover-on-downs, interception, lost fumble) before deciding
        # whether to force "home team receives" — see Round 13 bug fix below.
        self.step_start_possession_is_away = self.possession_is_away.copy()

        # Snapshot of every lane still in play at the start of this step
        # (before kneel/spike/timeout/kickoff/penalty/punt/FG progressively
        # narrow `active` for the rest of the step). _resolve_quarter_transitions
        # needs this: it runs once, at the very end of the step, and has to
        # catch a lane whose clock crossed zero via ANY of the step's
        # _run_clock call sites (not just the Phase 7 combined one) -- using
        # the narrowed, end-of-step `active` there would miss lanes that hit
        # zero via kneel/spike/kickoff/punt/FG.
        self.step_start_active = active.copy()

        # -------------------------------------------------------------
        # STRATEGIC PHASE: Kneel, Spike, Timeout, and Clock Bleed Logic
        # -------------------------------------------------------------
        poss_scores = np.where(self.possession_is_away, self.score_away, self.score_home)
        def_scores = np.where(self.possession_is_away, self.score_home, self.score_away)
        
        is_hurry = active & (self.quarter == 4) & (self.time_remaining < 120) & (poss_scores < def_scores)
        is_victory = active & (poss_scores > def_scores) & (self.quarter == 4) & (self.time_remaining < 120)
        
        # 1. Victory Formation (Kneel Down)
        timeouts_def = np.where(self.possession_is_away, self.timeouts_home, self.timeouts_away)
        max_bleed_time = (4 - self.down) * 40 - (timeouts_def * 40)
        # BUG (found in the game_engine cleanup audit): max_bleed_time is
        # always <=0 at down==4 -- (4-4)*40=0 and timeouts_def*40>=0 -- so
        # `time_remaining < max_bleed_time` is unsatisfiable there (time
        # can't be negative). A leading team that legitimately reached 4th
        # down with real time on the clock fell through to the punt/FG/GO
        # model instead of kneeling, which isn't realistic -- real coaches
        # concede the instant the clock genuinely can't be stopped, before
        # even snapping the ball. concede_mask below handles that directly,
        # independent of down: if the trailing team has zero timeouts and
        # <=42 seconds remain, a single kneel (up to 40s off the clock)
        # leaves nothing meaningful behind regardless of what down it is,
        # so the game is just over -- this is Cam's stated rule, not a
        # fix to the max_bleed_time formula itself (left as-is; it still
        # correctly handles the multi-down bleed-out case where the
        # defense still has a timeout or two in the bank).
        concede_mask = is_victory & (self.time_remaining <= 42) & (timeouts_def == 0)
        kneel_mask = (is_victory & (self.time_remaining < max_bleed_time)) | concede_mask
        if np.any(kneel_mask):
            cost = np.minimum(40, self.time_remaining[kneel_mask])
            self._run_clock(cost, kneel_mask)
            self.down[kneel_mask] += 1
            game_over_mask = kneel_mask & ((self.down > 4) | (self.time_remaining <= 0))
            self.game_over[game_over_mask] = True
            # For games that kneeled, we don't run any more logic this step
            active = active & ~kneel_mask

        if not np.any(active): return

        # 2. Spike Logic
        timeouts_pos = np.where(self.possession_is_away, self.timeouts_away, self.timeouts_home)
        spike_mask = active & is_hurry & ~self.clock_stopped & (timeouts_pos == 0) & (self.down < 4)
        if np.any(spike_mask):
            # Run clock 15s
            cost = 15
            out_of_time = spike_mask & (self.time_remaining < cost)
            self.time_remaining[out_of_time] = 0
            self.game_over[out_of_time] = True
            
            normal_spike = spike_mask & ~out_of_time
            self._run_clock(cost, normal_spike)
            self.down[normal_spike] += 1
            self.clock_stopped[normal_spike] = True
            active = active & ~spike_mask

        if not np.any(active): return

        # 3. Timeout & Pre-Play Runoff Logic
        timeout_off_mask = active & is_hurry & ~self.clock_stopped & (timeouts_pos > 0)
        if np.any(timeout_off_mask):
            # Decrement possession team timeouts
            self.timeouts_away[timeout_off_mask & self.possession_is_away] -= 1
            self.timeouts_home[timeout_off_mask & ~self.possession_is_away] -= 1
            self.clock_stopped[timeout_off_mask] = True
            active = active & ~timeout_off_mask

        if not np.any(active): return

        # B3 (clock_physics_v020): widened beyond the old <2:00-only gate.
        # "Early" strategy lanes start burning timeouts at 4:00 remaining;
        # "Late" strategy lanes keep the original <2:00 gate.
        late_window = self.time_remaining < 120
        early_window = self.trailing_defense_early_strategy & (self.time_remaining < 240)
        timeout_def_mask = active & (self.quarter == 4) & (late_window | early_window) & (poss_scores > def_scores) & ~self.clock_stopped & (timeouts_def > 0)
        if np.any(timeout_def_mask):
            # Decrement defending team timeouts
            self.timeouts_home[timeout_def_mask & self.possession_is_away] -= 1
            self.timeouts_away[timeout_def_mask & ~self.possession_is_away] -= 1
            self.clock_stopped[timeout_def_mask] = True
            active = active & ~timeout_def_mask

        if not np.any(active): return

        # -------------------------------------------------------------
        # CORE 7-PHASE PLAY SIMULATION
        # -------------------------------------------------------------
        # Kickoffs Handling
        ko_mask = active & self.needs_kickoff
        if np.any(ko_mask):
            self.needs_kickoff[ko_mask] = False
            self.kickoffs_run[ko_mask] += 1
            self.down[ko_mask] = 1
            self.distance[ko_mask] = 10
            self.clock_stopped[ko_mask] = True

            n_ko = np.sum(ko_mask)
            # Touchback rate confirmed via real 2025 PBP (see
            # scripts/eda/analyze_kicking_timing.py, docs/eda_outputs/kicking_timing/README.md):
            # 20.68% (576/2785), a real, dramatic drop from 2021-2024's 57-73%
            # range following the new 2025 kickoff rule -- this constant was
            # already correctly calibrated to 2025, it just had no citation
            # explaining why it's so much lower than the pre-2025 norm.
            is_tb = np.random.rand(n_ko) < 0.2068

            # Live-play runoff, verified against the same EDA: touchbacks are
            # essentially instant (real 2021-2025 mean 0.009s, median/P25/P75
            # all 0s -- the whistle blows dead in the end zone, no live clock
            # runs) -- previously modeled with the same 4-7s range as returns,
            # overstating touchback time every time. Returns keep the existing
            # range (real median ~5s, matches).
            ko_runoff = np.zeros(n_ko, dtype=np.int32)
            ko_runoff[~is_tb] = np.random.randint(4, 7, size=np.sum(~is_tb))

            # Initialize default yardline
            yardlines = np.zeros(n_ko, dtype=np.int32)

            # Touchbacks go to 30-yard line (70 yards to go)
            yardlines[is_tb] = 70

            # Returns: Shifted Log-Normal. Params undocumented -- no citation
            # of source/fit methodology found (2026-07-21 audit). Flagged for
            # a future real-data recheck, same as punt/INT/fumble return
            # yardage below -- deprioritized since these are all comparatively
            # rare plays; Cam's call is this matters more for its downstream
            # clock-timing effect (long returns run more clock) than for the
            # exact yardage shape, so a recheck should measure both together.
            n_ret = np.sum(~is_tb)
            if n_ret > 0:
                ret_vals = np.random.lognormal(mean=0.2134, sigma=2.9122, size=n_ret) - 1.0
                ret_yds = np.round(ret_vals).astype(np.int32)
                ko_runoff[~is_tb] = np.clip(np.round(4 + np.maximum(0, ret_yds) / 5.0), 4, 14).astype(np.int32)

                # Kickoff starts at 100 yards to go
                final_yds = 100 - ret_yds
                
                # Check for touchdowns (final_yds <= 0 means return yards >= 100)
                is_td = final_yds <= 0
                
                # Assign returns
                ret_indices = np.where(~is_tb)[0]
                
                # Update score and state for TDs
                td_indices_in_ko = ret_indices[is_td]
                if len(td_indices_in_ko) > 0:
                    td_mask = np.zeros(self.N, dtype=bool)
                    global_td_indices = np.where(ko_mask)[0][td_indices_in_ko]
                    td_mask[global_td_indices] = True
                    
                    # Award 7 points to return team
                    self.score_away[td_mask & self.possession_is_away] += 7
                    self.score_home[td_mask & ~self.possession_is_away] += 7
                    self.needs_kickoff[td_mask] = True
                    
                    self._switch_possession(scored=True, mask=td_mask)
                
                # Assign non-TD returns
                non_td_in_ret = ~is_td
                non_td_ret_indices = ret_indices[non_td_in_ret]
                yardlines[non_td_ret_indices] = np.maximum(1, np.minimum(99, final_yds[non_td_in_ret]))
            
            self.yardline_100[ko_mask] = yardlines
            self._run_clock(ko_runoff, ko_mask)
            active = active & ~ko_mask

        if not np.any(active): return

        # Phase 1: Pre-Snap Penalty
        # Check pre-snap penalties using Chaos Gate 1 weights
        game_sec = (4 - self.quarter) * 900 + self.time_remaining
        score_diff = poss_scores - def_scores
        is_home_pos = np.where(self.possession_is_away, 0.0, 1.0)
        
        # Chaos Model features: down, ydstogo, yardline_100, score_differential, game_seconds_remaining, is_home
        # Vectorized Sigmoid math for Gate 1
        x_presnap = np.stack([
            self.down[active],
            self.distance[active],
            self.yardline_100[active],
            score_diff[active],
            game_sec[active],
            is_home_pos[active]
        ], axis=1).astype(np.float32)
        
        dot = np.dot(x_presnap, self.registry.chaos_model._g1_coef) + self.registry.chaos_model._g1_intercept
        penalty_prob = 1.0 / (1.0 + np.exp(-dot))
        
        has_penalty = np.zeros(self.N, dtype=bool)
        has_penalty[active] = np.random.rand(np.sum(active)) < penalty_prob
        if np.any(has_penalty):
            # 50% offense, 50% defense -- only has_penalty lanes need a draw
            is_offensive = np.zeros(self.N, dtype=bool)
            is_offensive[has_penalty] = np.random.rand(np.sum(has_penalty)) < 0.5
            off_penalty_mask = has_penalty & is_offensive
            def_penalty_mask = has_penalty & ~is_offensive
            
            # Offense False Start: move back 5 yards
            self.yardline_100[off_penalty_mask] = np.minimum(99, self.yardline_100[off_penalty_mask] + 5)
            self.distance[off_penalty_mask] += 5
            
            # Defense Offsides: move forward 5 yards
            penalty_yds = np.minimum(5, self.yardline_100[def_penalty_mask] - 1)
            self.yardline_100[def_penalty_mask] -= penalty_yds
            self.distance[def_penalty_mask] = np.maximum(1, self.distance[def_penalty_mask] - penalty_yds)
            
            # Penalties end the play step immediately
            self.penalties_accepted[has_penalty] += 1
            self.presnap_penalty_snaps[has_penalty] += 1
            active = active & ~has_penalty

        if not np.any(active): return

        # Phase 2: 4th Down Evaluation
        fourth_down_mask = active & (self.down == 4)
        fourth_down_decisions = np.empty(self.N, dtype=object)
        
        if np.any(fourth_down_mask):
            pos_agg = np.where(self.possession_is_away, self.coach_aggression[self.away_team], self.coach_aggression[self.home_team])
            # Predict 4th down probas vectorially
            probas = self._predict_4th_down_probas_batch(
                self.yardline_100[fourth_down_mask],
                self.distance[fourth_down_mask],
                game_sec[fourth_down_mask],
                score_diff[fourth_down_mask]
            )
            probas[:, 2] += pos_agg[fourth_down_mask]
            probas = np.clip(probas, 0.0, 1.0)
            row_sums = probas.sum(axis=1, keepdims=True)
            zero_sums = (row_sums[:, 0] == 0)
            probas[zero_sums] = [1.0, 0.0, 0.0]
            row_sums[zero_sums] = 1.0
            probas /= row_sums
            
            # Sample decision
            cum_p = np.cumsum(probas, axis=1)
            r = np.random.rand(cum_p.shape[0])
            dec_idx = (r[:, None] > cum_p).sum(axis=1)
            fourth_down_decisions[fourth_down_mask] = np.where(dec_idx == 0, 'PUNT', np.where(dec_idx == 1, 'FIELD_GOAL', 'GO'))
            
            # Handle Punts
            punt_mask = fourth_down_mask & (fourth_down_decisions == 'PUNT')
            if np.any(punt_mask):
                n_punts = np.sum(punt_mask)
                self.punts_run[punt_mask] += 1
                # Base runoff covers the punt kick flight + touchback/fair-catch
                # (no meaningful return); scaled up below for actual returns.
                # Range verified/updated against real 2021-2025 PBP (see
                # scripts/eda/analyze_kicking_timing.py): touchback median 8s,
                # fair-catch median 7s -- the previous 4-7s range undershot both.
                punt_runoff = np.random.randint(7, 10, size=n_punts)

                # Roll touchback probability using logistic model based on starting yardline_100
                starting_yds = self.yardline_100[punt_mask]
                logit_val = 2.3127 - 0.0828 * starting_yds
                tb_prob = 1.0 / (1.0 + np.exp(-logit_val))
                is_tb = np.random.rand(n_punts) < tb_prob
                
                # Split punts into touchback vs non-touchback
                # Global indices
                global_punt_indices = np.where(punt_mask)[0]
                
                tb_mask = np.zeros(self.N, dtype=bool)
                tb_mask[global_punt_indices[is_tb]] = True
                
                non_tb_mask = np.zeros(self.N, dtype=bool)
                non_tb_mask[global_punt_indices[~is_tb]] = True
                
                # 1. Touchbacks: Set yardline_100 to 80 (20-yard line) and switch possession
                if np.any(tb_mask):
                    self._switch_possession(scored=False, mask=tb_mask)
                    self.yardline_100[tb_mask] = 80
                    
                # 2. Non-Touchbacks: Simulate punt distance, then possession switch, then return logic
                if np.any(non_tb_mask):
                    n_non_tb = np.sum(non_tb_mask)
                    # Sample punt distance
                    dist = np.random.randint(35, 50, size=n_non_tb)
                    self.yardline_100[non_tb_mask] -= dist
                    self._switch_possession(scored=False, mask=non_tb_mask)
                    
                    # Ensure yardline_100 is within [1, 99] before return
                    self.yardline_100[non_tb_mask] = np.maximum(1, np.minimum(99, self.yardline_100[non_tb_mask]))
                    
                    # Roll Fair Catch (29.42% of non-touchbacks)
                    is_fc = np.random.rand(n_non_tb) < 0.2942
                    
                    # For non-fair catches, roll return TD (0.91% of returns)
                    is_ret = ~is_fc
                    n_ret = np.sum(is_ret)
                    
                    if n_ret > 0:
                        is_td = np.random.rand(n_ret) < 0.0091
                        
                        ret_indices_in_non_tb = np.where(is_ret)[0]
                        td_indices_in_ret = ret_indices_in_non_tb[is_td]
                        normal_ret_indices_in_ret = ret_indices_in_non_tb[~is_td]
                        
                        # Handle return TDs
                        if len(td_indices_in_ret) > 0:
                            td_mask = np.zeros(self.N, dtype=bool)
                            td_mask[global_punt_indices[~is_tb][td_indices_in_ret]] = True

                            self.score_away[td_mask & self.possession_is_away] += 7
                            self.score_home[td_mask & ~self.possession_is_away] += 7
                            self.needs_kickoff[td_mask] = True
                            self._switch_possession(scored=True, mask=td_mask)
                            # A return TD is by definition a long, unstopped return.
                            punt_runoff[td_mask[punt_mask]] = np.random.randint(12, 15, size=len(td_indices_in_ret))

                        # Handle normal returns (Shifted Exponential: Y = X + loc where X ~ Exponential(scale))
                        # Params undocumented -- see the kickoff-return "Shifted
                        # Log-Normal" note earlier in this method (Kickoffs Handling
                        # section) for the flagged-for-future-recheck status shared
                        # by all return-yardage distributions in this file.
                        if len(normal_ret_indices_in_ret) > 0:
                            normal_ret_mask = np.zeros(self.N, dtype=bool)
                            global_normal_ret = global_punt_indices[~is_tb][normal_ret_indices_in_ret]
                            normal_ret_mask[global_normal_ret] = True

                            ret_vals = np.random.exponential(scale=21.8765, size=len(normal_ret_indices_in_ret)) - 13.0
                            ret_yds = np.round(ret_vals).astype(np.int32)

                            # Capped at remaining distance (yardline_100 - 1) to avoid unsanctioned TDs
                            max_ret = self.yardline_100[normal_ret_mask] - 1
                            ret_yds = np.minimum(ret_yds, max_ret)

                            # BUG FIX (#6): missing the same lower-bound clip
                            # the kickoff-return branch already has (line 555,
                            # np.maximum(1, np.minimum(99, ...))). ret_yds can
                            # be as negative as -13 (a big return loss); a
                            # short/deep punt combined with that could push
                            # yardline_100 past 99 -- an invalid field
                            # position (real football: a safety).
                            self.yardline_100[normal_ret_mask] = np.maximum(1, np.minimum(99, self.yardline_100[normal_ret_mask] - ret_yds))
                            punt_runoff[normal_ret_mask[punt_mask]] = np.clip(np.round(4 + np.maximum(0, ret_yds) / 5.0), 4, 14).astype(np.int32)

                self._run_clock(punt_runoff, punt_mask)
                # Punts are a change of possession — clock stops for the exchange
                # (was missing; left `clock_stopped` stale for the receiving
                # team's first play, which could wrongly trigger a spike/timeout
                # burn on the next call if the clock was running beforehand).
                self.clock_stopped[punt_mask] = True
                active = active & ~punt_mask
                
            # Handle Field Goals
            fg_mask = fourth_down_mask & (fourth_down_decisions == 'FIELD_GOAL')
            if np.any(fg_mask):
                self.fg_attempts_away[fg_mask & self.possession_is_away] += 1
                self.fg_attempts_home[fg_mask & ~self.possession_is_away] += 1
                
                fg_success_prob = self.registry.predict_fg_success(self.yardline_100[fg_mask])
                is_good = np.random.rand(np.sum(fg_mask)) < fg_success_prob
                
                fg_good_mask = fg_mask.copy()
                fg_good_mask[fg_mask] = is_good
                
                fg_miss_mask = fg_mask.copy()
                fg_miss_mask[fg_mask] = ~is_good
                
                # Good Field Goal
                self.score_away[fg_good_mask & self.possession_is_away] += 3
                self.score_home[fg_good_mask & ~self.possession_is_away] += 3
                self.needs_kickoff[fg_good_mask] = True
                
                # Flat 4s for both outcomes -- real 2021-2025 PBP (see
                # scripts/eda/analyze_kicking_timing.py) shows made and missed
                # FGs take about the same live-play time (~4s median either
                # way), not the previous made<missed asymmetry (4-6s vs 5-9s).
                FG_RUNOFF_SEC = 4
                self._run_clock(FG_RUNOFF_SEC, fg_good_mask)
                self._run_clock(FG_RUNOFF_SEC, fg_miss_mask)
                self._switch_possession(scored=True, mask=fg_good_mask)
                self._switch_possession(scored=False, mask=fg_miss_mask)

                # Made or missed, a field goal attempt stops the clock for the
                # exchange (was missing — same stale-state risk as punts above).
                self.clock_stopped[fg_mask] = True
                active = active & ~fg_mask
                
        if not np.any(active): return

        # Phase 3: Play Selection (Vectorized grouping)
        # Compute play selection probabilities vectorially
        base_pass_prob = np.zeros(self.N, dtype=np.float32)
        
        # Build down/distance bucket groups for XGBoost predictions
        # bucket name format: '1_10', '1_long', '1_short', '2_short', '2_med', etc.
        bucket_names = np.empty(self.N, dtype=object)
        
        # Down 1
        m1 = active & (self.down == 1)
        if np.any(m1):
            bucket_names[m1] = np.where(self.distance[m1] == 10, '1_10',
                                        np.where(self.distance[m1] > 10, '1_long', '1_short'))
        # Downs 2 & 3
        m23 = active & ((self.down == 2) | (self.down == 3))
        if np.any(m23):
            suffix = np.where(self.distance[m23] <= 3, 'short', np.where(self.distance[m23] <= 7, 'med', 'long'))
            bucket_names[m23] = np.char.add(np.char.add(self.down[m23].astype(str), '_'), suffix)
            
        # Down 4 (only the active ones going for it)
        m4 = active & (self.down == 4)
        if np.any(m4):
            suffix = np.where(self.distance[m4] <= 2, 'short', 'med_long')
            bucket_names[m4] = np.char.add('4_', suffix)

        unique_buckets = np.unique(bucket_names[active])
        for b in unique_buckets:
            idx = active & (bucket_names == b)
            if not np.any(idx): continue
            
            yd = self.yardline_100[idx]
            zones = _classify_zone(yd)
            
            for zone in ['goalline', 'redzone', 'primary']:
                zone_mask = (zones == zone)
                if not np.any(zone_mask): continue
                
                sub_idx = np.where(idx)[0][zone_mask]

                is_away_zone = self.possession_is_away[sub_idx]

                cpoe_away = self.zone_cpoe[self.away_team][zone]
                cpoe_home = self.zone_cpoe[self.home_team][zone]
                target_share_away = self.zone_target_share[self.away_team][zone]
                target_share_home = self.zone_target_share[self.home_team][zone]
                carry_share_away = self.zone_carry_share[self.away_team][zone]
                carry_share_home = self.zone_carry_share[self.home_team][zone]

                cpoe_by_filter = np.where(is_away_zone, cpoe_away, cpoe_home)
                target_share_by_filter = np.where(is_away_zone, target_share_away, target_share_home)
                carry_share_by_filter = np.where(is_away_zone, carry_share_away, carry_share_home)

                leverage_sub = score_diff[sub_idx] * game_sec[sub_idx]
                timeouts_pos_sub = np.where(self.possession_is_away[sub_idx], self.timeouts_away[sub_idx], self.timeouts_home[sub_idx])
                timeouts_def_sub = np.where(self.possession_is_away[sub_idx], self.timeouts_home[sub_idx], self.timeouts_away[sub_idx])

                X_b_zone = np.stack([
                    self.yardline_100[sub_idx].astype(np.float32),
                    game_sec[sub_idx].astype(np.float32),
                    score_diff[sub_idx].astype(np.float32),
                    timeouts_pos_sub.astype(np.float32),
                    timeouts_def_sub.astype(np.float32),
                    leverage_sub.astype(np.float32),
                    cpoe_by_filter,
                    target_share_by_filter,
                    carry_share_by_filter
                ], axis=1)
                
                booster_name = f"{zone}_{b}"
                booster = self.registry.play_selection_buckets.get(booster_name)
                if not booster:
                    booster = self.registry.play_selection_buckets.get(f"primary_{b}")
                    
                if booster:
                    base_pass_prob[sub_idx] = booster.inplace_predict(X_b_zone)
                else:
                    base_pass_prob[sub_idx] = 0.58

        # Apply PROE Overlay in logit space vectorially, faded by game script
        # (see _proe_script_weight -- PROE is a neutral-situation tendency).
        proe_offset = np.where(self.possession_is_away,
                               self.proe_offsets[self.away_team],
                               self.proe_offsets[self.home_team])
        proe_offset = proe_offset * _proe_script_weight(score_diff, game_sec)
        adjusted_pass_prob = _sigmoid(_logit(base_pass_prob) + proe_offset)

        # Q4 trailing overlay: blend toward the real empirical pass rate for
        # down-2+-scores-in-the-4th (the one spot the base model under-reacts).
        if Q4_TRAIL_BLEND > 0:
            _q4t = _q4_trailing_pass_target(score_diff, game_sec)
            _m = ~np.isnan(_q4t)
            if np.any(_m):
                adjusted_pass_prob[_m] = (
                    (1.0 - Q4_TRAIL_BLEND) * adjusted_pass_prob[_m]
                    + Q4_TRAIL_BLEND * _q4t[_m]
                )

        adjusted_pass_prob = np.clip(adjusted_pass_prob, 0.01, 0.99)
        
        is_pass = active & (np.random.rand(self.N) < adjusted_pass_prob)
        is_run = active & ~is_pass

        # Play-call-by-script instrumentation (off by default). Tallies
        # [plays, passes, sum(base_pass_prob), sum(adjusted_pass_prob)] per
        # (score band x time band) cell for comparison against
        # scripts/eda/analyze_pass_rate_by_script.py.
        if getattr(self, "track_playcall", False):
            _sd = score_diff[active]
            _gs = game_sec[active]
            _sb = np.select(
                [_sd >= 17, _sd >= 9, _sd >= 1, _sd == 0, _sd >= -8, _sd >= -16],
                [0, 1, 2, 3, 4, 5], default=6)
            _tb = np.select([_gs > 1800, _gs > 900, _gs > 240], [0, 1, 2], default=3)
            _cell = _sb * 4 + _tb
            _p = is_pass[active]
            np.add.at(self._playcall_tally[0], _cell, 1)
            np.add.at(self._playcall_tally[1], _cell, _p)
            np.add.at(self._playcall_tally[2], _cell, base_pass_prob[active])
            np.add.at(self._playcall_tally[3], _cell, adjusted_pass_prob[active])

        # -------------------------------------------------------------
        # Phase 4: Play Execution (PASS & RUN)
        # -------------------------------------------------------------
        play_gain = np.zeros(self.N, dtype=np.int32)
        play_is_complete = np.zeros(self.N, dtype=bool)
        play_target = np.empty(self.N, dtype=object)
        play_rusher = np.empty(self.N, dtype=object)
        play_is_interception = np.zeros(self.N, dtype=bool)
        play_is_sack = np.zeros(self.N, dtype=bool)
        play_is_scramble = np.zeros(self.N, dtype=bool)
        play_is_throwaway = np.zeros(self.N, dtype=bool)
        play_is_fumble = np.zeros(self.N, dtype=bool)
        play_is_fumble_lost = np.zeros(self.N, dtype=bool)
        play_air_yards = np.zeros(self.N, dtype=np.int32)
        play_yac = np.zeros(self.N, dtype=np.int32)
        play_ttt = np.zeros(self.N, dtype=np.float32)
        
        # Mid-Play Penalties
        play_is_off_holding = np.zeros(self.N, dtype=bool)
        play_is_dpi = np.zeros(self.N, dtype=bool)
        play_is_def_holding = np.zeros(self.N, dtype=bool)
        has_accepted_penalty = np.zeros(self.N, dtype=bool)

        # 4A. Execute PASS Plays
        if np.any(is_pass):
            # Target selection vectorially, zone-aware: a play's target pool
            # weights come from that play's own zone (self.yardline_100 at
            # the point of the snap), not one flat game-long distribution --
            # see receivers_cache's construction for why this matters (a real
            # redzone/goalline target-share specialist actually gets fed more
            # near the goal line now, instead of reading the same odds as
            # every other zone).
            pass_zones = _classify_zone(self.yardline_100)
            for team in [self.away_team, self.home_team]:
                team_mask = is_pass & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue

                for zone in ('primary', 'redzone', 'goalline'):
                    zone_mask = team_mask & (pass_zones == zone)
                    if not np.any(zone_mask): continue
                    recs, cum_shares = self.receivers_cache[team][zone]
                    r_vals = np.random.rand(np.sum(zone_mask))
                    rec_idx = np.searchsorted(cum_shares, r_vals)
                    rec_idx = np.minimum(rec_idx, len(cum_shares) - 1)
                    play_target[zone_mask] = recs[rec_idx]
                
            # QB TTT Distribution. Design is intentional (confirmed with Cam
            # 2026-07-21): center a Normal on the QB's own real
            # avg_time_to_throw_sec, with a made-up-but-roughly-right spread
            # (std 0.6, clipped to [1.5, 4.5]) -- not verified against a real
            # per-QB TTT variance distribution, and that's an accepted
            # simplification for now, not an oversight. Future idea (deferred,
            # sounds involved): tie this to the pass-block/pass-rush trench
            # matchup so better protection means more time to throw, not just
            # a QB-intrinsic draw -- not started.
            for team in [self.away_team, self.home_team]:
                team_mask = is_pass & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue

                qb = self.qb_starters[team]
                # Phase 7 fix: prefer the live per-week roster value, fall
                # back to the static career-average DNA file.
                qb_dna = self.rosters[team].get(qb, {})
                avg_ttt = qb_dna.get('avg_time_to_throw_sec', self.dna['qb'].get(qb, {}).get('avg_time_to_throw_sec', 2.7))
                sim_ttt = np.clip(np.random.normal(avg_ttt, 0.6, size=np.sum(team_mask)), 1.5, 4.5)
                play_ttt[team_mask] = sim_ttt

            # Trench Sacks Gate (Gate 2)
            # Build features for Gate 2
            # Feature order in metadata: avg_time_to_throw_sec_qb, cpoe_qb, def_pressure_rate, def_sack_rate, sack_rate_allowed, off_sack_rate_l4, def_sack_rate_l4, down, ydstogo, yardline_100, score_differential
            
            # Team-level trench/CPOE features -- fixed for the whole game
            # (starters don't change mid-game), so this is a straight
            # per-team lookup into the precompute built once in __init__
            # (self._precompute_matchup_tables) rather than a re-derivation
            # per play. Defense stats come from the opponent's entry, offense
            # stats (sack_rate_allowed, qb_cpoe) from the possessing team's own.
            away_feat = self.team_sack_features[self.away_team]
            home_feat = self.team_sack_features[self.home_team]
            is_away_pass = self.possession_is_away[is_pass]

            t_def_pressure_rate = np.where(is_away_pass, home_feat['def_pressure_rate'], away_feat['def_pressure_rate'])
            t_def_sack_rate = np.where(is_away_pass, home_feat['def_sack_rate'], away_feat['def_sack_rate'])
            t_sack_rate_allowed = np.where(is_away_pass, away_feat['sack_rate_allowed'], home_feat['sack_rate_allowed'])
            # Phase 7e fix: genuine last-4-games rates (see team_sack_features'
            # construction in _precompute_matchup_tables) -- these used to be
            # a straight duplicate of the season-level t_def_sack_rate/
            # t_sack_rate_allowed above, despite the feature names implying
            # rolling L4 data. Same possessing-team-vs-opponent masking as
            # the season-level features.
            t_def_sack_rate_l4 = np.where(is_away_pass, home_feat['def_sack_rate_l4'], away_feat['def_sack_rate_l4'])
            t_sack_rate_allowed_l4 = np.where(is_away_pass, away_feat['sack_rate_allowed_l4'], home_feat['sack_rate_allowed_l4'])
            # Full-N (not is_pass-filtered): reused below by Gate 4 via the
            # `no_sack_pass` mask, so kept at the same shape as
            # `self.possession_is_away` rather than pre-filtered like the
            # trench features above (which are only ever consumed by X_g2).
            qb_cpoe = np.where(self.possession_is_away, away_feat['qb_cpoe'], home_feat['qb_cpoe']).astype(np.float32)

            X_g2 = np.stack([
                play_ttt[is_pass],
                qb_cpoe[is_pass],
                t_def_pressure_rate,
                t_def_sack_rate,
                t_sack_rate_allowed,
                t_sack_rate_allowed_l4, # off_sack_rate_l4
                t_def_sack_rate_l4, # def_sack_rate_l4
                self.down[is_pass].astype(np.float32),
                self.distance[is_pass].astype(np.float32),
                self.yardline_100[is_pass].astype(np.float32),
                score_diff[is_pass].astype(np.float32)
            ], axis=1)
            
            # Round 14/15 calibration (clock_physics_v020): gate 2's raw sack_prob
            # measured −13.1% under real recorded sack rate on its own (before any
            # diversion), via scripts/eda/test_sacks_diversion_hypothesis.py. Same
            # multiplicative calibration pattern already used for gate 4's
            # interception probability (`* 0.80`, a few hundred lines below in the
            # no_sack_pass branch). Backtrack: remove `* SACK_PROB_CALIBRATION_MULT`.
            SACK_PROB_CALIBRATION_MULT = 1.134
            sack_prob = self.registry.chaos_model._g2_booster.inplace_predict(X_g2) * SACK_PROB_CALIBRATION_MULT
            # Missing clip, unlike adjusted_pass_prob a few hundred lines up
            # (line 843). Investigated (2026-07-20): checked a real 3000-game
            # batch (raw booster output never exceeded 0.355) and the single
            # most extreme real-league matchup possible (worst pass block vs.
            # best pass rush, slowest/worst QB, 3rd-and-15, own 5, down 14 --
            # raw output ~0.10, calibrated ~0.13) -- current real team/QB data
            # never actually gets remotely close to the 0.8065 raw-output
            # threshold where *1.24 would cross 1.0. Not a live bug today,
            # just an unguarded edge case; clipped for the same reason
            # adjusted_pass_prob already is -- cheap insurance against future
            # data/model changes, and consistent with the rest of the file.
            sack_prob = np.clip(sack_prob, 0.0, 1.0)

            # Gate 2b: correct Gate 2's own probability using the real
            # pass-block/pass-rush composites it wasn't trained on. Verified
            # (2026-07-20): near-zero aggregate brier-score change vs. Gate 2
            # alone, but a real, consistent bucket-level correction of Gate
            # 2's own over-prediction bias, worst at bad matchups (Q1: Gate 2
            # alone predicts 8.28% vs. real 7.78%) and smaller at good ones
            # (Q5: 5.97% vs. real 5.37%) -- aggregate brier hides this because
            # most plays are middling matchups where Gate 2 was already fine.
            pass_block_off_z_vec = np.where(self.possession_is_away[is_pass], self.pass_block_off_z_away, self.pass_block_off_z_home)
            pass_def_z_vec = np.where(self.possession_is_away[is_pass], self.pass_def_z_home, self.pass_def_z_away)
            X_g2b = np.stack([sack_prob, pass_block_off_z_vec, pass_def_z_vec], axis=1).astype(np.float32)
            sack_prob = np.clip(self.registry.chaos_model._g2b_booster.inplace_predict(
                X_g2b, iteration_range=self.registry.chaos_model.g2b_iteration_range), 0.0, 1.0)

            has_sack = np.zeros(self.N, dtype=bool)
            has_sack[is_pass] = np.random.rand(np.sum(is_pass)) < sack_prob

            # QB Scramble: an independent roll on EVERY dropback, not just
            # would-be sacks. Previously `escapes` only fired conditional on
            # `has_sack` (a "scramble escape hatch"), which made scrambles
            # purely a sack-escape valve -- for a real per-dropback
            # scramble_rate around 5-10%, that gate produced well under 1% of
            # all dropbacks as scrambles (rate * sack_prob), nowhere near real
            # QB rushing volume (found 2026-08-12; Cam's spec: even the
            # league's highest scramble-rate QB should scramble on ~10% of
            # dropbacks -- roughly 4 times per ~40 dropbacks in a game -- not
            # a sack-conditional trickle). A scramble roll on a would-be-sack
            # lane still converts it to a scramble instead of a sack (same
            # "helps escape sacks" property as before), it's just no longer
            # gated behind that condition -- it now also fires on clean-pocket
            # dropbacks that were never going to be sacked at all.
            scramble_rate = np.zeros(self.N, dtype=np.float32)
            for team in [self.away_team, self.home_team]:
                team_mask = is_pass & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue
                qb = self.qb_starters[team]
                scramble_rate[team_mask] = self.rosters[team].get(qb, {}).get('scramble_rate', 0.05)

            escapes = is_pass & (np.random.rand(self.N) < scramble_rate)
            real_sacks = has_sack & ~escapes

            if np.any(has_sack) or np.any(escapes):
                # Round 14/15 (clock_physics_v020): cut from 0.20 to 0.06. Gate 2's
                # sack_prob was measured to already run −13.1% under real recorded
                # sack rate BEFORE any diversion (test_sacks_diversion_hypothesis.py)
                # — since gate 2 was almost certainly trained on real recorded sacks
                # (which are already net of real QBs' own scrambling/throwing-away
                # tendencies), stacking a second, engine-level diversion on top
                # double-counts that same real-world behavior. `scramble_rate` above
                # is left untouched — it's real per-QB roster data (mobile QBs
                # escape more), not an arbitrary constant, unlike this flat rate.
                # Note: this moves further from the separately-deferred throwaway-
                # rate target (~5% of all dropbacks, AGENTS.md §11.6/Round 6) — that
                # target assumes a not-yet-built clean-pocket path contributing 25%
                # of throwaways; reconcile the two when that work happens.
                # Backtrack: restore `0.20`.
                is_throwaway = real_sacks & (np.random.rand(self.N) < PRESSURE_THROWAWAY_DIVERT)
                actual_sacks = real_sacks & ~is_throwaway
                play_is_throwaway[is_throwaway] = True
                
                # Handle QB Scramble
                # Normal(7.0, 6.1) verified against real 2020-2025 PBP (see
                # scripts/eda/analyze_scramble_yardage.py, docs/eda_outputs/
                # qb_scramble_yardage/README.md) -- previous Normal(5, 4) was
                # undershooting the real mean by ~2 yards. Real distribution
                # is right-skewed (mean 7.0 > median 6.0); a Normal
                # approximation is a deliberate simplification (Cam's call),
                # not a claim the shape is exactly right.
                if np.any(escapes):
                    scramble_yds = np.random.normal(7.0, 6.1, size=np.sum(escapes)).astype(np.int32)
                    scramble_yds = np.clip(scramble_yds, -2, self.yardline_100[escapes])
                    play_gain[escapes] = scramble_yds
                    play_is_scramble[escapes] = True
                    # Record rush stats for QBs
                    for team in [self.away_team, self.home_team]:
                        team_mask = escapes & (self.possession_is_away == (team == self.away_team))
                        if not np.any(team_mask): continue
                        qb = self.qb_starters[team]
                        self.player_stats[team][qb]['rAtt'][team_mask] += 1
                        self.player_stats[team][qb]['rYds'][team_mask] += scramble_yds[team_mask[escapes]]

                # Handle Sacks
                if np.any(actual_sacks):
                    play_is_sack[actual_sacks] = True

                    # Sack-fumble rate: flat across all QBs, not scaled by a
                    # per-QB DNA field. The previous version scaled by
                    # `qb_dna['sack_rate']` (a mislabeled variable named
                    # `qb_fumble_rate` -- that field is the QB's own sack
                    # rate, not a fumble-proneness measure; there is no real
                    # fumble-rate field in the DNA schema). Real fumble
                    # propensity doesn't meaningfully differ across rostered
                    # QBs (see the general-fumble note below), so this is a
                    # flat rate: 963 fumbles / 7687 sacks, 2020-2025 real PBP
                    # (nfl_data_py), used directly with no calibration
                    # dampener (Cam's call -- every other rate in this file
                    # gets a dampener, this one doesn't).
                    SACK_FUMBLE_RATE = 0.1253
                    fumbles = actual_sacks & (np.random.rand(self.N) < SACK_FUMBLE_RATE)
                    fumbles_lost = fumbles & (np.random.rand(self.N) < 0.50)
                    
                    play_is_fumble[fumbles] = True
                    play_is_fumble_lost[fumbles_lost] = True
                    
                    # Gamma distribution sampling for sack loss
                    shape = self.registry.chaos_model.gamma_params['shape']
                    loc = self.registry.chaos_model.gamma_params['loc']
                    scale = self.registry.chaos_model.gamma_params['scale']
                    loss = stats.gamma.rvs(shape, loc, scale, size=np.sum(actual_sacks))
                    loss = -np.round(np.clip(loss, 1.0, 25.0)).astype(np.int32)
                    play_gain[actual_sacks] = loss
                    
                    # Record QB sack stats
                    for team in [self.away_team, self.home_team]:
                        team_mask = actual_sacks & (self.possession_is_away == (team == self.away_team))
                        if not np.any(team_mask): continue
                        qb = self.qb_starters[team]
                        self.player_stats[team][qb]['sacks_taken'][team_mask] += 1
                        self.player_stats[team][qb]['fumbles'][team_mask & fumbles] += 1
                        self.player_stats[team][qb]['fumbles_lost'][team_mask & fumbles_lost] += 1
                        
                        def_team = self.home_team if team == self.away_team else self.away_team
                        self.player_stats[def_team]['Defense']['def_sack'][team_mask] += 1
                        self.player_stats[def_team]['Defense']['def_fumble_rec'][team_mask & fumbles_lost] += 1

            # 4A.2 No-Sack Passing Plays (Air Yards & Interceptions)
            no_sack_pass = is_pass & ~play_is_sack & ~play_is_scramble & ~play_is_throwaway

            # Clean-pocket / "nothing open" throwaways: a flat fraction of
            # otherwise-normal dropbacks. Diverted here, before target assignment
            # and the completion model -- they resolve as plain incomplete passes
            # (the existing inc_mask logic handles down/clock). See
            # CLEAN_POCKET_THROWAWAY_RATE.
            if CLEAN_POCKET_THROWAWAY_RATE > 0 and np.any(no_sack_pass):
                clean_tw = no_sack_pass & (np.random.rand(self.N) < CLEAN_POCKET_THROWAWAY_RATE)
                play_is_throwaway[clean_tw] = True
                no_sack_pass = no_sack_pass & ~clean_tw

            if self.track_cmp_by_depth:
                self._dropback_tally += [
                    int(np.sum(is_pass)), int(np.sum(play_is_sack)),
                    int(np.sum(play_is_scramble)), int(np.sum(play_is_throwaway)),
                ]

            if np.any(no_sack_pass):
                # Build Air Yards features. Deployed model (air_yards_v_0_1_1, V.0.3.0) is an
                # 11-feature zone-split gate: down, ydstogo, yardline_100, score_differential,
                # game_seconds_remaining, cpoe_by_filter, target_share_by_filter,
                # carry_share_by_filter, play_ttt, avg_air_yards_per_att, deep_target_rate — see
                # air_yards_v_0_1_1/metadata.json and the X_ay_zone stack below. (A larger,
                # richer QB/coach/receiver-trait feature set was scaffolded here at one point but
                # never wired up; removed as dead code during the 2026-07 repo audit — see
                # docs/sims/inputs/README.md §5 bug #9 and AGENTS.md.)
                elusiveness = np.zeros(self.N, dtype=np.float32)
                broken_tackle_rate = np.zeros(self.N, dtype=np.float32)
                catch_rate = np.zeros(self.N, dtype=np.float32)
                primary_catch_rate = np.zeros(self.N, dtype=np.float32)
                redzone_catch_rate = np.zeros(self.N, dtype=np.float32)
                goalline_catch_rate = np.zeros(self.N, dtype=np.float32)
                contested_catch_rate_recv = np.zeros(self.N, dtype=np.float32)
                avg_target_depth_yds_recv = np.zeros(self.N, dtype=np.float32)
                deep_target_rate_recv = np.zeros(self.N, dtype=np.float32)
                avg_separation_yds_recv = np.zeros(self.N, dtype=np.float32)
                pos_recv = np.empty(self.N, dtype=object)

                for team in [self.away_team, self.home_team]:
                    team_mask = no_sack_pass & (self.possession_is_away == (team == self.away_team))
                    if not np.any(team_mask): continue

                    team_traits = self.receiver_traits[team]
                    for r_name in np.unique(play_target[team_mask]):
                        r_mask = team_mask & (play_target == r_name)
                        traits = team_traits.get(r_name, self.default_receiver_traits)

                        catch_rate[r_mask] = traits['catch_rate']
                        avg_target_depth_yds_recv[r_mask] = traits['avg_target_depth_yds']
                        deep_target_rate_recv[r_mask] = traits['deep_target_rate']
                        avg_separation_yds_recv[r_mask] = traits['avg_separation_yds']
                        primary_catch_rate[r_mask] = traits['primary_catch_rate']
                        redzone_catch_rate[r_mask] = traits['redzone_catch_rate']
                        goalline_catch_rate[r_mask] = traits['goalline_catch_rate']
                        contested_catch_rate_recv[r_mask] = traits['contested_catch_rate']
                        elusiveness[r_mask] = traits['elusiveness']
                        broken_tackle_rate[r_mask] = traits['broken_tackle_rate']
                        pos_recv[r_mask] = traits['pos']

                ay_q_avg_air = np.zeros(self.N, dtype=np.float32)
                for team in [self.away_team, self.home_team]:
                    team_mask = no_sack_pass & (self.possession_is_away == (team == self.away_team))
                    if not np.any(team_mask): continue
                    qb = self.qb_starters[team]
                    # Phase 7 fix: prefer the live per-week roster value, fall
                    # back to the static career-average DNA file.
                    q_dna = self.rosters[team].get(qb, {})
                    ay_q_avg_air[team_mask] = q_dna.get('avg_air_yards_per_att', self.dna['qb'].get(qb, {}).get('avg_air_yards_per_att', 8.0))

                # Get zone per play in no_sack_pass
                yd_ay = self.yardline_100[no_sack_pass]
                zones_ay = _classify_zone(yd_ay)
                
                is_away_ay = self.possession_is_away[no_sack_pass]
                recv_names_ay = play_target[no_sack_pass]

                cpoe_away_vec = _zone_scalar_vector(self.zone_cpoe[self.away_team], zones_ay)
                cpoe_home_vec = _zone_scalar_vector(self.zone_cpoe[self.home_team], zones_ay)
                cpoe_by_filter_ay = np.where(is_away_ay, cpoe_away_vec, cpoe_home_vec)

                target_share_by_filter_ay = _zone_split_vector(self.precomputed_skill_target_share, recv_names_ay, zones_ay)
                carry_share_by_filter_ay = _zone_split_vector(self.precomputed_skill_carry_share, recv_names_ay, zones_ay)
                
                sampled_ay = np.zeros(np.sum(no_sack_pass), dtype=np.float32)
                for zone in ['goalline', 'redzone', 'primary']:
                    zone_mask = (zones_ay == zone)
                    if not np.any(zone_mask): continue
                    
                    X_ay_zone = np.stack([
                        self.down[no_sack_pass][zone_mask].astype(np.float32),
                        self.distance[no_sack_pass][zone_mask].astype(np.float32),
                        self.yardline_100[no_sack_pass][zone_mask].astype(np.float32),
                        score_diff[no_sack_pass][zone_mask].astype(np.float32),
                        game_sec[no_sack_pass][zone_mask].astype(np.float32),
                        cpoe_by_filter_ay[zone_mask],
                        target_share_by_filter_ay[zone_mask],
                        carry_share_by_filter_ay[zone_mask],
                        play_ttt[no_sack_pass][zone_mask],
                        ay_q_avg_air[no_sack_pass][zone_mask],
                        deep_target_rate_recv[no_sack_pass][zone_mask]
                    ], axis=1)
                    
                    sampled_ay[zone_mask] = self.registry.air_yards_sampler.sample(X_ay_zone, zone=zone)
                
                sampled_ay = np.clip(np.round(sampled_ay).astype(np.int32), -5, self.yardline_100[no_sack_pass])
                play_air_yards[no_sack_pass] = sampled_ay

                # Interception Gate (Gate 4)
                # features: air_yards, cpoe_qb, down, ydstogo, yardline_100, score_differential
                X_g4 = np.stack([
                    sampled_ay.astype(np.float32),
                    qb_cpoe[no_sack_pass],
                    self.down[no_sack_pass].astype(np.float32),
                    self.distance[no_sack_pass].astype(np.float32),
                    self.yardline_100[no_sack_pass].astype(np.float32),
                    score_diff[no_sack_pass].astype(np.float32)
                ], axis=1)
                
                int_prob = self.registry.chaos_model._g4_booster.inplace_predict(
                    X_g4, iteration_range=self.registry.chaos_model.g4_iteration_range) * 0.80

                # INT risk proxy (2026-08-12, Cam's call): Gate 4 has no
                # defensive-side feature at all -- every defense has identical
                # INT odds for a given situation, which is why league-wide INT
                # rates were coming out flat regardless of matchup. Real fix is
                # a Gate 4 retrain with a defensive feature (not done yet, see
                # AGENTS.md); this is an explicitly provisional proxy in the
                # meantime, using two signals already computed for this play:
                # CPOE (more negative -> more risk) and this specific dropback's
                # sampled time-to-throw vs. league baseline (held longer -> more
                # risk). Applied in PROBABILITY SPACE, not logit space (same
                # rule as CPOE's completion-probability adjustment, see AGENTS.md
                # §8 -- adding inside a logit sum before the sigmoid would
                # compress the effect). Deliberately small and symmetric: a
                # high-CPOE, quick-release QB (Burrow, Mahomes) gets a real INT
                # reduction, not just a smaller penalty than everyone else.
                INT_PROXY_CPOE_SENSITIVITY = 0.0008   # prob per CPOE point
                INT_PROXY_TTT_SENSITIVITY = 0.004     # prob per second vs. baseline
                INT_PROXY_BASELINE_TTT = 2.8          # sec, roughly league-average TTT
                int_proxy_adj = (
                    -qb_cpoe[no_sack_pass] * INT_PROXY_CPOE_SENSITIVITY
                    + (play_ttt[no_sack_pass] - INT_PROXY_BASELINE_TTT) * INT_PROXY_TTT_SENSITIVITY
                )
                int_prob = np.clip(int_prob + int_proxy_adj, 0.0, 1.0)

                is_int = np.zeros(self.N, dtype=bool)
                is_int[no_sack_pass] = np.random.rand(np.sum(no_sack_pass)) < int_prob
                play_is_interception[is_int] = True

                # Completion Roll for those not intercepted
                valid_pass = no_sack_pass & ~is_int
                if np.any(valid_pass):
                    # Logistic depth-decay model with receiver split baseline, play separation rolls, and contested catch override
                    catch_prob = np.zeros(self.N, dtype=np.float32)
                    is_screen = valid_pass & (play_air_yards <= 0)
                    is_normal = valid_pass & (play_air_yards > 0)
                    
                    if np.any(is_normal):
                        # Determine spatial zone based on scrimmage line
                        yd_normal = self.yardline_100[is_normal]
                        zone_normal = _classify_zone(yd_normal)
                        
                        # Load baseline catch rate split for this zone
                        zone_baseline = np.zeros(np.sum(is_normal), dtype=np.float32)
                        zone_baseline[zone_normal == 'primary'] = primary_catch_rate[is_normal][zone_normal == 'primary']
                        zone_baseline[zone_normal == 'redzone'] = redzone_catch_rate[is_normal][zone_normal == 'redzone']
                        zone_baseline[zone_normal == 'goalline'] = goalline_catch_rate[is_normal][zone_normal == 'goalline']
                        
                        # Fallback to general catch rate if splits are 0 (e.g. unobserved or missing)
                        is_zero = (zone_baseline == 0)
                        if np.any(is_zero):
                            zone_baseline[is_zero] = catch_rate[is_normal][is_zero]

                        # Roll play-specific separation: Normal(avg_separation_yds, 1.0) clipped at 0
                        # The 1.0-yard std isn't verified against real per-play
                        # separation variance -- flagged for future research,
                        # not touched now (2026-07-21).
                        sep_mean = avg_separation_yds_recv[is_normal]
                        sep_roll = np.maximum(0.0, np.random.normal(sep_mean, 1.0))

                        # Split plays into Contested (sep_roll <= 1.0) and Open (sep_roll > 1.0).
                        # The 1-yard cutoff matches the community definition of a
                        # "contested catch" (receiver has 1 yard of separation or
                        # less from the defender) -- believed correct, not verified
                        # against an official source; flagged for future research
                        # alongside the std above, not touched now (2026-07-21).
                        is_contested = (sep_roll <= 1.0)
                        is_open = ~is_contested
                        
                        probs_normal = np.zeros(np.sum(is_normal), dtype=np.float32)

                        # Shared completion model (A2, 2026-09-06). In logit space:
                        #   logit(P) = curve(air_yards)
                        #            + SKILL_SHRINK * (logit(anchor_rate) - curve(anchor_adot))
                        #            + sep_deviation            [open path only]
                        #   P = sigmoid(logit(P)) + qb_cpoe - offset   [offset open only]
                        # where curve() is the real completion-vs-depth curve (`_depth_logit`),
                        # `anchor_rate` is the receiver's zone catch rate (open) or
                        # contested_catch_rate (contested), and `anchor_adot` is their
                        # soft-capped ADOT. When a throw lands at the receiver's anchor depth
                        # the curve terms cancel and P -> anchor_rate; away from it, P follows
                        # the real curve, shrunk toward the population by SKILL_SHRINK.

                        # 1. CONTESTED PATH (sep_roll <= 1.0): anchored to contested_catch_rate.
                        if np.any(is_contested):
                            ay_val_c = play_air_yards[is_normal][is_contested]
                            adot_val_c = avg_target_depth_yds_recv[is_normal][is_contested]
                            contested_wr_rate = contested_catch_rate_recv[is_normal][is_contested]
                            qb_cpoe_val_c = qb_cpoe[is_normal][is_contested] / 100.0

                            skill_c = SKILL_SHRINK * (_logit_arr(contested_wr_rate) - _depth_logit(_anchor_adot(adot_val_c)))
                            logit_p_c = _depth_logit(ay_val_c) + skill_c
                            probs_normal[is_contested] = _sigmoid_arr(logit_p_c) + qb_cpoe_val_c

                        # 2. OPEN FIELD PATH (sep_roll > 1.0): anchored to the zone catch rate,
                        # plus the zero-mean separation deviation and the flat calibration offset.
                        if np.any(is_open):
                            ay_val = play_air_yards[is_normal][is_open]
                            adot_val = avg_target_depth_yds_recv[is_normal][is_open]

                            skill = SKILL_SHRINK * (_logit_arr(zone_baseline[is_open]) - _depth_logit(_anchor_adot(adot_val)))

                            # Separation adjustment -- ZERO-MEAN per receiver: only the deviation
                            # from the receiver's own average separation matters (the level is
                            # already in zone_baseline). See the SEP_BONUS_SCALE note.
                            sep_bonus = SEP_BONUS_SCALE * (sep_roll[is_open] - sep_mean[is_open])

                            logit_p = _depth_logit(ay_val) + skill + sep_bonus

                            # QB CPOE in PROBABILITY space, post-sigmoid (CPOE is a pp-of-
                            # completion quantity, not logit-space; adding it pre-sigmoid
                            # compresses it -- Round 7 fix). Flat calibration offset: see the
                            # module-level OPEN_FIELD_CALIBRATION_OFFSET note.
                            qb_cpoe_val = qb_cpoe[is_normal][is_open] / 100.0
                            probs_normal[is_open] = _sigmoid_arr(logit_p) + qb_cpoe_val - OPEN_FIELD_CALIBRATION_OFFSET
                            
                        # Apply to catch_prob and clip final probabilities to [0.01, 0.99]
                        catch_prob[is_normal] = np.clip(probs_normal, 0.01, 0.99)

                        # A2 Phase 2/4 capture: one row per non-screen throw with
                        # everything needed to replay the completion formula
                        # offline under candidate params. Columns:
                        # ay, adot, zone_baseline, contested_wr_rate, sep_roll,
                        # qb_cpoe_frac, is_contested, avg_separation.
                        if self.capture_completion:
                            self._completion_cap.append(np.column_stack([
                                play_air_yards[is_normal].astype(np.float32),
                                avg_target_depth_yds_recv[is_normal].astype(np.float32),
                                zone_baseline.astype(np.float32),
                                contested_catch_rate_recv[is_normal].astype(np.float32),
                                sep_roll.astype(np.float32),
                                (qb_cpoe[is_normal] / 100.0).astype(np.float32),
                                is_contested.astype(np.float32),
                                sep_mean.astype(np.float32),
                            ]))
                    
                    if np.any(is_screen):
                        p_screen = pos_recv[is_screen]
                        is_rb = (p_screen == 'RB')
                        is_wr_te = ~is_rb

                        # Recalibrated against real 2021-2025 screen completion
                        # rates by receiver position (found while investigating
                        # the completion-rate overshoot, clock_physics_v020
                        # session): RB screens were already close to real
                        # (82.68%); WR/TE screens were the dominant driver of
                        # the whole completion-rate problem (95% vs real 80.01%).
                        screen_probs = np.zeros(np.sum(is_screen), dtype=np.float32)
                        screen_probs[is_rb] = 0.83
                        screen_probs[is_wr_te] = 0.80
                        catch_prob[is_screen] = screen_probs
                        
                    is_complete = np.zeros(self.N, dtype=bool)
                    is_complete[valid_pass] = np.random.rand(np.sum(valid_pass)) < catch_prob[valid_pass]
                    play_is_complete[is_complete] = True

                    # A2 Phase 0 instrumentation: tally completion vs incompletion
                    # by target depth over all non-sack pass attempts (INTs
                    # included as attempts with 0 completions, matching the
                    # real-side definition). No effect on game logic.
                    if self.track_cmp_by_depth and np.any(no_sack_pass):
                        _ay = play_air_yards[no_sack_pass]
                        _cmp = play_is_complete[no_sack_pass]
                        _b = np.digitize(_ay, self._cmp_depth_edges, right=True)
                        np.add.at(self._cmp_depth_att, _b, 1)
                        np.add.at(self._cmp_depth_cmp, _b, _cmp)

                    # Resolve YAC for completed passes
                    if np.any(is_complete):
                        yac_ay = play_air_yards[is_complete].astype(np.float32)
                        
                        yd_yac = self.yardline_100[is_complete]
                        zones_yac = _classify_zone(yd_yac)
                        
                        is_away_yac = self.possession_is_away[is_complete]
                        recv_names_yac = play_target[is_complete]

                        cpoe_away_vec = _zone_scalar_vector(self.zone_cpoe[self.away_team], zones_yac)
                        cpoe_home_vec = _zone_scalar_vector(self.zone_cpoe[self.home_team], zones_yac)
                        cpoe_by_filter_yac = np.where(is_away_yac, cpoe_away_vec, cpoe_home_vec)

                        target_share_by_filter_yac = _zone_split_vector(self.precomputed_skill_target_share, recv_names_yac, zones_yac)
                        carry_share_by_filter_yac = _zone_split_vector(self.precomputed_skill_carry_share, recv_names_yac, zones_yac)
                                
                        yac = np.zeros(np.sum(is_complete), dtype=np.float32)
                        for zone in ['goalline', 'redzone', 'primary']:
                            zone_mask = (zones_yac == zone)
                            if not np.any(zone_mask): continue
                            
                            booster = self.registry.yac_model._boosters.get(zone)
                            if not booster:
                                booster = self.registry.yac_model._boosters.get('primary')
                                
                            # room_after_catch (yardline_100 at the snap minus air yards travelled =
                            # distance from the catch spot to the goal line) — added in the Round 8/9
                            # retrain (clock_physics_v020) specifically to fix goalline/redzone YAC
                            # over-prediction (the model had no way to know how little field was left
                            # after the catch). Must stay last and match train_zone_split.py's FEATURES
                            # order exactly, or the booster silently reads garbage.
                            pre_yd_zone = self.yardline_100[is_complete][zone_mask].astype(np.float32)
                            room_after_catch_zone = pre_yd_zone - yac_ay[zone_mask].astype(np.float32)

                            X_yac_zone = np.stack([
                                yac_ay[zone_mask].astype(np.float32),
                                pre_yd_zone,
                                self.distance[is_complete][zone_mask].astype(np.float32),
                                score_diff[is_complete][zone_mask].astype(np.float32),
                                game_sec[is_complete][zone_mask].astype(np.float32),
                                cpoe_by_filter_yac[zone_mask],
                                target_share_by_filter_yac[zone_mask],
                                carry_share_by_filter_yac[zone_mask],
                                room_after_catch_zone
                            ], axis=1)
                            
                            if booster:
                                yac[zone_mask] = booster.inplace_predict(X_yac_zone)
                            else:
                                yac[zone_mask] = 4.2
                                
                        # Inject right-tailed exponential noise to simulate broken tackles and explosive plays.
                        # Noise scale is dynamically adjusted using the receiver's DNA traits (elusiveness & broken_tackle_rate).
                        #
                        # Round 9 fix (clock_physics_v020): the base scale used to be a flat 6.0
                        # regardless of context. That's a reasonable relative magnitude against a
                        # primary-zone mean of ~5.5 yards, but wildly oversized against goalline's
                        # ~0.9-yard mean — adding a 6-yard-scale exponential to a ~1-yard base and
                        # then flooring at zero (`max(0, ...)`) truncates most of the left tail,
                        # which mechanically inflates the mean, worse the smaller the zone's typical
                        # YAC. Measured impact before this fix: goalline YAC ran +237% over real,
                        # redzone +17%, primary negligible — exactly the pattern predicted by a fixed
                        # floor colliding with a shrinking base mean. Fix: scale the base term to
                        # this play's own predicted YAC instead of a flat constant, preserving
                        # primary-zone behavior (~unchanged) while shrinking it proportionally in
                        # low-mean zones. Backtrack: restore `scale = 6.0 + ...` to revert.
                        rec_elusiveness = elusiveness[is_complete]
                        rec_btk = broken_tackle_rate[is_complete]
                        base_scale = np.maximum(0.75, yac * 1.25)
                        # The 1.8 (elusiveness) and 6.0 (broken-tackle-rate)
                        # coefficients below are tuned by feel, not fit against
                        # real data -- unlike base_scale above (Round 9, cited).
                        # Flagged as an offseason investigation, not touched now
                        # (2026-07-21).
                        scale = base_scale + np.maximum(0.0, rec_elusiveness) * 1.8 + rec_btk * 6.0
                        noise = np.random.exponential(scale=scale) - scale

                        yac = np.maximum(0.0, yac + noise).astype(np.int32)
                        play_yac[is_complete] = yac

                        # Total gain = air yards + yac, scaled by pass multipliers to raise totals and widen spreads
                        pass_multipliers = np.where(self.possession_is_away[is_complete], self.pass_mult_away_off, self.pass_mult_home_off)
                        raw_gain = play_air_yards[is_complete] + yac
                        play_gain[is_complete] = np.clip(np.round(raw_gain * pass_multipliers).astype(np.int32), -10, self.yardline_100[is_complete])

            # Roll for mid-play penalties on pass plays (excluding sacks)
            pass_penalty_eligible = is_pass & ~play_is_sack
            if np.any(pass_penalty_eligible):
                r_pen = np.random.rand(np.sum(pass_penalty_eligible))
                
                # Masks relative to pass_penalty_eligible
                off_holding_roll = r_pen < 0.0149
                dpi_roll = (r_pen >= 0.0149) & (r_pen < 0.0149 + 0.0120)
                def_holding_roll = (r_pen >= 0.0149 + 0.0120) & (r_pen < 0.0149 + 0.0120 + 0.0090)
                
                # Map to global masks
                pass_off_holding = np.zeros(self.N, dtype=bool)
                pass_off_holding[pass_penalty_eligible] = off_holding_roll
                
                pass_dpi = np.zeros(self.N, dtype=bool)
                pass_dpi[pass_penalty_eligible] = dpi_roll
                
                pass_def_holding = np.zeros(self.N, dtype=bool)
                pass_def_holding[pass_penalty_eligible] = def_holding_roll
                
                # BUG FIX (#9): this used to decline off_holding/DPI/def_holding
                # whenever the actual play result (interception, or enough
                # yardage) beat the penalty -- real NFL logic, but wrong here.
                # Verified directly against real 2023 PBP data: when a penalty
                # is declined, nflverse's structured `penalty` column is 0, not
                # 1 -- confirmed on an actual "Offensive Holding, declined" play
                # that was also an interception (penalty=0.0, interception=1.0).
                # Declined penalties are invisible to the structured data,
                # only mentioned in free-text `desc`. off_holding_roll's 0.0149
                # (and DPI's 0.0120, def-holding's 0.0090) were derived by
                # filtering on penalty==1, i.e. they already ARE the real-world
                # accepted-only rate, with declines (including exactly the
                # holding-vs-interception case this logic tried to model)
                # already priced out. Adding a decline mechanism on top of an
                # already-accepted-only roll rate double-discounts it, pushing
                # the simulated accepted-penalty rate below the calibration
                # target. The run branch's penalty roll (a few hundred lines
                # down) never had this decline step and was correct by
                # omission -- every roll here is now unconditionally accepted
                # too, matching it.
                play_is_off_holding[pass_off_holding] = True
                play_is_dpi[pass_dpi] = True
                play_is_def_holding[pass_def_holding] = True
                
            has_accepted_penalty = play_is_off_holding | play_is_dpi | play_is_def_holding

            # Record passing attempts and targets vectorially
            pass_attempts_mask = (no_sack_pass | play_is_throwaway) & ~has_accepted_penalty
            for team in [self.away_team, self.home_team]:
                team_mask = pass_attempts_mask & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue
                
                qb = self.qb_starters[team]
                self.player_stats[team][qb]['pAtt'][team_mask] += 1
                self.player_stats[team][qb]['pCmp'][team_mask & play_is_complete] += 1
                self.player_stats[team][qb]['pYds'][team_mask & play_is_complete] += play_gain[team_mask & play_is_complete]
                self.player_stats[team][qb]['int'][team_mask & play_is_interception] += 1
                # play_air_yards is 0 for sacks/scrambles/throwaways (only no_sack_pass
                # lanes get a real sampled value, see no_sack_pass definition below) --
                # summing over the full team_mask is equivalent to summing only real
                # targeted attempts, no separate masking needed.
                self.player_stats[team][qb]['air_yards'][team_mask] += play_air_yards[team_mask]
                
                def_team = self.home_team if team == self.away_team else self.away_team
                self.player_stats[def_team]['Defense']['def_int'][team_mask & play_is_interception] += 1
                
                # Record targets only for actual targeted routes (no_sack_pass)
                team_target_mask = no_sack_pass & (self.possession_is_away == (team == self.away_team)) & ~has_accepted_penalty
                if np.any(team_target_mask):
                    for r_name in np.unique(play_target[team_target_mask]):
                        r_mask = team_target_mask & (play_target == r_name)
                        self.player_stats[team][r_name]['targets'][r_mask] += 1
                        self.player_stats[team][r_name]['rec'][r_mask & play_is_complete] += 1
                        self.player_stats[team][r_name]['recYds'][r_mask & play_is_complete] += play_gain[r_mask & play_is_complete]
                        # Air yards on every target, not just catches (matches real
                        # NFL ADOT convention -- see the QB air_yards note above).
                        self.player_stats[team][r_name]['air_yards'][r_mask] += play_air_yards[r_mask]

        # 4B. Execute RUN Plays
        if np.any(is_run):
            # Select rusher vectorially, zone-aware -- same reasoning as pass
            # target selection above (a real goalline bruiser now actually
            # gets more of the ball in that zone).
            run_zones_full = _classify_zone(self.yardline_100)
            for team in [self.away_team, self.home_team]:
                team_mask = is_run & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue

                for zone in ('primary', 'redzone', 'goalline'):
                    zone_mask = team_mask & (run_zones_full == zone)
                    if not np.any(zone_mask): continue
                    rushers, cum_shares = self.rusher_cache[team][zone]
                    r_vals = np.random.rand(np.sum(zone_mask))
                    rush_idx = np.searchsorted(cum_shares, r_vals)
                    rush_idx = np.minimum(rush_idx, len(cum_shares) - 1)
                    play_rusher[zone_mask] = rushers[rush_idx]

            # Segment is_run by zone
            yd_run = self.yardline_100[is_run]
            zones_run = _classify_zone(yd_run)
            
            is_away_run = self.possession_is_away[is_run]
            rusher_names_run = play_rusher[is_run]

            cpoe_away_vec = _zone_scalar_vector(self.zone_cpoe[self.away_team], zones_run)
            cpoe_home_vec = _zone_scalar_vector(self.zone_cpoe[self.home_team], zones_run)
            cpoe_by_filter_run = np.where(is_away_run, cpoe_away_vec, cpoe_home_vec)

            target_share_by_filter_run = _zone_split_vector(self.precomputed_skill_target_share, rusher_names_run, zones_run)
            carry_share_by_filter_run = _zone_split_vector(self.precomputed_skill_carry_share, rusher_names_run, zones_run)
                    
            pred_unshifted = np.zeros(np.sum(is_run), dtype=np.float32)
            noise = np.zeros(np.sum(is_run), dtype=np.float32)
            
            for zone in ['goalline', 'redzone', 'primary']:
                zone_mask = (zones_run == zone)
                if not np.any(zone_mask): continue
                
                booster = self.registry.rush_model._boosters.get(zone)
                if not booster:
                    booster = self.registry.rush_model._boosters.get('primary')
                    
                X_run_zone = np.stack([
                    self.yardline_100[is_run][zone_mask].astype(np.float32),
                    self.distance[is_run][zone_mask].astype(np.float32),
                    game_sec[is_run][zone_mask].astype(np.float32),
                    score_diff[is_run][zone_mask].astype(np.float32),
                    cpoe_by_filter_run[zone_mask],
                    target_share_by_filter_run[zone_mask],
                    carry_share_by_filter_run[zone_mask]
                ], axis=1)
                
                if booster:
                    pred_log_zone = booster.inplace_predict(X_run_zone)
                    pred_unshifted[zone_mask] = np.exp(pred_log_zone) - 30.0
                else:
                    pred_unshifted[zone_mask] = 4.0
                    
                # Residuals noise
                pool = self.registry.rush_model.residuals_pools.get(zone, [0.0])
                noise[zone_mask] = np.random.choice(pool, size=np.sum(zone_mask))

            # Rush trench matchup gate: roll negative/explosive/normal per play.
            # Non-gated ("normal") plays use pred_unshifted+noise completely
            # unchanged -- the gate replaces the old flat multiplier, it does
            # not add a second effect on top of the base model.
            p_neg_gate = np.where(self.possession_is_away[is_run], self.p_neg_gate_away, self.p_neg_gate_home)
            p_exp_gate = np.where(self.possession_is_away[is_run], self.p_exp_gate_away, self.p_exp_gate_home)

            gate_roll = np.random.rand(np.sum(is_run))
            is_gated_negative = gate_roll < p_neg_gate
            is_gated_explosive = (~is_gated_negative) & (gate_roll < p_neg_gate + p_exp_gate)

            gain = np.round(pred_unshifted + noise).astype(np.int32)
            if np.any(is_gated_negative):
                gain[is_gated_negative] = np.round(
                    np.random.choice(self.rush_negative_pool, size=np.sum(is_gated_negative))
                ).astype(np.int32)
            if np.any(is_gated_explosive):
                gain[is_gated_explosive] = np.round(
                    np.random.choice(self.rush_explosive_pool, size=np.sum(is_gated_explosive))
                ).astype(np.int32)

            # Rescue gate: at matchups where the base model's own organic
            # negative rate already exceeds the real target (the neg-gate above
            # floors at 0 there, since it can only ADD negative plays, never
            # remove them), redraw a normal-mode play that landed <=0 as a
            # non-negative outcome instead. Only ever active where p_rescue_gate
            # is nonzero, i.e. only at the matchup extremes where organic
            # overshoots -- everywhere else this is a no-op.
            is_normal_mode = ~is_gated_negative & ~is_gated_explosive
            landed_negative = is_normal_mode & (gain <= 0)
            if np.any(landed_negative):
                p_rescue = np.where(self.possession_is_away[is_run], self.p_rescue_gate_away, self.p_rescue_gate_home)
                rescue_roll = np.random.rand(np.sum(is_run))
                is_rescued = landed_negative & (rescue_roll < p_rescue)
                if np.any(is_rescued):
                    gain[is_rescued] = np.round(
                        np.random.choice(self.rush_positive_pool, size=np.sum(is_rescued))
                    ).astype(np.int32)

            gain = np.clip(gain, -10, self.yardline_100[is_run])
            play_gain[is_run] = gain

            # Roll for mid-play penalties on run plays
            run_penalty_eligible = is_run
            if np.any(run_penalty_eligible):
                r_pen = np.random.rand(np.sum(run_penalty_eligible))
                run_off_holding = np.zeros(self.N, dtype=bool)
                run_off_holding[run_penalty_eligible] = r_pen < 0.0149
                
                # Assign to global step variables
                play_is_off_holding[run_off_holding] = True
                
            has_accepted_penalty = play_is_off_holding | play_is_dpi | play_is_def_holding

            # Record rushing stats vectorially
            for team in [self.away_team, self.home_team]:
                team_mask = is_run & (self.possession_is_away == (team == self.away_team)) & ~has_accepted_penalty
                if not np.any(team_mask): continue
                
                for r_name in np.unique(play_rusher[team_mask]):
                    r_mask = team_mask & (play_rusher == r_name)
                    self.player_stats[team][r_name]['rAtt'][r_mask] += 1
                    self.player_stats[team][r_name]['rYds'][r_mask] += play_gain[r_mask]

        # Mid-play accepted penalties (pass or run branch) are still a snap —
        # they consume a down/play_id in real pbp data, so tally them for the
        # total-snap audit without double-counting against play_count below.
        # Reference the arrays directly (not the branch-local `has_accepted_penalty`
        # name) since they're unconditionally initialized at the top of Phase 4
        # (before the is_pass/is_run split), while `has_accepted_penalty` itself
        # is only assigned inside those branches.
        self.penalties_accepted[play_is_off_holding | play_is_dpi | play_is_def_holding] += 1

        # -------------------------------------------------------------
        # Phase 5: Mid-Play Chaos (Fumbles)
        # -------------------------------------------------------------
        # BUG FIX (game_engine cleanup audit, #5): scrambles were previously
        # invisible to fumble risk entirely -- valid_carrier_mask required
        # is_run | (is_pass & play_is_complete), and play_is_complete is only
        # ever set inside no_sack_pass, which explicitly excludes scrambles
        # (they're a separate branch). play_gain/play_is_scramble are already
        # fully resolved earlier, in the sack-gate section (~line 1010), well
        # before this phase runs, so no sequencing change was needed here --
        # just adding play_is_scramble to the mask. Scrambles are treated as
        # run-like for fumble purposes (base_fumble_prob below), matching how
        # they're already recorded as rushing attempts elsewhere (rAtt, not
        # a passing stat) -- a QB in the open field getting hit is a rushing-
        # style fumble risk, not a receiver-gets-popped-after-the-catch one.
        # carrier must resolve to the QB (not play_target, which is the
        # receiver who was never actually thrown to on a busted dropback)
        # for scramble lanes specifically.
        qb_per_lane = np.where(self.possession_is_away, self.qb_starters[self.away_team], self.qb_starters[self.home_team])
        carrier = np.where(is_run, play_rusher, np.where(play_is_scramble, qb_per_lane, play_target))
        valid_carrier_mask = active & (is_run | (is_pass & play_is_complete) | play_is_scramble) & ~has_accepted_penalty

        if np.any(valid_carrier_mask):
            # Per-carrier DNA fumble-rate multiplier removed: 'fumble_rate' is
            # not a field in any DNA file (confirmed via grep across
            # rb_dna.json/wr_dna.json/te_dna.json) -- every player has always
            # hit the `.get('fumble_rate', 0.015)` default and multiplied by
            # exactly 1.0, 100% of the time. Dead-code deletion, not a
            # recalibration: real fumble propensity doesn't meaningfully vary
            # across rostered players anyway (bad fumblers don't stay on a
            # roster). Unknown/unresolved carriers (only possible via the
            # empty-roster fallback path) keep the prior behavior of never
            # fumbling, same as when `fumble_factor` defaulted to 0 for them.
            known_carrier_mask = valid_carrier_mask & (carrier != None) & (carrier != "Unknown")
            base_fumble_prob = np.where(is_run | play_is_scramble, 0.006520 * 0.80, 0.005246 * 0.80)

            fumbles = known_carrier_mask & (np.random.rand(self.N) < base_fumble_prob)
            fumbles_lost = fumbles & (np.random.rand(self.N) < 0.50)
            
            play_is_fumble[fumbles] = True
            play_is_fumble_lost[fumbles_lost] = True
            
            # Record carrier fumbles
            for team in [self.away_team, self.home_team]:
                team_mask = fumbles & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue
                for c_name in np.unique(carrier[team_mask]):
                    if c_name is None or c_name == "Unknown": continue
                    c_mask = team_mask & (carrier == c_name)
                    self.player_stats[team][c_name]['fumbles'][c_mask] += 1
                    self.player_stats[team][c_name]['fumbles_lost'][c_mask & fumbles_lost] += 1
                
                def_team = self.home_team if team == self.away_team else self.away_team
                self.player_stats[def_team]['Defense']['def_fumble_rec'][team_mask & fumbles_lost] += 1

        # -------------------------------------------------------------
        # Phase 6: Finalize State
        # -------------------------------------------------------------
        self.play_count[active] += 1

        # Safety detection: yardline_100 > 100 means the ball carrier was
        # tackled behind their own goal line. Checked (OR'd in) right after
        # each of the 3 places yardline_100 can be pushed past 100 by a loss
        # -- sacks (up to -25 yards) and big losses deep in a team's own
        # territory can both do this, and previously nothing caught it; the
        # fumble-recovery site in particular already re-clips its own return
        # yardage to [1,99] a few lines after the raw subtraction, which was
        # silently hiding the overshoot instead of flagging it. Applied once,
        # consolidated, at the end of Phase 6 (after `td_mask`/turnover logic)
        # so it can correctly override an erroneous turnover-on-downs read on
        # the same play -- a sack for a safety is a safety, not a turnover on
        # downs, regardless of what down it happened on.
        is_safety = np.zeros(self.N, dtype=bool)

        # Sacks (excluding lost fumbles)
        active_sack_no_lost_fumble = active & play_is_sack & ~play_is_fumble_lost & ~has_accepted_penalty
        if np.any(active_sack_no_lost_fumble):
            self.yardline_100[active_sack_no_lost_fumble] -= play_gain[active_sack_no_lost_fumble]
            is_safety |= active_sack_no_lost_fumble & (self.yardline_100 > 100)
            self.down[active_sack_no_lost_fumble] += 1
            self.distance[active_sack_no_lost_fumble] -= play_gain[active_sack_no_lost_fumble]
            
            turnover_mask = active_sack_no_lost_fumble & (self.down > 4)
            self._switch_possession(scored=False, mask=turnover_mask)

        # Turnover return yards, hoisted so Phase 7's clock runoff (which comes
        # later) can scale by actual return length instead of a flat constant.
        turnover_return_yards = np.zeros(self.N, dtype=np.int32)

        # Interceptions
        active_int = active & play_is_interception & ~has_accepted_penalty
        if np.any(active_int):
            # Roll for defensive TD (8.92% chance based on 10-year fit)
            int_td_mask = active_int & (np.random.rand(self.N) < 0.0892)
            int_no_td = active_int & ~int_td_mask

            # Handle TD
            if np.any(int_td_mask):
                self.score_away[int_td_mask & ~self.possession_is_away] += 7
                self.score_home[int_td_mask & self.possession_is_away] += 7
                self.needs_kickoff[int_td_mask] = True
                self.yardline_100[int_td_mask] = 70
                self.down[int_td_mask] = 1
                self.distance[int_td_mask] = 10
                turnover_return_yards[int_td_mask] = 50  # by definition a long return

                # Record DST stats
                for team in [self.away_team, self.home_team]:
                    def_team = self.home_team if team == self.away_team else self.away_team
                    team_mask = int_td_mask & (self.possession_is_away == (team == self.away_team))
                    self.player_stats[def_team]['Defense']['def_td'][team_mask] += 1

            # Handle normal INT
            if np.any(int_no_td):
                # Place ball at interception spot (yardline_100 - play_air_yards) and switch possession
                self.yardline_100[int_no_td] = np.maximum(1, np.minimum(99, self.yardline_100[int_no_td] - play_air_yards[int_no_td]))
                self._switch_possession(scored=False, mask=int_no_td)

                n_no_td = np.sum(int_no_td)

                # Roll for slide/tackle (40.04% of non-TD INTs)
                is_slide = np.random.rand(n_no_td) < 0.4004

                # For non-slides, sample from Shifted Gamma. Params undocumented
                # -- see the kickoff-return note (Kickoffs Handling section) for
                # the flagged-for-future-recheck status shared by all
                # return-yardage distributions in this file.
                is_active = ~is_slide
                n_active = np.sum(is_active)

                ret_yds = np.zeros(n_no_td, dtype=np.int32)
                if n_active > 0:
                    gamma_vals = np.random.gamma(shape=3.0552, scale=7.6877, size=n_active) - 7.3231
                    ret_yds[is_active] = np.round(gamma_vals).astype(np.int32)

                # Capped at remaining distance (yardline_100 - 1) to prevent unsanctioned TDs
                max_ret = self.yardline_100[int_no_td] - 1
                ret_yds = np.minimum(ret_yds, max_ret)
                # BUG FIX (#7): floor at the recovery spot -- a negative raw
                # sample (returner tackled immediately) previously stayed
                # negative for the actual yardline_100 update below while the
                # recorded stat separately floored it to 0, so the two could
                # silently disagree. Flooring ret_yds itself here means both
                # use the same, consistent value: a "return" is never a loss
                # from the recovery point, worst case it's a 0-yard return.
                ret_yds = np.maximum(0, ret_yds)

                self.yardline_100[int_no_td] = np.maximum(1, np.minimum(99, self.yardline_100[int_no_td] - ret_yds))
                turnover_return_yards[int_no_td] = ret_yds

        # Lost Fumbles (from run or completed pass or sack-fumble)
        active_lost_fumble = active & play_is_fumble_lost & ~has_accepted_penalty
        if np.any(active_lost_fumble):
            # Roll for defensive TD (8.02% chance based on 10-year fit)
            fumble_td_mask = active_lost_fumble & (np.random.rand(self.N) < 0.0802)
            fumble_no_td = active_lost_fumble & ~fumble_td_mask
            
            # Handle TD
            if np.any(fumble_td_mask):
                self.score_away[fumble_td_mask & ~self.possession_is_away] += 7
                self.score_home[fumble_td_mask & self.possession_is_away] += 7
                self.needs_kickoff[fumble_td_mask] = True
                self.yardline_100[fumble_td_mask] = 70
                self.down[fumble_td_mask] = 1
                self.distance[fumble_td_mask] = 10
                turnover_return_yards[fumble_td_mask] = 50  # by definition a long return

                # Record DST stats
                for team in [self.away_team, self.home_team]:
                    def_team = self.home_team if team == self.away_team else self.away_team
                    team_mask = fumble_td_mask & (self.possession_is_away == (team == self.away_team))
                    self.player_stats[def_team]['Defense']['def_td'][team_mask] += 1
            
            # Handle normal fumble
            if np.any(fumble_no_td):
                # Place ball at fumble recovery spot (yardline_100 - play_gain) and switch possession
                self.yardline_100[fumble_no_td] -= play_gain[fumble_no_td]
                # Must check for safety here, BEFORE _switch_possession -- it
                # remaps yardline_100 to (100 - yardline_100), which would
                # turn an overshoot past 100 into an invalid negative value
                # that then gets silently clamped to 1 by the return-yardage
                # clip a few lines below instead of ever being caught.
                is_safety |= fumble_no_td & (self.yardline_100 > 100)
                self._switch_possession(scored=False, mask=fumble_no_td)
                
                n_no_td = np.sum(fumble_no_td)
                
                # Roll for securing ball (94.10% of non-TD lost fumbles)
                is_secure = np.random.rand(n_no_td) < 0.9410
                
                # For non-secure active returns, sample from Shifted Exponential.
                # Params undocumented -- see the kickoff-return note (Kickoffs
                # Handling section) for the flagged-for-future-recheck status
                # shared by all return-yardage distributions in this file.
                is_active = ~is_secure
                n_active = np.sum(is_active)
                
                ret_yds = np.zeros(n_no_td, dtype=np.int32)
                if n_active > 0:
                    expon_vals = np.random.exponential(scale=23.7483, size=n_active) - 9.0
                    ret_yds[is_active] = np.round(expon_vals).astype(np.int32)
                
                # Capped at remaining distance (yardline_100 - 1) to prevent unsanctioned TDs
                max_ret = self.yardline_100[fumble_no_td] - 1
                ret_yds = np.minimum(ret_yds, max_ret)
                # BUG FIX (#7): same fix as the interception-return case above
                # -- floor at the recovery spot so the actual field-position
                # update and the recorded return-yardage stat can't disagree.
                ret_yds = np.maximum(0, ret_yds)

                self.yardline_100[fumble_no_td] = np.maximum(1, np.minimum(99, self.yardline_100[fumble_no_td] - ret_yds))
                turnover_return_yards[fumble_no_td] = ret_yds

        # Accepted Mid-Play Penalties
        if np.any(has_accepted_penalty):
            # 1. Offensive Holding: 10 yards, replay down
            oh_mask = active & play_is_off_holding
            if np.any(oh_mask):
                self.yardline_100[oh_mask] = np.minimum(99, self.yardline_100[oh_mask] + 10)
                self.distance[oh_mask] += 10
                
            # 2. Defensive Pass Interference (DPI): spot of foul, automatic 1st down
            dpi_mask = active & play_is_dpi
            if np.any(dpi_mask):
                self.yardline_100[dpi_mask] = np.maximum(1, np.minimum(99, self.yardline_100[dpi_mask] - play_air_yards[dpi_mask]))
                self.down[dpi_mask] = 1
                self.distance[dpi_mask] = np.minimum(10, self.yardline_100[dpi_mask])
                
            # 3. Defensive Holding: 5 yards, automatic 1st down
            dh_mask = active & play_is_def_holding
            if np.any(dh_mask):
                self.yardline_100[dh_mask] = np.maximum(1, self.yardline_100[dh_mask] - 5)
                self.down[dh_mask] = 1
                self.distance[dh_mask] = np.minimum(10, self.yardline_100[dh_mask])

        # Regular Runs / Passes / Scrambles (No Sacks/Interceptions/Lost Fumbles/Penalties)
        # td_mask/turnover_down initialized here (not inside the branch below) so
        # they're always defined for Phase 7's clock-stop exclusions, even on a
        # step where no lane in normal_play actually happens to be true.
        td_mask = np.zeros(self.N, dtype=bool)
        turnover_down = np.zeros(self.N, dtype=bool)
        is_oob = np.zeros(self.N, dtype=bool)
        normal_play = active & ~play_is_sack & ~play_is_interception & ~play_is_fumble_lost & ~has_accepted_penalty
        if np.any(normal_play):
            self.yardline_100[normal_play] -= play_gain[normal_play]
            is_safety |= normal_play & (self.yardline_100 > 100)

            # Check Touchdowns
            td_mask = normal_play & (self.yardline_100 <= 0)
            if np.any(td_mask):
                self.score_away[td_mask & self.possession_is_away] += 7
                self.score_home[td_mask & ~self.possession_is_away] += 7
                self.needs_kickoff[td_mask] = True
                
                # Record Touchdown stats vectorially
                # PASS TD
                td_pass = td_mask & is_pass
                if np.any(td_pass):
                    for team in [self.away_team, self.home_team]:
                        team_mask = td_pass & (self.possession_is_away == (team == self.away_team))
                        if not np.any(team_mask): continue
                        
                        qb = self.qb_starters[team]
                        self.player_stats[team][qb]['pTD'][team_mask] += 1
                        
                        for r_name in np.unique(play_target[team_mask]):
                            r_mask = team_mask & (play_target == r_name)
                            self.player_stats[team][r_name]['recTD'][r_mask] += 1
                
                # RUN or QB Scramble TD
                td_run = td_mask & (is_run | play_is_scramble)
                if np.any(td_run):
                    for team in [self.away_team, self.home_team]:
                        team_mask = td_run & (self.possession_is_away == (team == self.away_team))
                        if not np.any(team_mask): continue
                        
                        qb = self.qb_starters[team]
                        scramble_td = team_mask & play_is_scramble
                        self.player_stats[team][qb]['rTD'][scramble_td] += 1
                        
                        run_td = team_mask & is_run
                        for r_name in np.unique(play_rusher[run_td]):
                            r_mask = run_td & (play_rusher == r_name)
                            self.player_stats[team][r_name]['rTD'][r_mask] += 1

                self._switch_possession(scored=True, mask=td_mask)

            # Out-of-bounds roll (clock_physics_v020). Only non-scoring
            # run/completed-pass plays are eligible — can't go OOB after
            # scoring, and incomplete passes have their own clock treatment
            # already (inc_mask, Phase 7). Doesn't affect yardage/down
            # bookkeeping at all, purely a clock-timing flag consumed in Phase 7.
            oob_eligible = normal_play & ~td_mask & (is_run | (is_pass & play_is_complete))

            # Baseline: play-type-conditioned flat rate (real 2021-2025 overall
            # rates: completed pass 20.86%, run 7.45%).
            oob_prob = np.where(is_run, 0.0745, 0.2086)

            # Situational overrides (round 4, real 2021-2025 data by
            # score-margin x time window — docs/audit/clock_physics_v020).
            # Blended run+pass rate for that cohort, replacing the play-type
            # baseline only in these specific windows. Tied-and-not-urgent in
            # Q4 deliberately has no override — it measured within noise of
            # the overall baseline, unlike what intuition suggested.
            margin = poss_scores - def_scores
            is_q4 = self.quarter == 4
            is_q2 = self.quarter == 2

            leading_q4_late = is_q4 & (self.time_remaining <= 300) & (margin > 0)
            trailing_q4_late = is_q4 & (self.time_remaining <= 300) & (margin < 0)
            tied_q4_urgent = is_q4 & (self.time_remaining <= 60) & (margin == 0) & (self.yardline_100 > 50)
            q2_late_leading = is_q2 & (self.time_remaining <= 120) & (margin > 0)
            q2_late_trailing = is_q2 & (self.time_remaining <= 120) & (margin < 0)
            q2_late_tied = is_q2 & (self.time_remaining <= 120) & (margin == 0)

            oob_prob = np.where(leading_q4_late, 0.043, oob_prob)
            oob_prob = np.where(trailing_q4_late, 0.177, oob_prob)
            oob_prob = np.where(tied_q4_urgent, 0.313, oob_prob)
            oob_prob = np.where(q2_late_leading, 0.221, oob_prob)
            oob_prob = np.where(q2_late_trailing, 0.204, oob_prob)
            oob_prob = np.where(q2_late_tied, 0.181, oob_prob)

            oob_roll = np.random.rand(self.N)
            is_oob = oob_eligible & (oob_roll < oob_prob)

            # Regular play advancement (no TD)
            adv_mask = normal_play & (self.yardline_100 > 0)
            if np.any(adv_mask):
                is_first = play_gain >= self.distance
                first_down = adv_mask & is_first
                no_first = adv_mask & ~is_first
                
                self.down[first_down] = 1
                self.distance[first_down] = 10
                
                self.down[no_first] += 1
                self.distance[no_first] -= play_gain[no_first]
                
                # Turnover on downs
                turnover_down = no_first & (self.down > 4)
                self._switch_possession(scored=False, mask=turnover_down)

            # Track plays over 20 yards
            # BUG FIX (#5): same scramble blind spot as the Phase 5 fumble
            # mask above -- a 20+ yard scramble previously never counted.
            big_play = normal_play & (play_gain >= 20) & (is_run | (is_pass & play_is_complete) | play_is_scramble)
            self.plays_over_20_yds[big_play] += 1

        # Safety: score + free-kick handoff. Runs after normal_play's own
        # TD/turnover-on-downs logic above so it correctly overrides an
        # erroneous turnover-on-downs read for the same play -- getting
        # sacked or tackled in your own end zone is a safety regardless of
        # down, the whistle blows immediately. Same scored=True pattern as a
        # TD (line ~1860 above): award points, flag needs_kickoff, then
        # _switch_possession flips possession to the team that will RECEIVE
        # the ensuing free kick -- i.e. the team that just scored the safety,
        # since the team that was tackled in their own end zone is the one
        # who has to kick it away.
        if np.any(is_safety):
            self.score_home[is_safety & self.possession_is_away] += 2
            self.score_away[is_safety & ~self.possession_is_away] += 2
            self.needs_kickoff[is_safety] = True
            self._switch_possession(scored=True, mask=is_safety)

        # Overtime resolution. A safety ends the game immediately, full stop
        # -- no possession-guarantee exception (Cam's explicit rule, unlike
        # regulation where a safety is just a 2-point score and play
        # continues). Otherwise: once BOTH teams have had a possession
        # CONCLUDE this OT period (tracked in _switch_possession above, not
        # here -- see that method for why "concluded" vs. "started" matters),
        # the next score differential ends the game. Sudden death falls out
        # of this same check for free: both flags stay True for the rest of
        # the period once set, so any later score that breaks the tie ends
        # it the moment this check runs again.
        in_ot = self.quarter >= 5
        ot_safety_end = in_ot & is_safety
        ot_score_end = in_ot & self.ot_away_possessed & self.ot_home_possessed & (self.score_away != self.score_home)
        self.game_over[ot_safety_end | ot_score_end] = True

        # -------------------------------------------------------------
        # Phase 7: Clock Management
        # -------------------------------------------------------------
        # These 9 categories are a deliberate, exhaustive partition of `active`
        # (each mask explicitly subtracts every category before it, and the
        # regular/oob sub-splits are exhaustive within their parent) -- no lane
        # is ever touched by more than one. `_run_clock` itself pays a fixed
        # full-N cost per call (a full zeros() allocation plus a full-N
        # two-minute-warning check) regardless of how sparse its mask is, so
        # calling it once per category (up to 9x/step) was paying that fixed
        # cost 9 times over. Instead: each category writes its own seconds
        # value into its own (disjoint) slice of one shared `combined_seconds`
        # array -- cheap, no `_run_clock` overhead -- and there's a single
        # `_run_clock` call at the end covering all of `active` at once.
        # Grocery-trip analogy: 9 people writing their own item on one shared
        # list is free; the expensive part is the trip to the store, which now
        # happens once instead of 9 times.
        #
        # Note: this changes the exact sequence of np.random draws relative to
        # the pre-consolidation code whenever _run_clock's own internal
        # end-of-quarter/OT-tied coin flip fires (verified: ~500 steps out of
        # a 3000-game batch hit this), since that draw used to happen
        # interleaved between the 9 separate calls and now happens once at the
        # end. That means this change does NOT reproduce bit-for-bit under a
        # fixed seed the way the rest of this session's refactors did --
        # verified instead via a large-N statistical A/B (20000+ games each,
        # same team matchup, differently seeded) confirming aggregate rates
        # (tie rate, OT entry rate, mean total/spread) match within normal
        # Monte Carlo sampling noise -- see WORKLOG/this session's notes for
        # the actual z-test numbers.
        #
        # `regular_clock` (the catch-all "everything else" category, below)
        # used to be a hand-maintained NOT-list -- every other category had to
        # be named explicitly (`~inc_mask & ~turnover_clock & ~penalty_clock &
        # ...`), so adding a new category anywhere in this block meant also
        # remembering to add it there, or it would silently get double-handled
        # by both its own category AND regular_clock. Fixed: each category ORs
        # itself into a shared `clock_handled` mask as it's defined, and
        # `regular_clock` is just `active & ~clock_handled` -- a future new
        # category only needs the one `clock_handled |= new_mask` line right
        # next to where it's defined, nothing to remember anywhere else.
        combined_seconds = np.zeros(self.N, dtype=np.int32)
        clock_handled = np.zeros(self.N, dtype=bool)

        # Incomplete Pass clock runoff
        inc_mask = active & is_pass & ~play_is_complete & ~play_is_sack & ~play_is_interception & ~play_is_fumble_lost & ~has_accepted_penalty
        if np.any(inc_mask):
            runoff = np.round(play_ttt[inc_mask] + np.maximum(0, play_air_yards[inc_mask]) / 10.0).astype(np.int32)
            combined_seconds[inc_mask] = runoff
            self.clock_stopped[inc_mask] = True
        clock_handled |= inc_mask

        # Scoring-play clock stop (offensive TD via normal_play). Kickoff/punt
        # TDs never reach here (resolved in their own early-branch sections,
        # which remove those lanes from `active` before this point).
        # Turnover-return TDs also don't need excluding here: `td_mask` itself
        # (`normal_play & yardline_100<=0`) doesn't include them -- interception/
        # fumble-return scores are a separate flag (`int_td_mask`/`fumble_td_mask`)
        # handled entirely within the `turnover_clock` category below, not this
        # one. Bug fix: previously these fell through into the normal
        # running-clock treatment like any other play.
        # Live-play duration scales loosely with how far the ball traveled on
        # the score (a 1-yard punch-in is near-instant; a long house call takes
        # longer), matching the ~0-14s range real long plays show. Safeties
        # are included here too -- the whistle blows dead the instant the
        # ball carrier is down in their own end zone, same dead-ball-on-score
        # clock treatment as a TD.
        scoring_play_clock = active & (td_mask | is_safety)
        if np.any(scoring_play_clock):
            scoring_runoff = np.clip(np.round(3 + play_gain[scoring_play_clock] / 6.0), 2, 14).astype(np.int32)
            combined_seconds[scoring_play_clock] = scoring_runoff
            self.clock_stopped[scoring_play_clock] = True
        clock_handled |= scoring_play_clock

        # Turnover-on-downs clock stop. Bug fix: previously fell through into
        # the normal running-clock treatment. No return happens (ball's just
        # dead short of the sticks), so this is a short tackle-to-whistle
        # duration, not a variable return-length one.
        turnover_down_clock = active & turnover_down
        if np.any(turnover_down_clock):
            combined_seconds[turnover_down_clock] = np.random.randint(4, 8, size=np.sum(turnover_down_clock))
            self.clock_stopped[turnover_down_clock] = True
        clock_handled |= turnover_down_clock

        # Turnover clock runoff — scaled by actual return length (hoisted
        # turnover_return_yards) instead of a flat constant.
        turnover_clock = active & (play_is_interception | play_is_fumble_lost) & ~has_accepted_penalty
        if np.any(turnover_clock):
            turnover_runoff = np.clip(np.round(4 + turnover_return_yards[turnover_clock] / 5.0), 4, 14).astype(np.int32)
            combined_seconds[turnover_clock] = turnover_runoff
            self.clock_stopped[turnover_clock] = True
        clock_handled |= turnover_clock

        # Penalty clock runoff (Accepted mid-play penalties)
        penalty_clock = active & has_accepted_penalty
        if np.any(penalty_clock):
            combined_seconds[penalty_clock] = 10
            self.clock_stopped[penalty_clock] = True
        clock_handled |= penalty_clock

        # Out-of-bounds clock runoff (clock_physics_v020). Excludes lanes that
        # are also turnover_down — those already got their own clock treatment
        # above. Two regimes, matching the confirmed NFL rule (last 2:00 of Q2 /
        # last 5:00 of Q4, clock holds until snap; otherwise it resumes on the
        # ready-for-play signal well before the snap):
        #   - Outside the crunch window: behaves like a normal play minus the
        #     brief ball-spot freeze — draw from the same pace pool, then
        #     subtract a random 3-5s. Clock keeps running (clock_stopped=False),
        #     same as any other continuing-drive play.
        #   - Inside the crunch window: full stop. Runoff collapses to just the
        #     live-play action time (a few seconds) since the clock freezes
        #     until the next snap.
        oob_clock = active & is_oob & ~turnover_down
        if np.any(oob_clock):
            in_crunch = oob_clock & (
                ((self.quarter == 2) & (self.time_remaining <= 120)) |
                ((self.quarter == 4) & (self.time_remaining <= 300))
            )
            normal_oob = oob_clock & ~in_crunch
            if np.any(normal_oob):
                # Set False BEFORE the combined _run_clock call below, not
                # after — its two-minute warning clamp may set some of these
                # lanes True internally, and an unconditional overwrite
                # afterward would stomp on that.
                self.clock_stopped[normal_oob] = False
                pace_draw = self._sample_pace_runoff(normal_oob)
                # Play-type-specific reduction (confirmed via real data controlling
                # for play type: runs save ~2.18s going OOB, completed passes save
                # ~4.24s — a flat 3-5s for both overstated the run case).
                n_oob = int(np.sum(normal_oob))
                run_reduction = np.random.randint(1, 4, size=n_oob)
                pass_reduction = np.random.randint(3, 6, size=n_oob)
                oob_reduction = np.where(is_run[normal_oob], run_reduction, pass_reduction)
                combined_seconds[normal_oob] = np.maximum(pace_draw - oob_reduction, 4)
            if np.any(in_crunch):
                combined_seconds[in_crunch] = np.random.randint(3, 8, size=np.sum(in_crunch))
                self.clock_stopped[in_crunch] = True
        clock_handled |= oob_clock

        # Normal run/complete clock runoff -- catch-all for everything not
        # already claimed by a category above (see the `clock_handled` note
        # at the top of this phase).
        regular_clock = active & ~clock_handled
        if np.any(regular_clock):
            # Set False BEFORE the combined _run_clock call below, not after —
            # its two-minute warning clamp may set some of these lanes True
            # internally, and an unconditional overwrite afterward would stomp
            # on that (this was a real bug, caught via the sim_check pbp log).
            self.clock_stopped[regular_clock] = False
            is_hurry_reg = regular_clock & is_hurry
            combined_seconds[is_hurry_reg] = 10

            is_normal_reg = regular_clock & ~is_hurry

            # Removed (clock_physics_v020, Round 10): the discrete "squeeze play"
            # mechanic that rolled a probability to deliberately rush the snap
            # (8-13s runoff) specifically to guarantee an extra down fit in
            # before the two-minute warning. Cam's call: that's the "greedy for
            # an extra play" behavior we don't want — real teams don't get a
            # bonus down, they just play faster in that window. That realistic
            # speedup is already captured empirically: `_sample_pace_runoff`'s
            # pools are keyed by fine-grained time windows including "Q2
            # 4:00-2:00" and "Q4 5:00-2:00" (see analyze_clock_pace_grid.py),
            # which reflect real teams' actual hurry-up pace approaching the
            # warning — no separate mechanic needed to produce faster snaps
            # there, and no artificial extra-play guarantee riding along with
            # it. `_run_clock`'s own two-minute-warning clamp still fires
            # exactly at 2:00 regardless of how it's approached.
            combined_seconds[is_normal_reg] = self._sample_pace_runoff(is_normal_reg)

        # The single combined call: every category above wrote into its own
        # disjoint slice of combined_seconds, so one _run_clock pass over all
        # of `active` produces the exact same per-lane result as calling it
        # once per category did -- just without paying that call's fixed
        # full-N overhead up to 9 times.
        if np.any(active):
            self._run_clock(combined_seconds[active], active)

        # Quarter/OT-period transitions, once per step -- self.step_start_active
        # (not the by-now-narrowed `active`) so a lane that crossed zero via
        # an earlier branch (kneel/spike/kickoff/punt/FG) isn't missed.
        self._resolve_quarter_transitions(self.step_start_active)

        # -------------------------------------------------------------
        # Positional Evaluator Hook: snapshot this step's play classification.
        # `active` here reflects lanes that ran a real scrimmage down (kneel/spike/
        # timeout/kickoff lanes were already filtered out above). Additive only.
        # -------------------------------------------------------------
        self.last_play_scrimmage_mask = active
        self.last_play_is_pass = is_pass
        self.last_play_is_run = is_run
        self.last_play_air_yards = play_air_yards
        self.last_play_is_sack = play_is_sack
        self.last_play_is_scramble = play_is_scramble
        self.last_play_pre_yardline_100 = pre_yardline_100
        self.last_play_is_complete_pass = play_is_complete
        self.last_play_yac = play_yac
        self.last_play_gain = play_gain
        self.last_play_target_name = play_target
        self.last_play_rusher_name = play_rusher
        self.last_play_is_normal_play = normal_play

    def _sample_pace_runoff(self, mask):
        """
        Vectorized bootstrap sample from the clock_pace_v_0_1_0 empirical pools
        for the given mask's lanes, bucketed by (quarter/time-window x score-
        margin tier, posteam perspective). Replaces the old flat
        randint(18,30) baseline. See docs/audit/clock_physics_v020/ for the
        real-data grid these pools were built from and the rationale.

        Quarter 5+ (OT) reuses the Q4 buckets — no dedicated OT data, and
        endgame pacing is a reasonable stand-in.
        """
        n = int(np.sum(mask))
        if n == 0:
            return np.zeros(0, dtype=np.int32)

        pace_model = self.registry.clock_pace_model
        if pace_model is None:
            return np.random.randint(18, 30, size=n)

        quarter = self.quarter[mask]
        time_remaining = self.time_remaining[mask]
        poss_scores = np.where(self.possession_is_away[mask], self.score_away[mask], self.score_home[mask])
        def_scores = np.where(self.possession_is_away[mask], self.score_home[mask], self.score_away[mask])
        score_diff = poss_scores - def_scores

        row_group = np.empty(n, dtype=object)
        row_group[quarter == 1] = "Q1"

        q2 = quarter == 2
        row_group[q2 & (time_remaining > 240)] = "Q2 >4:00"
        row_group[q2 & (time_remaining <= 240) & (time_remaining > 120)] = "Q2 4:00-2:00"
        row_group[q2 & (time_remaining <= 120)] = "Q2 <2:00"

        q3 = quarter == 3
        row_group[q3 & (time_remaining > 600)] = "Q3 >10:00"
        row_group[q3 & (time_remaining <= 600) & (time_remaining > 300)] = "Q3 10:00-5:00"
        row_group[q3 & (time_remaining <= 300)] = "Q3 <5:00"

        q4_plus = quarter >= 4
        row_group[q4_plus & (time_remaining > 600)] = "Q4 >10:00"
        row_group[q4_plus & (time_remaining <= 600) & (time_remaining > 300)] = "Q4 10:00-5:00"
        row_group[q4_plus & (time_remaining <= 300) & (time_remaining > 120)] = "Q4 5:00-2:00"
        row_group[q4_plus & (time_remaining <= 120)] = "Q4 <2:00"

        margin_tier = np.empty(n, dtype=object)
        margin_tier[score_diff == 0] = "Tied"
        margin_tier[(score_diff >= 1) & (score_diff <= 8)] = "Leading 1-score"
        margin_tier[(score_diff >= 9) & (score_diff <= 16)] = "Leading 2-score"
        margin_tier[score_diff >= 17] = "Leading 3+ score"
        margin_tier[(score_diff <= -1) & (score_diff >= -8)] = "Trailing 1-score"
        margin_tier[(score_diff <= -9) & (score_diff >= -16)] = "Trailing 2-score"
        margin_tier[score_diff <= -17] = "Trailing 3+ score"

        # Q1's 3+ tiers were merged into 2-score tiers when the pools were built
        q1_lanes = row_group == "Q1"
        margin_tier[q1_lanes & (margin_tier == "Leading 3+ score")] = "Leading 2-score"
        margin_tier[q1_lanes & (margin_tier == "Trailing 3+ score")] = "Trailing 2-score"

        result = np.empty(n, dtype=np.float64)
        cell_keys = np.array([f"{rg}|{mt}" for rg, mt in zip(row_group, margin_tier)])
        for key in np.unique(cell_keys):
            idx = cell_keys == key
            rg, mt = key.split("|", 1)
            pool = pace_model.get_pool(rg, mt)
            result[idx] = np.random.choice(pool, size=int(np.sum(idx)))

        # Global calibration nudge, cumulative (clock_physics_v020):
        # round 3 added +1s (post-OOB/2-min-warning/squeeze overshoot was
        # ~7-9%, ~67 offensive plays/team vs. real ~62) which only recovered
        # ~4 plays/game. Round 4 added a second +1s (total +2s) per Cam's
        # call — still ~4/team high after round 3.
        # Round N (2026-08-13): re-measured after this session's sacks
        # recalibration, independent-scramble-roll rework, and INT proxy all
        # landed on top of the +2s baseline — none of them touch pace
        # directly, but completion/sack/scramble mix shifts plays-per-drive,
        # so the old +2s no longer held. Measured combined offensive
        # snaps/game via scripts/eda/test_pace_nudge_calibration.py (16-game
        # sample, N=500): +2s -> 134.3 (target 123.82, +8.5% high); +5s ->
        # 124.7 (+0.7%); +6s -> 121.9 (-1.5%). +5s is the closer of the two
        # integer bracket points, landing at ~62.3/team -- within Cam's
        # stated 60-63/team acceptable range. Propagates into OOB's runoff
        # too (draws from this same pool before its reduction) — intentional,
        # it's a broad "time between plays" adjustment, not play-type-specific.
        return np.round(result).astype(np.int32) + 5

    def _run_clock(self, seconds, mask):
        # Two-minute warning (clock_physics_v020): automatic, free stoppage the
        # first time the clock would cross below 2:00 remaining in Q2 or Q4.
        # Clamp so time lands exactly at 2:00 rather than skipping past it in
        # one big runoff. Doesn't charge either team a timeout.
        full_seconds = np.zeros(self.N, dtype=np.int32)
        full_seconds[mask] = seconds
        prospective = self.time_remaining - full_seconds
        crosses_warning = (
            mask &
            np.isin(self.quarter, [2, 4]) &
            ~self.two_minute_warning_used &
            (self.time_remaining > 120) &
            (prospective <= 120)
        )
        if np.any(crosses_warning):
            self.time_remaining[crosses_warning] = 120
            self.clock_stopped[crosses_warning] = True
            self.two_minute_warning_used[crosses_warning] = True
            mask = mask & ~crosses_warning

        self.time_remaining[mask] -= full_seconds[mask]
        # Quarter/OT-period transitions (including the OT-entry coinflip) are
        # no longer handled here -- see _resolve_quarter_transitions, called
        # once per simulate_play_step after every clock call for that step is
        # done, instead of re-running (and re-drawing its own randomness) once
        # per clock-consuming category.

    def _resolve_quarter_transitions(self, active):
        """Quarter/OT-period boundary handling, including the OT-entry
        coinflip -- called exactly once per simulate_play_step, after every
        `_run_clock` call for that step has already applied its runoff, so
        this fires on the final `self.time_remaining` for every lane at once
        instead of once per clock-consuming category. A lane can only be
        <=0 here as a direct result of THIS step's runoff -- every branch
        below resets it to a positive value or ends the game, so nothing
        carries a stale <=0 into the next step. `active` should be
        `self.step_start_active` (every lane in play at the top of the step,
        before kneel/spike/kickoff/punt/FG narrow it further) so a lane that
        crossed zero via one of those early branches isn't missed.
        """
        # Bug fix (found via manual play-by-play review, clock_physics_v020):
        # `pre_quarter` must be captured before any mutation below. The old
        # code read self.quarter a second time (for `end_game`) AFTER
        # already incrementing it for lanes moving Q3->Q4, so those lanes
        # satisfied `quarter >= 4` immediately and the game was marked over
        # the instant Q4 began — meaning Q4 was never actually simulated.
        end_qtr = active & (self.time_remaining <= 0)
        if not np.any(end_qtr):
            return

        pre_quarter = self.quarter.copy()

        # Move to next quarter
        next_qtr = end_qtr & (pre_quarter < 4)
        self.quarter[next_qtr] += 1
        self.time_remaining[next_qtr] = 900
        self.two_minute_warning_used[next_qtr] = False

        # Second Half Kickoff Logic
        #
        # Bug fix (clock_physics_v020, Round 13 — found via Cam spotting
        # repeated identical down/distance/yardline lines in a play-by-play,
        # confirmed via scripts/audit_play_continuity.py: 94/95 sampled
        # violations landed exactly here, all at yardline_100==70). This
        # block runs AFTER the current play's real yardage/down/distance have
        # already been applied this same step. Directly overwriting
        # yardline_100/down/distance here discarded that just-resolved play's
        # real result every single game, right at the Q2->Q3 boundary. It was
        # also redundant: the kickoff-resolution code (`ko_mask = active &
        # self.needs_kickoff`, top of simulate_play_step) already computes
        # the correct down/distance/yardline_100 on its own next call
        # (touchback vs. return), exactly like it does for touchdown/field-
        # goal kickoffs — it doesn't need or expect this block to pre-set
        # them. Fix: only flip possession (nothing else does, unlike a score,
        # which flips it via _switch_possession) and defer to the kickoff,
        # same pattern already used for every other needs_kickoff trigger.
        q3_mask = next_qtr & (self.quarter == 3)
        if np.any(q3_mask):
            # If possession already changed hands on this exact play (a
            # walk-off-the-half score, a turnover on downs, an
            # interception, a lost fumble — anything that already called
            # _switch_possession earlier this same step) that's already
            # the correct team to have the ball; don't override it with a
            # blanket "home team receives". Only force it for lanes whose
            # possession is unchanged from the start of this step.
            needs_forced_receive = q3_mask & (self.possession_is_away == self.step_start_possession_is_away)
            self.possession_is_away[needs_forced_receive] = False  # Home team receives (meaning possession)
            self.needs_kickoff[q3_mask] = True
            self.clock_stopped[q3_mask] = True

        # Overtime. reg_end/ot_end partition the pre_quarter>=4 remainder
        # of end_qtr (pre_quarter<4 is the ordinary quarter-to-quarter
        # transition already handled by next_qtr above -- no scoring/tie
        # logic applies there). A fresh 50/50 random draw decides who
        # receives each new OT period's kickoff -- no coin-toss logic
        # exists anywhere else in this engine (the game always has away
        # receive the opening kickoff, deterministically), so this is a
        # new, deliberately-random mechanic, not a continuation of
        # whoever had the ball when the period's clock expired.
        reg_end = end_qtr & (pre_quarter == 4)
        ot_end = end_qtr & (pre_quarter >= 5)
        is_tied = self.score_away == self.score_home
        reg_tied = reg_end & is_tied
        ot_tied = ot_end & is_tied

        if np.any(reg_tied):
            self.quarter[reg_tied] = 5
            self.time_remaining[reg_tied] = 900 if self.is_playoff else 600
            self.timeouts_away[reg_tied] = 3 if self.is_playoff else 2
            self.timeouts_home[reg_tied] = 3 if self.is_playoff else 2
            self.ot_away_possessed[reg_tied] = False
            self.ot_home_possessed[reg_tied] = False
            self.two_minute_warning_used[reg_tied] = False
            receives_away = np.random.rand(self.N) < 0.5
            self.possession_is_away[reg_tied] = receives_away[reg_tied]
            self.needs_kickoff[reg_tied] = True
            self.clock_stopped[reg_tied] = True

        # Playoffs only: still tied at the end of an OT period -> another
        # period, fresh kickoff. Timeouts reset to 3 only when entering a
        # new "half" (every 2 OT periods -- 5+6, 7+8, ...), matching
        # regulation's half structure, i.e. only on odd new quarters.
        ot_tied_playoff = ot_tied & self.is_playoff
        if np.any(ot_tied_playoff):
            self.quarter[ot_tied_playoff] += 1
            self.time_remaining[ot_tied_playoff] = 900
            self.ot_away_possessed[ot_tied_playoff] = False
            self.ot_home_possessed[ot_tied_playoff] = False
            self.two_minute_warning_used[ot_tied_playoff] = False
            new_half = ot_tied_playoff & (self.quarter % 2 == 1)
            self.timeouts_away[new_half] = 3
            self.timeouts_home[new_half] = 3
            receives_away = np.random.rand(self.N) < 0.5
            self.possession_is_away[ot_tied_playoff] = receives_away[ot_tied_playoff]
            self.needs_kickoff[ot_tied_playoff] = True
            self.clock_stopped[ot_tied_playoff] = True

        # Regular season only: an OT period (there's only ever one) ended
        # still tied -> the game ends in a tie.
        #
        # BUG FIX: this line previously read `ot_tied & ~self.is_playoff`.
        # `self.is_playoff` is a plain Python bool (one matchup simulated
        # N times, not a per-lane array) -- Python's `~` on a bool is
        # BITWISE negation of its int value (~True == -2, ~False == -1),
        # not logical negation. ANDing a numpy bool array with that
        # integer silently upcasts the result to int64. Since the array's
        # values still happened to be numerically correct (all 0s when
        # they should be all-False), `self.game_over[ot_tied_regseason] =
        # True` stopped being boolean-mask indexing and became INTEGER
        # fancy-indexing instead -- numpy read the int64 zeros as literal
        # index positions, not a mask, and wrote True to position 0 every
        # single time this branch ran, regardless of lane 0's actual
        # state or is_playoff's value. Confirmed via direct instrumentation
        # (traced the exact game_over[0] flip to this line). Fixed by
        # using `not self.is_playoff` (proper scalar boolean negation,
        # stays a plain bool, no upcast) instead of `~`.
        ot_tied_regseason = ot_tied & (not self.is_playoff)
        self.game_over[ot_tied_regseason] = True

        # Game Over — regulation ended untied, or any OT period (reg-
        # season's single period, or a playoff period) ended untied. Time
        # simply running out ends the game for the leading team the
        # instant it happens, regardless of how many possessions either
        # team has had -- the "guaranteed possession" rule only prevents
        # an early end while time still remains, not extra time once the
        # clock's already at zero (see the OT resolution check in
        # simulate_play_step, which handles the "still remains" case).
        # `is_tied` is a real numpy bool array here (not a Python scalar),
        # so `~is_tied` is fine -- numpy overloads `~` as logical NOT for
        # bool arrays specifically; the bug above only applies to `~` on
        # a plain Python bool.
        end_game = (reg_end | ot_end) & ~is_tied
        self.game_over[end_game] = True

    def _switch_possession(self, scored=False, mask=None):
        if mask is None:
            mask = np.ones(self.N, dtype=bool)
        if not np.any(mask): return

        # Overtime possession tracking: every reason a drive can end (score,
        # turnover, turnover on downs) funnels through this one method, so
        # this is the single correct place to mark a team's OT possession as
        # CONCLUDED -- not merely started. That distinction matters: the
        # "both teams get a guaranteed possession" rule means the second
        # team's full drive has to play out before a score-differential can
        # end the game, not just begin. `possession_is_away` still holds the
        # PRE-flip value here (who's about to lose the ball).
        in_ot = mask & (self.quarter >= 5)
        if np.any(in_ot):
            self.ot_away_possessed |= in_ot & self.possession_is_away
            self.ot_home_possessed |= in_ot & ~self.possession_is_away

        self.possession_is_away[mask] = ~self.possession_is_away[mask]
        self.yardline_100[mask] = np.where(scored, 70, 100 - self.yardline_100[mask])
        self.down[mask] = 1
        self.distance[mask] = 10

    def get_game_summaries(self) -> list:
        summaries = []
        for i in range(self.N):
            if self.score_away[i] > self.score_home[i]:
                winner = self.away_team
            elif self.score_home[i] > self.score_away[i]:
                winner = self.home_team
            else:
                winner = "TIE"
            summaries.append({
                'game_id': i,
                # Renamed from off_score/def_score (2026-07-21): this was
                # always a fixed away/home mapping, not real offense/defense
                # (which flips every possession change) -- see AGENTS.md's
                # off_score/def_score fragile-area note for the cross-path
                # naming trap this used to create with batch.py's legacy
                # worker and the unrelated (correctly-named) src/live/ usage.
                'away_score': int(self.score_away[i]),
                'home_score': int(self.score_home[i]),
                'total': int(self.score_away[i] + self.score_home[i]),
                'spread': int(self.score_away[i] - self.score_home[i]),
                'winner': winner,
                'total_plays': int(self.play_count[i]),
                'plays_over_20_yds': int(self.plays_over_20_yds[i]),
                'punts': int(self.punts_run[i]),
                'offensive_snaps': int(self.play_count[i]),
                'special_teams_snaps': int(self.punts_run[i] + self.fg_attempts_away[i] + self.fg_attempts_home[i] + self.kickoffs_run[i]),
                'presnap_penalty_snaps': int(self.presnap_penalty_snaps[i]),
                'total_snaps': int(self.play_count[i] + self.punts_run[i] + self.fg_attempts_away[i] + self.fg_attempts_home[i] + self.kickoffs_run[i] + self.presnap_penalty_snaps[i]),
                'penalties_accepted': int(self.penalties_accepted[i]),
                'fourth_down_decisions': [],
                'fg_attempts_details': [],
                'td_details': []
            })
        return summaries

    def get_player_stats_flat(self, player_to_slot) -> list:
        """Flattens all player stats vectorially for fast conversion to DataFrame."""
        player_dfs = []
        winner = np.where(
            self.score_away > self.score_home, self.away_team,
            np.where(self.score_home > self.score_away, self.home_team, "TIE")
        )
        
        for team in [self.away_team, self.home_team]:
            for player, stats_dict in self.player_stats[team].items():
                pos = stats_dict['Pos']
                slot = player_to_slot.get(team, {}).get(player, pos)
                
                # Check if this player recorded any actions across N games to save space/time (except Defense)
                rAtt = stats_dict['rAtt']
                pAtt = stats_dict['pAtt']
                rec = stats_dict['rec']
                if pos != 'DST' and not np.any(rAtt > 0) and not np.any(pAtt > 0) and not np.any(rec > 0):
                    continue
                
                pYds = stats_dict['pYds']
                rYds = stats_dict['rYds']
                recYds = stats_dict['recYds']
                pTD = stats_dict['pTD']
                rTD = stats_dict['rTD']
                recTD = stats_dict['recTD']
                intercepts = stats_dict['int']
                fumbles = stats_dict['fumbles']
                air_yards = stats_dict.get('air_yards', np.zeros(self.N, dtype=np.int32))

                def_sack = stats_dict.get('def_sack', np.zeros(self.N, dtype=np.int32))
                def_int = stats_dict.get('def_int', np.zeros(self.N, dtype=np.int32))
                def_fumble_rec = stats_dict.get('def_fumble_rec', np.zeros(self.N, dtype=np.int32))
                def_td = stats_dict.get('def_td', np.zeros(self.N, dtype=np.int32))
                pts_allowed = stats_dict.get('pts_allowed', np.zeros(self.N, dtype=np.int32))

                if pos == 'DST':
                    # DraftKings and FanDuel DST scores
                    # Points Allowed brackets: 0 (+10), 1-6 (+7), 7-13 (+4), 14-20 (+1), 21-27 (0), 28-34 (-1), 35+ (-4)
                    dk_pts_allowed_bonus = np.zeros(self.N, dtype=np.float32)
                    dk_pts_allowed_bonus[pts_allowed == 0] = 10.0
                    dk_pts_allowed_bonus[(pts_allowed >= 1) & (pts_allowed <= 6)] = 7.0
                    dk_pts_allowed_bonus[(pts_allowed >= 7) & (pts_allowed <= 13)] = 4.0
                    dk_pts_allowed_bonus[(pts_allowed >= 14) & (pts_allowed <= 20)] = 1.0
                    dk_pts_allowed_bonus[(pts_allowed >= 21) & (pts_allowed <= 27)] = 0.0
                    dk_pts_allowed_bonus[(pts_allowed >= 28) & (pts_allowed <= 34)] = -1.0
                    dk_pts_allowed_bonus[pts_allowed >= 35] = -4.0
                    
                    dk_score = def_sack * 1.0 + def_int * 2.0 + def_fumble_rec * 2.0 + def_td * 6.0 + dk_pts_allowed_bonus
                    fd_score = dk_score # FanDuel shares the same standard bracket points
                    std_score = dk_score # No separate "standard" DST bracket convention -- reuse the DK/FD bracket.
                else:
                    dk_score = (
                        pYds * 0.04 + pTD * 4.0 - intercepts * 1.0 +
                        rYds * 0.1 + rTD * 6.0 +
                        rec * 1.0 + recYds * 0.1 + recTD * 6.0 -
                        fumbles * 1.0 +
                        np.where(pYds >= 300, 3.0, 0.0) +
                        np.where(rYds >= 100, 3.0, 0.0) +
                        np.where(recYds >= 100, 3.0, 0.0)
                    )

                    fd_score = (
                        pYds * 0.04 + pTD * 4.0 - intercepts * 2.0 +
                        rYds * 0.1 + rTD * 6.0 +
                        rec * 0.5 + recYds * 0.1 + recTD * 6.0 -
                        fumbles * 2.0
                    )

                    # "Standard" league scoring (Cam's request, 2026-08-17):
                    # half-PPR, 4pt passing TD, no yardage bonuses. Same
                    # shape as fd_score above (half-PPR + no bonuses already),
                    # kept as its own field rather than aliased to fd_score
                    # so it stays correct if FanDuel's own formula ever
                    # changes independently of what "standard" scoring means.
                    std_score = (
                        pYds * 0.04 + pTD * 4.0 - intercepts * 2.0 +
                        rYds * 0.1 + rTD * 6.0 +
                        rec * 0.5 + recYds * 0.1 + recTD * 6.0 -
                        fumbles * 2.0
                    )
                
                df_player = pd.DataFrame({
                    'Player': player,
                    'Team': team,
                    'Pos': pos,
                    'Slot': slot,
                    'game_id': np.arange(self.N),
                    'winner': winner,
                    'rAtt': rAtt.astype(np.int32),
                    'rYds': rYds.astype(np.int32),
                    'rTD': rTD.astype(np.int32),
                    'pAtt': pAtt.astype(np.int32),
                    'pCmp': stats_dict['pCmp'].astype(np.int32),
                    'pYds': pYds.astype(np.int32),
                    'pTD': pTD.astype(np.int32),
                    'int': intercepts.astype(np.int32),
                    'rec': rec.astype(np.int32),
                    'recYds': recYds.astype(np.int32),
                    'recTD': recTD.astype(np.int32),
                    'targets': stats_dict['targets'].astype(np.int32),
                    'fumbles': fumbles.astype(np.int32),
                    'fumbles_lost': stats_dict['fumbles_lost'].astype(np.int32),
                    'sacks_taken': stats_dict['sacks_taken'].astype(np.int32),
                    'air_yards': air_yards.astype(np.int32),
                    'def_sack': def_sack.astype(np.int32),
                    'def_int': def_int.astype(np.int32),
                    'def_fumble_rec': def_fumble_rec.astype(np.int32),
                    'def_td': def_td.astype(np.int32),
                    'pts_allowed': pts_allowed.astype(np.int32),
                    'dk_score': np.round(dk_score, 2),
                    'fd_score': np.round(fd_score, 2),
                    'std_score': np.round(std_score, 2),
                    'touches': (rAtt + rec).astype(np.int32)
                })
                player_dfs.append(df_player)
                
        if not player_dfs:
            return []
        
        combined_df = pd.concat(player_dfs, ignore_index=True)
        return combined_df.to_dict(orient='records')

    def _predict_4th_down_probas_batch(self, yardline_100, distance, game_sec, score_diff):
        N_plays = len(yardline_100)
        probas = np.zeros((N_plays, 3), dtype=np.float32) # [PUNT, FIELD_GOAL, GO]
        
        # 1. Extreme Desperation: Last 7 minutes, trailing by 17+ or trailing by 9+ and <= 5 mins
        cond_extreme = (game_sec <= 420) & ((score_diff <= -17) | ((score_diff <= -9) & (game_sec <= 300)))
        # Or under 2 mins, trailing by 4 to 8
        cond_under2 = (game_sec < 120) & (score_diff >= -8) & (score_diff < -3)
        
        must_go_mask = cond_extreme | cond_under2
        probas[must_go_mask] = [0.0, 0.0, 1.0]
        
        # 2. Desperation at end of Q2/Q4:
        is_q2_end = (game_sec > 1800) & (game_sec <= 1830)
        is_q4_end = (game_sec > 0) & (game_sec <= 30)
        is_desperation_time = is_q2_end | (is_q4_end & (score_diff >= -3) & (score_diff <= 0))
        
        desp_mask = ~must_go_mask & is_desperation_time
        if np.any(desp_mask):
            fg_eligible = desp_mask & (yardline_100 <= 53)
            go_eligible = desp_mask & (yardline_100 > 53)
            probas[fg_eligible] = [0.0, 1.0, 0.0]
            probas[go_eligible] = [0.0, 0.0, 1.0]
            
        # 3. Normal situations
        normal_mask = ~must_go_mask & ~is_desperation_time
        if np.any(normal_mask):
            sub_yd = yardline_100[normal_mask]
            sub_dist = distance[normal_mask]
            sub_sec = game_sec[normal_mask]
            sub_diff = score_diff[normal_mask]
            
            if self.registry.fg_model:
                fg_prob = self.registry.fg_model.predict_success_probability(sub_yd)
            else:
                fg_prob = np.full(len(sub_yd), 0.82, dtype=np.float32)
            
            fd_state = pd.DataFrame({
                'ydstogo': sub_dist.astype(float),
                'yardline_100': sub_yd.astype(float),
                'score_differential': sub_diff.astype(float),
                'game_seconds_remaining': sub_sec.astype(float)
            })
            fd_prob = self.registry.fd_conversion_model.predict_conversion_probability(fd_state)
            
            # Initialize weights
            punt_w = np.zeros(len(sub_yd), dtype=np.float32)
            fg_w = np.zeros(len(sub_yd), dtype=np.float32)
            go_w = np.zeros(len(sub_yd), dtype=np.float32)
            
            # Short yardage (dist <= 1)
            is_short = (sub_dist <= 1)
            # Short yardage inside own territory (yd > 50)
            cond_short_own = is_short & (sub_yd > 50)
            go_w[cond_short_own] = fd_prob[cond_short_own]
            punt_w[cond_short_own] = 1.5 - fd_prob[cond_short_own]
            
            # Short yardage inside opp territory (yd <= 50)
            cond_short_opp = is_short & (sub_yd <= 50)
            go_w[cond_short_opp] = fd_prob[cond_short_opp] * 2.0
            fg_w[cond_short_opp] = fg_prob[cond_short_opp] * 0.5
            
            # Long yardage (dist > 1)
            is_long = ~is_short
            
            # Own territory long yardage (yd > 48)
            cond_long_own = is_long & (sub_yd > 48)
            go_w[cond_long_own] = fd_prob[cond_long_own] * 0.1
            punt_w[cond_long_own] = 1.0 - (fd_prob[cond_long_own] * 0.1)
            
            # Opponent territory long yardage (yd <= 48)
            cond_long_opp = is_long & (sub_yd <= 48)
            
            # Inside 40
            cond_long_opp_40 = cond_long_opp & (sub_yd <= 40)
            fg_w[cond_long_opp_40] = fg_prob[cond_long_opp_40]
            go_w[cond_long_opp_40] = fd_prob[cond_long_opp_40] * 1.2
            
            # Between 40 and 48
            cond_long_opp_40_48 = cond_long_opp & (sub_yd > 40)
            punt_w[cond_long_opp_40_48] = 0.3 * (sub_dist[cond_long_opp_40_48] / 10.0)
            fg_w[cond_long_opp_40_48] = fg_prob[cond_long_opp_40_48]
            go_w[cond_long_opp_40_48] = fd_prob[cond_long_opp_40_48]
            
            # Normalize weights
            total_w = punt_w + fg_w + go_w
            # Fallback for any invalid weights
            invalid = (total_w <= 0.0)
            punt_w[invalid] = 0.95
            go_w[invalid] = 0.05
            total_w[invalid] = 1.0
            
            probas[normal_mask, 0] = punt_w / total_w
            probas[normal_mask, 1] = fg_w / total_w
            probas[normal_mask, 2] = go_w / total_w
            
        return probas
