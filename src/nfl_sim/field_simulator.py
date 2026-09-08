"""DFS tournament field simulation engine (V1).

See docs/implementation_plans/field_simulation_implementation_plan.md for
the full design rationale. Summary: rather than a uniform-random field of
valid-but-implausible lineups (the old approach in app.py's /api/optimize),
this builds a field sampled from four tunable player archetypes (sharp,
fake_sharp, casual, toilet) so ownership, leverage, and portfolio EV all
come from a field that actually looks like a real contest's entrants.

Ownership bootstrapping: archetype construction needs *some* ownership
signal to seed leverage-fading and stacking decisions, but real ownership
is only knowable once the field itself is built (chicken-and-egg). This
module resolves that with one bootstrap pass: callers pass in a `prior_own`
dict (today, this is app.py's existing salary/Vegas/cash-consensus
_compute_ownership() softmax model) to seed construction, and the FINAL,
displayed ownership_pct is the empirical frequency of each player across
the built field sample -- naturally bounded [0, 100] by construction, no
clamping needed. See build_field_sample()'s docstring.
"""

import json
import os
import random
from typing import Any, Dict, List, Optional, Tuple

import numpy as np

BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
ARCHETYPE_PARAMS_PATH = os.path.join(BASE_DIR, "data", "dna", "field_archetype_params.json")

# Same position weighting as app.py's _compute_ownership() -- kept in sync
# deliberately, since both are modeling "how much does a raw value ratio
# matter at this position" for the same underlying reason.
POS_WEIGHTS = {'QB': 0.85, 'RB': 1.30, 'WR': 1.00, 'TE': 0.90, 'DST': 0.65}

_DEFAULT_PARAMS: Dict[str, Any] = {
    "archetype_shares": {"sharp": 0.10, "fake_sharp": 0.38, "casual": 0.30, "toilet": 0.22},
    "sharp": {"leverage_weight": 0.7, "stacking_threshold_pctile": 0.7, "stack_bonus": 1.6,
              "mistake_p": 0.075, "mistake_signal_distortion": [0.85, 1.15], "additive_own_target": 130.0},
    "fake_sharp": {"leverage_weight": 0.6, "stacking_threshold_pctile": 0.4, "stack_bonus": 1.4,
                   "mistake_p": 0.45, "pet_misread_count": [1, 2], "pet_misread_distortion": [0.4, 2.2],
                   "additive_own_target": 130.0, "additive_own_overshoot_range": [10, 15]},
    "casual": {"leverage_weight": 0.15, "stacking_threshold_pctile": 0.85, "stack_bonus": 1.15,
               "naive_stack_p": 0.20, "recency_bias_weight": 0.4, "name_bias_weight": 0.3,
               "additive_own_target": 150.0},
    "toilet": {"leverage_weight": 0.0, "name_bias_weight": 0.85, "additive_own_target": 999.0},
}


def load_archetype_params() -> Dict[str, Any]:
    """Reads data/dna/field_archetype_params.json -- the tunable archetype
    config (shares, leverage weights, mistake frequencies, etc; see that
    file's _comment). Falls back to an in-code default (identical values,
    kept in sync by hand) if the file is missing or malformed, so a bad
    edit to the JSON degrades to "known-good defaults" rather than a
    500 -- same fail-soft philosophy as dk_scraper.py.
    """
    if os.path.exists(ARCHETYPE_PARAMS_PATH):
        try:
            with open(ARCHETYPE_PARAMS_PATH, "r") as f:
                return json.load(f)
        except (json.JSONDecodeError, OSError) as e:
            print(f"field_simulator: failed to load {ARCHETYPE_PARAMS_PATH}, using defaults: {e}")
    return _DEFAULT_PARAMS


def _team_environment_scores(players: List[Dict[str, Any]]) -> Dict[str, float]:
    """Proxy for 'how good is this game environment to stack' -- sums each
    team's priced players' own median projections. Not real Vegas data
    (OptimizeRequest doesn't carry a week/schedule reference today, so the
    real implied-team-total used elsewhere in get_week_sim_results() isn't
    available here) -- a cruder but self-contained stand-in: a team whose
    priced players project well collectively is, in aggregate, in a better
    game environment than one that doesn't. Good enough to rank *which*
    games are the slate's best stacking candidates; not meant to be a
    precise point total. Upgrading this to real Vegas implied totals is a
    follow-up once OptimizeRequest carries a week reference.
    """
    totals: Dict[str, float] = {}
    for p in players:
        team = p.get('team')
        if not team:
            continue
        totals[team] = totals.get(team, 0.0) + (p.get('projection') or 0.0)
    return totals


SELECTION_TEMPERATURE = 0.55  # in Z-SCORE units (see _softmax_probs) -- not raw point units


def _softmax_probs(weights: np.ndarray, temperature: float = SELECTION_TEMPERATURE) -> np.ndarray:
    """Turns raw per-player selection weights into a peaked probability
    distribution. Using the raw weights directly (normalized to sum to 1)
    under-concentrates -- real DFS ownership piles hard onto a handful of
    the best plays, which a linearly-weighted random choice can't
    reproduce if the best player is only modestly better by raw value.

    Standardized to Z-scores (mean 0, std 1) *before* exponentiating,
    rather than exponentiating the raw weights directly -- weight_for()'s
    output scale isn't fixed (it changed from a ~1-10 value-ratio range to
    a ~5-45 raw-projection range during this module's own development, and
    could change again), so a fixed temperature tuned for one scale
    silently over- or under-concentrates if that scale shifts. Z-scoring
    first means `temperature` always means the same thing -- "how many
    standard deviations of edge does it take to meaningfully change
    selection odds" -- independent of whatever units weight_for() happens
    to produce.
    """
    std = weights.std()
    z = (weights - weights.mean()) / std if std > 1e-9 else np.zeros_like(weights)
    exp_w = np.exp(z / temperature)
    total = exp_w.sum()
    if total <= 0 or not np.isfinite(total):
        return np.full(len(weights), 1.0 / len(weights))
    return exp_w / total


def _percentile_rank(value: float, sorted_values: np.ndarray) -> float:
    """0-1 percentile rank of `value` within `sorted_values` (already sorted
    ascending). Used to turn team_environment_scores into 'is this game in
    the top X% of the slate,' which is what stacking_threshold_pctile
    actually compares against."""
    if len(sorted_values) == 0:
        return 0.5
    return float(np.searchsorted(sorted_values, value) / max(1, len(sorted_values) - 1))


def _archetype_signal(
    p: Dict[str, Any], archetype: str, arch_params: Dict[str, Any], rng: np.random.Generator,
) -> float:
    """This archetype's perceived value (fantasy points) for player `p`.
    Sharp and fake_sharp start from the true simulated projection (their
    mistake, if any, is applied separately in build_field_lineup() since it
    needs cross-player context -- see _pick_pet_targets()). Casual and
    toilet are distorted here since their bias is a simple per-player
    function, not a cross-player selection.
    """
    proj = p.get('projection') or 0.0
    if archetype == 'toilet':
        # Name/salary-driven, barely related to true value -- salary itself
        # is the best fame proxy already in the data (expensive players are
        # usually the recognizable ones), so lean on it almost entirely.
        salary = max(p.get('salary') or 3000, 1)
        return salary / 200.0
    if archetype == 'casual':
        params = arch_params.get('casual', {})
        name_w = params.get('name_bias_weight', 0.3)
        salary_component = max(p.get('salary') or 3000, 1) / 200.0
        # Recency bias placeholder: we don't have a clean 'last week's
        # actual box score' feed wired into this module yet (and for a
        # week-1 slate there isn't one to have), so this is approximated as
        # extra per-player noise rather than a genuine recency signal --
        # flagged here as a follow-up once real recent-actuals tracking
        # exists, per the implementation plan's deferred items.
        recency_sigma = params.get('recency_bias_weight', 0.4) * 0.15
        recency_noise = float(np.exp(rng.normal(0, recency_sigma)))
        return proj * (1 - name_w) * recency_noise + salary_component * name_w
    return proj  # sharp, fake_sharp (pre-mistake)


def _pick_pet_targets(
    pool: List[Dict[str, Any]], arch_params: Dict[str, Any], rng: np.random.Generator,
) -> List[str]:
    """For a fake_sharp entrant that rolled a mistake this week: choose
    which 1-2 players get misread, weighted toward high-uncertainty roles
    (wide P25-P95 spread -- new starters, committees, injury-replacement
    situations) rather than picked uniformly at random. This is the
    concrete proxy for 'ambiguous situations are where a real misread
    happens' agreed in the field-sim design discussion -- we don't have an
    external consensus feed (e.g. an ETR-quality projection) to diff
    against, but distribution width is a real, already-computed signal for
    genuine ambiguity.

    Returns a list of player names (not (name,team) tuples -- collisions
    across teams are rare enough for this to be an acceptable simplification
    in v1, and the caller only uses this to look up entries within the same
    single-lineup `pool`).
    """
    params = arch_params.get('fake_sharp', {})
    spreads = []
    for p in pool:
        pcts = p.get('dk_pcts_all')
        if pcts and len(pcts) == 101:
            spread = max(0.0, pcts[95] - pcts[25])
        else:
            spread = max(p.get('projection') or 0.0, 0.0) * 0.5  # fallback: assume moderate spread
        spreads.append(spread)
    spreads_arr = np.array(spreads, dtype=float)
    total = spreads_arr.sum()
    if total <= 0:
        return []
    weights = spreads_arr / total
    lo, hi = params.get('pet_misread_count', [1, 2])
    n_targets = int(rng.integers(lo, hi + 1))
    n_targets = min(n_targets, len(pool))
    chosen_idx = rng.choice(len(pool), size=n_targets, replace=False, p=weights)
    return [pool[i]['name'] for i in chosen_idx]


def build_field_lineup(
    players_by_pos: Dict[str, List[Dict[str, Any]]],
    archetype: str,
    arch_params: Dict[str, Any],
    prior_own: Dict[Tuple[str, str], float],
    avg_own: float,
    team_env_pctile: Dict[str, float],
    salary_cap: int,
    rng: np.random.Generator,
) -> Optional[List[Dict[str, Any]]]:
    """Constructs one valid 9-player DK-format field lineup (1 QB, 2 RB,
    3 WR, 1 TE, 1 FLEX, 1 DST) for the given archetype, using ONE shared
    stochastic builder parameterized per-archetype rather than four
    separate algorithms (see the implementation plan's "one construction
    pipeline" framing).

    Selection is weighted position-by-position (not a single global ILP
    solve) -- field lineups need to be *plausible*, not optimal, so a
    greedy weighted-random build respecting the salary cap is both
    sufficient and cheap enough to run thousands of times per field sample.

    `prior_own` is the bootstrap ownership prior (see module docstring) --
    used here to fade chalk (leverage_weight) and to steer each lineup's
    additive ownership toward the archetype's target/overshoot band.
    `team_env_pctile` is this player's team's stacking-environment
    percentile (see _team_environment_scores/_percentile_rank) -- what
    stacking_threshold_pctile compares against.

    Returns None if the position pools can't fill a valid lineup (e.g. an
    empty slate) -- caller should treat that as "skip this draw," not fail.
    """
    params = arch_params.get(archetype, {})
    leverage_k = params.get('leverage_weight', 0.0)
    stack_threshold = params.get('stacking_threshold_pctile')
    stack_bonus = params.get('stack_bonus', 1.0)
    own_target = params.get('additive_own_target', 130.0)

    # Fake-sharp mistake roll -- happens once per lineup (per "entrant"),
    # not per player. When it hits: 1-2 pet players get a large, randomly-
    # directed misread, and the additive-ownership target is allowed to
    # overshoot (the hype-chasing stand-in).
    mistake_rolled = False
    pet_targets: List[str] = []
    pet_distortions: Dict[str, float] = {}
    if archetype == 'fake_sharp':
        if rng.random() < params.get('mistake_p', 0.0):
            mistake_rolled = True
            all_priced = [p for pool in players_by_pos.values() for p in pool]
            pet_targets = _pick_pet_targets(all_priced, arch_params, rng)
            lo, hi = params.get('pet_misread_distortion', [0.4, 2.2])
            for name in pet_targets:
                pet_distortions[name] = float(rng.uniform(lo, hi))
            lo_over, hi_over = params.get('additive_own_overshoot_range', [10, 15])
            own_target = own_target + rng.uniform(lo_over, hi_over)

    # Casual naive-stack roll -- a much weaker, simpler version of sharp
    # stacking: just "try a QB+WR1 same-team pairing sometimes," no
    # environment validation.
    attempt_naive_stack = archetype == 'casual' and rng.random() < params.get('naive_stack_p', 0.0)

    used: set = set()
    lineup: List[Dict[str, Any]] = []
    remaining_salary = salary_cap
    additive_own = 0.0
    stack_team: Optional[str] = None

    def player_key(p):
        return (p['name'], p['team'])

    def weight_for(p: Dict[str, Any]) -> float:
        signal = _archetype_signal(p, archetype, arch_params, rng)
        if p['name'] in pet_distortions:
            signal *= pet_distortions[p['name']]
        # Raw projection, NOT points-per-dollar -- a real field entrant
        # rosters the best player they can still afford at each spot, not
        # the best value ratio (that's a good signal for _compute_ownership()
        # comparing players in isolation, but greedily ratio-optimizing a
        # whole lineup here produced a team of min-salary punts leaving
        # most of the cap unspent, projecting barely half of a realistic
        # DK build -- verified empirically before this fix). Salary cap
        # pressure alone (can't afford everything) is what should push
        # construction toward a realistic stars-and-scrubs/balanced spend.
        w = signal * POS_WEIGHTS.get(p['pos'], 1.0)

        if leverage_k > 0:
            own = prior_own.get(player_key(p), avg_own)
            w *= (avg_own / max(own, 0.5)) ** leverage_k

        # Stacking bonus: sharp/fake_sharp only stack a team once its
        # environment score clears the archetype's own validation bar
        # (see module docstring on this being a proxy, not real Vegas
        # data); casual's "naive stack" ignores environment entirely.
        if stack_team is not None and p['team'] == stack_team:
            if stack_threshold is not None:
                if team_env_pctile.get(stack_team, 0.0) >= stack_threshold:
                    w *= stack_bonus
            elif attempt_naive_stack:
                w *= stack_bonus

        # Additive-ownership steering: a soft nudge toward the archetype's
        # target/overshoot band, not a hard constraint (avoids expensive
        # rejection sampling on a 9-pick greedy build).
        own = prior_own.get(player_key(p), avg_own)
        projected_total = additive_own + own
        if projected_total > own_target * 1.15:
            w *= 0.5
        elif archetype == 'fake_sharp' and mistake_rolled and projected_total < own_target:
            w *= 1.3

        return max(w, 1e-6)

    def pick_pos(pos: str, count: int) -> bool:
        nonlocal remaining_salary, additive_own, stack_team
        for _ in range(count):
            pool = [p for p in players_by_pos.get(pos, []) if p['name'] not in used and (p.get('salary') or 0) <= remaining_salary]
            if not pool:
                pool = [p for p in players_by_pos.get(pos, []) if p['name'] not in used]
            if not pool:
                return False
            weights = np.array([weight_for(p) for p in pool], dtype=float)
            weights = _softmax_probs(weights)
            idx = rng.choice(len(pool), p=weights)
            player = pool[idx]
            lineup.append(player)
            used.add(player['name'])
            remaining_salary -= (player.get('salary') or 0)
            additive_own += prior_own.get(player_key(player), avg_own)
            if pos == 'QB':
                stack_team = player['team']
        return True

    for pos, count in [('QB', 1), ('RB', 2), ('WR', 3), ('TE', 1), ('DST', 1)]:
        if not pick_pos(pos, count):
            return None

    # FLEX: any remaining RB/WR/TE
    flex_pool = [p for pos in ('RB', 'WR', 'TE') for p in players_by_pos.get(pos, []) if p['name'] not in used and (p.get('salary') or 0) <= remaining_salary]
    if not flex_pool:
        flex_pool = [p for pos in ('RB', 'WR', 'TE') for p in players_by_pos.get(pos, []) if p['name'] not in used]
    if not flex_pool:
        return None
    weights = np.array([weight_for(p) for p in flex_pool], dtype=float)
    weights = _softmax_probs(weights)
    flex = flex_pool[rng.choice(len(flex_pool), p=weights)]
    lineup.append(flex)

    return lineup


def build_field_sample(
    players: List[Dict[str, Any]],
    prior_own: Dict[Tuple[str, str], float],
    salary_cap: int,
    K: int = 1000,
    arch_params: Optional[Dict[str, Any]] = None,
    seed: Optional[int] = None,
) -> Dict[str, Any]:
    """Builds a K-lineup field sample composed of the four archetypes per
    `archetype_shares`, and derives final ownership as each player's
    observed frequency across that field -- structurally bounded [0, 100]
    since it's a literal count/K, not a softmax share of an arbitrary
    budget (see module docstring on why this replaces the old softmax
    ownership model as the *displayed* number, while that softmax model
    becomes just the bootstrap prior feeding construction here).

    Deterministic when `seed` is given -- same seed/inputs always
    reproduce the same field, same reason ownership/EV need to be seeded
    elsewhere in this app: comparable run-to-run, not fresh noise on every
    build.

    Returns {
        'lineups': [[9 player dicts], ...],
        'archetype_counts': {archetype: n_built},
        'ownership_pct': {(name, team): pct},
        'k_built': int,  # may be < K if the pool ran out of valid combinations
    }
    """
    if arch_params is None:
        arch_params = load_archetype_params()
    rng = np.random.default_rng(seed)

    players_by_pos: Dict[str, List[Dict[str, Any]]] = {'QB': [], 'RB': [], 'WR': [], 'TE': [], 'DST': []}
    for p in players:
        pos = p.get('pos')
        if pos in players_by_pos and p.get('salary') is not None:
            players_by_pos[pos].append(p)

    team_env_scores = _team_environment_scores(players)
    sorted_env = np.array(sorted(team_env_scores.values()))
    team_env_pctile = {team: _percentile_rank(score, sorted_env) for team, score in team_env_scores.items()}

    n_priced = sum(len(v) for v in players_by_pos.values())
    avg_own = (900.0 / max(n_priced, 1)) if n_priced else 5.0

    shares = arch_params.get('archetype_shares', _DEFAULT_PARAMS['archetype_shares'])
    archetype_counts = {a: int(round(K * s)) for a, s in shares.items()}

    lineups: List[List[Dict[str, Any]]] = []
    built_counts = {a: 0 for a in shares}
    appearance_counts: Dict[Tuple[str, str], int] = {}

    for archetype, n_target in archetype_counts.items():
        attempts = 0
        max_attempts = n_target * 3 + 10
        while built_counts[archetype] < n_target and attempts < max_attempts:
            attempts += 1
            lu = build_field_lineup(
                players_by_pos, archetype, arch_params, prior_own, avg_own,
                team_env_pctile, salary_cap, rng,
            )
            if lu is None:
                continue
            lineups.append(lu)
            built_counts[archetype] += 1
            for p in lu:
                key = (p['name'], p['team'])
                appearance_counts[key] = appearance_counts.get(key, 0) + 1

    k_built = len(lineups)
    ownership_pct = {
        key: round(100.0 * count / k_built, 1) if k_built else 0.0
        for key, count in appearance_counts.items()
    }

    return {
        'lineups': lineups,
        'archetype_counts': built_counts,
        'ownership_pct': ownership_pct,
        'k_built': k_built,
    }


def score_field_at_iteration(
    lineups: List[List[Dict[str, Any]]],
    trial_scores_map: Dict[Tuple[str, str, str], np.ndarray],
    iter_idx: int,
) -> np.ndarray:
    """Scores every field lineup using ONE specific per-player-correlated
    iteration index -- the same aligned-iteration mechanism already used
    for our own lineup's scoring elsewhere in app.py, so a shared game
    environment (a shootout, a blowout) lifts our players and the field's
    players together rather than independently. `trial_scores_map` is
    keyed exactly like app.py's existing map: (name, team, pos) -> 1000
    real per-iteration dk_score values.
    """
    scores = np.zeros(len(lineups))
    for i, lu in enumerate(lineups):
        total = 0.0
        for p in lu:
            key = (p['name'], p['team'], p['pos'])
            arr = trial_scores_map.get(key)
            if arr is not None and len(arr) > iter_idx:
                total += float(arr[iter_idx])
            else:
                proj = p.get('projection') or 10.0
                total += max(0.0, proj)  # fallback: no per-iteration noise for an unmapped player
        scores[i] = total
    return scores
