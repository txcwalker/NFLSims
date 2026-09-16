"""The hand-tuned V1 ownership heuristic -- value core + softmax + bounded
nudges (salary-rank, Vegas, cash-consensus / optimal-CPT-FLEX) -- used both
live (src/api/app.py's /optimize and /showdown_prep endpoints) and offline
(scripts/dfs_ownership/calibrate_ownership_model.py,
scripts/dfs_ownership/train_ownership_model.py). Moved out of app.py so
offline scripts can import it directly without pulling in FastAPI or
triggering the app's startup-time caches/scraper calls -- these three
functions are pure numpy, no app-level state.

Per data/dfs_ownership/README.md's "Model design" section, this is the prior
and the baseline a trained per-bucket GBM is meant to eventually beat, not
the final word -- see train_ownership_model.py.
"""

from typing import Dict, Optional

import numpy as np

from src.ownership.normalize import normalize_classic_ownership, normalize_showdown_ownership


def _ownership_soft_cap(raw: Optional[float], value: float, v_med: float, v_p90: float) -> Optional[float]:
    """Squash one ownership estimate toward a realistic large-field ceiling.

    The archetype field builder (and, less so, the softmax prior) over-roster the
    top value plays -- observed ownership for the chalkiest WR/RB comes out
    60-80%, but in a real large-field NFL contest even the single most-owned
    player rarely clears ~40%. Anything above 35% is compressed so a would-be
    100% play lands at `ceiling` while the ordering among chalk is preserved.

    `ceiling` only rises above the 42% base for a genuinely mispriced player --
    points-per-$1k well past the pool's 90th percentile, which in practice means
    a post-salary injury vaulted them into a much bigger role.

    Inputs: raw ownership %, the player's pts-per-$1k `value`, and the pool's
    median / 90th-pctile value. Output: the capped ownership % (unchanged at or
    below 35%, None passed through).
    """
    if raw is None or raw <= 35.0:
        return raw
    excess = max(0.0, (value - v_p90) / (v_p90 - v_med)) if v_p90 > v_med else 0.0
    ceiling = min(62.0, 42.0 + 12.0 * excess)
    squash = (ceiling - 35.0) / 65.0
    return round(35.0 + (raw - 35.0) * squash, 1)


def _compute_ownership(players: list, seed: Optional[int] = None) -> list:
    """Compute synthetic V1 ownership using two-factor softmax + log-normal
    noise. Mutates and returns `players`.

    Deterministic when `seed` is given -- the same seed (and same pool)
    always reproduces the same numbers. This matters because ownership
    feeds contest-EV/portfolio metrics (see handle_optimize's EV math) that
    need to be comparable run-to-run, not a fresh random draw every time
    someone reloads the page or re-optimizes. Omit `seed` for a fresh draw.

    A player who already carries a non-None ownership_pct (a manual
    override, e.g. typed into the Optimizer's editable Own% column) is left
    untouched -- only players missing one get a computed value, though
    everyone with a real salary still participates in the softmax's
    relative scoring so one override doesn't skew the rest of the pool.

    A player with no real DK salary (None -- off the Main Slate, or DK
    hasn't priced them) can't be scored against priced players on a
    $/point basis, so their ownership_pct is left as whatever it already
    was (None, unless manually overridden) rather than guessed.

    Three bounded multiplicative nudges sit on top of the original
    value/salary core, each addressing a specific known way our own
    projections diverge from the public consensus that actually drives
    real ownership (our sim isn't the field's sim):
      - salary-rank (within position): DK's own pricing team already
        encodes an industry-consensus view of a player's role/talent into
        salary -- independent of whatever our particular median says. A
        player priced high at their position tends to get public chalk
        almost regardless of our own view of them.
      - Vegas implied team total: the public chases shootout/favorite
        narratives directly off the same lines everyone sees -- this is
        public information our own model doesn't otherwise get credit for
        just by having a good/bad median projection.
      - cash-lineup consensus (`cash_consensus_frac`, 0-1, from
        _generate_cash_consensus_lineups): players who recur across this
        slate's own top cash-optimal builds are usually exactly who's
        "solved" industry-wide by Thursday and therefore heavily owned in
        tournaments too, on top of pure salary/value.
    Both `implied_total` and `cash_consensus_frac` are optional per player;
    missing values fall back to neutral (no nudge).
    """
    POS_WEIGHTS = {'QB': 0.85, 'RB': 1.30, 'WR': 1.00, 'TE': 0.90, 'DST': 0.65}

    # Captured before any computation -- a manual override is only
    # identifiable as "already non-None" right now, since every priced
    # player has a non-None ownership_pct by the end of this function.
    locked = {i for i, p in enumerate(players) if p.get('ownership_pct') is not None}

    priced = [p for p in players if p.get('salary') is not None]
    if not priced:
        return players

    # Salary percentile within position -- a $6,500 RB and a $6,500 WR
    # aren't equally "expensive" relative to their peers, so this has to
    # be ranked within each position group, not across the whole pool.
    salary_pctile: Dict[int, float] = {}
    by_pos: Dict[str, list] = {}
    for i, p in enumerate(priced):
        by_pos.setdefault(p['pos'], []).append(i)
    for idx_list in by_pos.values():
        sals = np.array([priced[i]['salary'] for i in idx_list], dtype=float)
        order = sals.argsort()
        ranks = np.empty(len(sals))
        ranks[order] = np.arange(len(sals))
        pct = ranks / max(1, len(sals) - 1) if len(sals) > 1 else np.array([0.5])
        for i, p_val in zip(idx_list, pct):
            salary_pctile[i] = float(p_val)

    # Vegas implied-team-total percentile across the whole priced pool
    # (this is a team-level, not position-level, stat).
    totals = [p['implied_total'] for p in priced if p.get('implied_total') is not None]
    if len(totals) >= 2:
        totals_arr = np.array(sorted(totals))
        vegas_pctile = {
            i: float(np.searchsorted(totals_arr, p['implied_total']) / (len(totals_arr) - 1))
            for i, p in enumerate(priced) if p.get('implied_total') is not None
        }
    else:
        vegas_pctile = {}

    for i, p in enumerate(priced):
        pw = POS_WEIGHTS.get(p['pos'], 1.0)
        sal_k = max(p['salary'], 1) / 1000.0
        score = (p['projection'] / sal_k) * pw
        score *= 0.85 + 0.30 * salary_pctile.get(i, 0.5)          # +/-15% salary-rank nudge
        score *= 0.90 + 0.20 * vegas_pctile.get(i, 0.5)           # +/-10% Vegas-environment nudge
        score *= 1.0 + 0.6 * min(1.0, max(0.0, p.get('cash_consensus_frac') or 0.0))  # up to +60% cash-consensus boost
        p['_value_score'] = score

    # Chalk boost: top 10% by value score get 1.4x
    scores = [p['_value_score'] for p in priced]
    threshold = np.percentile(scores, 90) if len(scores) >= 10 else max(scores)
    for p in priced:
        if p['_value_score'] >= threshold:
            p['_value_score'] *= 1.4

    # Softmax with temperature T=1.5, scaled to sum=900
    T = 1.5
    vals = np.array([p['_value_score'] for p in priced], dtype=float)
    vals = vals - vals.max()  # numerical stability
    exp_vals = np.exp(vals / T)
    softmax = exp_vals / (exp_vals.sum() + 1e-9)
    raw_ownership = softmax * 900.0

    # Pool value stats for the soft cap below.
    _vals = [p['projection'] / (max(p['salary'], 1) / 1000.0) for p in priced]
    _v_med, _v_p90 = float(np.median(_vals)), float(np.percentile(_vals, 90))

    # Log-normal noise
    rng = np.random.default_rng(seed)
    for i, p in enumerate(priced):
        del p['_value_score']
        if p.get('ownership_pct') is not None:
            continue  # manual override -- leave it alone
        base = raw_ownership[i]
        if base > 25:
            sigma = 0.25
        elif base > 8:
            sigma = 0.35
        else:
            sigma = 0.45
        noise = float(np.exp(rng.normal(0, sigma)))
        # The 900 scale is a pool-wide budget (9 roster slots x 100%), not a
        # per-player one. Floor at 0.5%, then soft-cap the top toward a realistic
        # large-field ceiling (~42%, higher only for genuine value outliers) --
        # see _ownership_soft_cap.
        raw = max(0.5, round(base * noise, 1))
        p['ownership_pct'] = max(0.5, _ownership_soft_cap(raw, _vals[i], _v_med, _v_p90))

    # The softmax above targets sum=900 pool-wide, but per-player noise and
    # the soft-cap both nudge individual values after that, so the total
    # drifts off both the pool-wide number and (more importantly) the real
    # DK roster-math targets per position group -- renormalize to those now.
    normalize_classic_ownership(players, locked)
    return players


def _compute_showdown_ownership(players: list, seed: Optional[int] = None) -> list:
    """Synthetic V1 showdown ownership -- CPT and FLEX modelled separately,
    mirroring the classic optimizer's _compute_ownership shape (value core +
    a "this slate's own optimal builds" chalk proxy + a Vegas nudge).

    Mutates each player, adding:
      - ownership_pct      : FLEX roster rate (pool sums to ~500 = 5 slots x 100%)
      - cpt_ownership_pct  : CPT roster rate (pool sums to ~100 = 1 slot x 100%)

    Manual overrides (a non-None value already on the player) are left
    alone but still participate in the softmax's relative scoring so one
    override doesn't distort the rest of the pool.

    Why the two are modelled apart: the FLEX field behaves like a
    compressed classic slate -- driven by points-per-dollar with the usual
    chalk pile-on. The CPT field does not: captains are chosen for ceiling
    and name recognition, so CPT ownership concentrates on far fewer players
    and barely tracks salary.

    Optional per-player signals (missing -> neutral), the showdown analogue
    of the classic model's cash-consensus + Vegas inputs:
      - optimal_cpt_pct / optimal_flex_pct (0-100): how often this player is
        the optimal captain / a flex across the game sim's own iterations
        (from solve_showdown_iteration -- already in the sim response). The
        showdown version of "who's chalk by Thursday".
      - implied_total: the player's team's Vegas implied points -- the public
        chases the shootout/favourite side directly off the same number.

    Deterministic when `seed` is given.
    """
    POS_WEIGHTS = {'QB': 0.90, 'RB': 1.15, 'WR': 1.0, 'TE': 0.95, 'DST': 0.55, 'K': 0.6}
    # Captured before any computation -- see _compute_ownership's identical
    # comment on why this can't be determined after the fact.
    locked_flex = {i for i, p in enumerate(players) if p.get('ownership_pct') is not None}
    locked_cpt = {i for i, p in enumerate(players) if p.get('cpt_ownership_pct') is not None}

    priced = [p for p in players if p.get('salary')]
    if len(priced) < 6:
        return players

    projs = np.array([max(0.1, p.get('projection') or 0.1) for p in priced], dtype=float)
    sal_k = np.array([max(p['salary'], 1) / 1000.0 for p in priced], dtype=float)
    pos_w = np.array([POS_WEIGHTS.get(p['pos'], 1.0) for p in priced], dtype=float)

    opt_cpt = np.array([min(1.0, max(0.0, (p.get('optimal_cpt_pct') or 0.0) / 100.0)) for p in priced])
    opt_flex = np.array([min(1.0, max(0.0, (p.get('optimal_flex_pct') or 0.0) / 100.0)) for p in priced])
    totals = [p['implied_total'] for p in priced if p.get('implied_total') is not None]
    if len(totals) >= 2 and max(totals) > min(totals):
        lo, hi = min(totals), max(totals)
        vegas = np.array([((p.get('implied_total') - lo) / (hi - lo)) if p.get('implied_total') is not None else 0.5
                          for p in priced])
    else:
        vegas = np.full(len(priced), 0.5)

    # ── FLEX ownership: value core + optimal-flex chalk proxy + Vegas nudge ─
    flex_val = (projs / sal_k) * pos_w
    thr = np.percentile(flex_val, 88) if len(flex_val) >= 8 else flex_val.max()
    flex_val = np.where(flex_val >= thr, flex_val * 1.35, flex_val)
    flex_val = flex_val * (1.0 + 0.9 * opt_flex)          # up to +90% for a near-always-optimal flex
    flex_val = flex_val * (0.92 + 0.16 * vegas)           # +/-8% Vegas environment
    fv = flex_val - flex_val.max()
    flex_soft = np.exp(fv / (0.55 * (fv.std() + 1e-9)))
    flex_soft = flex_soft / (flex_soft.sum() + 1e-9)
    flex_raw = flex_soft * 500.0

    # ── CPT ownership: ceiling core + strong optimal-captain proxy ────────
    cpt_val = (projs ** 1.35) * (1.0 + 0.15 * (projs / sal_k / np.median(projs / sal_k) - 1.0))
    cpt_val = cpt_val * (1.0 + 2.0 * opt_cpt)             # optimal-CPT rate is the dominant real signal
    cpt_val = cpt_val * (0.90 + 0.20 * vegas)
    cv = cpt_val - cpt_val.max()
    cpt_soft = np.exp(cv / (0.42 * (cv.std() + 1e-9)))
    cpt_soft = cpt_soft / (cpt_soft.sum() + 1e-9)
    cpt_raw = cpt_soft * 100.0

    rng = np.random.default_rng(seed)
    for i, p in enumerate(priced):
        if p.get('ownership_pct') is None:
            base = flex_raw[i]
            sigma = 0.22 if base > 30 else (0.32 if base > 10 else 0.42)
            val = base * float(np.exp(rng.normal(0, sigma)))
            p['ownership_pct'] = round(float(min(62.0, max(0.5, val))), 1)
        if p.get('cpt_ownership_pct') is None:
            base = cpt_raw[i]
            sigma = 0.28 if base > 12 else (0.40 if base > 4 else 0.55)
            val = base * float(np.exp(rng.normal(0, sigma)))
            p['cpt_ownership_pct'] = round(float(min(45.0, max(0.3, val))), 1)

    # flex_raw/cpt_raw target sum=500/100 by construction, but per-player
    # noise and the min/max clamps above both nudge individual values after
    # that, so the total drifts -- renormalize to the real DK targets now.
    normalize_showdown_ownership(players, locked_flex, locked_cpt)
    return players
