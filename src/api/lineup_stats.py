"""Lineup-vs-field statistics: ITM%/Top1%/Top0.1%/EV% and the "range of
outcomes" percentile-finish histogram, ranked against a full simulated field
(not one random opponent -- see _compute_lineup_field_stats_batch's own
docstring for why that distinction matters).

Factored out of src/api/app.py (where /api/optimize and /api/optimize_showdown
still call these for freshly-generated lineups) so src/api/sim_replay_store.py
can reuse the exact same math for REAL submitted lineups ranked against a
REAL contest's rescored field, without a circular import back into app.py.
Pure functions -- no FastAPI/app-global dependencies, just numpy.
"""
import numpy as np


def get_default_payout_structure(contest_type: str, prize_pool: float, paying_positions: int, total_entries: int) -> list:
    """Generate a default payout structure based on contest type."""
    if contest_type == 'extreme_top_heavy':
        # Bat Flip style: top 3 = ~42% of pool
        tiers = [
            (1, 1, 0.286), (2, 2, 0.086), (3, 3, 0.057), (4, 4, 0.029),
            (5, 5, 0.014), (6, 7, 0.0075), (8, 10, 0.0045), (11, 25, 0.003),
            (26, 50, 0.0015), (51, 100, 0.001)
        ]
    elif contest_type == 'top_heavy':
        # Rally Cap style
        tiers = [
            (1, 1, 0.200), (2, 2, 0.093), (3, 3, 0.047), (4, 4, 0.027),
            (5, 5, 0.013), (6, 6, 0.0093), (7, 7, 0.0067), (8, 8, 0.0053),
            (9, 10, 0.004), (11, 15, 0.0027), (16, 20, 0.002), (21, 50, 0.0013),
            (51, 100, 0.001)
        ]
    elif contest_type == 'flat':
        # Home Plate style
        tiers = [
            (1, 1, 0.107), (2, 2, 0.071), (3, 3, 0.057), (4, 4, 0.043),
            (5, 5, 0.029), (6, 6, 0.021), (7, 8, 0.017), (9, 11, 0.014),
            (12, 15, 0.011), (16, 20, 0.010), (21, 30, 0.0086), (31, 55, 0.0071),
            (56, 96, 0.005)
        ]
    elif contest_type == 'cash':
        # 50/50 style: top 50% wins 1.9x
        cutoff = max(1, int(total_entries * 0.50))
        return [{'rank_start': 1, 'rank_end': cutoff, 'payout': prize_pool * 1.9 / cutoff}]
    else:
        tiers = [(1, 1, 0.200), (2, 10, 0.050), (11, 50, 0.010), (51, paying_positions, 0.002)]

    structure = []
    for r_start, r_end, pct in tiers:
        if r_start > paying_positions:
            break
        r_end = min(r_end, paying_positions)
        count = r_end - r_start + 1
        total_pct = pct * count
        per_entry = (prize_pool * pct) if count == 1 else (prize_pool * total_pct / count)
        structure.append({'rank_start': r_start, 'rank_end': r_end, 'payout': round(per_entry, 2)})

    # Fill remaining paying positions with min payout (entry fee recovery)
    if structure:
        last_covered = structure[-1]['rank_end']
        if last_covered < paying_positions:
            min_payout = prize_pool * 0.0008
            structure.append({'rank_start': last_covered + 1, 'rank_end': paying_positions, 'payout': round(min_payout, 2)})

    return structure


def compute_lineup_field_stats_batch(
    lineup_draws_list: list,
    field_matrix: np.ndarray,
    payout_structure: list,
    entry_fee: float,
    total_entries: int,
    paying_positions: int,
    n_bins: int = 40,
) -> list:
    """Score EVERY generated lineup against the FULL simulated contest field,
    iteration by iteration -- not a single random opponent, and not a cutoff
    blended across every game environment. `field_matrix` is (n_field, n_sims):
    every field-sample lineup's (sharp / fake_sharp / casual / toilet archetype
    mix, see field_simulator.py / _build_showdown_field) score in every
    aligned iteration, drawn from the SAME game-environment as each lineup's
    own draws (all read off the same iteration index upstream). So iteration i
    is one self-contained sim run: a lineup scored some points and, relative
    to the field scored at that same environment, finished at some percentile
    -- ranking against all of them in that same environment is what "how does
    this lineup do against the field" actually means. A hot game environment
    lifts a lineup AND the whole field together; a fixed marginal cutoff or a
    single random opponent per iteration both miss that, which is what made
    EV/Top1% read too high before this.

    Why batched: `n_field` needs to be in the thousands for stable top-1%/
    top-0.1% tail estimates, but a brute-force per-lineup comparison against
    the field is O(n_field * n_sims) and repeats for every generated lineup --
    it scales so badly that it becomes the request's dominant cost well before
    n_field is large enough to matter. Instead, every lineup's draws are
    stacked with the field into one (n_field + n_lineups, n_sims) matrix and
    rank-transformed (argsort of argsort, i.e. every entry's rank within its
    own iteration/column) in two vectorized numpy calls total, regardless of
    how many lineups there are.

    Returns one dict per lineup, same order as `lineup_draws_list`: ITM%/
    Top1%/Top0.1%/EV% (all empirical, from real per-iteration rank -> real
    payout-tier lookup), lineup percentile/volatility stats, and the
    percentile-finish histogram for the "range of outcomes" chart.
    """
    n_field = field_matrix.shape[0]
    n_sims = field_matrix.shape[1]
    n_lineups = len(lineup_draws_list)
    lineup_matrix = np.vstack(lineup_draws_list)  # (n_lineups, n_sims), float64 -- kept full
    # precision for this request's own lineups (used below for p50/p95/etc).

    # Rank (0-indexed, ascending) of every field + lineup entry within its own
    # iteration/column: argsort-of-argsort is the standard trick for "rank
    # within column" without a per-column Python loop or per-lineup compare.
    # float32/int32 here (not the numpy defaults of float64/int64) -- DK
    # fantasy scores don't need 15-digit precision, and n_field can be in the
    # thousands (int32 covers up to ~2.1B, plenty), so this halves the memory
    # of what's otherwise 3 full (n_field+n_lineups, n_sims) arrays alive at once.
    combined = np.vstack([field_matrix, lineup_matrix]).astype(np.float32, copy=False)
    order = np.argsort(combined, axis=0).astype(np.int32, copy=False)
    del combined
    ranks = np.empty_like(order)
    col_idx = np.arange(n_sims, dtype=np.int32)[None, :]
    ranks[order, col_idx] = np.arange(n_field + n_lineups, dtype=np.int32)[:, None]
    del order
    lineup_ranks = ranks[n_field:, :]  # (n_lineups, n_sims)
    denom = n_field + n_lineups

    top1_thresh = max(1, int(total_entries * 0.01))
    top01_thresh = max(1, int(total_entries * 0.001))
    paying_pct = paying_positions / max(total_entries, 1)
    cash_percentile = round((1 - paying_pct) * 100, 2)

    results = []
    for i in range(n_lineups):
        lineup_draws = lineup_matrix[i]
        # % of the field this lineup beats in that same iteration's environment.
        percentile = lineup_ranks[i] / max(denom, 1) * 100.0
        implied_rank = np.maximum(1, np.round(total_entries * (1 - percentile / 100.0))).astype(int)

        itm_pct = float(np.mean(implied_rank <= max(paying_positions, 1)) * 100)
        top1_pct = float(np.mean(implied_rank <= top1_thresh) * 100)
        top01_pct = float(np.mean(implied_rank <= top01_thresh) * 100)

        # ── EV: real payout at this iteration's implied rank, averaged ──────
        payouts = np.zeros(n_sims)
        if payout_structure:
            for tier in payout_structure:
                r_start = tier['rank_start'] if isinstance(tier, dict) else tier.rank_start
                r_end   = tier['rank_end']   if isinstance(tier, dict) else tier.rank_end
                payout  = tier['payout']     if isinstance(tier, dict) else tier.payout
                mask = (implied_rank >= r_start) & (implied_rank <= r_end)
                payouts[mask] = payout
        else:
            prize_pool = entry_fee * total_entries * 0.85
            flat = prize_pool / max(paying_positions, 1)
            payouts[implied_rank <= max(paying_positions, 1)] = flat

        ev_dollars = float(np.mean(payouts))
        ev_pct = round(((ev_dollars / max(entry_fee, 1)) - 1) * 100, 2)

        lineup_p50 = float(np.percentile(lineup_draws, 50))
        lineup_p75 = float(np.percentile(lineup_draws, 75))
        lineup_p95 = float(np.percentile(lineup_draws, 95))
        lineup_std = float(np.std(lineup_draws))

        counts, edges = np.histogram(percentile, bins=n_bins, range=(0, 100))
        top10_count = int(np.count_nonzero(implied_rank <= 10))
        top1_count = int(np.count_nonzero(implied_rank <= 1))

        results.append({
            'itm_pct':    round(itm_pct, 2),
            'top1_pct':   round(top1_pct, 2),
            'top01_pct':  round(top01_pct, 2),
            'ev_pct':     ev_pct,
            'lineup_p50': round(lineup_p50, 2),
            'lineup_p75': round(lineup_p75, 2),
            'lineup_p95': round(lineup_p95, 2),
            'lineup_std': round(lineup_std, 2),
            'cash_percentile': cash_percentile,
            'score_min': round(float(np.min(lineup_draws)), 2),
            'score_max': round(float(np.max(lineup_draws)), 2),
            'score_mean': round(float(np.mean(lineup_draws)), 2),
            'rank1_count': top1_count,
            'rank_top10_count': top10_count,
            'n_field': n_field,
            'histogram': {
                'bin_edges': [round(float(e), 2) for e in edges],
                'counts': [int(c) for c in counts],
                'n_sims': n_sims,
            },
        })
    return results
