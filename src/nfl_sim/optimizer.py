import pulp

def solve_showdown_iteration(names, salaries, scores, salary_cap=50000):
    """
    Finds the optimal Showdown lineup for a single trial iteration.
    Lineup structure: 1 CPT (1.5x salary, 1.5x score), 5 FLEX.
    names: list of player names
    salaries: numpy array of actual player salaries
    scores: numpy array of scores
    """
    n = len(names)
    if n < 6:
        return []
        
    best_score = -1.0
    best_lineup = []
    
    # Prune players with 0 or negative scores to speed up Flex selection
    valid_indices = [i for i in range(n) if scores[i] > 0]
    valid_indices.sort(key=lambda idx: scores[idx], reverse=True)
    
    # Try each player as captain
    for cpt in range(n):
        cpt_sal = salaries[cpt] * 1.5
        if cpt_sal > salary_cap:
            continue
        cpt_score = scores[cpt] * 1.5
        
        rem_budget = salary_cap - cpt_sal
        flex_candidates = [idx for idx in valid_indices if idx != cpt and salaries[idx] <= rem_budget]
        
        num_flex = len(flex_candidates)
        if num_flex < 5:
            continue
            
        # Suffix sums of scores for pruning
        suffix_sums = [0.0] * (num_flex + 1)
        for i in range(num_flex - 1, -1, -1):
            suffix_sums[i] = suffix_sums[i+1] + scores[flex_candidates[i]]
            
        best_flex_score = -1.0
        best_flex_set = []
        
        def dfs_flex(idx, count, current_sal, current_score, selected):
            nonlocal best_flex_score, best_flex_set
            if count == 5:
                if current_score > best_flex_score:
                    best_flex_score = current_score
                    best_flex_set = list(selected)
                return
                
            if idx >= num_flex or count + (num_flex - idx) < 5:
                return
                
            # Suffix sum pruning
            rem_needed = 5 - count
            max_possible = current_score + suffix_sums[idx] - suffix_sums[idx + rem_needed]
            if current_score + suffix_sums[idx] <= best_flex_score:
                return
                
            # Option 1: Select candidate
            cand_idx = flex_candidates[idx]
            sal = salaries[cand_idx]
            if current_sal + sal <= rem_budget:
                selected.append(cand_idx)
                dfs_flex(idx + 1, count + 1, current_sal + sal, current_score + scores[cand_idx], selected)
                selected.pop()
                
            # Option 2: Skip candidate
            dfs_flex(idx + 1, count, current_sal, current_score, selected)
            
        dfs_flex(0, 0, 0, 0.0, [])
        
        if best_flex_score >= 0:
            total_score = cpt_score + best_flex_score
            if total_score > best_score:
                best_score = total_score
                best_lineup = [names[cpt]] + [names[idx] for idx in best_flex_set]
                
    return best_lineup


def solve_optimal_lineup_milp(names, salaries, positions, scores, salary_cap=50000):
    """
    Finds the PROVABLY optimal Traditional lineup for a single trial
    iteration via a real MILP solve (PuLP + CBC), replacing this module's
    former solve_traditional_iteration -- a hand-rolled branch-and-bound
    that (a) wasn't actually exact (it ran under a fixed 1.5s/call time
    budget and returned its best-found-so-far lineup once that ran out, not
    a proven optimum) and (b) at ~1.5s/call was far too slow to run across
    more than a small sample of a week's ~10,000 sim iterations -- see
    src/api/app.py's OPTIMAL_LINEUP_SAMPLE_ITERATIONS and WORKLOG
    2026-09-15. Benchmarked against a real ~400-player multi-game slate:
    ~0.05-0.2s/solve, ~15x faster AND exact, which is what makes running it
    across genuinely every iteration of a week's sim (instead of 15 of
    them) affordable as a batch pass -- see
    scripts/simulation_runners/compute_optimal_pct_2026.py.

    Same constraint set as src.api.app._solve_lineup_ilp (1 QB, >=2 RB,
    >=3 WR, >=1 TE, 1 DST, 9 total, FLEX absorbing the extra RB/WR/TE slot,
    salary cap), minus that function's multi-lineup-portfolio constraints
    (locks/excludes/exposure/min-unique) -- this is always exactly one,
    unconstrained, single-lineup solve for one iteration's score draw, not
    a whole generated portfolio.

    names, positions: parallel lists, one entry per player -- any hashable
        value (a plain name string, or a (player, team) tuple), passed
        straight through into the returned lineup so the caller can key
        counts however it needs to.
    salaries: parallel list/array of DK salaries.
    scores: parallel array of this iteration's simulated DK score per player.

    Returns the list of 9 selected entries from `names`, or [] if no legal
    lineup exists (too few players at some position) or CBC can't find a
    feasible solution (e.g. salary cap unreachable with this pool).
    """
    n = len(names)
    if n < 9:
        return []

    qb_idx = [i for i in range(n) if positions[i] == 'QB']
    rb_idx = [i for i in range(n) if positions[i] == 'RB']
    wr_idx = [i for i in range(n) if positions[i] == 'WR']
    te_idx = [i for i in range(n) if positions[i] == 'TE']
    dst_idx = [i for i in range(n) if positions[i] == 'DST']
    flex_idx = rb_idx + wr_idx + te_idx

    if not qb_idx or len(rb_idx) < 2 or len(wr_idx) < 3 or not te_idx or not dst_idx:
        return []

    prob = pulp.LpProblem('OptimalLineup', pulp.LpMaximize)
    x = [pulp.LpVariable(f'x_{i}', cat='Binary') for i in range(n)]

    prob += pulp.lpSum(float(scores[i]) * x[i] for i in range(n))
    prob += pulp.lpSum(salaries[i] * x[i] for i in range(n)) <= salary_cap
    prob += pulp.lpSum(x[i] for i in qb_idx) == 1
    prob += pulp.lpSum(x[i] for i in rb_idx) >= 2
    prob += pulp.lpSum(x[i] for i in wr_idx) >= 3
    prob += pulp.lpSum(x[i] for i in te_idx) >= 1
    prob += pulp.lpSum(x[i] for i in dst_idx) == 1
    prob += pulp.lpSum(x[i] for i in flex_idx) >= 6
    prob += pulp.lpSum(x[i] for i in range(n)) == 9

    prob.solve(pulp.PULP_CBC_CMD(msg=0, timeLimit=5))
    if prob.status != 1:  # not optimal
        return []

    selected = [i for i in range(n) if pulp.value(x[i]) and pulp.value(x[i]) > 0.5]
    if len(selected) != 9:
        return []
    return [names[i] for i in selected]
