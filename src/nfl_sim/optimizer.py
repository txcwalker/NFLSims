import numpy as np

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


def solve_traditional_iteration(names, salaries, positions, teams, scores, salary_cap=50000):
    """
    Finds the optimal Traditional lineup for a single trial iteration.
    Lineup structure: 1 QB, 2 RB, 3 WR, 1 TE, 1 FLEX (RB/WR/TE), 1 DST.
    names, salaries, positions, teams, scores: lists/arrays of player attributes.
    """
    n = len(names)
    if n < 9:
        return []
        
    qbs = []
    rbs = []
    wrs = []
    tes = []
    dsts = []
    
    for i in range(n):
        if scores[i] < -5.0:
            continue
        pos = positions[i]
        sal = salaries[i]
        item = (scores[i], sal, names[i])
        
        if pos == 'QB':
            qbs.append(item)
        elif pos == 'RB':
            rbs.append(item)
        elif pos == 'WR':
            wrs.append(item)
        elif pos == 'TE':
            tes.append(item)
        elif pos == 'DST':
            dsts.append(item)
            
    if not qbs or len(rbs) < 2 or len(wrs) < 3 or not tes or not dsts:
        return []
        
    # Helper to prune by union of top scores and top cheapest
    def prune_position_group(group, limit_score, limit_cheap):
        group.sort(key=lambda x: x[0], reverse=True)
        top_scores = group[:limit_score]
        
        group.sort(key=lambda x: x[1])
        top_cheapest = group[:limit_cheap]
        
        seen = set()
        union_list = []
        for x in top_scores + top_cheapest:
            player_name = x[2]
            if player_name not in seen:
                seen.add(player_name)
                union_list.append(x)
        
        union_list.sort(key=lambda x: x[0], reverse=True)
        return union_list

    qbs = prune_position_group(qbs, 4, 3)
    dsts = prune_position_group(dsts, 4, 3)
    tes = prune_position_group(tes, 6, 4)
    rbs = prune_position_group(rbs, 10, 8)
    wrs = prune_position_group(wrs, 12, 10)
    
    def find_top_k_rb(rbs, num_rb, rem_cap, k=10):
        results = []
        def dfs(idx, count, cur_sal, cur_score, selected):
            if count == num_rb:
                results.append((cur_score, cur_sal, list(selected)))
                results.sort(key=lambda x: x[0], reverse=True)
                if len(results) > k:
                    results.pop()
                return
            if idx >= len(rbs) or count + (len(rbs) - idx) < num_rb:
                return
            # Score pruning
            if len(results) == k:
                rem_needed = num_rb - count
                max_possible = cur_score + sum(rbs[j][0] for j in range(idx, idx + rem_needed))
                if max_possible <= results[-1][0]:
                    return
            s_pts, s_sal, s_name = rbs[idx]
            if cur_sal + s_sal <= rem_cap:
                selected.append(idx)
                dfs(idx + 1, count + 1, cur_sal + s_sal, cur_score + s_pts, selected)
                selected.pop()
            dfs(idx + 1, count, cur_sal, cur_score, selected)
        dfs(0, 0, 0, 0.0, [])
        return results

    def find_top_k_wr(wrs, num_wr, rem_cap, k=10):
        results = []
        def dfs(idx, count, cur_sal, cur_score, selected):
            if count == num_wr:
                results.append((cur_score, cur_sal, list(selected)))
                results.sort(key=lambda x: x[0], reverse=True)
                if len(results) > k:
                    results.pop()
                return
            if idx >= len(wrs) or count + (len(wrs) - idx) < num_wr:
                return
            # Score pruning
            if len(results) == k:
                rem_needed = num_wr - count
                max_possible = cur_score + sum(wrs[j][0] for j in range(idx, idx + rem_needed))
                if max_possible <= results[-1][0]:
                    return
            s_pts, s_sal, s_name = wrs[idx]
            if cur_sal + s_sal <= rem_cap:
                selected.append(idx)
                dfs(idx + 1, count + 1, cur_sal + s_sal, cur_score + s_pts, selected)
                selected.pop()
            dfs(idx + 1, count, cur_sal, cur_score, selected)
        dfs(0, 0, 0, 0.0, [])
        return results

    def find_top_k_te(tes, num_te, rem_cap, k=10):
        results = []
        def dfs(idx, count, cur_sal, cur_score, selected):
            if count == num_te:
                results.append((cur_score, cur_sal, list(selected)))
                results.sort(key=lambda x: x[0], reverse=True)
                if len(results) > k:
                    results.pop()
                return
            if idx >= len(tes) or count + (len(tes) - idx) < num_te:
                return
            # Score pruning
            if len(results) == k:
                rem_needed = num_te - count
                max_possible = cur_score + sum(tes[j][0] for j in range(idx, idx + rem_needed))
                if max_possible <= results[-1][0]:
                    return
            s_pts, s_sal, s_name = tes[idx]
            if cur_sal + s_sal <= rem_cap:
                selected.append(idx)
                dfs(idx + 1, count + 1, cur_sal + s_sal, cur_score + s_pts, selected)
                selected.pop()
            dfs(idx + 1, count, cur_sal, cur_score, selected)
        dfs(0, 0, 0, 0.0, [])
        return results

    best_score = -1.0
    best_lineup = []
    
    for qb_score, qb_sal, qb_name in qbs:
        for dst_score, dst_sal, dst_name in dsts:
            rem_cap = salary_cap - qb_sal - dst_sal
            if rem_cap < 0:
                continue
                
            cases = [
                (3, 3, 1),
                (2, 4, 1),
                (2, 3, 2)
            ]
            for num_rb, num_wr, num_te in cases:
                if len(rbs) < num_rb or len(wrs) < num_wr or len(tes) < num_te:
                    continue
                    
                # Find top 50 RB combinations
                rb_combos = find_top_k_rb(rbs, num_rb, rem_cap, k=50)
                for rb_score, rb_sal, rb_sel in rb_combos:
                    rem_cap_after_rb = rem_cap - rb_sal
                    
                    # Find top 50 WR combinations
                    wr_combos = find_top_k_wr(wrs, num_wr, rem_cap_after_rb, k=50)
                    for wr_score, wr_sal, wr_sel in wr_combos:
                        rem_cap_after_wr = rem_cap_after_rb - wr_sal
                        
                        # Find top 1 TE combination (the best one)
                        te_combos = find_top_k_te(tes, num_te, rem_cap_after_wr, k=1)
                        if te_combos:
                            te_score, te_sal, te_sel = te_combos[0]
                            
                            total_score = qb_score + dst_score + rb_score + wr_score + te_score
                            if total_score > best_score:
                                    best_score = total_score
                                    best_lineup = (
                                        [qb_name, dst_name] +
                                        [rbs[i][2] for i in rb_sel] +
                                        [wrs[i][2] for i in wr_sel] +
                                        [tes[i][2] for i in te_sel]
                                    )
                                
    return best_lineup
