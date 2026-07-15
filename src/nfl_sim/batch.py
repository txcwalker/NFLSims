"""Monte Carlo dispatcher: runs many games and aggregates the results.

Runs thousands of NFLGameEngine simulations (vectorized by default, with a
sequential worker fallback), then aggregates play-by-play logs into game- and
player-level projections — means, medians, std, and 5th/95th-percentile floors
and ceilings — with dynamic slot mapping (QB1/RB1/WR1/TE1…) derived from
target/carry shares.

Entry point: BatchSimulator(...).run_batch(iterations, vectorized=True).
Full design rationale: see batch.md.
"""

import pandas as pd
import numpy as np
import os
import json
import warnings
warnings.filterwarnings('ignore')

from .game_engine import NFLGameEngine
from legacy.game_engine_sequential import SequentialNFLGameEngine
from .scoring import get_player_summary

def _simulate_single_game_worker(args):
    game_id, team_off, team_def, year, dna, team_coaches, rosters, trench_tiers, player_to_slot = args
    sim = SequentialNFLGameEngine(
        team_off, 
        team_def, 
        year=year,
        dna=dna,
        team_coaches=team_coaches,
        rosters=rosters,
        trench_tiers=trench_tiers
    )
    sim.run_game()
    
    summary = {
        'game_id': game_id,
        'off_score': sim.scores[team_off],
        'def_score': sim.scores[team_def],
        'total': sim.scores[team_off] + sim.scores[team_def],
        'spread': sim.scores[team_off] - sim.scores[team_def],
        'winner': team_off if sim.scores[team_off] > sim.scores[team_def] else team_def,
        'total_plays': sim.play_count,
        'plays_over_20_yds': getattr(sim, 'plays_over_20_yds', 0),
        'punts': getattr(sim, 'punts_run', 0),
        'fourth_down_decisions': getattr(sim, 'fourth_down_decisions', []),
        'fg_attempts_details': getattr(sim, 'fg_attempts_details', []),
        'td_details': getattr(sim, 'td_details', [])
    }
    
    player_stats = []
    # get_stats_report now returns a list of dictionaries directly
    for row in sim.get_stats_report():
        row['game_id'] = game_id
        row['winner'] = summary['winner']
        
        # Add Slot column dynamically using the precomputed map
        player_name = row.get('Player')
        team = row.get('Team')
        slot = player_to_slot.get(team, {}).get(player_name, row.get('Pos', 'WR/TE'))
        row['Slot'] = slot
        
        row = get_player_summary(row)
        player_stats.append(row)
        
    return summary, player_stats

# NOTE: ProcessPoolExecutor was removed because Windows worker processes import
# matplotlib/PIL at startup (via transitive imports), which causes a paging file
# DLL load failure. The sequential loop below is reliable and fast enough for
# typical usage (~0.4s/game). Re-enable multiprocessing only after ensuring
# worker processes cannot import any visualization libraries.

class BatchSimulator:
    _json_cache = {}

    def __init__(self, team_off="BUF", team_def="KC", context=None, year=2025):
        self.team_off = team_off
        self.team_def = team_def
        self.year = year
        self.context = context # Note: context (year) should be handled by engine
        self.raw_player_results = []
        self.game_summaries = []
        
        # Load assets once per batch
        self.dna = {
            'qb': self._load_json('data/dna/qb_dna.json'),
            'skill': self._load_skill_dna(),
            'coach': self._load_json('data/dna/coach_dna.json'),
            'trench': self._load_json('data/dna/trench_dna.json')
        }
        self.team_coaches = self._load_json('data/dna/team_to_coach_2025.json')
        self.rosters = {
            team_off: self._load_json(f"data/current_rosters/{team_off}_traits_{self.year}.json").get('traits', {}),
            team_def: self._load_json(f"data/current_rosters/{team_def}_traits_{self.year}.json").get('traits', {})
        }
        self.trench_tiers = self._load_json('data/dna/trench_tiers_2025.json')
        
        # Compute slot mapping dynamically once per batch
        self.player_to_slot = {
            self.team_off: self._build_slot_map(self.team_off),
            self.team_def: self._build_slot_map(self.team_def)
        }

    def _build_slot_map(self, team):
        traits = self.rosters.get(team, {})
        qbs = []
        rbs = []
        receivers = []
        
        for name, p_traits in traits.items():
            pos = p_traits.get('pos', 'WR/TE')
            if pos == 'QB':
                qbs.append((name, p_traits))
            elif pos == 'RB':
                rbs.append((name, p_traits))
            else:
                receivers.append((name, p_traits))
                
        slot_map = {}
        
        # Map QBs (highest total_attempts in qb_dna is starter, others are backup)
        qbs.sort(key=lambda x: self.dna['qb'].get(x[0], {}).get('total_attempts', 0), reverse=True)
        for idx, (name, _) in enumerate(qbs):
            if idx == 0:
                slot_map[name] = 'QB'
            else:
                slot_map[name] = f'QB{idx+1}'
                
        # Map RBs
        rbs.sort(key=lambda x: x[1].get('carry_share', 0.0), reverse=True)
        for idx, (name, _) in enumerate(rbs):
            slot_map[name] = f'RB{idx+1}'
            
        # Map Receivers (WR/TE)
        te_names = {
            'kincaid', 'knox', 'kelce', 'gray', 'morris', 'davidson', 'wiley', 'fortson', 'hendershot', 
            'pitts', 'andrews', 'kittle', 'laporta', 'bowers', 'njoku', 'goedert', 'engram', 'ferguson', 
            'schultz', 'kmet', 'freiermuth', 'henry', 'likely', 'musgrave', 'kraft', 'otton', 'conklin'
        }
        
        receivers.sort(key=lambda x: x[1].get('target_share', 0.0), reverse=True)
        
        wr_idx = 1
        te_idx = 1
        for name, _ in receivers:
            is_te = False
            for te in te_names:
                if te in name.lower():
                    is_te = True
                    break
            if is_te:
                slot_map[name] = f'TE{te_idx}'
                te_idx += 1
            else:
                slot_map[name] = f'WR{wr_idx}'
                wr_idx += 1
                
        return slot_map

    def _load_json(self, path):
        if path in self._json_cache:
            return self._json_cache[path]
        if os.path.exists(path):
            with open(path, 'r') as f:
                data = json.load(f)
                self._json_cache[path] = data
                return data
        return {}

    def _load_skill_dna(self):
        cache_key = f"__merged_skill_dna_{self.year}"
        if cache_key in self._json_cache:
            return self._json_cache[cache_key]
        rb = self._load_json('data/dna/rb_dna.json')
        wr = self._load_json('data/dna/wr_dna.json')
        te = self._load_json('data/dna/te_dna.json')
        merged = {}
        merged.update(rb)
        merged.update(wr)
        merged.update(te)
        if not merged:
            merged = self._load_json('data/dna/skill_dna.json')
        self._json_cache[cache_key] = merged
        return merged

    def run_batch(self, iterations=100, vectorized=True):
        if vectorized:
            print(f"Executing Vectorized Monte Carlo Simulation ({iterations} games): {self.team_off} vs {self.team_def}")
            sim = NFLGameEngine(
                self.team_off, 
                self.team_def, 
                year=self.year,
                dna=self.dna,
                team_coaches=self.team_coaches,
                rosters=self.rosters,
                trench_tiers=self.trench_tiers,
                N=iterations
            )
            sim.run_game()
            game_summaries = sim.get_game_summaries()
            raw_player_results = sim.get_player_stats_flat(self.player_to_slot)
            print("Vectorized Batch Simulation Complete.")
            return pd.DataFrame(game_summaries), pd.DataFrame(raw_player_results)
            
        print(f"Executing Sequential Monte Carlo Simulation ({iterations} games): {self.team_off} vs {self.team_def}")
        
        args_list = [
            (i, self.team_off, self.team_def, self.year, self.dna, self.team_coaches, self.rosters, self.trench_tiers, self.player_to_slot)
            for i in range(iterations)
        ]
        
        # Sequential execution — reliable on all platforms.
        # Each game runs ~0.4s steady-state after model warm-up.
        for i, args in enumerate(args_list):
            summary, p_stats = _simulate_single_game_worker(args)
            self.game_summaries.append(summary)
            self.raw_player_results.extend(p_stats)
            if (i + 1) % 25 == 0:
                print(f" - Completed {i+1} games...")
                
        print("Sequential Batch Simulation Complete.")
        return pd.DataFrame(self.game_summaries), pd.DataFrame(self.raw_player_results)

class StatAggregator:
    @staticmethod
    def aggregate_player_stats(player_df):
        """Computes Avg, Median, Std, and Percentiles for all tracked players."""
        metrics = [
            'pAtt', 'pCmp', 'pYds', 'pTD', 'int',
            'rAtt', 'rYds', 'rTD', 'recYds', 'recTD',
            'targets', 'rec', 'touches', 'fumbles', 'fumbles_lost', 'sacks_taken',
            'def_sack', 'def_int', 'def_fumble_rec', 'def_td', 'pts_allowed',
            'dk_score', 'fd_score'
        ]
        
        # Ensure all columns exist in player_df
        for m in metrics:
            if m not in player_df.columns:
                player_df[m] = 0.0
            else:
                player_df[m] = player_df[m].fillna(0.0)
                
        # Group by Team and Slot
        agg_results = []
        for (team, slot), group in player_df.groupby(['Team', 'Slot']):
            res = {'Team': team, 'Slot': slot, 'Games': len(group)}
            if 'Player' in group.columns:
                res['Player'] = group['Player'].iloc[0]
                
            use_weights = 'weight' in group.columns and group['weight'].sum() > 0
            for m in metrics:
                vals = group[m].values
                w = group['weight'].values if use_weights else None
                
                if use_weights:
                    sorter = np.argsort(vals)
                    sorted_vals = vals[sorter]
                    sorted_w = w[sorter]
                    cumsum_w = np.cumsum(sorted_w) - 0.5 * sorted_w
                    sum_w = cumsum_w[-1] + 0.5 * sorted_w[-1]
                    if sum_w > 0:
                        cumsum_w /= sum_w
                        
                        res[f'{m}_avg'] = round(np.average(vals, weights=w), 2)
                        res[f'{m}_med'] = round(np.interp(0.5, cumsum_w, sorted_vals), 2)
                        res[f'{m}_std'] = round(np.sqrt(np.average((vals - res[f'{m}_avg'])**2, weights=w)), 2)
                        res[f'{m}_p05'] = round(np.interp(0.05, cumsum_w, sorted_vals), 2)
                        res[f'{m}_p95'] = round(np.interp(0.95, cumsum_w, sorted_vals), 2)
                    else:
                        res[f'{m}_avg'] = round(vals.mean(), 2)
                        res[f'{m}_med'] = round(np.median(vals), 2)
                        res[f'{m}_std'] = round(vals.std(), 2)
                        res[f'{m}_p05'] = round(np.quantile(vals, 0.05), 2)
                        res[f'{m}_p95'] = round(np.quantile(vals, 0.95), 2)
                else:
                    res[f'{m}_avg'] = round(vals.mean(), 2)
                    res[f'{m}_med'] = round(np.median(vals), 2)
                    res[f'{m}_std'] = round(vals.std(), 2)
                    res[f'{m}_p05'] = round(np.quantile(vals, 0.05), 2)
                    res[f'{m}_p95'] = round(np.quantile(vals, 0.95), 2)
            agg_results.append(res)
            
        return pd.DataFrame(agg_results)

    @staticmethod
    def aggregate_game_stats(game_df, team_a):
        """Computes aggregate game stats, spreads, and totals."""
        summary = {
            'Total_Avg': round(game_df['total'].mean(), 2),
            'Total_Med': round(game_df['total'].median(), 2),
            'Spread_Avg': round(game_df['spread'].mean(), 2),
            'Win_Rate_A': round((game_df['winner'] == team_a).mean(), 3),
        }
        
        # Split stats by winner
        a_wins = game_df[game_df['winner'] == team_a]
        b_wins = game_df[game_df['winner'] != team_a]
        
        if not a_wins.empty:
            summary['Total_Avg_A_Win'] = round(a_wins['total'].mean(), 2)
            summary['Spread_Avg_A_Win'] = round(a_wins['spread'].mean(), 2)
        
        if not b_wins.empty:
            summary['Total_Avg_B_Win'] = round(b_wins['total'].mean(), 2)
            summary['Spread_Avg_B_Win'] = round(b_wins['spread'].mean(), 2)
            
        return summary
