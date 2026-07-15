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

class NFLGameEngine:
    def __init__(self, away_team, home_team, year=2025, dna=None, team_coaches=None, rosters=None, trench_tiers=None, N=10000):
        self.away_team = away_team
        self.home_team = home_team
        self.year = year
        self.N = N
        self.registry = ModelRegistry()
        
        # Load DNA Registry
        self.dna = dna if dna is not None else {
            'qb': self._load_json('data/dna/qb_dna.json'),
            'skill': self._load_skill_dna(),
            'coach': self._load_json('data/dna/coach_dna.json'),
            'trench': self._load_json('data/dna/trench_dna.json')
        }
        
        self.team_coaches = team_coaches if team_coaches is not None else self._load_json('data/dna/team_to_coach_2025.json')
        self.rosters = rosters if rosters is not None else {
            away_team: self._load_json(f"data/current_rosters/{away_team}_traits_{self.year}.json").get('traits', {}),
            home_team: self._load_json(f"data/current_rosters/{home_team}_traits_{self.year}.json").get('traits', {})
        }
        
        # Trench Tiers
        self.trench_tiers = trench_tiers if trench_tiers is not None else self._load_json('data/dna/trench_tiers_2025.json')
        
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
        
        # 1. Run multipliers: run_diff = def_run_stuff_tier - off_run_block_tier
        run_diff_away_off = home_t.get('run_stuff_tier', 3.0) - away_t.get('run_block_tier', 3.0)
        run_diff_home_off = away_t.get('run_stuff_tier', 3.0) - home_t.get('run_block_tier', 3.0)
        
        # Pass diff = def_pass_rush_tier - off_pass_block_tier
        pass_diff_away_off = home_t.get('pass_rush_tier', 3.0) - away_t.get('pass_block_tier', 3.0)
        pass_diff_home_off = away_t.get('pass_rush_tier', 3.0) - home_t.get('pass_block_tier', 3.0)
        
        # QB cpoe z difference (to further separate passing efficiency)
        qb_diff_away_off = away_t.get('qb_cpoe_z', 0.0) - home_t.get('qb_cpoe_z', 0.0)
        qb_diff_home_off = home_t.get('qb_cpoe_z', 0.0) - away_t.get('qb_cpoe_z', 0.0)
        
        self.run_mult_away_off = 1.06 + run_diff_away_off * 0.04
        self.run_mult_home_off = 1.06 + run_diff_home_off * 0.04
        self.pass_mult_away_off = 1.00 + (pass_diff_away_off + qb_diff_away_off) * 0.04
        self.pass_mult_home_off = 1.00 + (pass_diff_home_off + qb_diff_home_off) * 0.04

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
        self.play_count = np.zeros(self.N, dtype=np.int32)
        
        # Metrics Tracking (aggregated vectorially)
        self.plays_over_20_yds = np.zeros(self.N, dtype=np.int32)
        self.punts_run = np.zeros(self.N, dtype=np.int32)
        self.fourth_down_goes_away = np.zeros(self.N, dtype=np.int32)
        self.fourth_down_goes_home = np.zeros(self.N, dtype=np.int32)
        self.fg_attempts_away = np.zeros(self.N, dtype=np.int32)
        self.fg_attempts_home = np.zeros(self.N, dtype=np.int32)
        self.fg_made_away = np.zeros(self.N, dtype=np.int32)
        self.fg_made_home = np.zeros(self.N, dtype=np.int32)
        self.kickoffs_run = np.zeros(self.N, dtype=np.int32)
        self.presnap_penalty_snaps = np.zeros(self.N, dtype=np.int32)
        self.penalties_accepted = np.zeros(self.N, dtype=np.int32)
        self.oob_plays = np.zeros(self.N, dtype=np.int32)
        self.oob_eligible_plays = np.zeros(self.N, dtype=np.int32)
        self.drives_count = np.zeros(self.N, dtype=np.int32)  # diagnostic only

        # clock_physics_v020 Round 14 (sacks diversion hypothesis test): both teams
        # combined, whole-game. gross_sacks = gate 2's raw has_sack roll, BEFORE the
        # scramble-escape/throwaway diversion. net_sacks = actual_sacks, i.e. what
        # actually gets counted (matches player_stats[...]['sacks_taken'] summed).
        # Diagnostic only — does not affect game logic.
        self.gross_sacks = np.zeros(self.N, dtype=np.int32)
        self.net_sacks = np.zeros(self.N, dtype=np.int32)

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
                'def_sack': np.zeros(self.N, dtype=np.int32),
                'def_int': np.zeros(self.N, dtype=np.int32),
                'def_fumble_rec': np.zeros(self.N, dtype=np.int32),
                'def_td': np.zeros(self.N, dtype=np.int32),
                'pts_allowed': np.zeros(self.N, dtype=np.int32)
            }

        # Cache receivers and carry share cumulative distributions for fast sampling
        self.receivers_cache = {}
        self.rusher_cache = {}
        for team in [away_team, home_team]:
            team_roster = self.rosters[team]
            # Receivers: do not prune active receivers, keep all non-QBs who are active
            recs = [p for p, t in team_roster.items() if t.get('pos') != 'QB' and t.get('status', 'active') == 'active']
            if recs:
                shares = np.array([team_roster[r].get('target_share', 0.0) for r in recs], dtype=np.float32)
                total_shares = shares.sum()
                if total_shares <= 0:
                    shares = np.ones_like(shares) / len(shares)
                else:
                    shares /= total_shares
                self.receivers_cache[team] = (recs, np.cumsum(shares))
            else:
                self.receivers_cache[team] = (["Unknown"], np.array([1.0], dtype=np.float32))
                
            # Rushers
            rushers = [p for p, t in team_roster.items() if t.get('carry_share', 0) > 0 and t.get('status', 'active') == 'active']
            if rushers:
                shares = np.array([team_roster[r]['carry_share'] for r in rushers], dtype=np.float32)
                shares /= shares.sum()
                self.rusher_cache[team] = (rushers, np.cumsum(shares))
            else:
                self.rusher_cache[team] = ([self.rb_starters[team]], np.array([1.0], dtype=np.float32))

        # Precompute play selection logit offsets to avoid doing it inside the loops
        self.proe_offsets = {
            away_team: _proe_to_logit_offset(get_coach_proe(away_team)),
            home_team: _proe_to_logit_offset(get_coach_proe(home_team))
        }

        # Precompute DNA splits lookup maps for the loaded teams to avoid deep splits lookups inside the high-frequency loops
        self.precomputed_qb_cpoe = {}
        for qb_name, qb_data in self.registry.qb_dna.items():
            splits = qb_data.get('splits', {})
            self.precomputed_qb_cpoe[qb_name] = {
                'goalline': splits.get('goalline', {}).get('cpoe', qb_data.get('cpoe', 0.0)),
                'redzone': splits.get('redzone', {}).get('cpoe', qb_data.get('cpoe', 0.0)),
                'primary': splits.get('primary', {}).get('cpoe', qb_data.get('cpoe', 0.0)),
            }
            
        self.precomputed_skill_target_share = {}
        self.precomputed_skill_carry_share = {}
        for skill_name, skill_data in self.registry.skill_dna.items():
            splits = skill_data.get('splits', {})
            self.precomputed_skill_target_share[skill_name] = {
                'goalline': splits.get('goalline', {}).get('target_share', skill_data.get('target_share', 0.0)),
                'redzone': splits.get('redzone', {}).get('target_share', skill_data.get('target_share', 0.0)),
                'primary': splits.get('primary', {}).get('target_share', skill_data.get('target_share', 0.0)),
            }
            self.precomputed_skill_carry_share[skill_name] = {
                'goalline': splits.get('goalline', {}).get('carry_share', skill_data.get('carry_share', 0.0)),
                'redzone': splits.get('redzone', {}).get('carry_share', skill_data.get('carry_share', 0.0)),
                'primary': splits.get('primary', {}).get('carry_share', skill_data.get('carry_share', 0.0)),
            }
            
        self.precomputed_coach_proe = {}
        for coach_name, coach_data in self.registry.coach_proe_splits.items():
            self.precomputed_coach_proe[coach_name] = {
                'goalline': coach_data.get('goalline', 0.0),
                'redzone': coach_data.get('redzone', 0.0),
                'primary': coach_data.get('primary', 0.0),
            }


    def _load_json(self, path):
        if os.path.exists(path):
            with open(path, 'r') as f: return json.load(f)
        return {}

    def _load_skill_dna(self):
        rb = self._load_json('data/dna/rb_dna.json')
        wr = self._load_json('data/dna/wr_dna.json')
        te = self._load_json('data/dna/te_dna.json')
        merged = {}
        merged.update(rb)
        merged.update(wr)
        merged.update(te)
        if not merged:
            return self._load_json('data/dna/skill_dna.json')
        return merged

    def _get_starter_static(self, team, pos):
        team_roster = self.rosters[team]
        players = [p for p, t in team_roster.items() if t.get('pos') == pos and t.get('status', 'active') == 'active']
        if not players: return "Unknown"
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
        kneel_mask = is_victory & (self.time_remaining < max_bleed_time)
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
            self.drives_count[ko_mask] += 1  # diagnostic only — every kickoff starts a new drive
            self.kickoffs_run[ko_mask] += 1
            self.down[ko_mask] = 1
            self.distance[ko_mask] = 10
            self.clock_stopped[ko_mask] = True

            n_ko = np.sum(ko_mask)
            is_tb = np.random.rand(n_ko) < 0.2068

            # Live-play runoff: base kick-flight time for a touchback, scaled up
            # by return yardage for an actual return (up to a long house call).
            # Real play length varies here (not the flat 8s previously used);
            # clock still stops immediately after regardless of return length.
            ko_runoff = np.random.randint(4, 7, size=n_ko)

            # Initialize default yardline
            yardlines = np.zeros(n_ko, dtype=np.int32)

            # Touchbacks go to 30-yard line (70 yards to go)
            yardlines[is_tb] = 70

            # Returns: Shifted Log-Normal
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
            # 50% offense, 50% defense
            is_offensive = np.random.rand(self.N) < 0.5
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
                punt_runoff = np.random.randint(4, 7, size=n_punts)

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
                        if len(normal_ret_indices_in_ret) > 0:
                            normal_ret_mask = np.zeros(self.N, dtype=bool)
                            global_normal_ret = global_punt_indices[~is_tb][normal_ret_indices_in_ret]
                            normal_ret_mask[global_normal_ret] = True

                            ret_vals = np.random.exponential(scale=21.8765, size=len(normal_ret_indices_in_ret)) - 13.0
                            ret_yds = np.round(ret_vals).astype(np.int32)

                            # Capped at remaining distance (yardline_100 - 1) to avoid unsanctioned TDs
                            max_ret = self.yardline_100[normal_ret_mask] - 1
                            ret_yds = np.minimum(ret_yds, max_ret)

                            self.yardline_100[normal_ret_mask] -= ret_yds
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
                self.fg_made_away[fg_good_mask & self.possession_is_away] += 1
                self.fg_made_home[fg_good_mask & ~self.possession_is_away] += 1
                self.score_away[fg_good_mask & self.possession_is_away] += 3
                self.score_home[fg_good_mask & ~self.possession_is_away] += 3
                self.needs_kickoff[fg_good_mask] = True
                
                fg_made_runoff = np.random.randint(4, 6, size=np.sum(fg_good_mask))
                fg_miss_runoff = np.random.randint(5, 9, size=np.sum(fg_miss_mask))
                self._run_clock(fg_made_runoff, fg_good_mask)
                self._run_clock(fg_miss_runoff, fg_miss_mask)
                self._switch_possession(scored=True, mask=fg_good_mask)
                self._switch_possession(scored=False, mask=fg_miss_mask)

                # Made or missed, a field goal attempt stops the clock for the
                # exchange (was missing — same stale-state risk as punts above).
                self.clock_stopped[fg_mask] = True
                active = active & ~fg_mask
                
            # Track GO stats
            go_mask = fourth_down_mask & (fourth_down_decisions == 'GO')
            self.fourth_down_goes_away[go_mask & self.possession_is_away] += 1
            self.fourth_down_goes_home[go_mask & ~self.possession_is_away] += 1

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
            
            # Extract names vectorially for active plays in this bucket
            is_away = self.possession_is_away[idx]
            qb_names = np.where(is_away, self.qb_starters[self.away_team], self.qb_starters[self.home_team])
            rb_names = np.where(is_away, self.rb_starters[self.away_team], self.rb_starters[self.home_team])

            yd = self.yardline_100[idx]
            zones = np.where(yd <= 5, 'goalline', np.where(yd <= 20, 'redzone', 'primary'))
            
            for zone in ['goalline', 'redzone', 'primary']:
                zone_mask = (zones == zone)
                if not np.any(zone_mask): continue
                
                sub_idx = np.where(idx)[0][zone_mask]
                
                is_away_zone = self.possession_is_away[sub_idx]
                
                away_qb = self.qb_starters[self.away_team]
                home_qb = self.qb_starters[self.home_team]
                away_rb = self.rb_starters[self.away_team]
                home_rb = self.rb_starters[self.home_team]

                cpoe_away = self.precomputed_qb_cpoe.get(away_qb, {}).get(zone, 0.0)
                cpoe_home = self.precomputed_qb_cpoe.get(home_qb, {}).get(zone, 0.0)
                target_share_away = self.precomputed_skill_target_share.get(away_rb, {}).get(zone, 0.0)
                target_share_home = self.precomputed_skill_target_share.get(home_rb, {}).get(zone, 0.0)
                carry_share_away = self.precomputed_skill_carry_share.get(away_rb, {}).get(zone, 0.0)
                carry_share_home = self.precomputed_skill_carry_share.get(home_rb, {}).get(zone, 0.0)

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

        # Apply PROE Overlay in logit space vectorially!
        proe_offset = np.where(self.possession_is_away, 
                               self.proe_offsets[self.away_team], 
                               self.proe_offsets[self.home_team])
        adjusted_pass_prob = _sigmoid(_logit(base_pass_prob) + proe_offset)
        adjusted_pass_prob = np.clip(adjusted_pass_prob, 0.01, 0.99)
        
        is_pass = active & (np.random.rand(self.N) < adjusted_pass_prob)
        is_run = active & ~is_pass
        
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
            # Target selection vectorially
            for team in [self.away_team, self.home_team]:
                team_mask = is_pass & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue
                
                recs, cum_shares = self.receivers_cache[team]
                r_vals = np.random.rand(np.sum(team_mask))
                rec_idx = np.searchsorted(cum_shares, r_vals)
                rec_idx = np.minimum(rec_idx, len(cum_shares) - 1)
                play_target[team_mask] = np.array(recs)[rec_idx]
                
            # QB TTT Distribution
            for team in [self.away_team, self.home_team]:
                team_mask = is_pass & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue
                
                qb = self.qb_starters[team]
                qb_dna = self.dna['qb'].get(qb, {})
                avg_ttt = qb_dna.get('avg_time_to_throw_sec', 2.7)
                sim_ttt = np.clip(np.random.normal(avg_ttt, 0.6, size=np.sum(team_mask)), 1.5, 4.5)
                play_ttt[team_mask] = sim_ttt

            # Trench Sacks Gate (Gate 2)
            # Build features for Gate 2
            g2_features = np.zeros((np.sum(is_pass), 11), dtype=np.float32)
            # Feature order in metadata: avg_time_to_throw_sec_qb, cpoe_qb, def_pressure_rate, def_sack_rate, sack_rate_allowed, off_sack_rate_l4, def_sack_rate_l4, down, ydstogo, yardline_100, score_differential
            
            # Map values based on team
            t_def_pressure_rate = np.zeros(self.N, dtype=np.float32)
            t_def_sack_rate = np.zeros(self.N, dtype=np.float32)
            t_sack_rate_allowed = np.zeros(self.N, dtype=np.float32)
            
            for team in [self.away_team, self.home_team]:
                opp = self.home_team if team == self.away_team else self.away_team
                team_mask = is_pass & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue
                
                t_off = self.dna['trench'].get(str(self.year), {}).get(team, {})
                t_def = self.dna['trench'].get(str(self.year), {}).get(opp, {})
                
                t_def_pressure_rate[team_mask] = t_def.get('def_pressure_rate', 0.15)
                t_def_sack_rate[team_mask] = t_def.get('def_sack_rate', 0.06)
                t_sack_rate_allowed[team_mask] = t_off.get('sack_rate_allowed', 0.06)

            qb_cpoe = np.zeros(self.N, dtype=np.float32)
            for team in [self.away_team, self.home_team]:
                team_mask = is_pass & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue
                qb = self.qb_starters[team]
                qb_cpoe[team_mask] = self.dna['qb'].get(qb, {}).get('cpoe', 0.0)

            X_g2 = np.stack([
                play_ttt[is_pass],
                qb_cpoe[is_pass],
                t_def_pressure_rate[is_pass],
                t_def_sack_rate[is_pass],
                t_sack_rate_allowed[is_pass],
                t_sack_rate_allowed[is_pass], # off_sack_rate_l4
                t_def_sack_rate[is_pass], # def_sack_rate_l4
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
            SACK_PROB_CALIBRATION_MULT = 1.24
            sack_prob = self.registry.chaos_model._g2_booster.inplace_predict(X_g2) * SACK_PROB_CALIBRATION_MULT
            has_sack = np.zeros(self.N, dtype=bool)
            has_sack[is_pass] = np.random.rand(np.sum(is_pass)) < sack_prob
            self.gross_sacks[has_sack] += 1

            # QB Scramble Escape Hatch
            if np.any(has_sack):
                scramble_rate = np.zeros(self.N, dtype=np.float32)
                for team in [self.away_team, self.home_team]:
                    team_mask = has_sack & (self.possession_is_away == (team == self.away_team))
                    if not np.any(team_mask): continue
                    qb = self.qb_starters[team]
                    scramble_rate[team_mask] = self.rosters[team].get(qb, {}).get('scramble_rate', 0.05)
                    
                escapes = has_sack & (np.random.rand(self.N) < scramble_rate)
                real_sacks = has_sack & ~escapes
                
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
                is_throwaway = real_sacks & (np.random.rand(self.N) < 0.06)
                actual_sacks = real_sacks & ~is_throwaway
                play_is_throwaway[is_throwaway] = True
                
                # Handle QB Scramble
                if np.any(escapes):
                    scramble_yds = np.random.normal(5, 4, size=np.sum(escapes)).astype(np.int32)
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
                self.net_sacks[actual_sacks] += 1
                if np.any(actual_sacks):
                    play_is_sack[actual_sacks] = True
                    
                    # Roll sack-fumble
                    qb_fumble_rate = np.zeros(self.N, dtype=np.float32)
                    for team in [self.away_team, self.home_team]:
                        team_mask = actual_sacks & (self.possession_is_away == (team == self.away_team))
                        if not np.any(team_mask): continue
                        qb = self.qb_starters[team]
                        qb_fumble_rate[team_mask] = self.dna['qb'].get(qb, {}).get('sack_rate', 0.06)
                        
                    fumble_prob = 0.0724 * (qb_fumble_rate / 0.06) * 0.80
                    fumbles = actual_sacks & (np.random.rand(self.N) < fumble_prob)
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
            if np.any(no_sack_pass):
                # Build Air Yards features
                # features: down, ydstogo, yardline_100, qtr, score_differential, half_seconds_remaining, game_seconds_remaining, cpoe_qb, avg_air_yards_per_att_qb, deep_ball_rate_qb, scramble_rate_qb, sack_rate_qb, play_action_rate_qb, under_pressure_cpoe_qb, ngs_aggressiveness_index_qb, avg_time_to_throw_sec_qb, target_share_recv, catch_rate_recv, avg_target_depth_yds_recv, deep_target_rate_recv, air_yards_share_recv, yac_per_reception_recv, avg_separation_yds_recv, air_yards_tendency_coach, deep_shot_rate_coach, screen_rate_coach, play_action_rate_coach, no_huddle_rate_coach, rpo_rate_coach, conservative_score_bias_coach, def_pressure_rate_def_trench, def_sack_rate_def_trench
                ay_feats = np.zeros((np.sum(no_sack_pass), 32), dtype=np.float32)
                
                # Fetch QB, Receiver, and Trench details
                # Build index maps for fast feature assignment
                for team in [self.away_team, self.home_team]:
                    team_mask = no_sack_pass & (self.possession_is_away == (team == self.away_team))
                    if not np.any(team_mask): continue
                    
                    opp = self.home_team if team == self.away_team else self.away_team
                    qb = self.qb_starters[team]
                    q_dna = self.dna['qb'].get(qb, {})
                    c_name = self.team_coaches.get(team, "Unknown")
                    c_dna = self.dna['coach'].get(c_name, {})
                    t_def = self.dna['trench'].get(str(self.year), {}).get(opp, {})
                    
                    # Fill QB/Coach/Trench features
                    # Feature array construction for batch
                    indices = np.where(team_mask[no_sack_pass])[0]
                    # Assign standard values vectorially
                    # ...
                
                # To bypass pd.DataFrame construction and do it extremely fast:
                # We can construct the 2D numpy array directly!
                # Let's map receiver DNA details:
                elusiveness = np.zeros(self.N, dtype=np.float32)
                broken_tackle_rate = np.zeros(self.N, dtype=np.float32)
                catch_rate = np.zeros(self.N, dtype=np.float32)
                primary_catch_rate = np.zeros(self.N, dtype=np.float32)
                redzone_catch_rate = np.zeros(self.N, dtype=np.float32)
                goalline_catch_rate = np.zeros(self.N, dtype=np.float32)
                contested_catch_rate_recv = np.zeros(self.N, dtype=np.float32)
                avg_sep = np.zeros(self.N, dtype=np.float32)
                target_share_recv = np.zeros(self.N, dtype=np.float32)
                avg_target_depth_yds_recv = np.zeros(self.N, dtype=np.float32)
                deep_target_rate_recv = np.zeros(self.N, dtype=np.float32)
                air_yards_share_recv = np.zeros(self.N, dtype=np.float32)
                yac_per_reception_recv = np.zeros(self.N, dtype=np.float32)
                avg_separation_yds_recv = np.zeros(self.N, dtype=np.float32)
                pos_recv = np.empty(self.N, dtype=object)

                for team in [self.away_team, self.home_team]:
                    team_mask = no_sack_pass & (self.possession_is_away == (team == self.away_team))
                    if not np.any(team_mask): continue
                    
                    for r_name in np.unique(play_target[team_mask]):
                        r_mask = team_mask & (play_target == r_name)
                        r_dna = self.dna['skill'].get(r_name, {})
                        
                        def get_safe_float(d, key, default):
                            val = d.get(key)
                            if val is None or isinstance(val, (dict, list)):
                                return default
                            try:
                                return float(val)
                            except (ValueError, TypeError):
                                return default

                        target_share_recv[r_mask] = get_safe_float(r_dna, 'target_share', 0.1)
                        catch_rate[r_mask] = get_safe_float(r_dna, 'catch_rate', 0.65)
                        avg_target_depth_yds_recv[r_mask] = get_safe_float(r_dna, 'avg_target_depth_yds', 8.0)
                        deep_target_rate_recv[r_mask] = get_safe_float(r_dna, 'deep_target_rate', 0.12)
                        air_yards_share_recv[r_mask] = get_safe_float(r_dna, 'air_yards_share', 0.1)
                        yac_per_reception_recv[r_mask] = get_safe_float(r_dna, 'yac_per_reception', 4.0)
                        avg_separation_yds_recv[r_mask] = get_safe_float(r_dna, 'avg_separation_yds', 2.9)
                        
                        splits = r_dna.get('splits', {})
                        primary_catch_rate[r_mask] = get_safe_float(splits.get('primary', {}), 'catch_rate', catch_rate[r_mask])
                        redzone_catch_rate[r_mask] = get_safe_float(splits.get('redzone', {}), 'catch_rate', catch_rate[r_mask])
                        goalline_catch_rate[r_mask] = get_safe_float(splits.get('goalline', {}), 'catch_rate', catch_rate[r_mask])
                        contested_catch_rate_recv[r_mask] = get_safe_float(r_dna, 'contested_catch_rate', 0.45)

                        elusiveness[r_mask] = get_safe_float(r_dna, 'elusiveness', 0.0)
                        broken_tackle_rate[r_mask] = get_safe_float(r_dna, 'broken_tackle_rate', 0.10)
                        avg_sep[r_mask] = get_safe_float(r_dna, 'avg_separation_yds', 2.9)
                        pos_recv[r_mask] = self.rosters[team].get(r_name, {}).get('pos', r_dna.get('pos', 'WR'))

                # Now stack the 32 features for AirYards sampler
                # Features are: down, ydstogo, yardline_100, qtr, score_differential, half_seconds_remaining, game_seconds_remaining
                # And the rest of standard DNA features
                ay_q_avg_air = np.zeros(self.N, dtype=np.float32)
                ay_q_deep = np.zeros(self.N, dtype=np.float32)
                ay_q_scramble = np.zeros(self.N, dtype=np.float32)
                ay_q_sack = np.zeros(self.N, dtype=np.float32)
                ay_q_pa = np.zeros(self.N, dtype=np.float32)
                ay_q_pressure = np.zeros(self.N, dtype=np.float32)
                ay_q_agg = np.zeros(self.N, dtype=np.float32)
                
                coach_ay_tend = np.zeros(self.N, dtype=np.float32)
                coach_deep = np.zeros(self.N, dtype=np.float32)
                coach_screen = np.zeros(self.N, dtype=np.float32)
                coach_pa = np.zeros(self.N, dtype=np.float32)
                coach_no_huddle = np.zeros(self.N, dtype=np.float32)
                coach_rpo = np.zeros(self.N, dtype=np.float32)
                coach_cons = np.zeros(self.N, dtype=np.float32)

                for team in [self.away_team, self.home_team]:
                    team_mask = no_sack_pass & (self.possession_is_away == (team == self.away_team))
                    if not np.any(team_mask): continue
                    qb = self.qb_starters[team]
                    q_dna = self.dna['qb'].get(qb, {})
                    c_name = self.team_coaches.get(team, "Unknown")
                    c_dna = self.dna['coach'].get(c_name, {})
                    
                    ay_q_avg_air[team_mask] = q_dna.get('avg_air_yards_per_att', 8.0)
                    ay_q_deep[team_mask] = q_dna.get('deep_ball_rate', 0.12)
                    ay_q_scramble[team_mask] = q_dna.get('scramble_rate', 0.05)
                    ay_q_sack[team_mask] = q_dna.get('sack_rate', 0.06)
                    ay_q_pa[team_mask] = q_dna.get('play_action_rate', 0.20)
                    ay_q_pressure[team_mask] = q_dna.get('under_pressure_cpoe', -2.0)
                    ay_q_agg[team_mask] = q_dna.get('ngs_aggressiveness_index', 15.0)
                    
                    coach_ay_tend[team_mask] = c_dna.get('air_yards_tendency', 0.0)
                    coach_deep[team_mask] = c_dna.get('deep_shot_rate', 0.12)
                    coach_screen[team_mask] = c_dna.get('screen_rate', 0.2)
                    coach_pa[team_mask] = c_dna.get('play_action_rate', 0.20)
                    coach_no_huddle[team_mask] = c_dna.get('no_huddle_rate', 0.05)
                    coach_rpo[team_mask] = c_dna.get('rpo_rate', 0.05)
                    coach_cons[team_mask] = c_dna.get('conservative_score_bias', 0.0)

                # Get zone per play in no_sack_pass
                yd_ay = self.yardline_100[no_sack_pass]
                zones_ay = np.where(yd_ay <= 5, 'goalline', np.where(yd_ay <= 20, 'redzone', 'primary'))
                
                is_away_ay = self.possession_is_away[no_sack_pass]
                qb_names_ay = np.where(is_away_ay, self.qb_starters[self.away_team], self.qb_starters[self.home_team])
                recv_names_ay = play_target[no_sack_pass]
                
                cpoe_by_filter_ay = np.zeros(np.sum(no_sack_pass), dtype=np.float32)
                target_share_by_filter_ay = np.zeros(np.sum(no_sack_pass), dtype=np.float32)
                carry_share_by_filter_ay = np.zeros(np.sum(no_sack_pass), dtype=np.float32)
                
                away_qb = self.qb_starters[self.away_team]
                home_qb = self.qb_starters[self.home_team]
                
                cpoe_away_vec = np.zeros_like(zones_ay, dtype=np.float32)
                cpoe_home_vec = np.zeros_like(zones_ay, dtype=np.float32)
                for zone in ['goalline', 'redzone', 'primary']:
                    z_mask = (zones_ay == zone)
                    if not np.any(z_mask): continue
                    cpoe_away_vec[z_mask] = self.precomputed_qb_cpoe.get(away_qb, {}).get(zone, 0.0)
                    cpoe_home_vec[z_mask] = self.precomputed_qb_cpoe.get(home_qb, {}).get(zone, 0.0)
                
                cpoe_by_filter_ay = np.where(is_away_ay, cpoe_away_vec, cpoe_home_vec)
                
                unique_recvs = np.unique(recv_names_ay)
                for r_n in unique_recvs:
                    if r_n is None or r_n == "Unknown": continue
                    r_mask = (recv_names_ay == r_n)
                    
                    for zone in ['goalline', 'redzone', 'primary']:
                        z_mask = r_mask & (zones_ay == zone)
                        if not np.any(z_mask): continue
                        
                        target_share_by_filter_ay[z_mask] = self.precomputed_skill_target_share.get(r_n, {}).get(zone, 0.0)
                        carry_share_by_filter_ay[z_mask] = self.precomputed_skill_carry_share.get(r_n, {}).get(zone, 0.0)
                
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
                
                int_prob = self.registry.chaos_model._g4_booster.inplace_predict(X_g4) * 0.80
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
                        zone_normal = np.where(yd_normal <= 5, 'goalline', np.where(yd_normal <= 20, 'redzone', 'primary'))
                        
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
                        sep_mean = avg_separation_yds_recv[is_normal]
                        sep_roll = np.maximum(0.0, np.random.normal(sep_mean, 1.0))
                        
                        # Split plays into Contested (sep_roll <= 1.0) and Open (sep_roll > 1.0)
                        is_contested = (sep_roll <= 1.0)
                        is_open = ~is_contested
                        
                        probs_normal = np.zeros(np.sum(is_normal), dtype=np.float32)

                        # Shared depth-decay model constants and helpers — used by both the
                        # contested and open-field paths below so completion probability tapers
                        # with target depth in both regimes, not just the open one. b1 < 0 means
                        # probability decreases as air_yards increases.
                        b0 = 1.5
                        b1 = -0.08

                        def _sigmoid_arr(x):
                            return 1.0 / (1.0 + np.exp(-x))

                        def _logit_arr(p):
                            # clip p to prevent log(0) or division by zero
                            p_clipped = np.clip(p, 0.01, 0.99)
                            return np.log(p_clipped / (1.0 - p_clipped))

                        # 1. CONTESTED PATH (sep_roll <= 1.0):
                        # Same depth-decay construction as the open-field path below, anchored to
                        # the receiver's real contested_catch_rate at their own average depth
                        # (ADOT) instead of their zone catch rate — a contested target thrown
                        # deeper than that receiver's typical depth is harder to complete than one
                        # thrown short, same as an open target is.
                        if np.any(is_contested):
                            ay_val_c = play_air_yards[is_normal][is_contested]
                            adot_val_c = avg_target_depth_yds_recv[is_normal][is_contested]
                            contested_wr_rate = contested_catch_rate_recv[is_normal][is_contested]
                            qb_cpoe_val_c = qb_cpoe[is_normal][is_contested] / 100.0

                            expected_baseline_p_c = _sigmoid_arr(b0 + b1 * adot_val_c)
                            delta_wr_c = _logit_arr(contested_wr_rate) - _logit_arr(expected_baseline_p_c)
                            logit_p_c = b0 + b1 * ay_val_c + delta_wr_c

                            # QB CPOE applied in probability space, post-sigmoid — same convention
                            # as the open-field path (see the Round 7 note below).
                            probs_normal[is_contested] = _sigmoid_arr(logit_p_c) + qb_cpoe_val_c

                        # 2. OPEN FIELD PATH (sep_roll > 1.0):
                        # Depth-decay model: logit(P_catch) = b0 + b1 * air_yards + delta_wr + delta_qb + 0.15 * (sep_roll - 1.0)
                        if np.any(is_open):
                            ay_val = play_air_yards[is_normal][is_open]
                            adot_val = avg_target_depth_yds_recv[is_normal][is_open]

                            # Calculate receiver offset delta_wr so that at their overall ADOT, the baseline expected P(Catch)
                            # is aligned with their zone baseline catch rate.
                            expected_baseline_p = _sigmoid_arr(b0 + b1 * adot_val)
                            delta_wr = _logit_arr(zone_baseline[is_open]) - _logit_arr(expected_baseline_p)

                            # Separation bonus
                            sep_bonus = 0.15 * (sep_roll[is_open] - 1.0)

                            # Logit sum (QB CPOE deliberately NOT included here — see below)
                            logit_p = b0 + b1 * ay_val + delta_wr + sep_bonus

                            # QB CPOE offset — applied in PROBABILITY space, after the sigmoid,
                            # matching the contested-catch path above (which already does this
                            # correctly). Round 7 fix (clock_physics_v020 doc, completion-rate
                            # investigation): this used to be added INSIDE the logit sum as
                            # `delta_qb = qb_cpoe/100`, which the sigmoid then compressed — a QB
                            # with +3.5 CPOE ended up with only ~+1.1pp of actual effect instead
                            # of the ~+3.5pp CPOE is defined to mean, since CPOE is itself a
                            # probability-space quantity (percentage points of completion rate
                            # over expected), not a logit-space one. Backtrack: revert to
                            # `logit_p = b0 + b1*ay_val + delta_wr + (qb_cpoe[is_normal][is_open]/100.0) + sep_bonus`
                            # and `probs_normal[is_open] = _sigmoid_arr(logit_p)` if this needs undoing.
                            qb_cpoe_val = qb_cpoe[is_normal][is_open] / 100.0

                            # Baseline calibration offset (Round 7, clock_physics_v020, Fix 2 —
                            # separate and independently backtrackable from Fix 1 above). Note
                            # `b0` mathematically cancels out of `delta_wr`'s construction
                            # (logit(sigmoid(x)) = x), so it has zero effect on the final
                            # probability — adjusting it would have been a no-op. Population-wide
                            # completion rate measured 69.70% post-Fix-1 vs. Cam's confirmed real
                            # 2025 target of 64.40% (weighted CMP/ATT across 50 QBs). Deep-zone and
                            # screen-zone completion were already accurate, so this gap is
                            # concentrated in the open-field/standard-zone path — sized to the
                            # measured standard-zone overshoot (+7.24pp before this fix).
                            # Backtrack: remove `- OPEN_FIELD_CALIBRATION_OFFSET` below.
                            OPEN_FIELD_CALIBRATION_OFFSET = 0.075
                            probs_normal[is_open] = _sigmoid_arr(logit_p) + qb_cpoe_val - OPEN_FIELD_CALIBRATION_OFFSET
                            
                        # Apply to catch_prob and clip final probabilities to [0.01, 0.99]
                        catch_prob[is_normal] = np.clip(probs_normal, 0.01, 0.99)
                    
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

                    # Resolve YAC for completed passes
                    if np.any(is_complete):
                        yac_ay = play_air_yards[is_complete].astype(np.float32)
                        
                        yd_yac = self.yardline_100[is_complete]
                        zones_yac = np.where(yd_yac <= 5, 'goalline', np.where(yd_yac <= 20, 'redzone', 'primary'))
                        
                        is_away_yac = self.possession_is_away[is_complete]
                        qb_names_yac = np.where(is_away_yac, self.qb_starters[self.away_team], self.qb_starters[self.home_team])
                        recv_names_yac = play_target[is_complete]
                        
                        cpoe_by_filter_yac = np.zeros(np.sum(is_complete), dtype=np.float32)
                        target_share_by_filter_yac = np.zeros(np.sum(is_complete), dtype=np.float32)
                        carry_share_by_filter_yac = np.zeros(np.sum(is_complete), dtype=np.float32)
                        
                        away_qb = self.qb_starters[self.away_team]
                        home_qb = self.qb_starters[self.home_team]
                        
                        cpoe_away_vec = np.zeros_like(zones_yac, dtype=np.float32)
                        cpoe_home_vec = np.zeros_like(zones_yac, dtype=np.float32)
                        for zone in ['goalline', 'redzone', 'primary']:
                            z_mask = (zones_yac == zone)
                            if not np.any(z_mask): continue
                            cpoe_away_vec[z_mask] = self.precomputed_qb_cpoe.get(away_qb, {}).get(zone, 0.0)
                            cpoe_home_vec[z_mask] = self.precomputed_qb_cpoe.get(home_qb, {}).get(zone, 0.0)
                        
                        cpoe_by_filter_yac = np.where(is_away_yac, cpoe_away_vec, cpoe_home_vec)
                        
                        unique_recvs = np.unique(recv_names_yac)
                        for r_n in unique_recvs:
                            if r_n is None or r_n == "Unknown": continue
                            r_mask = (recv_names_yac == r_n)
                            
                            for zone in ['goalline', 'redzone', 'primary']:
                                z_mask = r_mask & (zones_yac == zone)
                                if not np.any(z_mask): continue
                                
                                target_share_by_filter_yac[z_mask] = self.precomputed_skill_target_share.get(r_n, {}).get(zone, 0.0)
                                carry_share_by_filter_yac[z_mask] = self.precomputed_skill_carry_share.get(r_n, {}).get(zone, 0.0)
                                
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
                
                # Decline Logic:
                # Offensive Holding declined if there is an interception
                off_holding_accepted = pass_off_holding & ~play_is_interception
                # DPI declined if complete and play gain >= spot of foul (air yards)
                dpi_accepted = pass_dpi & ~(play_is_complete & (play_gain >= play_air_yards))
                # Defensive Holding declined if complete and play gain >= 5 yards
                def_holding_accepted = pass_def_holding & ~(play_is_complete & (play_gain >= 5))
                
                # Assign to global step variables
                play_is_off_holding[off_holding_accepted] = True
                play_is_dpi[dpi_accepted] = True
                play_is_def_holding[def_holding_accepted] = True
                
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

        # 4B. Execute RUN Plays
        if np.any(is_run):
            # Select rusher vectorially
            for team in [self.away_team, self.home_team]:
                team_mask = is_run & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue
                
                rushers, cum_shares = self.rusher_cache[team]
                r_vals = np.random.rand(np.sum(team_mask))
                rush_idx = np.searchsorted(cum_shares, r_vals)
                rush_idx = np.minimum(rush_idx, len(cum_shares) - 1)
                play_rusher[team_mask] = np.array(rushers)[rush_idx]

            # Resolve rush blocks and box density vectorially
            run_block_z = np.zeros(self.N, dtype=np.float32)
            box_density_z = np.zeros(self.N, dtype=np.float32)
            efficiency_z = np.zeros(self.N, dtype=np.float32)
            
            for team in [self.away_team, self.home_team]:
                team_mask = is_run & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue
                
                off_t = self.trench_tiers.get(team, {})
                run_block_z[team_mask] = 3.0 - off_t.get('run_block_tier', 3.0)
                box_density_z[team_mask] = off_t.get('qb_cpoe_z', 0.0) * -0.5
                
                # Rusher efficiency
                for r_name in np.unique(play_rusher[team_mask]):
                    r_mask = team_mask & (play_rusher == r_name)
                    player_traits = self.rosters[team].get(r_name, {})
                    ypc = player_traits.get('ypc', 4.2)
                    efficiency_z[r_mask] = -(ypc - 4.2) / 0.6

            # Stack features: yardline_100, ydstogo, game_seconds_remaining, score_differential, yac_att_z, btk_rate_z, run_block_z, box_density_z, efficiency_z
            yac_att_z = np.zeros(self.N, dtype=np.float32)
            btk_rate_z = np.zeros(self.N, dtype=np.float32)
            for team in [self.away_team, self.home_team]:
                team_mask = is_run & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue
                for r_name in np.unique(play_rusher[team_mask]):
                    r_mask = team_mask & (play_rusher == r_name)
                    player_traits = self.rosters[team].get(r_name, {})
                    
                    yac_per_att = player_traits.get('yac_per_att', 2.0)
                    yac_att_z[r_mask] = (yac_per_att - 2.0) / 0.4
                    
                    btk_rate = player_traits.get('btk_rate', 0.10)
                    btk_rate_z[r_mask] = (btk_rate - 0.10) / 0.04

            # Segment is_run by zone
            yd_run = self.yardline_100[is_run]
            zones_run = np.where(yd_run <= 5, 'goalline', np.where(yd_run <= 20, 'redzone', 'primary'))
            
            is_away_run = self.possession_is_away[is_run]
            qb_names_run = np.where(is_away_run, self.qb_starters[self.away_team], self.qb_starters[self.home_team])
            rusher_names_run = play_rusher[is_run]
            
            cpoe_by_filter_run = np.zeros(np.sum(is_run), dtype=np.float32)
            target_share_by_filter_run = np.zeros(np.sum(is_run), dtype=np.float32)
            carry_share_by_filter_run = np.zeros(np.sum(is_run), dtype=np.float32)
            
            away_qb = self.qb_starters[self.away_team]
            home_qb = self.qb_starters[self.home_team]
            
            cpoe_away_vec = np.zeros_like(zones_run, dtype=np.float32)
            cpoe_home_vec = np.zeros_like(zones_run, dtype=np.float32)
            for zone in ['goalline', 'redzone', 'primary']:
                z_mask = (zones_run == zone)
                if not np.any(z_mask): continue
                cpoe_away_vec[z_mask] = self.precomputed_qb_cpoe.get(away_qb, {}).get(zone, 0.0)
                cpoe_home_vec[z_mask] = self.precomputed_qb_cpoe.get(home_qb, {}).get(zone, 0.0)
            
            cpoe_by_filter_run = np.where(is_away_run, cpoe_away_vec, cpoe_home_vec)
            
            unique_rushers = np.unique(rusher_names_run)
            for r_n in unique_rushers:
                if r_n is None or r_n == "Unknown": continue
                r_mask = (rusher_names_run == r_n)
                
                for zone in ['goalline', 'redzone', 'primary']:
                    z_mask = r_mask & (zones_run == zone)
                    if not np.any(z_mask): continue
                    
                    target_share_by_filter_run[z_mask] = self.precomputed_skill_target_share.get(r_n, {}).get(zone, 0.0)
                    carry_share_by_filter_run[z_mask] = self.precomputed_skill_carry_share.get(r_n, {}).get(zone, 0.0)
                    
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
                
            run_multipliers = np.where(self.possession_is_away[is_run], self.run_mult_away_off, self.run_mult_home_off)
            gain = np.round((pred_unshifted + noise) * run_multipliers).astype(np.int32)
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
        # name) since they're unconditionally initialized at lines 720-722, while
        # `has_accepted_penalty` is only assigned inside the is_pass/is_run branches.
        self.penalties_accepted[play_is_off_holding | play_is_dpi | play_is_def_holding] += 1

        # -------------------------------------------------------------
        # Phase 5: Mid-Play Chaos (Fumbles)
        # -------------------------------------------------------------
        carrier = np.where(is_run, play_rusher, play_target)
        valid_carrier_mask = active & (is_run | (is_pass & play_is_complete)) & ~has_accepted_penalty
        
        if np.any(valid_carrier_mask):
            fumble_factor = np.zeros(self.N, dtype=np.float32)
            for team in [self.away_team, self.home_team]:
                team_mask = valid_carrier_mask & (self.possession_is_away == (team == self.away_team))
                if not np.any(team_mask): continue
                for c_name in np.unique(carrier[team_mask]):
                    if c_name is None or c_name == "Unknown": continue
                    c_mask = team_mask & (carrier == c_name)
                    carrier_dna = self.dna['skill'].get(c_name, {})
                    fumble_factor[c_mask] = carrier_dna.get('fumble_rate', 0.015) / 0.015
                    
            base_fumble_prob = np.where(is_run, 0.006520 * 0.80, 0.005246 * 0.80)
            fumble_prob = base_fumble_prob * fumble_factor
            
            fumbles = valid_carrier_mask & (np.random.rand(self.N) < fumble_prob)
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
        
        # Sacks (excluding lost fumbles)
        active_sack_no_lost_fumble = active & play_is_sack & ~play_is_fumble_lost & ~has_accepted_penalty
        if np.any(active_sack_no_lost_fumble):
            self.yardline_100[active_sack_no_lost_fumble] -= play_gain[active_sack_no_lost_fumble]
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

                # For non-slides, sample from Shifted Gamma
                is_active = ~is_slide
                n_active = np.sum(is_active)

                ret_yds = np.zeros(n_no_td, dtype=np.int32)
                if n_active > 0:
                    gamma_vals = np.random.gamma(shape=3.0552, scale=7.6877, size=n_active) - 7.3231
                    ret_yds[is_active] = np.round(gamma_vals).astype(np.int32)

                # Capped at remaining distance (yardline_100 - 1) to prevent unsanctioned TDs
                max_ret = self.yardline_100[int_no_td] - 1
                ret_yds = np.minimum(ret_yds, max_ret)

                self.yardline_100[int_no_td] = np.maximum(1, np.minimum(99, self.yardline_100[int_no_td] - ret_yds))
                turnover_return_yards[int_no_td] = np.maximum(0, ret_yds)

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
                self._switch_possession(scored=False, mask=fumble_no_td)
                
                n_no_td = np.sum(fumble_no_td)
                
                # Roll for securing ball (94.10% of non-TD lost fumbles)
                is_secure = np.random.rand(n_no_td) < 0.9410
                
                # For non-secure active returns, sample from Shifted Exponential
                is_active = ~is_secure
                n_active = np.sum(is_active)
                
                ret_yds = np.zeros(n_no_td, dtype=np.int32)
                if n_active > 0:
                    expon_vals = np.random.exponential(scale=23.7483, size=n_active) - 9.0
                    ret_yds[is_active] = np.round(expon_vals).astype(np.int32)
                
                # Capped at remaining distance (yardline_100 - 1) to prevent unsanctioned TDs
                max_ret = self.yardline_100[fumble_no_td] - 1
                ret_yds = np.minimum(ret_yds, max_ret)

                self.yardline_100[fumble_no_td] = np.maximum(1, np.minimum(99, self.yardline_100[fumble_no_td] - ret_yds))
                turnover_return_yards[fumble_no_td] = np.maximum(0, ret_yds)

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
            self.oob_plays[is_oob] += 1
            self.oob_eligible_plays[oob_eligible] += 1

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
            big_play = normal_play & (play_gain >= 20) & (is_run | (is_pass & play_is_complete))
            self.plays_over_20_yds[big_play] += 1

        # -------------------------------------------------------------
        # Phase 7: Clock Management
        # -------------------------------------------------------------
        # Incomplete Pass clock runoff
        inc_mask = active & is_pass & ~play_is_complete & ~play_is_sack & ~play_is_interception & ~play_is_fumble_lost & ~has_accepted_penalty
        if np.any(inc_mask):
            runoff = np.round(play_ttt[inc_mask] + np.maximum(0, play_air_yards[inc_mask]) / 10.0).astype(np.int32)
            self._run_clock(runoff, inc_mask)
            self.clock_stopped[inc_mask] = True

        # Scoring-play clock stop (offensive TD via normal_play — kickoff/punt/
        # turnover-return TDs are already excluded from `active`/regular_clock
        # via their own branches earlier). Bug fix: previously these fell
        # through into the normal running-clock treatment like any other play.
        # Live-play duration scales loosely with how far the ball traveled on
        # the score (a 1-yard punch-in is near-instant; a long house call takes
        # longer), matching the ~0-14s range real long plays show.
        scoring_play_clock = active & td_mask
        if np.any(scoring_play_clock):
            scoring_runoff = np.clip(np.round(3 + play_gain[scoring_play_clock] / 6.0), 2, 14).astype(np.int32)
            self._run_clock(scoring_runoff, scoring_play_clock)
            self.clock_stopped[scoring_play_clock] = True

        # Turnover-on-downs clock stop. Bug fix: previously fell through into
        # the normal running-clock treatment. No return happens (ball's just
        # dead short of the sticks), so this is a short tackle-to-whistle
        # duration, not a variable return-length one.
        turnover_down_clock = active & turnover_down
        if np.any(turnover_down_clock):
            self._run_clock(np.random.randint(4, 8, size=np.sum(turnover_down_clock)), turnover_down_clock)
            self.clock_stopped[turnover_down_clock] = True

        # Turnover clock runoff — scaled by actual return length (hoisted
        # turnover_return_yards) instead of a flat constant.
        turnover_clock = active & (play_is_interception | play_is_fumble_lost) & ~has_accepted_penalty
        if np.any(turnover_clock):
            turnover_runoff = np.clip(np.round(4 + turnover_return_yards[turnover_clock] / 5.0), 4, 14).astype(np.int32)
            self._run_clock(turnover_runoff, turnover_clock)
            self.clock_stopped[turnover_clock] = True

        # Penalty clock runoff (Accepted mid-play penalties)
        penalty_clock = active & has_accepted_penalty
        if np.any(penalty_clock):
            self._run_clock(10, penalty_clock)
            self.clock_stopped[penalty_clock] = True

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
                # Set False BEFORE the call, not after — _run_clock may set some
                # of these lanes True internally (two-minute warning clamp), and
                # an unconditional overwrite afterward would stomp on that.
                self.clock_stopped[normal_oob] = False
                pace_draw = self._sample_pace_runoff(normal_oob)
                # Play-type-specific reduction (confirmed via real data controlling
                # for play type: runs save ~2.18s going OOB, completed passes save
                # ~4.24s — a flat 3-5s for both overstated the run case).
                n_oob = int(np.sum(normal_oob))
                run_reduction = np.random.randint(1, 4, size=n_oob)
                pass_reduction = np.random.randint(3, 6, size=n_oob)
                oob_reduction = np.where(is_run[normal_oob], run_reduction, pass_reduction)
                self._run_clock(np.maximum(pace_draw - oob_reduction, 4), normal_oob)
            if np.any(in_crunch):
                self._run_clock(np.random.randint(3, 8, size=np.sum(in_crunch)), in_crunch)
                self.clock_stopped[in_crunch] = True

        # Normal run/complete clock runoff
        regular_clock = active & ~inc_mask & ~turnover_clock & ~penalty_clock & ~scoring_play_clock & ~turnover_down_clock & ~oob_clock
        if np.any(regular_clock):
            # Set False BEFORE any _run_clock calls below, not after — the
            # two-minute warning clamp inside _run_clock may set some of these
            # lanes True, and an unconditional overwrite afterward would stomp
            # on that (this was a real bug, caught via the sim_check pbp log).
            self.clock_stopped[regular_clock] = False
            is_hurry_reg = regular_clock & is_hurry
            self._run_clock(10, is_hurry_reg)

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
            self._run_clock(self._sample_pace_runoff(is_normal_reg), is_normal_reg)

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

        # +2s global calibration nudge, cumulative (clock_physics_v020):
        # round 3 added +1s (post-OOB/2-min-warning/squeeze overshoot was
        # ~7-9%, ~67 offensive plays/team vs. real ~62) which only recovered
        # ~4 plays/game. Round 4 adds a second +1s per Cam's call — still 8
        # plays (4/team) high after round 3. Propagates into OOB's runoff too
        # (draws from this same pool before its reduction) — intentional,
        # it's a broad "time between plays" adjustment, not play-type-specific.
        return np.round(result).astype(np.int32) + 2

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
        end_qtr = mask & (self.time_remaining <= 0)
        if np.any(end_qtr):
            # Bug fix (found via manual play-by-play review, clock_physics_v020):
            # `pre_quarter` must be captured before any mutation below. The old
            # code read self.quarter a second time (for `end_game`) AFTER
            # already incrementing it for lanes moving Q3->Q4, so those lanes
            # satisfied `quarter >= 4` immediately and the game was marked over
            # the instant Q4 began — meaning Q4 was never actually simulated.
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
            # block runs from inside `_run_clock`, called AFTER the current
            # play's real yardage/down/distance have already been applied this
            # same step. Directly overwriting yardline_100/down/distance here
            # discarded that just-resolved play's real result every single
            # game, right at the Q2->Q3 boundary. It was also redundant: the
            # kickoff-resolution code (`ko_mask = active & self.needs_kickoff`,
            # top of simulate_play_step) already computes the correct
            # down/distance/yardline_100 on its own next call (touchback vs.
            # return), exactly like it does for touchdown/field-goal kickoffs
            # — it doesn't need or expect this block to pre-set them. Fix:
            # only flip possession (nothing else does, unlike a score, which
            # flips it via _switch_possession) and defer to the kickoff, same
            # pattern already used for every other needs_kickoff trigger.
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

            # Game Over — only for lanes that were ALREADY in Q4 (i.e. Q4 just
            # expired), not lanes that just transitioned into Q4 this call.
            end_game = end_qtr & (pre_quarter >= 4)
            self.game_over[end_game] = True


    def _switch_possession(self, scored=False, mask=None):
        if mask is None:
            mask = np.ones(self.N, dtype=bool)
        if not np.any(mask): return

        self.possession_is_away[mask] = ~self.possession_is_away[mask]
        self.yardline_100[mask] = np.where(scored, 70, 100 - self.yardline_100[mask])
        self.down[mask] = 1
        self.distance[mask] = 10
        if not scored:
            self.drives_count[mask] += 1  # diagnostic only — new drive for the team taking over (scored=True cases start their new drive via the ensuing kickoff, already counted there)

    def get_stats_report(self) -> list:
        """Converts the internal stats dict to a list of dicts for analysis."""
        rows = []
        for team in [self.away_team, self.home_team]:
            for player, stats_dict in self.player_stats[team].items():
                # For each simulation, we'll output a row
                # But to preserve existing structure: we need average stats or raw game-by-game stats!
                # Wait, in sequential engine: get_stats_report returns a list of players with scalar stats.
                # Since we ran N simulations concurrently, let's see how batch.py handles it!
                # In batch.py:
                # for row in sim.get_stats_report():
                #     row['game_id'] = game_id
                # So batch.py expects game-by-game player stats!
                # Let's extract the game-by-game stats vectorially!
                # For each game_id from 0 to N-1, we yield a dict for each player!
                pass
        return rows

    def get_game_summaries(self) -> list:
        summaries = []
        for i in range(self.N):
            winner = self.away_team if self.score_away[i] > self.score_home[i] else self.home_team
            summaries.append({
                'game_id': i,
                'off_score': int(self.score_away[i]),
                'def_score': int(self.score_home[i]),
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
        winner = np.where(self.score_away > self.score_home, self.away_team, self.home_team)
        
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
                    'def_sack': def_sack.astype(np.int32),
                    'def_int': def_int.astype(np.int32),
                    'def_fumble_rec': def_fumble_rec.astype(np.int32),
                    'def_td': def_td.astype(np.int32),
                    'pts_allowed': pts_allowed.astype(np.int32),
                    'dk_score': np.round(dk_score, 2),
                    'fd_score': np.round(fd_score, 2),
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
