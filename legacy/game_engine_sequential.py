import pandas as pd
import numpy as np
import os
import json
from src.nfl_sim.model_registry import ModelRegistry
from src.nfl_sim.proe_overlay_v_0_1_0 import apply_proe_overlay

class SequentialNFLGameEngine:
    def __init__(self, away_team, home_team, year=2025, dna=None, team_coaches=None, rosters=None, trench_tiers=None):
        self.away_team = away_team
        self.home_team = home_team
        self.year = year
        self.registry = ModelRegistry()
        
        # Load New DNA Registry (or use provided in-memory dicts)
        self.dna = dna if dna is not None else {
            'qb': self._load_json('data/dna/qb_dna.json'),
            'skill': self._load_skill_dna(),
            'coach': self._load_json('data/dna/coach_dna.json'),
            'trench': self._load_json('data/dna/trench_dna.json')
        }
        
        # Dynamically inject V.0.2.0 player DNA into loaded YAC model
        if hasattr(self.registry, 'yac_model') and self.registry.yac_model is not None:
            self.registry.yac_model.receiver_dna = self.dna['skill']
            self.registry.yac_model.qb_dna = self.dna['qb']
        
        # Team Mappings
        self.team_coaches = team_coaches if team_coaches is not None else self._load_json('data/dna/team_to_coach_2025.json')
        self.rosters = rosters if rosters is not None else {
            away_team: self._load_json(f"data/current_rosters/{away_team}_traits_{self.year}.json").get('traits', {}),
            home_team: self._load_json(f"data/current_rosters/{home_team}_traits_{self.year}.json").get('traits', {})
        }
        
        # Trench Tiers
        self.trench_tiers = trench_tiers if trench_tiers is not None else self._load_json('data/dna/trench_tiers_2025.json')
        
        # Game State
        self.possession = away_team
        self.defending = home_team
        self.yardline_100 = 70
        self.down = 1
        self.distance = 10
        self.scores = {away_team: 0, home_team: 0}
        self.quarter = 1
        self.time_remaining = 900 # Seconds
        self.game_over = False
        self.needs_kickoff = True
        self.clock_stopped = True
        self.timeouts = {away_team: 3, home_team: 3}

        # Initialize Stats tracking for all rostered players
        self.player_stats = {away_team: {}, home_team: {}}
        for team in [away_team, home_team]:
            for player, traits in self.rosters[team].items():
                self.player_stats[team][player] = {
                    'Pos': traits.get('pos', 'WR/TE'),
                    'rAtt': 0, 'rYds': 0, 'rTD': 0,
                    'pAtt': 0, 'pCmp': 0, 'pYds': 0, 'pTD': 0, 'int': 0,
                    'rec': 0, 'recYds': 0, 'recTD': 0, 'targets': 0,
                    'fumbles': 0, 'sacks_taken': 0, 'fumbles_lost': 0,
                    'def_sack': 0, 'def_int': 0, 'def_fumble_rec': 0, 'def_td': 0, 'pts_allowed': 0
                }
            self.player_stats[team]["Defense"] = {
                'Pos': 'DST',
                'rAtt': 0, 'rYds': 0, 'rTD': 0,
                'pAtt': 0, 'pCmp': 0, 'pYds': 0, 'pTD': 0, 'int': 0,
                'rec': 0, 'recYds': 0, 'recTD': 0, 'targets': 0,
                'fumbles': 0, 'sacks_taken': 0, 'fumbles_lost': 0,
                'def_sack': 0, 'def_int': 0, 'def_fumble_rec': 0, 'def_td': 0, 'pts_allowed': 0
            }
                
        # Track additional game metrics
        self.play_count = 0
        self.fg_attempts = {away_team: 0, home_team: 0}
        self.fg_made = {away_team: 0, home_team: 0}
        self.fourth_down_goes = {away_team: 0, home_team: 0}
        self.longest_pass = {away_team: -99, home_team: -99}
        self.shortest_pass = {away_team: 999, home_team: 999}
        self.longest_run = {away_team: -99, home_team: -99}
        self.shortest_run = {away_team: 999, home_team: 999}
        self.plays_over_20_yds = 0
        self.punts_run = 0
        self.fourth_down_decisions = []
        self.fg_attempts_details = []
        self.td_details = []

    def run_game(self):
        """Runs the simulation until game_over is True."""
        while not self.game_over:
            self.simulate_play()
        
        # Post-game defensive stats
        self.player_stats[self.away_team]['Defense']['pts_allowed'] = self.scores[self.home_team]
        self.player_stats[self.home_team]['Defense']['pts_allowed'] = self.scores[self.away_team]

    def get_stats_report(self) -> list:
        """Converts the internal stats dict to a list of dicts for analysis."""
        rows = []
        for team, players in self.player_stats.items():
            for name, stats in players.items():
                row = stats.copy()
                row['Player'] = name
                row['Team'] = team
                
                pos = row['Pos']
                pYds = row['pYds']
                pTD = row['pTD']
                intercepts = row['int']
                rYds = row['rYds']
                rTD = row['rTD']
                rec = row['rec']
                recYds = row['recYds']
                recTD = row['recTD']
                fumbles = row['fumbles']
                
                if pos == 'DST':
                    def_sack = row['def_sack']
                    def_int = row['def_int']
                    def_fumble_rec = row['def_fumble_rec']
                    def_td = row['def_td']
                    pts_allowed = row['pts_allowed']
                    
                    dk_pts_allowed_bonus = 0.0
                    if pts_allowed == 0:
                        dk_pts_allowed_bonus = 10.0
                    elif 1 <= pts_allowed <= 6:
                        dk_pts_allowed_bonus = 7.0
                    elif 7 <= pts_allowed <= 13:
                        dk_pts_allowed_bonus = 4.0
                    elif 14 <= pts_allowed <= 20:
                        dk_pts_allowed_bonus = 1.0
                    elif 21 <= pts_allowed <= 27:
                        dk_pts_allowed_bonus = 0.0
                    elif 28 <= pts_allowed <= 34:
                        dk_pts_allowed_bonus = -1.0
                    else:
                        dk_pts_allowed_bonus = -4.0
                        
                    row['dk_score'] = round(def_sack * 1.0 + def_int * 2.0 + def_fumble_rec * 2.0 + def_td * 6.0 + dk_pts_allowed_bonus, 2)
                    row['fd_score'] = row['dk_score']
                else:
                    dk_score = (
                        pYds * 0.04 + pTD * 4.0 - intercepts * 1.0 +
                        rYds * 0.1 + rTD * 6.0 +
                        rec * 1.0 + recYds * 0.1 + recTD * 6.0 -
                        fumbles * 1.0 +
                        (3.0 if pYds >= 300 else 0.0) +
                        (3.0 if rYds >= 100 else 0.0) +
                        (3.0 if recYds >= 100 else 0.0)
                    )
                    fd_score = (
                        pYds * 0.04 + pTD * 4.0 - intercepts * 2.0 +
                        rYds * 0.1 + rTD * 6.0 +
                        rec * 0.5 + recYds * 0.1 + recTD * 6.0 -
                        fumbles * 2.0
                    )
                    row['dk_score'] = round(dk_score, 2)
                    row['fd_score'] = round(fd_score, 2)
                rows.append(row)
        return rows

    def simulate_play(self) -> str:
        """Wrapper to prefix play with quarter, clock, situation, and scores."""
        if self.game_over: return "Game Over"
        situation_str = self._format_situation()
        play_desc = self._simulate_play_logic()
        return f"{situation_str} {play_desc}"

    def _simulate_play_logic(self) -> str:
        """Main Orchestration Loop (The 7 Phases)"""
        if self.game_over: return "Game Over"

        # -------------------------------------------------------------
        # STRATEGIC PHASE: Kneel, Spike, Timeout, and Clock Bleed Logic
        # -------------------------------------------------------------
        is_hurry = (self.quarter == 4 and self.time_remaining < 120 and self.scores[self.possession] < self.scores[self.defending])
        
        # 1. Victory Formation (Kneel Down)
        if self.scores[self.possession] > self.scores[self.defending] and self.quarter == 4 and self.time_remaining < 120:
            max_bleed_time = (4 - self.down) * 40 - (self.timeouts[self.defending] * 40)
            if self.time_remaining < max_bleed_time:
                cost = min(40, self.time_remaining)
                self._run_clock(cost)
                self.down += 1
                if self.down > 4 or self.time_remaining <= 0:
                    self.game_over = True
                    return "[STRATEGY] Victory Formation - Kneel down. Game Over!"
                return f"[STRATEGY] Victory Formation - Kneel down (-{cost}s)."

        # 2. Spike Logic (Hurry up clock stop)
        if is_hurry and not self.clock_stopped and self.timeouts[self.possession] == 0 and self.down < 4:
            cost = 15
            if self.time_remaining < cost:
                self.time_remaining = 0
                self.game_over = True
                return "[STRATEGY] Trailing offense could not spike in time! Game Over."
            self._run_clock(cost)
            self.down += 1
            self.clock_stopped = True
            return f"[STRATEGY] Clock Spiked to stop the clock (-{cost}s, Down: {self.down})."

        # 3. Timeout & Pre-Play Runoff Logic
        if not self.clock_stopped:
            # Check if trailing offense wants to use a timeout
            if is_hurry and self.timeouts[self.possession] > 0:
                self.timeouts[self.possession] -= 1
                self.clock_stopped = True
                return "[STRATEGY] Timeout called by Offense to stop the clock."
            
            # Check if leading defense wants to use a timeout to preserve time
            elif self.quarter == 4 and self.time_remaining < 120 and self.scores[self.possession] > self.scores[self.defending] and self.timeouts[self.defending] > 0:
                self.timeouts[self.defending] -= 1
                self.clock_stopped = True
                return "[STRATEGY] Timeout called by Defense to stop the clock."

        # -------------------------------------------------------------
        # CORE 7-PHASE PLAY SIMULATION
        # -------------------------------------------------------------
        if self.needs_kickoff: return self._handle_kickoff()

        # Phase 1: Pre-Snap
        penalty = self._check_pre_snap_penalties()
        if penalty: return penalty

        # Phase 2: 4th Down Evaluation
        if self.down == 4:
            decision = self._evaluate_fourth_down()
            self.fourth_down_decisions.append({
                "yardline_100": int(self.yardline_100),
                "distance": int(self.distance),
                "decision": str(decision)
            })
            if decision == "PUNT": return self._execute_punt()
            if decision == "FIELD_GOAL": return self._execute_field_goal()

        # Phase 3: Play Selection (Run/Pass)
        is_pass = self._select_play_type()

        # Phase 4: Execution
        if is_pass:
            play_result = self._execute_pass_play()
        else:
            play_result = self._execute_run_play()

        # Phase 5: Mid-Play Chaos
        play_result = self._apply_chaos(play_result)

        # Track additional game metrics
        self.play_count += 1
        pos = self.possession
        yards = play_result.get('yards', 0)
        
        # Check for play over 20 yards
        is_interception = play_result.get('is_interception', False)
        is_fumble_lost = play_result.get('is_fumble_lost', False)
        if yards >= 20 and not is_interception and not is_fumble_lost:
            if play_result.get('type') == 'RUN' or (play_result.get('type') == 'PASS' and play_result.get('is_complete', False)):
                self.plays_over_20_yds += 1
        
        if play_result.get('type') == 'RUN':
            if yards > self.longest_run[pos]:
                self.longest_run[pos] = yards
            if yards < self.shortest_run[pos]:
                self.shortest_run[pos] = yards
        elif play_result.get('type') == 'PASS' and play_result.get('is_complete', False):
            if yards > self.longest_pass[pos]:
                self.longest_pass[pos] = yards
            if yards < self.shortest_pass[pos]:
                self.shortest_pass[pos] = yards

        # Phase 6: Finalize State
        narrative = self._finalize_play(play_result)

        # Phase 7: Clock Management
        self._manage_clock(play_result)

        return narrative

    # =========================================================================
    # PHASE IMPLEMENTATIONS
    # =========================================================================

    def _check_pre_snap_penalties(self):
        """Phase 1: Roll for false starts, offsides, etc. using Gate 1."""
        game_sec = self._get_game_sec()
        score_diff = self.scores[self.possession] - self.scores[self.defending]
        is_home = 1.0 if self.possession == self.home_team else 0.0
        
        features = {
            'down': self.down,
            'ydstogo': self.distance,
            'yardline_100': self.yardline_100,
            'score_differential': score_diff,
            'game_seconds_remaining': game_sec,
            'is_home': is_home
        }
        
        prob = self.registry.predict_presnap_penalty_proba(features)
        
        if np.random.random() < prob:
            # Determine offense vs defense penalty (empirical: ~50% offense False Start, ~50% defense Offsides)
            if np.random.random() < 0.50:
                # Offensive penalty: False Start, move back 5 yards (yardline_100 increases)
                penalty_yds = 5
                self.yardline_100 = min(99, self.yardline_100 + penalty_yds)
                self.distance += penalty_yds
                return f"PENALTY: False Start on {self.possession}, 5 yards."
            else:
                # Defensive penalty: Offsides, move forward 5 yards (yardline_100 decreases)
                penalty_yds = min(5, self.yardline_100 - 1)
                self.yardline_100 -= penalty_yds
                self.distance = max(1, self.distance - penalty_yds)
                return f"PENALTY: Offsides on {self.defending}, 5 yards."
        return None

    def _evaluate_fourth_down(self):
        """Phase 2: 4th Down Bot + Coach Aggression"""
        game_sec = self._get_game_sec()
        score_diff = self.scores[self.possession] - self.scores[self.defending]
        features = [self.yardline_100, self.distance, game_sec, score_diff, self.timeouts[self.possession]]
        
        probas = self.registry.predict_4th_down_probas(features)
        
        # Apply Coach Adjustment
        coach_name = self.team_coaches.get(self.possession, "Unknown")
        coach_dna = self.dna['coach'].get(coach_name, {})
        aggression = (coach_dna.get('deep_shot_rate', 0.12) - 0.12) * 2.0
        probas[2] += aggression # Bias the 'Go' probability
        
        decision = ["PUNT", "FIELD_GOAL", "GO"][np.argmax(probas)]
        if decision == "GO":
            self.fourth_down_goes[self.possession] += 1
        return decision

    def _select_play_type(self) -> bool:
        """Phase 3: Play Selection (Situational Buckets + PROE)"""
        game_sec = self._get_game_sec()
        score_diff = self.scores[self.possession] - self.scores[self.defending]
        leverage = score_diff * game_sec
        
        # V.0.1.0 Feature Vector: [yardline_100, game_seconds_remaining, score_differential, timeouts_off, timeouts_def, leverage]
        features = [
            float(self.yardline_100), 
            float(game_sec), 
            float(score_diff), 
            float(self.timeouts[self.possession]), 
            float(self.timeouts[self.defending]), 
            float(leverage)
        ]
        
        state = {
            'down': self.down, 
            'distance': self.distance,
            'active_qb': self._get_starter('QB'),
            'active_rb': self._get_starter('RB'),
            'posteam_coach': self.team_coaches.get(self.possession, "Unknown"),
            'yardline_100': self.yardline_100
        }
        base_pass_prob = self.registry.predict_play_selection_proba(state, features)
        
        # Apply PROE Overlay!
        adjusted_pass_prob = apply_proe_overlay(
            base_pass_prob,
            team=self.possession
        )
        
        return np.random.random() < adjusted_pass_prob

    def _execute_pass_play(self):
        """Phase 4A: Passing Resolution (Double Hurdle) with Chaos Sacks/Interceptions"""
        qb_name = self._get_starter('QB')
        qb_stats = self._get_player_stat(self.possession, qb_name)
        team_roster = self.rosters[self.possession]
        
        # 1. Target Selection (Roll for 5 receivers based on target share)
        target = self._select_target_from_top_5()
        target_stats = self._get_player_stat(self.possession, target)
        
        # 2. QB DNA & TTT Distribution Pull
        qb_dna = self.dna['qb'].get(qb_name, {})
        avg_ttt = qb_dna.get('avg_time_to_throw_sec', 2.7)
        sim_ttt = np.clip(np.random.normal(avg_ttt, 0.6), 1.5, 4.5)
        
        # 3. Trench Sacks Gate (Gate 2)
        t_dna_off = self.dna['trench'].get(str(self.year), {}).get(self.possession, {})
        t_dna_def = self.dna['trench'].get(str(self.year), {}).get(self.defending, {})
        
        def_pressure_rate = t_dna_def.get('def_pressure_rate', 0.15)
        def_sack_rate_dna = t_dna_def.get('def_sack_rate', 0.06)
        sack_rate_allowed_dna = t_dna_off.get('sack_rate_allowed', 0.06)
        
        score_diff = self.scores[self.possession] - self.scores[self.defending]
        
        g2_features = {
            'avg_time_to_throw_sec_qb': sim_ttt,
            'cpoe_qb': qb_dna.get('cpoe', 0.0),
            'def_pressure_rate': def_pressure_rate,
            'def_sack_rate': def_sack_rate_dna,
            'sack_rate_allowed': sack_rate_allowed_dna,
            'off_sack_rate_l4': sack_rate_allowed_dna,
            'def_sack_rate_l4': def_sack_rate_dna,
            'down': self.down,
            'ydstogo': self.distance,
            'yardline_100': self.yardline_100,
            'score_differential': score_diff
        }
        
        sack_prob = self.registry.predict_sack_proba(g2_features)
        
        if np.random.random() < sack_prob:
            # Roll for QB Scramble escape hatch (Sourced from dynamically loaded roster traits)
            scramble_rate = self.rosters[self.possession].get(qb_name, {}).get('scramble_rate', 0.05)
            if np.random.random() < scramble_rate:
                # QB escapes the sack and scrambles!
                scramble_yards = max(-2, min(int(np.random.normal(5, 4)), self.yardline_100))
                qb_stats['rAtt'] += 1
                qb_stats['rYds'] += scramble_yards
                return {
                    "type": "RUN",
                    "yards": scramble_yards,
                    "is_complete": False,
                    "is_scramble": True,
                    "target": None
                }
                
            # Roll throwaways (20% of non-scramble pressures are thrown away)
            if np.random.random() < 0.20:
                qb_stats['pAtt'] += 1
                return {
                    "type": "PASS",
                    "yards": 0,
                    "is_complete": False,
                    "is_throwaway": True,
                    "target": None,
                    "air_yards": 0,
                    "ttt": sim_ttt
                }
                
            # Sack hits! Roll for Sack-Fumble (Gate 3)
            qb_fumble_rate = qb_dna.get('sack_rate', 0.06)
            fumble_mult = qb_fumble_rate / 0.06
            fumble_prob = 0.0724 * fumble_mult * 0.80  # 20% elite QB reduction
            
            is_fumble = np.random.random() < fumble_prob
            is_fumble_lost = False
            if is_fumble:
                is_fumble_lost = np.random.random() < 0.50
                
            # Sample Sack yards lost from Gamma distribution
            loss = self.registry.sample_sack_yards()
            
            # Record stats
            qb_stats['sacks_taken'] += 1
            self.player_stats[self.defending]['Defense']['def_sack'] += 1
            if is_fumble:
                qb_stats['fumbles'] += 1
                if is_fumble_lost:
                    qb_stats['fumbles_lost'] = qb_stats.get('fumbles_lost', 0) + 1
                    self.player_stats[self.defending]['Defense']['def_fumble_rec'] += 1
                    
            return {
                "type": "SACK",
                "yards": loss,
                "is_complete": False,
                "is_fumble": is_fumble,
                "is_fumble_lost": is_fumble_lost,
                "target": None
            }
            
        # 4. Extract DNA Features for Air Yards V.0.1.1
        coach_name = self.team_coaches.get(self.possession, "Unknown")
        feat_dict = self._build_air_yards_features(qb_name, target, coach_name)
        feat_dict['passer_player_name'] = qb_name
        feat_dict['receiver_player_name'] = target
        feat_dict['avg_time_to_throw_sec_qb'] = sim_ttt
        
        # 5. Predict Air Yards (Pass dictionary directly to bypass DataFrame overhead)
        ay = self.registry.predict_air_yards(feat_dict)
        ay = max(-5, min(int(ay), self.yardline_100))
        
        # 6. Interception Gate (Gate 4)
        g4_features = {
            'air_yards': ay,
            'cpoe_qb': qb_dna.get('cpoe', 0.0),
            'off_int_rate_l4': 0.023,
            'down': self.down,
            'ydstogo': self.distance,
            'yardline_100': self.yardline_100,
            'score_differential': score_diff
        }
        int_prob = self.registry.predict_interception_proba(g4_features) * 0.80  # 20% elite QB reduction
        
        if np.random.random() < int_prob:
            # Record target targets
            qb_stats['pAtt'] += 1
            target_stats['targets'] += 1
            return {
                "type": "PASS",
                "yards": 0,
                "is_complete": False,
                "is_interception": True,
                "target": target,
                "air_yards": ay,
                "ttt": sim_ttt
            }
            
        # 7. Completion Roll
        target_dna = self.dna['skill'].get(target, {})
        pos_recv = team_roster.get(target, {}).get('pos', target_dna.get('pos', 'WR'))
        
        if ay <= 0:
            if pos_recv == 'RB':
                catch_prob = 0.80
            else:
                catch_prob = 0.95
        else:
            zone = 'goalline' if self.yardline_100 <= 5 else 'redzone' if self.yardline_100 <= 20 else 'primary'
            splits = target_dna.get('splits', {})
            zone_baseline = target_dna.get('catch_rate', 0.65)
            if zone in splits and 'catch_rate' in splits[zone]:
                zone_baseline = float(splits[zone]['catch_rate'])
                if zone_baseline == 0.0:
                    zone_baseline = target_dna.get('catch_rate', 0.65)
                    
            avg_sep = target_dna.get('avg_separation_yds', 2.9)
            sep_roll = max(0.0, np.random.normal(avg_sep, 1.0))
            
            if sep_roll <= 1.0:
                contested_wr_rate = target_dna.get('contested_catch_rate', 0.45)
                qb_cpoe = qb_dna.get('cpoe', 0.0) / 100.0
                catch_prob = contested_wr_rate + qb_cpoe
            else:
                b0 = 1.5
                b1 = -0.08
                adot = target_dna.get('avg_target_depth_yds', 8.0)
                
                def sigmoid(x):
                    return 1.0 / (1.0 + np.exp(-x))
                def logit(p):
                    p_clipped = np.clip(p, 0.01, 0.99)
                    return np.log(p_clipped / (1.0 - p_clipped))
                    
                expected_baseline_p = sigmoid(b0 + b1 * adot)
                delta_wr = logit(zone_baseline) - logit(expected_baseline_p)
                delta_qb = qb_dna.get('cpoe', 0.0) / 100.0
                sep_bonus = 0.15 * (sep_roll - 1.0)
                
                logit_p = b0 + b1 * ay + delta_wr + delta_qb + sep_bonus
                catch_prob = sigmoid(logit_p)
                
            catch_prob = np.clip(catch_prob, 0.01, 0.99)
            
        is_complete = np.random.random() < catch_prob
        
        qb_stats['pAtt'] += 1
        target_stats['targets'] += 1
        
        gain = 0
        if is_complete:
            # 8. YAC Resolution (Double Hurdle V.0.1.1 YAC)
            yac_features = {
                "air_yards": ay,
                "yardline_100": self.yardline_100,
                "ydstogo": self.distance,
                "score_differential": score_diff,
                "game_seconds_remaining": self._get_game_sec(),
                "timeouts_pos": self.timeouts[self.possession],
                "timeouts_def": self.timeouts[self.defending],
                "receiver_name": target,
                "qb_name": qb_name
            }
            yac = max(0, int(self.registry.predict_yac(yac_features)))
            
            # Inject tail YAC noise
            rec_elusiveness = target_dna.get('elusiveness', 0.0)
            rec_btk = target_dna.get('broken_tackle_rate', 0.10)
            scale = 2.5 + max(0.0, rec_elusiveness) * 1.8 + rec_btk * 6.0
            noise = np.random.exponential(scale=scale) - scale
            yac = max(0, int(round(yac + noise)))
            
            raw_gain = ay + yac
            
            # Matchup passing strength multiplier
            off_t = self.trench_tiers.get(self.possession, {})
            def_t = self.trench_tiers.get(self.defending, {})
            pass_diff = def_t.get('pass_rush_tier', 3.0) - off_t.get('pass_block_tier', 3.0)
            qb_cpoe_z = off_t.get('qb_cpoe_z', 0.0)
            
            pass_mult = 1.0
            gain = max(-10, min(int(round(raw_gain * pass_mult)), self.yardline_100))
            
            qb_stats['pCmp'] += 1
            qb_stats['pYds'] += gain
            target_stats['rec'] += 1
            target_stats['recYds'] += gain
            
        return {
            "type": "PASS",
            "yards": gain,
            "is_complete": is_complete,
            "target": target,
            "air_yards": ay,
            "ttt": sim_ttt
        }

    def _select_target_from_top_5(self):
        """Rolls to select 5 receivers weighted by their target shares, then selects a target from those 5"""
        team_roster = self.rosters[self.possession]
        # Keep all active receivers (no filtering/pruning of low-target-share players)
        receivers = [p for p, t in team_roster.items() if t.get('pos') != 'QB' and t.get('status', 'active') == 'active']
        if not receivers: return "Unknown"
        
        shares = np.array([team_roster[r].get('target_share', 0.0) for r in receivers], dtype=np.float32)
        total_share = shares.sum()
        if total_share <= 0:
            probs = np.ones_like(shares) / len(shares)
        else:
            probs = shares / total_share
            
        # If 5 or fewer receivers, just select directly
        if len(receivers) <= 5:
            return np.random.choice(receivers, p=probs)
            
        # Draw 5 unique receivers without replacement weighted by their target shares
        selected_receivers = np.random.choice(receivers, size=5, replace=False, p=probs)
        
        # Select target receiver from the 5 selected, based on their target shares
        selected_shares = np.array([team_roster[r].get('target_share', 0.0) for r in selected_receivers], dtype=np.float32)
        selected_total = selected_shares.sum()
        if selected_total <= 0:
            selected_probs = np.ones_like(selected_shares) / len(selected_shares)
        else:
            selected_probs = selected_shares / selected_total
        
        return np.random.choice(selected_receivers, p=selected_probs)

    def _execute_run_play(self):
        """Phase 4B: Rushing Resolution (Double Hurdle/Empirical Rushing V.0.1.0)"""
        rusher = self._select_rusher()
        
        # Resolve trench ratings
        off_t = self.trench_tiers.get(self.possession, {})
        run_block_z = 3 - off_t.get('run_block_tier', 3)
        box_density_z = off_t.get('qb_cpoe_z', 0.0) * -0.5
        
        feat_dict = {
            'yardline_100': self.yardline_100,
            'distance': self.distance,
            'ydstogo': self.distance,
            'game_seconds_remaining': self._get_game_sec(),
            'score_differential': self.scores[self.possession] - self.scores[self.defending],
            'rusher': rusher,
            'rusher_name': rusher,
            'active_qb': self._get_starter('QB'),
            'team': self.possession,
            'run_block_z': run_block_z,
            'box_density_z': box_density_z
        }
        
        gain = int(self.registry.predict_rush_yards(feat_dict, self.rosters[self.possession]))
        
        # Matchup rushing strength multiplier
        def_t = self.trench_tiers.get(self.defending, {})
        run_diff = def_t.get('run_stuff_tier', 3.0) - off_t.get('run_block_tier', 3.0)
        run_mult = 1.0
        gain = max(-10, min(int(round(gain * run_mult)), self.yardline_100))
        
        # Record rushing stats
        rusher_stats = self._get_player_stat(self.possession, rusher)
        rusher_stats['rAtt'] += 1
        rusher_stats['rYds'] += gain
        
        return {"type": "RUN", "yards": gain, "rusher": rusher}

    def _subtract_passing_stats(self, result):
        qb_name = self._get_starter('QB')
        qb_stats = self._get_player_stat(self.possession, qb_name)
        qb_stats['pAtt'] = max(0, qb_stats.get('pAtt', 0) - 1)
        if result.get('is_complete'):
            qb_stats['pCmp'] = max(0, qb_stats.get('pCmp', 0) - 1)
            qb_stats['pYds'] = max(0, qb_stats.get('pYds', 0) - result.get('yards', 0))
        if result.get('is_interception'):
            qb_stats['int'] = max(0, qb_stats.get('int', 0) - 1)
            
        target = result.get('target')
        if target:
            target_stats = self._get_player_stat(self.possession, target)
            target_stats['targets'] = max(0, target_stats.get('targets', 0) - 1)
            if result.get('is_complete'):
                target_stats['rec'] = max(0, target_stats.get('rec', 0) - 1)
                target_stats['recYds'] = max(0, target_stats.get('recYds', 0) - result.get('yards', 0))

    def _subtract_rushing_stats(self, result):
        if result.get('is_scramble'):
            qb_name = self._get_starter('QB')
            qb_stats = self._get_player_stat(self.possession, qb_name)
            qb_stats['rAtt'] = max(0, qb_stats.get('rAtt', 0) - 1)
            qb_stats['rYds'] = max(0, qb_stats.get('rYds', 0) - result.get('yards', 0))
        else:
            rusher = result.get('rusher')
            if rusher:
                rusher_stats = self._get_player_stat(self.possession, rusher)
                rusher_stats['rAtt'] = max(0, rusher_stats.get('rAtt', 0) - 1)
                rusher_stats['rYds'] = max(0, rusher_stats.get('rYds', 0) - result.get('yards', 0))

    def _apply_chaos(self, result):
        """Phase 5: Mid-play turnovers/sacks/penalties using Gate 5."""
        if result.get('type') not in ['PASS', 'RUN'] or result.get('type') == 'SACK':
            return result
            
        # Roll mid-play penalties
        is_pass = result.get('type') == 'PASS'
        is_run = result.get('type') == 'RUN'
        
        accepted_penalty = None
        
        if is_run:
            # Roll Offensive Holding
            if np.random.random() < 0.0149:
                accepted_penalty = 'OFFENSIVE_HOLDING'
                self._subtract_rushing_stats(result)
        elif is_pass:
            r_pen = np.random.random()
            if r_pen < 0.0149:
                # Offensive Holding accepted if not an interception
                if not result.get('is_interception'):
                    accepted_penalty = 'OFFENSIVE_HOLDING'
                    if result.get('is_scramble'):
                        self._subtract_rushing_stats(result)
                    else:
                        self._subtract_passing_stats(result)
            elif r_pen < 0.0149 + 0.0120:
                # DPI declined if complete and gain >= air_yards
                if not (result.get('is_complete') and result.get('yards', 0) >= result.get('air_yards', 0)):
                    accepted_penalty = 'DPI'
                    self._subtract_passing_stats(result)
            elif r_pen < 0.0149 + 0.0120 + 0.0090:
                # Defensive Holding declined if complete and gain >= 5
                if not (result.get('is_complete') and result.get('yards', 0) >= 5):
                    accepted_penalty = 'DEFENSIVE_HOLDING'
                    self._subtract_passing_stats(result)
                    
        if accepted_penalty:
            result['penalty'] = accepted_penalty
            # Wiping out play results
            result['is_complete'] = False
            result['is_interception'] = False
            result['is_fumble'] = False
            result['is_fumble_lost'] = False
            return result

        if result.get('type') == 'PASS' and not result.get('is_complete'):
            return result
            
        # Determine player who has the ball
        carrier = result.get('target') or result.get('rusher')
        if not carrier:
            return result
            
        # Get carrier traits
        carrier_dna = self.dna['skill'].get(carrier, {})
        fumble_factor = carrier_dna.get('fumble_rate', 0.015) / 0.015
        
        # Empirical prior rates (reduced by 20% for elite matchups)
        if result['type'] == 'RUN':
            base_prob = 0.006520 * 0.80
        else:
            base_prob = 0.005246 * 0.80
            
        fumble_prob = base_prob * fumble_factor
        
        if np.random.random() < fumble_prob:
            is_fumble = True
            is_fumble_lost = np.random.random() < 0.50
            
            result['is_fumble'] = True
            result['is_fumble_lost'] = is_fumble_lost
            
            carrier_stats = self._get_player_stat(self.possession, carrier)
            carrier_stats['fumbles'] = carrier_stats.get('fumbles', 0) + 1
            if is_fumble_lost:
                carrier_stats['fumbles_lost'] = carrier_stats.get('fumbles_lost', 0) + 1
        return result

    def _finalize_play(self, result):
        """Phase 6: Update state and generate narrative"""
        # Handle accepted mid-play penalties first!
        if result.get('penalty') == 'OFFENSIVE_HOLDING':
            self.yardline_100 = min(99, self.yardline_100 + 10)
            self.distance += 10
            return f"PENALTY: Offensive Holding on {self.possession}, 10 yards, replay down."
        elif result.get('penalty') == 'DPI':
            ay = result.get('air_yards', 0)
            self.yardline_100 = max(1, min(99, self.yardline_100 - ay))
            self.down = 1
            self.distance = min(10, self.yardline_100)
            return f"PENALTY: Defensive Pass Interference on {self.defending}, spot foul ({ay} yards), automatic first down."
        elif result.get('penalty') == 'DEFENSIVE_HOLDING':
            self.yardline_100 = max(1, self.yardline_100 - 5)
            self.down = 1
            self.distance = min(10, self.yardline_100)
            return f"PENALTY: Defensive Holding on {self.defending}, 5 yards, automatic first down."

        # Handle Interceptions first!
        if result.get('is_interception'):
            # Record passing stats
            qb_name = self._get_starter('QB')
            qb_stats = self._get_player_stat(self.possession, qb_name)
            qb_stats['int'] += 1 # interceptive count
            
            # Record defense stats
            self.player_stats[self.defending]['Defense']['def_int'] += 1
            
            # Roll for defensive TD (2.5% chance)
            if np.random.random() < 0.025:
                self.player_stats[self.defending]['Defense']['def_td'] += 1
                self.scores[self.defending] += 7
                self.needs_kickoff = True
                self.yardline_100 = 70
                self.down = 1
                self.distance = 10
                msg = f"PASS: Passer {qb_name} targeted {result['target']}, but it was INTERCEPTED by {self.defending} and returned for a TOUCHDOWN!"
                return msg
                
            # The interception occurs at the target spot (yardline_100 - air_yards)
            ay = result.get('air_yards', 0)
            self.yardline_100 = max(1, min(99, self.yardline_100 - ay))
            
            # Switch possession
            self._switch_possession(scored=False)
            
            # Return yards = 10 yards closer to opponent endzone (yardline_100 decreases by 10)
            self.yardline_100 = max(1, min(99, self.yardline_100 - 10))
            
            msg = f"PASS: Passer {qb_name} targeted {result['target']}, but it was INTERCEPTED by {self.possession} (former defending) and returned 10 yards!"
            return msg
            
        # Handle Lost Fumbles!
        if result.get('is_fumble_lost'):
            gain = result['yards']
            self.yardline_100 -= gain
            
            # Resolve player details before switching possession!
            fumbling_qb = self._get_starter('QB')
            play_actor = result.get('target') or result.get('rusher') or fumbling_qb
            
            # Record defense stats
            self.player_stats[self.defending]['Defense']['def_fumble_rec'] += 1
            
            # Roll for defensive TD (1.8% chance)
            if np.random.random() < 0.018:
                self.player_stats[self.defending]['Defense']['def_td'] += 1
                self.scores[self.defending] += 7
                self.needs_kickoff = True
                self.yardline_100 = 70
                self.down = 1
                self.distance = 10
                msg = f"{result['type']} to {play_actor} for {gain} yards, but {play_actor} FUMBLED, recovered by {self.defending}, and returned for a TOUCHDOWN!"
                return msg
                
            # Switch possession
            self._switch_possession(scored=False)
            
            # Return yards = 10 yards closer to opponent endzone (yardline_100 decreases by 10)
            self.yardline_100 = max(1, min(99, self.yardline_100 - 10))
            
            msg = f"{result['type']} to {play_actor} for {gain} yards, but {play_actor} FUMBLED, recovered by {self.possession} (former defending), and returned 10 yards!"
            return msg
            
        # Handle Sacks (which are not lost fumbles)
        if result.get('type') == 'SACK':
            gain = result['yards']
            self.yardline_100 -= gain
            
            # Sacks are a loss of yards, update down and distance
            msg = f"SACK: {self._get_starter('QB')} sacked for a loss of {-gain} yards."
            
            if result.get('is_fumble'):
                msg += " (Fumble, but recovered by offense)"
                
            self.down += 1
            self.distance -= gain
            
            if self.down > 4:
                self._switch_possession(scored=False)
                return msg + " (Turnover on Downs)"
            return msg
            
        # Handle regular Scrambles / Passes / Runs
        gain = result['yards']
        self.yardline_100 -= gain
        
        # Scramble vs normal run/pass
        if result.get('is_throwaway'):
            play_actor = self._get_starter('QB')
            msg = f"PASS: QB {play_actor} under pressure, thrown away."
        elif result.get('is_scramble'):
            play_actor = self._get_starter('QB')
            msg = f"SCRAMBLE: QB {play_actor} ran for {gain} yards."
        elif result.get('type') == 'PASS' and not result.get('is_complete'):
            play_actor = result.get('target')
            msg = f"PASS to {play_actor} INCOMPLETE."
        else:
            play_actor = result.get('target') or result.get('rusher')
            msg = f"{result['type']} to {play_actor} for {gain} yards."
            if result.get('is_fumble'):
                msg += " (Fumble, but recovered by offense)"
            
        if self.yardline_100 <= 0:
            self.scores[self.possession] += 7
            self.needs_kickoff = True
            
            # Record touchdown details for EDA
            play_type = 'PASS' if result.get('type') == 'PASS' else 'RUN'
            self.td_details.append({
                "play_type": play_type,
                "length": int(gain)
            })
            
            # Record Touchdown stats
            if result['type'] == 'PASS':
                qb_name = self._get_starter('QB')
                self._get_player_stat(self.possession, qb_name)['pTD'] += 1
                self._get_player_stat(self.possession, play_actor)['recTD'] += 1
            elif result['type'] == 'RUN' or result.get('is_scramble'):
                qb_name = self._get_starter('QB')
                actor = qb_name if result.get('is_scramble') else play_actor
                self._get_player_stat(self.possession, actor)['rTD'] += 1
                
            self._switch_possession(scored=True)
            return msg + " TOUCHDOWN!"
            
        if gain >= self.distance:
            self.down = 1; self.distance = 10
            msg += " (1st Down)"
        else:
            self.down += 1; self.distance -= gain
            
        if self.down > 4:
            self._switch_possession(scored=False)
            return msg + " (Turnover on Downs)"
            
        return msg

    def _manage_clock(self, result):
        """Phase 7: Clock runoff and stoppages"""
        if result.get('penalty'):
            self._run_clock(10)
            self.clock_stopped = True
        elif result.get('is_interception') or result.get('is_fumble_lost'):
            self._run_clock(10)
            self.clock_stopped = True
        elif result.get('type') == "PASS" and not result.get('is_complete'):
            ay = result.get('air_yards', 0)
            ttt = result.get('ttt', 2.7)
            # Runoff is time to throw + 1 second for every 10 yards traveled
            runoff = int(round(ttt + max(0, ay) / 10.0))
            self._run_clock(runoff)
            self.clock_stopped = True
        else:
            # Normal run / completed pass / sack clock runoff
            is_hurry = (self.quarter == 4 and self.time_remaining < 120 and self.scores[self.possession] < self.scores[self.defending])
            runoff = 10 if is_hurry else np.random.randint(18, 30)
            self._run_clock(runoff)
            self.clock_stopped = False


    # =========================================================================
    # HELPERS
    # =========================================================================

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

    def _get_player_stat(self, team, player):
        if player not in self.player_stats[team]:
            # Lazily initialize if they don't exist
            traits = self.rosters[team].get(player, {})
            self.player_stats[team][player] = {
                'Pos': traits.get('pos', 'WR/TE'),
                'rAtt': 0, 'rYds': 0, 'rTD': 0,
                'pAtt': 0, 'pCmp': 0, 'pYds': 0, 'pTD': 0, 'int': 0,
                'rec': 0, 'recYds': 0, 'recTD': 0, 'targets': 0,
                'fumbles': 0, 'sacks_taken': 0
            }
        return self.player_stats[team][player]

    def _get_starter(self, pos):
        team_roster = self.rosters[self.possession]
        players = [p for p, t in team_roster.items() if t.get('pos') == pos and t.get('status', 'active') == 'active']
        if not players: return "Unknown"
        dna_key = 'qb' if pos == 'QB' else 'skill'
        return max(players, key=lambda p: self.dna[dna_key].get(p, {}).get('total_attempts' if pos == 'QB' else 'total_targets', 0))

    def _select_rusher(self):
        team_roster = self.rosters[self.possession]
        eligible = [p for p, t in team_roster.items() if t.get('carry_share', 0) > 0 and t.get('status', 'active') == 'active']
        if not eligible: return self._get_starter('RB')
        shares = [team_roster[p]['carry_share'] for p in eligible]
        return np.random.choice(eligible, p=[s/sum(shares) for s in shares])

    def _get_game_sec(self):
        return (4 - self.quarter) * 900 + self.time_remaining

    def _run_clock(self, seconds):
        self.time_remaining -= seconds
        if self.time_remaining <= 0:
            if self.quarter < 4:
                self.quarter += 1
                self.time_remaining = 900
                if self.quarter == 3:
                    self.possession = self.home_team
                    self.defending = self.away_team
                    self.needs_kickoff = True
                    self.clock_stopped = True
            else:
                self.game_over = True


    def _handle_kickoff(self):
        self.needs_kickoff = False
        self.yardline_100 = 70 # returned to the 30 yard line (100 - 30 = 70)
        self.down = 1; self.distance = 10
        self.clock_stopped = True
        self._run_clock(8)
        return "KICKOFF: Return to the 30 yard line."

    def _switch_possession(self, scored=False):
        self.possession, self.defending = self.defending, self.possession
        self.yardline_100 = 70 if scored else 100 - self.yardline_100
        self.down = 1; self.distance = 10

    def _execute_field_goal(self):
        success_prob = self.registry.predict_fg_success([self.yardline_100])
        is_good = np.random.random() < success_prob
        
        # Record field goal attempt details
        self.fg_attempts_details.append({
            "yardline_100": int(self.yardline_100),
            "length": int(self.yardline_100 + 17),
            "is_good": bool(is_good)
        })
        
        # Run clock runoff for FG attempt (approx 5-8 seconds)
        self._run_clock(np.random.randint(5, 9))
        
        self.fg_attempts[self.possession] += 1
        if is_good:
            self.fg_made[self.possession] += 1
            self.scores[self.possession] += 3
            self.needs_kickoff = True
        self._switch_possession(scored=is_good)
        return f"FIELD GOAL: {'GOOD' if is_good else 'MISSED'}"

    def _execute_punt(self):
        self.punts_run += 1
        dist = np.random.randint(35, 50)
        self.yardline_100 -= dist
        
        # Run clock runoff for punt play (approx 10-14 seconds)
        self._run_clock(np.random.randint(10, 15))
        
        # Switch possession first
        self._switch_possession(scored=False)
        
        # Apply the 7 yard return towards the new defending team's endzone
        # i.e., new offense returns the ball 7 yards closer to the opponent endzone
        self.yardline_100 = max(1, min(99, self.yardline_100 - 7))
        
        return f"PUNT: {dist} yards. Return: 7 yards."

    def _build_air_yards_features(self, qb_name, target, coach_name):
        q_dna = self.dna['qb'].get(qb_name, {})
        r_dna = self.dna['skill'].get(target, {})
        c_dna = self.dna['coach'].get(coach_name, {})
        t_dna = self.dna['trench'].get(str(self.year), {}).get(self.possession, {})
        d_dna = self.dna['trench'].get(str(self.year), {}).get(self.defending, {})
        
        def v(d, k, def_val=0.0):
            val = d.get(k, def_val)
            return val if isinstance(val, (int, float)) else def_val

        return {
            "down": self.down, "ydstogo": self.distance, "yardline_100": self.yardline_100,
            "qtr": self.quarter, "score_differential": self.scores[self.possession] - self.scores[self.defending],
            "half_seconds_remaining": self.time_remaining, "game_seconds_remaining": self._get_game_sec(),
            "cpoe_qb": v(q_dna, 'cpoe'), "avg_air_yards_per_att_qb": v(q_dna, 'avg_air_yards_per_att', 8.0),
            "deep_ball_rate_qb": v(q_dna, 'deep_ball_rate', 0.12), "scramble_rate_qb": v(q_dna, 'scramble_rate'),
            "sack_rate_qb": v(q_dna, 'sack_rate', 0.06), "play_action_rate_qb": v(q_dna, 'play_action_rate'),
            "under_pressure_cpoe_qb": v(q_dna, 'under_pressure_cpoe'), "ngs_aggressiveness_index_qb": v(q_dna, 'ngs_aggressiveness_index', 15.0),
            "avg_time_to_throw_sec_qb": v(q_dna, 'avg_time_to_throw_sec', 2.7), "target_share_recv": v(r_dna, 'target_share', 0.1),
            "catch_rate_recv": v(r_dna, 'catch_rate', 0.65), "avg_target_depth_yds_recv": v(r_dna, 'avg_target_depth_yds', 8.0),
            "deep_target_rate_recv": v(r_dna, 'deep_target_rate', 0.12), "air_yards_share_recv": v(r_dna, 'air_yards_share', 0.1),
            "yac_per_reception_recv": v(r_dna, 'yac_per_reception', 4.0), "avg_separation_yds_recv": v(r_dna, 'avg_separation_yds', 2.9),
            "air_yards_tendency_coach": v(c_dna, 'air_yards_tendency'), "deep_shot_rate_coach": v(c_dna, 'deep_shot_rate', 0.12),
            "screen_rate_coach": v(c_dna, 'screen_rate', 0.2), "play_action_rate_coach": v(c_dna, 'play_action_rate'),
            "no_huddle_rate_coach": v(c_dna, 'no_huddle_rate'), "rpo_rate_coach": v(c_dna, 'rpo_rate'),
            "conservative_score_bias_coach": v(c_dna, 'conservative_score_bias'), "def_pressure_rate_def_trench": v(d_dna, 'def_pressure_rate', 0.15),
            "def_sack_rate_def_trench": v(d_dna, 'def_sack_rate', 0.06)
        }

    def _format_time(self):
        minutes = self.time_remaining // 60
        seconds = self.time_remaining % 60
        return f"Q{self.quarter} {minutes:02d}:{seconds:02d}"

    def _format_yardline(self):
        if self.yardline_100 == 50:
            return "50"
        elif self.yardline_100 > 50:
            own_side = 100 - self.yardline_100
            return f"{self.possession} {own_side}"
        else:
            opp_side = self.yardline_100
            return f"{self.defending} {opp_side}"

    def _format_situation(self):
        score_str = f"{self.away_team} {self.scores[self.away_team]} - {self.home_team} {self.scores[self.home_team]}"
        if self.needs_kickoff:
            return f"[{self._format_time()}] Score: {score_str} | Kickoff |"
        
        yard_str = self._format_yardline()
        down_str = {1: "1st", 2: "2nd", 3: "3rd", 4: "4th"}.get(self.down, f"{self.down}th")
        return f"[{self._format_time()}] ({down_str} & {self.distance} at {yard_str}) Score: {score_str} |"
