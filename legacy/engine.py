import pandas as pd
import numpy as np
import os
import json
from .utils import NameCentralizer
from .model_registry import ModelRegistry

class NFLGameSimulator:
    def __init__(self, away_team, home_team, year=2024, week=1):
        self.away_team = away_team
        self.home_team = home_team
        self.year = year
        self.week = week
        self.registry = ModelRegistry()
        
        # Load DNA Registry (V.0.1.0 Standard)
        self.qb_dna = self._load_json('data/dna/qb_dna.json')
        self.skill_dna = self._load_json('data/dna/skill_dna.json')
        self.coach_dna = self._load_json('data/dna/coach_dna.json')
        self.trench_dna = self._load_json('data/dna/trench_dna.json')
        
        # Legacy mappings for backward compatibility
        self.team_to_off_coach = self._load_json('data/team_to_coach_2025.json')
        self.team_to_def_coach = self._load_json('data/team_to_dc_2025.json')
        self.traits_data = {
            away_team: self.load_traits(away_team),
            home_team: self.load_traits(home_team)
        }
        
        self.starters = {
            away_team: self.identify_starters(away_team),
            home_team: self.identify_starters(home_team)
        }
        
        # Game State
        self.possession = away_team
        self.defending = home_team
        self.yardline_100 = 75
        self.down = 1
        self.distance = 10
        self.scores = {away_team: 0, home_team: 0}
        self.quarter = 1
        self.time_remaining = 900
        self.game_over = False
        self.needs_kickoff = True
        self.timeouts = {away_team: 3, home_team: 3}
        self.clock_stopped = True
        self.prev_play_pass = 1

        # Stats tracking
        self.player_stats = {away_team: {}, home_team: {}}

    def _load_json(self, path):
        if os.path.exists(path):
            with open(path, 'r') as f: return json.load(f)
        return {}

    def load_traits(self, team):
        path = f"data/rosters/{team}_traits.json"
        if os.path.exists(path):
            with open(path, 'r') as f: return json.load(f).get('traits', {})
        return {}

    def identify_starters(self, team):
        traits = self.traits_data[team]
        qbs = [p for p, t in traits.items() if t.get('pos') == 'QB']
        if not qbs: return {'QB': "Unknown"}
        # Choose starter based on total attempts or a provided depth chart
        return {'QB': max(qbs, key=lambda p: self.qb_dna.get(p, {}).get('total_attempts', 0))}

    def simulate_play(self):
        """Phase-based Play Orchestration (Engine V2)"""
        if self.game_over: return "Game Over"
        
        if self.needs_kickoff:
            return self._handle_kickoff()

        # 1. Pre-Snap Penalties
        penalty_msg = self._handle_pre_snap_penalties()
        if penalty_msg: return penalty_msg

        # 2. 4th Down Evaluation
        if self.down == 4:
            decision = self._evaluate_fourth_down()
            if decision == "PUNT": return self._execute_punt()
            if decision == "FIELD_GOAL": return self._execute_field_goal()

        # 3. Play Selection (Run/Pass)
        is_pass = self._select_play_type()

        # 4. Execute Play
        if is_pass:
            play_result = self._execute_pass_play()
        else:
            play_result = self._execute_run_play()

        # 5. Mid-Play Chaos (TODO: Integrate dedicated chaos model)
        # play_result = self._apply_chaos(play_result)

        # 6. Clock & State Update
        narrative = self._finalize_play(play_result)
        self._manage_clock(play_result)

        return narrative

    # --- PHASE 1: PRE-SNAP ---
    def _handle_pre_snap_penalties(self):
        # Placeholder for pre-snap penalty logic (False start, Neutral zone)
        # 1% chance for now
        if np.random.random() < 0.01:
            penalty_yards = 5
            self.yardline_100 += penalty_yards # Assuming offensive penalty
            return f"PENALTY: False Start, 5 yards."
        return None

    # --- PHASE 2: 4TH DOWN ---
    def _evaluate_fourth_down(self):
        game_sec = self.time_remaining + ((4 - self.quarter) * 900)
        score_diff = self.scores[self.possession] - self.scores[self.defending]
        features = [self.yardline_100, self.distance, game_sec, score_diff, self.timeouts[self.possession]]
        
        probas = self.registry.predict_4th_down_probas(features)
        
        # Apply Coach Aggression (PROE-style bias for 'Go')
        hc_name = self.team_to_off_coach.get(self.possession, "Unknown")
        dna = self.coach_dna.get(hc_name, {})
        # Use deep_shot_rate as a proxy for aggression until 'aggression' field is formalized
        aggression_bias = (dna.get('deep_shot_rate', 0.12) - 0.12) * 2.0
        probas[2] += aggression_bias
        
        choice = np.argmax(probas)
        return ["PUNT", "FIELD_GOAL", "GO"][choice]

    def _execute_field_goal(self):
        success_prob = self.registry.predict_fg_success([self.yardline_100])
        is_good = np.random.random() < success_prob
        if is_good:
            self.scores[self.possession] += 3
            self.needs_kickoff = True
        self._switch_possession(scored=is_good)
        return f"FIELD GOAL: {'GOOD' if is_good else 'MISSED'}"

    def _execute_punt(self):
        dist = np.random.randint(35, 50)
        self.yardline_100 -= dist
        self._switch_possession(scored=False)
        return f"PUNT: {dist} yards."

    # --- PHASE 3: PLAY SELECTION ---
    def _select_play_type(self):
        game_sec = self.time_remaining + ((4 - self.quarter) * 900)
        score_diff = self.scores[self.possession] - self.scores[self.defending]
        
        # Construct V2.8 Feature Vector
        # Features: [yardline_100, game_sec, timeouts, score_diff, leverage, redzone, cpoe_blend]
        qb_name = self.starters[self.possession]['QB']
        qb_dna = self.qb_dna.get(qb_name, {})
        cpoe = qb_dna.get('cpoe', 0.0)
        
        lev = score_diff * game_sec
        rz = self.distance * self.yardline_100
        
        features = [self.yardline_100, game_sec, self.timeouts[self.possession], score_diff, lev, rz, cpoe]
        state = {'down': self.down, 'distance': self.distance}
        
        base_prob = self.registry.predict_play_selection_proba(state, features)
        
        # Add Coach PROE bias
        hc_name = self.team_to_off_coach.get(self.possession, "Unknown")
        # Logic for PROE overlay (using atlas for now until dna has off_proe)
        off_proe = 0.0 # self.get_strategic_metric(hc_name, 'off_proe')
        
        final_prob = np.clip(base_prob + (off_proe / 100.0), 0.01, 0.99)
        return np.random.random() < final_prob

    # --- PHASE 4: EXECUTION ---
    def _execute_pass_play(self):
        # 1. Target Selection
        off_traits = self.traits_data[self.possession]
        receivers = [p for p, t in off_traits.items() if t.get('pos') != 'QB' and t.get('target_share', 0) > 0]
        if not receivers: receivers = ["Unknown Receiver"]
        
        # Roll for 5 candidates
        shares = [off_traits.get(r, {}).get('target_share', 0.05) for r in receivers]
        norm_shares = [s/sum(shares) for s in shares]
        candidates = np.random.choice(receivers, size=min(5, len(receivers)), p=norm_shares, replace=False)
        
        # Final target roll among candidates
        c_shares = [off_traits.get(r, {}).get('target_share', 0.05) for r in candidates]
        c_norm = [s/sum(c_shares) for s in c_shares]
        target = np.random.choice(candidates, p=c_norm)
        
        # 2. Construct V.0.1.1 Feature DataFrame for Air Yards
        qb_name = self.starters[self.possession]['QB']
        q_dna = self.qb_dna.get(qb_name, {})
        r_dna = self.skill_dna.get(target, {})
        c_dna = self.coach_dna.get(self.team_to_off_coach.get(self.possession, "Unknown"), {})
        t_dna = self.trench_dna.get(str(self.year), {}).get(self.possession, {})
        d_dna = self.trench_dna.get(str(self.year), {}).get(self.defending, {})
        
        score_diff = self.scores[self.possession] - self.scores[self.defending]
        half_sec = self.time_remaining # Simplified
        
        # Fill missing values with defaults
        def val(d, k, default=0.0):
            v = d.get(k, default)
            return v if isinstance(v, (int, float)) else default

        feat_dict = {
            "down": self.down, "ydstogo": self.distance, "yardline_100": self.yardline_100,
            "qtr": self.quarter, "score_differential": score_diff,
            "half_seconds_remaining": half_sec, "game_seconds_remaining": game_sec_calc(self.quarter, self.time_remaining),
            "cpoe_qb": val(q_dna, 'cpoe'),
            "avg_air_yards_per_att_qb": val(q_dna, 'avg_air_yards_per_att', 8.0),
            "deep_ball_rate_qb": val(q_dna, 'deep_ball_rate', 0.12),
            "scramble_rate_qb": val(q_dna, 'scramble_rate'),
            "sack_rate_qb": val(q_dna, 'sack_rate', 0.06),
            "play_action_rate_qb": val(q_dna, 'play_action_rate'),
            "under_pressure_cpoe_qb": val(q_dna, 'under_pressure_cpoe'),
            "ngs_aggressiveness_index_qb": val(q_dna, 'ngs_aggressiveness_index', 15.0),
            "avg_time_to_throw_sec_qb": val(q_dna, 'avg_time_to_throw_sec', 2.7),
            "target_share_recv": val(r_dna, 'target_share', 0.1),
            "catch_rate_recv": val(r_dna, 'catch_rate', 0.65),
            "avg_target_depth_yds_recv": val(r_dna, 'avg_target_depth_yds', 8.0),
            "deep_target_rate_recv": val(r_dna, 'deep_target_rate', 0.12),
            "air_yards_share_recv": val(r_dna, 'air_yards_share', 0.1),
            "yac_per_reception_recv": val(r_dna, 'yac_per_reception', 4.0),
            "avg_separation_yds_recv": val(r_dna, 'avg_separation_yds', 2.9),
            "air_yards_tendency_coach": val(c_dna, 'air_yards_tendency'),
            "deep_shot_rate_coach": val(c_dna, 'deep_shot_rate', 0.12),
            "screen_rate_coach": val(c_dna, 'screen_rate', 0.2),
            "play_action_rate_coach": val(c_dna, 'play_action_rate'),
            "no_huddle_rate_coach": val(c_dna, 'no_huddle_rate'),
            "rpo_rate_coach": val(c_dna, 'rpo_rate'),
            "conservative_score_bias_coach": val(c_dna, 'conservative_score_bias'),
            "def_pressure_rate_def_trench": val(d_dna, 'def_pressure_rate', 0.15),
            "def_sack_rate_def_trench": val(d_dna, 'def_sack_rate', 0.06)
        }
        
        ay = self.registry.predict_air_yards(pd.DataFrame([feat_dict]))
        ay = max(-5, min(int(ay), self.yardline_100))
        
        # 3. Completion Roll
        # Simple completion model for now: base catch rate + complexity penalty
        catch_prob = val(r_dna, 'catch_rate', 0.65) - (abs(ay - 8) * 0.01)
        is_complete = np.random.random() < catch_prob
        
        if is_complete:
            # 4. YAC Roll
            feat_dict['qb_name'] = qb_name
            feat_dict['receiver_name'] = target
            feat_dict['air_yards'] = ay
            yac = max(0, int(self.registry.predict_yac(feat_dict)))
            gain = ay + yac
        else:
            gain = 0
            
        return {"type": "PASS", "yards": gain, "is_complete": is_complete, "target": target, "air_yards": ay}

    def _execute_run_play(self):
        # Placeholder for rushing model
        off_traits = self.traits_data[self.possession]
        rushers = [p for p, t in off_traits.items() if t.get('pos') in ['RB', 'WR/TE'] and t.get('carry_share', 0) > 0]
        rusher = np.random.choice(rushers) if rushers else "Unknown Rusher"
        
        # Rushing features V3.0
        rush_features = [self.yardline_100, self.distance, self.time_remaining, 0, 0, 0, 0, 0, 0]
        gain = int(self.registry.predict_rush_yards(rush_features))
        return {"type": "RUN", "yards": gain, "rusher": rusher}

    def _finalize_play(self, result):
        gain = result['yards']
        self.yardline_100 -= gain
        
        msg = f"{result['type']} by {result.get('target') or result.get('rusher')} for {gain} yards."
        
        if self.yardline_100 <= 0:
            self.scores[self.possession] += 7
            self.needs_kickoff = True
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
        # Simple runoff for now
        # Pass incomplete = clock stops
        if result.get('type') == "PASS" and not result.get('is_complete'):
            self.run_clock(6)
            self.clock_stopped = True
        else:
            self.run_clock(np.random.randint(25, 40))
            self.clock_stopped = False

    def _handle_kickoff(self):
        self.needs_kickoff = False
        self.yardline_100 = 75
        self.down = 1; self.distance = 10
        self.clock_stopped = True
        self.run_clock(8)
        return "KICKOFF: Touchback."

    def _switch_possession(self, scored=False):
        self.possession, self.defending = self.defending, self.possession
        self.yardline_100 = 75 if scored else 100 - self.yardline_100
        self.down = 1; self.distance = 10

    def run_clock(self, seconds):
        self.time_remaining -= seconds
        if self.time_remaining <= 0:
            if self.quarter < 4:
                self.quarter += 1
                self.time_remaining = 900
            else:
                self.game_over = True

def game_sec_calc(qtr, time_rem):
    return (4 - qtr) * 900 + time_rem
