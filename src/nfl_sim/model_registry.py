"""Central registry that loads and serves all trained ML submodels.

Loads and caches every simulation submodel once at startup (chaos, air_yards,
yac, rush_yards, field_goal, win_probability, fourth_down_conversion, and the
zone-split play-selection classifiers) so the game engine can fetch predictions
without repeated file reads. Also holds player-DNA lookups (qb/skill) and
computes 4th-down win-probability recommendations (Go/Punt/FG), including
desperation-time overrides when trailing late.

Singleton: ModelRegistry() caches a single instance in memory.
Full design rationale: see model_registry.md.
"""

import xgboost as xgb
import numpy as np
import os
import json
import pandas as pd
from .models.air_yards_v_0_1_1.inference import AirYardsDoubleHurdleSampler
from .models.yac_model_v_0_1_1.inference import YACModelV011
from .models.rush_yards_v_0_1_0.inference import RushYardsModelV010

class ModelRegistry:
    _instance_cache = None

    def __new__(cls, *args, **kwargs):
        if cls._instance_cache is None:
            instance = super(ModelRegistry, cls).__new__(cls)
            instance.model_dir = kwargs.get('model_dir', 'src/nfl_sim/models')
            instance.models = {}
            instance.play_selection_buckets = {}
            instance.air_yards_sampler = None
            instance.yac_model = None
            instance.rush_model = None
            instance.chaos_model = None
            instance.fg_model = None
            instance.wp_model = None
            instance.fd_conversion_model = None
            instance.clock_pace_model = None
            instance.coach_proe_splits = {}
            instance.load_all()
            cls._instance_cache = instance
        return cls._instance_cache

    def __init__(self, model_dir='src/nfl_sim/models'):
        pass

    def load_all(self):
        # V0.1.0 Chaos Model
        chaos_v10_dir = os.path.join(self.model_dir, 'chaos_v_0_1_0')
        if os.path.exists(chaos_v10_dir):
            from .models.chaos_v_0_1_0.inference import ChaosModelV010
            self.chaos_model = ChaosModelV010(chaos_v10_dir)
            
        # V0.1.1 Air Yards (Double Hurdle)
        ay_v11_dir = os.path.join(self.model_dir, 'air_yards_v_0_1_1')
        if os.path.exists(ay_v11_dir):
            self.air_yards_sampler = AirYardsDoubleHurdleSampler(ay_v11_dir)
        
        # V0.1.1 YAC (XGBoost Regressor)
        yac_v11_dir = os.path.join(self.model_dir, 'yac_model_v_0_1_1')
        if os.path.exists(yac_v11_dir):
            self.yac_model = YACModelV011(yac_v11_dir)
        
        # V0.1.0 Rushing Yards
        rush_v10_dir = os.path.join(self.model_dir, 'rush_yards_v_0_1_0')
        if os.path.exists(rush_v10_dir):
            self.rush_model = RushYardsModelV010(rush_v10_dir)
        
        # V0.1.0 Field Goal Model
        fg_v10_dir = os.path.join(self.model_dir, 'fg_v_0_1_0')
        if os.path.exists(fg_v10_dir):
            from .models.fg_v_0_1_0.inference import FieldGoalModelV010
            self.fg_model = FieldGoalModelV010(fg_v10_dir)
            
        # V0.1.0 Win Probability Model
        wp_v10_dir = os.path.join(self.model_dir, 'win_probability_v_0_1_0')
        if os.path.exists(wp_v10_dir):
            from .models.win_probability_v_0_1_0.inference import WinProbabilityModelV010
            self.wp_model = WinProbabilityModelV010(wp_v10_dir)
            
        # V0.1.0 Fourth Down Conversion Model
        fd_conv_dir = os.path.join(self.model_dir, 'fourth_down_conversion_v_0_1_0')
        if os.path.exists(fd_conv_dir):
            from .models.fourth_down_conversion_v_0_1_0.inference import FourthDownConversionModelV010
            self.fd_conversion_model = FourthDownConversionModelV010(fd_conv_dir)

        # V0.1.0 Clock Pace Model (empirical bootstrap pools, clock_physics_v020)
        clock_pace_dir = os.path.join(self.model_dir, 'clock_pace_v_0_1_0')
        if os.path.exists(clock_pace_dir):
            from .models.clock_pace_v_0_1_0.inference import ClockPaceModelV010
            self.clock_pace_model = ClockPaceModelV010(clock_pace_dir)
        
        # Play Selection V.0.1.0 Buckets (three zones loaded dynamically)
        ps_dir = os.path.join(self.model_dir, 'play_selection_v_0_1_0')
        if os.path.exists(ps_dir):
            for f in os.listdir(ps_dir):
                if f.endswith('.json') and f != 'metadata.json':
                    b_name = f.replace('.json', '')
                    m = xgb.XGBClassifier()
                    m.load_model(os.path.join(ps_dir, f))
                    self.play_selection_buckets[b_name] = m.get_booster()

        # Find project root dynamically
        curr = os.path.dirname(os.path.abspath(__file__))
        project_root = curr
        while project_root and project_root != os.path.dirname(project_root):
            if os.path.exists(os.path.join(project_root, 'data')):
                break
            project_root = os.path.dirname(project_root)
            
        qb_path = os.path.join(project_root, "data", "dna", "qb_dna.json")
        skill_path = os.path.join(project_root, "data", "dna", "skill_dna.json")
        
        self.qb_dna = {}
        self.skill_dna = {}
        if os.path.exists(qb_path):
            with open(qb_path, 'r') as f:
                self.qb_dna = json.load(f)
        if os.path.exists(skill_path):
            with open(skill_path, 'r') as f:
                self.skill_dna = json.load(f)

    def get_bucket_name(self, d, dist):
        if d == 1: return '1_10' if dist == 10 else '1_long' if dist > 10 else '1_short'
        suffix = 'short' if dist <= 3 else 'med' if dist <= 7 else 'long'
        if d == 4: suffix = 'short' if dist <= 2 else 'med_long'
        return f'{d}_{suffix}'

    def get_zone(self, yd_100):
        if yd_100 <= 5: return 'goalline'
        elif yd_100 <= 20: return 'redzone'
        return 'primary'

    def predict_play_selection_proba(self, state, features):
        yd_100 = state.get('yardline_100', 75)
        zone = self.get_zone(yd_100)
        bucket = self.get_bucket_name(state['down'], state['distance'])
        
        # Select correct submodel based on zone
        booster = self.play_selection_buckets.get(f"{zone}_{bucket}")
        if not booster:
            booster = self.play_selection_buckets.get(f"primary_{bucket}")
        if not booster: return 0.58  # Baseline
        
        # Build features mapping
        qb_name = state.get('active_qb', 'Unknown')
        rb_name = state.get('active_rb', 'Unknown')
        coach_name = state.get('posteam_coach', 'Unknown')
        
        # Map dynamic splits
        def get_qb_val(qb, key):
            if qb not in self.qb_dna: return 0.0
            splits = self.qb_dna[qb].get('splits', {})
            if zone in splits and key in splits[zone]:
                return splits[zone][key]
            return self.qb_dna[qb].get(key, 0.0)
            
        def get_skill_val(player, key):
            if player not in self.skill_dna: return 0.0
            splits = self.skill_dna[player].get('splits', {})
            if zone in splits and key in splits[zone]:
                return splits[zone][key]
            return self.skill_dna[player].get(key, 0.0)
            
        cpoe_val = get_qb_val(qb_name, 'cpoe')
        tgt_share = get_skill_val(rb_name, 'target_share')
        cry_share = get_skill_val(rb_name, 'carry_share')
        
        proe_val = 0.0
        if coach_name in self.coach_proe_splits:
            proe_val = self.coach_proe_splits[coach_name].get(zone, 0.0)
            
        # Reconstruct features list matching features_sel EXACTLY:
        # [yardline_100, game_seconds_remaining, score_differential, timeouts_off, timeouts_def, leverage,
        #  cpoe_by_filter, target_share_by_filter, carry_share_by_filter, proe_by_filter]
        full_feat = [
            float(features[0]), # yardline_100
            float(features[1]), # game_seconds_remaining
            float(features[2]), # score_differential
            float(features[3]), # timeouts_off
            float(features[4]), # timeouts_def
            float(features[5]), # leverage
            float(cpoe_val),
            float(tgt_share),
            float(cry_share),
            float(proe_val)
        ]
        
        X = np.array([full_feat], dtype=np.float32)
        return float(booster.inplace_predict(X)[0])

    def predict_4th_down_probas(self, features):
        yardline_100 = features[0]
        distance = features[1]
        game_sec = features[2]
        score_diff = features[3]
        
        # Desperation Time: Trailing by 3+ scores late in the game (surrender check)
        if game_sec <= 420: # Last 7 minutes
            if score_diff <= -17:
                return [0.0, 0.0, 1.0] # Must Go!
            elif score_diff <= -9 and game_sec <= 300:
                return [0.0, 0.0, 1.0] # Must Go!
                
        if game_sec < 120 and -8 <= score_diff < -3:
            return [0.0, 0.0, 1.0] # Must Go!
            
        is_q2_end = 1800 < game_sec <= 1830
        is_q4_end = 0 < game_sec <= 30
        is_desperation_time = is_q2_end or (is_q4_end and -3 <= score_diff <= 0)
        
        if is_desperation_time:
            if yardline_100 <= 53:
                return [0.0, 1.0, 0.0]
            else:
                return [0.0, 0.0, 1.0]
                
        # Query Model Probabilities
        fg_prob = self.predict_fg_success([yardline_100])
        
        fd_prob = 0.54
        if self.fd_conversion_model:
            fd_state = {
                'ydstogo': distance,
                'yardline_100': yardline_100,
                'score_differential': score_diff,
                'game_seconds_remaining': game_sec
            }
            fd_prob = self.fd_conversion_model.predict_conversion_probability(fd_state)
            
        # Dynamic Heuristic weighting using actual model predictions
        if distance <= 1:
            if yardline_100 > 50:
                # Own territory short yardage
                go_w = fd_prob
                punt_w = 1.5 - fd_prob
                fg_w = 0.0
            else:
                # Opponent territory short yardage
                go_w = fd_prob * 2.0
                fg_w = fg_prob * 0.5
                punt_w = 0.0
        else:
            if yardline_100 > 48:
                # Own territory long yardage: No FG option, smooth Punt vs Go
                go_w = fd_prob * 0.1
                punt_w = 1.0 - (fd_prob * 0.1)
                fg_w = 0.0
            else:
                # Opponent territory long yardage
                if yardline_100 <= 40:
                    # Inside 40: Never punt
                    punt_w = 0.0
                    fg_w = fg_prob
                    go_w = fd_prob * 1.2
                else:
                    # Between 40 and 48
                    punt_w = 0.3 * (distance / 10.0)
                    fg_w = fg_prob
                    go_w = fd_prob
                    
        total_w = punt_w + fg_w + go_w
        if total_w <= 0.0:
            return [0.95, 0.0, 0.05]
            
        return [punt_w / total_w, fg_w / total_w, go_w / total_w]

    def predict_fg_success(self, features):
        if self.fg_model:
            yardline_100 = features[0]
            return self.fg_model.predict_success_probability(yardline_100)
        return 0.82

    def predict_air_yards(self, feature_df: pd.DataFrame):
        yd_100 = feature_df.get('yardline_100', 75)
        if isinstance(yd_100, pd.Series):
            yd_100 = yd_100.iloc[0]
        elif isinstance(feature_df, dict):
            yd_100 = feature_df.get('yardline_100', 75)
            
        zone = self.get_zone(yd_100)
        
        # Map dynamic splits
        qb_name = feature_df.get('passer_player_name', 'Unknown') if isinstance(feature_df, dict) else feature_df['passer_player_name'].iloc[0] if 'passer_player_name' in feature_df.columns else 'Unknown'
        recv_name = feature_df.get('receiver_player_name', 'Unknown') if isinstance(feature_df, dict) else feature_df['receiver_player_name'].iloc[0] if 'receiver_player_name' in feature_df.columns else 'Unknown'
        
        def get_qb_val(qb, key):
            if qb not in self.qb_dna: return 0.0
            splits = self.qb_dna[qb].get('splits', {})
            if zone in splits and key in splits[zone]:
                return splits[zone][key]
            return self.qb_dna[qb].get(key, 0.0)
            
        def get_skill_val(player, key):
            if player not in self.skill_dna: return 0.0
            splits = self.skill_dna[player].get('splits', {})
            if zone in splits and key in splits[zone]:
                return splits[zone][key]
            return self.skill_dna[player].get(key, 0.0)
            
        cpoe_val = get_qb_val(qb_name, 'cpoe')
        tgt_share = get_skill_val(recv_name, 'target_share')
        cry_share = get_skill_val(recv_name, 'carry_share')
        
        full_feat = {
            'down': float(feature_df.get('down', 1) if isinstance(feature_df, dict) else feature_df['down'].iloc[0]),
            'ydstogo': float(feature_df.get('ydstogo', 10) if isinstance(feature_df, dict) else feature_df['ydstogo'].iloc[0]),
            'yardline_100': float(yd_100),
            'score_differential': float(feature_df.get('score_differential', 0) if isinstance(feature_df, dict) else feature_df['score_differential'].iloc[0]),
            'game_seconds_remaining': float(feature_df.get('game_seconds_remaining', 1800) if isinstance(feature_df, dict) else feature_df['game_seconds_remaining'].iloc[0]),
            'cpoe_by_filter': cpoe_val,
            'target_share_by_filter': tgt_share,
            'carry_share_by_filter': cry_share
        }
        
        if self.air_yards_sampler:
            return self.air_yards_sampler.sample(full_feat, zone=zone)
        return 8.0

    def predict_yac(self, features):
        yd_100 = features.get('yardline_100', 75)
        zone = self.get_zone(yd_100)
        if self.yac_model:
            return self.yac_model.predict(features, zone=zone)
        return 4.2

    def predict_rush_yards(self, features, roster_traits=None):
        yd_100 = features.get('yardline_100', 75.0)
        zone = self.get_zone(yd_100)
        if self.rush_model:
            return self.rush_model.predict(features, roster_traits, zone=zone)
        return 4.0

    def predict_presnap_penalty_proba(self, features):
        if self.chaos_model:
            return self.chaos_model.predict_presnap_penalty(features)
        return 0.05

    def predict_sack_proba(self, features):
        if self.chaos_model:
            return self.chaos_model.predict_sack_proba(features)
        return 0.066

    def sample_sack_yards(self):
        if self.chaos_model:
            return self.chaos_model.sample_sack_yards()
        return -7

    def predict_interception_proba(self, features):
        if self.chaos_model:
            return self.chaos_model.predict_interception_proba(features)
        return 0.023

    def predict_win_probability(self, game_state):
        if self.wp_model:
            return self.wp_model.predict_win_probability(game_state)
        score_diff = game_state.get('score_differential', 0.0) if isinstance(game_state, dict) else game_state['score_differential'].iloc[0]
        return 0.65 if score_diff > 0 else 0.35 if score_diff < 0 else 0.5
