import os
import json
import numpy as np
import xgboost as xgb

class RushYardsModelV010:
    def __init__(self, model_dir="src/nfl_sim/models/rush_yards_v_0_1_0"):
        self.model_dir = model_dir
        self.meta_path = os.path.join(model_dir, "metadata.json")
        
        with open(self.meta_path, 'r') as f:
            self.metadata = json.load(f)
            
        self.features = self.metadata['features']
        self.residuals_pools = self.metadata.get('residuals_pools', {})
        
        # Load boosters
        self._boosters = {}
        for zone in ['primary', 'redzone', 'goalline']:
            path = os.path.join(model_dir, f"{zone}_rush_yards_model.json")
            if os.path.exists(path):
                m = xgb.XGBRegressor()
                m.load_model(path)
                self._boosters[zone] = m.get_booster()
                
        # Find project root dynamically
        curr = os.path.dirname(os.path.abspath(__file__))
        project_root = curr
        while project_root and project_root != os.path.dirname(project_root):
            if os.path.exists(os.path.join(project_root, 'data')):
                break
            project_root = os.path.dirname(project_root)
            
        with open(os.path.join(project_root, "data", "dna", "qb_dna.json"), 'r') as f:
            self.qb_dna = json.load(f)
        with open(os.path.join(project_root, "data", "dna", "skill_dna.json"), 'r') as f:
            self.skill_dna = json.load(f)
            
        self.fallbacks = {
            "cpoe": 0.0,
            "target_share": 0.0,
            "carry_share": 0.0
        }
        
    def predict(self, feature_data, roster_traits=None, zone='primary'):
        if zone not in self._boosters:
            zone = 'primary'
            
        if isinstance(feature_data, (list, np.ndarray)):
            # Fallback legacy list mapping
            yardline_100 = feature_data[0]
            ydstogo = feature_data[1]
            game_seconds_remaining = feature_data[2]
            score_differential = feature_data[3]
            feature_data = {
                "ydstogo": ydstogo,
                "yardline_100": yardline_100,
                "game_seconds_remaining": game_seconds_remaining,
                "score_differential": score_differential,
                "rusher_name": "Unknown",
                "active_qb": "Unknown"
            }
            
        if isinstance(feature_data, dict):
            rusher_name = feature_data.get('rusher_name') or feature_data.get('rusher') or "Unknown"
            qb_name = feature_data.get('active_qb') or feature_data.get('qb_name') or "Unknown"
            
            # Map splits
            def get_qb_val(qb, key):
                if qb not in self.qb_dna: return self.fallbacks.get(key, 0.0)
                splits = self.qb_dna[qb].get('splits', {})
                if zone in splits and key in splits[zone]:
                    return splits[zone][key]
                return self.qb_dna[qb].get(key, self.fallbacks.get(key, 0.0))
                
            def get_skill_val(player, key):
                if player not in self.skill_dna: return self.fallbacks.get(key, 0.0)
                splits = self.skill_dna[player].get('splits', {})
                if zone in splits and key in splits[zone]:
                    return splits[zone][key]
                return self.skill_dna[player].get(key, self.fallbacks.get(key, 0.0))
                
            row = {
                'yardline_100': float(feature_data.get('yardline_100', 75.0)),
                'ydstogo': float(feature_data.get('ydstogo', 10.0)),
                'game_seconds_remaining': float(feature_data.get('game_seconds_remaining', 1800.0)),
                'score_differential': float(feature_data.get('score_differential', 0.0)),
                'cpoe_by_filter': get_qb_val(qb_name, 'cpoe'),
                'target_share_by_filter': get_skill_val(rusher_name, 'target_share'),
                'carry_share_by_filter': get_skill_val(rusher_name, 'carry_share')
            }
            
            X = np.array([[row.get(col, 0.0) for col in self.features]])
        else:
            raise TypeError("Unsupported feature_data format.")
            
        X_f32 = X.astype(np.float32, copy=False)
        pred_log = float(self._boosters[zone].inplace_predict(X_f32)[0])
        pred_unshifted = np.exp(pred_log) - 30.0
        
        # Empirical residual noise injection
        pool = self.residuals_pools.get(zone, [0.0])
        noise = float(np.random.choice(pool))
        pred_noisy = pred_unshifted + noise
        
        return max(-10.0, min(pred_noisy, float(feature_data.get('yardline_100', 100.0))))
