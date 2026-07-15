import os
import json
import joblib
import numpy as np

class YACModelV011:
    def __init__(self, model_dir="src/nfl_sim/models/yac_model_v_0_1_1"):
        self.model_dir = model_dir
        self.meta_path = os.path.join(model_dir, "metadata.json")
        
        with open(self.meta_path, 'r') as f:
            self.metadata = json.load(f)
            
        self.features = self.metadata['features']
        
        # Load zone boosters
        self._boosters = {}
        for zone in ['primary', 'redzone', 'goalline']:
            path = os.path.join(model_dir, f"{zone}_yac_reg.joblib")
            if os.path.exists(path):
                self._boosters[zone] = joblib.load(path).get_booster()
                
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
        
    def predict(self, feature_data, zone='primary'):
        if zone not in self._boosters:
            zone = 'primary'
            
        if isinstance(feature_data, (list, np.ndarray)):
            # Fallback legacy list mapping
            yardline_100 = feature_data[0]
            down = feature_data[1]
            ydstogo = feature_data[2]
            air_yards = feature_data[3]
            feature_data = {
                "down": down,
                "ydstogo": ydstogo,
                "yardline_100": yardline_100,
                "air_yards": air_yards,
                "receiver_name": "Unknown",
                "qb_name": "Unknown"
            }
            
        if isinstance(feature_data, dict):
            qb_name = feature_data.get('qb_name') or feature_data.get('passer_player_name') or "Unknown"
            recv_name = feature_data.get('receiver_name') or feature_data.get('receiver_player_name') or "Unknown"
            
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
                'air_yards': feature_data.get('air_yards', 5.0),
                'yardline_100': feature_data.get('yardline_100', 75.0),
                'ydstogo': feature_data.get('ydstogo', 10.0),
                'score_differential': feature_data.get('score_differential', 0.0),
                'game_seconds_remaining': feature_data.get('game_seconds_remaining', 1800.0),
                'cpoe_by_filter': get_qb_val(qb_name, 'cpoe'),
                'target_share_by_filter': get_skill_val(recv_name, 'target_share'),
                'carry_share_by_filter': get_skill_val(recv_name, 'carry_share')
            }
            
            X = np.array([[row.get(col, 0.0) for col in self.features]])
        else:
            raise TypeError("Unsupported feature_data format.")
            
        X = X.astype(np.float32, copy=False)
        pred = float(self._boosters[zone].inplace_predict(X)[0])
        return max(0.0, pred)
