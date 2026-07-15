import joblib
import json
import numpy as np
import os

MODEL_DIR = "src/nfl_sim/models/air_yards_v_0_1_1"

class AirYardsDoubleHurdleSampler:
    def __init__(self, model_dir=MODEL_DIR):
        self.model_dir = model_dir
        with open(os.path.join(model_dir, "metadata.json"), 'r') as f:
            self.metadata = json.load(f)
        
        self.features = self.metadata['features']
        self.screen_max = self.metadata.get('screen_max', 0)
        self.deep_min = self.metadata.get('deep_min', 20)
        
        # Load boosters for all zones
        self._gate_boosters = {}
        self._reg_boosters = {}
        
        for zone in ['primary', 'redzone', 'goalline']:
            gate_path = os.path.join(model_dir, f"{zone}_tri_gate.joblib")
            if os.path.exists(gate_path):
                gate_sklearn = joblib.load(gate_path)
                self._gate_boosters[zone] = gate_sklearn.get_booster()
                
                self._reg_boosters[zone] = {
                    0: joblib.load(os.path.join(model_dir, f"{zone}_screen_reg.joblib")).get_booster(),
                    1: joblib.load(os.path.join(model_dir, f"{zone}_std_reg.joblib")).get_booster(),
                    2: joblib.load(os.path.join(model_dir, f"{zone}_deep_reg.joblib")).get_booster()
                }

    def sample(self, X, zone='primary'):
        if zone not in self._gate_boosters:
            zone = 'primary'
            
        # 1. Build float32 numpy array
        if isinstance(X, dict):
            X_feat = np.array([[X.get(f, 0.0) for f in self.features]], dtype=np.float32)
            length = 1
        elif isinstance(X, np.ndarray):
            X_feat = X.astype(np.float32, copy=False)
            length = X.shape[0]
        else:
            import pandas as pd
            if isinstance(X, pd.DataFrame):
                X_feat = X[self.features].values.astype(np.float32)
                length = len(X)
            else:
                raise TypeError("X must be a dict or numpy array")

        # 2. Gate
        probs = self._gate_boosters[zone].inplace_predict(X_feat)
        if probs.ndim == 1:
            probs = probs.reshape(1, -1)
        
        cum_probs = np.cumsum(probs, axis=1)
        r = np.random.rand(length)
        levels = (r[:, None] > cum_probs).sum(axis=1)
        
        samples = np.zeros(length)
        noise_std = {0: 1.5, 1: 4.0, 2: 12.0}
        
        for lvl in [0, 1, 2]:
            mask = (levels == lvl)
            if any(mask):
                X_lvl = X_feat[mask]
                base_preds = self._reg_boosters[zone][lvl].inplace_predict(X_lvl)
                noise = np.random.normal(0, noise_std[lvl], size=sum(mask))
                samples[mask] = base_preds + noise
                if lvl == 0: samples[mask] = np.minimum(samples[mask], self.screen_max)
                if lvl == 1: samples[mask] = np.clip(samples[mask], self.screen_max + 1, self.deep_min - 1)
                if lvl == 2: samples[mask] = np.maximum(samples[mask], self.deep_min)
            
        return samples
