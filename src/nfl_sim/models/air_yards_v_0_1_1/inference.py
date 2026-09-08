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

        # Post-regression sampling params (A2 Phase 1, 2026-09-06). The regressors
        # predict the correct MEAN air-yards per level; the job here is to add
        # realistic scatter around it. The old code used a symmetric Gaussian
        # (std 1.5/4.0/12.0) with hard clips [1,19]/[>=20] -- but the real
        # conditional air-yards distribution within the std and deep levels is
        # strongly right-skewed (many more short throws than long), so a
        # symmetric bell over-fills the middle depth buckets. Calibrated
        # 2026-09-06 (scripts/eda/calibrate_air_yards_sampler.py against the real
        # 2021-2025 target-depth histogram, fit score 42 -> 8):
        #   screen -> clipped Gaussian (its distribution IS ~symmetric, small)
        #   std/deep -> shifted Gamma: sample = floor + (pred - floor) * G,
        #               G ~ Gamma(k, 1/k)  =>  E[sample] == pred (mean kept),
        #               k sets the skew (k=1 exponential, k->inf Gaussian).
        # See docs/implementation_plans/completion_rate_calibration_plan.md Phase 1.
        s = self.metadata.get("sampling", {})
        scr = s.get("screen", {})
        std = s.get("std", {})
        dp = s.get("deep", {})
        self.screen_sd = float(scr.get("sd", 1.5))
        self.screen_clip_hi = float(scr.get("clip_hi", self.screen_max))
        self.std_k = float(std.get("k", 2.0))
        self.std_floor = float(std.get("floor", -1.0))
        self.deep_k = float(dp.get("k", 1.0))
        self.deep_floor = float(dp.get("floor", 14.0))

        # Calibration capture hook (off by default, zero cost). When True,
        # every sample() call appends (X_feat.copy(), zone) to self.captured
        # so the calibration script can replay the gate+regressors offline
        # under candidate params without re-running the game engine.
        self.capture = False
        self.captured = []
        
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

        if self.capture:
            self.captured.append((np.array(X_feat, copy=True), zone))

        # 2. Gate
        probs = self._gate_boosters[zone].inplace_predict(X_feat)
        if probs.ndim == 1:
            probs = probs.reshape(1, -1)

        cum_probs = np.cumsum(probs, axis=1)
        r = np.random.rand(length)
        levels = (r[:, None] > cum_probs).sum(axis=1)

        samples = np.zeros(length)

        for lvl in [0, 1, 2]:
            mask = (levels == lvl)
            n = int(mask.sum())
            if n == 0:
                continue
            base_preds = self._reg_boosters[zone][lvl].inplace_predict(X_feat[mask])

            if lvl == 0:
                # screen: near-symmetric, keep clipped Gaussian
                samples[mask] = np.minimum(
                    base_preds + np.random.normal(0, self.screen_sd, size=n),
                    self.screen_clip_hi,
                )
            else:
                # std / deep: shifted Gamma, mean == base_preds, right-skewed
                k = self.std_k if lvl == 1 else self.deep_k
                floor = self.std_floor if lvl == 1 else self.deep_floor
                scale = np.maximum(base_preds - floor, 0.25)
                samples[mask] = floor + scale * np.random.gamma(k, 1.0 / k, size=n)

        return samples
