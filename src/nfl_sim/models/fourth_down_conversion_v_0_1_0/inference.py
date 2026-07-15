import os
import json
import joblib
import pandas as pd
import numpy as np

MODEL_DIR = "src/nfl_sim/models/fourth_down_conversion_v_0_1_0"

class FourthDownConversionModelV010:
    def __init__(self, model_dir=MODEL_DIR):
        self.model_dir = model_dir
        self.model_path = os.path.join(model_dir, "fd_conversion_model.joblib")
        self.meta_path = os.path.join(model_dir, "metadata.json")
        
        if not os.path.exists(self.model_path):
            raise FileNotFoundError(f"Model file not found at {self.model_path}")
        if not os.path.exists(self.meta_path):
            raise FileNotFoundError(f"Metadata file not found at {self.meta_path}")
            
        # Load serialized model
        self.model = joblib.load(self.model_path)
        
        # Load metadata
        with open(self.meta_path, 'r') as f:
            self.metadata = json.load(f)
            
        self.features = self.metadata['features']
        self.fallbacks = self.metadata['fallbacks']
        
    def predict_conversion_probability(self, game_state):
        """
        Predicts the conversion probability of a 4th down attempt.
        Supports single states (dict or 1-row DataFrame) and batch states.
        """
        # 1. Parse and validate input
        is_batch = False
        if isinstance(game_state, dict):
            row = []
            for col in self.features:
                val = game_state.get(col, self.fallbacks.get(col))
                if val is None:
                    val = self.fallbacks.get(col)
                row.append(float(val))
            X = pd.DataFrame([row], columns=self.features)
        elif isinstance(game_state, pd.DataFrame):
            X = game_state.copy()
            for col in self.features:
                if col not in X.columns:
                    X[col] = self.fallbacks.get(col)
                else:
                    X[col] = X[col].fillna(self.fallbacks.get(col))
            X = X[self.features].astype(float)
            is_batch = True
        else:
            raise TypeError("Unsupported game_state format. Must be dict or DataFrame.")
            
        # 2. Handle absolute physical boundaries pre-inference
        if is_batch:
            # Vectorized boundary checks
            ydstogo = X['ydstogo'].values
            yardline_100 = X['yardline_100'].values
            
            # Distance greater than yardline_100 is capped
            capped_ydstogo = np.minimum(ydstogo, yardline_100)
            X['ydstogo'] = capped_ydstogo
            
            # Run model inference
            probs = self.model.predict_proba(X)[:, 1]
            
            # Apply post-inference rules vectorially
            # Distance of 0 or negative is always converted
            probs[ydstogo <= 0.0] = 1.0
            
            # If distance is extremely long, decay toward 0.02
            probs[(ydstogo >= 35.0)] = np.minimum(probs[(ydstogo >= 35.0)], 0.02)
            
            # Clamp to [0.0, 1.0]
            probs = np.clip(probs, 0.0, 1.0)
            return probs
        else:
            ydstogo = float(X.iloc[0]['ydstogo'])
            yardline_100 = float(X.iloc[0]['yardline_100'])
            
            if ydstogo <= 0.0:
                return 1.0
                
            if ydstogo > yardline_100:
                X.loc[0, 'ydstogo'] = yardline_100
                ydstogo = yardline_100
                
            prob = float(self.model.predict_proba(X)[0][1])
            
            if ydstogo >= 35.0:
                prob = min(prob, 0.02)
                
            prob = max(0.0, min(prob, 1.0))
            return prob

if __name__ == "__main__":
    # Quick validation tests (will run once trained)
    print("==================================================")
    print("Testing FourthDownConversionModelV010 Inference Wrapper")
    print("==================================================")
    
    try:
        fd_model = FourthDownConversionModelV010()
        
        scenarios = [
            {
                "desc": "4th and 1 on own 20",
                "state": {
                    "ydstogo": 1,
                    "yardline_100": 80,
                    "score_differential": 0,
                    "game_seconds_remaining": 1800
                }
            },
            {
                "desc": "4th and 10 on opponent 35",
                "state": {
                    "ydstogo": 10,
                    "yardline_100": 35,
                    "score_differential": -3,
                    "game_seconds_remaining": 60
                }
            },
            {
                "desc": "4th and 35 in Own Territory (Desperation)",
                "state": {
                    "ydstogo": 35,
                    "yardline_100": 70,
                    "score_differential": -7,
                    "game_seconds_remaining": 15
                }
            },
            {
                "desc": "4th and Goal from the 1 (Goal Line)",
                "state": {
                    "ydstogo": 1,
                    "yardline_100": 1,
                    "score_differential": 0,
                    "game_seconds_remaining": 1800
                }
            }
        ]
        
        for tc in scenarios:
            p = fd_model.predict_conversion_probability(tc["state"])
            print(f"\nScenario: {tc['desc']}")
            print(f"Inputs: {tc['state']}")
            print(f"Resulting P(Convert): {p*100:6.2f}%")
    except Exception as e:
        print(f"Error loading model: {e}")
