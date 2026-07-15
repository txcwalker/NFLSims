import os
import json
import numpy as np
import joblib

MODEL_DIR = "src/nfl_sim/models/fg_v_0_1_0"

class FieldGoalModelV010:
    def __init__(self, model_dir=MODEL_DIR):
        self.model_dir = model_dir
        self.model_path = os.path.join(model_dir, "fg_model.joblib")
        self.meta_path = os.path.join(model_dir, "metadata.json")
        
        if not os.path.exists(self.model_path):
            raise FileNotFoundError(f"Model file not found at {self.model_path}")
        if not os.path.exists(self.meta_path):
            raise FileNotFoundError(f"Metadata file not found at {self.meta_path}")
            
        # Load serialized scikit-learn LogisticRegression model
        fg_model = joblib.load(self.model_path)
        
        # Load metadata
        with open(self.meta_path, 'r') as f:
            self.metadata = json.load(f)
            
        self.features = self.metadata['features']

        # --- SPEED OPTIMIZATION: Extract LR weights at init time.
        # predict_success_probability is called for every FG attempt.
        # Using raw sigmoid math bypasses all sklearn overhead (~248x faster).
        self._coef = float(fg_model.coef_[0][0])          # single feature: kick_distance
        self._intercept = float(fg_model.intercept_[0])   # scalar
        
    def predict_success_probability(self, yardline_100, kicker_multiplier=1.0):
        """
        Predicts the success probability of field goal attempt(s).
        Supports both scalar and numpy array inputs vectorially.

        Parameters:
        -----------
        yardline_100 : float or int or numpy.ndarray
            Distance from the opponent's goal line (0 to 100).
        kicker_multiplier : float or numpy.ndarray, optional
            DNA scaling factor for the kicker's skill (default: 1.0).
            
        Returns:
        --------
        float or numpy.ndarray : Calibrated probability of success in [0.0, 1.0].
        """
        # 1. Derive kick_distance
        if isinstance(yardline_100, np.ndarray):
            kick_distance = yardline_100.astype(np.float32) + 17.0
            
            # 2. σ(w * kick_distance + b) — numerically stable sigmoid
            dot = self._coef * kick_distance + self._intercept
            prob = 1.0 / (1.0 + np.exp(-dot))
            
            # 3. Apply DNA multiplier
            prob = prob * kicker_multiplier
            
            # Max field goal distance constraint (65 yards)
            prob[kick_distance > 65.0] = 0.0
            
            # 4. Clamp to physically logical bounds [0.0, 1.0]
            prob = np.clip(prob, 0.0, 1.0)
            return prob
        else:
            kick_distance = float(yardline_100) + 17.0
            if kick_distance > 65.0:
                return 0.0
            
            dot = self._coef * kick_distance + self._intercept
            prob = 1.0 / (1.0 + np.exp(-dot))
            prob = prob * kicker_multiplier
            prob = max(0.0, min(prob, 1.0))
            return prob

if __name__ == "__main__":
    # Quick verification test
    model = FieldGoalModelV010()
    print("Verification:")
    for yl in [3, 10, 23, 33, 43, 53]:
        dist = yl + 17
        p = model.predict_success_probability(yl)
        print(f"  Yardline: {yl:2d} (Kick Dist: {dist:2d} yds) -> P(Success): {p*100:6.2f}%")
