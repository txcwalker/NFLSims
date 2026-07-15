import os
import json
import joblib
import numpy as np

MODEL_DIR = "src/nfl_sim/models/win_probability_v_0_1_0"

class WinProbabilityModelV010:
    def __init__(self, model_dir=MODEL_DIR):
        self.model_dir = model_dir
        self.model_path = os.path.join(model_dir, "wp_model.joblib")
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
        
    def predict_win_probability(self, game_state):
        """
        Predicts the win probability of the possession team (posteam).
        
        Parameters:
        -----------
        game_state : dict or np.ndarray
            The current game state. Must include:
            - score_differential (posteam score - defteam score)
            - game_seconds_remaining
            - down
            - ydstogo
            - yardline_100
            - posteam_timeouts_remaining
            - defteam_timeouts_remaining
            - receive_2h_ko
            
        Returns:
        --------
        float : The win probability of the posteam in [0.0, 1.0].
        """
        # 1. Parse and validate input
        if isinstance(game_state, dict):
            # --- SPEED OPTIMIZATION: Build bare numpy array directly instead of pd.DataFrame.
            # Avoids all pandas construction overhead.
            row = [float(game_state.get(col, self.fallbacks.get(col, 0.0))) for col in self.features]
            X = np.array([row])
            score_diff = float(game_state.get('score_differential', 0.0))
            sec_rem = float(game_state.get('game_seconds_remaining', 1800.0))
        elif isinstance(game_state, np.ndarray):
            X = game_state if game_state.ndim == 2 else game_state.reshape(1, -1)
            score_diff = float(X[0, self.features.index('score_differential')]) if 'score_differential' in self.features else 0.0
            sec_rem = float(X[0, self.features.index('game_seconds_remaining')]) if 'game_seconds_remaining' in self.features else 1800.0
        else:
            raise TypeError("Unsupported game_state format. Must be dict or numpy ndarray.")
            
        # 2. Run model inference
        prob = float(self.model.predict_proba(X)[0][1])
        
        # 3. Handle physical boundary scenarios
        if sec_rem <= 0.0:
            if score_diff > 0.0:
                prob = 1.0
            elif score_diff < 0.0:
                prob = 0.0
            else:
                prob = 0.5
                
        # Clamp to physically logical bounds [0.0, 1.0]
        prob = max(0.0, min(prob, 1.0))
        
        return prob

if __name__ == "__main__":
    # Quick validation tests
    print("==================================================")
    print("Testing WinProbabilityModelV010 Inference Wrapper")
    print("==================================================")
    
    wp_model = WinProbabilityModelV010()
    
    scenarios = [
        {
            "desc": "Tied 0-0, Kickoff to start the game",
            "state": {
                "score_differential": 0,
                "game_seconds_remaining": 3600,
                "down": 1,
                "ydstogo": 10,
                "yardline_100": 75,
                "posteam_timeouts_remaining": 3,
                "defteam_timeouts_remaining": 3,
                "receive_2h_ko": 1.0
            }
        },
        {
            "desc": "Up by 7 late in the 4th quarter, opponent has no timeouts",
            "state": {
                "score_differential": 7,
                "game_seconds_remaining": 30,
                "down": 1,
                "ydstogo": 10,
                "yardline_100": 10,
                "posteam_timeouts_remaining": 3,
                "defteam_timeouts_remaining": 0,
                "receive_2h_ko": 0.0
            }
        },
        {
            "desc": "Down by 21, end of the 1st quarter",
            "state": {
                "score_differential": -21,
                "game_seconds_remaining": 2700,
                "down": 1,
                "ydstogo": 10,
                "yardline_100": 75,
                "posteam_timeouts_remaining": 3,
                "defteam_timeouts_remaining": 3,
                "receive_2h_ko": 1.0
            }
        },
    ]
    
    for tc in scenarios:
        p = wp_model.predict_win_probability(tc["state"])
        print(f"\nScenario: {tc['desc']}")
        print(f"Resulting P(Win): {p*100:6.2f}%")
