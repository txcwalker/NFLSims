import os
import json
import numpy as np
import xgboost as xgb

MODEL_DIR = "src/nfl_sim/models/efsd_v_0_1_0"

class EFSDModelV010:
    """
    Inputs:  game state as dict or numpy array
    Outputs: predicted expected final score differential (posteam final score - defteam final score)
    Purpose: fast scalar and batch inference for EFSD predictions from any game state
    """

    def __init__(self, model_dir=MODEL_DIR):
        self.model_dir = model_dir
        self.model_path = os.path.join(model_dir, "efsd_model.json")
        self.meta_path = os.path.join(model_dir, "metadata.json")

        if not os.path.exists(self.model_path):
            raise FileNotFoundError(f"Model file not found at {self.model_path}")
        if not os.path.exists(self.meta_path):
            raise FileNotFoundError(f"Metadata file not found at {self.meta_path}")

        # Load serialized model
        self.booster = xgb.Booster()
        self.booster.load_model(self.model_path)

        # Load metadata
        with open(self.meta_path, 'r') as f:
            self.metadata = json.load(f)

        self.features = self.metadata['features']

    def predict_efsd(self, game_state):
        """
        Inputs:  game_state (dict or numpy array with required fields)
        Outputs: float — predicted final score differential
        Purpose: single-point prediction for one game state

        Expected game_state dict keys:
          - score_differential
          - game_seconds_remaining
          - down
          - ydstogo
          - yardline_100
          - posteam_timeouts_remaining
          - defteam_timeouts_remaining
          - receive_2h_ko
        """
        if isinstance(game_state, dict):
            row = [float(game_state.get(col, 0.0)) for col in self.features]
            X = np.array([row], dtype=np.float32)
        elif isinstance(game_state, np.ndarray):
            X = game_state.astype(np.float32)
            if X.ndim == 1:
                X = X.reshape(1, -1)
        else:
            raise TypeError("Unsupported game_state format. Must be dict or numpy ndarray.")

        pred = float(self.booster.inplace_predict(X)[0])
        return pred

    def predict_batch(self, X):
        """
        Inputs:  X (numpy array, shape [n_samples, n_features])
        Outputs: numpy array of predictions, shape [n_samples]
        Purpose: fast vectorized inference for evaluator integration
        """
        if not isinstance(X, np.ndarray):
            X = np.array(X)
        X = X.astype(np.float32)
        if X.ndim == 1:
            X = X.reshape(1, -1)
        preds = self.booster.inplace_predict(X)
        return preds


if __name__ == "__main__":
    print("=" * 60)
    print("Testing EFSDModelV010 Inference Wrapper")
    print("=" * 60)

    efsd_model = EFSDModelV010()

    # Test scenarios
    scenarios = [
        {
            "desc": "Tied 0-0, kickoff to start the game",
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

    print("\n--- Scalar Predictions (dict input) ---")
    for tc in scenarios:
        pred = efsd_model.predict_efsd(tc["state"])
        print(f"\n{tc['desc']}")
        print(f"  Predicted EFSD: {pred:+.2f} points")

    print("\n--- Batch Predictions (numpy array input) ---")
    batch_data = np.array([
        [0, 3600, 1, 10, 75, 3, 3, 1.0],
        [7, 30, 1, 10, 10, 3, 0, 0.0],
        [-21, 2700, 1, 10, 75, 3, 3, 1.0],
    ], dtype=np.float32)
    batch_preds = efsd_model.predict_batch(batch_data)
    for i, pred in enumerate(batch_preds):
        print(f"  Scenario {i+1}: {pred:+.2f} points")

    print("\n[OK] Inference wrapper validation complete.")
