"""
positional_ep_inference.py
==========================
# Status: live | v0.1.0 | 2026-06-21

Inference wrapper for the Positional Expected Points model (V.0.1.0).

Loads the trained XGBoost booster from positional_ep_model.json and exposes
two prediction interfaces:

  predict_expected_points(yardline_100, down, ydstogo, goal_to_go) -> float
      Scalar inference for a single game state (used by API endpoints).

  predict_batch(X: np.ndarray) -> np.ndarray
      Vectorized inference over an (N, 4) feature matrix. Uses XGBoost's
      DMatrix for speed — avoids all pandas overhead inside hot loops.
"""

import os
import json
import numpy as np
import xgboost as xgb

MODEL_DIR = "src/nfl_sim/models/positional_ep_v_0_1_0"


class PositionalEPModelV010:
    """
    Inputs (predict_expected_points):
        yardline_100 : int    yards to the end zone (1=goal, 99=own 1-yd line)
        down         : int    1–4
        ydstogo      : int    yards needed for a first down
        goal_to_go   : int    1 if it's a goal-to-go situation, else 0

    Outputs: float -- expected points value for the possession team

    Purpose:
        Provides a fast, purely situational field-position value independent of
        score, clock, or roster. Used by the chess-style positional evaluator to
        report EP alongside KEP, and by API endpoints for the slider tool.
    """

    def __init__(self, model_dir=MODEL_DIR):
        model_path = os.path.join(model_dir, "positional_ep_model.json")
        meta_path = os.path.join(model_dir, "metadata.json")

        if not os.path.exists(model_path):
            raise FileNotFoundError(
                f"EP model not found at {model_path}. "
                "Run train_positional_ep.py first."
            )

        self.booster = xgb.Booster()
        self.booster.load_model(model_path)

        with open(meta_path, "r") as f:
            self.metadata = json.load(f)

        self.features = self.metadata["features"]

    def predict_expected_points(self, yardline_100, down, ydstogo, goal_to_go=0):
        """Single-state scalar prediction."""
        X = np.array([[yardline_100, down, ydstogo, goal_to_go]], dtype=np.float32)
        dm = xgb.DMatrix(X, feature_names=self.features)
        return float(self.booster.predict(dm)[0])

    def predict_batch(self, X):
        """
        Inputs:  X : np.ndarray of shape (N, 4) with columns matching self.features.
        Outputs: np.ndarray of shape (N,) with EP predictions.
        Purpose: Vectorized inference for bulk evaluation (e.g., full game play stream).
        """
        dm = xgb.DMatrix(X.astype(np.float32), feature_names=self.features)
        return self.booster.predict(dm)


if __name__ == "__main__":
    print("=" * 50)
    print("PositionalEPModelV010 — Inference Sanity Check")
    print("=" * 50)

    ep_model = PositionalEPModelV010()

    print("\n[EP by yardline, 1st & 10]")
    for yl in [99, 75, 50, 25, 10, 5, 1]:
        ep = ep_model.predict_expected_points(yl, down=1, ydstogo=10, goal_to_go=0)
        print(f"  yardline_100={yl:3d}  EP={ep:+.3f}")

    print("\n[EP by down & distance, midfield (yl=50)]")
    for down, dist in [(1, 10), (2, 7), (3, 5), (3, 10), (4, 1)]:
        ep = ep_model.predict_expected_points(50, down=down, ydstogo=dist)
        print(f"  {down}rd & {dist:2d}  EP={ep:+.3f}")
