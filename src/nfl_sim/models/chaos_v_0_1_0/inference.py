import os
import json
import joblib
import numpy as np
import scipy.stats as stats

class ChaosModelV010:
    def __init__(self, model_dir="src/nfl_sim/models/chaos_v_0_1_0"):
        self.model_dir = model_dir
        self.g1_path = os.path.join(model_dir, "gate_1_presnap.joblib")
        self.g2_path = os.path.join(model_dir, "gate_2_sack.joblib")
        self.g2b_path = os.path.join(model_dir, "gate_2b_trench_correction.joblib")
        self.g4_path = os.path.join(model_dir, "gate_4_interception.joblib")
        self.meta_path = os.path.join(model_dir, "metadata.json")

        # Load artifacts
        if not os.path.exists(self.g1_path):
            raise FileNotFoundError(f"Gate 1 joblib not found at {self.g1_path}")
        if not os.path.exists(self.g2_path):
            raise FileNotFoundError(f"Gate 2 joblib not found at {self.g2_path}")
        if not os.path.exists(self.g2b_path):
            raise FileNotFoundError(f"Gate 2b joblib not found at {self.g2b_path}")
        if not os.path.exists(self.g4_path):
            raise FileNotFoundError(f"Gate 4 joblib not found at {self.g4_path}")
        if not os.path.exists(self.meta_path):
            raise FileNotFoundError(f"Metadata JSON not found at {self.meta_path}")

        g1_model = joblib.load(self.g1_path)
        g2_sklearn = joblib.load(self.g2_path)
        g2b_sklearn = joblib.load(self.g2b_path)
        g4_sklearn = joblib.load(self.g4_path)

        # Store raw boosters: inplace_predict bypasses sklearn validation
        # overhead. Gate 2b gets the same treatment as Gate 2/4 for
        # consistency -- it's called every pass play, same hot path.
        self._g2_booster = g2_sklearn.get_booster()
        self._g2b_booster = g2b_sklearn.get_booster()
        self._g4_booster = g4_sklearn.get_booster()

        # Gate 2b and Gate 4 were both trained with early stopping, so each
        # has a validated best_iteration cutoff short of its full tree count
        # (Gate 2b: 42 of 73; Gate 4: 88 of 119) -- predict_proba() applies
        # that truncation automatically during training/eval, but
        # inplace_predict() does NOT unless iteration_range is passed
        # explicitly. Without it, every live prediction silently used more
        # (later, past-best-checkpoint) trees than what was actually
        # validated -- measured divergence from the validated output: Gate 4
        # up to 0.058 probability, Gate 2b up to 0.035 (mean 0.0033). Gate 2
        # (`_g2_booster`) has no best_iteration (not trained with early
        # stopping) so needs no equivalent.
        g2b_best_it = getattr(g2b_sklearn, 'best_iteration', None)
        g4_best_it = getattr(g4_sklearn, 'best_iteration', None)
        self.g2b_iteration_range = (0, g2b_best_it + 1) if g2b_best_it is not None else None
        self.g4_iteration_range = (0, g4_best_it + 1) if g4_best_it is not None else None

        with open(self.meta_path, 'r') as f:
            self.metadata = json.load(f)

        self.g1_features = self.metadata['gate_1']['features']
        self.g2_features = self.metadata['gate_2']['features']
        self.g2b_features = self.metadata['gate_2b']['features']
        self.g4_features = self.metadata['gate_4']['features']

        self.gamma_params = self.metadata['gate_2']['gamma_params']
        
        # --- SPEED OPTIMIZATION: Gate 1 is a simple LogisticRegression.
        # Extract coef/intercept at init time and compute the sigmoid directly,
        # bypassing ALL scikit-learn validation overhead (~248x faster per call).
        self._g1_coef = g1_model.coef_[0]          # shape: (n_features,)
        self._g1_intercept = g1_model.intercept_[0] # scalar
        
    def predict_presnap_penalty(self, feature_data):
        """
        Calculates the probability of a pre-snap penalty (Gate 1).
        Uses raw math (sigmoid of dot product) instead of sklearn.predict_proba
        for ~248x speedup in the high-frequency simulation loop.

        Inputs: down, ydstogo, yardline_100, score_differential,
                game_seconds_remaining, is_home
        """
        # Build feature vector in the same order the model was trained on
        x = [feature_data.get(col, 0.0) for col in self.g1_features]
        # σ(w·x + b)  — numerically stable sigmoid
        dot = sum(xi * wi for xi, wi in zip(x, self._g1_coef)) + self._g1_intercept
        return 1.0 / (1.0 + np.exp(-dot))
        
    def predict_sack_proba(self, feature_data):
        """
        Calculates the probability of a pass play resulting in a sack (Gate 2).
        Uses booster.inplace_predict with float32 array for maximum speed.
        """
        X = np.array([[feature_data.get(col, 0.0) for col in self.g2_features]], dtype=np.float32)
        return float(self._g2_booster.inplace_predict(X)[0])

    def predict_sack_proba_corrected(self, feature_data):
        """
        Gate 2b: corrects Gate 2's own sack probability using the newer
        pass-blocking/pass-rush trench composites (pass_block_off_z,
        pass_def_z) that Gate 2 itself was never trained on. Does NOT alter
        Gate 2's own weights or calibration -- feature_data must already
        include 'gate2_prob' (Gate 2's calibrated output for this play, i.e.
        predict_sack_proba(...) * SACK_PROB_CALIBRATION_MULT, clipped) plus
        pass_block_off_z/pass_def_z for this play's teams.
        """
        X = np.array([[feature_data.get(col, 0.0) for col in self.g2b_features]], dtype=np.float32)
        return float(self._g2b_booster.inplace_predict(X, iteration_range=self.g2b_iteration_range)[0])
        
    def sample_sack_yards(self):
        """
        Samples the yardage lost on a sack using the fitted Negative Gamma distribution.
        Returns a negative integer representing yards lost (e.g. -7).
        """
        shape = self.gamma_params['shape']
        loc = self.gamma_params['loc']
        scale = self.gamma_params['scale']
        
        # Sample positive yards lost
        yards_lost = stats.gamma.rvs(shape, loc, scale)
        # Enforce realistic bounds (median ~7 yards lost, clip between 1 and 25)
        yards_lost_clipped = max(1.0, min(float(yards_lost), 25.0))
        
        return -int(round(yards_lost_clipped))
        
    def predict_interception_proba(self, feature_data):
        """
        Calculates the probability of a clean incomplete pass being intercepted (Gate 4).
        Uses booster.inplace_predict with float32 array for maximum speed.
        """
        X = np.array([[feature_data.get(col, 0.0) for col in self.g4_features]], dtype=np.float32)
        return float(self._g4_booster.inplace_predict(X, iteration_range=self.g4_iteration_range)[0])
