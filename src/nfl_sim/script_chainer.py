
import pandas as pd
import numpy as np
import os

class ScriptChainer:
    def __init__(self, pool_path='data/starting_position_pool.csv'):
        if os.path.exists(pool_path):
            self.pool = pd.read_csv(pool_path)
            self.rz_pool = self.pool[self.pool['is_redzone'] == 1].reset_index(drop=True)
            self.field_pool = self.pool[self.pool['is_redzone'] == 0].reset_index(drop=True)
        else:
            self.pool = None
            print("Warning: Starting Position Pool not found. Falling back to heuristics.")

    def get_next_play(self, yardline_100):
        if self.pool is None:
            return {'play_type': 'pass' if np.random.random() < 0.55 else 'run', 'yards_gained': 5, 'is_turnover': 0, 'penalty': 0}
        
        is_rz = 1 if yardline_100 <= 20 else 0
        pool = self.rz_pool if is_rz else self.field_pool
        
        # Pull a random play from the appropriate geographic pool
        idx = np.random.randint(0, len(pool))
        return pool.iloc[idx].to_dict()
