import os
import json


class ClockPaceModelV010:
    """
    Empirical bootstrap pools for snap-to-snap clock runoff, keyed by
    (quarter/time-window x score-margin tier). See metadata.json for the
    cell key format and docs/audit/clock_physics_v020/pace_grid.md for the
    real-data grid these pools were built from.

    Vectorized sampling happens in game_engine.py directly against `self.pools`
    (matching the codebase convention of chaos_model's gamma sack-yardage
    sampling, which also bypasses per-call wrappers for the hot loop) — this
    class just owns loading and cell-key resolution.
    """

    def __init__(self, model_dir="src/nfl_sim/models/clock_pace_v_0_1_0"):
        self.model_dir = model_dir
        pools_path = os.path.join(model_dir, "pace_pools.json")
        meta_path = os.path.join(model_dir, "metadata.json")

        if not os.path.exists(pools_path):
            raise FileNotFoundError(f"Pace pools JSON not found at {pools_path}")
        if not os.path.exists(meta_path):
            raise FileNotFoundError(f"Metadata JSON not found at {meta_path}")

        with open(pools_path, 'r') as f:
            self.pools = json.load(f)
        with open(meta_path, 'r') as f:
            self.metadata = json.load(f)

        self.row_groups = self.metadata['row_groups']
        self.margin_tiers = self.metadata['margin_tiers']
        # Fallback pool for any (row_group, tier) combo that doesn't exist
        # (e.g. Q1's merged-away 3+ tiers) — use the whole-quarter Q1 tied pool.
        self._fallback_pool = self.pools.get("Q1|Tied", [38])

    def get_pool(self, row_group, margin_tier):
        key = f"{row_group}|{margin_tier}"
        return self.pools.get(key, self._fallback_pool)
