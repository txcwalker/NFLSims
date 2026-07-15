"""
nfl_positional_evaluator.py
===========================
# Status: live | v1.0.0 | 2026-06-21

Chess-style football position evaluator (V1).

Translates a live game state into point-equivalent positional values and ranks
the first-play tactical concepts (Run / Screen / Short / Medium / Deep) by how
much they move the position, expressed in Kickoff-Equivalent Points (KEP) and
Expected Points (EP).

Public classes:

  KEPConverter
    Builds the kickoff Win-Probability curve from the trained WP model, denoises
    it with isotonic regression, and inverts it so any current Win Probability maps
    to the score differential at kickoff that produces the same WP. This is the
    clock-aware "centipawn" evaluation axis.

  PositionalEvaluator
    Seeds the vectorized NFLGameEngine at an arbitrary live state, runs N drive-long
    simulations to change of possession, classifies the FIRST executed play of each
    simulation lane into a tactical concept, converts every drive-ending outcome into
    KEP and EFSD, and reports the mean + delta per concept on both axes. Also reports
    EP for the starting state directly from the trained positional EP model.

    KEP vs EFSD: KEP inverts the Win-Probability curve onto a bounded (±24) points axis,
    while EFSD (Expected Final Score Differential) directly regresses the unbounded final
    margin. EFSD needs no curve inversion — the model output is already in points — so the
    possession flip is a simple sign negation (efsd_off = -efsd_opp) and terminal lanes
    report the exact final margin.

Design notes:
  - Rollouts use the engine's real team DNA. League-average roster neutralization
    is a documented follow-up toggle (see WORKFLOW.md / AGENTS.md).
  - Drive-ending KEP is evaluated at a canonical "ball to the opponent at a fresh
    1st-and-10 from their own 25" state, carrying the post-drive score and clock.

Run standalone (from repo root) for a sanity check:
    python -m src.nfl_sim.nfl_positional_evaluator
"""

import numpy as np
from sklearn.isotonic import IsotonicRegression

try:  # package-relative (normal import path)
    from .game_engine import NFLGameEngine
    from .models.win_probability_v_0_1_0.inference import WinProbabilityModelV010
    from .models.positional_ep_v_0_1_0.positional_ep_inference import PositionalEPModelV010
    from .models.efsd_v_0_1_0.efsd_inference import EFSDModelV010
except ImportError:  # pragma: no cover - fallback when run as a loose script
    from game_engine import NFLGameEngine
    from models.win_probability_v_0_1_0.inference import WinProbabilityModelV010
    from models.positional_ep_v_0_1_0.positional_ep_inference import PositionalEPModelV010
    from models.efsd_v_0_1_0.efsd_inference import EFSDModelV010


# Canonical kickoff reference state used to define the KEP axis:
# t = 3600s, 1st & 10 from the receiving team's own 25 (yardline_100 = 75), full timeouts.
KICKOFF_REFERENCE = {
    "game_seconds_remaining": 3600.0,
    "down": 1.0,
    "ydstogo": 10.0,
    "yardline_100": 75.0,
    "posteam_timeouts_remaining": 3.0,
    "defteam_timeouts_remaining": 3.0,
    "receive_2h_ko": 0.0,
}

# Air-yards thresholds for first-play concept classification (yards).
SCREEN_MAX_AIR = 0      # air_yards <= 0  -> Screen
SHORT_MAX_AIR = 10      # 0 < air < 10    -> Short
MEDIUM_MAX_AIR = 20     # 10 <= air < 20  -> Medium (>= 20 -> Deep)

CONCEPTS = ["Run", "Screen", "Short", "Medium", "Deep"]


class KEPConverter:
    """
    Converts Win Probability <-> Kickoff-Equivalent Points (KEP).

    Inputs:
        wp_model : WinProbabilityModelV010
            Trained WP model, queried across a score-differential grid at the fixed
            kickoff reference state to build a monotonic WP(score_diff) curve.
        grid_lo, grid_hi : float
            Score-differential grid bounds (points). KEP saturates at these bounds.
            Kept to the range with real training support (~ +/-24): the WP classifier
            has essentially no data for large leads/deficits at a full game clock, so
            wider grids feed it out-of-distribution garbage.

    Outputs:
        kep_from_wp(wp)         -> float | np.ndarray : KEP for a given WP (vectorized).
        kep_from_state(state)   -> float              : KEP for a full game state.

    Purpose:
        KEP is the score differential at kickoff that matches the current Win Probability,
        i.e. KEP = WP_kickoff^{-1}(WP_current). Because WP at kickoff is strictly a function
        of the score differential, KEP is a clock-aware remap of WP onto the points axis.

    Why isotonic regression (not a running max):
        The raw kickoff WP curve is noisy and locally non-monotonic (out-of-distribution
        score diffs at t=3600). IsotonicRegression is the least-squares projection onto the
        monotone-increasing cone -- it AVERAGES OUT spikes. A running max (np.maximum.accumulate)
        would instead PROPAGATE a single spurious high value forward and corrupt the inverse.
    """

    def __init__(self, wp_model, grid_lo=-24.0, grid_hi=24.0):
        self.wp_model = wp_model
        # Integer score grid over the supported range.
        self.score_grid = np.arange(grid_lo, grid_hi + 1.0, 1.0)

        # Build the raw kickoff WP curve: WP of the receiving team for each score differential.
        raw = np.empty_like(self.score_grid, dtype=np.float64)
        for i, sd in enumerate(self.score_grid):
            state = dict(KICKOFF_REFERENCE)
            state["score_differential"] = float(sd)
            raw[i] = wp_model.predict_win_probability(state)
        self.wp_curve_raw = raw

        # Monotone (non-decreasing) denoised fit, then a tiny ramp so np.interp has a
        # strictly increasing x-axis for a stable inverse.
        iso = IsotonicRegression(increasing=True, y_min=0.001, y_max=0.999,
                                 out_of_bounds="clip").fit(self.score_grid, raw)
        fit = iso.predict(self.score_grid)
        self.wp_curve_mono = fit + np.arange(fit.size) * 1e-9

    def kep_from_wp(self, wp):
        """Invert the kickoff curve: given WP (scalar or array), return KEP (points)."""
        return np.interp(wp, self.wp_curve_mono, self.score_grid)

    def kep_from_state(self, game_state):
        """Convenience: current WP -> KEP for a full WP-model game-state dict."""
        wp = self.wp_model.predict_win_probability(game_state)
        return float(self.kep_from_wp(wp))


class PositionalEvaluator:
    """
    Drive-rollout positional evaluator.

    Inputs:
        wp_model : WinProbabilityModelV010 | None
            WP model used for both KEP construction and drive-ending evaluation.
            Built on demand if not supplied.

    Outputs (from .evaluate()):
        dict with the starting KEP, per-concept mean KEP + KEP delta + sample counts,
        and excluded-bucket counts (sacks/scrambles/unclassified).

    Purpose:
        Runs N drive-long simulations from a live state, classifies each lane's first
        executed play, converts each drive outcome to KEP, and reports how much each
        tactical concept is worth relative to standing pat (the starting KEP).
    """

    # Safety cap on plays per drive (a real drive is rarely > ~20 plays).
    MAX_STEPS = 40

    def __init__(self, wp_model=None, ep_model=None, efsd_model=None):
        self.wp_model = wp_model if wp_model is not None else WinProbabilityModelV010()
        self.ep_model = ep_model if ep_model is not None else PositionalEPModelV010()
        self.efsd_model = efsd_model if efsd_model is not None else EFSDModelV010()
        self.kep = KEPConverter(self.wp_model)
        self._features = self.wp_model.features        # WP feature order for batched inference
        self._efsd_features = self.efsd_model.features  # EFSD feature order (may differ from WP)

    # ------------------------------------------------------------------ helpers
    @staticmethod
    def _clock_to_quarter_time(game_seconds_remaining):
        """Map total game-seconds-remaining -> (quarter, seconds_left_in_quarter)."""
        gsr = max(0, int(round(game_seconds_remaining)))
        if gsr <= 0:
            return 4, 0
        q_from_end = min(3, (gsr - 1) // 900)
        quarter = 4 - q_from_end
        time_remaining = gsr - q_from_end * 900
        return int(quarter), int(time_remaining)

    def _seed_engine(self, eng, gs):
        """Overwrite all N lanes of the engine with the single live state `gs`.

        `gs` keys (offense = possession team perspective):
          score_differential, game_seconds_remaining, down, ydstogo, yardline_100,
          posteam_timeouts_remaining, defteam_timeouts_remaining
        The offense is the engine's away_team (we always instantiate as
        NFLGameEngine(off_team, def_team)), so possession_is_away starts True.
        """
        N = eng.N
        score_diff = float(gs["score_differential"])
        quarter, time_rem = self._clock_to_quarter_time(gs["game_seconds_remaining"])

        eng.possession_is_away[:] = True  # away = offense holds the ball
        eng.yardline_100[:] = int(gs["yardline_100"])
        eng.down[:] = int(gs["down"])
        eng.distance[:] = int(gs["ydstogo"])
        # Raw scores only feed late-game strategic logic; the split is arbitrary as long
        # as (away - home) == score_differential.
        eng.score_away[:] = int(max(0, round(score_diff)))
        eng.score_home[:] = int(max(0, round(-score_diff)))
        eng.quarter[:] = quarter
        eng.time_remaining[:] = time_rem
        eng.timeouts_away[:] = int(gs.get("posteam_timeouts_remaining", 3))
        eng.timeouts_home[:] = int(gs.get("defteam_timeouts_remaining", 3))

        eng.game_over[:] = False
        eng.needs_kickoff[:] = False  # critical: first step must be a scrimmage play
        eng.clock_stopped[:] = False
        eng.play_count[:] = 0

    def _classify_concepts(self, captured, is_pass, is_run, air, is_sack, is_scramble):
        """Return an object array of concept labels per lane (N,)."""
        N = captured.size
        labels = np.full(N, "Unclassified", dtype=object)

        labels[captured & is_run] = "Run"

        # Sacks and scrambles are play OUTCOMES, not design concepts -> excluded buckets.
        pass_clean = captured & is_pass & ~is_sack & ~is_scramble
        labels[pass_clean & (air <= SCREEN_MAX_AIR)] = "Screen"
        labels[pass_clean & (air > SCREEN_MAX_AIR) & (air < SHORT_MAX_AIR)] = "Short"
        labels[pass_clean & (air >= SHORT_MAX_AIR) & (air < MEDIUM_MAX_AIR)] = "Medium"
        labels[pass_clean & (air >= MEDIUM_MAX_AIR)] = "Deep"

        labels[captured & is_pass & is_sack] = "Sack"
        labels[captured & is_pass & is_scramble & ~is_sack] = "Scramble"
        return labels

    def _drive_end_kep(self, score_diff_off, game_sec, terminal, off_to, def_to):
        """Vectorized KEP from each lane's snapshotted drive-ending state (offense perspective).

        Inputs (all length-N arrays captured at the moment each lane's drive ended):
            score_diff_off : offense points - defense points on this drive
            game_sec       : game seconds remaining at drive end
            terminal       : True if the drive ended the game/half-with-no-handoff (score sign decides WP)
        """
        N = score_diff_off.size
        score_diff_off = score_diff_off.astype(np.float64)
        game_sec = game_sec.astype(np.float64)

        # Non-terminal ends: ball goes to the opponent at a fresh 1st-&-10 from their own 25.
        # Evaluate the opponent's WP, then offense WP = 1 - opponent WP.
        opp_state = {
            "score_differential": -score_diff_off,
            "game_seconds_remaining": game_sec,
            "down": np.full(N, 1.0),
            "ydstogo": np.full(N, 10.0),
            "yardline_100": np.full(N, 75.0),
            "posteam_timeouts_remaining": np.full(N, float(def_to)),
            "defteam_timeouts_remaining": np.full(N, float(off_to)),
            "receive_2h_ko": np.zeros(N),
        }
        X = np.column_stack([opp_state[f] for f in self._features]).astype(np.float64)
        wp_opp = self.wp_model.model.predict_proba(X)[:, 1]
        wp_off = 1.0 - wp_opp

        # Terminal lanes (game over or time expired): WP is decided by the score sign.
        terminal = terminal | (game_sec <= 0)
        wp_off = np.where(terminal,
                          np.where(score_diff_off > 0, 1.0,
                                   np.where(score_diff_off < 0, 0.0, 0.5)),
                          wp_off)
        wp_off = np.clip(wp_off, 0.0, 1.0)
        return self.kep.kep_from_wp(wp_off)

    def _drive_end_efsd(self, score_diff_off, game_sec, terminal, off_to, def_to):
        """Vectorized EFSD (expected final score differential, offense perspective) from
        each lane's snapshotted drive-ending state.

        EFSD is already expressed in points (the predicted final margin), so unlike KEP
        there is NO win-probability curve to invert — the model output IS the answer.

        Inputs (all length-N arrays captured at the moment each lane's drive ended):
            score_diff_off : offense points - defense points on this drive
            game_sec       : game seconds remaining at drive end
            terminal       : True if the drive ended the game/half with no handoff

        Non-terminal ends: the ball goes to the opponent at a fresh 1st-&-10 from their
        own 25. We predict the OPPONENT's expected final margin from that state, then flip
        the sign to express it from the offense's perspective: efsd_off = -efsd_opp.
        This is the EFSD analog of the KEP path's wp_off = 1 - wp_opp.

        Terminal ends (game/half over, or clock expired): the final margin is decided —
        it is exactly the current score differential. EFSD returns it directly, with no
        ±24 saturation (EFSD is unbounded, so a 28-point terminal lane reports +28).
        """
        N = score_diff_off.size
        score_diff_off = score_diff_off.astype(np.float64)
        game_sec = game_sec.astype(np.float64)

        # Non-terminal ends: opponent receives at their own 25; predict their final margin.
        opp_state = {
            "score_differential": -score_diff_off,
            "game_seconds_remaining": game_sec,
            "down": np.full(N, 1.0),
            "ydstogo": np.full(N, 10.0),
            "yardline_100": np.full(N, 75.0),
            "posteam_timeouts_remaining": np.full(N, float(def_to)),
            "defteam_timeouts_remaining": np.full(N, float(off_to)),
            "receive_2h_ko": np.zeros(N),
        }
        X = np.column_stack([opp_state[f] for f in self._efsd_features]).astype(np.float64)
        efsd_opp = self.efsd_model.predict_batch(X)
        efsd_off = -efsd_opp

        # Terminal lanes: the final margin is exactly the offense's current score diff.
        terminal = terminal | (game_sec <= 0)
        efsd_off = np.where(terminal, score_diff_off, efsd_off)
        return efsd_off

    # ------------------------------------------------------------------ main API
    def evaluate(self, game_state, off_team, def_team, n_sims=1000, year=2025, engine=None):
        """
        Evaluate the live position and rank first-play concepts by KEP delta.

        Inputs:
            game_state : dict   offense-perspective live state (see _seed_engine for keys).
            off_team, def_team : str   team codes (offense instantiated as away_team).
            n_sims : int        number of drive simulations (lanes).
            year : int          roster/DNA season for engine construction.
            engine : NFLGameEngine | None   optional pre-built engine to re-seed (perf).

        Output:
            dict {
              'kep_start', 'ep' (None for now), 'n_sims',
              'concepts': {concept: {'mean_kep','delta_kep','n'}},
              'excluded': {'Sack','Scramble','Unclassified'},
              'drive_end_rate'  # fraction of lanes whose drive completed within MAX_STEPS
            }
        """
        kep_start = self.kep.kep_from_state(game_state)
        efsd_start = float(self.efsd_model.predict_efsd(game_state))
        off_to = int(game_state.get("posteam_timeouts_remaining", 3))
        def_to = int(game_state.get("defteam_timeouts_remaining", 3))

        eng = engine if engine is not None else NFLGameEngine(off_team, def_team, year=year, N=n_sims)
        if eng.N != n_sims:
            raise ValueError(f"Engine N={eng.N} does not match n_sims={n_sims}")
        self._seed_engine(eng, game_state)

        N = eng.N
        init_poss = eng.possession_is_away.copy()  # all True
        captured = np.zeros(N, dtype=bool)
        first_pass = np.zeros(N, dtype=bool)
        first_run = np.zeros(N, dtype=bool)
        first_air = np.zeros(N, dtype=np.int32)
        first_sack = np.zeros(N, dtype=bool)
        first_scramble = np.zeros(N, dtype=bool)

        # Per-lane drive-end snapshot (filled once, at the step the lane's drive ends).
        end_filled = np.zeros(N, dtype=bool)
        end_score_diff = np.zeros(N, dtype=np.float64)
        end_game_sec = np.zeros(N, dtype=np.float64)
        end_terminal = np.zeros(N, dtype=bool)

        for _ in range(self.MAX_STEPS):
            eng.simulate_play_step()

            # Snapshot the FIRST scrimmage play for any lane that just ran one.
            new_cap = eng.last_play_scrimmage_mask & ~captured
            if np.any(new_cap):
                first_pass[new_cap] = eng.last_play_is_pass[new_cap]
                first_run[new_cap] = eng.last_play_is_run[new_cap]
                first_air[new_cap] = eng.last_play_air_yards[new_cap]
                first_sack[new_cap] = eng.last_play_is_sack[new_cap]
                first_scramble[new_cap] = eng.last_play_is_scramble[new_cap]
                captured |= new_cap

            # A drive ends on change of possession, a score (pending kickoff), or game over.
            now_ended = (eng.possession_is_away != init_poss) | eng.needs_kickoff | eng.game_over
            newly = now_ended & ~end_filled
            if np.any(newly):
                gsec = np.maximum(0, (4 - eng.quarter) * 900 + eng.time_remaining)
                end_score_diff[newly] = (eng.score_away - eng.score_home)[newly]
                end_game_sec[newly] = gsec[newly]
                # Capture terminal-vs-handoff BEFORE freezing (freezing sets game_over).
                end_terminal[newly] = eng.game_over[newly]
                end_filled |= newly
                # Freeze ended lanes so the engine stops simulating subsequent drives in them.
                eng.game_over[newly] = True

            if np.all(end_filled):
                break

        # Genuine fraction of drives that reached a natural end within MAX_STEPS.
        natural_end_rate = float(np.mean(end_filled))

        # Lanes that never ended within MAX_STEPS: snapshot their current state as-is.
        if not np.all(end_filled):
            stuck = ~end_filled
            gsec = np.maximum(0, (4 - eng.quarter) * 900 + eng.time_remaining)
            end_score_diff[stuck] = (eng.score_away - eng.score_home)[stuck]
            end_game_sec[stuck] = gsec[stuck]
            end_terminal[stuck] = eng.game_over[stuck]
            end_filled |= stuck

        labels = self._classify_concepts(captured, first_pass, first_run, first_air,
                                          first_sack, first_scramble)
        kep_lane = self._drive_end_kep(end_score_diff, end_game_sec, end_terminal, off_to, def_to)
        efsd_lane = self._drive_end_efsd(end_score_diff, end_game_sec, end_terminal, off_to, def_to)

        concepts = {}
        for c in CONCEPTS:
            mask = labels == c
            n = int(np.sum(mask))
            if n > 0:
                mean_kep = float(np.mean(kep_lane[mask]))
                mean_efsd = float(np.mean(efsd_lane[mask]))
                concepts[c] = {"mean_kep": mean_kep,
                               "delta_kep": mean_kep - kep_start,
                               "mean_efsd": mean_efsd,
                               "delta_efsd": mean_efsd - efsd_start,
                               "n": n}
            else:
                concepts[c] = {"mean_kep": None, "delta_kep": None,
                               "mean_efsd": None, "delta_efsd": None, "n": 0}

        excluded = {b: int(np.sum(labels == b)) for b in ("Sack", "Scramble", "Unclassified")}

        goal_to_go = 1 if int(game_state["ydstogo"]) >= int(game_state["yardline_100"]) else 0
        ep_start = self.ep_model.predict_expected_points(
            yardline_100=int(game_state["yardline_100"]),
            down=int(game_state["down"]),
            ydstogo=int(game_state["ydstogo"]),
            goal_to_go=goal_to_go,
        )

        return {
            "kep_start": kep_start,
            "efsd_start": efsd_start,
            "ep_start": ep_start,
            "n_sims": int(N),
            "drive_end_rate": natural_end_rate,
            "concepts": concepts,
            "excluded": excluded,
        }


    # ------------------------------------------------------------------ one-step API
    def evaluate_one_step(self, game_state, off_team, def_team, n_sims=200, year=2025, engine=None):
        """
        One-play lookahead: simulate exactly one scrimmage play per lane, then compute
        KEP of the resulting game state per tactical concept.

        Unlike evaluate() (full drive), this answers: "what does THIS specific play call
        do to our position right now?" — the concept delta reflects the immediate outcome
        of one play, not the entire drive that follows.

        Inputs: same as evaluate().
        Output:
            dict {
              'kep_start', 'n_sims',
              'concepts': {concept: {'mean_kep','delta_kep','n','mean_next_state'}},
              'excluded': {...}
            }
          'mean_next_state' is the average surviving game state (possession kept, no score)
          for lanes that used this concept — used for chaining lines.
        """
        kep_start = self.kep.kep_from_state(game_state)
        efsd_start = float(self.efsd_model.predict_efsd(game_state))
        off_to = int(game_state.get("posteam_timeouts_remaining", 3))
        def_to = int(game_state.get("defteam_timeouts_remaining", 3))

        eng = engine if engine is not None else NFLGameEngine(off_team, def_team, year=year, N=n_sims)
        if eng.N != n_sims:
            raise ValueError(f"Engine N={eng.N} does not match n_sims={n_sims}")
        self._seed_engine(eng, game_state)

        N = eng.N
        init_poss = eng.possession_is_away.copy()

        # Run until each lane has executed exactly one scrimmage play.
        captured = np.zeros(N, dtype=bool)
        first_pass = np.zeros(N, dtype=bool)
        first_run = np.zeros(N, dtype=bool)
        first_air = np.zeros(N, dtype=np.int32)
        first_sack = np.zeros(N, dtype=bool)
        first_scramble = np.zeros(N, dtype=bool)

        for _ in range(self.MAX_STEPS):
            eng.simulate_play_step()
            new_cap = eng.last_play_scrimmage_mask & ~captured
            if np.any(new_cap):
                first_pass[new_cap] = eng.last_play_is_pass[new_cap]
                first_run[new_cap] = eng.last_play_is_run[new_cap]
                first_air[new_cap] = eng.last_play_air_yards[new_cap]
                first_sack[new_cap] = eng.last_play_is_sack[new_cap]
                first_scramble[new_cap] = eng.last_play_is_scramble[new_cap]
                captured |= new_cap
            if np.all(captured):
                break

        labels = self._classify_concepts(captured, first_pass, first_run, first_air,
                                          first_sack, first_scramble)

        # Read resulting state for each lane immediately after that one play.
        score_diff_off = (eng.score_away - eng.score_home).astype(np.float64)
        gsec = np.maximum(0, (4 - eng.quarter) * 900 + eng.time_remaining).astype(np.float64)

        poss_kept = (eng.possession_is_away == init_poss)
        scored     = eng.needs_kickoff
        terminal   = eng.game_over
        turnover   = ~poss_kept & ~scored & ~terminal
        mid_drive  = poss_kept & ~scored & ~terminal

        kep_lane = np.zeros(N, dtype=np.float64)
        efsd_lane = np.zeros(N, dtype=np.float64)

        # Mid-drive: direct WP -> KEP and direct EFSD from the new (offense-perspective) state.
        if np.any(mid_drive):
            state_mid = {
                "score_differential":           score_diff_off[mid_drive],
                "game_seconds_remaining":       gsec[mid_drive],
                "down":                         eng.down[mid_drive].astype(np.float64),
                "ydstogo":                      eng.distance[mid_drive].astype(np.float64),
                "yardline_100":                 eng.yardline_100[mid_drive].astype(np.float64),
                "posteam_timeouts_remaining":   eng.timeouts_away[mid_drive].astype(np.float64),
                "defteam_timeouts_remaining":   eng.timeouts_home[mid_drive].astype(np.float64),
                "receive_2h_ko":                np.zeros(int(np.sum(mid_drive))),
            }
            X = np.column_stack([state_mid[f] for f in self._features])
            wp = self.wp_model.model.predict_proba(X)[:, 1]
            kep_lane[mid_drive] = self.kep.kep_from_wp(wp)
            # EFSD is offense-perspective directly (no sign flip mid-drive).
            X_efsd = np.column_stack([state_mid[f] for f in self._efsd_features])
            efsd_lane[mid_drive] = self.efsd_model.predict_batch(X_efsd)

        # Scored or terminal: use drive-end KEP/EFSD (opponent gets ball at own 25 or game ends).
        end_mask = scored | terminal
        if np.any(end_mask):
            kep_lane[end_mask] = self._drive_end_kep(
                score_diff_off[end_mask], gsec[end_mask], terminal[end_mask], off_to, def_to
            )
            efsd_lane[end_mask] = self._drive_end_efsd(
                score_diff_off[end_mask], gsec[end_mask], terminal[end_mask], off_to, def_to
            )

        # Turnover: opponent has ball at exact turnover field position (more accurate than own 25).
        if np.any(turnover):
            opp_yd = (100 - eng.yardline_100[turnover]).astype(np.float64)
            opp_yd = np.clip(opp_yd, 1, 99)
            state_turn = {
                "score_differential":           -score_diff_off[turnover],
                "game_seconds_remaining":       gsec[turnover],
                "down":                         np.ones(int(np.sum(turnover))),
                "ydstogo":                      np.full(int(np.sum(turnover)), 10.0),
                "yardline_100":                 opp_yd,
                "posteam_timeouts_remaining":   np.full(int(np.sum(turnover)), float(def_to)),
                "defteam_timeouts_remaining":   np.full(int(np.sum(turnover)), float(off_to)),
                "receive_2h_ko":                np.zeros(int(np.sum(turnover))),
            }
            X_turn = np.column_stack([state_turn[f] for f in self._features])
            wp_opp = self.wp_model.model.predict_proba(X_turn)[:, 1]
            kep_lane[turnover] = self.kep.kep_from_wp(1.0 - wp_opp)
            # EFSD: predict opponent's final margin from the turnover spot, then flip sign.
            X_turn_efsd = np.column_stack([state_turn[f] for f in self._efsd_features])
            efsd_lane[turnover] = -self.efsd_model.predict_batch(X_turn_efsd)

        # Aggregate by concept.
        concepts = {}
        for c in CONCEPTS:
            mask = labels == c
            n = int(np.sum(mask))
            if n > 0:
                mean_kep = float(np.mean(kep_lane[mask]))
                mean_efsd = float(np.mean(efsd_lane[mask]))
                # Mean surviving state for line-chaining (only mid-drive lanes).
                surviving = mask & mid_drive
                mean_next_state = None
                if np.any(surviving):
                    mean_next_state = {
                        "score_differential":         float(np.mean(score_diff_off[surviving])),
                        "game_seconds_remaining":     float(np.mean(gsec[surviving])),
                        "down":                       float(np.round(np.mean(eng.down[surviving]))),
                        "ydstogo":                    float(np.mean(eng.distance[surviving])),
                        "yardline_100":               float(np.mean(eng.yardline_100[surviving])),
                        "posteam_timeouts_remaining": float(off_to),
                        "defteam_timeouts_remaining": float(def_to),
                    }
                concepts[c] = {
                    "mean_kep":        mean_kep,
                    "delta_kep":       mean_kep - kep_start,
                    "mean_efsd":       mean_efsd,
                    "delta_efsd":      mean_efsd - efsd_start,
                    "n":               n,
                    "mean_next_state": mean_next_state,
                }
            else:
                concepts[c] = {"mean_kep": None, "delta_kep": None,
                               "mean_efsd": None, "delta_efsd": None,
                               "n": 0, "mean_next_state": None}

        excluded = {b: int(np.sum(labels == b)) for b in ("Sack", "Scramble", "Unclassified")}
        return {"kep_start": kep_start, "efsd_start": efsd_start,
                "n_sims": N, "concepts": concepts, "excluded": excluded}

    def suggest_lines(self, game_state, off_team, def_team,
                      n_sims=150, depth=2, n_lines=3, year=2025, metric="kep"):
        """
        Returns n_lines suggested play sequences (principal variations) of given depth.

        Analogous to chess engine 'lines': shows the best first play, then the best
        follow-up play given the resulting average game state, etc.

        Inputs:
            game_state : offense-perspective state dict (see _seed_engine).
            depth      : number of plays per line (2 or 3 recommended).
            n_lines    : number of distinct lines to return.
            n_sims     : sims per one-step evaluation (lower is fine here — this is
                         chained 2-3 times so total compute scales with depth).
            metric     : "kep" (Kickoff-Equivalent Points) or "efsd" (Expected Final
                         Score Differential). Selects the value axis used to rank
                         concepts and chain the principal variation.

        Output (keys are metric-specific, e.g. kep_* or efsd_*):
            dict {
              '<metric>_start': float,
              'metric': str,
              'lines': [
                {'concepts': ['Short','Run'], '<metric>_steps': [...],
                 '<metric>_final': float, 'delta_<metric>': float, 'n': int},
                ...
              ]
            }
        """
        metric = metric.lower()
        if metric not in ("kep", "efsd"):
            raise ValueError(f"Unsupported metric '{metric}' (expected 'kep' or 'efsd').")
        mean_key  = f"mean_{metric}"
        delta_key = f"delta_{metric}"

        if metric == "kep":
            start_val = self.kep.kep_from_state(game_state)
        else:
            start_val = float(self.efsd_model.predict_efsd(game_state))

        step1 = self.evaluate_one_step(game_state, off_team, def_team, n_sims, year)

        # Sort concepts by the selected metric's delta (descending), take top n_lines.
        ranked = sorted(
            [(c, d) for c, d in step1["concepts"].items()
             if d["n"] > 0 and d[delta_key] is not None],
            key=lambda x: x[1][delta_key],
            reverse=True,
        )
        lines = []
        for c1, d1 in ranked[:n_lines]:
            line_concepts = [c1]
            line_steps    = [round(d1[mean_key], 3)]

            current_state = d1.get("mean_next_state")
            for _ in range(depth - 1):
                if current_state is None:
                    break
                step_n = self.evaluate_one_step(current_state, off_team, def_team, n_sims, year)
                next_ranked = sorted(
                    [(c, d) for c, d in step_n["concepts"].items()
                     if d["n"] > 0 and d[delta_key] is not None],
                    key=lambda x: x[1][delta_key],
                    reverse=True,
                )
                if not next_ranked:
                    break
                c_next, d_next = next_ranked[0]
                line_concepts.append(c_next)
                line_steps.append(round(d_next[mean_key], 3))
                current_state = d_next.get("mean_next_state")

            lines.append({
                "concepts":          line_concepts,
                f"{metric}_steps":   line_steps,
                f"{metric}_final":   line_steps[-1],
                delta_key:           round(line_steps[-1] - start_val, 3),
                "n":                 d1["n"],
            })

        return {f"{metric}_start": round(start_val, 3), "n_sims": n_sims,
                "metric": metric, "lines": lines}


if __name__ == "__main__":
    # ------------------------------------------------------------------ sanity run
    print("=" * 64)
    print("Phase 1 sanity: KEPConverter + drive-rollout harness")
    print("=" * 64)

    wp = WinProbabilityModelV010()
    kep = KEPConverter(wp)

    # 1) KEP axis should be monotonic and pass through ~0 at a tied kickoff.
    print("\n[KEP axis checks]")
    for sd in (-21, -10, -3, 0, 3, 10, 21):
        state = dict(KICKOFF_REFERENCE, score_differential=float(sd))
        wp_val = wp.predict_win_probability(state)
        back = kep.kep_from_wp(wp_val)
        print(f"  score_diff={sd:+3d}  ->  WP={wp_val:5.3f}  ->  KEP={back:+6.2f}")
    # The KEP axis itself (the denoised, inverted curve) must be strictly monotonic.
    axis_kep = kep.kep_from_wp(kep.wp_curve_mono)
    print(f"  KEP axis strictly monotonic (by construction): "
          f"{bool(np.all(np.diff(axis_kep) > 0))}")

    # 2) Full evaluation from a midfield 1st-and-10, tied, early 2nd quarter.
    print("\n[Drive-rollout evaluation]  KC offense vs BUF, 1st & 10 at own 25, tied, ~Q2")
    ev = PositionalEvaluator(wp_model=wp)
    gs = {
        "score_differential": 0.0,
        "game_seconds_remaining": 2400.0,
        "down": 1,
        "ydstogo": 10,
        "yardline_100": 75,
        "posteam_timeouts_remaining": 3,
        "defteam_timeouts_remaining": 3,
    }
    import time as _t
    t0 = _t.time()
    res = ev.evaluate(gs, off_team="KC", def_team="BUF", n_sims=1000)
    dt = _t.time() - t0

    print(f"  kep_start = {res['kep_start']:+.3f}   efsd_start = {res['efsd_start']:+.3f}"
          f"   ep_start = {res['ep_start']:+.3f}"
          f"   drive_end_rate = {res['drive_end_rate']:.3f}"
          f"   ({dt:.2f}s for {res['n_sims']} sims)")
    print(f"  {'concept':<8} {'n':>5} {'mean_kep':>10} {'delta_kep':>10} "
          f"{'mean_efsd':>10} {'delta_efsd':>11}")
    for c in CONCEPTS:
        d = res["concepts"][c]
        if d["n"] > 0:
            print(f"  {c:<8} {d['n']:>5} {d['mean_kep']:>10.3f} {d['delta_kep']:>+10.3f} "
                  f"{d['mean_efsd']:>10.3f} {d['delta_efsd']:>+11.3f}")
        else:
            print(f"  {c:<8} {d['n']:>5} {'--':>10} {'--':>10} {'--':>10} {'--':>11}")
    print(f"  excluded: {res['excluded']}")

    # 3) suggest_lines on both metrics — confirm EFSD ranking path runs.
    print("\n[suggest_lines]  metric=kep vs metric=efsd")
    for m in ("kep", "efsd"):
        sl = ev.suggest_lines(gs, off_team="KC", def_team="BUF",
                              n_sims=150, depth=2, n_lines=3, metric=m)
        print(f"  metric={sl['metric']}  {m}_start={sl[f'{m}_start']:+.3f}")
        for ln in sl["lines"]:
            print(f"    {' -> '.join(ln['concepts']):<22} "
                  f"{m}_final={ln[f'{m}_final']:+7.3f}  delta_{m}={ln[f'delta_{m}']:+7.3f}  n={ln['n']}")
