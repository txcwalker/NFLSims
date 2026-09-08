# src/live/decision_engine.py
# Python port of R/simulators/fourth_down/fourth_down_decision.R.
# Computes Go/Punt/FG expected win probability for a 4th-down game state,
# using the same three trained models the rest of the Python sim stack uses
# (src/nfl_sim/models/{win_probability_v_0_1_0,fg_v_0_1_0,fourth_down_conversion_v_0_1_0}),
# replacing the old Rscript subprocess bridge (simulator_bridge.py) in the live-bot hot path.
# References: R/simulators/fourth_down/fourth_down_decision.R, src/live/simulator_bridge.py
# ------------------------------------------------------------------------------

import os
import sys
from typing import Any, Dict, Optional

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")))

from src.nfl_sim.models.win_probability_v_0_1_0.inference import WinProbabilityModelV010
from src.nfl_sim.models.fg_v_0_1_0.inference import FieldGoalModelV010
from src.nfl_sim.models.fourth_down_conversion_v_0_1_0.inference import FourthDownConversionModelV010

# Models are lightweight (joblib/xgboost artifacts, no roster/DNA context needed)
# so instantiating once at module load is safe and avoids the ModelRegistry
# singleton-staleness footgun documented in AGENTS.md.
_wp_model = WinProbabilityModelV010()
_fg_model = FieldGoalModelV010()
_fd_model = FourthDownConversionModelV010()

# Same configurable thresholds as .FD_ENV in fourth_down_decision.R
NO_PUNT_INSIDE = float(os.getenv("NO_PUNT_INSIDE", "35"))
_no_punt_kdist_env = os.getenv("NO_PUNT_KDIST", "")
NO_PUNT_KDIST = float(_no_punt_kdist_env) if _no_punt_kdist_env else None
NO_FG_MAXDIST = float(os.getenv("NO_FG_MAXDIST", "50"))


def _base_state(game_state: Dict[str, Any]) -> Dict[str, float]:
    """Normalizes a raw game_state dict to the fields every model call needs."""
    return {
        "down": int(game_state.get("down") or 4),
        "yardline_100": float(game_state.get("yardline_100")),
        "ydstogo": float(game_state.get("ydstogo")),
        "game_seconds_remaining": float(game_state.get("game_seconds_remaining") or 0),
        "score_differential": float(game_state.get("score_differential") or 0),
        "posteam_timeouts_remaining": int(game_state.get("posteam_timeouts_remaining") or 3),
        "defteam_timeouts_remaining": int(game_state.get("defteam_timeouts_remaining") or 3),
        "receive_2h_ko": float(game_state.get("receive_2h_ko") or 0.0),
    }


def _flip(state: Dict[str, float]) -> Dict[str, float]:
    """Possession-change state transform: negate score diff, swap timeouts."""
    flipped = dict(state)
    flipped["score_differential"] = -state["score_differential"]
    flipped["posteam_timeouts_remaining"] = state["defteam_timeouts_remaining"]
    flipped["defteam_timeouts_remaining"] = state["posteam_timeouts_remaining"]
    return flipped


def predict_win_probability(game_state: Dict[str, Any]) -> float:
    """Thin public passthrough to the shared WP model instance (posteam perspective)."""
    return float(_wp_model.predict_win_probability(_base_state(game_state)))


def evaluate_fourth_down(game_state: Dict[str, Any]) -> Dict[str, Any]:
    """
    Computes Go/Punt/FG expected win probability (posteam perspective) for a
    single 4th-down game state. Mirrors simulate_fourth_down_decision()'s
    output shape from fourth_down_decision.R so callers (main.py) don't change.

    Inputs (game_state dict): down, yardline_100, ydstogo, game_seconds_remaining,
        score_differential, posteam_timeouts_remaining, defteam_timeouts_remaining.
    Outputs: dict with base_wp, field_goal_ev/make_wp/miss_wp, go_for_it_ev/success_wp/
        failure_wp, punt_wp, punt_suppressed, fg_suppressed, fg_prob, fd_prob,
        delta_fg/go/punt, recommendation {action, wp} (action is lowercase
        "go"/"fg"/"punt" to match espn_adapter.infer_called_action()'s convention).
    """
    base = _base_state(game_state)
    yardline_100 = base["yardline_100"]
    ydstogo = base["ydstogo"]

    base_wp = _wp_model.predict_win_probability(base)
    kick_distance = yardline_100 + 17.0

    # NO-PUNT ZONE
    suppress_punt = (yardline_100 <= NO_PUNT_INSIDE) or (
        NO_PUNT_KDIST is not None and kick_distance <= NO_PUNT_KDIST
    )

    # ------------------------ Field Goal ------------------------
    fg_suppressed = kick_distance > NO_FG_MAXDIST
    fg_prob = _fg_model.predict_success_probability(yardline_100)
    fg_ev = fg_make_wp = fg_miss_wp = None
    if not fg_suppressed:
        fg_make_state = dict(base)
        fg_make_state["score_differential"] = -(base["score_differential"] + 3)
        fg_make_state["yardline_100"] = 70.0
        fg_make_state["down"] = 1
        fg_make_state["ydstogo"] = 10.0
        fg_make_state["posteam_timeouts_remaining"] = base["defteam_timeouts_remaining"]
        fg_make_state["defteam_timeouts_remaining"] = base["posteam_timeouts_remaining"]
        fg_make_state["game_seconds_remaining"] = base["game_seconds_remaining"] - 5
        fg_make_wp = 1.0 - _wp_model.predict_win_probability(fg_make_state)

        fg_miss_state = _flip(base)
        fg_miss_state["yardline_100"] = 100.0 - yardline_100
        fg_miss_state["down"] = 1
        fg_miss_state["ydstogo"] = 10.0
        fg_miss_state["game_seconds_remaining"] = base["game_seconds_remaining"] - 5
        fg_miss_wp = 1.0 - _wp_model.predict_win_probability(fg_miss_state)

        fg_ev = fg_prob * fg_make_wp + (1 - fg_prob) * fg_miss_wp

    # ------------------------ Go For It ------------------------
    fd_prob = _fd_model.predict_conversion_probability(base)
    go_ev = go_success_wp = go_fail_wp = None
    if yardline_100 != ydstogo:
        go_success_state = dict(base)
        go_success_state["yardline_100"] = yardline_100 - ydstogo - 1
        go_success_state["down"] = 1
        go_success_state["ydstogo"] = 10.0
        go_success_state["game_seconds_remaining"] = base["game_seconds_remaining"] - 8
        go_success_wp = _wp_model.predict_win_probability(go_success_state)

        go_fail_state = _flip(base)
        go_fail_state["yardline_100"] = 100.0 - yardline_100
        go_fail_state["down"] = 1
        go_fail_state["ydstogo"] = 10.0
        go_fail_state["game_seconds_remaining"] = base["game_seconds_remaining"] - 8
        go_fail_wp = 1.0 - _wp_model.predict_win_probability(go_fail_state)

        go_ev = fd_prob * go_success_wp + (1 - fd_prob) * go_fail_wp
    else:
        # Goal-to-go TD attempt
        go_success_state = dict(base)
        go_success_state["score_differential"] = -(base["score_differential"] + 7)
        go_success_state["yardline_100"] = 75.0
        go_success_state["down"] = 1
        go_success_state["ydstogo"] = 10.0
        go_success_state["posteam_timeouts_remaining"] = base["defteam_timeouts_remaining"]
        go_success_state["defteam_timeouts_remaining"] = base["posteam_timeouts_remaining"]
        go_success_state["game_seconds_remaining"] = base["game_seconds_remaining"] - 6
        go_success_wp = 1.0 - _wp_model.predict_win_probability(go_success_state)

        go_fail_state = _flip(base)
        go_fail_state["yardline_100"] = 100.0 - yardline_100
        go_fail_state["down"] = 1
        go_fail_state["ydstogo"] = 10.0
        go_fail_state["game_seconds_remaining"] = base["game_seconds_remaining"] - 6
        go_fail_wp = 1.0 - _wp_model.predict_win_probability(go_fail_state)

        go_ev = fd_prob * go_success_wp + (1 - fd_prob) * go_fail_wp

    # ------------------------ Punt ------------------------
    punt_wp: Optional[float] = None
    if not suppress_punt:
        punt_state = _flip(base)
        punt_state["yardline_100"] = min(85.0, 100.0 + (45.0 - yardline_100))
        punt_state["down"] = 1
        punt_state["ydstogo"] = 10.0
        punt_state["game_seconds_remaining"] = base["game_seconds_remaining"] - 10
        punt_wp = 1.0 - _wp_model.predict_win_probability(punt_state)

    # ------------------------ Recommendation ------------------------
    candidates = {}
    if fg_ev is not None:
        candidates["fg"] = fg_ev
    if go_ev is not None:
        candidates["go"] = go_ev
    if punt_wp is not None:
        candidates["punt"] = punt_wp

    if candidates:
        best_action = max(candidates, key=candidates.get)
        best_wp = candidates[best_action]
    else:
        best_action = "none"
        best_wp = base_wp

    def _f(x):
        # FieldGoalModelV010's scalar path returns np.float64 (via np.exp on a
        # plain float) -- normalize every numeric output to plain float so
        # downstream sqlite3/json serialization (state_store, posting_policy)
        # never trips on an unsupported numpy scalar type.
        return None if x is None else float(x)

    return {
        "base_wp": _f(base_wp),
        "field_goal_ev": _f(fg_ev),
        "field_goal_make_wp": _f(fg_make_wp),
        "field_goal_miss_wp": _f(fg_miss_wp),
        "go_for_it_ev": _f(go_ev),
        "go_for_it_success_wp": _f(go_success_wp),
        "go_for_it_failure_wp": _f(go_fail_wp),
        "punt_wp": _f(punt_wp),
        "punt_suppressed": suppress_punt,
        "fg_suppressed": fg_suppressed,
        "fg_prob": _f(fg_prob),
        "fd_prob": _f(fd_prob),
        "delta_fg": _f(fg_ev - base_wp) if fg_ev is not None else None,
        "delta_go": _f(go_ev - base_wp) if go_ev is not None else None,
        "delta_punt": _f(punt_wp - base_wp) if punt_wp is not None else None,
        "recommendation": {"action": best_action, "wp": _f(best_wp)},
    }


if __name__ == "__main__":
    print("=" * 60)
    print("Testing decision_engine.evaluate_fourth_down()")
    print("=" * 60)

    scenarios = [
        {
            "desc": "4th & 2 at Opp 45, Q3 20:00 remaining, down 2 (matches yesterday's R test)",
            "state": {
                "down": 4, "yardline_100": 45, "ydstogo": 4,
                "game_seconds_remaining": 1200, "score_differential": -2,
                "posteam_timeouts_remaining": 2, "defteam_timeouts_remaining": 2,
            },
        },
        {
            "desc": "4th & Goal from the 2, tied, 2:00 left in Q4",
            "state": {
                "down": 4, "yardline_100": 2, "ydstogo": 2,
                "game_seconds_remaining": 120, "score_differential": 0,
                "posteam_timeouts_remaining": 1, "defteam_timeouts_remaining": 1,
            },
        },
        {
            "desc": "4th & 12 on own 30 (obvious punt), up 10, Q1",
            "state": {
                "down": 4, "yardline_100": 70, "ydstogo": 12,
                "game_seconds_remaining": 2700, "score_differential": 10,
                "posteam_timeouts_remaining": 3, "defteam_timeouts_remaining": 3,
            },
        },
    ]

    for tc in scenarios:
        res = evaluate_fourth_down(tc["state"])
        print(f"\n{tc['desc']}")
        print(f"  base_wp={res['base_wp']:.3f}")
        print(f"  GO ev={res['go_for_it_ev']}, PUNT wp={res['punt_wp']}, FG ev={res['field_goal_ev']}")
        print(f"  recommendation={res['recommendation']}")
