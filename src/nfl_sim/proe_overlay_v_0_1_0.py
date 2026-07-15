"""
Play Type Selection — PROE Overlay Module
Version: V.0.1.0
Last Updated: May 2026

Purpose:
    Applies a coach Pass Rate Over Expected (PROE) adjustment to the base
    XGBoost play type model output. The adjustment is made in logit space
    to preserve mathematical consistency across all probability levels.

Design Decisions (documented in docs/models/play_selection_v_0_1_0.md Section 5.3):
    - Uses HC-level PROE from coordinator_atlas.json (off_proe field).
      The atlas tracks the effective play-calling identity regardless of
      whether the coach holds an HC or OC title.
    - Blending uses Bayesian shrinkage with k=8:
        proe_blended = (n_curr / (n_curr + k)) * proe_curr
                     + (k     / (n_curr + k)) * proe_prior
      At Week 0: 100% prior. By Week 8: ~50/50. By Week 16: ~67% current.
    - Applied in logit space:
        logit_base     = log(p / (1 - p))
        logit_adjusted = logit_base + proe_logit_offset
        p_final        = sigmoid(logit_adjusted)
    - PROE values in the atlas are in percentage points (e.g., +5.0 means
      the coach passes 5pp more than expected given game state). They are
      converted to a logit offset before application.

Data Dependencies:
    - data/coordinator_atlas.json    — historical PROE by coach name
    - data/team_to_coach_2025.json   — team abbreviation → coach name mapping
"""

import json
import math
import os
from typing import Dict, Optional

# ── Load reference data at import time (read-once) ───────────────────────────
_BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
_ATLAS_PATH = os.path.join(_BASE_DIR, "data", "dna", "coordinator_atlas.json")
_TEAM_MAP_PATH = os.path.join(_BASE_DIR, "data", "dna", "team_to_coach_2025.json")

with open(_ATLAS_PATH, "r") as f:
    _ATLAS = json.load(f)

with open(_TEAM_MAP_PATH, "r") as f:
    _TEAM_TO_COACH = json.load(f)

_HISTORICAL_PROE: Dict[str, float] = _ATLAS.get("off_proe", {})

# Bayesian shrinkage parameter.
# k=8 means the prior year is worth the equivalent of 8 current-season games.
# At n_current=8, weights are 50/50. Tunable for V.0.2.0.
_K = 8

# League-average PROE (by definition = 0.0 — any coach with no data
# gets treated as perfectly average).
_LEAGUE_AVG_PROE = 0.0


import numpy as np

# ── Core math helpers ─────────────────────────────────────────────────────────

def _logit(p) -> float:
    """Convert a probability to logit (log-odds) space."""
    p = np.clip(p, 1e-6, 1.0 - 1e-6)
    return np.log(p / (1.0 - p))


def _sigmoid(x) -> float:
    """Convert a logit value back to a probability."""
    return 1.0 / (1.0 + np.exp(-x))


def _proe_to_logit_offset(proe_pct: float, base_pass_rate: float = 0.57) -> float:
    """
    Convert a PROE value in percentage points to a logit-space offset.

    PROE is defined as: actual_pass_rate - expected_pass_rate (given game state).
    We convert by computing what logit shift produces the same delta around the
    league-average pass rate. This is an approximation that works well near the
    center of the probability distribution and degrades gracefully at the extremes
    (which is exactly where the logit application also moderates the adjustment).

    Args:
        proe_pct:       PROE in percentage points (e.g., +5.0, -3.2)
        base_pass_rate: League-average pass rate anchor (default 0.57 from EDA)

    Returns:
        Logit-space offset (additive, dimensionless)
    """
    adjusted_rate = max(0.01, min(0.99, base_pass_rate + proe_pct / 100.0))
    return _logit(adjusted_rate) - _logit(base_pass_rate)


# ── Blending ──────────────────────────────────────────────────────────────────

def blend_proe(
    historical_proe: float,
    current_proe: float,
    n_current_games: int,
    k: int = _K
) -> float:
    """
    Bayesian shrinkage blend of historical and current-season PROE.

    Formula:
        proe_blended = (n / (n + k)) * current + (k / (n + k)) * historical

    Args:
        historical_proe:  Prior-year PROE in percentage points.
        current_proe:     Current-season PROE in percentage points.
        n_current_games:  Number of current-season games played so far.
        k:                Shrinkage constant (default 8).

    Returns:
        Blended PROE in percentage points.
    """
    if n_current_games <= 0:
        return historical_proe
    w_curr = n_current_games / (n_current_games + k)
    w_hist = k / (n_current_games + k)
    return w_curr * current_proe + w_hist * historical_proe


# ── Public API ────────────────────────────────────────────────────────────────

def get_coach_proe(
    team: str,
    n_current_games: int = 0,
    current_season_proe: Optional[float] = None
) -> float:
    """
    Return the blended PROE for a team's offensive play caller.

    Args:
        team:                NFL team abbreviation (e.g., "KC", "SF").
        n_current_games:     Games played in the current season (0 = pre-season).
        current_season_proe: Current-season PROE if available (percentage points).
                             If None and n_current_games > 0, falls back to historical.

    Returns:
        Blended PROE in percentage points. Returns 0.0 (league average) if
        the team or coach is not found.
    """
    coach = _TEAM_TO_COACH.get(team)
    if coach is None:
        return _LEAGUE_AVG_PROE

    historical = _HISTORICAL_PROE.get(coach, _LEAGUE_AVG_PROE)

    if n_current_games <= 0 or current_season_proe is None:
        return historical

    return blend_proe(historical, current_season_proe, n_current_games)


def apply_proe_overlay(
    base_prob: float,
    team: str,
    n_current_games: int = 0,
    current_season_proe: Optional[float] = None,
    base_pass_rate: float = 0.57
) -> float:
    """
    Apply the coach PROE adjustment to a base pass probability in logit space.

    This is the primary function called by the simulation engine.

    Args:
        base_prob:           Raw pass probability from the XGBoost submodel (0–1).
        team:                NFL team abbreviation.
        n_current_games:     Current-season games played.
        current_season_proe: Current-season PROE in percentage points (optional).
        base_pass_rate:      League-average pass rate anchor for logit conversion.

    Returns:
        Adjusted pass probability (0–1), clipped to [0.01, 0.99].

    Example:
        >>> p = apply_proe_overlay(0.55, team="KC", n_current_games=0)
        # Andy Reid PROE ≈ +6.4pp → logit offset applied → slightly higher than 0.55
    """
    proe = get_coach_proe(team, n_current_games, current_season_proe)
    if abs(proe) < 0.01:
        return base_prob  # No adjustment for league-average coaches

    logit_offset = _proe_to_logit_offset(proe, base_pass_rate)
    adjusted = _sigmoid(_logit(base_prob) + logit_offset)
    return np.clip(adjusted, 0.01, 0.99)


# ── Diagnostics (not called during simulation) ────────────────────────────────

def print_overlay_table(teams: Optional[list] = None) -> None:
    """
    Print a diagnostic table showing PROE and logit offset for each team.
    Useful for validating the overlay before integration.
    """
    if teams is None:
        teams = sorted(_TEAM_TO_COACH.keys())

    print("\n{:<6} {:<22} {:>10} {:>13} {:>8} {:>8}".format(
        "Team", "Coach", "Hist PROE", "Logit Offset", "55%->", "45%->"))
    print("-" * 72)
    for team in teams:
        coach = _TEAM_TO_COACH.get(team, "Unknown")
        proe  = _HISTORICAL_PROE.get(coach, 0.0)
        offset = _proe_to_logit_offset(proe)
        adj55 = apply_proe_overlay(0.55, team)
        adj45 = apply_proe_overlay(0.45, team)
        print(f"{team:<6} {coach:<22} {proe:>+10.2f} {offset:>+13.4f} {adj55:>7.1%} {adj45:>7.1%}")


if __name__ == "__main__":
    print("=== PROE Overlay Diagnostic ===")
    print_overlay_table()
