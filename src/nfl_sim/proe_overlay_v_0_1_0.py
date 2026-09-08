"""
Play Type Selection — PROE Overlay Module
Version: V.0.1.0
Last Updated: May 2026

Purpose:
    Applies a coach Pass Rate Over Expected (PROE) adjustment to the base
    XGBoost play type model output. The adjustment is made in logit space
    to preserve mathematical consistency across all probability levels.

Design Decisions (documented in docs/models/play_selection_v_0_1_0.md Section 5.3):
    - Uses HC-level PROE from coach_dna.json's "proe" field (merged in from the
      retired coordinator_atlas.json on 2026-07-16 — see AGENTS.md). The join
      key is always team_to_coach_2025.json's listed name, same as every other
      coach_dna.json-backed feature (e.g. air-yards' coach traits).
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
    - data/dna/coach_dna.json          — historical PROE (field "proe") by coach name
    - data/dna/team_to_coach_{year}.json — team abbreviation → coach name mapping,
      year-parameterized (Phase 7 fix, 2026-07-22 -- this used to be a single
      import-time load of team_to_coach_2025.json regardless of which year a
      sim was run for, same bug class as game_engine.py's original Phase 0 fix)
"""

import json
import math
import os
from typing import Dict, Optional

# ── Load reference data at import time (read-once) ───────────────────────────
_BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
_COACH_DNA_PATH = os.path.join(_BASE_DIR, "data", "dna", "coach_dna.json")

with open(_COACH_DNA_PATH, "r") as f:
    _COACH_DNA = json.load(f)

_HISTORICAL_PROE: Dict[str, float] = {
    name: entry["proe"] for name, entry in _COACH_DNA.items()
    if name != "_metadata" and "proe" in entry
}

# team_to_coach_{year}.json is small and genuinely year-specific (unlike
# coach_dna.json above, a multi-season career atlas) -- lazily loaded and
# cached per year instead of at import time, since the year isn't known
# until a caller actually asks.
_TEAM_TO_COACH_CACHE: Dict[int, Dict[str, str]] = {}


def _load_team_to_coach(year: int) -> Dict[str, str]:
    if year not in _TEAM_TO_COACH_CACHE:
        path = os.path.join(_BASE_DIR, "data", "dna", f"team_to_coach_{year}.json")
        if os.path.exists(path):
            with open(path, "r") as f:
                _TEAM_TO_COACH_CACHE[year] = json.load(f)
        else:
            _TEAM_TO_COACH_CACHE[year] = {}
    return _TEAM_TO_COACH_CACHE[year]

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
    year: int = 2025,
    n_current_games: int = 0,
    current_season_proe: Optional[float] = None
) -> float:
    """
    Return the blended PROE for a team's offensive play caller.

    Args:
        team:                NFL team abbreviation (e.g., "KC", "SF").
        year:                Season year, selects which team_to_coach_{year}.json
                             to join against (Phase 7 fix -- used to be
                             hardcoded to 2025 regardless of this value).
        n_current_games:     Games played in the current season (0 = pre-season).
        current_season_proe: Current-season PROE if available (percentage points).
                             If None and n_current_games > 0, falls back to historical.

    Returns:
        Blended PROE in percentage points. Returns 0.0 (league average) if
        the team or coach is not found.
    """
    coach = _load_team_to_coach(year).get(team)
    if coach is None:
        return _LEAGUE_AVG_PROE

    historical = _HISTORICAL_PROE.get(coach, _LEAGUE_AVG_PROE)

    if n_current_games <= 0 or current_season_proe is None:
        return historical

    return blend_proe(historical, current_season_proe, n_current_games)


def apply_proe_overlay(
    base_prob: float,
    team: str,
    year: int = 2025,
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
        year:                Season year (Phase 7 fix -- see get_coach_proe()).
        n_current_games:     Current-season games played.
        current_season_proe: Current-season PROE in percentage points (optional).
        base_pass_rate:      League-average pass rate anchor for logit conversion.

    Returns:
        Adjusted pass probability (0–1), clipped to [0.01, 0.99].

    Example:
        >>> p = apply_proe_overlay(0.55, team="KC", year=2025, n_current_games=0)
        # Andy Reid PROE ≈ +6.4pp → logit offset applied → slightly higher than 0.55
    """
    proe = get_coach_proe(team, year, n_current_games, current_season_proe)
    if abs(proe) < 0.01:
        return base_prob  # No adjustment for league-average coaches

    logit_offset = _proe_to_logit_offset(proe, base_pass_rate)
    adjusted = _sigmoid(_logit(base_prob) + logit_offset)
    return np.clip(adjusted, 0.01, 0.99)


# ── Diagnostics (not called during simulation) ────────────────────────────────

def print_overlay_table(teams: Optional[list] = None, year: int = 2025) -> None:
    """
    Print a diagnostic table showing PROE and logit offset for each team.
    Useful for validating the overlay before integration.
    """
    team_to_coach = _load_team_to_coach(year)
    if teams is None:
        teams = sorted(team_to_coach.keys())

    print("\n{:<6} {:<22} {:>10} {:>13} {:>8} {:>8}".format(
        "Team", "Coach", "Hist PROE", "Logit Offset", "55%->", "45%->"))
    print("-" * 72)
    for team in teams:
        coach = team_to_coach.get(team, "Unknown")
        proe  = _HISTORICAL_PROE.get(coach, 0.0)
        offset = _proe_to_logit_offset(proe)
        adj55 = apply_proe_overlay(0.55, team, year)
        adj45 = apply_proe_overlay(0.45, team, year)
        print(f"{team:<6} {coach:<22} {proe:>+10.2f} {offset:>+13.4f} {adj55:>7.1%} {adj45:>7.1%}")


if __name__ == "__main__":
    print("=== PROE Overlay Diagnostic ===")
    print_overlay_table()
