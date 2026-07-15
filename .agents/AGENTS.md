# NFLSims Project: AI Agent Guidelines (AGENTS.md)

This document contains rules, guidelines, and constraints for all AI agents working on the `NFLSims` project. These rules ensure that all modeling, simulation, and analysis remain statistically sound, consistent, and well-integrated.

---

## 🏈 1. Core Modeling Principles

### 1.1 Positional Evaluation & Impartiality
- **Roster-Agnostic Evaluations:** When performing chess-style positional evaluations (such as KEP or EP calculation), the agent must evaluate the game state **impartially**, independent of the active teams' roster talent.
- **Roster Neutralization:** Overwrite team/player DNA matrices with baseline league-average metrics ($CPOE = 0.0$, $Separation = 2.9\text{ yds}$, etc.) during positional rollouts. This prevents the evaluator from measuring roster talent instead of pure situational value.

### 1.2 Evaluation Metrics
- **KEP (Kickoff Equivalent Points):** Use KEP as the central "centipawn" evaluation metric of the game. It represents the point differential at kickoff that matches the current Win Probability.
- **EP (Expected Points):** Use static Expected Points models to measure situational field-position value, aiding in optimal play selection.
- **Do Not Use PPS:** The Pure Positional Score ($\text{Score Diff} + \text{EP}$) is deprecated. Clock-aware and situational metrics should be represented directly via KEP and EP.

### 1.3 Play Concept Classification
When categorizing simulated plays for analytical reporting or recommendations, inspect the play execution variables to classify them as follows:
- **Run:** Standard rush play (structured to support left/center/right direction splits in future updates).
- **Screen Pass:** Pass play where `air_yards <= 0` (or `air_yards < 3` with high yards after catch).
- **Short Pass:** Pass play where $0 < \text{air\_yards} < 10$.
- **Medium Pass:** Pass play where $10 \le \text{air\_yards} < 20$.
- **Deep Pass:** Pass play where $\text{air\_yards} \ge 20$.

---

## 🛠️ 2. Coding & Implementation Guidelines

### 2.1 File & Directory Specificity
- All script and folder names must be highly descriptive and specific to their sub-projects. Avoid generic names like `evaluator.py`. Use specific prefixes/suffixes (e.g. `nfl_positional_evaluator.py`, `positional_ep_v_0_1_0/`).
- Place all machine learning models under `src/nfl_sim/models/` in separate, versioned subdirectories containing their own `train_*.py` and `*_inference.py` scripts.

### 2.2 Vectorized Operations
- Keep the game engine (`game_engine.py`) and math helpers highly optimized using bare `numpy` vectors.
- Avoid pandas constructions and DataFrame lookups inside execution loops, as it introduces significant overhead during Monte Carlo runs.
