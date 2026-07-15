# Positional Evaluation Metrics — Reference
**Last Updated:** June 2026
**Implemented in:** `src/nfl_sim/nfl_positional_evaluator.py`
**API exposure:** `/api/positional-evaluator`, `/api/historical/suggest-lines?metric=kep|efsd`
**Frontend:** Historical Testing Lab — stacked KEP and EFSD timeline charts

---

## Overview

The Positional Evaluator assigns numeric value to NFL game states using a chess-style frame: *who has the better board position right now, and by how much?* Two user-defined metrics express this, each with different scope and scale:

| Metric | Full Name | Scale | Bounded? | Time Horizon |
|:---|:---|:---|:---|:---|
| **KEP** | Kickoff-Equivalent Points | pts equivalent to a kickoff advantage | Yes, ±24 | Drive-end |
| **EFSD** | Expected Final Score Differential | predicted final margin in points | No | End of game |

Both metrics are **offense-reference** internally (positive = good for the possession team) and converted to **home-team reference** in the frontend (positive = home team leading positionally).

---

## KEP — Kickoff-Equivalent Points

### What it measures
KEP answers: *"If we froze the game here, how much better off is the possession team than a team receiving a neutral kickoff?"*

A neutral kickoff spot is worth **0 KEP**. Having the ball 1st & 10 at your own 20 is worth slightly positive KEP. Being pinned deep on 4th & 15 is worth negative KEP. The metric captures positional advantage, not current score.

### How it's computed
1. The Win Probability model produces a WP curve for the current game state.
2. The KEP model inverts that curve through a calibrated mapping: `wp → points_on_a_±24_scale`.
3. The mapping is derived from the kickoff WP baseline — the "neutral" reference point.

### Interpretation

| KEP value | Meaning |
|:---|:---|
| **+24** | Maximum positional advantage — equivalent to an opponent kickoff in a blowout |
| **+6 to +12** | Strong positional control (e.g., 1st & 10 in opponent territory) |
| **0** | Roughly neutral, similar to a normal kickoff spot |
| **−6 to −12** | Opponent has the positional advantage |
| **−24** | Maximum positional disadvantage |

### Key constraint
KEP is **bounded ±24** because it inherits the WP model's ceiling/floor at 0% and 100% win probability. This means it correctly compresses at extreme score differentials or very late clock situations — but cannot represent the magnitude of a blowout beyond that ceiling.

---

## EFSD — Expected Final Score Differential

### What it measures
EFSD answers: *"If we played this game to completion a thousand times from this exact game state, what would the average final margin be?"*

A positive EFSD means the possession team is expected to win by that many points on average. Zero means a pick-em. A negative EFSD means the possession team is expected to lose.

### How it's computed
A dedicated XGBoost regressor (`efsd_model.json`) is trained directly on historical play-by-play data. The target is `final_posteam_score − final_defteam_score`. The 8 game-state features are identical to those used by the WP model.

See [`docs/models/efsd_v_0_1_0.md`](../models/efsd_v_0_1_0.md) for full model documentation.

### Interpretation

| EFSD value | Meaning |
|:---|:---|
| **+21** | Expected to win by 3 scores — dominant position |
| **+7** | Expected to win by about a touchdown |
| **0** | Pick-em — neither side has a material expected advantage |
| **−7** | Expected to lose by about a touchdown |
| **−21** | Expected to lose by 3 scores — dire position |

**EFSD is unbounded.** A Q4 state where one team leads by 30 can return an EFSD of ±30 or higher. This is correct — unlike KEP, EFSD is not capped at ±24.

### Key behavioral properties
- **Shrinks early:** Q1 predictions are smaller in magnitude than the eventual final margin. This is correct regression-to-mean behavior — the model expresses genuine uncertainty.
- **Converges late:** By Q4 with 2 minutes remaining, EFSD tightly tracks the current score differential (Q4 R² = 0.826).
- **Asymmetric by design:** A team up 7 predicts a larger EFSD (≈ +10–11) than a team down 7 predicts negatively (≈ −4–5). Leading teams play conservatively and can expand their margin. Trailing teams take risks that increase variance and pull the expected margin back toward zero.

---

## Choosing Between KEP and EFSD

| Question | Better metric |
|:---|:---|
| How does field position affect strategic value? | **KEP** — field position matters even without score context |
| What is the expected game result from here? | **EFSD** — directly predicts the final margin |
| The game is a blowout — which captures magnitude? | **EFSD** — unbounded; KEP saturates at ±24 |
| Early game — which is more meaningful? | **KEP** — EFSD shrinks toward 0 in Q1 (correct but less informative) |
| Late game with a lead — which is sharper? | **EFSD** — converges to the score differential as clock runs out |
| Evaluating a single play concept (Run vs Deep) | **Either** — use the `metric` toggle on Suggested Lines |

**Rule of thumb:** KEP is better for asking *"who controls the board?"* EFSD is better for asking *"who wins the game?"*

---

## How They Appear in the System

### Positional Evaluator output
```json
{
  "kep": 4.21,
  "efsd": 6.03,
  "concepts": {
    "Run":  { "mean_kep": 3.9,  "delta_kep": -0.31, "mean_efsd": 5.7,  "delta_efsd": -0.33 },
    "Deep": { "mean_kep": 5.1,  "delta_kep": +0.89, "mean_efsd": 7.2,  "delta_efsd": +1.17 }
  }
}
```

### Suggested Lines API
```
GET /api/historical/suggest-lines?metric=kep    # rank by delta_kep
GET /api/historical/suggest-lines?metric=efsd   # rank by delta_efsd
```

### Historical Timeline plays
```json
{
  "play_id": 1234,
  "home_kep":  4.21,   // KEP in home-team reference
  "home_efsd": 6.03    // EFSD in home-team reference (separate chart, unbounded y-axis)
}
```

---

## Scope Boundary

Neither KEP nor EFSD is used inside the **play-by-play simulation engine** (`src/nfl_sim/`). They are evaluator-only metrics — computed after the fact for analysis, not used to drive play-selection logic during a Monte Carlo iteration. This is intentional: the simulation uses WP and EPA directly for in-game decision-making, while KEP/EFSD are for post-hoc positional analysis.
