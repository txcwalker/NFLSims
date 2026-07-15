# 🏈 Dynamic Matchup Multipliers & Quality Separation

To resolve the issue of narrow game spreads (which were clustered between -3.0 and +3.0) and accurately reflect quality differences between elite and bottom-tier teams, we introduced **dynamic matchup-based yardage multipliers**.

## 1. Rationale for Multipliers
The underlying regression/XGBoost models for rushing and passing yards are situational (based on down, distance, yardline, and game seconds) and incorporate individual player DNA splits, but they do not receive team-level defensive EPA or offensive line ratings as direct features. 

Consequently, a simulation between two teams would generate highly similar yardage potentials regardless of their relative strength, squashing spreads. 

By applying relative trench-tier and QB quality differentials to the yards gained on rushes and passes, we introduce team matchup advantages directly into the play-by-play execution.

---

## 2. Mathematical Formulas

We leverage the precomputed `trench_tiers_2025.json` and `qb_cpoe_z` values to calculate offensive advantages for both the home and away teams.

### 🏃 Rushing Multipliers
Rushing yards are scaled based on the run-blocking tier of the offense versus the run-stuffing tier of the defense:

$$\text{run\_diff} = \text{def\_run\_stuff\_tier} - \text{off\_run\_block\_tier}$$

$$\text{run\_multiplier} = 1.06 + \text{run\_diff} \times 0.04$$

* *Elite Rushing Offense vs. Poor Run Defense (Tiers 1 vs 5)*: $\text{run\_diff} = +4 \implies \text{run\_multiplier} = 1.22$
* *Poor Rushing Offense vs. Elite Run Defense (Tiers 5 vs 1)*: $\text{run\_diff} = -4 \implies \text{run\_multiplier} = 0.90$

### 🎯 Passing Multipliers
Passing yards are scaled based on pass protection versus pass rush, combined with the relative quality (Z-scores) of the starting quarterbacks:

$$\text{pass\_diff} = \text{def\_pass\_rush\_tier} - \text{off\_pass\_block\_tier}$$

$$\text{qb\_diff} = \text{off\_qb\_cpoe\_z} - \text{def\_qb\_cpoe\_z}$$

$$\text{pass\_multiplier} = 1.00 + (\text{pass\_diff} + \text{qb\_diff}) \times 0.04$$

---

## 3. Impact on Spreads and Win Probabilities

These formulas widen margins significantly in mismatches while keeping even matchups highly competitive:

* **Mismatches**: Teams like Kansas City (vs. Giants) see their spreads widen to **+20.2** points, and Dallas (vs. Giants) widens to **+15.2** points, matching real-world odds.
* **Even Matchups**: Games like Detroit at Green Bay remain tightly contested with a spread of **+0.4** points.
