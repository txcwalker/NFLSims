# Clock Pace Model — Documentation
**Version:** V.0.1.0
**Status:** ✅ COMPLETE — deliberately non-trained by design
**Location:** `src/nfl_sim/models/clock_pace_v_0_1_0/`

---

## 1. Purpose

**What does this model do?**
After each play, samples how many seconds run off the game clock before the next snap (the "pace" of the game), conditioned on the quarter/time-window and the current score margin.

**Why do we need it?**
Real NFL pace is not a smooth function of time — it's driven by discrete behavioral regimes (two-minute drill urgency, trailing-team hurry-up, leading-team clock-killing) that a regression model tends to blur into an average. Sampling directly from real empirical pools preserves that regime structure without having to hand-model each behavior.

**Why this isn't a trained model:** the pace distribution within each (time-window × margin-tier) cell is itself the thing worth preserving — fitting a parametric distribution to it would smooth over exactly the bimodality (rushed vs. deliberate snaps) that makes pace realistic. Sampling from the real empirical pool sidesteps that problem entirely.

---

## 2. Mechanism

- **Call site:** `NFLGameEngine._sample_pace_runoff()`, `game_engine.py:2042-2110`.
- **Lookup key:** `(row_group, margin_tier) → empirical sample pool`, drawn via `np.random.choice`.
- **`row_group`** (11 buckets): `Q1`, then `Q2`/`Q3`/`Q4` each split into 2-3 time windows (e.g. `Q4 5:00-2:00`, `Q4 <2:00`) to capture two-minute-drill behavior separately from normal-pace play.
- **`margin_tier`** (7 tiers, posteam's perspective): `Trailing 3+ score` through `Leading 3+ score`. In Q1, the two most extreme tiers are merged into their 2-score neighbors (too rare/noisy for a separate bucket this early).
- **Source data:** 116,258 real running-clock snaps, `nfl_data_py` PBP 2021-2025 regular season.
- **Calibration nudge:** a flat `+2` seconds is added to every sampled runoff (tuned to match observed real pace after the raw empirical draw ran slightly fast).
- **Special case — Q4 late-game override:** the `Q4 5:00-2:00`/`Q4 <2:00` windows' `Tied`/`Leading` tiers reuse the `Q4 10:00-5:00` pool instead of their own. Their own raw pools are bimodal — contaminated by trailing-team timeout stoppages, which are already handled separately by the timeout model. Reusing the clean pre-timeout-window pool avoids double-counting that slowdown.

---

## 3. Data File

`pace_pools.json` stores the actual sample pools keyed by `"{row_group}|{margin_tier}"`. `metadata.json` documents the row groups, margin tiers, and the Q4 override rule (reproduced above).
