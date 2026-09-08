# Benchmark Re-Sim Results — 2026-09-06

Full-season re-sim after the 2026-09-06 batch: completion-rate calibration
(A2 Phases 1–4), throwaway mechanism, trench tail-widening + per-team z
overrides, PROE game-script fade + Q4-trailing overlay, and the week-aware
season sim (injury return weeks + 4 mid-season QB swaps).

**Run note:** the first `regenerate_2026_reports.py` OOM'd in the *playoff*
phase — `BatchSimulator._json_cache` is class-level and the week-aware change
made it load 18×32 = 576 roster trees instead of 32, never releasing them.
Fixed (`batch.py`: per-week / DFS roster paths are no longer cached — re-parse
is <2 ms). The 272-matchup regular-season parquet was already written and valid
(272 game_ids × 1000 iters, all 18 weeks); reports were recovered off it.

---

## ✅ Landed and verified

### Completion model — calibrated
Real-target completion by throw depth (32-matchup measurement):

| air_yards | sim cmp% | real cmp% | Δ |
|---|---|---|---|
| ≤0 | 80.9 | 81.4 | −0.5 |
| 0–5 | 76.7 | 76.4 | +0.3 |
| 5–10 | 67.4 | 65.9 | +1.5 |
| 10–15 | 59.8 | 58.8 | +1.0 |
| 15–20 | 53.0 | 52.8 | +0.2 |
| 20–30 | 42.1 | 39.2 | +2.9 |
| 30+ | 29.9 | 30.1 | −0.2 |

Overall **68.4%** vs 67.8% (real curve @ sim's depth mix). Every bucket within
~1.5pp except 20–30 (+2.9, minor). Was: 30+ at −14pp, whole curve mis-shaped.

### Per-receiver catch rates — population gap essentially closed
`mean(sim − sheet catch_rate)` across 129 WR/TE: **−0.028 → +0.005**.
`corr(split, realized)`: 0.81 → **0.87**.

| receiver | sim | sheet | real 24–25 |
|---|---|---|---|
| Diggs | 0.764 | 0.811 | 0.795 |
| Nabers | 0.601 | 0.614 | 0.620 |
| Mike Evans | 0.641 | 0.602 | 0.624 |
| A.-R. St Brown | 0.767 | 0.748 | 0.741 |
| Ja'Marr Chase | 0.733 | 0.707 | 0.700 |
| **Olave** | **0.619** | 0.699 | 0.660 |
| **Puka Nacua** | **0.714** | 0.757 | 0.765 |
| Thornton | 0.567 | 0.415 | 0.510 |

### QB completion % — realistic
League 64.2% (real ~64.5). **0 QBs over 70%** (was 7), 2 at 69–70. Throwaways
5.3% of dropbacks (real ~5%). Rookie post-swap QBs land low and real: Sanders
55.9%, Mendoza 61.1%.

### Week-aware — working
- **QB swaps** all flip at the right week: Penix (ATL) / Sanders (CLE) /
  Mendoza (LV) from wk 5, Beck (ARI) from wk 12.
- **Injury returns**: Conner (ARI) 0 carries wk 1–7, ~4.7/game from wk 8;
  Charbonnet (SEA) 0 wk 1–6, 8.8/game from wk 7; Tyson (NO) held then real
  target role from wk 8.

### Play-call by script — Q4 trailing fixed
League 56.4% pass (real 57.5). Q4 trailing bands within ~1.5pp of real except
one cell (below). PROE fade + Q4 overlay both doing their job.

---

## ⚠️ Open — needs Cam's call

### 1. Passing volume ~9% low  (biggest issue — drags every QB/WR line)
| per team / season | sim | real |
|---|---|---|
| pass attempts | 518 | ~570 |
| pass yards | 3611 | ~3900–4100 |
| pass TD | 27.4 | ~26–30 |
| total offensive plays | 1041 | ~1080 |
| pass rate | ~54–56% | ~57.5% |

Two compounding causes: (a) the one-score script bands run ~2pp below real
(`tied`/`trail 1-8`/`lead 1-8` at −0.8 to −2.3) — PROE applies at full weight
there and the current coach set skews run; (b) ~4% fewer total plays (a
clock/pace issue, separate from play-selection — ties to the clock_physics
snap-count thread). Levers: a small league-wide base-rate pass nudge, and/or
the plays-per-game work.

### 2. Win distribution compressed  (Cam's review theme #6)
Expected wins: best 12.1 (DET), worst 4.8 (LV), **std 1.89**. Real NFL ~13.5 /
~3.5, std ~2.7–3.0. Trench tail-widening helped marginally; teams are still
bunched toward .500. This is a signal-vs-noise balance across the whole engine
(game outcomes too close to 50/50), not one knob — its own workstream.

### 3. Possession WRs ~4–5pp below real  (`SKILL_SHRINK` residual — in back pocket)
Olave 0.62/0.66, Puka 0.71/0.77, Wan'Dale 0.63/0.66. `SKILL_SHRINK = 0.7`
shrinks *every* receiver's skill term, including the genuinely-good ones. Some
stars nudged slightly high (ARSB, Chase). Fix: move the shrink onto the
depth-adjustment term only so a receiver at their own ADOT gets their exact
catch rate.

### 4. Top RB carries still high  (blowout-logic item)
Gibbs 374, J.Taylor 354, CMC 348, Bijan 326 (within Cam's ~330 cap), Judkins
318. Down a touch from pre-batch but Gibbs/Taylor over. Needs the "pull
starters when differential × time is large" mechanic.

### 5. Deferred / minor
- **trail 2 scores, <4:00 Q4**: sim 86% pass vs real 90% (~1.3% of plays).
- **Leading teams, <4:00 Q4**: sim passes 7–12pp more than real (doesn't kill
  the clock). A `_q4_leading_run_target` mirror of the trailing overlay.
- **20–30 air yards** completion +2.9pp.
- **trail 17+ early (Q1–Q2)**: base model −5pp (frantic offense down 3 scores
  early); the Q4 overlay doesn't cover pre-Q4.
