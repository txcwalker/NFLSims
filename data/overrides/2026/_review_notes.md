# 2026 season-sheet review — running notes

Teams Cam has reviewed, and open items to come back to.

## Reviewed

| team | status |
|---|---|
| ARI | ✅ done |
| ATL | ✅ removed Trevor Siemian / Nate Carter / Casey Washington (→CAR) |
| BAL | ✅ removed 3 cut rows (Cornelius Johnson / Dayton Wade / Lucas Scott) |
| BUF | ✅ done |
| CAR | ⚠️ **come back to** — see below |
| CHI | ✅ done |
| CIN | ✅ removed Charlie Jones (→ NYG practice squad) |
| CLE | ✅ removed Tillman (→NO PS) / Larvadain / Jack Stoll — all cut |
| DAL | ✅ Joe Milton → practice_squad; removed MVS + Jaydon Blue (cut). Dak CPOE bumped (Cam). |
| DEN | ✅ removed Jaleel McLaughlin (→ CLE practice squad). Coleman rookie projections added (Cam). |
| DET | ✅ removed Greg Dortch (→ ATL PS); Cedrick Wilson → ir / return_week 99 (season-ending). |
| GB  | ⚠️ **come back to** — Jacobs gap: model GB as one of the most pass-happy teams. See below. |
| HOU | ✅ removed Justin Watson (cut); Jayden Higgins + Tank Dell → ir / return_week 99 (out for year). Nico Collins on a league-high target rate (Cam) — monitor. |
| IND | ✅ removed Ulysses Bentley (released). |
| JAX | ✅ removed DeeJay Dallas (→ MIN practice squad). Trevor Lawrence aypa 9.14 looks high vs his ~7.5-8.0 career — flag. |
| KC  | ✅ removed Emari Demercado (→ DAL). |
| LA  | ✅ done |
| LAC | ✅ done |
| LV  | ✅ done — **discussion:** implement a mid-season QB change (Cousins → Mendoza after a benching). Same pattern needed for ATL / CLE / ARI. See below. |
| MIA | ✅ removed Tutu Atwell (→LA) / Theo Wease (→LAC PS) / Quinn Ewers (cut) / Tahj Washington (unsigned). |
| MIN | ✅ done |
| NE  | ✅ removed Kayshon Boutte (→ HOU, already on that sheet w/ a role); Terrell Jennings → return_week 99 (season-ending IR). |

| NO  | ✅ removed Bub Means (cut) / Ja'Lynn Polk (retired). **Only 4 active WR left (Olave, Vele, Barion Brown, Bryce Lance) — thin room, keep an eye on it.** |
| NYG | ✅ removed JuJu Smith-Schuster (off the team). |

| NYJ | ✅ done — Geno CPOE lowered ~1pt; Omar Cooper + Kenyon Sadiq given full rookie projections (Cam) — **watch these two for over-efficiency in sim output.** |
| PHI | ✅ removed Quez Watkins (cut). Lemon + Stowers projections added (Cam). Hurts CPOE looks high — monitor. |
| PIT | ✅ removed Kaleb Johnson (→ GB, already there). Germie Bernard projection added (Cam). |
| SEA | ✅ removed Harrison Bryant (cut). Jadarian Price projection added. Charbonnet (PUP) return_week already at 7 per positive reports. |

| SF  | ✅ removed Malik Turner (cut, unsigned elsewhere). Stribling given full projection. 4 real WR contributors (Evans/Robinson/Stribling/Samuel) — Cowing/Watkins are on the roster but unlikely to see a real role. |
| TB  | ✅ done |
| TEN | ✅ done — Carnell Tate given full projection. |
| WAS | ✅ removed Van Jefferson (cut); Jerome Ford → `ir` / return_week 8, note flags cut-vs-IR uncertainty (kept reversible). |

**All 32 teams reviewed and applied as of 2026-09-04.**

## Open items

### CAR — revisit
- **QB:** Bryce Young is the starter; undecided whether to bump him up or down. CPOE already nudged down.
- **TE room** — unresolved.
- **WR bumps** — possible. McMillan touched up; Coker's catch_rate nudged down. Leggette has a better
  catch_rate than McMillan but left as-is for now.

Full 32-team PROE table (by coach history): [`_team_proe.md`](_team_proe.md).

### Mid-season QB changes (LV, ATL, CLE, ARI) — design pending
Need a way to project a starter flip partway through the season (LV Cousins→Mendoza,
ARI Brissett→Beck?, ATL ?, CLE Watson↔Sanders). Proposed: a `_qb_transitions.csv`
(team, wk1_starter, later_starter, change_week[, range]) that the season-sim wrapper reads
and uses to set the engine's existing `starter_override` flag per week — mirrors the injury
`return_week` mechanism. Blocked on the same "wire resolve_week_rows into the season sim" step.
CLE is a *split* not a transition (Cam models Watson/Sanders ~50/50 from wk 1) — may need its
own handling. Confirm each team's two QBs + rough change week with Cam before building.

### GB — make one of the most pass-happy teams (Jacobs gap)
The PROE (pass-rate-over-expected) mechanism:
- **Live knob:** `coach_dna.json`'s flat `"proe"` per coach (Matt LaFleur = -0.78), applied as a
  post-model pass-rate bump every play (`game_engine.py:1185`). NFL team PROE historically ~ -8 to
  +8; pass-heavy teams (recent CIN/MIA) sit +3 to +6. Set GB to ~+7/+8 to be the most pass-happy.
- **`coach_coordinator_levers_2026.csv`** (has `proe_primary/redzone/goalline`) is **NOT wired into
  anything** — built as the edit surface, never connected.
- **Dormant zone-split PROE:** `coach_proe_splits` is a real feature of the play-selection model
  (`proe_by_filter`) but is fed `0.0` at inference (never populated). Wiring the levers CSV into it
  would give redzone/goalline nuance + fix that latent bug. Bigger job.
- **Decision pending:** quick (durable override CSV → `coach_dna.json` proe) vs. proper (wire the
  levers CSV + zone-split model feature).

### Rookie preseason-efficiency fields — league-wide gap
~15 players with a real projected role (target_share ≥ 3%) are still on flat position-default
`catch_rate` / `adot` / `yac_per_rec` — nearly all drafted rookies (Carnell Tate, Makai Lemon,
KC Concepcion, Jordyn Tyson, Kenyon Sadiq, Omar Cooper, Denzel Boston, Makai Lemon, etc.).
See the 2026-09-03 findings: 2026 NFL data still unpublished; college data only cleanly gives RB
`ypc` (done). **Decision (Cam): hand-fill every rookie manually — the important ones before the
first sim run, the rest after.** No script.
