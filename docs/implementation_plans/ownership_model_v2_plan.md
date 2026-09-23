# Implementation Plan — Ownership Model v2 (Segmentation Features + Prior-Week Signal)

**Status:** A and B both implemented and shipped 2026-09-22 (built autonomously while
Cam was out, per his go-ahead). Full session detail, files touched, and verification
in [WORKLOG.md](../../WORKLOG.md)'s 2026-09-22 entry — this doc is left as the design
record; treat WORKLOG.md as the source of truth for what actually landed and any
open follow-ups.
**Author:** drafted with Claude, 2026-09-22; built by Claude Sonnet 5, 2026-09-22
**Roadmap slot:** Tier 2 — DFS ownership projection model. `GOAL_TRACKER.md` currently
marks this line "✅ Completed (Aug 2026)" — this plan reopens it (see §6).

---

## 0. Data-capture process gap — found 2026-09-22, recovered same day

Rebuilding `_processed/*.parquet` to get real numbers for this plan surfaced
a process gap: week 2's pre-lock salary snapshot was never taken (main slate
+ 3 showdown slates), and neither was one week-1 showdown slate
(`showdown_DAL_NYG`) — no `manifest.json` existed for any of them, so their
already-downloaded standings CSVs couldn't be joined to a salary/Vegas
snapshot.

**Recovered anyway.** DK's *draftables* endpoint (not just the lobby) kept
serving each of these after the fact:
- **Classic main slate (week 2):** `resolve_main_slate_draft_group_id`'s
  sticky per-week pin ([data/dk_main_slate_pins.json](../../data/dk_main_slate_pins.json)) still pointed at the
  real draft group (`153428`, cross-confirmed against the Optimizer's own
  build history in `data/optimizer/2026/week_02/builds/`) — refetched
  directly, 670 players.
- **3 week-2 showdown slates (BUF/DET, IND/KC, LA/NYG):** no manifest to
  `--backfill` from, so their draft-group IDs were located by probing nearby
  IDs (DK assigns them in a tight sequential block per week) and verifying
  each fetch's returned team names against the schedule before trusting it.
  DK runs multiple product variants (Showdown, Snake Showdown, etc.) per
  matchup under different draft-group IDs — confirmed all variants returned
  byte-identical player/salary data before picking one arbitrarily.
- Also fixed in passing: 3 of week 2's classic contest CSVs
  (`firstdown_1_20max .csv` — trailing space, `minimax_,5_150max.csv` /
  `pocket_,01_150max.csv` — comma instead of period) had typo'd filenames and
  were being silently skipped by the parser even after salaries came back.
  Renamed to the correct `name_price_xmax.csv` convention.
- Also found and fixed a real bug this recovery exposed: `slate_id` in
  `build_ownership_dataset.py` is just the archive folder's basename (e.g.
  `"main_slate"`), which collides across *different weeks* by construction.
  `train_ownership_model.py`'s leave-one-slate-out CV was grouping on bare
  `slate_id`, so even with week 2 back, classic CV would have kept reporting
  "only 1 slate" — week 1 and week 2's `main_slate` rows were silently
  merging into one fold. Fixed to group on `(year, week, slate_id)` instead
  (`_heuristic_baseline` had the same bug — it would've fed the heuristic a
  garbage pool mixing two different weeks' salaries for the same player
  name). `slate_id` itself was left alone — other scripts
  (`score_paper_entries.py`) parse its exact string format, so widening the
  fix's blast radius wasn't worth it for a bug that only mattered inside CV
  grouping.

**Net effect — real, current coverage:** 2 classic slates, 6 showdown slates
(week 1 + week 2). Re-ran `train_ownership_model.py` (not `--dry-run`) with
the fix in place; both models now beat the heuristic on every held-out slate
and are saved to `data/dfs_ownership/_processed/models/` — see §3.1 and §3.3
for the actual numbers.

**Still worth doing going forward:** DK only posts each showdown slate close
to its own kickoff, not all at once — that's almost certainly what caused
the original gap. Re-run this a few times across the week, not once:
```
venv\Scripts\python.exe scripts/dfs_ownership/snapshot_slate_salaries.py --week 3
```
Week 3's main slate + the one showdown game live so far (ATL@GB, Thu) are
already snapshotted as of 2026-09-22.

---

## 1. What this is

Two additive improvements to the existing classic + showdown ownership pipeline
([src/ownership/](../../src/ownership/), [scripts/dfs_ownership/train_ownership_model.py](../../scripts/dfs_ownership/train_ownership_model.py)),
scoped in a design discussion with Cam on 2026-09-22:

- **A — Segmentation features.** Feed contest field size, stakes tier, and
  entry-max into the *existing* pooled classic/showdown models as features
  (not separate models per bucket — see §3 for why).
- **B — Prior-week performance signal.** A per-player feature for last week's
  realized score, with sentinel codes for the cases where "last week's score"
  isn't a normal number: `-99` no history (week 1, or first week we've tracked
  this player), `-76` returning from a multi-week injury absence, `-51` team
  was on a bye last week. A one-week injury absence uses the real last score
  (per Cam's own spec — not a separate code).

Both are feature additions on top of the current architecture — two formats
(classic / showdown-FLEX / showdown-CPT), XGBoost regressor on logit-ownership,
leave-one-slate-out CV, heuristic fallback when no model is on disk, and
[normalize.py](../../src/ownership/normalize.py)'s exact-sum renormalization to real DK roster math. None of that
changes here.

**What prompted this:** a review of the current model against Cam's variable
list turned up two real gaps (field/stakes/max-entries data is archived but
never reaches the model; there's no prior-week signal anywhere) and one
already-solved worry (the 100%/500%/QB-exactly-100 constraints are not
hand-wavy — `normalize.py` already enforces them exactly, derived from roster
slot math). Full variable-by-variable audit lives in this conversation's
transcript, not duplicated here.

---

## 2. Scope

**In scope**
- A1: Add `field_size` (or `field_bucket`), `stakes_tier`, `max_entries` to
  `CLASSIC_FEATURES` / `SHOWDOWN_FEATURES` in `train_ownership_model.py`
  (training side — the columns already exist in `features.parquet`, this is
  just adding them to the feature list).
- A2: Resolve the live-inference plumbing gap (§3.2) — this is the harder
  half of A and needs a decision before it's buildable.
- A3: Retrain, confirm leave-one-slate-out CV doesn't regress (§3.3 — the bar
  is "doesn't hurt," not "improves," given current data volume).
- B1: Decide the data source for last-week's realized DK score, bye-week
  detection, and injury-return detection (§4.2 — open questions, not yet
  answered).
- B2: Wire the resolved signal into `build_ownership_dataset.py`'s
  `_enrich()` and `model_inference.py`, for both classic and showdown (same
  signal, both formats).

**Explicitly deferred**
- True per-bucket model splitting (separate model files per field-size ×
  stakes × max-entries combination). Revisit once coverage reaches the
  README's own stated target (~4–6 slates per bucket), and decide via
  leave-one-slate-out CV (pooled-with-features vs. split) rather than by
  default.
- Cam's item 6 ("what position/value is screaming in FLEX this week") — he
  flagged this as unsolved himself; needs its own design conversation, not
  bundled into this plan.
- FanDuel ownership modeling — separate model, own roadmap item, mentioned by
  Cam as "eventually."

---

## 3. A — Segmentation features

### 3.1 Why pooled + features, not separate models per bucket

Real usable archived data as of 2026-09-22, after the §0 recovery: **2
classic slates, 6 showdown slates** (week 1 + week 2). Better than the 1/3
first estimated, but a full field-size (3) × stakes-tier (3) × max-entries
(~3) split is still up to 27 slices against 2-6 slates per format — nearly
all would train on zero rows, and even a "full" slate isn't fully independent
data (every player on it shares the same salary pool and Vegas lines that
week). That's still memorizing a handful of weeks, not modeling a segment —
the exact overfitting risk Cam raised unprompted.

Pooling with these as model features shares the Vegas/salary/projection
signal across all available data while still letting XGBoost carve out
segment-specific behavior (including interactions, e.g. "small field AND high
stakes") as soon as there's evidence for it — and degrades gracefully to "no
effect" when there isn't. Confirmed decision (Cam, 2026-09-22): pooled +
features, not per-bucket models.

Current (pre-this-plan, post-§0-recovery) leave-one-slate-out CV, retrained
and saved 2026-09-22:

| Target | Folds | Model MAE | Heuristic MAE | Wins |
|---|---|---|---|---|
| Classic total ownership | 2 | 0.5pp | 2.6pp | 2/2 |
| Showdown FLEX | 6 | 7.0pp | 30.2pp | 6/6 |
| Showdown CPT | 6 | 2.2pp | 21.1pp | 6/6 |

Strong numbers, and classic can finally be measured at all now that week 2 is
back — but 2-6 folds is still few, and CV is currently in-sample-ish (every
held-out slate is one of only 2-3 weeks the whole league's schedule has
played so far, so the folds aren't as independent as they'll be once there
are a dozen+ weeks). Treat as promising and directionally real, not as a
mature, stable estimate.

### 3.2 Live-inference plumbing — what "contest-aware" actually means

Clarifying the confusion from last time: the model never sees a contest's
*name* — only its *characteristics* (field_size, stakes_tier, max_entries).
Two contests with identical characteristics get identical ownership
predictions, whether or not they're literally the same contest. Your
example — the $1 First Down 20-max and the $3 Play Action 20-max — would
land in the same `stakes_tier` bucket (both well under the `sharp` cutoff)
and the same `max_entries`, so unless their actual field sizes diverge, the
model naturally predicts the same ownership for both. Nothing forces them
apart; they just don't happen to look alike.

The remaining question isn't modeling, it's plumbing: right now the API has
no way to tell the ownership predictor *which contest's characteristics* to
use at all. [model_inference.py](../../src/ownership/model_inference.py)'s
`predict_classic_ownership(players, week, cash_consensus, seed)` and
`predict_showdown_ownership(players, week, seed)` take no contest info, and
both `app.py` call sites ([line 4996](../../src/api/app.py) `/optimize`,
[line 6196](../../src/api/app.py) `/showdown_prep`) predict once per **week**,
not per **contest** — today's whole Optimizer session shares one set of
ownership numbers.

**Decided approach:** add an optional `contest` param (field_size, entry_fee
→ stakes_tier, max_entries) to both predict functions and the `/optimize` /
`/showdown_prep` request bodies. Omitted → falls back to today's behavior
(no regression for existing callers). This is also the natural hook for
`optimizer_persistence_plan.md`'s already-planned "target contest chosen up
front" — once that lands, it can populate this param automatically instead
of the frontend needing its own separate UI for it.

### 3.3 Retraining acceptance bar

Re-run leave-one-slate-out CV after adding the segmentation features and
compare against the §3.1 baseline (classic 0.5pp / showdown FLEX 7.0pp / CPT
2.2pp mean MAE). Acceptance bar is "doesn't get worse," not "must improve" —
2-6 slates is still few, and the segmentation features may not vary much
within any single week's contests, so a clean win isn't a realistic
expectation yet. Re-check this bar again after each future week's data lands.

---

## 4. B — Prior-week performance signal

### 4.1 Sentinel encoding (as specified by Cam)

| Code | Meaning |
|---|---|
| `-99` | No prior game to reference (week 1, or first week we've tracked this player on any slate) |
| `-76` | Returning from a multi-week (2+ games) injury absence |
| `-51` | Player's team was on a bye last week |
| *(real score, >0)* | Otherwise, last week's actual DK fantasy score — including a one-week injury absence, which uses the real last-played score rather than a code |

### 4.2 Data sourcing — resolved 2026-09-22

**Last week's realized score.** Cam's call: pull it from the previous week's
own settled contest (`actual_dk_score` in `ownership_actuals.parquet`, already
populated per player per slate from DK's standings export — confirmed present
for both classic and showdown rows). No new archive needed for the score
itself.

One wrinkle found while confirming this: `build_ownership_dataset.py`'s
`process_slate()` requires `salaries_prelock.csv` before it will process a
folder *at all* — so a slate missing its salary snapshot (see §0) currently
loses its realized scores too, even though the standings CSV (the only thing
the score actually needs) is sitting right there. **Recommend decoupling
score extraction from the salary-snapshot gate** — a small standalone reader
(reads standings CSVs only, no salary dependency) that emits
`{year, week, player, team, pos, actual_dk_score}` regardless of whether
`salaries_prelock.csv` exists. This is also, functionally, the start of the
lightweight "player-week actuals" table Cam flagged we'll need soon — worth
building now specifically because it rescues data that the salary-snapshot
gap would otherwise strand permanently (week 2's standings CSVs still have
real scores in them even though their salary features are lost).

**Bye-week detection.** Confirmed: `data/external/schedule_2026.csv` is
sufficient — a team absent from that week's `REG` games (checked against
`home_team`/`away_team`) is on a bye. Verified both week 1 and week 2 have
all 32 teams active (no byes yet this season, as expected this early) — the
detection logic is trivial and ready to build, just untestable against a
real case until the season's first bye week.

**Injury-return detection (`-76`).** Cam's call: no separate injury-status
data source for now — instead, count consecutive weeks (excluding byes) where
a player scored 0 *or* wasn't on the slate's salary pool at all ("track
multiple 0s ... a player scoring 0 in one game is totally possible, but in 3,
4 or more they were almost certainly injured"). Proposed rule:

1. Walking backward from the target week, skip any bye week entirely (doesn't
   count toward or reset the streak).
2. Count consecutive non-bye weeks where the player scored 0 or had no salary
   listing that week at all (treated the same — both mean "didn't play").
3. Streak of 0 → use the real last score (even if it's a single bad/zero
   game — per Cam's spec, one zero week is normal, not flagged).
4. Streak of 3+ → tag the comeback week's feature as `-76` instead of the
   literal (likely 0) value, so the model can tell "one bad game" apart from
   "just came back from a real absence."
5. Open detail to confirm with Cam before building: does "no salary listing
   that week" count the same as "salaried but scored 0," or should those be
   tracked separately? (§4.2 rule above treats them as equivalent.)

Note this rule needs 3+ weeks of consecutive history to ever fire — with the
current 1-classic/3-showdown, single-week dataset, `-76` can't trigger yet.
Build it now anyway so it activates automatically as weeks accumulate, same
pattern as the rest of this codebase (heuristic ships thin, improves as data
grows).

### 4.3 Wiring

Same shape as the existing enrichment features — join into
`build_ownership_dataset.py`'s `_enrich()` (training) and
`model_inference.py`'s row-building loops (live), for both classic and
showdown since it's the same underlying signal for either format. The
standalone score/actuals reader from §4.2 becomes the join source instead of
re-deriving from `ownership_actuals.parquet` directly, so it works even for
slates whose salary snapshot is missing.

### 4.4 Future formal database (flagged, not building yet)

Cam asked to be told if a more formal database is needed for this — yes,
soon, but not blocking today. The standalone score reader in §4.2 is
effectively step one of a `player_week_actuals` table (one row per player per
week: rostered Y/N, realized score, team, bye flag). Once that exists as its
own artifact rather than a derived join, it's also the natural foundation for
grading the DFS projection model and for the injury-streak logic once it
needs to look back further than 2-3 weeks. Recommend formalizing it around
week 4-5, once there's enough history for the injury detection in §4.2 to
actually exercise the multi-week branch — not urgent before then.

---

## 5. Sequencing recommendation

Both A and B are now fully scoped — the open decisions from the first draft
are resolved (§3.2, §4.2). Revised recommendation given §0:

1. **Keep the snapshot process disciplined going forward** (§0) — not a code
   task, a workflow one. Week 2's gap got recovered this time, but that
   depended on DK's draftables endpoint still serving stale draft groups and
   the Optimizer's build history happening to have logged the right IDs —
   not something to rely on twice. Run `snapshot_slate_salaries.py`
   (no `--main`) a few times across each week as new showdown slates post,
   not just once.
2. **A** (segmentation features + contest-aware inference) — small, uses data
   already archived, one API contract change (§3.2) to implement.
3. **B** (prior-week signal, §4.2's decoupled score reader) — slightly larger
   surface (new reader + bye/injury-streak logic), but no longer blocked on
   an open question.

---

## 6. Docs/tracker note

`GOAL_TRACKER.md`'s Tier 2 table currently lists "DFS ownership projection
model (salary/Vegas/cash-consensus blend) — ✅ Completed — Aug 2026." This
plan's existence means that line is stale — same "false-complete" pattern the
2026-09 audit pass already fixed elsewhere in this repo (see `a3948b3`).
Recommend flipping it to 🔄 In Progress and adding this plan's two line items
as sub-goals once Cam approves moving forward — not changed yet, flagging for
his call rather than editing the tracker unilaterally.
