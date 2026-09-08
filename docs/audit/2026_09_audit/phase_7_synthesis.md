# Phase 7 — Synthesis

Every finding from Phases 0–6, ranked; a recommended fix sequence; the drift
assessment (Cam's #8); and a concrete "done enough to publish" bar.

---

## 1. State of the project (one paragraph)

The engine works, runs fast, and is **close** on the numbers — after the two
carve-out fixes (kickoff returns, iteration_range) game totals sit at ~44 (real
~44), completion-by-depth is within ~1.5 pp, play-selection decisions land at
57.5%. The vision is intact and broad: season sim, DFS optimizer, live 4th-down
bot, two frontends, a full DNA/roster override system. What's missing is
**closure** — the roadmap says "a validated season simulator ships first, then
build outward," and in practice the project has built outward while the sim
stayed in a permanent calibration loop, accreting surface area (40 roster
scripts, a dual-implemented bot, 220 versioned filenames, ~950 MB of data with
backup-dir cruft, no engine tests) faster than the foundation got locked. The
audit found ~50 issues; **none is a deep architectural flaw**, most are "moved
fast on many fronts and didn't circle back."

---

## 2. Findings inventory

**Done this session:** S2-1 (iteration_range), S2-2 (kickoff returns).

### Blocker / near-blocker

| id | finding | phase | effort |
|---|---|---|---|
| **S2-5** | repo lives in a syncing OneDrive folder writing `*-Cams-Desktop` conflict copies (4 locations, `.obsidian` conflicted 8×). Silent data-corruption risk, evidence it's already happening. | 3 | S — move repo, ~1 hr |
| **S2-6** | 308 MB pass-training master + R model `.rds` splits: gitignored, no builder, **no backup**. 4 model families' training data unrecoverable if lost. | 3 | S — back up now; M — write builders |
| **S2-13** | `game_engine.py` (3,377 lines, most-changed file) has **zero unit tests**. The absence that let every other finding hide. | 6 | M — invariant suite |

### Significant (S2)

| id | finding | phase | effort |
|---|---|---|---|
| S2-3 | pervasive silent-degradation loading (`_load_json` → `{}`, missing model → `None`, missing coach → league-avg). AGENTS.md is a catalogue of this running undetected for months. | 1 | S — a require/assert layer |
| S2-4 | play-selection `is_pass` label excludes scrambles; engine drains ~5% of pass calls to scrambles → box pass rate 0.540 vs real 0.567. | 2 | M — retrain (script exists) |
| S2-7 | `run_week_1_only_2025.py` overwrites the shared season parquet with 16 games. | 3 | XS — delete/repoint |
| S2-8 | `/api/simulate`, `/api/optimize` have no iteration cap → trivial DoS. | 4 | S — `Field(le=…)` + `slowapi` |
| S2-9 | analytics frontend has no env-var API base (`API_BASE='/api'` hardcoded) — can't point at a prod API. #1 publish blocker. | 4 | S — copy DFS site's pattern |
| S2-10 | `safeFetch` silently serves mock data on API failure — a broken deploy shows fabricated plays as real. | 4 | S — visible error state |
| S2-11 | 4th Down Explorer calls `/api/fourth-down-evaluate`, which doesn't exist → headline feature shows mock results. | 4 | S — build the endpoint |
| S2-12 | two bot entry points (`src/live/main.py` vs `R/bots/run_live_today.R`), two posting policies; CI runs the R one whose cooldowns reset every 10-min invocation. Dry-run only today. | 5 | M — pick one, Cam decision |

### Minor (S3) — grouped

- **Dead code:** sequential engine path + `predict_play_selection_proba`
  (S3-4, S3-10); `legacy/` 38 files; root `reports/`; empty `Notebooks/`
  `artifacts/`; `venv_py38_old/`; `cleanup_data_dir.py`; `data/interim/_pre_*`;
  `R/scripts/play_selection_*.R`; 17 R logs; empty R dirs; `.pyc` cruft;
  `compileRosters.js`.
- **Tooling:** no `pyproject.toml` → `pytest` wanders + `sys.path` hacks in 10
  runners (S0.3, S3-16, S3-32); no CI test run (S3-24, S3-31); no `renv.lock`
  (S0.4, S3-26).
- **Storage:** 5 uncoordinated parquet writers (S3-12); `_cache_is_stale`
  false-positives (S3-14); year-hardcoded runner duplication ×5 (S3-13); hand-
  rolled data backups (S3-15).
- **API:** 4k-line `app.py` (S3-17); `no-store` on all responses (S3-20); RAM
  loads 65 MB parquet (S3-21 — DuckDB fixes); ESPN passthrough fragility
  (S3-19); no auth (S3-18 — fine for F&F if S2-8 done).
- **Docs:** `README.md` stale + absolute paths (S0.1); `DEVELOPMENT.md` wrongly
  says boxscores untracked, 5 wks stale (S0.11); `AGENTS.md` §0 = 71-line
  worklog, "92 tests" (S0.9); `GOAL_TRACKER.md` multiple false completes
  (S0.2, S3-33); 5 stale root planning docs (S0.5); 82 tracked generated
  boxscores (S3-34); `docs/` grab-bag dirs (S3-35/36/37); `src/live/README.md`
  absolute paths (S3-30); API-doc scatter (S0.10); 6 coach levers need an
  "inert" note (S3-9).
- **Correctness-adjacent:** `_build_slot_map` hardcoded TE surnames (S3-5);
  module-level `warnings.filterwarnings('ignore')` (S3-6); 6th zone-classify
  copy (S3-7); JSON cache no mtime check (S3-8).

### Cam decisions required

1. **Filename `_v_0_1_0`** — 220 files. Strip from code (keep on model dirs)?
2. **S2-12** — which bot entry point is canonical?
3. **Coach levers** — confirmed: keep + document as inert (not deleting).
4. **Publish path** — Option A (static, now) confirmed direction? See §5.

### Deferred — own workstreams, not fix-pass

- `app.py` → routers · Publishing Option B (season start) · the 4 R-era model
  retrains · serverless parquet datalake · win-distribution compression ·
  `SKILL_SHRINK` possession-WR residual · trailing-in-Q4 remaining ~2pp ·
  leading-team Q4 clock-kill · plays-per-game ~1% low · return-yardage refit
  for punt/INT/fumble (KO done).

---

## 3. Recommended fix sequence

Ordered by dependency and risk. Each batch is its own commit (or few).

### Batch A — safety, before anything else
1. **S2-5** — move the working copy out of OneDrive; GitHub is the sync. Delete
   conflict copies, add `.gitignore` tripwire.
2. **S2-6** — push the 308 MB master + `.rds` splits to Cloudflare R2/B2.
3. **S2-3** — `_require(...)` guards: fail loud at `NFLGameEngine.__init__` if a
   non-optional input is missing/empty.

### Batch B — dead-code removal (unblocks the rest)
4. **S3-4 + S3-10** — delete the sequential engine path, its worker, the
   `legacy.game_engine_sequential` import, `predict_play_selection_proba`.
5. **S3-5, S3-6, S3-7, S3-8** — the small engine/batch cleanups.
6. Dead-file sweep (Phase 6 §5 table) — one commit.

### Batch C — tooling foundation
7. **`pyproject.toml`** + `pip install -e .` → kills `sys.path` hacks, fixes
   `pytest` paths, adds a ruff config. (S0.3, S3-16, S3-32)
8. **`ci.yml`** — `pytest tests/` + `npm run build` ×2 on push. (S3-24)
9. **`renv.lock`** for R. (S0.4, S3-26)

### Batch D — the engine test suite
10. **S2-13** — deterministic invariant tests (Phase 6 §1 list). Do this
    *before* Batch E so the calibration changes have a floor.

### Batch E — engine calibration fixes + storage
11. **S2-4** — retrain play-selection with the dropback label.
12. **S3-12** — one parquet writer + `*_meta.json` sidecar (SHA/N/weeks/date).
13. **S3-14** — content-hash cache staleness (kills the false-positive re-sims).
14. **S3-13** — collapse the 5 year-hardcoded runners → `run_full_season_sim.py <year>`.
15. gitignore `docs/boxscores/`; `data/_snapshots/` retention policy.

### Batch F — the validation benchmark  ← the gate
16. Full season regenerate on the post-Batch-E engine.
17. Compare against the bar in §4. **If it clears → mark Goal 1 validated.**

### Batch G — API/frontend publish-prep (only if §4 clears)
18. **S2-8** iteration cap + `slowapi`. **S2-9** env-var API base.
    **S2-10** error state. **S2-11** build `/api/fourth-down-evaluate`.
19. **S3-22** gate Testing Lab / Live WP / BotFeed out of the prod build.
20. Static build step for the 5 season JSON files.

### Batch H — docs
21. Rewrite `README.md`. Fix `DEVELOPMENT.md`. Trim `AGENTS.md` §0. Correct
    `GOAL_TRACKER.md`. Document the inert coach levers.
22. Root `.md` → `docs/planning/`; `docs/` subdir consolidation.

### Cam-gated (slot in when decided)
- Filename `_v_0_1_0` strip → one rename commit (after Batch B).
- **S2-12** bot consolidation.

### Then: publish (Option A) — its own mini-project, not a fix-pass commit.

---

## 4. "Goal 1 is validated" — a concrete bar

The roadmap's Phase 1.5 gate ("Full system validation") has no numbers. Propose
these, measured on a full-season regenerate vs real 2023–2025:

| metric | pass if | current (post carve-out) |
|---|---|---|
| game total pts | within ±1.5 of real (~44) | ~43–44 ✅ |
| completion % by throw-depth bucket | every bucket within ±2.5 pp | within ~1.5 ✅ except 20–30 (+2.9) |
| box pass rate | within ±1.5 pp of ~56.7 | 54.0 ✗ (S2-4 fixes) |
| sacks / game | within ±5 % of 2.41 | 2.4 ✅ |
| pass yds / team / g | within ±5 % of ~225 | 214 ~✗ (−5 %) |
| top-RB season carries | ≤ ~370 | Gibbs ~365 ~✅ |
| expected-wins spread (std) | ≥ 2.4 | 1.9 ✗ — **known, Cam accepts for now** |
| return TDs / game | < 0.1 | fixed ✅ |
| 97+ tests + new engine suite green | yes | pending S2-13 |

Realistic read: after Batch E, everything except **win-distribution spread**
should clear. Cam has already said the compressed spread is acceptable for the
2026 launch. So the honest position is: **fix the audit's S2 items, run the
benchmark, and if it looks like the table above — declare Goal 1 validated for
the 2026 season, publish Option A, and treat win-distribution as a documented
Tier-1.1 follow-up.** Don't let "one more round" run forever.

---

## 5. Drift assessment (Cam's #8)

**Have we strayed from the original goals?** No — every component maps to a
roadmap item. The season sim, DFS optimizer, positional evaluator, 4th-down bot,
DNA system are all on the plan.

**Have we strayed from the original *plan*?** Yes, in sequencing. The roadmap's
core discipline — *"Everything is downstream of an accurate, validated season
simulator. This ships first."* — has been inverted in practice:

- **Goal 1** (season sim) is ~90 % and has been ~90 % for months. It runs, but
  "validated" was marked ✅ in the tracker while the engine was actively being
  recalibrated (completion, PROE, trench, kickoffs — this audit found more).
  It has **never been published**, though "Tier 1 Public Release Sept 2026" is
  now.
- **Goal 2** (DFS/betting) is substantially built — optimizer, projections,
  DK/FD export, a second frontend, betting-line helpers, a sim-vs-market plan.
  The roadmap gates this on "a validated Season Simulator as its data engine."
  It got built anyway.
- **Goal 4** (live tools) — the 4th-down bot runs (dry-run). Roadmap gates it on
  "Goals 1–3 substantially complete."

This isn't reckless — the DNA/roster override work *is* Goal 1 (accurate inputs
= part of validation), and the DFS optimizer is genuinely useful and mostly
independent. But the **cost of breadth-before-depth** is exactly what this audit
enumerated: no engine tests, a dual-implemented bot, 40 roster scripts, 220
versioned filenames, 950 MB of data with hand-rolled backups, silent-failure
loading, doc-rot in the tracker. Each was individually cheap; together they're
the reason a "90 % done" sim can't just be shipped.

**Are we still set up to grow?** Yes — the architecture is sound (vectorized
engine, clean model/DNA separation, the override-sheet workflow, the serverless
publish plan). Nothing here is a rewrite. The growth risk isn't the design;
it's the discipline: **more surface area without closing loops** compounds into
unmaintainability. The audit's Batch C (tooling) and Batch D (engine tests) are
the antidote — they make "moving fast" safe again.

**Recommendation:** run the fix-pass, hit the §4 bar, **declare Goal 1 done for
2026 and publish**. Then adopt a rule: a roadmap item isn't ✅ until it has
tests and its docs are current. That single change turns the drift around
without slowing the vision.

---

## Audit complete

7 phases, findings in `phase_0`–`phase_7`. Baseline `c2287e0`; carve-out fixes
in `75b49bc`. Next step is Cam's call on the four decisions in §2 and whether to
start the fix-pass.
