# Implementation Plan — Optimizer Persistence & Weekly Logbook

**Status:** Draft for review — not yet approved for build
**Author:** drafted with Claude, 2026-09-06
**Roadmap slot:** DFS Optimizer tooling. Candidate line item for `FRONTEND_GOALS.md`.

---

## 1. What this is

Today the DFS Optimizer holds everything in memory — the player pool rebuilds
from scratch on every week/sim change ([Optimizer.jsx](../../frontend/src/pages/Optimizer.jsx) `useEffect` on
`weekProjections/allSimResults/games`), and a page reload wipes settings,
projection adjustments, and generated lineups. Nothing survives.

Cam's use case has two halves:

- **In-week:** come back to the site mid-week and have your settings, projection
  adjustments, exclusions, and every lineup set you've built already loaded.
  Iterate toward the lineups you submit to contests on Sunday.
- **Review:** at the end of each week and end of season, pull up every build you
  made, what you actually submitted, and — eventually — how those lineups scored
  and what they returned.

So this is a **weekly logbook**, and generated lineups are first-class saved
objects, not a byproduct of clicking Optimize.

Storage: **backend JSON files** (decided). Rationale: the optimizer can't run
without the backend anyway; JSON on disk is portable, inspectable, hand-editable,
git-friendly, and matches the project's pandas/JSON/parquet idiom. The review
layer (Phase 4) loads the week folders into pandas — no database needed.

## 2. Scope

**In scope**
- Persist per-week working state (settings + overlay of manual changes)
- Auto-save an Optimize run as an immutable "build" **when the inputs changed**
  since the last build (pure re-runs don't create a build); toggle to opt out
- Browse / label / pin / delete builds; bulk delete; end-of-week auto-prune
- Restore a build's overlay + settings back into the working state (cross-week OK)
- **Target contest is chosen up front** (part of settings, from the live DK list)
  and frozen into the build; recording a submission = confirm + entry count
- **Sticky ownership** — a "freeze ownership for the week" action so the
  portfolio math stops moving run-to-run; hand-edit the few players that matter
- Refactor the player pool so manual edits are an overlay applied on top of a
  fresh sim pool (fixes the current wipe-on-refresh bug as a side effect)

**Deferred to Phase 4 (own effort)**
- Ingesting actual weekly DK player scores
- Grading builds (actual lineup score, field percentile, contest ROI)
- Season-level review page (ROI, ITM rate, projection calibration, did the manual
  overlay adjustments pay off)

**Non-goals**
- Multi-user / auth (single user for 2026)
- Real-time contest sync with DraftKings

---

## 3. Data model

### 3.1 Directory layout

```
data/optimizer/
  2026/
    week_03/
      state.json                 working state (auto-saved, debounced)
      builds/
        20260921T144233Z.json    one Optimize run, immutable except metadata
        20260921T173118Z.json
      results.json               (Phase 4) actual scores + build grades
    week_04/
      ...
```

Paths are season-scoped, so 2027 just works. `week_NN` zero-padded. Build id =
UTC timestamp `YYYYMMDDThhmmssZ` (sortable, unique enough for one user).

### 3.2 `state.json`

```jsonc
{
  "schema_version": 1,
  "season": 2026,
  "week": 3,
  "updated_at": "2026-09-21T14:42:33Z",
  "slate": { "draft_group_id": 151307, "platform": "DK" },
  "settings": {
    "contestType": "top_heavy", "nLineups": 20, "minUnique": 2,
    "includeDstUnique": false, "maxExposure": 40, "projThreshold": 0,
    "contest": {
      "dk_contest_id": "182734551",
      "name": "NFL $3.5M Fantasy Football Millionaire",
      "entry_fee": 20.0,
      "field_size": 206000,
      "max_entries_per_user": 150,
      "paying_positions": 45000,
      "payout_structure": [ /* frozen from /api/dk/contest_payout, or null = estimate */ ]
    }
  },
  "overlay": {
    "ownershipFrozen": true,
    "ownershipFrozenAt": "2026-09-19T18:14:00Z",
    "players": {
      "Jahmyr Gibbs_DET": { "projAdjust": 2.3, "ownershipPct": 18.0, "locked": true, "excluded": false },
      "Chris Godwin_TB":   { "ownershipPct": 6.5 }
    },
    "excludedTeams": ["CHI"],
    "excludedGames": [],
    "gameExclusions": { "NO@DET": 1 }
  },
  "prefs": { "autosaveBuilds": true }
}
```

- `overlay.players` is keyed by the existing pool id `"{name}_{team}"`.
- `projAdjust` is an **additive delta vs the sim median** (see §5). Already how
  the frontend implements manual proj edits.
- When `ownershipFrozen` is true, **every** pool player gets an explicit
  `ownershipPct` here (snapshotted from the live weekly value); when false, only
  players with an override appear and the rest use the live weekly number.
- `settings.contest` replaces the old loose `contestSize / entryFee /
  payingPositions / payoutStructure` fields — you pick a real DK contest up
  front and its identity + numbers ride along. `contest: null` (or a bare
  `{contestType}`) falls back to the contest-type-shaped estimate as today.
- Other than ownership, only players with a non-default override appear.

### 3.3 `builds/<id>.json`

Everything needed to reconstruct and later review one Optimize run.

```jsonc
{
  "schema_version": 1,
  "build_id": "20260921T144233Z",
  "season": 2026, "week": 3,
  "created_at": "2026-09-21T14:42:33Z",
  "source": "autosave",              // "autosave" | "manual"
  "label": null,                     // user-set, e.g. "milly v3"
  "pinned": false,
  "submitted": false,

  "slate": { "draft_group_id": 151307, "platform": "DK" },
  "settings": { /* frozen copy — includes settings.contest, the target contest */ },
  "overlay":  { /* frozen copy — includes the frozen ownership snapshot */ },

  "players_used": {                   // every player appearing in any lineup,
    "Jahmyr Gibbs_DET": {             // frozen at build time so a later sim
      "name": "Jahmyr Gibbs", "team": "DET", "pos": "RB", "salary": 8000,
      "proj": 28.1, "gpp_proj": 38.6,
      "p25": 21.3, "p50": 28.1, "p75": 35.0, "p95": 47.4,
      "proj_adjust": 2.3, "ownership_pct": 18.0
    }
  },

  "lineups": [
    {
      "index": 0,
      "players": [ { "name": "...", "team": "...", "pos": "QB", "slot": "QB", "salary": 6800 } /* x9 */ ],
      "total_salary": 49800,
      "projected_score": 142.3,
      "ev_pct": 12.4, "itm_pct": 41.2, "top1_pct": 3.8, "top01_pct": 0.4,
      "lineup_p50": 138.1, "lineup_p75": 158.9, "lineup_p95": 191.2, "lineup_std": 22.4
    }
  ],
  "portfolio": {
    "eff_lineup_count": 17.1, "avg_correlation": 0.147,
    "coverage_score": 0.62, "total_ev_pct": 8.9
  },

  "submission": {                    // set when you confirm you entered this build
    "my_entry_count": 20,
    "lineup_indices": null,           // null = all lineups; or a subset
    "also_entered": [                 // optional: same lineups in other contests
      { "dk_contest_id": "199001234", "name": "NFL $50K Mini-MAX", "entry_fee": 5.0,
        "field_size": 11764, "max_entries_per_user": 20, "payout_structure": [ /* … */ ],
        "my_entry_count": 20 }
    ]
  }
}
```

The `lineups` / `portfolio` blocks are the `/api/optimize` response as-is.
`players_used`, `settings` (incl. the target `contest`), `overlay`, `slate` are
the inputs that produced it. Because the contest is chosen **before** building,
`submission` only has to capture "yes I entered it, N entries" plus any
secondary contests the same lineups went into.

### 3.4 `results.json` (Phase 4 — sketch only)

```jsonc
{
  "season": 2026, "week": 3, "ingested_at": "2026-09-24T12:00:00Z",
  "players": { "Jahmyr Gibbs_DET": { "dk_points": 31.2 } },
  "build_grades": {
    "20260921T144233Z": {
      "lineups": [ { "index": 0, "actual_score": 151.7, "field_percentile": 0.88, "cashed": true } ],
      "entries": [ { "dk_contest_id": "182734551", "gross_payout": 240.0, "net": -160.0, "roi_pct": -40.0 } ]
    }
  }
}
```

---

## 4. Backend

New module `src/api/optimizer_store.py` — thin file store, no ORM.

- `_week_dir(season, week)` → `BASE_DIR/data/optimizer/<season>/week_<NN>/`, created on write
- Atomic writes: write to `*.tmp`, `os.replace()` onto the target
- `read_state / write_state`
- `list_builds` (returns summaries: id, created_at, label, pinned, submitted,
  source, n_lineups, portfolio.total_ev_pct), `read_build`, `write_build`,
  `patch_build` (label/pinned/submitted/entries only), `delete_build`

### Endpoints (add to `src/api/app.py`)

| Method | Path | Body / notes |
|---|---|---|
| GET | `/api/optimizer/state?season=2026&week=N` | `{}` if none yet |
| PUT | `/api/optimizer/state?season=2026&week=N` | full state; debounced from FE |
| GET | `/api/optimizer/builds?season=2026&week=N` | list of summaries, newest first |
| GET | `/api/optimizer/builds/{id}?season=2026&week=N` | full build |
| POST | `/api/optimizer/builds?season=2026&week=N` | full build (server sets id/created_at) |
| PATCH | `/api/optimizer/builds/{id}?season=2026&week=N` | `{label?, pinned?, submitted?, submission?}` |
| DELETE | `/api/optimizer/builds/{id}?season=2026&week=N` | |
| POST | `/api/optimizer/prune?season=2026&week=N` | deletes autosave builds that are not pinned, labeled or submitted; returns what it removed |

`season` defaults to the sim year (2026). `week` required. Pydantic models mirror
the schemas in §3. No auth.

The frontend calls `POST /prune` for the **previous** week when you first switch
to a new week (with a confirm + a toast listing what was removed). Pinned,
labeled, and submitted builds are always kept.

---

## 5. Manual-edit semantics (the re-apply rules)

- **`projAdjust` is an additive delta vs the sim median.** Editing 16.9 → 17.2
  stores `+0.3`; the whole distribution (P25/P50/P75/P95, the 101-pt array, GPP
  blend, mean) slides +0.3. Clearing the field resets to 0.
  *(Already implemented this way in the frontend.)*
- **The delta rides along with sim re-runs.** If a Tuesday sim had a player at
  16.9 and Thursday's has him at 15.2, your saved `+0.3` now yields 15.5, not
  17.2. The adjustment represents *your standing disagreement with the model*,
  not a frozen target. **Flag if you'd rather it store the absolute.**
- **On load, overlay is matched to the current pool by `"{name}_{team}"`.**
  Entries with no matching player this week (bye, cut, traded, IR) are kept in
  `state.json` but not applied, and surfaced as a small
  "N saved overrides not in this slate" note so nothing silently disappears.
- **Builds freeze `players_used`** — a later sim re-run never rewrites a build.

---

## 5a. Sticky ownership

**Problem.** `/api/optimize` fills in ownership for any player lacking an explicit
value via `_compute_ownership`, seeded by a hash of every active player's
name/team/salary/**projection** ([app.py:3500](../../src/api/app.py)). So editing one projection
reshuffles the computed ownership for every not-yet-owned player, and the
portfolio/leverage/EV numbers drift run-to-run. Players the weekly field sample
did roster already have a stable `ownership_proj`; the churn is in the deep/punt
tier and anything the user has been editing.

**Fix — a "Freeze ownership for the week" toggle in the Player Pool / Builds area.**

- **On freeze:** snapshot the current Own% for **every** pool player into
  `overlay.players[id].ownershipPct`. Players with no live weekly value get a
  one-time `_compute_ownership` baseline (or a 0.5% floor) so they're not frozen
  at zero. Set `overlay.ownershipFrozen = true` + `ownershipFrozenAt`.
- **While frozen:** the optimize payload carries an explicit `ownership_pct` for
  every player → `_compute_ownership` no-ops → ownership (and therefore the
  portfolio math) is byte-stable across runs, regardless of projection edits.
- **Late news:** hand-edit the handful of players that moved in the Own% column;
  those writes persist in the overlay like any other override.
- **Unfreeze / re-sync:** clears the frozen snapshot; ownership reverts to the
  live weekly `ownership_proj` (picks up a fresh sim's numbers).
- Header shows the state: `Ownership: LIVE` vs
  `Ownership: FROZEN (Fri 6:14pm) · 3 hand-edits`.

This is just more overlay data — it persists in `state.json` and freezes into
builds automatically.

---

## 6. Frontend

### 6.1 Overlay refactor + sticky ownership (Phase 1)

- `playerPool` becomes **pure sim output** — never mutated by user actions.
- New `overlay` object (lifted to `App.jsx` next to `optimizerSettings`, or kept
  in `Optimizer` and persisted): `{ players: {id: {projAdjust, ownershipPct,
  locked, excluded}}, excludedTeams, excludedGames, gameExclusions,
  ownershipFrozen, ownershipFrozenAt }`.
- Setters (`setProjOverride`, `setOwnership`, `toggleLock`, `toggleExclude`,
  team/game exclusion handlers) write to `overlay`, not `playerPool`.
- `enrichedPool` merges `playerPool` + `overlay` (it already applies most of
  this; formalize and move exclusion state in).
- **Freeze ownership** control (see §5a): "Freeze for the week" snapshots every
  player's Own% into `overlay.players`; the optimize payload then always carries
  explicit ownership so `_compute_ownership` no-ops. Unfreeze reverts to live.
- **Result:** manual edits survive a sim refresh within a session, and the
  portfolio math stops drifting between runs. No change to optimize output for an
  unchanged overlay with ownership unfrozen.

### 6.2 Persistence (Phase 2)

- `useOptimizerPersistence(season, week)`:
  - on week change → `GET /state`, hydrate `settings` + `overlay`
  - on `settings`/`overlay` change → debounced (~1.5s) `PUT /state`
  - a small saved/saving indicator
- Each week is an independent document.

### 6.2a Contest picker stores identity (Phase 2)

`ContestPicker.applyContest` ([Optimizer.jsx:398](../../frontend/src/pages/Optimizer.jsx)) currently copies a
DK contest's *numbers* into settings but not its identity. Extend it to write a
`settings.contest` object (id, name, fee, field size, max entries/user, paying
positions, frozen payout table). This is the target contest the build is for.

### 6.3 Builds panel (Phase 3)

- New collapsible section (or tab) in the Optimizer: **Builds — Week N**.
- **Change-detection:** hash the optimize inputs (settings + overlay + slate).
  After `/api/optimize` returns, autosave a build **only if that hash differs
  from the most recent build's**. A pure re-run (same inputs, new RNG) does not
  create a build — the latest run is still viewable in-session, and "Save build"
  force-saves it if you want that RNG draw kept.
- `source: "autosave"` vs `"manual"` (the button).
- **"Auto-save runs" toggle** in the panel (persisted in `state.prefs`).
- List rows (newest first): time, editable label, lineup count, portfolio EV,
  ★ pin, ✓ submitted, ⟲ restore, 🗑 delete. Checkbox column + "Delete selected".
- **Restore-from-build:** confirm dialog → copies `build.overlay`,
  `build.settings` (incl. target contest), `build.slate` into working state
  (immediately `PUT /state`). Allowed across weeks — this is also how you clone
  last week's setup.
- **Record submission:** on a build, "I entered this" →
  - `my_entry_count`, optional lineup subset (default: all)
  - optional `also_entered` — pick other contests from the live list
    (`GET /api/dk/contests` + `/contest_payout`) that got the same lineups
  - sets `submitted: true` → `PATCH /builds/{id}`
- **End-of-week prune:** on switching to a later week, prompt to
  `POST /api/optimizer/prune` the prior week (keeps pinned / labeled / submitted).

---

## 7. Phasing & acceptance

| Phase | Deliverable | Acceptance |
|---|---|---|
| **1** ✅ | Overlay refactor + sticky ownership (FE only) | Edit a proj / lock / exclude, trigger a sim refresh in-session → edits persist. Freeze ownership, edit several projections, run Optimize twice → every player's ownership and the portfolio EV are identical across both runs. Unfreeze → ownership tracks the sim again. Optimize output unchanged vs. today for an unchanged overlay + unfrozen ownership. |
| **2** ✅ | `state.json` + endpoints + autosave/hydrate + contest identity | Configure a week (incl. picking a real DK contest), reload page → settings + overlay + frozen ownership + target contest restored. Each week is an independent file. Backend down → FE starts clean, resumes saving when it's back. *(Multi-week UI is gated by `/api/weeks`, still `[1]` — the per-week store is ready for more.)* |
| **3** ✅ | Builds: change-gated autosave, panel, label/pin/delete, restore, submission, prune | Verified: 1 Optimize → 1 build; identical re-run → still 1; edit a proj + re-run → 2. Pin persists to disk; prune removes throwaway autosaves and keeps pinned/labeled/submitted. Restore reverts the working state to the build's overlay/settings. `submission` is a ✓ toggle + entry-count via PATCH; `also_entered` deferred. Week-advance auto-prune is wired but untestable (only Week 1 in `/api/weeks`). |
| **4** | Review layer (separate plan) | — |

Phases 1–3 are the buildable unit here. Do 1, verify, then 2, then 3 —
check in between each.

---

## 8. Phase 4 sketch (for later)

- **Actual scores:** weekly player DK points from nflverse post-game stats
  (`nflreadr` player stats → DK scoring formula, which the sim calibration path
  likely already computes). Write `results.json` per week via a script.
- **Grading:** for each build, replay lineups against actual scores; if `entries`
  present, compute payout from the frozen `payout_structure` and field size →
  gross, net, ROI. Field percentile from the week's actual field score
  distribution.
- **Review page / report:** season table — builds made, submitted, ROI, ITM
  rate; projection calibration (did players hit `p50`?); overlay scorecard (mean
  outcome of your `projAdjust` bets vs. leaving the sim alone).

---

## 9. Decisions (resolved 2026-09-06)

1. **`projAdjust` = additive delta vs sim median, rides along with sim re-runs.** ✅
2. **Autosave is change-gated** — a build is written only when settings/overlay/
   slate changed since the last build; pure re-runs don't. Toggle to turn
   autosave off entirely; "Save build" always available. ✅
3. **Contest is chosen up front**, not recorded after — it's `settings.contest`,
   picked from the live DK list, and frozen into the build. A build targets one
   contest; `submission.also_entered` covers same-lineups-into-other-contests. ✅
4. **End-of-week prune** — on advancing to a new week, prompt to delete the prior
   week's autosave builds that aren't pinned, labeled, or submitted. ✅

### Resolved during build
- Sticky-ownership never-rostered players → **floor at 0.5%** (done).
- Restore-from-build carries that build's `settings.contest` (done).

### Known limitations (single-user scope)
- **Concurrent writes clobber.** Two clients (two tabs, or two dev sessions)
  both autosaving `state.json` last-write-wins each other. Fine for one
  user / one tab; add optimistic-concurrency (`If-Unmodified-Since` on
  `updated_at`) if multi-tab safety is ever needed.
- **Slate-id race**: an Optimize fired within ~1s of page load can hash
  `draft_group_id: null` before the slate settles, producing one extra build.
- **Optimize EV% is wildly inflated** (`+6000%`+) — a pre-existing bug in the
  `/api/optimize` EV math, now visible in the Builds panel's "Port. EV"
  column. Separate fix.
