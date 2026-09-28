# Implementation Plan — Prediction-Market Player Props (Phase 1: capture + grade)

**Status:** 📝 DRAFT for Cam's review (2026-09-26). A read-only **UI draft is live** (see §2), everything in §4 is
**not started** — waiting on sign-off.
**Author:** drafted with Claude, 2026-09-26
**Goal served:** "How are our sims doing vs. prediction markets on player props?" → later, a bet/prediction
recommender. Starts with **Polymarket US**; Kalshi and others slot in as additional venues (§8).
**Relation to older plans:** supersedes the prediction-market part of
[sim_vs_market_comparison_plan.md](sim_vs_market_comparison_plan.md) (parked 2026-09-06, game-level, written
before Polymarket US existed). Game lines stay on the existing Vegas-based Game Lines evaluation for now.

---

## 1. Verified facts (all checked live, 2026-09-26)

| Fact | Detail |
|---|---|
| Venue | **Polymarket US** (`gateway.polymarket.us`) — the CFTC-regulated exchange you can legally trade on (not available in NV, AZ, IL, MA, MD, MI, MT, OH). The global `polymarket.com` exchange is geoblocked for US trading; not used. |
| Auth for reading | **None.** Events, markets, quotes and price history are public. 20 req/s per IP. |
| API key | Only for trading/portfolio (Ed25519 key from polymarket.us/developer after in-app ID verification). **Not needed anywhere in Phase 1.** |
| Finding a game | Event slug = `nfl-{away}-{home}-{gameday}` built from our `schedule_2026.csv`; only the Rams differ (`LA` → `lar`). 16/16 week-3 games resolved. One call returns all ~750–880 markets for a game (~3.5 MB JSON). |
| Props format | Ladders of binary contracts: *"Will X record N+ receiving yards?"* (slug suffix `-gteN`). YES = stat ≥ N. ~4,200 player-prop rungs per week across 12 stat types we can price. |
| Quotes | `bestBidQuote` / `bestAskQuote` per contract. Pre-game props are **liquid**: median spread 1–3¢ on yards/receptions/TD/pass markets, 6–10¢ on attempts/fantasy/scrimmage. |
| Fees | Taker fee = `0.0695 × p × (1−p)` per contract (1.74¢ max at 50¢). Break-even ≈ 51.7% at a 50¢ price (vs. 52.4% at −110). Maker rebate `0.0125 × p × (1−p)`. |
| Price history | `GET /v1/price-history?symbol={market slug}` — public, **minute-level** with a custom `timestamp.startTimestamp/endTimestamp` range, and still served for **settled** markets (tested on ATL@GB, week 3 Thursday). Returns `longPrice` (≈ cost to buy YES) and `shortPrice` (≈ cost to buy NO). |
| Settlement rules | Player TDs exclude passing TDs; fantasy points = ESPN PPR; a player who doesn't take a snap settles at "last fair market price" (a void, effectively). Stat corrections after the game don't count. |

**Consequence that shapes the whole plan:** because price history is public and kept after settlement, the
**closing (pre-kickoff) price can be backfilled for any game, any time.** So grading doesn't depend on us
remembering to capture the book before kickoff. The capture ledger (P1.2) is only needed for *opening* prices
and line movement (CLV).

---

## 2. What already exists (UI draft, 2026-09-26)

Live, read-only, no disk writes:

| File | What it does |
|---|---|
| [src/data_pipeline/polymarket_us_client.py](../../src/data_pipeline/polymarket_us_client.py) | Slug builder + parallel event fetch (8 workers, backoff on 429/5xx). ~1 s for 16 games. |
| [src/evaluation/prop_markets.py](../../src/evaluation/prop_markets.py) | Normalizes player-prop markets → rows; prices every rung from `dfs_week_{W}_players.parquet` (P(stat ≥ N) over 10K iterations); after-fee EV of each side; sanity flags; coverage summary. |
| `GET /api/props/polymarket?week=&refresh=` in [src/api/app.py](../../src/api/app.py) | ~7 s cold, 5-min cache (refresh=true forces a new pull), re-prices when the week sim parquet changes. |
| [frontend/src/pages/PropMarkets.jsx](../../frontend/src/pages/PropMarkets.jsx) | The **Prop Bet Finder** tab (was a mock-data "in development" preview; now real data only). Coverage-by-stat table, per-player ladder rows, market-vs-sim probability curve + rung table per ladder, filters (game/stat/position/spread/pre-kickoff/⚠). **"How we're doing" card is intentionally an empty state** until P1.3–P1.5 exist. On API failure the page shows the error and no data (no mock fallback). |

Sim-pricable stats: pass yds / TD / att / comp, INT, rush yds / att, rec yds, receptions, scrimmage yds,
TDs (rush+rec), PPR fantasy points. **Not pricable yet:** first TD scorer, team first TD (sim doesn't record
TD order), longest reception (no per-play lengths kept), "most passing yards" head-to-head (doable, not built).

---

## 3. Issues surfaced while building the draft (need Cam's eyes)

1. **Negative receiving yards in the sim** — Elic Ayomanor (TEN, −10.0 yds on *every* catch in week 3),
   Eli Raridon (NE), Jahdae Walker (CHI) average negative yards/reception in the week 2 and 3 sims. That's a
   sim/roster-input bug, and it affects DFS projections too. Spun off as its own task.
2. **Backup QBs starting in the week 3 sims** — NYG Jameis Winston, WAS Marcus Mariota, SEA Drew Lock,
   CHI Tyson Bagent, while Polymarket lists full prop ladders for Dart / Daniels / Darnold / C. Williams.
   Might be correct injury overrides, might be stale `dfs_status_ledger` toggles. **Please confirm.**
3. **The biggest "edges" are usage disagreements, not value.** The top of the unfiltered list is players our sim gives
   near-zero volume (e.g. Terrance Ferguson: sim 10% vs. market 77% for 3+ receptions). This is exactly why
   the recommender must wait for grading (§7) and why ladders with any rung ≥ 30 pts off are hidden by default.
4. **~28% of TD-scorer rungs have no sim match** (fringe players / backups Polymarket lists but our week
   sim doesn't roster), plus 122 unmatched market players overall. The UI lists them; the plan below adds a
   manual name-override CSV for the real mismatches.

---

## 4. Phase 1 tasks

Each step is a separate, check-in-able unit. Tests are their own step (P1.0 / P1.6), per the working standard.

### P1.0 — Tests for the draft module *(before building on it)*
- `tests/fixtures/polymarket_us/` — two **real** saved event payloads, trimmed to the player-prop markets
  (one pre-game, one settled), so tests never hit the network.
- `tests/test_prop_markets.py`: `parse_threshold` (slug and label forms, decimal `gte12p5`), `taker_fee` /
  `side_edges` (hand-computed), ladder pricing on a tiny synthetic sim frame (incl. the "missing iteration = 0"
  rule), name matching (exact, last+initial, ambiguous → no match), team-code map (`lar` → `LA`), `sanity_flag`.

### P1.1 — Player identity map
- `data/eval/2026/pm_player_overrides.csv` (committed, hand-edited): `pm_player_id, pm_player, team, sim_player`.
  Consulted before the name matcher. Polymarket's `playerId` is its own ID (not GSIS), stable across weeks.
- UI: the "not found in our sims" list gets a copy-able CSV line per player.

### P1.2 — Snapshot ledger (opening prices + movement)
- `src/evaluation/market_ledger.py` → `append_market_snapshot(rows, year, week)`, `load_ledger(year, week)`.
- Storage: `data/eval/{year}/pm_snapshots/week_{NN}.parquet`, one row per (slug, captured_at), **written only
  when bid or ask changed** (same dedupe-on-change rule as `line_history.py`). Columns: slug, game_id, team,
  pm_player_id, stat, threshold, bid, ask, captured_at, venue.
- Trigger: every "Pull latest book" / cache refill of `/api/props/polymarket` appends (Cam runs refreshes by
  hand, no scheduler — same convention as the Vegas refresh). Optional script:
  `scripts/evaluation/capture_prop_markets.py <week>`.
- Size: ~4,200 rungs × ~40 B compressed ≈ 170 KB per full snapshot; ~20 snapshots/week with dedupe ≈
  **2–4 MB/week, ~50 MB/season**. **Gitignored** (the committed closing file in P1.3 is what grading needs).

### P1.3 — Closing-price backfill
- `scripts/evaluation/backfill_prop_closes.py <week> [--year 2026]` → for every *pricable* rung in the week's
  events, `GET /v1/price-history` over `[kickoff − 3h, kickoff]` at fidelity 1; keep the **last point at or
  before kickoff**: `close_yes = longPrice`, `close_no = shortPrice`, `close_mid = (longPrice + 1 − shortPrice)/2`.
  Also records the event's settlement status.
- Output: `data/eval/{year}/pm_closes.csv` (append per week; ~4,200 rows/week ≈ 250 KB/week, **committed** — same
  reasoning as `line_history.csv`: once Polymarket prunes history it can't be regenerated).
- Load: ~4,200 requests/week at ≤ 12 req/s (under the 20/s cap, with backoff) ≈ **6 min per week**; weeks 1–3
  in one ~20-minute run.
- **To verify first (15-minute spike):** (a) weeks 1–2 event slugs still resolve and still have history,
  (b) `longPrice`/`shortPrice` really equal ask / (1 − bid) — compare against live `bestAskQuote`/`bestBidQuote`
  on open markets at the same minute.

### P1.4 — Outcomes
- Reuse `src/evaluation/player_actuals.py` (nflverse `stats_player`, already cached). Map each stat to its
  actual: `passing_yards`, `passing_tds`, `attempts`, `completions`, `passing_interceptions`, `rushing_yards`,
  `carries`, `receiving_yards`, `receptions`; scrimmage = rush + rec yds; TDs = rush + rec + **special-teams**
  TDs (market counts return TDs; excludes passing); PPR = ESPN PPR incl. 2-pt conversions and fumbles lost.
- **No stat line → void** (the market's "didn't play" rule), never graded as a zero. Same policy as Player
  Projections.
- Cross-check: when Polymarket shows a market resolved, its settlement should match our computed YES/NO;
  mismatches get listed (they'd mean a stat-mapping bug on our side, or a stat correction).

### P1.5 — Grading module + endpoint
- `src/evaluation/prop_market_eval.py`:
  - `build_prop_eval(year, weeks)` → one row per graded rung: sim_p (from the **kickoff-locked** pre-game sim,
    `sim_run_at` < kickoff — already enforced by the sim writers), close_mid, close_yes/no, outcome, stat, player.
  - `summarize_prop_eval(df)` → per stat and overall: **Brier and log-loss, sim vs. market close** (+ skill
    score = 1 − sim/market), calibration buckets (reuse `game_line_eval._calibration`), paper P&L of
    "take the +EV side at the close price + fee when EV > X¢" at X ∈ {0, 2, 5}, by week.
  - **Correlation guard:** rungs on one ladder are nearly the same bet. Headline metrics use **one rung per
    ladder** (the rung whose close_mid is nearest 50%, i.e. "the line"); all-rungs numbers are secondary.
    Confidence intervals by bootstrapping over ladders, not rungs.
- `GET /api/eval/prop_markets` (cached on input mtimes, like the other eval endpoints) +
  `POST /api/eval/backfill_prop_closes?week=` (button-triggered).

### P1.6 — Tests for P1.1–P1.5
- `tests/test_prop_market_eval.py`: hand-computed Brier/log-loss/P&L on a 6-row fixture, void handling,
  one-rung-per-ladder selection, close-price extraction from a saved price-history payload, outcome mapping
  (esp. TD and PPR definitions).

### P1.7 — UI: fill in "How we're doing"
- Card → headline tiles (sim vs. market Brier, log-loss, graded ladders, paper result) — reuse `Tile` / `Empty`
  from `GameLinesEval.jsx`.
- By-stat table (who's better where), calibration chart (sim vs. market, same axes), weekly running line.
- Ladder rows on past weeks show the **actual** value as a marker on the probability chart + the settled rung.
- Evaluation tab gets a small "Prediction-market props" section linking here (or embeds the card).

### P1.8 — Docs
- AGENTS.md §1 file map + §7 test commands, DEVELOPMENT.md (new data files, endpoints), WORKLOG entry,
  `.gitignore` for `pm_snapshots/`. `requirements.txt` unchanged (uses `requests`, already pinned).

---

## 5. Costs, storage, memory

| Item | Estimate |
|---|---|
| Data cost | **$0** — public endpoints only. No paid odds/history vendor needed. |
| Trading fees (later) | 0.0695·p·(1−p) taker per contract; rebate tiers above $250K/month volume. |
| Committed storage | `pm_closes.csv` ≈ 250 KB/week → ~4.5 MB/season. Overrides CSV: KB. |
| Gitignored storage | Snapshot ledger ≈ 2–4 MB/week → ~50 MB/season. |
| Backend memory | Sim pricing reads 16 of the players parquet's 32 columns (~0.5 GB transient for a 4M-row week) — same order as Player Projections eval. Books: ~56 MB JSON transient per pull, parsed to ~4K rows. |
| Latency | Live page: ~7 s cold, instant cached. Close backfill: ~6 min/week (script/button, not per page load). |

---

## 6. Open decisions for Cam

1. **Confirm §3 items 1–2** (negative-yards bug, backup QBs) — both directly corrupt any grading.
2. **Capture cadence.** Manual refresh only (current convention) vs. a light scheduled capture (e.g. Tue / Thu /
   Sat / Sun-morning) for opening prices + CLV. Grading itself doesn't need it (closes are backfillable).
3. **Headline metric.** Proposed: one-rung-per-ladder log-loss, sim vs. market close. Alternative: Brier.
4. **Scope of stats.** Grade all 12 pricable types, or start with the liquid, high-volume ones (rec yds, receptions,
   rush yds, pass yds, TDs)? Recommend: all 12 in the data, headline on the liquid five.
5. **Backfill weeks 1–2?** Depends on the P1.3 spike; weeks 1–2 sims were run before some calibration fixes, so
   they're a slightly different model — label them as such or start the record at week 3.
6. **Game lines on Polymarket** (spreads/totals/ML ladders) — same pipeline would grade them for free alongside the
   Vegas ledger. In or out of Phase 1? (Recommend: Phase 1b, after props land.)

---

## 7. What comes after Phase 1 (not in scope)

- **Phase 2 — Recommender (read-only).** Only for stat types where graded history shows the sim adds
  information over the close. Probability = fitted blend of market and sim (logistic on both logits, weights
  from graded history — shrink toward the market by default), fractional-Kelly sizing capped per ladder/game,
  liquidity filter (spread ≤ 3–4¢ and book depth ≥ stake via the order-book endpoint). Output = a list; no orders.
- **Phase 3 — Execution (optional).** Polymarket US API key (stored in `.env` as `POLYMARKET_US_KEY_ID` /
  `POLYMARKET_US_SECRET`, never committed), `polymarket-us` SDK, limit orders (maker rebate instead of taker fee).
  Separate decision; needs Cam's explicit go-ahead.

---

## 8. Adding Kalshi and other venues

The draft already keeps venue-specific code in one client file and one normalizer. For each new venue:
1. `src/data_pipeline/{venue}_client.py` — event lookup from our schedule + market fetch.
2. A normalizer producing the **same row shape** (`venue, game_id, team, player, stat, threshold, bid, ask,
   fee_model`) plus the venue's fee function.
3. Close-price source (history endpoint, or our own ledger if the venue has no history).
Everything downstream (pricing, grading, UI) is venue-agnostic once rows carry a `venue` column, and the UI gets
a venue switch plus a "best price across venues" column. Kalshi's public market-data access and prop coverage
still need verifying — not checked yet.

---

## 9. Suggested order and size

| Step | Size | Can start without Cam? |
|---|---|---|
| P1.0 tests for draft | small | yes, once plan approved |
| P1.3 spike (weeks 1–2 history + long/short semantics) | 15 min | yes |
| P1.1 + P1.2 | 1 session | yes |
| P1.3 + P1.4 | 1 session | after spike |
| P1.5 + P1.6 | 1 session | after §6.3 decision |
| P1.7 + P1.8 | 1 session | — |

**Stop condition for Phase 1:** weeks 3+ graded end-to-end on the page, with sim vs. market log-loss by stat and
a paper result, tests green, docs updated. Then check in before any recommender work.
