# Implementation Plan — Sim vs. Market Comparison Page

**Status:** 🅿️ PARKED (2026-09-06) — Cam's call. The game-block sim-vs-Vegas
comparison (Season2026 Matchups tab + DFS Simulator) covers the near-term need.
Revisit the multi-book odds feeds and prediction-market integration below when
there's appetite for a paid odds API and account setup. Nothing here is started.
**Author:** drafted with Claude, 2026-09-06
**Roadmap slot:** Frontend / analytics tooling (post-DFS-site MVP). Candidate line item for `FRONTEND_GOALS.md`.

---

## 1. Goal

A single page that puts our simulation's game-level outputs next to the betting
market and prediction markets, and surfaces **where our numbers disagree with the
consensus and whether that disagreement has historically been right**.

Concretely, for every game on a slate:

| Field | Sim | Consensus book | Book range | Prediction market |
| :-- | :-- | :-- | :-- | :-- |
| Spread (home-relative) | `CAR +5.7` | `CAR +2.5` | `+2.0 … +3.0` | — |
| Total | `48.1` | `46.5` | `46.0 … 47.0` | — |
| Home win % | `35.4%` | `41%` (no-vig) | — | `38%` (Kalshi) |
| Home ML | `+180` | `+124` | `+118 … +130` | `+164` (implied) |
| **Edge** | **+3.2 pts home / model likes the dog** | | | |

Plus an aggregate view: our calibration and ROI-if-bet against each source over
the games that have since been graded.

The framing decision is already settled elsewhere in the codebase (see
`frontend/src/bettingLines.js`): **all spreads are stated from the home team's
side**, negative = home favored.

---

## 2. Data sources — the real decision

We currently ingest exactly **one** line per game: `data/external/schedule_2026.csv`
carries `spread_line`, `total_line`, `away_moneyline`, `home_moneyline`,
`away_spread_odds`, `home_spread_odds`, `over_odds`, `under_odds` — a single
consensus snapshot from nflverse (updates a few times a week, ~52/272 games
populated this far out). That is enough for a "sim vs. one market number" page but
not for "compare against common sportsbooks and prediction markets".

### Option A — Consensus line only (no new feeds)
- **Build cost:** low. Backend endpoint + page, reuses existing schedule data.
- **Limitation:** no book-to-book spread, no prediction markets, no line-movement
  history, no closing-line value (the single most useful backtest metric).
- **Verdict:** good as **Phase 1** to ship the page shell and the edge math.

### Option B — Add one multi-book odds feed
- **Candidate:** The Odds API (`the-odds-api.com`). Free tier = 500 req/month;
  `$` tiers for more. Returns per-book spreads/totals/MLs for NFL, plus historical
  snapshots on paid tiers.
- **Alternatives:** OddsJam (expensive, pro), SportsGameOdds, scraping (fragile,
  ToS risk — not recommended).
- **Build cost:** medium. New ingestion job (`scripts/data_ingest/fetch_odds.py`),
  a store for snapshots, API-key management (`.env`, never committed).
- **Payoff:** book range, consensus vs. best-price, and — if we snapshot on a
  schedule — line movement and closing-line value.

### Option C — Books + prediction markets
- **Kalshi** — regulated US event-contract exchange, has NFL game markets, clean
  REST API, needs an account + API key. Prices are true probabilities (no vig).
- **Polymarket** — crypto-settled, NFL markets exist, GraphQL/REST via the CLOB
  API. No account strictly needed to read. Liquidity varies by game.
- **Build cost:** high. Two more clients, each with its own market-ID mapping to
  our `game_id`, plus liquidity/last-trade filtering so a stale thin market
  doesn't look like a signal.
- **Payoff:** the fullest picture, and prediction markets are often sharper than
  books on win probability for high-profile games.

### Recommendation
Phase in: **A → B → C**. Ship A behind the nav, add B once we have an API key and
a cron slot, treat C as a follow-up. Each phase is independently useful and the
page layout is designed once to accommodate all three.

---

## 3. Data model

New table / cache: **`market_lines`** — one row per (game_id, source, captured_at).

```
game_id            TEXT      2026_01_CHI_CAR
source             TEXT      'nflverse' | 'oddsapi:draftkings' | 'kalshi' | 'polymarket' | 'sim'
captured_at        TIMESTAMP snapshot time (UTC)
home_spread        REAL      home-relative points (negative = home favored)
spread_odds_home   INTEGER   American (nullable)
spread_odds_away   INTEGER
total              REAL
total_odds_over    INTEGER
total_odds_under   INTEGER
home_ml            INTEGER   American
away_ml            INTEGER
home_win_prob      REAL      0..1 — de-vigged for books, raw for markets/sim
is_closing         BOOLEAN   set on the last snapshot before kickoff
```

- The **sim** is just another `source` in the same table, written by a small
  exporter that reads the season-sim games parquet (same numbers the Season2026
  Matchups tab and the DFS rails now show).
- De-vig method for books: normalize the two-way implied probs
  (`p_home / (p_home + p_away)`), i.e. proportional/"multiplicative" de-vig.
  Note the method in the UI; power/Shin de-vig can come later.
- Storage: start as a parquet/JSON cache under `data/external/` written by the
  ingestion job (consistent with how `schedule_2026.csv` and the sim caches
  already work). Move to the project DB only if snapshot volume warrants it.

---

## 4. Backend

- `scripts/data_ingest/fetch_odds.py` — pulls the configured sources, appends
  snapshot rows to the cache. Idempotent per (game_id, source, captured_at).
  Wired into the weekly refresh job alongside the existing DNA refresh.
- `GET /api/market_comparison?week=N` in `src/api/app.py` — returns, per game:
  the sim row, the latest snapshot per source, the book min/max, and the
  precomputed edges. Same mtime-gated cache pattern as the other 2026 endpoints.
- Edge math (server-side so the page and any future alerting agree):
  - `spread_edge = sim.home_spread - consensus.home_spread` (pts; sign = which
    side the model favors)
  - `total_edge  = sim.total - consensus.total`
  - `winprob_edge = sim.home_win_prob - source.home_win_prob`
  - `ml_value`: expected value of a 1-unit bet at the book's ML given the sim's
    win prob — `p_sim * (payout) - (1 - p_sim)`.

## 5. Grading & backtest (the part that answers "where do we line up best")

- After a game finalizes, compute per source:
  - **ATS result** vs. that source's spread, **total result** vs. its total,
    **ML result**.
  - **Closing-line value**: did our earlier disagreement move toward us by close?
  - **Brier score** of the win-prob prediction.
- Aggregate page section: for the season to date, per source —
  `n games`, `sim ATS record when edge ≥ X`, `ROI at -110`, `Brier(sim) vs Brier(source)`,
  a calibration plot (predicted vs. realized win rate, decile bins).
- This needs `is_closing` snapshots to be meaningful, which is the main argument
  for Option B/C over A.

## 6. Frontend

New page `frontend_analysis/src/pages/MarketComparison.jsx` (analytics site — it
already has the Season2026 sim data and the nav room; the DFS site links to it).

- **Slate grid** (default): one card per game, sim line vs. consensus vs. book
  range vs. prediction market, edge chips color-coded by size and direction.
  Reuses `bettingLines.js` formatters; promote those to a shared location if the
  DFS site needs them too (today they live under `frontend/src/`).
- **Game detail** (expand a card): all snapshots for that game as a small
  line-movement chart (once B is in), the de-vig math shown explicitly, the
  EV-per-bet table.
- **Season scorecard** tab: the aggregate/backtest section from §5, with the
  calibration plot (Recharts, already a dependency).
- Empty/partial states: most weeks this far out have no book line — show the sim
  column always, dash the rest, never block the page on missing market data.

## 7. Phasing

| Phase | Scope | Depends on |
| :-- | :-- | :-- |
| 1 | `market_lines` schema, sim exporter, `/api/market_comparison`, slate grid page vs. the nflverse consensus line only, edge chips | nothing new |
| 2 | The Odds API ingestion, snapshot history, book min/max, line-movement chart, closing-line capture | API key, cron slot |
| 3 | Season scorecard + calibration/ROI backtest | Phase 2 running for several weeks |
| 4 | Kalshi + Polymarket sources, prediction-market column, liquidity filtering | market-ID mapping work |

## 8. Open questions for Cam

1. **Budget for a paid odds feed?** The Odds API free tier (500/mo) is ~16
   pulls/day — enough for 1-2 snapshots/day across a slate, not enough for
   fine-grained line movement. Paid starts around $30/mo.
2. **Which books matter to you?** DK/FD/MGM/Caesars consensus, or a specific one
   you actually bet?
3. **Prediction markets — Kalshi, Polymarket, or both?** Kalshi needs an account;
   are you willing to set one up?
4. **Where does this live** — analytics site, DFS site, or both (shared page)?
5. **Backtest horizon:** just 2026 as it happens, or do we also replay the
   2025 season sim vs. historical closing lines for an immediate sample?
6. **Is closing-line value the primary success metric**, or straight ATS/ROI?
