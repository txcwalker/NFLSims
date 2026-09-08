# Phase 4 — API, Frontends & Publishing

**Scope:** `src/api/` + both React apps (flag-only per the plan) + a concrete
deploy-readiness assessment for the analytics site — Cam's focus area #4
("something my friends and family can visit at camanalytics.com").

---

## Part 1 — API (`src/api/app.py`, flag-only)

`app.py` is **3,999 lines**, ~35 endpoints, one file, no router split. Both
frontends hit the same app (`src.api.app:app` on :8000 and :8002 per
`launch.json`).

| # | sev | finding |
|---|---|---|
| S3-17 | S3 | **one 4k-line file.** Split into `routers/` (season, dfs, games, optimizer, live) — it's at the point where finding an endpoint is a scroll hunt. |
| S2-8 | S2 | **`/api/simulate` and `/api/optimize` have no iteration cap.** `SimulationRequest.iterations: int = 10000` with no `Field(le=…)`. One POST with `iterations: 10_000_000` hangs/OOMs the box. There's a response cache (repeat identical requests are free) but the first one still runs. `docs/api/production_migration.md` already flags this — the fix (`slowapi` 5/min + `Field(ge=1, le=10000)`) is a prerequisite for any public deploy that keeps these endpoints. |
| S3-18 | S3 | **no auth on any endpoint.** Acceptable for a friends-and-family read-only site *if* the compute endpoints are capped (S2-8) or disabled. The migration doc's JWT plan is for a later "premium" tier — not needed now. |
| S3-19 | S3 | **7 ESPN passthrough endpoints** (`/api/live-games`, `/api/games/{id}/play-by-play`, …) proxy ESPN's unofficial API live. In prod the server IP can get rate-limited/blocked by ESPN, and these 502 on any hiccup. Fine for now; needs a caching layer + backoff before it's load-bearing. |
| S3-20 | S3 | **`no_store_cache_headers` middleware sets `Cache-Control: no-store` on _every_ response**, not just `/api/*`. For a CDN-fronted deploy that kills caching of the static shell and the season JSON too. Scope it to `/api/` dynamic routes. |
| S3-21 | S3 | API loads the **65 MB players parquet + all XGBoost models into RAM** at startup and builds per-`game_id` dicts. This is the RAM cost that caps which host tiers work (Phase 3's DuckDB-over-parquet change removes most of it). |
| ✓ | — | `BASE_DIR` resolved from `__file__` (not CWD) — correct, unlike the sim runners. Health checks (`/health`, `/api/health`) and env-var CORS already exist. |

---

## Part 2 — Frontends (flag-only)

### S2-9 — the analytics site has no way to point at a production API

| | DFS site (`frontend/`) | Analytics site (`frontend_analysis/`) |
|---|---|---|
| API base | `import.meta.env.VITE_API_BASE_URL ?? 'http://127.0.0.1:8002/api'` ✅ | **`const API_BASE = '/api'`** — hardcoded |
| vite proxy | reads `VITE_API_BASE_URL` ✅ | **hardcoded `http://127.0.0.1:8000`** |

The DFS site already has the env-var pattern from the migration doc. The
analytics site assumes the API is same-origin at `/api` — which only works if a
reverse proxy sits in front of both. **This is the #1 code blocker for
publishing the analytics site** and it's a ~20-line fix (copy the DFS site's
`api.js` + `vite.config.js` pattern).

### S2-10 — `safeFetch` silently serves mock data on any API failure

```js
catch (err) { console.warn(`... Using mock sandbox fallback.`, err); return fallbackData; }
```

If the API is down / misconfigured / returns 500, the site renders the fallback
with only a `console.warn`. Scoped impact:

- **Season pages** (standings, team stats, leaders) fall back to `[]` / `{}` →
  empty tables. Ugly but not misleading.
- **Game Center / Live WP / 4th-down** fall back to `MOCK_PLAY_BY_PLAY`,
  `MOCK_FOURTH_DOWNS`, `MOCK_CHESS_EVALUATOR` → **a friend sees Mahomes-to-Kelce
  fake plays and fabricated numbers as if real.**

For a public deploy this needs a visible "data unavailable / reconnecting"
state, not a silent swap. Keep the mock path for local dev behind an explicit
`import.meta.env.DEV` check.

### S2-11 — `/api/fourth-down-evaluate` is called by the analytics site but doesn't exist in the API

`frontend_analysis/src/api.js:272` POSTs to `${API_BASE}/fourth-down-evaluate`.
No such route in `app.py` (the real one is `/api/positional-evaluator` /
`/api/games/{id}/positional-eval`). So the **4th Down Explorer's "input your own
parameters" sandbox** — a headline feature on the home page — 404s and shows
mock results (S2-10). Broken feature, masked by the silent fallback.

### S3-22 — pre-launch cruft self-flagged in the code

- `pagesConfig.js`: the **Testing Lab** page's own description ends *"Remove
  before launch."*
- `historical-lab` / `BotFeed` are internal-facing pages that shouldn't ship in
  the friends-and-family build.

### S3-23 — verify frontend lockfiles + dep currency

`package.json` pins React 19.2 / Vite 8 / ESLint 10 (very recent). Confirm
`package-lock.json` is committed for both apps and `npm audit` is clean before a
public build. (Not checked in this pass — flag for the fix-pass.)

---

## Part 3 — CI

Only two workflows, both for the 4th-down live bot (`nfl_live.yml` scheduled
game-window scraping, `nfl_manuel.yml` manual test). **Nothing runs the 97-test
suite on push/PR**, and there's no build/deploy pipeline.

**S3-24:** add a `ci.yml` — `pytest tests/` + `npm run build` for both apps on
every push. Cheap insurance; the audit is about to land a batch of engine
changes and there's currently no gate catching a broken commit.

---

## Part 4 — Publishing the analytics site to camanalytics.com

### What the site actually serves (three tiers)

| tier | pages | backing | update cadence |
|---|---|---|---|
| **Static season data** | Home, Full Standings, 2026 Season | 5 files: `season_summaries_2026.csv`, `team_stats_2026.csv`, `season_leaders_2026.json`, `matchup_win_probabilities_2026.json`, `teams_data.json` | whenever Cam re-runs the season sim |
| **Live compute** | 4th Down Explorer (sandbox), Game Center next-play | Python engine + models in RAM | real-time |
| **ESPN passthrough** | Live Win Probability, live 4th-down feed | proxies ESPN | only during live games |

### Good news

`docs/api/production_migration.md` already lays out a sound plan — subdomain
strategy, CORS lockdown, Let's Encrypt/Cloudflare HTTPS, `slowapi` rate
limiting, env-var API URL, health checks. It's not stale; it's a to-do list.

### Options

**Option A — static-only, ship this month**

Build `frontend_analysis` pointing at the 5 season JSON files bundled as static
assets (or served from the same static host). Deploy the whole thing to
**Cloudflare Pages / Netlify / GitHub Pages** (all free, global CDN, automatic
HTTPS). Hide the live-compute and ESPN pages for v1.

- **Cost:** ~$10/yr for the domain, $0 hosting.
- **Effort:** ~1–2 days. Blockers below.
- **Gets you:** "here is my model's full 2026 season projection" — standings,
  playoff odds, every team's page, league leaders, matchup win probabilities.
  That's the shareable story.
- **Loses:** the interactive sandbox tools (which are cool but half-broken right
  now anyway — S2-11) and live-game tracking. Data only updates on redeploy.

**Blockers for A:**
1. S2-9 — env-var API base (or switch season calls to fetch static JSON
   directly)
2. S2-10 — visible error state instead of silent mock
3. a build step that copies the 5 season files into the deployed bundle (or a
   tiny `data.json` manifest) — new, ~30 lines
4. `pagesConfig.js` — gate Live WP / 4th Down Explorer / Testing Lab / BotFeed
   out of the production build
5. buy `camanalytics.com`, point DNS at Cloudflare Pages

**Option B — static frontend + a small always-on API, at season start**

Frontend on Cloudflare Pages (free). API on **Fly.io / Render free tier** or a
**$5/mo VPS** (Hetzner/DigitalOcean). Brings back the sandbox tools and
weekly-updating data.

- **Cost:** $10/yr domain + $0–5/mo.
- **Effort:** ~1 week, and it needs: S2-8 (iteration cap + `slowapi`), S2-11
  (build the missing endpoint), the Phase 3 DuckDB change (so the API fits in
  512 MB), a deploy pipeline (S3-24), ESPN response caching (S3-19).
- Free tiers cold-start (Render spins down after 15 min idle); a $5 VPS avoids
  that.

**Option C — the `serverless_parquet_datalake.md` plan**

Frontend static, parquets on Cloudflare R2, FastAPI uses DuckDB HTTP range
requests. Best long-term (this is also the DFS-weekly plan), more upfront
setup. Fold into B rather than doing separately.

### Recommendation

**A now, B at season start (Sept 2026).**

Ship the season projection as a static site this month — it's the
lowest-risk, lowest-cost way to get `camanalytics.com` live and shareable, and
it sidesteps every security/DoS/RAM concern. Then when real games start and the
weekly refresh matters, stand up the small API (Option B, with the Phase 3
DuckDB work already done) to bring back the live tools.

Do **not** build the interactive tools into the public site until S2-11 is
fixed and S2-8 is capped — a friend clicking "4th Down Explorer" today gets mock
numbers.

---

## Fix-pass items from Phase 4

| # | sev | item |
|---|---|---|
| S2-8 | S2 | iteration cap (`Field(le=10000)`) + `slowapi` on `/api/simulate`, `/api/optimize` |
| S2-9 | S2 | analytics frontend: env-var API base (copy DFS site's pattern) |
| S2-10 | S2 | replace silent mock fallback with a visible error state (keep mock behind `import.meta.env.DEV`) |
| S2-11 | S2 | build the missing `/api/fourth-down-evaluate` endpoint (or repoint the frontend to the real one) |
| S3-17 | S3 | split `app.py` into routers |
| S3-20 | S3 | scope `no-store` to `/api/` only |
| S3-22 | S3 | gate Testing Lab / Live WP / BotFeed out of the prod build |
| S3-24 | S3 | `ci.yml` — run `pytest` + `npm run build` on push |
| — | — | (publishing itself is a post-audit project, not a fix-pass item) |
