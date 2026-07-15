# DFS Multi-Game Optimizer — Build Prompt

## Context & Codebase Overview

This is a React + Vite frontend (`frontend/src/`) backed by a Python FastAPI server (`src/api/`). The app already has:
- A **Simulator** page that runs Monte Carlo sims per game and stores results in `allSimResults` (keyed by game ID) and `simResults` (current game) at App.jsx level.
- A **DFS Summary** page (`DfsSummary.jsx`) that reads `allSimResults`, `simResults`, and `weekProjections` props to display per-player sim percentiles. It shows `dk_pcts_all` (a 101-element array indexed 0–100) and `dk_points` (median projected score) per player.
- A **`pagesConfig.js`** where the `optimizer` page already exists with `isDevelopment: true`. We are replacing that placeholder with a real page.
- **`App.jsx`** has lifted state: `allSimResults`, `simResults`, `weekProjections`, `generatedLineups`, `setGeneratedLineups`, `selectedWeek`, `selectedSlate`, `games`.
- **`api.js`** has mock sandbox fallback logic. If the Python backend is unreachable, it falls back gracefully to mock data.
- **`dfsRules.js`** defines `DK_TRADITIONAL` slot structure: `['QB', 'RB', 'RB', 'WR', 'WR', 'WR', 'TE', 'FLEX', 'DST']` with `flexEligible: ['RB', 'WR', 'TE']`.
- **Salary cap**: $50,000 (DraftKings). For this build use **fake salaries** from `allRosters.js` which already has `salary` fields per player.
- **Design system**: Dark glassmorphism theme. CSS variables `--bg-dark`, `--text-white`, `--accent-blue`, `--accent-green`, `--accent-red`, `--card-bg`, `--border-color`. Match the visual style of DfsSummary.jsx and Simulator.jsx exactly.
- **Team colors**: `TEAM_COLORS` object already defined in DfsSummary.jsx — reuse this same map in the Optimizer.

---

## Design Philosophy — Sims and Optimizer Are Separate Tools

**This is the most important architectural principle of the optimizer:**

The **Simulator** and the **Optimizer** are parallel tools that inform each other but do NOT depend on each other at runtime.

- The **Simulator** is a research tool. Use it to understand game flow, see score distributions under different scenarios (high total, blowout, shootout), and develop conviction on player outcomes. Its output — percentile distributions — provides *context* for your projections.

- The **Optimizer** is a production tool. It loads immediately with a default projection set and the user works from there. No sim needs to run. No waiting. The page is interactive the moment you navigate to it.

The user is responsible for the relationship between projections. If they bump JSN to 18 points because they believe it's a shootout, they understand they should probably also bump Darnold — because that game flow is why JSN scores 18. The optimizer does not enforce or auto-propagate these relationships at the player pool editing stage. That's the user's job.

### Default Projection Priority (page load — no waiting at any tier)

```
Tier 1: allSimResults (games already simulated this session)
         → Full data: projection, p25, p40, p50, p75, p95, mean
         → Percentile hotkey buttons fully active

Tier 2: weekProjections (pre-computed from backend API)
         → Median projection and salary only
         → Percentile columns shown but grayed out (no distribution data)

Tier 3: Mock roster data (allRosters.js) with synthetic baseline projections
         → Always available, no backend required
         → Projections generated from salary-weighted estimates
         → Percentile columns grayed out
```

The player pool populates from whichever tier is available — the optimizer **never triggers a sim run**. If a user wants richer percentile data, they go run the Simulator first, then come back. That's their workflow choice, not a hard requirement.

---

## What To Build

### 1. Remove the `isDevelopment` Flag from `optimizer` in `pagesConfig.js`

Change `isDevelopment: true` to `isDevelopment: false` for the `optimizer` entry. Keep all other fields the same.

---

### 2. Add `Optimizer` to `App.jsx`

Add two new pieces of lifted state:

```js
const [optimizerSettings, setOptimizerSettings] = useState(null);
const [optimizerLineups, setOptimizerLineups] = useState([]);
```

Add the `optimizer` case to `renderPageContent()`:

```jsx
case 'optimizer':
  return (
    <Optimizer
      allSimResults={allSimResults}
      simResults={simResults}
      weekProjections={weekProjections}
      games={games}
      optimizerLineups={optimizerLineups}
      setOptimizerLineups={setOptimizerLineups}
      optimizerSettings={optimizerSettings}
      setOptimizerSettings={setOptimizerSettings}
      setCurrentPage={setCurrentPage}
    />
  );
```

Import `Optimizer` at the top: `import Optimizer from './pages/Optimizer'`

---

### 3. Python Backend — New Optimizer Endpoint

**File**: `src/api/optimizer_endpoint.py` (or add to existing routes file, whichever pattern exists)

Add a POST endpoint: `POST /api/optimize`

**Request body**:
```json
{
  "players": [
    {
      "name": "Josh Allen",
      "team": "BUF",
      "pos": "QB",
      "salary": 7800,
      "projection": 24.5,
      "locked": false,
      "excluded": false,
      "ownership_pct": 22.4,
      "dk_pcts_all": [0, 2.1, 3.4, ..., 55.2]
    }
  ],
  "n_lineups": 20,
  "salary_cap": 50000,
  "contest_type": "top_heavy",
  "contest_size": 11000,
  "min_unique_players": 2,
  "include_dst_in_unique": false,
  "max_exposure": 0.40,
  "payout_structure": [
    {"rank": 1, "payout": 50000},
    {"rank": 2, "payout": 15000},
    ...
  ],
  "entry_fee": 18,
  "total_entries": 11437,
  "paying_positions": 2480
}
```

**Response**:
```json
{
  "lineups": [
    {
      "players": [
        {"name": "Josh Allen", "pos": "QB", "team": "BUF", "salary": 7800, "projection": 24.5, "slot": "QB"},
        ...
      ],
      "total_salary": 49800,
      "projected_score": 162.4,
      "ev_pct": 12.3,
      "itm_pct": 21.4,
      "top1_pct": 3.2,
      "top01_pct": 0.8,
      "portfolio_score": 0.0
    }
  ],
  "portfolio": {
    "total_ev_pct": 11.8,
    "effective_lineup_count": 14.2,
    "avg_correlation": 0.31,
    "coverage_score": 0.72
  }
}
```

#### Optimizer Logic (Python)

**Install dependencies**: `pulp`, `numpy`, `scipy`

**Algorithm**:

```python
import pulp
import numpy as np
from scipy.stats import norm

def run_optimizer(players, n_lineups, salary_cap, contest_type, min_unique,
                  include_dst_unique, max_exposure, payout_structure,
                  entry_fee, total_entries, paying_positions):

    # 1. BUILD CORRELATION MATRIX from dk_pcts_all distribution shape
    #    For each player, extract (p25, p50, p75, p95) from dk_pcts_all
    #    Use structural fallback if pcts are all equal (no sim run)
    #    Correlation groups:
    #      - QB ↔ WR/TE same team: +0.55 to +0.65 (scale by team pass volume)
    #      - QB ↔ RB same team: +0.15
    #      - QB ↔ opposing DST: -0.40
    #      - WR1 ↔ WR2 same team: +0.25
    #      - Cross-game players: ~0.0

    # 2. CHOLESKY DECOMPOSITION of covariance matrix
    #    sigma_i = (p75_i - p25_i) / 1.35  (IQR-based std dev estimate)
    #    cov[i,j] = rho[i,j] * sigma_i * sigma_j
    #    L = np.linalg.cholesky(nearest_positive_definite(cov))

    # 3. DISTRIBUTION WIDTH scaling by contest_type:
    #    cash: width_multiplier = 0.0 (deterministic — use projections exactly)
    #    flat: width_multiplier = 0.6
    #    top_heavy: width_multiplier = 1.0
    #    extreme_top_heavy: width_multiplier = 1.4

    # 4. FOR EACH LINEUP i = 1..n_lineups:
    #    a. Draw correlated scores:
    #       z = np.random.standard_normal(N)
    #       draw_scores = projections + width_multiplier * (L @ z)
    #       draw_scores = np.clip(draw_scores, 0, None)  # no negative scores
    #
    #    b. Solve ILP with PuLP:
    #       - maximize: sum(draw_scores[j] * x[j])
    #       - subject to:
    #           sum(salary[j] * x[j]) <= salary_cap
    #           sum(x[j] for j in QBs) == 1
    #           sum(x[j] for j in RBs) >= 2
    #           sum(x[j] for j in WRs) >= 3
    #           sum(x[j] for j in TEs) >= 1
    #           sum(x[j] for j in DSTs) == 1
    #           sum(x[j]) == 9  (total players)
    #           FLEX: any RB/WR/TE can fill the 9th non-DST/QB slot
    #           For locked players: x[j] == 1
    #           For excluded players: x[j] == 0
    #           Max exposure: for each player j,
    #             (total appearances so far) / n_lineups <= max_exposure
    #           Min unique: for each prior lineup k,
    #             sum(x[j] for j in lineup_k) <= 9 - min_unique
    #
    #    c. Assign slots in canonical order: QB, RB, RB, WR, WR, WR, FLEX, DST
    #       The FLEX slot gets whichever eligible player (RB/WR/TE) wasn't
    #       placed in a primary slot — always exactly 1 per lineup.

    # 5. COMPUTE LINEUP STATS:
    #    For each lineup, run 10,000 Monte Carlo score draws (fast — just numpy):
    #    - Draw from each player's distribution (using their dk_pcts_all as empirical CDF)
    #    - Sum lineup scores across draws
    #    - Compare against field score distribution (estimate from all-player sim data)
    #    - ITM% = P(lineup_score > field_cutoff_19pct)
    #    - Top1% = P(lineup_score > field_cutoff_1pct)
    #    - Top0.1% = P(lineup_score > field_cutoff_01pct)
    #    - EV% = sum over payout_structure of P(finish at rank k) * payout_k / entry_fee - 1

    # 6. COMPUTE PORTFOLIO STATS:
    #    - Pairwise similarity: for each pair (i,j), shared_players / 9
    #    - Portfolio correlation: average pairwise similarity weighted by EV
    #    - ELC (Effective Lineup Count): 1 / sum(w_i^2) where w_i = 1/n_lineups
    #      Note: ELC scales with diversity — fully diverse = ELC ≈ n_lineups
    #    - Portfolio Score per lineup: marginal improvement to portfolio EV
    #      when this lineup is added vs. removed from the book
    #    - Coverage score: fraction of "game outcome buckets" covered
    #      (a bucket = a team's top scorer going above their p75)

    pass
```

#### Ownership Model (computed server-side before optimization)

```python
def compute_ownership(players):
    # Step 1: value score
    POS_WEIGHTS = {'QB': 0.85, 'RB': 1.3, 'WR': 1.0, 'TE': 0.9, 'DST': 0.65}
    for p in players:
        p['value_score'] = (p['projection'] / (p['salary'] / 1000)) * POS_WEIGHTS[p['pos']]

    # Step 2: chalk boost — top 10% by value_score get 1.4x
    threshold = np.percentile([p['value_score'] for p in players], 90)
    for p in players:
        if p['value_score'] >= threshold:
            p['value_score'] *= 1.4

    # Step 3: softmax normalization (T=1.5)
    T = 1.5
    vals = np.array([p['value_score'] for p in players])
    exp_vals = np.exp(vals / T)
    softmax = exp_vals / exp_vals.sum()
    # Scale so sum = 900 (9-player lineups, 100% ownership each)
    raw_ownership = softmax * 900

    # Step 4: log-normal noise
    for i, p in enumerate(players):
        base = raw_ownership[i]
        sigma = 0.25 if base > 25 else (0.35 if base > 8 else 0.45)
        p['ownership_pct'] = float(base * np.exp(np.random.normal(0, sigma)))

    return players
```

---

### 4. Frontend — `Optimizer.jsx` Page

**File**: `frontend/src/pages/Optimizer.jsx`

This page has **two internal views** toggled by a state variable `view`:
- `'pool'` — Player pool, settings, and optimize button
- `'results'` — Generated lineups display

#### 4a. Player Pool View (`view === 'pool'`)

**Layout**: Two-panel layout. Left panel: settings sidebar. Right panel: player pool table.

##### Left Sidebar — Settings

```
┌─────────────────────────────┐
│  🏈 DFS Optimizer            │
│  Multi-Game Slate            │
├─────────────────────────────┤
│  CONTEST SETTINGS           │
│  Platform: [DK] [FD]        │
│  Contest Type:              │
│    ○ Cash                   │
│    ○ Flat Tournament        │
│    ○ Top-Heavy Tournament   │
│    ● Extreme Top-Heavy      │
│  Contest Size: [_____]      │
│  Entry Fee: $[___]          │
│  Payout Positions: [___]    │
│  Total Entries: [_____]     │
├─────────────────────────────┤
│  LINEUP SETTINGS            │
│  # Lineups: [___] (max 1000)│
│  Min Unique Players: [2]    │
│  ☐ Include DST in Unique    │
│  Max Exposure: [40]%        │
├─────────────────────────────┤
│  FILTERS                    │
│  Hide players below [___] pts│
│  Position: [ALL▼]           │
│  Team: [ALL▼]               │
│  Game: [ALL▼]               │
├─────────────────────────────┤
│  [⚡ OPTIMIZE]               │
└─────────────────────────────┘
```

- Contest types map to payout templates (see Payout Templates below)
- Salary cap is fixed at $50,000 for DK Traditional
- # Lineups input: integer 1–1000
- Min Unique Players: integer 1–8, default 2
- Max Exposure: 10–100%, default 40%

##### Right Panel — Player Pool Table

Columns (left to right):
```
Lock | Excl | Player Name | Pos | Team | Game | Salary | Proj | P25 | P40 | P50 | P75 | P95 | Mean | Proj Edit | Ownership%
```

- **Lock (🔒)**: Toggle button. Locks player into 100% of lineups. Yellow highlight when active.
- **Excl (✕)**: Toggle button. Removes player from pool. Grays out the row.
- **Player Name**: With team color dot indicator.
- **Pos**: QB / RB / WR / TE / DST badge.
- **Team**: Team abbreviation.
- **Game**: "BUF@KC" format (away@home).
- **Salary**: `$7,800` formatted.
- **Proj (projection)**: Editable number field. Pre-filled from sim median (`dk_pcts_all[50]` if available, else `dk_points`). This is the user's working projection — their "new median" for optimizer draws.
- **P25 / P40 / P50 / P75 / P95**: Read-only display from `dk_pcts_all`. These are **hotkey buttons** — clicking any of them sets the Proj field to that percentile value.
  - Style: small pill buttons, default neutral. Clicking updates `Proj` for that player.
- **Mean**: Read-only, from `dk_points` (the sim mean projection).
- **Ownership%**: Computed server-side from ownership model. Editable — user can override.

**Projection input color coding** (based on how far above sim median the user has set it):
- At or below p50: neutral (white/gray)
- p50 to p65: `#f5c542` yellow tint on the input background
- p65 to p75: `#f59e0b` orange tint
- Above p75: `#ef4444` red tint + tooltip: "Above p75 — aggressive projection"

**Table header features**:
- Click column headers to sort ascending/descending
- Default sort: by Proj descending
- Filter row below header: search by player name

**Eliminated players**: Excluded players are grayed out in-place with strikethrough text. They are NOT removed from the table — user can click ✕ again to restore them. Add a count badge: "3 players excluded" above the table if any are excluded.

**Eliminated teams / games**: Add team/game filter dropdowns in the sidebar. Selecting "Exclude" for a team excludes all players on that team. Selecting "Exclude" for a game excludes all players from both teams (including both DSTs). Excluded teams/games are shown with a badge in the filter area and can be removed.

#### 4b. Results View (`view === 'results'`)

Automatically shown after optimization completes. Back button returns to pool view (preserves all pool settings and adjustments).

##### Portfolio Summary Panel (top)

```
┌──────────────────────────────────────────────────────────────┐
│  PORTFOLIO SUMMARY               [← Back to Pool]           │
│  20 Lineups Generated                                        │
│                                                              │
│  Portfolio EV%: +14.2%   ELC: 16.8/20   Avg Correlation: 0.28│
│  Coverage Score: 74%                                         │
└──────────────────────────────────────────────────────────────┘
```

- ELC displayed as `{value}/{n_lineups}` to make the scale clear
- Color coding: ELC > 80% of n_lineups = green; 60–80% = yellow; < 60% = red

##### Lineup Table

Each lineup is a row. Left to right:

```
#  | EV%  | Portfolio Score | ITM% | Top1% | Top.1% | QB | RB | RB | WR | WR | WR | FLEX | DST | Score | Salary
```

- **#**: Lineup number (1-indexed)
- **EV%**: Expected value as % of entry fee. Color: positive = green, negative = red.
- **Portfolio Score**: `+X.X%` — how much this lineup improves portfolio EV vs. removing it. Color coded green/red.
- **ITM%**: Probability of cashing (finishing in paying positions). Show as `21.4%`.
- **Top1%**: Show as `3.2%`.
- **Top.1%**: Show as `0.8%`.
- **Player slots (QB through DST)**: Show player last name only + team color dot. Display in canonical order: QB, RB, RB, WR, WR, WR, FLEX, DST.
- **Score**: Projected total lineup score (the median expected output, not the draw score used to build it).
- **Salary**: Total salary used. Show remaining cap in small text below: `$200 rem`.

**Row color tinting**:
- Top quartile of EV%: subtle green tint on row background
- Bottom quartile: subtle red tint
- Middle 50%: no tint

**Expandable row**: Clicking any lineup row expands it to show a detail card beneath:
```
Player details: Name | Pos | Team | Salary | Projection | Percentile Used | Ownership%
Slot breakdown: exactly which slot each player fills (QB, RB1, RB2, WR1, WR2, WR3, FLEX, DST)
```

**Sort**: Clicking any column header sorts the lineup table by that metric.

**Export button**: `[📥 Export CSV]` — downloads all lineups as a CSV with columns:
`QB, RB, RB, WR, WR, WR, FLEX, DST` (player names), `Salary`, `Projected Score`

---

### 5. Payout Structure Templates

Pre-load these three templates based on contest type selection. User can view and edit the payout table if needed.

#### Template 1: Extreme Top-Heavy (Bat Flip / Millionaire Maker style)
- 1st: 28.6% of prize pool
- 2nd: 8.6%
- 3rd: 5.7%
- 4th: 2.9%
- 5th: 1.4%
- 6th–7th: ~0.75%
- 8th–10th: ~0.45%
- 11th–25th: steps down from 0.3% to 0.14%
- Remaining paying positions: flat minimum (entry fee recovery tier)
- Paying positions: ~21% of field

#### Template 2: Top-Heavy (Rally Cap style)
- 1st: 20% of prize pool
- 2nd: 9.3%
- 3rd: 4.7%
- 4th: 2.7%
- 5th: 1.3%
- 6th–8th: steps 0.93% → 0.53%
- 9th–20th: steps 0.4% → 0.2%
- Remaining: flat minimum
- Paying positions: ~23% of field

#### Template 3: Flat (Home Plate style)
- 1st: 10.7% of prize pool
- 2nd: 7.1%
- 3rd: 5.7%
- 4th: 4.3%
- 5th–6th: 2.9% → 2.1%
- 7th–10th: 1.7% → 1.4%
- 11th–30th: steps down to 0.75%
- Remaining: flat minimum
- Paying positions: ~23% of field

#### Cash (50/50 / Head-to-Head)
- Top 50% of field wins 1.9× entry fee (after site rake)
- No stochastic draws needed — use deterministic projection-only ILP
- Only 1 lineup generated regardless of n_lineups setting

---

### 6. Field Score Distribution (for EV/ITM/Top% calculations)

Estimate the field's score distribution from available sim data:

```python
def estimate_field_distribution(players, paying_pct=0.19):
    # Build 10,000 simulated "field lineups" using random valid lineups
    # weighted by ownership percentage
    # Each field lineup score = sum of 9 player draws
    # Use the player dk_pcts_all distributions for draws
    # Fit a normal distribution to the resulting field lineup scores
    # field_mean ≈ typical DK score for a random field lineup
    # field_std ≈ spread of field lineup scores
    # cutoff_itm = field score at the (1 - paying_pct) quantile
    # cutoff_top1 = field score at the 0.99 quantile
    # cutoff_top01 = field score at the 0.999 quantile
    pass
```

---

### 7. Mock / Sandbox Fallback

Add a mock optimizer response to `api.js` for when the Python backend is unreachable:

```js
async optimizeLineups(payload) {
  // Mock: generate N lineups from the provided players using a simple
  // greedy salary-cap approach (no stochastic draws in mock mode)
  // Return realistic-looking lineup objects with plausible EV/ITM stats
  return safeFetch(`${API_BASE}/optimize`, {
    method: 'POST',
    body: JSON.stringify(payload)
  }, generateMockLineups(payload));
}
```

The `generateMockLineups` function should produce valid DK Traditional lineups (1 QB, 2 RB, 3 WR, 1 TE, 1 FLEX, 1 DST) from the player pool, respecting salary cap and position constraints. Use greedy selection with slight randomization across lineups to simulate diversity. Generate plausible EV%, ITM%, Top1%, Top0.1% values based on projected score.

---

### 8. Ownership Model — V1 Synthetic (computed backend, used in EV calculations)

See `docs/frontend/ownership_model.md` for full spec. Summary:

```python
# 1. value_score = (projection / (salary/1000)) * POS_WEIGHT[pos]
# 2. Top 10% by value_score: apply 1.4x chalk boost
# 3. Softmax with T=1.5, scaled to sum=900
# 4. Log-normal noise: sigma = 0.25 (high-owned), 0.35 (mid), 0.45 (low)
# Result: ownership_pct per player, realistic distribution
```

Ownership is displayed in the player pool table and is editable. If the user overrides a player's ownership, that value is used in EV calculations instead of the model-computed value.

---

### 9. Style Requirements

- Match the existing dark glassmorphism theme throughout. Reference `DfsSummary.jsx` and `Simulator.jsx` for colors, card styles, and table styles.
- Use the same `TEAM_COLORS` map for team color indicators.
- Player pool table should feel like a professional DFS tool — dense but readable, not cluttered.
- Results table should be scannable at a glance — the player name columns should be tight (last name only), the stat columns prominent.
- Use subtle transitions on row expand/collapse. No janky layout shifts.
- The Optimize button should have a loading state (spinner, disabled) while the API call is running.
- Show a progress indicator if generating > 50 lineups: "Generating lineup 34 of 150..."

---

### 10. Files to Create / Modify

| Action | File |
|---|---|
| MODIFY | `frontend/src/pagesConfig.js` — remove `isDevelopment: true` from `optimizer` |
| MODIFY | `frontend/src/App.jsx` — add `optimizerLineups`, `optimizerSettings` state + Optimizer case |
| CREATE | `frontend/src/pages/Optimizer.jsx` — full optimizer page (pool + results views) |
| MODIFY | `frontend/src/api.js` — add `optimizeLineups()` method + mock fallback |
| CREATE | `src/api/optimizer_endpoint.py` (or add to existing routes) — POST /api/optimize |

---

### 11. Out of Scope for This Build

- Showdown (CPT/FLEX) optimizer — separate build
- FanDuel salary cap / scoring (DK only for now)
- Real ownership data (synthetic model only)
- Real DK/FD contest APIs for salary import
- Lineup CSV upload/import
- Historical lineup performance tracking
- Kelly Criterion entry sizing
- Full Markowitz portfolio optimization pass (ELC and Portfolio Score use the simplified similarity/marginal-EV approach described above)

---

### 12. Verification Checklist

After building, verify:
- [ ] Navigating to Optimizer from nav no longer shows InDevelopment placeholder
- [ ] Player pool populates from `allSimResults` when sims have been run, falls back to `weekProjections`, falls back to mock roster data
- [ ] Percentile hotkeys (P25/P40/P50/P75/P95) update the Proj field and trigger color coding
- [ ] Excluding a player grays the row; excluding a team/game excludes all their players; all can be re-included
- [ ] Lock toggle forces player into all generated lineups
- [ ] Max exposure setting is respected (no player appears in > X% of lineups, unless locked)
- [ ] Min unique players setting means no two lineups share more than (9 - min_unique) players
- [ ] Generated lineups are displayed in QB/RB/RB/WR/WR/WR/FLEX/DST canonical order
- [ ] Portfolio summary panel shows EV%, ELC, avg correlation, coverage score
- [ ] Lineup row expansion shows player detail
- [ ] CSV export produces valid DK upload format
- [ ] Mock fallback produces valid lineups when backend is unreachable
- [ ] Salary cap is never exceeded in any generated lineup
- [ ] All 9 lineup slots are filled in every lineup (no empty slots)
