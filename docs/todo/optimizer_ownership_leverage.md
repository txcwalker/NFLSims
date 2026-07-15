# TODO: Ownership, Leverage & Player Boosters

## Context
The current GPP optimizer uses a blended projection formula:

```
GPP_proj = P25×w1 + P50×w2 + P75×w3 + P95×w4
```

Weights are calibrated by contest type (see `GPP_WEIGHTS_BY_TYPE` in `Optimizer.jsx`).
This is a solid interim heuristic, but **ownership/leverage** is the missing variable
that separates edge from noise in large-field GPPs.

---

## 1. Ownership-Adjusted Projection (Leverage)

### The Problem
A player projecting 40 GPP points at 34% ownership is worth considerably less
than a player projecting 35 GPP points at 3% ownership. When the 34% guy scores
big, the whole field has him — no differentiation. When the 3% guy goes off,
you're one of very few lineups with him.

### The Formula
```
True GPP Value = GPP_proj × leverage_multiplier
leverage_multiplier = (avg_ownership% / player_ownership%)^k
```
where `k` controls how aggressively you discount chalk (typically 0.3–0.7).

### Data Needed
- **Projected ownership %** per player — currently no data source
- Possible sources once available:
  - DraftKings ownership API (post-lock)
  - Rotogrinders / Awesemo projected ownership API (paid)
  - Our own ownership model (ML trained on historical DK ownership vs player
    characteristics: salary, matchup quality, position scarcity, media attention)
- The `ownershipPct` field on each player object in the pool is already wired
  for manual override — this is the hook for the leverage multiplier.

### Implementation Plan (When Ready)
1. Add an `ownershipSource` toggle in Settings: Manual | Projected | Uniform
2. When projected ownership is available (API or CSV), auto-populate `ownershipPct`
3. Add `useLeverage` toggle in Settings (off by default)
4. When on, blend ownership into the ILP objective:
   ```
   effective_proj = GPP_proj * (avg_own / player_own)^k
   ```
5. Expose `k` (leverage aggressiveness) as a slider in the advanced Settings panel

---

## 2. Player Boosters / "Stickers" System

### The Concept
Some players systematically outperform statistical models because of physical or
situational attributes that are hard to capture in sim outputs. Classic example:
**Derrick Henry** — his size/power means his "ceiling" is consistently higher
than a typical RB with similar efficiency metrics.

A "sticker" is a user-defined multiplier or additive boost applied on top of
the blended projection, allowing the optimizer to correctly weight players whose
true ceilings are systematically underestimated.

### Proposed Data Model
```json
{
  "player_id": "Derrick_Henry_TEN",
  "booster_type": "ceiling_multiplier",
  "value": 1.12,
  "description": "Elite athlete — historical ceiling exceeds model predictions",
  "applies_to": ["p75", "p95"],
  "created_at": "2026-06-14",
  "expires_week": null
}
```

### Booster Types
| Type | Effect | Use Case |
|---|---|---|
| `ceiling_multiplier` | P75/P95 × value | Elite athletes, favorable matchups |
| `floor_multiplier` | P25 × value | Workhorse backs, high snap-count players |
| `projection_additive` | All pcts + value | PPR format corrections, weather adjustments |
| `ownership_discount` | Ownership% × value | Expected chalkier than DK pricing suggests |

### Implementation Plan (When Ready)
1. Create `data/manual/player_boosters.json` — user-editable sticker store
2. Load boosters in the frontend at startup (or via API endpoint)
3. In `buildPlayerPool`, apply booster multipliers before computing `gppProjection`
4. Show a visual "sticker" indicator (⚡ or star icon) on boosted players in the table
5. Add a Boosters management UI in Settings: add, edit, expire stickers

---

## 3. Portfolio EV via Field Simulation (Tier 3 Optimizer)

### The Goal
Move beyond per-lineup EV to evaluating the entire portfolio of N lineups
as a set against a simulated field.

### What We Have
- `dk_pcts_all` — 101-percentile score distribution per player (already in API)
- Basic Monte Carlo per-lineup evaluation (already in backend)
- `_compute_lineup_stats` in `app.py` — currently uses simplified random
  9-player field sampling (no position constraints — known weak point)

### What Needs to Change
1. **Replace** the random field sampler with proper lineup construction:
   - Sample players by position using ownership weights
   - Enforce position constraints (1 QB, 2 RB, 3 WR, 1 TE, 1 FLEX, 1 DST)
   - Enforce salary cap ($50k DK / $60k FD)
2. **Evaluate** the portfolio holistically:
   ```
   For each of N_FIELD_SIMS realizations:
     - Draw scores from dk_pcts_all for all players
     - Build K field lineups (K ~ contest_size)
     - Rank our N lineups within the field
     - Compute payout based on final rank × payout_structure
   Portfolio EV = avg(total_payout) / (N × entry_fee) - 1
   ```
3. **Use** the portfolio EV as the lineup *selection* criterion, not just
   a post-hoc stat.

### Dependencies
- Ownership data (see section 1) — without it, assume uniform ownership
- Real payout structure (see dk_contest_api.md) — without it, use tier-based approx

---

## Notes

- The `ownershipPct` field on each pool player is **already wired** for
  manual override — this is the ready-made hook for automated ownership.
- The `dk_pcts_all` array (101 values, P0–P100) is **already populated**
  from the API — field simulation can sample from this immediately.
- All GPP weight constants live in `GPP_WEIGHTS_BY_TYPE` in `Optimizer.jsx`.
  Adjusting weights is a single-object edit — easy to tune as we learn.
