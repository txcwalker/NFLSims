# Ownership Model — V1 Design Notes

## Purpose

Ownership percentage is required to calculate lineup EV accurately. In a GPP tournament, being correct on a player that 40% of the field also has is worth far less than being correct on a player only 3% of the field has — because in the first case, you didn't gain meaningful ground on most of your competition.

In V1, we do not have access to real contest ownership data (this requires community aggregation tools, Twitter/X scraping, or purchased ownership projections). Instead, we use a **two-factor synthetic ownership model** with log-normal noise to produce realistic-feeling ownership distributions.

---

## Model Design

### Step 1 — Value Score Per Player

$$\text{value}_i = \frac{\text{projection}_i}{\text{salary}_i} \times \text{position\_weight}_{pos_i}$$

**Position weights** reflect how the field concentrates by position:

| Position | Weight | Rationale |
|---|---|---|
| RB | 1.3 | Field over-indexes on top RBs — volume and touchdowns are obvious |
| WR | 1.0 | Baseline |
| QB | 0.85 | More differentiation in QB selection than the median DFS player shows |
| TE | 0.9 | Field under-indexes on TE upside |
| DST | 0.65 | Field consistently under-prices DSTs relative to point potential |

### Step 2 — Chalk Concentration Boost

Players in the **top 10% of value score** within their position group receive a `1.4×` multiplier.

This simulates the "field piles on obvious plays" effect. A top-5 value RB in a great matchup will be significantly more owned than his raw projection-per-dollar suggests — because casual players also recognize the obvious play.

### Step 3 — Softmax Normalization

All value scores are converted to ownership percentages using a **temperature-scaled softmax**:

$$\text{ownership}_i = \frac{e^{\text{value}_i / T}}{\sum_j e^{\text{value}_j / T}}$$

Where T (temperature) controls concentration:
- **Low T**: ownership concentrates on top players (realistic for obvious chalk weeks)
- **High T**: ownership spreads more evenly (realistic for uncertain weeks)

We normalize so the total ownership across all players sums approximately to `900` (9 players × 100% — the implied ownership budget for a single-entry contest with a large field).

### Step 4 — Log-Normal Noise

Raw ownership estimates are multiplied by a **log-normal noise factor**:

$$\text{final\_ownership}_i = \text{ownership}_i \times e^{\mathcal{N}(0, \sigma_i)}$$

Where σ varies by ownership tier:
- High-owned players (>25%): σ = 0.25 (tighter — consensus is stronger)
- Mid-tier players (8–25%): σ = 0.35
- Low-owned players (<8%): σ = 0.45 (wider — disagreement is high)

**Why log-normal (not normal)?** Ownership cannot be negative. Log-normal is right-skewed in the correct direction (a player can be 60% owned but not −20% owned). The log-normal shape matches observed empirical ownership distributions in real DFS contests.

---

## Expected Output Shape

| Ownership Tier | Typical Range | Example Players |
|---|---|---|
| Elite chalk | 30–50% | Top RB with perfect matchup, lock injury backup |
| Strong chalk | 18–30% | QB1 in high O/U game, WR1 in shootout |
| Mid-tier | 8–18% | Secondary stacks, safe TE options |
| Low-tier | 3–8% | Contrarian plays, secondary WRs |
| Streamer | 1–3% | True low-owned, often game-script dependent |

---

## Why Randomize At All?

Without noise, every week would produce near-identical ownership shapes based purely on projection ranks. This creates two problems:

1. **EV is artificially flat** — If ownership is deterministically tied to projection rank, every high-projection player has the same ownership-to-projection ratio and EV differences between lineups collapse toward zero.
2. **Portfolio Score is misleading** — The Effective Lineup Count metric depends on ownership dispersion. Without realistic ownership variation, the portfolio optimizer can't identify which players to differentiate on.

The log-normal noise introduces enough variance to produce realistic EV spread across lineups while keeping the central tendency anchored to the projection-derived baseline.

---

## Future Version — Real Ownership Signals

V1 synthetic ownership will be replaced in a future version with a model that incorporates:

- **Community aggregation**: Twitter/X DFS content creator picks, Discord consensus
- **Optimizer tool outputs**: What the field's popular optimizer tools are generating
- **Historical ownership patterns**: What actual ownership looked like in similar game environments
- **Site-specific tendencies**: DraftKings vs. FanDuel ownership patterns differ meaningfully

This will likely require its own dedicated module and will be designed separately when that data pipeline is in place.
