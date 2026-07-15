# Optimizer Usage Guide

## Projection Adjustment — Percentile Hotkeys

The optimizer pre-populates each player's projected score from simulation medians. You can adjust any player's projection manually or use the **percentile hotkeys** as quick reference points.

### What the Percentiles Mean

| Hotkey | Meaning | Use Case |
|---|---|---|
| **p25** | 25th percentile | Floor — bad game but not a complete bust |
| **p40** | 40th percentile | Slight underperformance |
| **p50** | 50th percentile (median) | Default — expected outcome |
| **p75** | 75th percentile | Strong lean — you like the matchup or situation |
| **p95** | 95th percentile | Elite outcome — near-ceiling game |

Clicking a hotkey sets that percentile value **as the player's new projection median** for stochastic draws. The optimizer will then sample around this new median using the scaled sim distribution.

### Projection Bump Color Guide

When you adjust a player's projection above their simulation median, the input field changes color to reflect how aggressive the adjustment is:

| Color | Range | Interpretation |
|---|---|---|
| ⬜ White/Default | At or below p50 | No adjustment or negative lean |
| 🟡 Yellow | p50 → p65 | Mild lean — slightly more optimistic than sims |
| 🟠 Orange | p65 → p75 | Strong lean — meaningful divergence from sims |
| 🔴 Red | Above p75 | Aggressive — you're projecting above a 75th percentile outcome as the expected result |

> **Note on aggressive bumps**: Setting a player above their p75 means their stochastic floor in optimizer draws will be near their historical median. Use this deliberately. Valid reasons include a late injury to a teammate (volume increase), a must-start matchup, or a known positive game-script scenario not captured in the base sims.

---

## Elimination Controls

### Eliminating Players
Click the ✕ next to any player in the pool to remove them from lineup consideration. They will appear in a **"Removed Players"** panel at the bottom where they can be re-added individually.

### Eliminating Teams
Click the team badge in the player pool header or team filter to remove all players from that team (including their DST). Individual players can be re-added from the Removed panel.

### Eliminating Games
Removing a game eliminates all players from **both teams** including both DSTs. If you want a specific DST from an eliminated game, re-add them individually from the Removed panel.

---

## Lock & Exposure Controls

### Lock Player (🔒)
Locks a player into **100% of generated lineups**. Overrides max exposure setting. Use for:
- Injury replacement with extreme salary efficiency (backup RB at half the starter's price)
- Multi-game slate where one player is a near-certain dominant performer

### Max Exposure (%)
Sets the maximum percentage of lineups any single player can appear in. Default is no cap.

**Recommended ranges by contest type:**

| Contest Type | Recommended Max Exposure |
|---|---|
| Cash (H2H, 50/50) | N/A — build one optimal lineup |
| Flat Tournament | 25-35% |
| Top-Heavy Tournament | 30-40% |
| Extreme Top-Heavy (Bat Flip style) | 40-50% for anchors, 20-30% for complements |

---

## Minimum Unique Players Per Lineup

Sets the minimum number of players that must differ between any two lineups in your portfolio. Default: **2 players**.

**Include DST in uniqueness count**: Toggle whether the DST slot counts toward the uniqueness minimum. Recommended **off** for large slates where DST differentiation is common anyway.

| Portfolio Size | Recommended Min Unique |
|---|---|
| 1-5 lineups | 2-3 |
| 6-20 lineups | 2-3 |
| 21-150 lineups | 2 (let correlation model drive diversity) |

---

## Contest Type Settings

### Payout Type
Controls how the stochastic distribution width scales for each draw.

| Setting | Width | Optimization Target |
|---|---|---|
| **Cash** | No variance (deterministic) | Maximize median projected score |
| **Flat** | Narrow | Maximize in-the-money % |
| **Top-Heavy** | Medium-wide | Maximize top 10% probability |
| **Extreme Top-Heavy** | Wide | Maximize top 1% / top 0.1% probability |

### Contest Size
Adjusts how aggressively the portfolio penalizes same-direction clustering and high-chalk lineups.

| Size | Adjustment |
|---|---|
| Small (< 5K entries) | Minimal — focus on raw projected score |
| Medium (5K–25K entries) | Moderate — some differentiation rewarded |
| Large (25K–100K+ entries) | Aggressive — being different when right has high EV |

---

## Understanding the Results Page

### Per-Lineup Metrics (left side of each lineup row)

| Metric | What It Means |
|---|---|
| **EV%** | Expected return as a % of entry fee, given the prize structure and modeled field |
| **Portfolio Score** | How much this lineup improves the overall book's EV vs. adding a duplicate |
| **ITM%** | Probability of finishing in a paying position (~top 19-23% of field) |
| **Top 1%** | Probability of finishing in the top 1% of the field |
| **Top 0.1%** | Probability of finishing in the top 0.1% of the field |

### Portfolio Summary Panel (top of results page)

| Metric | What It Means |
|---|---|
| **Portfolio EV%** | Total expected return across all entered lineups |
| **Effective Lineup Count (ELC)** | Diversity measure: 10 truly diverse lineups = ELC of 10; 10 near-copies = ELC of ~1 |
| **Avg Correlation** | Average pairwise similarity between lineups in your book |
| **Coverage Score** | % of meaningful player "outcome buckets" covered by at least one lineup |

### Lineup Color Coding (results page)

| Color | Meaning |
|---|---|
| 🟢 Green tint | Top quartile of EV% in your book |
| ⬜ Neutral | Middle 50% |
| 🔴 Red tint | Bottom quartile of EV% — consider replacing |

---

## Workflow Recommendation

1. **Review sim projections** — Check the pre-populated medians. Look for players where your assessment differs from the model.
2. **Apply adjustments** — Use percentile hotkeys for quick reference. Be deliberate with bumps above p75.
3. **Eliminate weak spots** — Filter out players projecting below your threshold. Remove games or teams you want to fade.
4. **Set contest parameters** — Choose payout type, contest size, and entry count.
5. **Run optimizer** — Hit optimize. The portfolio is generated with stochastic draws and correlation structure.
6. **Review results** — Check Portfolio Score distribution. If ELC is low (e.g., 12 effective lineups from 50 generated), consider loosening max exposure or increasing min-unique.
7. **Export** — Download CSV for DK/FD upload.
