# NFLSims: Fantasy & Betting Platform Plan

This document outlines the architecture, feature set, design guidelines, and implementation roadmap for the NFLSims Fantasy & Betting platform. This website is dedicated to monetization, DFS optimization, and finding sports betting edges using simulated data.

---

## 🎯 1. Platform Focus & Target Audience
The Fantasy & Betting platform target audience includes daily fantasy sports (DFS) players, prop bettors, and season-long fantasy managers looking for quantitative advantages. The site leverages Monte Carlo simulation distributions to find value edges against sportsbooks and DFS salary structures.

---

## 🛠️ 2. Core Tools & Features

### A. DFS Simulator & Projection Engine
- **Purpose**: Generates high-fidelity, distribution-based player projections instead of static mean projections.
- **Key Capabilities**:
  - Execute 10,000 simulations for specific game slates.
  - Export player distributions (median, ceiling, floor, 75th/90th percentile).
  - Calculate correlation coefficients between quarterbacks, wide receivers, and opposing players.
  - Support customized scoring systems (DraftKings, FanDuel, Yahoo).

### B. DFS Lineup Optimizer (Knapsack Solver)
- **Purpose**: Solve the mathematical constraint problem to build optimal lineups within salary caps.
- **Key Capabilities**:
  - Multi-lineup generator (e.g., 20, 50, 150 lineups) using mixed-integer linear programming (MILP).
  - Custom rules engine:
    - QB + WR/TE stacking.
    - Run-back options (QB + WR + opposing WR).
    - Position limits and exposure caps.
    - Player locks and exclusions.
  - Direct CSV export formatted for DraftKings and FanDuel uploads.

### C. Prop Bet Finder (+EV Calculator)
- **Purpose**: Automatically identify discrepancies between sportsbook lines and simulated outcomes.
- **Key Capabilities**:
  - Live API integration with sportsbooks to pull current prop lines (passing yards, rushing yards, touchdowns, receptions).
  - Expected Value (+EV) calculation based on simulated probability density functions.
  - Edge highlighting (e.g., "Simulated Over probability: 68% | Sportsbook implied probability: 55% | Net Edge: +13%").
  - Filterable dashboard by game, player position, sportsbook, and size of edge.

### D. NFL Awards & Futures Tracker
- **Purpose**: Aggregate season-long simulations to calculate the likelihood of season awards and futures.
- **Key Capabilities**:
  - Simulated odds for MVP, Offensive Player of the Year, Defensive Player of the Year, and Coach of the Year.
  - Simulated probability of division titles, playoff seeds, conference championships, and Super Bowl wins.
  - Comparison against live sportsbook futures odds to pinpoint long-term investment value.

---

## 🎨 3. Design System & User Interface

### A. Color Palette & Typography
- **Background**: Deep space obsidian and charcoal (`#030712`, `#0b0f19`) to convey a professional financial-terminal feel.
- **Accents**:
  - Value Green (`#10b981` / `#00f5d4`): Indicates positive EV, high ROI, and validated value edges.
  - Premium Cyan (`#00f2fe`): Primary interactive state color for sliders, buttons, and selection fields.
  - Electric Purple (`#8b5cf6`): Denotes premium analytics, optimizer configurations, and advanced filters.
- **Typography**: Inter (clean, highly legible sans-serif) for all tables, matrices, and numerical grids; Outfit for headings and primary metrics.

### B. Page Structure
1. **Dashboard Home**: Real-time ticker of top value edges, current slate status, and quick-launch cards for each tool.
2. **Prop Finder**: Clean, tabular view of simulated player statistics compared to live sportsbook odds.
3. **DFS Workspace**: Unified layout containing slate selectors, simulator sliders, optimization parameters, and the generated lineup output grid.
4. **Futures Explorer**: Graphical probability distributions for award races and team accomplishments.

---

## 🔌 4. Backend Integrations & Data Flow
- **Simulation Input**: Receives raw simulations from the core simulation engine.
- **Odds API Service**: Polls active betting markets every 15 minutes to retrieve lines.
- **Contest Salary Feeds**: Processes upload files from DraftKings/FanDuel to map active salaries and eligible positions to players.
- **Optimization Solver**: Fast client-side or backend solver executing MILP in WebAssembly or Python.

---

## 🚀 5. Implementation Roadmap
1. **Phase 1: DFS Simulator Refinement** - Connect the simulation outputs to DraftKings and FanDuel scoring rules.
2. **Phase 2: Live Odds Scraping** - Establish robust integrations with odds APIs to ingest player prop lines.
3. **Phase 3: Prop Bet Finder Page** - Build the +EV comparator table and filtering system.
4. **Phase 4: DFS Optimizer Solver** - Integrate the knapsack mathematical solver and lineup generator.
5. **Phase 5: Futures and Awards** - Implement the season-long simulation aggregator to track MVP and Super Bowl betting odds.
