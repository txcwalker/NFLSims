# NFLSims Frontend Restructure Plan (FRONTEND_GOALS)

This document details the architecture, component breakdown, page specs, styling principles, and integration boundaries for the NFLSims frontend restructure. It establishes a modular framework that allows adding, removing, or editing pages via a single registry.

---

## 🏗️ 1. Frontend Architecture & Modular Routing

To ensure the site is highly modular and easy to extend, we will decouple the routing logic and page definitions into a centralized registry.

### A. Central Route Registry: `src/pagesConfig.js`
This file serves as the single source of truth for all routes. Each page is represented as a config object:
- **`id`**: Unique string identifier (used as the state key for routing).
- **`label`**: The display name in menus and nav links.
- **`showInNavbar`**: Boolean to determine if it appears directly in the main navigation.
- **`isDevelopment`**: If true, routes dynamically to the `InDevelopment` component rendering a premium interactive preview wireframe.
- **`category`**: Groups pages (e.g., `'core'`, `'tool'`, `'meta'`) to organize the navigation layout.
- **`targetDate`**: Target release date displayed on preview cards.
- **`icon`**: Emoji or SVG icon representing the tool.
- **`description`**: Brief tagline detailing what the page does or will do.

```javascript
// src/pagesConfig.js
export const PAGES = [
  {
    id: 'home',
    label: 'Home',
    showInNavbar: false,
    category: 'core',
    description: 'Welcome portal for NFLSims'
  },
  {
    id: 'simulator',
    label: 'DFS Simulator',
    showInNavbar: true,
    category: 'core',
    icon: '🏈',
    description: 'Run 10,000 parallel Monte Carlo simulations to find weekly game outcomes and player statistics.'
  },
  {
    id: 'optimizer',
    label: 'DFS Optimizer',
    showInNavbar: true,
    category: 'tool',
    isDevelopment: true,
    targetDate: 'Jan 2027',
    icon: '⚡',
    description: 'Build optimal DraftKings and FanDuel lineups using mathematical constraints (knapsack solver) on simulated projections.'
  },
  {
    id: 'props',
    label: 'Prop Bet Finder',
    showInNavbar: true,
    category: 'tool',
    isDevelopment: true,
    targetDate: 'June 2027',
    icon: '🎯',
    description: 'Identify value edges by comparing simulated player performance thresholds with live sportsbook lines.'
  },
  {
    id: 'war',
    label: 'WAR Metric',
    showInNavbar: true,
    category: 'tool',
    isDevelopment: true,
    targetDate: 'Jan 2027',
    icon: '🛡️',
    description: 'Evaluate individual player impact on team wins compared to a replacement-level baseline in the NFL.'
  },
  {
    id: 'chess',
    label: 'Chess Evaluator',
    showInNavbar: true,
    category: 'tool',
    isDevelopment: true,
    targetDate: 'Aug 2026',
    icon: '♟️',
    description: 'Analyze coaching decisions, expected points, and win probabilities play-by-play using a situational grid.'
  },
  {
    id: 'bot',
    label: '4th Down Bot',
    showInNavbar: true,
    category: 'tool',
    isDevelopment: true,
    targetDate: 'Nov 2026',
    icon: '🤖',
    description: 'Real-time decision optimizer that calculates the exact math for Go, Punt, and Field Goal decisions.'
  },
  {
    id: 'about',
    label: 'Methodology',
    showInNavbar: true,
    category: 'meta',
    icon: '📚',
    description: 'Deep dive into the underlying physics of the simulation engine.'
  },
  {
    id: 'roadmap',
    label: 'Roadmap',
    showInNavbar: true,
    category: 'meta',
    icon: '🗺️',
    description: 'Living map and progress log of all project components.'
  }
];
```

### B. Shell Architecture: `src/App.jsx`
- Holds global layout elements (Header, Navbar, Footer).
- Manages routing state: `const [currentPage, setCurrentPage] = useState('home');`.
- Syncs the window location hash (e.g., `#simulator`) to allow bookmarking, browser back-navigation, and sharing links.
- Uses conditional rendering driven by the registry to display the active view.
- Renders the global `Navbar` at the top and the dynamic `ProgressFooter` at the bottom of all views.

---

## 🎛️ 2. Component & Page Specifications

### A. Navigation Bar (`src/components/Navbar.jsx`)
- **Visuals:** Frameless glass background with bottom borders. Uses flexbox to space items. Includes a glowing brand logo with a live pulse dot.
- **Dynamic Links:** 
  - Direct links for core metadata (`DFS Simulator`, `Methodology`, `Roadmap`).
  - A dropdown menu labeled **"Analytic Tools (Beta)"** which aggregates all items where `category === 'tool'`.
  - Development tools render with a subtle `BETA` or `DEV` badge in the dropdown.
- **State Integration:** Clicking links sets `currentPage` and updates the URL hash.

### B. Progress Footer (`src/components/ProgressFooter.jsx`)
- **Visuals:** A sticky, thin bottom footer with a blurred glassmorphic overlay (`backdrop-filter`) and a dark border. 
- **Layout & Metrics:**
  - **Left Section:** App name and active tag (e.g., `NFLSims Platform v0.1.0-alpha`).
  - **Center Section:** Mini progress indicators for the project milestones:
    - *Tier 1 (Engine):* `75%` (renders a tiny cyan progress bar).
    - *Tier 2 (DFS/Betting):* `10%` (renders a tiny purple progress bar).
    - *Tier 3 (Live Tools):* `0%` (renders a tiny gray progress bar).
  - **Right Section:** A status marquee/ticker cycling through active items: `"Currently Hardening: Clock Physics & Punting logic"` → `"Up Next: DFS Lineup Optimizer"` → `"Model standard locked: 10/10 models audited"`.
  - **Interactive Action:** Clicking the footer navigates directly to the full **Roadmap** page (`currentPage => 'roadmap'`).

### C. Home Page (`src/pages/Home.jsx`)
- **Hero Banner:** Uses high-contrast styling (white/cyan text gradient, outfit font) with custom glass-card styling. Features a clear subtitle and CTA buttons:
  - `Launch DFS Simulator` (primary neon button)
  - `View Project Roadmap` (secondary glass button)
- **Features Grid:** Maps through `PAGES`. Renders a grid of hoverable card layouts.
  - Active tools show a "Launch Tool" button.
  - Development tools show a lock icon, progress percentage, target date, and a "Preview Module" button.
- **Engine Diagnostics Dashboard:** Shows the status of backend simulator models:
  - *Play type selection (Pass/Run odds):* `Hardened` 🟢
  - *Yardage Engines (Air yards, Rush, YAC):* `Active` 🟢
  - *Chaos Generator (Sacks, Slices, Turnovers):* `Active` 🟢
  - *4th Down Decision Math:* `Active` 🟢
  - *Clock Physics Engine:* `Hardening` 🟡
  - *Lineup Optimization Solver:* `Planning` 🟣
- **Interactive Metrics:** Highlight key performance details (e.g., "10,000 simulations completed in 1.2s", "Parallelized across all CPU cores").

### D. Methodology Page (`src/pages/About.jsx`)
- **Overview:** Introduces the mathematical foundation of Monte Carlo gaming simulation.
- **Interactive Play Lifecycle Explorer:**
  - A visual horizontal/vertical step layout representing how the backend simulates a single play.
  - **Steps:** 
    1. *Pre-Snap Checks:* Penalties and situational alignment.
    2. *Play Selection:* Pass/Run ratio based on tempo, clock, and coach PROE.
    3. *Execution:* Yards gained, air yards calculation, throw depth.
    4. *Post-Catch/Rush YAC:* Physical tackle evasion and yards-after-contact math.
    5. *Post-Snap Chaos:* Sacks, fumbles, interceptions, and defensive pressures.
    6. *Game State Update:* Play clock ticking, scoreboard updates, and drive progression.
  - **Interactivity:** Clicking a step opens a card describing which user overrides (e.g. PROE, Defensive Pressure) and backend coefficients govern that phase of the simulation.
- **DFS Scoring Accordion:** Compare DraftKings and FanDuel scoring mechanisms in a structured table.

### E. Living Roadmap Page (`src/pages/Roadmap.jsx`)
- A beautiful, chronological timeline divided into three Tiers based on `PROJECT_ROADMAP.md`:
  - **Tier 1: Simulation Engine Foundation** (Target: Sept 2026 Kickoff)
  - **Tier 2: DFS & Betting Integration** (Target: Offseason 2026-2027)
  - **Tier 3: Live Analytics & Contracts** (Target: 2027 Season)
- **Visuals:** Vertical timeline track. Completed items render with green checkmark circles; in-progress items glow cyan; pending items remain in gray.
- **Interactivity:** Clicking a timeline card slides open a detailed drawer showing implementation requirements, model validation states, and current blocker descriptions.

### F. In Development Preview (`src/pages/InDevelopment.jsx`)
A shared component rendering mock dashboards that correspond to upcoming tools, proving modular capability:
- **DFS Optimizer Preview:**
  - Renders a mock lineup builder window with salary constraints ($50,000).
  - Displays mock optimized player cards (Player, Position, Salary, Projected Points, Simulated ROI).
  - Includes an inactive "Export Lineups (CSV)" button with a tooltip: "Optimizer integration slated for Jan 2027".
- **Prop Bet Finder Preview:**
  - Renders a mock tabular view comparing Book Line vs. Sim Line.
  - Highlights mock edges (e.g., "Patrick Mahomes: Over 2.5 Passing TDs - Sim: 72% | Book: 58% | Edge: +14%").
  - Includes a mock selector to switch between Weekly and Season-Long props.
- **WAR Metric Preview:**
  - Displays mock cards showing "Wins Above Replacement" value across different players.
  - Renders a mock chart or table comparing player contracts to their simulated WAR value.
- **Chess Evaluator Preview:**
  - A mock situational builder input (e.g., Down: 3rd, Distance: 8 yds, Field Position: Own 35-yd line).
  - Renders a mock recommendation matrix displaying the expected points (EP) and win probability (WP) for choosing a Pass, Rush, or Play-Action.
- **4th Down Bot Preview:**
  - Renders a mock calculator output comparing three options: Go For It, Punt, or Field Goal.
  - Shows simulated success rates and win probability graphs for each choice.

---

## 🎨 3. Design System & CSS Rules

All styling will extend the core rules in `index.css` and `App.css`:
- **Harmonious Palette:** Maintain HSL-based space colors:
  - Deep space blue (`#070b19` & `#0b1126`).
  - Primary accents: Neon Cyan (`#00f2fe`) for actions, Electric Purple (`#9d4edd`) for secondary items, Mint Green (`#00f5d4`) for completed statuses.
- **Typography:** Outfit for headings (bold, modern spacing), Inter for clean tabular data.
- **Transitions:** Every link hover, card selection, and slide drawer will use smooth CSS transitions: `transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);`.
- **Glassmorphism:** Subtly glowing semi-transparent backdrops (`rgba(13, 22, 47, 0.7)`) with fine transparent borders.

---

## 🔌 4. Integration Boundaries

The frontend links with the FastAPI backend through unified HTTP fetch endpoints. The existing endpoints in `App.jsx` will be preserved inside `Simulator.jsx`:
- **`GET /api/weeks`**: Fetch weeks available for simulation.
- **`GET /api/games?week={week}`**: Fetch game matchups and schedules.
- **`GET /api/rosters?away={team}&home={team}&year=2025`**: Fetch player rosters and basic settings.
- **`POST /api/simulate`**: Send user overrides and run 10,000 parallel Monte Carlo simulations, returning win probabilities and player stats.

*We will ensure these API hooks are scoped cleanly inside `Simulator.jsx` and do not bleed into the global layout state.*

---

## 📊 5. Verification Plan

### A. Local Build
Run build scripts to verify there are no compilation errors:
```bash
npm run build
```

### B. Route Integrity
Verify that clicking nav links, Home cards, or the Progress Footer updates the URL hash (e.g., `/#props`) and renders the correct page component instantly.

### C. Simulation Dashboard Sandbox
Verify that compiling and running simulations in the refactored `Simulator.jsx` component preserves all interactivity:
- Changing sliders works correctly.
- Workload sums balance in real-time.
- Running parallel simulations communicates with the FastAPI server and updates stats, probabilities, and graphs correctly.
