# DFS Site Source (`frontend/src/`)

React (Vite) source for the **DFS Optimizer & Betting-Edge** site (dev port
5173). Proxies `/api` to the DFS backend on port 8002 (see `../vite.config.js`).

## Directory Structure

```
frontend/src/
├── README.md         # This file
├── main.jsx          # React entry point (mounts <App/>)
├── App.jsx           # Root component; renders the active page from pagesConfig
├── pagesConfig.js    # Page registry (id → component, nav label); add pages here + App.jsx
├── api.js            # API_BASE='/api', safeFetch wrapper + mock-data fallbacks
├── allRosters.js     # Static roster data for the lineup builder
├── dfsRules.js       # DraftKings/FanDuel salary, roster-slot, and scoring rules
├── App.css, index.css
├── components/       # Shared/reusable UI components
├── assets/           # Static assets (images, icons)
└── pages/            # One component per page (see below)
```

## Pages (`pages/`)

* **[`Optimizer.jsx`](pages/Optimizer.jsx)** — DraftKings/FanDuel lineup optimizer.
* **[`Simulator.jsx`](pages/Simulator.jsx)** — interactive matchup simulator.
* **[`SlateLeaders.jsx`](pages/SlateLeaders.jsx)** — projected slate leaders.
* **[`DfsSummary.jsx`](pages/DfsSummary.jsx)** — DFS projection summary view.
* **[`Home.jsx`](pages/Home.jsx)**, **[`About.jsx`](pages/About.jsx)**,
  **[`Roadmap.jsx`](pages/Roadmap.jsx)** — landing / informational pages.
* **[`InDevelopment.jsx`](pages/InDevelopment.jsx)** — WIP placeholder. Note:
  chess code was removed and there is no `chess` page in `pagesConfig.js` — do
  not re-add one without explicit instruction (see AGENTS.md §8).

## Running

```bash
cd frontend && npm install && npm run dev   # serves on http://localhost:5173
```
Requires the DFS backend running on port 8002 (`start_backend_api.bat`);
`safeFetch` falls back to mock data when the API is unreachable.
