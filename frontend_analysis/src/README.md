# Analytics Site Source (`frontend_analysis/src/`)

React (Vite) source for the **Game Analysis & Next-Play Predictor** site (dev
port 5174). Proxies `/api` to the analytics backend on port 8000 (see
`../vite.config.js`).

## Directory Structure

```
frontend_analysis/src/
├── README.md         # This file
├── main.jsx          # React entry point (mounts <App/>)
├── App.jsx           # Root component; renders the active page from pagesConfig
├── pagesConfig.js    # Page registry (id → component, nav label); add pages here + App.jsx
├── api.js            # API_BASE='/api', safeFetch wrapper + all mock data
├── App.css, index.css
├── assets/           # Static assets (images, icons)
└── pages/            # One component per page (see below)
```

## Pages (`pages/`)

* **[`GameSummary.jsx`](pages/GameSummary.jsx)** — main game view: win-probability
  graph, 4th-down decisions, and the KEP/EP chess-evaluator tab.
* **[`FourthDowns.jsx`](pages/FourthDowns.jsx)** — 4th-down decision breakdowns.
* **[`LiveWP.jsx`](pages/LiveWP.jsx)** — live win-probability tracker.
* **[`Standings.jsx`](pages/Standings.jsx)** — Monte Carlo standings / playoff odds.
* **[`Home.jsx`](pages/Home.jsx)** — landing page.
* **[`HistoricalLab.jsx`](pages/HistoricalLab.jsx)** — Week 1 2025 chess-evaluator
  testing lab. **Marked "remove before launch"** — routed as `historical-lab` in
  `pagesConfig.js`; strip from `pagesConfig.js` + `App.jsx` before production
  (see AGENTS.md §8).

## Running

```bash
cd frontend_analysis && npm install && npm run dev   # serves on http://localhost:5174
```
Requires the analytics backend on port 8000
(`python -m uvicorn src.api.app:app --port 8000`); `safeFetch` falls back to
mock data when the API is unreachable.
