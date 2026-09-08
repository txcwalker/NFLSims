# Shared Components (`frontend/src/components/`)

Small, reusable UI components shared across pages of the DFS site.

## Components

* **[`Navbar.jsx`](Navbar.jsx)**: Top navigation bar. Renders page links grouped by category from `pagesConfig.js`, and polls `ApiService.checkHealth()` every 30s to show backend online/offline status.
* **[`ProgressFooter.jsx`](ProgressFooter.jsx)**: Footer with a rotating ticker of project-status messages (current focus, next milestone).
* **[`SandboxBadge.jsx`](SandboxBadge.jsx)**: Small warning badge shown when a page is displaying mock/sandbox data instead of live backend data (e.g. backend unreachable).
