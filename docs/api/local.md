# Local API Architecture & Documentation

This document outlines the architecture, overall goal, roles, and endpoints of the custom REST API designed for the NFL DFS Exploration and Simulation platform.

---

## 1. Architecture Map

```mermaid
graph TD
    %% Frontend Layer
    subgraph Frontend [User Interface]
        A["React Frontend (Vite, Port 5173)"]
    end

    %% API Gateway Layer
    subgraph API [FastAPI Server (Port 8000)]
        B["FastAPI Application (src/api/app.py)"]
    end

    %% Data & Model Layers
    subgraph Engine [Simulation & Data Layer]
        C["In-Memory Parquet Cache (sim_results_2025_*.parquet)"]
        D["JSON DNA Databases (trench, coach, skill DNA)"]
        E["Monte Carlo Simulation Engine (src/nfl_sim/)"]
    end

    %% Connections
    A -->|"HTTP requests (safeFetch)"| B
    B -->|"/api/week_projections"| C
    B -->|"/api/rosters"| D
    B -->|"/api/simulate"| E
    E -->|"CPU Parallel Workloads"| E
```

---

## 2. Overall Goal & Role

The primary goal of the local API is to act as a **high-performance bridge** between the Python-based data science models (Monte Carlo simulations, coordinate atlas, trench DNA) and the React-based frontend dashboard. 

### Core Roles:
* **Decoupling Concerns:** Separates computationally expensive simulations and heavy statistics aggregation (Python) from UI layout and interactions (React).
* **Caching & Performance:** Pre-loads large binary data (parquets) into RAM at startup to ensure endpoints like player projections respond in milliseconds instead of seconds.
* **On-Demand Computations:** Intelligently checks for pre-calculated matchups. It serves baseline results instantly, only starting CPU-intensive Monte Carlo simulations when users modify default settings (like Custom Spreads, PROE, or Player shares).

---

## 3. Component Breakdown & Descriptions

### A. React Frontend
* **Role:** Renders the dashboard, sliders, Vegas scenario controls, and the greedy DFS lineup optimizer.
* **Interaction:** Uses a central `ApiService` wrapper (`frontend/src/api.js`) with failover handling to send JSON payloads to the API.

### B. FastAPI Application (`src/api/app.py`)
* **Role:** The routing gateway. Handles request validation via Pydantic schemas, routes requests to specific data structures, and manages CORS settings.
* **Entry Point:** Startable locally via `venv/Scripts/python.exe -m uvicorn src.api.app:app --reload`.

### C. In-Memory Parquet Cache
* **Role:** Houses the pre-calculated baseline data.
* **Files:** 
  * `data/interim/sim_results_2025_games.parquet` (game-level iterations)
  * `data/interim/sim_results_2025_players.parquet` (trial-by-trial player metrics)

### D. JSON DNA Databases
* **Role:** Stores baseline team parameters, coach tendencies, and skill metrics collected from historical years (2015–2024).
* **Files:**
  * `data/dna/trench_dna.json` (continuous team pressure and sack rates)
  * `data/dna/coordinator_atlas.json` (coach pass rate overrides - PROE)
  * `data/current_rosters/*_traits_2025.json` (player workload profiles)

### E. Monte Carlo Simulation Engine
* **Role:** Runs randomized plays concurrently to project scores, cover rates, win probabilities, and fantasy outcomes.
* **Execution:** Leverages Python's `multiprocessing` to distribute the simulation load across the host machine's physical CPU cores.

---

## 4. Main API Endpoints

| Endpoint | Method | Description | Cache Behavior |
| :--- | :--- | :--- | :--- |
| `/api/weeks` | `GET` | Returns list of available regular season weeks. | Fast read from schedule CSV. |
| `/api/games?week=N` | `GET` | Returns matchups, schedules, records, and Vegas lines for a week. | Fast read from schedule CSV. |
| `/api/rosters?away=T1&home=T2` | `GET` | Serves rosters, catch rates, baseline PROE, baseline pressure, and baseline pace. | Baseline plays are computed dynamically from parquet cache; DNA is read from JSON. |
| `/api/week_projections?week=N` | `GET` | Aggregates all player simulations and percentiles for a multi-game slate. | Cached in-memory after first calculation (`WEEK_PROJECTIONS_CACHE`). |
| `/api/simulate` | `POST` | Runs (or fetches) a single-game Monte Carlo simulation applying user-configured DNA/Vegas overrides. | Returns baseline from parquet memory instantly; spins up CPU cores if overrides are active. |

### Positional Evaluator Endpoints

| Endpoint | Method | Description | Notes |
| :--- | :--- | :--- | :--- |
| `/api/positional-evaluator` | `GET` | Evaluates a single game state. Returns EP, KEP, EFSD, and per-concept (Run/Screen/Short/Medium/Deep) mean + delta for both KEP and EFSD. | `n_sims` controls simulation depth (100–5000). |

### Historical Testing Lab Endpoints (Week 1 2025)

| Endpoint | Method | Description | Notes |
| :--- | :--- | :--- | :--- |
| `/api/historical/week1-2025` | `GET` | Returns the 16 Week 1 2025 game records (metadata only). | Lazy-loads via `nfl_data_py` on first call; cached in process memory. |
| `/api/historical/plays/{game_id}` | `GET` | Returns all scrimmage plays for a game with pre-computed `kep_off`, `home_kep`, `efsd_off`, `home_efsd`, and `ep` per play. | `home_kep` / `home_efsd` positive = home team leading positionally. |
| `/api/historical/suggest-lines` | `GET` | Returns `n_lines` suggested play sequences of a given `depth` (principal variations). | `metric` param selects ranking: `kep` (default) or `efsd`. Metric controls both the sort key and which step values are returned (`kep_steps` vs `efsd_steps`). |

**`suggest-lines` key parameters:**

| Param | Type | Default | Description |
|:---|:---|:---|:---|
| `down` | int | 1 | Current down (1–4) |
| `distance` | int | 10 | Yards to first down |
| `yardline_100` | int | 75 | Yards from opponent goal line |
| `clock` | int | 1800 | Game seconds remaining |
| `score_differential` | float | 0 | Offense minus defense score |
| `n_sims` | int | 150 | Simulations per concept evaluation |
| `depth` | int | 2 | How many play concepts to chain (1–3) |
| `n_lines` | int | 3 | How many lines to return (1–5) |
| `metric` | str | `kep` | Ranking metric: `kep` or `efsd` |

**Response shape (metric=efsd example):**
```json
{
  "lines": [
    {
      "concepts": ["Deep", "Run"],
      "delta_efsd": 1.17,
      "efsd_steps": [7.2, 6.1],
      "delta_kep": 0.89,
      "kep_steps": [5.1, 4.3],
      "n": 42
    }
  ]
}
```
