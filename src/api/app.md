# app.py — FastAPI Matchup Service V.0.2.0

### Why do we need it
The web application requires an interface that sits between the React frontend UI and the intensive Python simulation engine. This file exposes high-speed endpoints to fetch matchup rosters, apply custom in-memory scenario adjustments, and trigger Monte Carlo simulations on demand.

### What is it doing
* **Roster & Settings Compilation**: Gathers player trait JSONs, head coach PROE targets, and trench pressure statistics to build a baseline matchup profile for any two teams.
* **Workload & Pace Overlays**: Intercepts custom inputs (overriding features like targets, carries, defensive pressures, and pace targets) and dynamically redistributes simulated touchdowns and carries among eligible players.
* **Vectorized Simulation Dispatching**: Spawns parallelized game simulators using ProcessPoolExecutors, safely injecting head-coach settings into worker processes.
* **Fantasy Post-Processing**: Computes projected DraftKings and FanDuel scoring averages, floors, ceilings, and salaries.

### Subject matter expertise utilized
* **Dynamic Workload Scaling**: Allocates team touchdowns back to skill players proportional to user-defined shares rather than raw random counts.
* **DFS Salary Pricing Modeling**: Generates baseline salaries dynamically from projected workload shares (targets/carries) and passing efficiency matrices.
* **Vegas Line Conversion**: Translates home spreads and game totals into individual game-by-game cover and over/under hit frequencies.

### Decisions made & metrics/reasoning used
* **Multiprocessing DLL Hatch**: Explicitly initializes head coach PROE overlays in spawned workers, and completely shuts down the ProcessPoolExecutor upon completion. This keeps worker footprints minimal and bypasses PIL/Matplotlib DLL import collisions on Windows.
* **Touchdown Redistributor**: Passing touchdowns are dynamically assigned to the starting QB based on the sum of all receiving touchdowns allocated to team skill players in that iteration.

### Why this metric vs alternatives
* **FastAPI vs Django/Flask**: Selected FastAPI because it natively offers asynchronous request boundaries, automatic Pydantic schema validation, and significantly lower overhead under parallel simulation traffic.
* **ProcessPoolExecutor vs Threading**: Utilized process pools rather than standard threading to bypass the Python Global Interpreter Lock (GIL) since the simulation engine runs CPU-bound statistical loops.

### What project goal does this accomplish
Exposes a clean API interface that powers the interactive NFL game day dashboard, letting users perform real-time "what-if" scenario tests by adjusting sliders and instantly seeing simulated fantasy distributions.
