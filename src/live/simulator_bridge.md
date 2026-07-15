# simulator_bridge.py — Python-to-R Subprocess Bridge

### Why do we need it
The live polling bot runs on Python, but the mathematical simulation model that computes fourth-down decisions is written in R. This bridge provides a fast and reliable interface to trigger R simulations from Python without needing a heavy server-to-server connection.

### What is it doing
* **JSON Serialization**: Packs the Python play state dictionary into a single JSON string.
* **Process Spawning**: Starts `Rscript R/simulators/fourth_down/run_one_sim.R` as an independent subprocess using `subprocess.Popen`.
* **Standard Input Communication**: Passes the JSON string directly to the R script's stdin channel.
* **Response Capture & Parsing**: Captures the simulation's stdout JSON block, safely filters out any pre-flight logging noise, and parses the results back into a Python dictionary.

### Subject matter expertise utilized
* **Strict Process Boundaries**: Configures a strict 5.0-second timeout limit on process execution to prevent hung R processes from blocking the Python daemon loop.
* **Robust Output Filtering**: Loops through stdout lines in reverse to isolate and extract the raw JSON response block, filtering out environment warnings or package load notes.

### Decisions made & metrics/reasoning used
* **Subprocess vs API**: Chose subprocess spawning over running a persistent R-based API service because the bot only polls every 15 seconds. Subprocess boundaries offer maximum isolation and absolute memory cleanup after every execution.

### Why this metric vs alternatives
* **Rscript Subprocess vs rpy2 Library**: Selected standard Rscript subprocess communication over python-R bindings like `rpy2` to guarantee platform compatibility, avoiding complex C-level compilation requirements on Windows.

### What project goal does this accomplish
Forms the core execution bridge that allows the Python daemon to leverage the statistical simulation algorithms built in the R environment.
