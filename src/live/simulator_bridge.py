# src/live/simulator_bridge.py
# Python-R bridge for running 4th down simulations.
# Spawns Rscript as a subprocess, feeding game states via stdin and reading JSON.
# References: docs/database_guide.md
# ------------------------------------------------------------------------------

import subprocess
import json
import logging
from typing import Dict, Any

logger = logging.getLogger("4thDownBot.SimulatorBridge")


def run_simulation(game_state: Dict[str, Any], r_script_path: str = "R/simulators/fourth_down/run_one_sim.R") -> Dict[str, Any]:
    """
    Spawns an R subprocess to evaluate expected Win Probabilities for 4th down options.
    
    Args:
        game_state: Dictionary containing current play inputs.
        r_script_path: Path to the R execution script.
        
    Returns:
        A dictionary containing predicted WPs and recommended actions, or an error dict.
    """
    try:
        # Convert the Python dictionary to a JSON string
        input_json = json.dumps(game_state)
        
        # Execute the R script as a subprocess
        process = subprocess.Popen(
            ["Rscript", r_script_path],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        
        # Communicate stdin inputs and wait with a strict timeout boundary (5 seconds)
        stdout, stderr = process.communicate(input=input_json, timeout=5.0)
        
        if process.returncode != 0:
            logger.error(f"R subprocess failed with return code {process.returncode}: {stderr.strip()}")
            return {"error": f"R process exited with code {process.returncode}", "details": stderr}
            
        # Parse the JSON response
        # Note: We split by lines in case the R environment outputs any non-JSON noise before the JSON block
        lines = stdout.strip().split("\n")
        json_line = None
        for line in reversed(lines):
            if line.strip().startswith("{") and line.strip().endswith("}"):
                json_line = line.strip()
                break
                
        if not json_line:
            logger.error(f"No valid JSON found in R subprocess output: {stdout}")
            return {"error": "Invalid output format from simulator", "output": stdout}
            
        result = json.loads(json_line)
        return result

    except subprocess.TimeoutExpired as e:
        logger.error(f"R simulation bridge timed out: {e}")
        return {"error": "R simulation timed out after 5 seconds"}
    except Exception as e:
        logger.error(f"Error executing R simulation bridge: {e}")
        return {"error": f"Internal execution error: {str(e)}"}
