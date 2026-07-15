# Roster & Player DNA Management

This folder contains data preparation, compilation, and validation tools used to map real-life NFL player profiles to their simulated DNA structures.

## Scripts & Functions

* **`build_full_name_dna.py`**:
  * Ingests seasonal player statistics to compile core DNA parameters (e.g., target shares, catch rates, and carry rates) mapping them directly to full player names.

* **`compileRosters.js`**:
  * A Node.js compiler script that handles the initial parsing and structuring of raw JSON roster data.

* **`update_rosters_full_name.py`**:
  * Updates existing roster JSON lists to ensure full-name consistency across matchups, avoiding mapping collisions.

* **`validate_rosters.py`**:
  * Validates rosters against the team coaching traits and DNA registry to ensure no active player is missing required statistics or mapping keys.
