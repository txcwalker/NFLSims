# Specification: DFS Roster Quarterback Filtering (Option 1)

This specification outlines the prompt, architecture changes, and implementation steps required to enforce that **only one starting quarterback per team** is loaded and simulated for DFS slate slates, eliminating backups from both the frontend UI and the simulation engine inputs.

---

## 1. Context & Rationale
Daily Fantasy Sports (DFS) platforms (such as DraftKings and FanDuel) only value active starters. While season-long simulators model injuries and quarterback substitutions (using backup QBs like Cooper Rush or Trey Lance), DFS slates should never project backups to take random snaps unless there is a known starter transition. 

By pruning backup quarterbacks at the API roster-loading layer, we ensure that:
1.  **Cleaner UI**: Users don't see backup QBs in player-workload adjustment sliders or optimal lineup tables.
2.  **No Stat Leakage**: All quarterback pass attempts, completions, yards, and scores go entirely to the designated starter.
3.  **Simpler State**: The frontend doesn't need custom filters to hide backups, as they are never returned in the roster endpoint.

---

## 2. Technical Specification & Implementation Prompt

### System Prompt for Implementation
```markdown
Please implement Option 1 (API-level Roster Pruning) to ensure only the starting quarterback is considered in DFS slates. 

Follow these steps exactly:

1. Locate the rosters endpoint `@app.get("/api/rosters")` in `src/api/app.py`.
2. Right after loading the roster traits from the JSON file on disk, identify all players listed with `pos == "QB"`.
3. If more than one QB exists:
   - Load `data/dna/qb_dna.json`.
   - Identify the starting QB by finding the player key with the maximum `total_attempts` recorded in the DNA database.
   - Delete all other backup QBs from the team's traits dictionary before building the payload.
4. Verify that this logic handles case sanitization and missing DNA keys gracefully (defaulting to 0 attempts).
5. In `frontend/src/api.js`, verify that the `MOCK_ROSTERS` and dynamic rosters fallback behavior mirrors this pruning so the sandbox environment also only returns a single QB per team.
```

---

## 3. Reference Implementation Details

### FastAPI Backend Change (`src/api/app.py`):
```python
# Insert inside get_rosters()
qbs = [p for p, t in traits.items() if t.get("pos") == "QB"]
if len(qbs) > 1:
    qb_dna = load_json(os.path.join(BASE_DIR, "data", "dna", "qb_dna.json"))
    # Max attempts determines the starter QB
    starter_qb = max(qbs, key=lambda p: qb_dna.get(p, {}).get("total_attempts", 0))
    for qb in qbs:
        if qb != starter_qb:
            del traits[qb]
```

### Frontend Safe Sandbox Check (`frontend/src/api.js`):
In case the backend is offline, the client-side `getRosters` function must also prune backups from `MOCK_ROSTERS` (which is imported from `allRosters.js`) to align with the database starter rules.
