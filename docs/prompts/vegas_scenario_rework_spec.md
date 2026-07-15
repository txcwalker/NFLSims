# Specification: Vegas & Simulation Scenario Panel Rework

This specification outlines the technical proposal and blueprint for rewriting the scenario-shifting overlays panel in the DFS React dashboard. It targets three main objectives:
1. **Scenario Splitting**: Separating Vegas Market-based lines from Simulation-derived baselines.
2. **State Bug Resolving**: Fixing overlapping/stuck button highlights and custom override calculations.
3. **Overlays Persistence**: Storing overrides per-game so navigation doesn't reset user configurations.

---

## 1. Architectural & UI Split

Instead of a single combined bar, we will split the **Game Flow Settings** into two distinct pillars:

### Left Column: Vegas Market Game Flow (Implied Target Lines)
Focuses on shifts relative to the **Vegas Betting Market** lines (loaded from the CSV schedule).
*   **Pace & Totals Buttons**:
    *   `Vegas Baseline Total` (Standard Vegas O/U)
    *   `Vegas Shootout` (Vegas O/U +10)
    *   `Vegas Defensive` (Vegas O/U -10)
*   **Spread Scenarios Buttons**:
    *   `Vegas Baseline Spread` (Standard Vegas Spread)

### Right Column: Simulated Physics Game Flow (Model Expectations)
Focuses on shifts relative to the **baseline unweighted simulation** outputs.
*   **Pace & Totals Buttons**:
    *   `Sim Baseline Total` (Model average simulated total)
    *   `Sim Shootout` (Sim total +10)
    *   `Sim Defensive` (Sim total -10)
*   **Spread Scenarios Buttons**:
    *   `Sim Baseline Spread` (Model average simulated score margin)

---

## 2. Implied Team Totals & Custom Sliders

To prevent inputs from clashing, we will replace the independent Custom Spread and Custom Total sliders with **Implied Team Totals** controls.
*   The UI will display simple **text box input fields** for `Away Implied Total` and `Home Implied Total`.
*   When a user types a value into these text boxes:
    *   The corresponding target **Spread** is automatically calculated as: `Spread = Home Implied Total - Away Implied Total`
    *   The corresponding target **O/U Total** is automatically calculated as: `O/U Total = Home Implied Total + Away Implied Total`
*   This ensures that target scenarios are mathematically consistent, eliminating conflicting override states.

---

## 3. Resolving Button Highlighting Bugs

### The Issue
Currently, multiple buttons (e.g. `STANDARD SPREAD` and `HOME BLOWOUT`) can remain highlighted simultaneously because the highlighting logic checks active criteria loosely or fails to unset mutually exclusive scenarios.

### The Fix
*   Define a strict scenario state model in the React page:
    ```typescript
    type SpreadScenario = 'VEGAS_STANDARD' | 'SIM_STANDARD' | 'CUSTOM';
    type TotalScenario = 'VEGAS_STANDARD' | 'VEGAS_SHOOTOUT' | 'VEGAS_DEFENSIVE' | 'SIM_STANDARD' | 'SIM_SHOOTOUT' | 'SIM_DEFENSIVE' | 'CUSTOM';
    ```
*   Ensure that selecting any scenario in a group automatically updates the selected state key and sets the other mutually exclusive keys to false.
*   The button highlighting class will strictly evaluate `activeScenario === 'SCENARIO_NAME'`.

---

## 4. Overlays Persistence Across Game Navigation

### The Issue
Selecting a different game and returning resets all overrides, sliders, and workload adjustments back to defaults. This prevents users from adjusting settings on multiple games before running a unified multi-game slate optimizer.

### The Fix
*   Elevate the scenario overrides state from the local `Simulator.jsx` mount to the parent context (or a parent state container in the dashboard).
*   Structure the state as a dictionary keyed by `game_id`:
    ```javascript
    const [slateOverrides, setSlateOverrides] = useState({
      // "game_id": { teamOverrides, playerOverrides, spreadOffset, totalOffset, activeSpreadScenario, activeTotalScenario }
    });
    ```
*   When loading a game, check if `slateOverrides[game_id]` exists. If it does, populate the sliders and overrides from the stored state instead of resetting them to baseline defaults.
