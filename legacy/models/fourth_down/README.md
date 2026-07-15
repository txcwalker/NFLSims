# Legacy Fourth Down Simulation

This directory contains `simulate_fd_decision.R`, an older/experimental version of the 4th-down simulation engine.

## Inefficiencies and Overlaps
- **Repeated Model Sourcing/Loading**: Relied on global file sourcing (`predict_wp.R`, `predict_fg.R`, `predict_fd.R`) which repeatedly loads xgboost models on demand instead of caching or bundling them in-memory, causing heavy IO overhead during simulations.
- **Inconsistent Play Clocks**: Did not account for the runtime seconds removed during field goals or converted go-for-it situations, making fourth-quarter win probabilities slightly inaccurate.
- **Manual Hardcoded Overrides**: Rather than using flexible environmental configuration (e.g., `NO_PUNT_INSIDE` zone), it used manual hardcoded logic for suppressing punts or field goal attempts.
