# model_registry.py — Predictive Model Registry

### Why do we need it
A physics simulation engine needs statistical models to predict play outcomes (completion probabilities, rush gains, sacks, interceptions). This registry loads, caches, and coordinates all machine learning models in a single module.

### What is it doing
* **Model Loading & Caching**: Initializes and loads specific machine learning submodels from their versioned directories (`chaos_v_0_1_0`, `air_yards_v_0_1_1`, `yac_model_v_0_1_1`, `rush_yards_v_0_1_0`, `fg_v_0_1_0`, `win_probability_v_0_1_0`, `fourth_down_conversion_v_0_1_0`).
* **Play Selection Mapping**: Dynamically registers XGBoost classifiers for different down/distance buckets, mapping them to game zones.
* **4th Down Strategic Recommendations**: Computes expected win probabilities for 4th down actions (Go, Punt, FG) using situational features, and applies desperation-time rules when trailing late.
* **DNA Lookup Integration**: Manages player DNA dictionaries (`qb_dna.json` and `skill_dna.json`) to adjust features based on who is active on the play.

### Subject matter expertise utilized
* **Desperation-Time Logic**: Hardcodes override gates that force teams to "Go for it" on fourth down when trailing late (e.g. by 17+ points in the last 7 minutes) to match real-world coaching urgency.
* **Play Selection Zoning**: Splits play selection submodels into three distinct zones (`goalline`, `redzone`, `primary`) to capture changing play-calling tendencies near the endzone.

### Decisions made & metrics/reasoning used
* **Double Hurdle Sampler**: Utilizes a customized double-hurdle model for air yards, separating target decisions from depth-of-target simulations.
* **Roster Traits Mapping**: Uses dynamic player DNA splits (e.g. catch rates and target shares) to adjust player performance based on the field zone.

### Why this metric vs alternatives
* **Registry Class Singleton vs Global Imports**: Selected a centralized `ModelRegistry` singleton because it loads heavy ML files (XGBoost boosters, joblib files) once into memory at startup, preventing costly file reads during subsequent simulation calls.

### What project goal does this accomplish
Exposes a single, unified interface for all ML submodels, ensuring the game engine can fetch predictions, fourth-down ratios, and field goal success rates instantly.
