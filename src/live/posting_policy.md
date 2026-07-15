# posting_policy.py — Social Posting Policy & Narrative Formatter

### Why do we need it
A live sports bot should not flood social feeds with trivial or obvious decisions (e.g. punting on 4th & 25 from own 10-yard line). This file implements the strategic gating rules and formatting engines to ensure only high-value, analytically interesting decisions or major coaching mistakes get posted.

### What is it doing
* **Mistake Detection**: Identifies clear coaching errors when the called action differs from the model's recommended choice and represents a loss of 3% or more in win probability (`wp_gap >= 0.03`).
* **Obviousness Filtration**: Blocks posting of correct decisions if they were extremely obvious (e.g. spread between options >= 30% WP, or a "must go" situation where a punt's WP < 10%).
* **Contextual Gating**: Focuses coverage on high-leverage plays (e.g., margins <= 8 points in one-score games, standalone primetime matchups, or late-game scenarios).
* **Cooldown Enforcement**: Ensures the bot respects SQLite-tracked rate limits (e.g. max 12 posts per hour globally, no duplicate posts on the same drive, and a 45–60s cooldown between game posts).
* **Narrative Formatter**: Constructs engaging post copy complete with game situation headers, win probabilities for each scenario, decision evaluations, and trending tags.

### Subject matter expertise utilized
* **Leverage Calculations**: Computes leverage as the difference between the best option's WP and the average of all valid options, ensuring only high-impact plays pass the gate.
* **Obviousness Margins**: Uses a 30% WP spread threshold to define "obviousness", filtering out standard decisions so the feed remains analytical and unique.

### Decisions made & metrics/reasoning used
* **Mistake Override**: Sets mistake plays (`clear_mistake`) to bypass obviousness blocks and game cooldown gates entirely, ensuring major coaching blunders are posted instantly.
* **Pacing Thresholds**: Sets game cooldowns dynamically (45s for standalone primetime slots, 60s for standard games) to balance posting frequency across different audience sizes.

### Why this metric vs alternatives
* **Expected Win Probability Gap vs Raw Win Probability**: Relying on the expected win probability gap (best option WP minus called option WP) is superior to using absolute win percentages because it focuses purely on the quality of the coaching decision itself, regardless of whether the team is winning or losing.

### What project goal does this accomplish
Enforces the editorial policy and formatting criteria of the bot, keeping social feeds engaging, analytical, and highly professional.
