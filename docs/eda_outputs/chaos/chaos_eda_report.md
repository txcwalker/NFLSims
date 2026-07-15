# NFLSims Chaos Model: Exploratory Data Analysis (EDA) Report

**Target Scope:** NFL play-by-play data from **2016 to 2025 seasons** (representing 403,639 total plays).
**Applies to:** Chaos simulation gates inside `game_engine.py`.

---

## 1. Detailed Penalty Type Distribution

Across the 2016�2025 seasons, a total of **32196** plays resulted in a penalty. The table below lists the top 20 most frequent penalties and their share of the total:

| Penalty Type | Count | Percentage |
| --- | --- | --- |
| Offensive Holding | 6021 | 18.70% |
| False Start | 5981 | 18.58% |
| Defensive Pass Interference | 2822 | 8.77% |
| Defensive Holding | 2051 | 6.37% |
| Unnecessary Roughness | 1659 | 5.15% |
| Defensive Offside | 1463 | 4.54% |
| Delay of Game | 1445 | 4.49% |
| Neutral Zone Infraction | 1181 | 3.67% |
| Roughing the Passer | 1113 | 3.46% |
| Illegal Block Above the Waist | 869 | 2.70% |
| Offensive Pass Interference | 814 | 2.53% |
| Face Mask | 814 | 2.53% |
| Illegal Use of Hands | 744 | 2.31% |
| Illegal Contact | 558 | 1.73% |
| Illegal Formation | 541 | 1.68% |
| Ineligible Downfield Pass | 415 | 1.29% |
| Intentional Grounding | 395 | 1.23% |
| Unsportsmanlike Conduct | 382 | 1.19% |
| Encroachment | 380 | 1.18% |
| Illegal Shift | 370 | 1.15% |

### Key Penalty Insights:
*   **Offensive Holding & False Starts**: Combined, these two infractions account for **37.28%** of all accepted penalties in the NFL. They represent the primary structural failures on offense.
*   **Defensive Pass Interference (DPI)**: DPI represents the single most frequent secondary penalty (**8.77%**), spiking on deep vertical targets.
*   **Roughing the Passer**: Accounted for **3.46%** of penalties, representing high-impact hits during the quarterback release phase.

---

## 2. Interceptions by Air Yards Depth (Clean Passes Only)

We analyzed the relationship between throw depth (`air_yards`) and interception rate:

| ay_bucket | total_passes | interceptions | int_rate |
| --- | --- | --- | --- |
| Deep (20+) | 21831 | 1353 | 6.20% |
| Intermediate (10-19) | 39454 | 1372 | 3.48% |
| Screens (<0) | 28905 | 224 | 0.77% |
| Short (0-9) | 94907 | 1355 | 1.43% |
| Unknown | 13154 | 0 | 0.00% |

*   **Deep Shots (20+ yards)** carry a massive **1.43%** interception rate�nearly **8 times higher** than screens.
*   *Average Air Yards on Interceptions*: **15.76 yards** compared to **5.95 yards** on clean completions.

---

## 3. Sacks and Trench Matchups

We evaluated how rolling offensive protection quality (`oline_tier`) and rolling defensive pass rush strength (`dline_tier`) dictate sack rates:

| oline_tier | dline_tier | total_passes | sacks | sack_rate |
| --- | --- | --- | --- | --- |
| Elite (Low Sacks) | Weak Rush | 27554 | 1559 | 5.66% |
| Elite (Low Sacks) | Average | 20627 | 1215 | 5.89% |
| Elite (Low Sacks) | Strong Rush | 18541 | 1076 | 5.80% |
| Average | Weak Rush | 31216 | 1975 | 6.33% |
| Average | Average | 23417 | 1603 | 6.85% |
| Average | Strong Rush | 22101 | 1568 | 7.09% |
| Poor (High Sacks) | Weak Rush | 22587 | 1769 | 7.83% |
| Poor (High Sacks) | Average | 17669 | 1328 | 7.52% |
| Poor (High Sacks) | Strong Rush | 14539 | 1159 | 7.97% |

*   **Elite O-Line vs. Weak D-Line** keeps sack rates at **5.66%**.
*   **Poor O-Line vs. Strong D-Line** drives sack rates up to **7.97%** (a 40.8% increase).

---

## 4. Sack-Fumble & Ball-Carrier Fumble Priors

Fumble probabilities vary drastically by the physical scenario of the play:

| Fumble Scenario | Prior Probability (Lost Fumble) | Key Insight |
| :--- | :--- | :--- |
| **Sack-Fumble (Lost Given Sack)** | **7.2442%** | Highest risk moment. Sacks force fumbles at a rate 11x higher than standard runs. |
| **Punt Return Fumble (Lost)** | **1.0746%** | Returns are highly chaotic, carrying double the fumble rate of offensive runs. |
| **Running Play Fumble (Lost)** | **0.6520%** | Baseline security rate for running backs. |
| **Completed Pass Fumble (Lost)** | **0.5246%** | Fumbles after catching in space. |
| **Field Goal Fumble (Lost)** | **0.0093%** | Extreme outlier (botched snaps/holds). |

---

## 5. Visualizations Reference
The generated premium charts are saved in `/visualizations/`:
*   [Interceptions by Depth Chart](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFLSims%20-%20Copy/visualizations/interception_by_air_yards.png)
*   [Penalty Rates by Play Phase](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFLSims%20-%20Copy/visualizations/penalty_rate_by_play_type.png)
*   [Sack Matchup Matrix Heatmap](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFLSims%20-%20Copy/visualizations/sack_rate_by_trench_matchup.png)
*   [Sack Yardage Distribution Curve](file:///c:/Users/txcwa/OneDrive/Desktop/Antigravity%20Projects/NFLSims%20-%20Copy/visualizations/sack_yardage_distribution.png)