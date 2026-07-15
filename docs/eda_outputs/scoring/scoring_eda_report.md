# 🏈 NFL Scoring EDA: Plays, Yards, and Points

This report presents the Exploratory Data Analysis (EDA) of the relationship between offensive plays, yards gained, and final points scored across NFL team-games from 2015 to 2024.

## 📊 Summary Statistics

- **Total Team-Game Observations**: 5357
- **Average Points Scored**: 22.86 (Std: 9.96)
- **Average Plays**: 62.53 (Std: 8.36)
- **Average Total Yards**: 345.99 (Std: 84.08)
  - **Passing Yards**: 230.60 (Std: 76.76)
  - **Rushing Yards**: 115.39 (Std: 51.40)

## 📈 Linear Regression Analyses

Two separate simple linear regressions were performed to estimate the impact of plays and yards on points scored:

### 1. Regression: Points vs. Plays
- **Formula**: `Points = 9.0181 + 0.2214 * Plays`
- **R-squared**: `0.0345`
- **Plays Coefficient**: `0.2214` (p-value: `0.0000`)
- **Interpretation**: Offensive volume (number of plays) alone is a relatively weak predictor of scoring. A 10-play increase in a game is associated with an increase of only ~1.5 points.

### 2. Regression: Points vs. Total Yards Gained
- **Formula**: `Points = -3.7799 + 0.0770 * Yards`
- **R-squared**: `0.4222`
- **Yards Coefficient**: `0.0770` (p-value: `0.0000`)
- **Interpretation**: Total yards gained is a strong predictor of points scored, explaining 47.9% of the variation in scoring. For every 100 additional yards gained, a team scores approximately 7.2 more points.

### 3. Multiple Regression (Contextual)
- **Formula**: `Points = 7.0818 + 0.1092 * RushingYards + 0.0836 * PassingYards + -0.2574 * Plays`
- **R-squared**: `0.4703`
- **Rushing Yards Coeff**: `0.1092` (p-value: `0.0000`)
- **Passing Yards Coeff**: `0.0836` (p-value: `0.0000`)
- **Plays Coeff**: `-0.2574` (p-value: `0.0000`)
- **Interpretation**: Interestingly, when controlling for yardage efficiency, the coefficient on plays becomes negative (`-0.2574`). This indicates that for a given amount of yards, running more plays (lower yards per play) actually leads to *fewer* points, highlighting the importance of play efficiency over raw volume.

## 📊 Distributions at Constant Scores (21, 24, 27, 30)

Below are the average plays and yards for teams when their final score is held constant at typical scoring benchmarks:

| Final Score | N (Games) | Avg Plays | Avg Rushing Yards | Avg Passing Yards | Avg Total Yards |
| :---: | :---: | :---: | :---: | :---: | :---: |
| **21** | 175 | 61.5 | 109.7 | 219.7 | 329.4 |
| **24** | 325 | 61.9 | 120.6 | 229.9 | 350.5 |
| **27** | 312 | 63.6 | 120.6 | 251.4 | 372.0 |
| **30** | 200 | 64.9 | 136.2 | 258.2 | 394.5 |

### Distribution Visualizations
- **Plays Distribution**: [plays_distribution.png](plays_distribution.png)
- **Yards Distribution (Rushing vs Passing)**: [yards_distribution.png](yards_distribution.png)
- **Regression Analysis Plot**: [regression_fits.png](regression_fits.png)
