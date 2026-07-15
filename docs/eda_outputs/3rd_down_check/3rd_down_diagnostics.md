# 📊 NFLSims 3rd Down Deep-Dive EDA Report

This report provides a detailed comparison of **play calling, average gains, and conversion success rates** on 3rd downs between **1,000 simulated games** and **10 years of NFL play-by-play data (2016-2025)**.

---

## 📈 1. Side-by-Side Comparison Matrix

### 🏈 3rd & Short (1–3 yards to go)

| Metric | NFL 10-Yr Avg | Simulation Avg | Delta / Variance |
| :--- | :---: | :---: | :---: |
| **Overall Conversion Rate** | **60.4%** | **67.7%** | +7.3% |
| Pass Play Selection % | 51.0% | 46.9% | -4.2% |
| Run Play Selection % | 49.0% | 53.1% | +4.2% |
| Pass Conversion Rate | 53.1% | 65.6% | +12.5% |
| Run Conversion Rate | 68.1% | 69.6% | +1.5% |
| Avg Yards Gained (Pass) | 5.52 yds | 4.46 yds | -1.06 yds |
| Avg Yards Gained (Run) | 3.57 yds | 2.80 yds | -0.77 yds |
| Sack Rate per Pass | 6.9% | 4.9% | -2.0% |

### 🏈 3rd & Medium (4–6 yards to go)

| Metric | NFL 10-Yr Avg | Simulation Avg | Delta / Variance |
| :--- | :---: | :---: | :---: |
| **Overall Conversion Rate** | **44.2%** | **51.8%** | +7.6% |
| Pass Play Selection % | 86.8% | 85.4% | -1.4% |
| Run Play Selection % | 13.2% | 14.6% | +1.4% |
| Pass Conversion Rate | 43.5% | 53.9% | +10.4% |
| Run Conversion Rate | 49.1% | 39.4% | -9.7% |
| Avg Yards Gained (Pass) | 5.43 yds | 4.26 yds | -1.17 yds |
| Avg Yards Gained (Run) | 5.70 yds | 3.86 yds | -1.85 yds |
| Sack Rate per Pass | 9.7% | 6.9% | -2.7% |

### 🏈 3rd & Long (7+ yards to go)

| Metric | NFL 10-Yr Avg | Simulation Avg | Delta / Variance |
| :--- | :---: | :---: | :---: |
| **Overall Conversion Rate** | **26.0%** | **31.5%** | +5.4% |
| Pass Play Selection % | 87.6% | 88.3% | +0.6% |
| Run Play Selection % | 12.4% | 11.7% | -0.6% |
| Pass Conversion Rate | 26.3% | 33.4% | +7.1% |
| Run Conversion Rate | 24.2% | 16.8% | -7.4% |
| Avg Yards Gained (Pass) | 5.81 yds | 4.61 yds | -1.20 yds |
| Avg Yards Gained (Run) | 6.92 yds | 3.41 yds | -3.51 yds |
| Sack Rate per Pass | 10.8% | 8.1% | -2.8% |

## 🔍 2. Deep-Dive Diagnostics & Root Cause Analysis

Based on the side-by-side matrices, here are the critical failure modes identified:

### ⚠️ The 3rd & Short (1-3 yards) Meltdown
1. **Pass/Run Split**: In actual NFL games, teams run the ball on 3rd & short about **45-50%** of the time because it is high-probability short-yardage. Look at how our simulation calls plays here compared to real life.
2. **Short Yardage Gains**: Real-life 3rd & short runs average over **3-4 yards**, which is enough to move the chains. In the simulation, check if the run game fails to pick up the 1-2 yards needed or if there's a truncation/scaling issue.
3. **Pass Success**: On 3rd & short, pass completions should be high (due to quick slants/flat routes). If simulation pass conversion is low, it suggests route-depth selection is either too deep or completion rates are artificially low in tight coverage.

### ⚙️ Engine Level Observations
* Note if there are any specific lines of code in the engine where the yards gained is computed or where the route depth is selected that might explain the disparity.
