# 📊 NFLSims Scoring Deficiency Diagnostic Report

This report compares key play-by-play execution metrics between **1,000 simulated games (V.0.2.0)** and **10 years of NFL play-by-play data (2016–2025)** to isolate why simulations project games to score approximately 10 points below the NFL average.

---

## 📈 1. Diagnostics Comparison Matrix

| Metric | 10-Yr NFL Average | Simulated V.0.2.0 Average | Variance / Discrepancy |
| :--- | :---: | :---: | :---: |
| **Avg Starting Field Position (yds to endzone)** | **71.2** | **75.0** | +3.8 yards (deeper) |
| **Average Pass Gain (clean plays, yards)** | **7.11** | **6.78** | -0.33 yards |
| **Average Run Gain (clean plays, yards)** | **4.45** | **4.42** | -0.03 yards |
| **3rd Down Conversion Rate (Overall)** | **40.2%** | **43.7%** | +3.5% |
| **3rd Down Conv (Short: 1–3 yds)** | **60.4%** | **61.9%** | +1.5% |
| **3rd Down Conv (Medium: 4–6 yds)** | **44.2%** | **48.2%** | +4.0% |
| **3rd Down Conv (Long: 7+ yds)** | **26.0%** | **30.9%** | +4.8% |
| **Explosive Pass Rate (gains >= 20 yds)** | **8.89%** | **8.75%** | -0.14% |
| **Explosive Run Rate (gains >= 20 yds)** | **2.49%** | **2.43%** | -0.06% |
| **Sack Rate per Pass Play** | **6.53%** | **5.12%** | -1.40% |

---

## 🔍 2. Analysis of Discrepancies & Stalled Drives

Based on the diagnostic metrics, we have identified the following critical bottlenecks:

### 1. The Starting Field Position Penalty
* **Finding**: Simulated drives start on average at **75.0** yards to the endzone, whereas actual NFL drives start at **71.2**. 
* **Impact**: Our simulations require offenses to travel **~3.8 yards further** on every single drive. This is due to a combination of punting distance bias (over-punting/under-returning) and kickoffs lacking touchback vs return variance.

### 2. 3rd Down Conversion Stalls
* **Finding**: The simulated 3rd down conversion rate of **43.7%** lags behind the historical baseline (**40.2%**).
* **Impact**: The deficit is particularly pronounced on **3rd-and-long (30.9% vs 26.0%)** and **3rd-and-medium (48.2% vs 44.2%)**. When offenses face these distances, the engine's routes (air yards) or tight-window catch rates are slightly too conservative, forcing premature punts.

### 3. Yardage and Explosive Play Rates
* **Finding**: The simulated average pass gain of **6.78 yards** is lower than the NFL baseline (**7.11 yards**).
* **Impact**: The primary driver of this yardage deficit is the **Explosive Pass Rate (8.75% vs 8.89%)**. A 1% deficit in explosive passes translates to roughly ~2 less big gains per game, taking away chunk yards that flip field position and directly lead to quick points.

---

## 🛠️ 3. Proposed Resolution Plan

To resolve the 10-point scoring deficit and align the diagnostics with the 10-year NFL standard, we should focus on:
1. **Starting Field Position Adjustments**: Tune punting, kickoffs, and return yardage distributions so the average starting field position sits closer to 72.5 yards to the end zone.
2. **Chunk Yards & Explosive Play Tuning**: Add slight vertical scaling to the air yards and YAC residual distributions to match the historical **8.89%** explosive pass rate.
3. **3rd Down Conversions**: Re-evaluate route-depth selection on 3rd down, ensuring quarterbacks target at or past the sticks rather than dumping off short of the first down marker.
