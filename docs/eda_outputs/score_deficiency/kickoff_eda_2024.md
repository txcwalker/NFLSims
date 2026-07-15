# 🏈 2024 NFL Kickoff Rules & Field Position EDA

This report analyzes kickoffs during the **2024 NFL regular season** (the first season of the new dynamic kickoff rules) to determine touchback rates and the resulting starting field positions.

---

## 📈 1. Key Kickoff Metrics (2024)

| Metric | Value | Meaning / Context |
| :--- | :---: | :--- |
| **Total Kickoffs Analyzed** | **2803** | All regular season kickoffs |
| **Touchback Rate** | **64.3%** | Percentage of kickoffs resulting in a touchback |
| **Return Rate** | **35.7%** | Percentage of kickoffs that were returned |
| **Avg Starting Field Position (Overall)** | **70.1 yds** to go | Average drive start at the **29.9-yard line** |
| **Avg Start on Touchbacks** | **70.2 yds** to go | Average drive start at the **29.8-yard line** |
| **Avg Start on Returns** | **69.8 yds** to go | Average drive start at the **30.2-yard line** |

---

## 🔍 2. Analysis of the 2024 Dynamic Kickoff Rule
Under the 2024/2025 kickoff rules:
1. **Touchback to the 30-Yard Line**: A touchback on a kickoff that lands in the end zone or is kicked out of the end zone in the air spots the ball at the **30-yard line** (70 yards to go).
2. **Touchback to the 20-Yard Line**: A touchback where the ball lands in the landing zone, rolls into the end zone, and is downed spots the ball at the **20-yard line** (80 yards to go).
3. **Returns**: Returned kickoffs in 2024 averaged a starting position at the **30.2-yard line**, as coverage teams were set up further downfield.

### 🛠️ Proposed Simulator Calibration
To match the **2024/2025 NFL kickoff environment**:
* Instead of hardcoding all kickoffs to **75.0 yards** (25-yard line), we should roll a distribution for kickoff starting field position:
  * **64.3%** chance of a Touchback: Spot the ball at the **30-yard line** (70 yards to go) or **20-yard line** (80 yards to go), averaging **70.2 yards** to go.
  * **35.7%** chance of a Return: Spot the ball with a distribution centered around **69.8 yards** to go (e.g. `Normal(28.5, 5.0)`).
