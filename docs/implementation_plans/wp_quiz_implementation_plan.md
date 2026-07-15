# Implementation Plan: Win Probability Intuition Quiz

This document outlines the implementation plan, design, and 20 real historical scenarios for the standalone **Win Probability Intuition Quiz**. The goal of this quiz is to measure human estimation of high-leverage and compressed game situations against roster-neutral Win Probability models.

---

## 1. Objectives & Value
*   **Demonstrate KEP Need:** Highlight the cognitive difficulty humans have in parsing compressed probabilities (e.g., distinguishing $95\%$ from $99\%$ WP late in a game, which represents a massive difference in kickoff-equivalent points).
*   **Establish human baseline calibration:** Gather data on how human estimation correlates with our roster-neutral WP model.
*   **Zero Leakage:** Host the quiz as a single standalone HTML/CSS/JS file completely isolated from the main website codebase.

---

## 2. Quiz Interface Design
The user interface should be extremely lightweight, premium, and easy to interact with:
*   **Slider Control:** A custom slider ($0\%$ to $100\%$) for estimating the win probability of Team A.
*   **Expected Margin:** A numerical field to capture expected margin (Points Team A wins/loses by).
*   **Confidence Rating:** A scale of 1-5 to rate their confidence.
*   **Visual Feedback:** Immediately after submitting a guess, show a gauge chart comparing their guess directly to the roster-neutral model's exact WP.

---

## 3. Data Collection Webhook
To collect responses anonymously and seamlessly:
*   Use a simple POST request to a free serverless webhook endpoint (e.g., Webhook.site, Formspree, or a Google Apps Script linked to a Google Sheet).
*   **Fields Sent:**
    *   `scenario_id` [1-20]
    *   `estimated_wp` (0-100)
    *   `estimated_margin` (points)
    *   `confidence` (1-5)
    *   `predicted_winner` (Team A or Team B)
    *   `time_taken` (seconds)

---

## 4. Curated Scenarios & Model WP (Team A is always possessing)

Every scenario uses the exact roster-neutral win probability calculated by our project's custom XGBoost model (`win_probability_v_0_1_0`).

| Scenario | Game Context | Score (A - B) | Situation & Position | Model WP (Team A) |
| :--- | :--- | :--- | :--- | :--- |
| **1** | 2023 DET @ KC Kickoff | 0 - 0 | Q1 15:00, 1st & 10 at Own 25 | **50.12%** |
| **2** | SB LVII KC trailing PHI | 0 - 7 | Q1 9:25, 1st & Goal at Opp 8 | **46.53%** |
| **3** | 2019 HOU @ KC early blowout | 0 - 24 | Q2 10:18, 1st & 10 at Own 26 | **20.00%** |
| **4** | SB LI 28-3 peak | 3 - 28 | Q3 8:31, 1st & 10 at Own 25 | **0.49%** |
| **5** | 2020 NFC CG LaFleur FG | 23 - 31 | Q4 2:09, 4th & Goal at Opp 8 (FG State) | **7.68%** |
| **6** | 2018 AFC CG 4th Down Go/No-Go | 0 - 7 | Q2 3:20, 4th & 1 at Opp 15 | **41.62%** |
| **7** | 2021 Divisional 13-Sec Drive | 33 - 36 | Q4 0:13, 1st & 10 at Own 25 | **9.47%** |
| **8** | 2022 Wild Card JAX Comeback | 14 - 30 | Q3 5:18, 1st & 10 at Own 20 | **7.15%** |
| **9** | SB XLIX Goal-Line Pick | 24 - 28 | Q4 0:26, 2nd & Goal at Opp 1 | **68.25%** |
| **10** | SB LII Philly Special | 15 - 12 | Q2 0:38, 4th & Goal at Opp 1 | **73.44%** |
| **11** | 2019 Divisional Seattle Trail | 17 - 28 | Q3 1:12, 1st & 10 at Opp 33 | **13.89%** |
| **12** | SB LIV Tied Q3 Start | 10 - 10 | Q3 12:00, 1st & 10 at Own 25 | **54.30%** |
| **13** | 2021 Divisional Bills trailing late | 21 - 26 | Q4 8:55, 1st & 10 at Own 25 | **23.62%** |
| **14** | 2006 Chargers Pats Fumble | 13 - 21 | Q4 6:25, 4th & 5 at Opp 41 | **9.55%** |
| **15** | 2021 Bengals Raiders Q4 | 16 - 26 | Q4 15:00, 1st & 10 at Own 25 | **14.89%** |
| **16** | SB LI Patriots down two scores | 0 - 14 | Q1 4:00, 1st & 10 at Own 25 | **18.96%** |
| **17** | 2023 PHI @ KC Close Q4 | 17 - 21 | Q4 7:00, 2nd & 10 at Own 35 | **31.43%** |
| **18** | 2021 Bengals @ Titans tied Q4 | 16 - 16 | Q4 2:42, 1st & 10 at Own 20 | **55.98%** |
| **19** | 2021 Chiefs @ Ravens clock grind | 36 - 35 | Q4 1:05, 4th & 1 at Own 43 | **84.98%** |
| **20** | SB XLVI Patriots backed up | 0 - 0 | Q1 11:20, 1st & 10 at Own 6 | **56.24%** |

---

## 5. Deployment Options
1.  **Vercel / Netlify Single Page:** Drag and drop the `index.html` file into Netlify Drop for an instant, secure, and isolated hosting link.
2.  **Raw File Distribution:** Email or share the standalone `html` file to test takers, as it runs entirely in the browser without any dependencies.
