# NFLSims API Contract

This document acts as the source of truth mapping all API interactions between the React frontend and the FastAPI backend.

---

## 1. Health Check
* **Endpoint:** `GET /api/health`
* **Description:** Lightweight endpoint for frontend health checking and showing status dots.
* **Response Body:**
  ```json
  {
    "status": "ok",
    "version": "1.0.0"
  }
  ```

---

## 2. Simulate Slate Matchup
* **Endpoint:** `POST /api/simulate`
* **Description:** Aggregates simulated plays, yards, touchdowns, turnovers, and computes win probabilities and fantasy scores.
* **Request Body (JSON):**
  ```json
  {
    "away_team": "MIA",
    "home_team": "IND",
    "year": 2025,
    "iterations": 10000,
    "spread_override": null,
    "total_override": null,
    "apply_weighting": false,
    "team_overrides": {},
    "player_overrides": []
  }
  ```
* **Response Body (JSON):**
  ```json
  {
    "summary": {
      "away_avg_score": 23.0,
      "home_avg_score": 24.5,
      "win_probability_away": 46.3,
      "win_probability_home": 53.7,
      "away_cover_rate": 50.0,
      "home_cover_rate": 50.0,
      "over_probability": 50.0,
      "under_probability": 50.0,
      "away_plays": 61.2,
      "home_plays": 62.4,
      "away_pass_yds": 218.4,
      "home_pass_yds": 232.0,
      "away_rush_yds": 103.1,
      "home_rush_yds": 114.5,
      "away_projected_tds": 3.15,
      "home_projected_tds": 3.36,
      "away_turnovers": 1.2,
      "home_turnovers": 1.1
    },
    "projections": [
      {
        "name": "Tua Tagovailoa",
        "pos": "QB",
        "team": "MIA",
        "salary": 7200,
        "dk_points": 18.42,
        "fd_points": 15.65,
        "dk_value": 2.56,
        "fd_value": 2.17,
        "pAtt": 32.4,
        "pCmp": 21.1,
        "pYds": 218.4,
        "pTD": 1.62,
        "int": 0.65,
        "rAtt": 2.1,
        "rYds": 8.4,
        "rTD": 0.05,
        "targets": 0.0,
        "rec": 0.0,
        "recYds": 0.0,
        "recTD": 0.0,
        "fumbles": 0.1,
        "sacks_taken": 1.8,
        "dk_pcts_all": [...],
        "fd_pcts_all": [...]
      }
    ],
    "score_density": [
      {
        "range": "35-38",
        "probability": 12.5
      }
    ]
  }
  ```

---

## 3. Weekly Projections
* **Endpoint:** `GET /api/week_projections?week={week}`
* **Description:** Retrieve week-wide projections and stats.
* **Response Body (JSON):**
  ```json
  {
    "week": 1,
    "players": [
      {
        "name": "Dak Prescott",
        "pos": "QB",
        "team": "DAL",
        "opponent": "@PHI",
        "salary": 6800,
        "dk_points": 17.52,
        "fd_points": 15.01,
        "pAtt": 34.2,
        "pCmp": 22.4,
        "pYds": 248.5,
        "pTD": 1.55,
        "int": 0.72,
        "rAtt": 1.8,
        "rYds": 7.5,
        "rTD": 0.08,
        "fumbles": 0.1
      }
    ],
    "games": [...]
  }
  ```
