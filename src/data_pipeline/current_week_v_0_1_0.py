"""Determines the current NFL week from a season's schedule CSV + today's
date. Shared by src/api/app.py (Current Season endpoints) and
run_full_season_sim_2026.py (additive rest-of-season cutoff) so both agree
on the same week without duplicating the logic.
"""

import os
from datetime import date, datetime

import pandas as pd

BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def get_current_week(year: int = 2026, schedule_path: str = None, today: date = None) -> int:
    """Returns the earliest REG-season week that isn't fully in the past yet
    (i.e. has at least one game on or after `today`). Falls back to the
    season's final REG week if every game's date has already passed, and to
    week 1 if the schedule file is missing or empty.
    """
    if schedule_path is None:
        schedule_path = os.path.join(BASE_DIR, "data", "external", f"schedule_{year}.csv")
    if today is None:
        today = datetime.now().date()

    if not os.path.exists(schedule_path):
        return 1

    sched = pd.read_csv(schedule_path)
    reg = sched[sched["game_type"] == "REG"].copy()
    if reg.empty:
        return 1
    reg["gameday"] = pd.to_datetime(reg["gameday"]).dt.date

    upcoming = reg[reg["gameday"] >= today]
    if upcoming.empty:
        return int(reg["week"].max())
    return int(upcoming.sort_values("gameday").iloc[0]["week"])
