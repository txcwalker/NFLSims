"""Re-pulls the nflverse schedule (spread_line, total_line, moneylines, odds)
via nfl_data_py and overwrites data/external/schedule_{year}.csv in place.

nflverse's schedule feed is the one Vegas source already wired into the app
(see SCHEDULE_CSV_PATH in src/api/app.py) -- it's a single consensus snapshot,
not live per-book odds (that's the parked Odds API integration in
GOAL_TRACKER.md Tier 3), but it does move as books move lines pre-kickoff, and
the checked-in CSV only reflects whatever it looked like the day it was last
pulled. This gives that pull a one-call trigger instead of a manual R/Python
one-liner.

Shared by the /api/refresh_vegas_lines endpoint in src/api/app.py (button-
triggered from the frontend) -- see that endpoint for the request/response
shape. Deliberately no scheduled/cron path: Cam runs data refreshes like this
by hand (see refresh_weekly_dna_v_0_1_0.py precedent in WORKLOG.md).
"""

import os

import nfl_data_py as nfl
import pandas as pd

BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Columns whose movement is worth reporting back to the caller -- the odds
# fields a line move actually shows up in. (gameday/roof/stadium etc. can
# also drift row to row as nflverse corrects data, but that's not "the line
# moved" and would just make every refresh look noisier than it is.)
LINE_COLUMNS = [
    "spread_line", "total_line", "away_moneyline", "home_moneyline",
    "away_spread_odds", "home_spread_odds", "under_odds", "over_odds",
]


def refresh_vegas_lines(year: int = 2026, csv_path: str = None) -> dict:
    """Fetches this season's schedule fresh from nflverse and overwrites
    `csv_path` with it.

    Inputs: year (season to pull), csv_path (defaults to
    data/external/schedule_{year}.csv, matching SCHEDULE_CSV_PATH's naming).
    Outputs: overwrites csv_path on disk; returns a summary dict --
    {updated_at, total_games, games_with_line_change, games_changed: [game_id, ...]}.
    Purpose: the diff (old vs. new) is computed before the overwrite so the
    caller/UI can say "N lines moved" rather than just "refreshed".
    """
    if csv_path is None:
        csv_path = os.path.join(BASE_DIR, "data", "external", f"schedule_{year}.csv")

    new_df = nfl.import_schedules([year])

    changed_game_ids = []
    if os.path.exists(csv_path):
        old_df = pd.read_csv(csv_path)
        old_by_id = old_df.set_index("game_id")
        new_by_id = new_df.set_index("game_id")
        for game_id, new_row in new_by_id.iterrows():
            old_row = old_by_id.loc[game_id] if game_id in old_by_id.index else None
            for col in LINE_COLUMNS:
                old_val = None if old_row is None else old_row.get(col)
                new_val = new_row.get(col)
                # NaN != NaN, so treat "both missing" as unchanged rather than a diff.
                if pd.isna(old_val) and pd.isna(new_val):
                    continue
                if old_val != new_val:
                    changed_game_ids.append(game_id)
                    break

    # Write via a temp file + atomic replace so a crash mid-write can't leave
    # the CSV the whole app reads from truncated or half-written.
    tmp_path = csv_path + ".tmp"
    new_df.to_csv(tmp_path, index=False)
    os.replace(tmp_path, csv_path)

    return {
        "updated_at": pd.Timestamp.utcnow().isoformat(),
        "total_games": int(len(new_df)),
        "games_with_line_change": len(changed_game_ids),
        "games_changed": changed_game_ids,
    }


if __name__ == "__main__":
    import sys

    year_arg = int(sys.argv[1]) if len(sys.argv) > 1 else 2026
    result = refresh_vegas_lines(year_arg)
    print(
        f"Refreshed {result['total_games']} games for {year_arg}: "
        f"{result['games_with_line_change']} had a line move."
    )
    if result["games_changed"]:
        print("Changed:", ", ".join(result["games_changed"]))
