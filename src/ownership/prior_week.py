"""Prior-week performance signal for the ownership model (added 2026-09-22).

Sentinel encoding, per Cam (2026-09-22/23 design discussion):
  -99  = genuinely never seen before in our archive -- true rookie/debut,
         whether that's literally week 1 or a later week ("for rookies lets
         go with -99 whether it is week 1 or not is their week 1")
  -76  = returning from a 3+ consecutive-week absence while SALARIED each of
         those weeks -- Cam's call: "even injured players have salaries", so
         a real injury shows up as priced-but-scored-0 repeatedly, not as a
         missing salary row. A single scored-0 week is normal, not flagged
         ("a player scoring 0 in one game is totally possible").
  -51  = the player's team was on a bye the prior week
  0    = no salary listing at all the prior week, but the player HAS
         appeared in the archive at some earlier week -- practice-squad
         elevation, a re-signed veteran, a return from retirement. Cam's
         call: "0 ... is most representative of their previous weeks",
         since there's no way to tell how long they'd been off the pool.
  else = the player's real DK score the prior week

Shared by build_ownership_dataset.py's training-time enrichment and
model_inference.py's live row-building so both compute this identically.
Lives in src/ (not scripts/) on purpose: scripts/ already imports from src/
(see train_ownership_model.py importing src.ownership.heuristic), so this
keeps that one-way dependency direction instead of having the always-loaded
live API import from the scripts/ tree, where a broken import would take the
whole server down with it.
"""
from __future__ import annotations

import os

import pandas as pd

BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
ARCHIVE = os.path.join(BASE_DIR, "data", "dfs_ownership")
SCHEDULE_CSV_PATH = os.path.join(BASE_DIR, "data", "external", "schedule_2026.csv")

_salary_keys_cache: dict = {}   # (year, week) -> {(normalized_name, team)}
_score_cache: dict = {}         # (year, week) -> {(normalized_name, team): score}
_teams_played_cache: dict = {}  # (year, week) -> {team}


def clear_cache() -> None:
    """Call after archiving a new settled week within a long-lived process
    (the API server) -- otherwise a week that goes final mid-session stays
    invisible to these lookups."""
    _salary_keys_cache.clear()
    _score_cache.clear()
    _teams_played_cache.clear()


def _teams_played(year: int, week: int) -> set:
    key = (year, week)
    if key in _teams_played_cache:
        return _teams_played_cache[key]
    teams: set = set()
    if os.path.exists(SCHEDULE_CSV_PATH):
        sched = pd.read_csv(SCHEDULE_CSV_PATH)
        wk = sched[(sched["week"] == week) & (sched["game_type"] == "REG")]
        teams = set(wk["home_team"]) | set(wk["away_team"])
    _teams_played_cache[key] = teams
    return teams


def _salary_keys(year: int, week: int) -> set:
    """{(normalized_name, team)} priced anywhere that week -- main slate
    UNION every showdown slate.

    NOT main-slate-only (the original 2026-09-22 version was, and it was
    wrong): DK's classic "Main Slate" deliberately excludes Thursday/Monday/
    Sunday-Night games -- confirmed 2026-09-22 by cross-referencing which
    teams were missing from week 3's real main-slate snapshot against the
    schedule (exactly the Thu/SNF/MNF matchups, nothing else). A player on
    one of those teams is still genuinely rostered/playable that week, just
    through a different DK product (their own classic slate and/or a
    showdown), so main-slate-only wrongly read them as "not in the pool" --
    exactly the rookie/practice-squad signal this module exists to detect,
    but for the wrong reason. Unioning every showdown_* folder for the week
    covers the gap for any of those teams we happened to snapshot a
    showdown for; a genuinely un-snapshotted primetime team still falls
    through to the same graceful "not found" handling as any other gap."""
    key = (year, week)
    if key in _salary_keys_cache:
        return _salary_keys_cache[key]
    from src.scrapers.dk_scraper import normalize_player_name
    import glob
    week_dir = os.path.join(ARCHIVE, str(year), f"week_{week:02d}")
    out: set = set()
    for path in glob.glob(os.path.join(week_dir, "*", "salaries_prelock.csv")):
        df = pd.read_csv(path)
        out |= set(zip(df["name"].map(normalize_player_name), df["team"]))
    _salary_keys_cache[key] = out
    return out


def _scores(year: int, week: int) -> dict:
    """{(normalized_name, team): actual_dk_score} for a settled week, from
    ownership_actuals.parquet -- deduped across that week's contests (every
    contest carries the same real score for a given player)."""
    key = (year, week)
    if key in _score_cache:
        return _score_cache[key]
    from src.scrapers.dk_scraper import normalize_player_name
    path = os.path.join(ARCHIVE, "_processed", "ownership_actuals.parquet")
    out: dict = {}
    if os.path.exists(path):
        df = pd.read_parquet(path, columns=["year", "week", "player", "team", "actual_dk_score"])
        sub = df[(df["year"] == year) & (df["week"] == week)].dropna(subset=["actual_dk_score"])
        sub = sub.drop_duplicates(subset=["player", "team"])
        out = {(normalize_player_name(r.player), r.team): float(r.actual_dk_score) for r in sub.itertuples()}
    _score_cache[key] = out
    return out


def prior_week_feature(name: str, team: str, year: int, week: int, max_lookback: int = 8) -> float:
    """The sentinel-coded prior-week feature for `name`/`team` heading into
    `week` -- see module docstring for the five cases. `max_lookback` bounds
    how far back the -76 injury-streak walk goes; running off the front of
    the archive (week < 1) ends the walk the same as any other stop
    condition."""
    if week is None or week <= 1:
        return -99.0

    from src.scrapers.dk_scraper import normalize_player_name
    key = (normalize_player_name(name), team)
    prev = week - 1

    if team not in _teams_played(year, prev):
        return -51.0

    if key not in _salary_keys(year, prev):
        # Never salaried last week -- rookie/debut vs. practice-squad/
        # re-signed veteran hinges on whether we've EVER seen them salaried
        # before, at any earlier archived week (prev itself already known
        # False, so start the search at prev - 1).
        for w in range(prev - 1, 0, -1):
            if key in _salary_keys(year, w):
                return 0.0
            if prev - w >= max_lookback:
                break
        return -99.0

    # Salaried last week -- walk back the injury streak: consecutive
    # salaried-but-scored-0 weeks, skipping byes (a bye doesn't count either
    # way -- no game, no score expected). A genuine non-salaried gap ends
    # the streak rather than extending it; that's a different situation
    # (handled by the branch above on ITS OWN week), not part of this one.
    streak = 0
    w = prev
    while w >= 1 and prev - w < max_lookback:
        if team not in _teams_played(year, w):
            w -= 1
            continue
        if key not in _salary_keys(year, w):
            break
        if _scores(year, w).get(key) == 0.0:
            streak += 1
            w -= 1
            continue
        break

    if streak >= 3:
        return -76.0

    score = _scores(year, prev).get(key)
    return float(score) if score is not None else 0.0
