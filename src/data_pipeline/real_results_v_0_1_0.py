"""Thin real-data pulls shared by build_actual_season_stats_2026.py (the
"Current Season" report) and run_full_season_sim_2026.py's additive mode
(real results for completed weeks + simulation for the rest). Both scripts
need the same two nfl_data_py pulls; this just avoids hitting the network
and reshaping team abbreviations twice.

No real per-play data source is published for 2026 yet (import_pbp_data /
import_weekly_data both 404 as of this writing) -- NGS is the only real
per-player counting-stat source available, and its own `week == 0` row per
player is already nflverse's season-to-date aggregate, so no manual
per-week summation is needed here.
"""

import nfl_data_py as nfl

# NGS's team_abbr for the Rams differs from the schedule/roster convention
# used everywhere else in this repo.
TEAM_ABBR_FIX = {'LAR': 'LA'}


def import_real_played_games(year: int):
    """Real REG-season games with a final score, from nfl_data_py's schedule."""
    sched = nfl.import_schedules([year])
    reg = sched[sched['game_type'] == 'REG']
    return reg[reg['home_score'].notna() & reg['away_score'].notna()].copy()


def _fix_team(df, col='team_abbr'):
    df = df.copy()
    df[col] = df[col].replace(TEAM_ABBR_FIX)
    return df


def import_real_player_ngs(year: int):
    """(passing, rushing, receiving) DataFrames, each nflverse's own
    season-to-date aggregate per player (NGS week==0 rows)."""
    out = []
    for kind in ('passing', 'rushing', 'receiving'):
        df = nfl.import_ngs_data(kind, [year])
        df = df[df['week'] == 0]
        out.append(_fix_team(df))
    return tuple(out)


REAL_TOTAL_FIELDS = ['pAtt', 'pCmp', 'pYds', 'pTD', 'int', 'rAtt', 'rYds', 'rTD', 'rec', 'recYds', 'recTD', 'targets']


def real_player_totals_by_name(passing, rushing, receiving):
    """{(team, player_display_name): {pAtt, pCmp, pYds, pTD, int, rAtt, rYds,
    rTD, rec, recYds, recTD, targets}} season-to-date real totals, keyed to
    match against the sim's own roster player names for the additive
    rest-of-season offset. Sums a player's passing+rushing (QBs) or
    rushing+receiving (skill players) rows together under one name key.

    fumbles/sacks_taken/air_yards/dk_score/fd_score/std_score/DST fields
    are deliberately NOT here -- no real per-play source exists for 2026
    yet (see module docstring), so those stay purely simulated for a
    completed week's contribution. std_score/dk_score/fd_score should be
    (re)computed from these fields via calculate_fantasy_points() by the
    caller, not looked up here.
    """
    totals = {}

    def _get(team, name):
        key = (team, name)
        if key not in totals:
            totals[key] = {f: 0.0 for f in REAL_TOTAL_FIELDS}
        return totals[key]

    for _, p in passing.iterrows():
        t = _get(p['team_abbr'], p['player_display_name'])
        t['pAtt'] += p['attempts']
        t['pCmp'] += p['completions']
        t['pYds'] += p['pass_yards']
        t['pTD'] += p['pass_touchdowns']
        t['int'] += p['interceptions']
    for _, r in rushing.iterrows():
        t = _get(r['team_abbr'], r['player_display_name'])
        t['rAtt'] += r['rush_attempts']
        t['rYds'] += r['rush_yards']
        t['rTD'] += r['rush_touchdowns']
    for _, r in receiving.iterrows():
        t = _get(r['team_abbr'], r['player_display_name'])
        t['targets'] += r['targets']
        t['rec'] += r['receptions']
        t['recYds'] += r['yards']
        t['recTD'] += r['rec_touchdowns']

    return totals
