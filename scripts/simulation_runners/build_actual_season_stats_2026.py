"""Builds REAL, actual 2026 season-to-date standings/team-stats/leaders/teams
reports for the analytics site's "Current Season" page -- as opposed to
run_full_season_sim_2026.py's hypothetical/simulated "Rest of Season" report.

Data sources (both real, no simulation):
  - nfl_data_py.import_schedules([2026]) -- real final scores, for standings.
  - nfl_data_py.import_ngs_data(..., [2026]) -- Next Gen Stats passing/
    rushing/receiving. Its own `week == 0` row is NGS's pre-aggregated
    season-to-date total per player, so no manual week-by-week summing is
    needed here. NGS uses 'LAR' for the Rams where the rest of this repo
    uses 'LA' (TEAM_ABBR_FIX below).

Known gaps (no real per-play data source is published for 2026 yet --
nfl_data_py's import_pbp_data/import_weekly_data both 404 as of this
writing): fumbles and sacks_taken are not available from NGS, so they're
left at 0 for every player (std_score is understated by a small amount for
QBs who've been sacked or fumbled). DST stats and the "rookies" leader
split are not built here (would need PBP / roster join work) -- Current
Season leaders are QB/RB/WR/TE "overall" only until a real PBP source is
available. Revisit both once nfl_data_py publishes 2026 PBP.

Run by hand after each week completes (same manual pattern as
refresh_weekly_dna_v_0_1_0.py -- not scheduled/automated).

Usage:
    venv\\Scripts\\python.exe scripts/simulation_runners/build_actual_season_stats_2026.py
"""

import sys
import os
import json

sys.path.append(os.getcwd())

import pandas as pd  # noqa: E402

from src.data_pipeline.current_week_v_0_1_0 import get_current_week  # noqa: E402
from src.data_pipeline.real_results_v_0_1_0 import import_real_played_games, import_real_player_ngs  # noqa: E402
from src.nfl_sim.scoring import calculate_fantasy_points  # noqa: E402

SIM_YEAR = 2026
OUT_DIR = os.path.join("docs", "reports", "season_actuals_2026")

TEAM_DIVISIONS = {
    'BUF': ('AFC', 'East'), 'MIA': ('AFC', 'East'), 'NE': ('AFC', 'East'), 'NYJ': ('AFC', 'East'),
    'BAL': ('AFC', 'North'), 'CIN': ('AFC', 'North'), 'CLE': ('AFC', 'North'), 'PIT': ('AFC', 'North'),
    'HOU': ('AFC', 'South'), 'IND': ('AFC', 'South'), 'JAX': ('AFC', 'South'), 'TEN': ('AFC', 'South'),
    'DEN': ('AFC', 'West'), 'KC': ('AFC', 'West'), 'LV': ('AFC', 'West'), 'LAC': ('AFC', 'West'),
    'DAL': ('NFC', 'East'), 'NYG': ('NFC', 'East'), 'PHI': ('NFC', 'East'), 'WAS': ('NFC', 'East'),
    'CHI': ('NFC', 'North'), 'DET': ('NFC', 'North'), 'GB': ('NFC', 'North'), 'MIN': ('NFC', 'North'),
    'ATL': ('NFC', 'South'), 'CAR': ('NFC', 'South'), 'NO': ('NFC', 'South'), 'TB': ('NFC', 'South'),
    'ARI': ('NFC', 'West'), 'LA': ('NFC', 'West'), 'SF': ('NFC', 'West'), 'SEA': ('NFC', 'West'),
}

def build_standings(played_games):
    stats = {
        team: {'wins': 0.0, 'losses': 0.0, 'ties': 0.0, 'pf': 0.0, 'pa': 0.0,
               'div_wins': 0.0, 'div_losses': 0.0, 'div_ties': 0.0, 'games_played': 0}
        for team in TEAM_DIVISIONS
    }
    for _, g in played_games.iterrows():
        away, home = g['away_team'], g['home_team']
        away_score, home_score = g['away_score'], g['home_score']
        div = bool(g['div_game'])
        stats[away]['pf'] += away_score
        stats[away]['pa'] += home_score
        stats[home]['pf'] += home_score
        stats[home]['pa'] += away_score
        stats[away]['games_played'] += 1
        stats[home]['games_played'] += 1
        if away_score > home_score:
            stats[away]['wins'] += 1
            stats[home]['losses'] += 1
            if div:
                stats[away]['div_wins'] += 1
                stats[home]['div_losses'] += 1
        elif home_score > away_score:
            stats[home]['wins'] += 1
            stats[away]['losses'] += 1
            if div:
                stats[home]['div_wins'] += 1
                stats[away]['div_losses'] += 1
        else:
            stats[away]['ties'] += 1
            stats[home]['ties'] += 1
            stats[away]['wins'] += 0.5
            stats[away]['losses'] += 0.5
            stats[home]['wins'] += 0.5
            stats[home]['losses'] += 0.5
            if div:
                stats[away]['div_wins'] += 0.5
                stats[away]['div_losses'] += 0.5
                stats[home]['div_wins'] += 0.5
                stats[home]['div_losses'] += 0.5

    rows = []
    for team, s in stats.items():
        conf, div = TEAM_DIVISIONS[team]
        rows.append({
            'Team': team, 'Conference': conf, 'Division': div,
            'Wins_Expected': s['wins'], 'Losses_Expected': s['losses'],
            'PF_Avg': s['pf'], 'PA_Avg': s['pa'],
            'Div_Wins_Avg': s['div_wins'], 'Div_Losses_Avg': s['div_losses'],
            # No Playoffs/SB probabilities here -- those are projections
            # and stay on the Rest of Season page. Kept at 0 so any shared
            # frontend component can still call .toFixed() on them safely
            # even when not displayed.
            'Division_%': 0.0, 'Playoffs_%': 0.0, 'Champion_%': 0.0,
            'games_played': s['games_played'],
        })
    return rows


def build_team_stats(passing, rushing, standings_rows):
    pass_by_team = passing.groupby('team_abbr')['pass_yards'].sum()
    rush_by_team = rushing.groupby('team_abbr')['rush_yards'].sum()
    rows = []
    for r in standings_rows:
        team = r['Team']
        pYds = float(pass_by_team.get(team, 0.0))
        rYds = float(rush_by_team.get(team, 0.0))
        rows.append({
            'Team': team,
            'PF_avg': r['PF_Avg'], 'PA_avg': r['PA_Avg'],
            'PointDiff_avg': r['PF_Avg'] - r['PA_Avg'],
            'pYds_avg': pYds, 'rYds_avg': rYds, 'TotalYds_avg': pYds + rYds,
            'games_played': r['games_played'],
        })
    return rows


def _stat_line(pass_row=None, rush_row=None, rec_row=None):
    """Builds a calculate_fantasy_points()-compatible stats dict from
    whichever NGS rows exist for this player, real counting stats only.
    fumbles/sacks_taken stay 0 -- see module docstring (no PBP source yet)."""
    s = {'pYds': 0, 'pTD': 0, 'int': 0, 'rYds': 0, 'rTD': 0, 'rec': 0, 'recYds': 0, 'recTD': 0, 'fumbles': 0}
    if pass_row is not None:
        s['pYds'] = pass_row['pass_yards']
        s['pTD'] = pass_row['pass_touchdowns']
        s['int'] = pass_row['interceptions']
    if rush_row is not None:
        s['rYds'] += rush_row['rush_yards']
        s['rTD'] += rush_row['rush_touchdowns']
    if rec_row is not None:
        s['rec'] = rec_row['receptions']
        s['recYds'] = rec_row['yards']
        s['recTD'] = rec_row['rec_touchdowns']
    return s


def build_leaders(passing, rushing, receiving):
    rush_by_id = {r['player_gsis_id']: r for _, r in rushing.iterrows()}
    rec_by_id = {r['player_gsis_id']: r for _, r in receiving.iterrows()}

    qb_rows = []
    for _, p in passing.iterrows():
        pid = p['player_gsis_id']
        rush_row = rush_by_id.get(pid)
        stats = _stat_line(pass_row=p, rush_row=rush_row)
        adot = float(p.get('avg_intended_air_yards', 0) or 0)
        qb_rows.append({
            'Player': p['player_display_name'], 'Team': p['team_abbr'],
            'pAtt_avg': float(p['attempts']), 'pCmp_avg': float(p['completions']),
            'cmp_pct': (p['completions'] / p['attempts'] * 100) if p['attempts'] else 0.0,
            'pYds_avg': float(p['pass_yards']), 'pTD_avg': float(p['pass_touchdowns']),
            'rYds_avg': stats['rYds'], 'rTD_avg': stats['rTD'],
            'fumbles_avg': 0.0, 'sacks_taken_avg': 0.0, 'int_avg': float(p['interceptions']),
            'air_yards_avg': adot * float(p['attempts']), 'adot': adot,
            'std_score_avg': calculate_fantasy_points(stats, 'STD'),
        })

    def _skill_rows(pos_filter):
        rows = []
        seen = set()
        for pid, rush_row in rush_by_id.items():
            if pos_filter is not None and rush_row.get('player_position') not in pos_filter:
                continue
            rec_row = rec_by_id.get(pid)
            stats = _stat_line(rush_row=rush_row, rec_row=rec_row)
            adot = float(rec_row['avg_intended_air_yards']) if rec_row is not None and pd.notna(rec_row.get('avg_intended_air_yards')) else 0.0
            targets = float(rec_row['targets']) if rec_row is not None else 0.0
            rows.append({
                'Player': rush_row['player_display_name'], 'Team': rush_row['team_abbr'], 'Slot': '',
                'rAtt_avg': float(rush_row['rush_attempts']), 'rYds_avg': float(rush_row['rush_yards']),
                'rTD_avg': float(rush_row['rush_touchdowns']),
                'targets_avg': targets, 'rec_avg': stats['rec'], 'recYds_avg': stats['recYds'],
                'recTD_avg': stats['recTD'], 'fumbles_avg': 0.0,
                'air_yards_avg': adot * targets, 'adot': adot,
                'std_score_avg': calculate_fantasy_points(stats, 'STD'),
            })
            seen.add(pid)
        # Receivers with zero rush attempts never show up in rush_by_id --
        # add them from receiving directly so pure pass-catchers aren't lost.
        for pid, rec_row in rec_by_id.items():
            if pid in seen:
                continue
            if pos_filter is not None and rec_row.get('player_position') not in pos_filter:
                continue
            stats = _stat_line(rec_row=rec_row)
            adot = float(rec_row['avg_intended_air_yards']) if pd.notna(rec_row.get('avg_intended_air_yards')) else 0.0
            targets = float(rec_row['targets'])
            rows.append({
                'Player': rec_row['player_display_name'], 'Team': rec_row['team_abbr'], 'Slot': '',
                'rAtt_avg': 0.0, 'rYds_avg': 0.0, 'rTD_avg': 0.0,
                'targets_avg': targets, 'rec_avg': stats['rec'], 'recYds_avg': stats['recYds'],
                'recTD_avg': stats['recTD'], 'fumbles_avg': 0.0,
                'air_yards_avg': adot * targets, 'adot': adot,
                'std_score_avg': calculate_fantasy_points(stats, 'STD'),
            })
        return rows

    return {
        'overall': {
            'qb': qb_rows,
            'rb': _skill_rows({'RB', 'FB'}),
            'wr': _skill_rows({'WR'}),
            'te': _skill_rows({'TE'}),
        },
        # Rookie split needs a roster join this script doesn't do yet -- see
        # module docstring.
        'rookies': {'qb': [], 'rb': [], 'wr': [], 'te': []},
    }


def build_teams_data(passing, rushing, receiving):
    """Per-team usage boards, same shape TeamsTab already renders. Field
    names deliberately keep the existing '_p50' suffix (originally meaning
    "simulated median") purely so the shared TeamsTab component can render
    real actual totals with zero code changes -- a real total is its own
    single-point distribution."""
    teams = {}
    for team in TEAM_DIVISIONS:
        team_rush = rushing[rushing['team_abbr'] == team].sort_values('rush_yards', ascending=False)
        team_rec = receiving[receiving['team_abbr'] == team].sort_values('targets', ascending=False)
        team_pass = passing[passing['team_abbr'] == team].sort_values('pass_yards', ascending=False)
        rush_by_id = {r['player_gsis_id']: r for _, r in rushing.iterrows()}

        usage_rushing = [{
            'Player': r['player_display_name'], 'Pos': r.get('player_position', ''),
            'rAtt_p50': round(float(r['rush_attempts']), 1), 'rYds_p50': round(float(r['rush_yards']), 1),
            'rTD_p50': round(float(r['rush_touchdowns']), 1),
            'std_score_p50': calculate_fantasy_points(_stat_line(rush_row=r), 'STD'),
        } for _, r in team_rush.iterrows()]

        usage_targets = [{
            'Player': r['player_display_name'], 'Pos': r.get('player_position', ''),
            'targets_p50': round(float(r['targets']), 1), 'rec_p50': round(float(r['receptions']), 1),
            'recYds_p50': round(float(r['yards']), 1), 'recTD_p50': round(float(r['rec_touchdowns']), 1),
            'std_score_p50': calculate_fantasy_points(_stat_line(rec_row=r), 'STD'),
        } for _, r in team_rec.iterrows()]

        usage_qb = []
        for _, p in team_pass.iterrows():
            rush_row = rush_by_id.get(p['player_gsis_id'])
            stats = _stat_line(pass_row=p, rush_row=rush_row)
            usage_qb.append({
                'Player': p['player_display_name'], 'Slot': '',
                'pAtt_p50': round(float(p['attempts']), 1), 'pCmp_p50': round(float(p['completions']), 1),
                'cmp_pct_p50': round((p['completions'] / p['attempts'] * 100) if p['attempts'] else 0.0, 1),
                'pYds_p50': round(float(p['pass_yards']), 1), 'pTD_p50': round(float(p['pass_touchdowns']), 1),
                'int_p50': round(float(p['interceptions']), 1),
                'rAtt_p50': round(float(rush_row['rush_attempts']), 1) if rush_row is not None else 0.0,
                'rYds_p50': round(float(rush_row['rush_yards']), 1) if rush_row is not None else 0.0,
                'rTD_p50': round(float(rush_row['rush_touchdowns']), 1) if rush_row is not None else 0.0,
                'std_score_p50': calculate_fantasy_points(stats, 'STD'),
                'dk_score_p50': calculate_fantasy_points(stats, 'DK'),
            })

        teams[team] = {
            'usage_rushing': usage_rushing,
            'usage_targets': usage_targets,
            'usage_qb': usage_qb,
            # No 'matchups' key here -- Current Season's TeamsTab renders
            # just the current week's single matchup (from the separate
            # /api/season2026/current/matchups endpoint), not a full
            # projected schedule.
        }
    return teams


def build():
    current_week = get_current_week(SIM_YEAR)
    print(f"Current week (auto-detected): {current_week}")

    played = import_real_played_games(SIM_YEAR)
    print(f"Real completed games found: {len(played)}")
    if played.empty:
        print("No completed 2026 games yet -- nothing to build. Run again after Week 1 finishes.")
        return

    standings_rows = build_standings(played)

    passing, rushing, receiving = import_real_player_ngs(SIM_YEAR)
    print(f"NGS season-to-date rows: passing={len(passing)} rushing={len(rushing)} receiving={len(receiving)}")

    team_stats_rows = build_team_stats(passing, rushing, standings_rows)
    leaders = build_leaders(passing, rushing, receiving)
    teams_data = build_teams_data(passing, rushing, receiving)

    os.makedirs(OUT_DIR, exist_ok=True)
    pd.DataFrame(standings_rows).to_csv(os.path.join(OUT_DIR, 'standings.csv'), index=False)
    pd.DataFrame(team_stats_rows).to_csv(os.path.join(OUT_DIR, 'team_stats.csv'), index=False)
    with open(os.path.join(OUT_DIR, 'leaders.json'), 'w', encoding='utf-8') as f:
        json.dump(leaders, f)
    with open(os.path.join(OUT_DIR, 'teams_data.json'), 'w', encoding='utf-8') as f:
        json.dump(teams_data, f)
    print(f"Wrote standings.csv, team_stats.csv, leaders.json, teams_data.json to {OUT_DIR}/")


if __name__ == '__main__':
    build()
