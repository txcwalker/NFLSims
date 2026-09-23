"""Generates a per-team season offense/defense stat collection for the 2026
simulated season, from the game + player parquet caches written by
run_full_season_sim_2026.py.

Offense (team's own players, Pos != 'DST'):
  - Pass Yds: sum of the team's QB(s) pYds (not double-counted via recYds,
    which credits the same yardage to the receiver individually)
  - Rush Yds: sum of rYds across all positions (RB/WR jet sweeps/QB scrambles)
  - Turnovers: sum of int + fumbles_lost
  - Sacks Taken: sum of sacks_taken

Defense (team's own "Defense" D/ST rows, already tracked directly by the
engine): def_sack, def_int, def_fumble_rec, def_td, pts_allowed.

Yards/turnovers Allowed: the SAME per-play stats above, but summed for the
opponent in each game -- computed via an opponent join derived from the
games cache's away_team/home_team per game_id (each player row's game_id
maps to exactly one opponent).

All figures are season totals: summed per team per season Monte Carlo
iteration, then averaged (Expected) and medianed across iterations -- same
convention as generate_season_leaders_2026.py and run_full_season_sim_2026.py.

ADDITIVE REST-OF-SEASON (2026-09-16): games/players from weeks before the
current week (see current_week_v_0_1_0.get_current_week) are dropped from
the simulated aggregation below, then each team's real PF/PA/pass-yards/
rush-yards through those completed weeks (from real_results_v_0_1_0, same
NGS-based source build_actual_season_stats_2026.py uses) is added back as a
constant offset. Turnovers/sacks/defense/"Allowed" columns have no real
per-play source yet (see real_results_v_0_1_0's docstring) and stay purely
simulated for now -- a known, documented gap, same one
build_actual_season_stats_2026.py's own docstring calls out.

Usage: python generate_team_stats_2026.py
"""
import os
import sys
import pandas as pd
import numpy as np

sys.path.append(os.getcwd())
from src.data_pipeline.current_week_v_0_1_0 import get_current_week
from src.data_pipeline.real_results_v_0_1_0 import import_real_played_games, import_real_player_ngs

SIM_YEAR = 2026
GAMES_CACHE = f"data/interim/sim_results_{SIM_YEAR}_games.parquet"
PLAYERS_CACHE = f"data/interim/sim_results_{SIM_YEAR}_players.parquet"
SCHEDULE_CSV = f"data/external/schedule_{SIM_YEAR}.csv"
OUTPUT_DIR = "docs/reports"

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


def season_avg_med(df, group_cols, metrics):
    """Sum per iteration, then Expected (mean) + Median across iterations."""
    totals = df.groupby(group_cols + ['iteration'])[metrics].sum().reset_index()
    expected = totals.groupby(group_cols)[metrics].mean().reset_index()
    median = totals.groupby(group_cols)[metrics].median().reset_index()
    expected = expected.rename(columns={m: f"{m}_avg" for m in metrics})
    median = median.rename(columns={m: f"{m}_med" for m in metrics})
    return pd.merge(expected, median, on=group_cols)


def generate():
    if not os.path.exists(GAMES_CACHE) or not os.path.exists(PLAYERS_CACHE):
        print(f"Error: parquet cache not found -- run run_full_season_sim_{SIM_YEAR}.py first.")
        return

    print("Loading simulation caches...")
    games_df = pd.read_parquet(GAMES_CACHE)
    players_df = pd.read_parquet(PLAYERS_CACHE)

    # --- ADDITIVE: drop completed-week games/players from the simulated
    # aggregation -- their real contribution gets added back below instead.
    current_week = get_current_week(SIM_YEAR)
    print(f"Current week (auto-detected): {current_week}")
    sched = pd.read_csv(SCHEDULE_CSV)
    week_by_game_id = dict(zip(sched['game_id'], sched['week']))
    games_df = games_df[games_df['game_id'].map(week_by_game_id).fillna(999) >= current_week]
    players_df = players_df[players_df['game_id'].map(week_by_game_id).fillna(999) >= current_week]

    real_offsets = {}
    real_played = import_real_played_games(SIM_YEAR)
    real_played = real_played[real_played['week'] < current_week]
    if not real_played.empty:
        real_passing, real_rushing, real_receiving = import_real_player_ngs(SIM_YEAR)
        for team in TEAM_DIVISIONS:
            pf = real_played.loc[real_played['away_team'] == team, 'away_score'].sum() + \
                 real_played.loc[real_played['home_team'] == team, 'home_score'].sum()
            pa = real_played.loc[real_played['away_team'] == team, 'home_score'].sum() + \
                 real_played.loc[real_played['home_team'] == team, 'away_score'].sum()
            pYds = real_passing.loc[real_passing['team_abbr'] == team, 'pass_yards'].sum()
            rYds = real_rushing.loc[real_rushing['team_abbr'] == team, 'rush_yards'].sum()
            real_offsets[team] = {'pf': pf, 'pa': pa, 'pYds': pYds, 'rYds': rYds}
        print(f"Real completed-week team offsets available for {len(real_played)} games.")

    # --- Points For/Against, per team per iteration ---
    print("Computing points for/against...")
    away_pf = games_df[['game_id', 'iteration', 'away_team', 'away_score', 'home_score']].rename(
        columns={'away_team': 'Team', 'away_score': 'pf', 'home_score': 'pa'})
    home_pf = games_df[['game_id', 'iteration', 'home_team', 'home_score', 'away_score']].rename(
        columns={'home_team': 'Team', 'home_score': 'pf', 'away_score': 'pa'})
    pf_pa = pd.concat([away_pf[['Team', 'iteration', 'pf', 'pa']], home_pf[['Team', 'iteration', 'pf', 'pa']]])
    pf_pa_season = pf_pa.groupby(['Team', 'iteration'])[['pf', 'pa']].sum().reset_index()
    pf_pa_expected = pf_pa_season.groupby('Team')[['pf', 'pa']].mean().rename(columns={'pf': 'PF_avg', 'pa': 'PA_avg'})
    pf_pa_median = pf_pa_season.groupby('Team')[['pf', 'pa']].median().rename(columns={'pf': 'PF_med', 'pa': 'PA_med'})

    # --- Opponent join: each player row's game_id -> the OTHER team in that game ---
    print("Building opponent join...")
    game_teams = games_df.groupby('game_id')[['away_team', 'home_team']].first().reset_index()
    players_df = players_df.merge(game_teams, on='game_id', how='left')
    players_df['Opponent'] = np.where(players_df['Team'] == players_df['away_team'],
                                       players_df['home_team'], players_df['away_team'])

    offense_df = players_df[players_df['Pos'] != 'DST'].copy()
    defense_df = players_df[players_df['Pos'] == 'DST'].copy()

    qb_df = offense_df[offense_df['Pos'] == 'QB']

    # --- Team Offense: Pass Yds (QB only), Rush Yds (all), Turnovers, Sacks Taken ---
    print("Aggregating team offense...")
    pass_yds = season_avg_med(qb_df, ['Team'], ['pYds', 'pTD'])
    offense_df['turnovers'] = offense_df['int'] + offense_df['fumbles_lost']
    off_rest = season_avg_med(offense_df, ['Team'], ['rYds', 'rTD', 'turnovers', 'sacks_taken'])
    team_offense = pd.merge(pass_yds, off_rest, on='Team')
    team_offense['TotalYds_avg'] = team_offense['pYds_avg'] + team_offense['rYds_avg']
    team_offense['TotalYds_med'] = team_offense['pYds_med'] + team_offense['rYds_med']

    # --- Team Defense: own D/ST rows (already tracked directly by the engine) ---
    print("Aggregating team defense...")
    team_defense = season_avg_med(defense_df, ['Team'], ['def_sack', 'def_int', 'def_fumble_rec', 'def_td'])

    # --- Yards/Turnovers Allowed: same offense stats, grouped by Opponent ---
    print("Aggregating yards/turnovers allowed...")
    qb_allowed = season_avg_med(qb_df.rename(columns={'Opponent': 'AllowedBy'}), ['AllowedBy'], ['pYds'])
    qb_allowed = qb_allowed.rename(columns={'pYds_avg': 'PassYdsAllowed_avg', 'pYds_med': 'PassYdsAllowed_med', 'AllowedBy': 'Team'})
    off_allowed = season_avg_med(offense_df.rename(columns={'Opponent': 'AllowedBy'}), ['AllowedBy'], ['rYds', 'turnovers'])
    off_allowed = off_allowed.rename(columns={'rYds_avg': 'RushYdsAllowed_avg', 'rYds_med': 'RushYdsAllowed_med',
                                               'turnovers_avg': 'Takeaways_avg', 'turnovers_med': 'Takeaways_med',
                                               'AllowedBy': 'Team'})
    yds_allowed = pd.merge(qb_allowed, off_allowed, on='Team')
    yds_allowed['TotalYdsAllowed_avg'] = yds_allowed['PassYdsAllowed_avg'] + yds_allowed['RushYdsAllowed_avg']
    yds_allowed['TotalYdsAllowed_med'] = yds_allowed['PassYdsAllowed_med'] + yds_allowed['RushYdsAllowed_med']

    # --- Combine everything ---
    team_stats = pf_pa_expected.join(pf_pa_median).reset_index()
    team_stats = team_stats.merge(team_offense, on='Team').merge(team_defense, on='Team').merge(yds_allowed, on='Team')
    team_stats['Conference'] = team_stats['Team'].map(lambda t: TEAM_DIVISIONS[t][0])
    team_stats['Division'] = team_stats['Team'].map(lambda t: TEAM_DIVISIONS[t][1])

    # --- ADDITIVE: add each team's real completed-week PF/PA/pass/rush
    # yards back on top of the (now remaining-weeks-only) simulated totals.
    # Adding a constant to a distribution shifts its mean/median by the
    # same constant, so this is valid for both the _avg and _med columns.
    if real_offsets:
        for col, key in [('PF_avg', 'pf'), ('PF_med', 'pf'), ('PA_avg', 'pa'), ('PA_med', 'pa'),
                          ('pYds_avg', 'pYds'), ('pYds_med', 'pYds'), ('rYds_avg', 'rYds'), ('rYds_med', 'rYds')]:
            team_stats[col] = team_stats.apply(lambda r: r[col] + real_offsets.get(r['Team'], {}).get(key, 0), axis=1)
        team_stats['TotalYds_avg'] = team_stats['pYds_avg'] + team_stats['rYds_avg']
        team_stats['TotalYds_med'] = team_stats['pYds_med'] + team_stats['rYds_med']

    team_stats['PointDiff_avg'] = team_stats['PF_avg'] - team_stats['PA_avg']

    # Save full CSV
    csv_path = os.path.join(OUTPUT_DIR, f"team_stats_{SIM_YEAR}.csv")
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    team_stats.to_csv(csv_path, index=False)
    print(f"Wrote {csv_path}")

    # Save markdown, sorted by point differential
    md = f"# NFL {SIM_YEAR} Simulated Season Team Stat Collection\n"
    md += "**Season-long totals, averaged (Expected) across 1,000 Monte Carlo season iterations.**\n"
    md += "*Yards/turnovers Allowed computed via an opponent join on each simulated game.*\n\n"
    md += ("| Team | PF (Avg) | PA (Avg) | Diff | Pass Yds | Rush Yds | Total Yds | Pass Yds Allowed | "
           "Rush Yds Allowed | Total Yds Allowed | Giveaways | Takeaways | Sacks Taken | Def Sacks | Def INTs | Def Fum Rec | Def TDs |\n")
    md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
    for _, r in team_stats.sort_values('PointDiff_avg', ascending=False).iterrows():
        md += (f"| **{r['Team']}** | {r['PF_avg']:.1f} | {r['PA_avg']:.1f} | `{r['PointDiff_avg']:+.1f}` | "
               f"{r['pYds_avg']:.0f} | {r['rYds_avg']:.0f} | {r['TotalYds_avg']:.0f} | "
               f"{r['PassYdsAllowed_avg']:.0f} | {r['RushYdsAllowed_avg']:.0f} | {r['TotalYdsAllowed_avg']:.0f} | "
               f"{r['turnovers_avg']:.1f} | {r['Takeaways_avg']:.1f} | {r['sacks_taken_avg']:.1f} | "
               f"{r['def_sack_avg']:.1f} | {r['def_int_avg']:.1f} | {r['def_fumble_rec_avg']:.1f} | {r['def_td_avg']:.1f} |\n")

    md_path = os.path.join(OUTPUT_DIR, f"team_stats_{SIM_YEAR}.md")
    with open(md_path, "w", encoding="utf-8") as f:
        f.write(md)
    print(f"Wrote {md_path}")


if __name__ == "__main__":
    generate()
