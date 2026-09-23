"""Generates league-leader reports (overall + rookies-only) for the 2026
simulated season, from the player parquet cache written by
run_full_season_sim_2026.py.

QB table: Att, Cmp, Pass Yds, Pass TDs, Rush Yds, Rush TDs, Fumbles, Sacks
Taken, INTs, Air Yards, ADOT.
RB/WR/TE table: Rush Att, Rush Yds, Rush TDs, Targets, Rec, Rec Yds, Rec
TDs, Fumbles, Air Yards, ADOT.

All counting stats are season-long totals: summed per player per season
Monte Carlo iteration (a player's games that iteration), then averaged
(Expected) and medianed across iterations -- same convention as
generate_season_leaders.py (2025) and run_full_season_sim_2026.py's own
player_summary_df. ADOT is a rate stat, NOT summed/averaged directly --
computed as season-total air yards / season-total attempts (or targets),
using the already-averaged season totals (ratio-of-means, standard
convention for season-aggregate ADOT reporting).

Rookie flag comes from data/current_rosters/{TEAM}_traits_2026.json's
traits[name]["rookie"] -- joined onto the leaders table by (Team, Player).

ADDITIVE REST-OF-SEASON (2026-09-16): games from weeks before the current
week (get_current_week) are dropped before aggregation, then each player's
real completed-week totals (real_results_v_0_1_0, keyed by (Team, Player)
name -- a real name that doesn't exactly match the sim's roster name is
silently skipped, so it stays purely simulated) are added back as a
constant offset to pAtt/pCmp/pYds/pTD/int/rAtt/rYds/rTD/targets/rec/
recYds/recTD, plus dk_score/std_score recomputed from that real subset
(fumbles=0 -- no real per-play source yet, same gap documented in
real_results_v_0_1_0). sacks_taken/air_yards stay purely simulated.

Usage: python generate_season_leaders_2026.py
"""
import os
import sys
import glob
import json
import pandas as pd
import numpy as np

sys.path.append(os.getcwd())
from src.data_pipeline.current_week_v_0_1_0 import get_current_week
from src.data_pipeline.real_results_v_0_1_0 import import_real_played_games, import_real_player_ngs, real_player_totals_by_name
from src.nfl_sim.scoring import calculate_fantasy_points

SIM_YEAR = 2026
PLAYERS_CACHE = f"data/interim/sim_results_{SIM_YEAR}_players.parquet"
SCHEDULE_CSV = f"data/external/schedule_{SIM_YEAR}.csv"
ROSTERS_DIR = "data/current_rosters"
OUTPUT_DIR = "docs/reports"

QB_METRICS = ['pAtt', 'pCmp', 'pYds', 'pTD', 'rAtt', 'rYds', 'rTD', 'fumbles', 'sacks_taken', 'int', 'air_yards', 'dk_score', 'std_score']
SKILL_METRICS = ['rAtt', 'rYds', 'rTD', 'targets', 'rec', 'recYds', 'recTD', 'fumbles', 'air_yards', 'dk_score', 'std_score']


def load_rookie_lookup(year):
    """(Team, Player) -> bool, from every current_rosters traits file."""
    lookup = {}
    for path in glob.glob(os.path.join(ROSTERS_DIR, f"*_traits_{year}.json")):
        data = json.load(open(path))
        team = data["team"]
        for name, traits in data["traits"].items():
            lookup[(team, name)] = bool(traits.get("rookie", False))
    return lookup


def build_leaders_table(df, metrics):
    """Season totals per iteration -> Expected (mean) and Median across iterations."""
    season_totals = df.groupby(['Player', 'Team', 'Pos', 'Slot', 'iteration'])[metrics].sum().reset_index()
    expected = season_totals.groupby(['Player', 'Team', 'Pos', 'Slot'])[metrics].mean().reset_index()
    median = season_totals.groupby(['Player', 'Team', 'Pos', 'Slot'])[metrics].median().reset_index()

    avg_cols = {col: f"{col}_avg" for col in metrics}
    med_cols = {col: f"{col}_med" for col in metrics}
    expected = expected.rename(columns=avg_cols)
    median = median.rename(columns=med_cols)

    return pd.merge(expected, median, on=['Player', 'Team', 'Pos', 'Slot'])


def qb_table_md(df, title):
    md = f"## {title}\n\n"
    md += "| Rank | Player | Team | Att | Cmp | Cmp % | Pass Yds | Pass TDs | Rush Yds | Rush TDs | Fumbles | Sacks Taken | INTs | Air Yds | ADOT | Std (Season) | DK (Season) |\n"
    md += "| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
    if df.empty:
        return md + "*None.*\n\n"
    for idx, (_, r) in enumerate(df.iterrows()):
        adot = r['air_yards_avg'] / r['pAtt_avg'] if r['pAtt_avg'] > 0 else 0.0
        cmp_pct = r['pCmp_avg'] / r['pAtt_avg'] * 100 if r['pAtt_avg'] > 0 else 0.0
        md += (f"| {idx+1} | **{r['Player']}** | {r['Team']} | {r['pAtt_avg']:.1f} | {r['pCmp_avg']:.1f} | {cmp_pct:.1f}% | "
               f"**{r['pYds_avg']:.1f}** | {r['pTD_avg']:.1f} | {r['rYds_avg']:.1f} | {r['rTD_avg']:.1f} | "
               f"{r['fumbles_avg']:.1f} | {r['sacks_taken_avg']:.1f} | {r['int_avg']:.1f} | {r['air_yards_avg']:.1f} | "
               f"{adot:.1f} | {r['std_score_avg']:.1f} | **{r['dk_score_avg']:.1f}** |\n")
    return md + "\n"


def skill_table_md(df, title):
    md = f"## {title}\n\n"
    md += "| Rank | Player | Team | Slot | Rush Att | Rush Yds | Rush TDs | Targets | Rec | Rec Yds | Rec TDs | Fumbles | Air Yds | ADOT | Std (Season) | DK (Season) |\n"
    md += "| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
    if df.empty:
        return md + "*None.*\n\n"
    for idx, (_, r) in enumerate(df.iterrows()):
        adot = r['air_yards_avg'] / r['targets_avg'] if r['targets_avg'] > 0 else 0.0
        md += (f"| {idx+1} | **{r['Player']}** | {r['Team']} | {r['Slot']} | {r['rAtt_avg']:.1f} | {r['rYds_avg']:.1f} | "
               f"{r['rTD_avg']:.1f} | {r['targets_avg']:.1f} | {r['rec_avg']:.1f} | **{r['recYds_avg']:.1f}** | "
               f"{r['recTD_avg']:.1f} | {r['fumbles_avg']:.1f} | {r['air_yards_avg']:.1f} | {adot:.1f} | "
               f"{r['std_score_avg']:.1f} | **{r['dk_score_avg']:.1f}** |\n")
    return md + "\n"


def apply_real_offsets(df, real_totals, real_fields):
    """Adds each player's real completed-week total (a constant) to the
    matching _avg/_med columns -- valid since adding a constant to a
    distribution shifts its mean/median by that same constant. Also adds a
    dk_score_avg/std_score_avg/dk_score_med/std_score_med offset recomputed
    from the real subset (fumbles=0, see module docstring). Players with no
    real-name match stay purely simulated."""
    if df.empty or not real_totals:
        return df
    df = df.copy()

    def _real(row):
        return real_totals.get((row['Team'], row['Player']))

    zero_stats = {f: 0 for f in ['pYds', 'pTD', 'int', 'rYds', 'rTD', 'rec', 'recYds', 'recTD', 'fumbles']}
    reals = df.apply(_real, axis=1)
    std_offset = reals.apply(lambda r: calculate_fantasy_points({**zero_stats, **r}, 'STD') if r else 0.0)
    dk_offset = reals.apply(lambda r: calculate_fantasy_points({**zero_stats, **r}, 'DK') if r else 0.0)

    for suffix in ('_avg', '_med'):
        for field in real_fields:
            col = f"{field}{suffix}"
            if col not in df.columns:
                continue
            df[col] = df[col] + reals.apply(lambda r, f=field: (r or {}).get(f, 0.0))
        if f"std_score{suffix}" in df.columns:
            df[f"std_score{suffix}"] = df[f"std_score{suffix}"] + std_offset
        if f"dk_score{suffix}" in df.columns:
            df[f"dk_score{suffix}"] = df[f"dk_score{suffix}"] + dk_offset
    return df


def qb_records(df):
    if df.empty:
        return []
    out = df.copy()
    out['adot'] = np.where(out['pAtt_avg'] > 0, out['air_yards_avg'] / out['pAtt_avg'], 0.0).round(2)
    out['cmp_pct'] = np.where(out['pAtt_avg'] > 0, out['pCmp_avg'] / out['pAtt_avg'] * 100, 0.0).round(2)
    return out.round(2).to_dict('records')


def skill_records(df):
    if df.empty:
        return []
    out = df.copy()
    out['adot'] = np.where(out['targets_avg'] > 0, out['air_yards_avg'] / out['targets_avg'], 0.0).round(2)
    return out.round(2).to_dict('records')


def generate(top_n=32, rookie_top_n=20):
    if not os.path.exists(PLAYERS_CACHE):
        print(f"Error: {PLAYERS_CACHE} not found -- run run_full_season_sim_{SIM_YEAR}.py first.")
        return

    print("Loading player simulation cache...")
    df = pd.read_parquet(PLAYERS_CACHE)

    # --- ADDITIVE: drop completed-week games from the simulated
    # aggregation -- real per-player totals get added back below instead.
    current_week = get_current_week(SIM_YEAR)
    print(f"Current week (auto-detected): {current_week}")
    sched = pd.read_csv(SCHEDULE_CSV)
    week_by_game_id = dict(zip(sched['game_id'], sched['week']))
    df = df[df['game_id'].map(week_by_game_id).fillna(999) >= current_week]

    real_totals = {}
    real_played = import_real_played_games(SIM_YEAR)
    if not real_played[real_played['week'] < current_week].empty:
        real_passing, real_rushing, real_receiving = import_real_player_ngs(SIM_YEAR)
        real_totals = real_player_totals_by_name(real_passing, real_rushing, real_receiving)
        print(f"Real per-player offsets available for {len(real_totals)} players.")

    print("Loading rookie lookup from current_rosters...")
    rookie_lookup = load_rookie_lookup(SIM_YEAR)

    qb_df = df[df['Pos'] == 'QB']
    skill_df = df[df['Pos'].isin(['RB', 'WR', 'TE'])]

    print("Building QB leaders...")
    qb_leaders = build_leaders_table(qb_df, QB_METRICS)
    qb_leaders = apply_real_offsets(qb_leaders, real_totals, ['pAtt', 'pCmp', 'pYds', 'pTD', 'int', 'rAtt', 'rYds', 'rTD'])
    qb_leaders['Rookie'] = qb_leaders.apply(lambda r: rookie_lookup.get((r['Team'], r['Player']), False), axis=1)
    qb_leaders = qb_leaders[qb_leaders['pAtt_avg'] > 0].sort_values('pYds_avg', ascending=False)

    print("Building RB/WR/TE leaders...")
    skill_leaders = build_leaders_table(skill_df, SKILL_METRICS)
    skill_leaders = apply_real_offsets(skill_leaders, real_totals, ['rAtt', 'rYds', 'rTD', 'targets', 'rec', 'recYds', 'recTD'])
    skill_leaders['Rookie'] = skill_leaders.apply(lambda r: rookie_lookup.get((r['Team'], r['Player']), False), axis=1)
    skill_leaders = skill_leaders[(skill_leaders['rAtt_avg'] > 0) | (skill_leaders['targets_avg'] > 0)]

    rb_leaders = skill_leaders[skill_leaders['Pos'] == 'RB'].sort_values('rYds_avg', ascending=False)
    wr_leaders = skill_leaders[skill_leaders['Pos'] == 'WR'].sort_values('recYds_avg', ascending=False)
    te_leaders = skill_leaders[skill_leaders['Pos'] == 'TE'].sort_values('recYds_avg', ascending=False)

    md = f"# NFL {SIM_YEAR} Simulated Season League Leaders\n"
    md += "**Season-long cumulative totals, averaged (Expected) across 1,000 Monte Carlo season iterations.**\n"
    md += "*ADOT = season air yards / season attempts (QB) or targets (RB/WR/TE), a rate stat -- not averaged directly.*\n\n"

    md += "# Overall League Leaders\n\n"
    md += qb_table_md(qb_leaders.head(top_n), "Quarterbacks")
    md += skill_table_md(rb_leaders.head(top_n), "Running Backs")
    md += skill_table_md(wr_leaders.head(top_n), "Wide Receivers")
    md += skill_table_md(te_leaders.head(top_n), "Tight Ends")

    md += "# Rookie Leaders\n\n"
    md += qb_table_md(qb_leaders[qb_leaders['Rookie']].head(rookie_top_n), "Rookie Quarterbacks")
    md += skill_table_md(rb_leaders[rb_leaders['Rookie']].head(rookie_top_n), "Rookie Running Backs")
    md += skill_table_md(wr_leaders[wr_leaders['Rookie']].head(rookie_top_n), "Rookie Wide Receivers")
    md += skill_table_md(te_leaders[te_leaders['Rookie']].head(rookie_top_n), "Rookie Tight Ends")

    out_path = os.path.join(OUTPUT_DIR, f"season_leaders_{SIM_YEAR}.md")
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as f:
        f.write(md)
    print(f"Wrote {out_path}")

    # Structured JSON sibling for the analytics dev site (frontend_analysis
    # Season2026 page) -- same head(top_n)/head(rookie_top_n) slices as the
    # markdown above, just as records instead of table rows.
    leaders_json = {
        "overall": {
            "qb": qb_records(qb_leaders.head(top_n)),
            "rb": skill_records(rb_leaders.head(top_n)),
            "wr": skill_records(wr_leaders.head(top_n)),
            "te": skill_records(te_leaders.head(top_n)),
        },
        "rookies": {
            "qb": qb_records(qb_leaders[qb_leaders['Rookie']].head(rookie_top_n)),
            "rb": skill_records(rb_leaders[rb_leaders['Rookie']].head(rookie_top_n)),
            "wr": skill_records(wr_leaders[wr_leaders['Rookie']].head(rookie_top_n)),
            "te": skill_records(te_leaders[te_leaders['Rookie']].head(rookie_top_n)),
        },
    }
    json_path = os.path.join(OUTPUT_DIR, f"season_leaders_{SIM_YEAR}.json")
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(leaders_json, f)
    print(f"Wrote {json_path}")


if __name__ == "__main__":
    generate()
