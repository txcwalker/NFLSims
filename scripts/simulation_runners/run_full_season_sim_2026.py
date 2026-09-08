import sys
import os
import glob
import pandas as pd
import numpy as np
import json
import random
import time

# Headless matplotlib to avoid GUI issues in background processes
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

# Add project root to system path
sys.path.append(os.getcwd())

from src.nfl_sim.batch import BatchSimulator

# --- TEAM AND DIVISION METADATA ---
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

# Full names mapping for plotting/directory names if needed, otherwise use abbreviation
TEAM_FULL_NAMES = {
    'BUF': 'Buffalo Bills', 'MIA': 'Miami Dolphins', 'NE': 'New England Patriots', 'NYJ': 'New York Jets',
    'BAL': 'Baltimore Ravens', 'CIN': 'Cincinnati Bengals', 'CLE': 'Cleveland Browns', 'PIT': 'Pittsburgh Steelers',
    'HOU': 'Houston Texans', 'IND': 'Indianapolis Colts', 'JAX': 'Jacksonville Jaguars', 'TEN': 'Tennessee Titans',
    'DEN': 'Denver Broncos', 'KC': 'Kansas City Chiefs', 'LV': 'Las Vegas Raiders', 'LAC': 'Los Angeles Chargers',
    'DAL': 'Dallas Cowboys', 'NYG': 'New York Giants', 'PHI': 'Philadelphia Eagles', 'WAS': 'Washington Commanders',
    'CHI': 'Chicago Bears', 'DET': 'Detroit Lions', 'GB': 'Green Bay Packers', 'MIN': 'Minnesota Vikings',
    'ATL': 'Atlanta Falcons', 'CAR': 'Carolina Panthers', 'NO': 'New Orleans Saints', 'TB': 'Tampa Bay Buccaneers',
    'ARI': 'Arizona Cardinals', 'LA': 'Los Angeles Rams', 'SF': 'San Francisco 49ers', 'SEA': 'Seattle Seahawks',
}

SIM_YEAR = 2026


def _cache_is_stale(cache_paths, input_globs):
    """True if any cache file is missing, or any matching input file was
    modified more recently than the oldest cache file.

    Previously the season-long Parquet cache (sim_results_{year}_*.parquet)
    was reused unconditionally whenever it existed on disk, with no check
    against the roster/override/DNA data it was built from. That meant a
    hand-edit -- a new rookie added to current_rosters, a target_share/
    carry_share tweak in preseason_overrides -- silently never showed up in
    any report until someone remembered to delete the cache files by hand.
    """
    cache_mtimes = [os.path.getmtime(p) for p in cache_paths if os.path.exists(p)]
    if len(cache_mtimes) != len(cache_paths):
        return True
    oldest_cache_mtime = min(cache_mtimes)
    for pattern in input_globs:
        for path in glob.glob(pattern):
            if os.path.getmtime(path) > oldest_cache_mtime:
                return True
    return False

# Matchup Cache for Playoff simulations to avoid repeating 1000-sim batches
PLAYOFF_MATCHUP_CACHE = {}

def simulate_matchup_cached(away, home):
    """Simulates a matchup of 1,000 runs or retrieves from cache."""
    key = f"{away}_at_{home}"
    if key in PLAYOFF_MATCHUP_CACHE:
        return PLAYOFF_MATCHUP_CACHE[key]

    batch = BatchSimulator(away, home, year=SIM_YEAR)
    game_df, _ = batch.run_batch(iterations=1000, vectorized=True)

    # Store minimal info to save memory: a list of dicts with scores and winner
    results = []
    for _, row in game_df.iterrows():
        results.append({
            'away_score': row['away_score'], # Away team score
            'home_score': row['home_score'], # Home team score
            'winner': row['winner']
        })

    PLAYOFF_MATCHUP_CACHE[key] = results
    return results

def run_playoff_game(away, home):
    """Simulates a single playoff matchup, sampling 11 games to find the winner."""
    results = simulate_matchup_cached(away, home)

    # Sample 11 outcomes
    samples = random.sample(results, 11)

    away_wins = sum(1 for r in samples if r['away_score'] > r['home_score'])
    home_wins = sum(1 for r in samples if r['home_score'] > r['away_score'])

    # Return winner (if tied 5-5-1, use sum of scores as tiebreaker for playoff progression)
    if away_wins > home_wins:
        return away
    elif home_wins > away_wins:
        return home
    else:
        # Fallback to sum of scores
        away_score = sum(r['away_score'] for r in samples)
        home_score = sum(r['home_score'] for r in samples)
        return away if away_score >= home_score else home

def get_seeding_sort_key(team_stats):
    """Sort key for breaking standings/seeding ties: overall win %, div win %, point diff."""
    wins = team_stats['wins']
    losses = team_stats['losses']
    ties = team_stats['ties']
    total_games = wins + losses + ties
    win_pct = (wins + 0.5 * ties) / total_games if total_games > 0 else 0.0

    div_wins = team_stats['div_wins']
    div_losses = team_stats['div_losses']
    div_ties = team_stats['div_ties']
    total_div = div_wins + div_losses + div_ties
    div_pct = (div_wins + 0.5 * div_ties) / total_div if total_div > 0 else 0.0

    point_diff = team_stats['pf'] - team_stats['pa']

    return (win_pct, div_pct, point_diff)

def _team_group_totals(season_player_grouped, team, pos_group, counting_metrics):
    """Team-level total distribution for one position group (e.g. every RB
    on a team) -- built by summing each iteration's real per-player totals
    FIRST, then taking percentiles of that summed distribution. Deliberately
    NOT built by summing the already-collapsed per-player percentiles
    (percentiles aren't additive across players -- summing two players' P95
    overstates the team's real P95, since it's very unlikely both players
    simultaneously hit their own 95th percentile in the same simulated
    season). cmp_pct is recomputed from the summed pAtt/pCmp after summing
    (ratio-of-sums), same rule as the per-player version. Returns None if
    the team has no players in this group.
    """
    sub = season_player_grouped[
        (season_player_grouped['Team'] == team) & (season_player_grouped['Pos'].isin(pos_group))
    ]
    if sub.empty:
        return None
    totals = sub.groupby('iteration')[counting_metrics].sum()
    if 'pAtt' in totals.columns and 'pCmp' in totals.columns:
        totals['cmp_pct'] = np.where(totals['pAtt'] > 0, totals['pCmp'] / totals['pAtt'] * 100, np.nan)
    quantiles = [0.0, 0.05, 0.25, 0.50, 0.75, 0.95, 1.0]
    q = totals.quantile(quantiles)
    out = {}
    for col in totals.columns:
        for quant in quantiles:
            out[f"{col}_p{int(quant * 100):02d}"] = q.loc[quant, col]
    return out


def simulate_full_season_and_playoffs(iterations=1000, num_seasons=100):
    print(f"\n==========================================")
    print(f" Simulating NFL {SIM_YEAR} Full Season")
    print(f" Iterations per game: {iterations}")
    print(f" Number of Season Monte Carlos: {num_seasons}")
    print(f"==========================================\n")

    # 1. LOAD 2026 SCHEDULE
    sched_df = pd.read_csv(f"data/external/schedule_{SIM_YEAR}.csv")
    reg_games = sched_df[sched_df["game_type"] == "REG"]

    # 2. CACHING OR PRE-SIMULATION PHASE
    games_cache_path = f"data/interim/sim_results_{SIM_YEAR}_games.parquet"
    players_cache_path = f"data/interim/sim_results_{SIM_YEAR}_players.parquet"

    cache_input_globs = [
        f"data/current_rosters/*_traits_{SIM_YEAR}.json",
        f"data/current_rosters/week_*/*_traits_{SIM_YEAR}.json",   # week-aware trees
        "data/dna/*.json",
        "data/dna/*.csv",
        f"data/overrides/{SIM_YEAR}/qb_swaps_{SIM_YEAR}.csv",
        f"data/external/schedule_{SIM_YEAR}.csv",
    ]
    cache_paths = [games_cache_path, players_cache_path]
    stale = _cache_is_stale(cache_paths, cache_input_globs)

    if not stale:
        print("Loading pre-simulated season results from Parquet cache...")
        all_games_df = pd.read_parquet(games_cache_path)
        all_players_df = pd.read_parquet(players_cache_path)
    else:
        if os.path.exists(games_cache_path) or os.path.exists(players_cache_path):
            print("Roster/DNA/override inputs changed since the cache was built -- re-simulating instead of reusing a stale cache.")
        print(f"Pre-simulating all {len(reg_games)} regular season matchups...")
        all_games_list = []
        all_players_list = []

        start_time = time.time()
        week_tree_missing_warned = False
        for idx, row in reg_games.iterrows():
            away = row["away_team"]
            home = row["home_team"]
            game_id = row["game_id"]

            # Week-aware: use the per-week roster tree for this matchup's week
            # (injury returns + mid-season QB swaps -- see
            # scripts/roster_management/build_season_week_rosters_v_0_1_0.py).
            # Falls back to the season-long tree if the week dir isn't built.
            wk = int(row["week"])
            wk_dir = f"data/current_rosters/week_{wk:02d}"
            if os.path.isdir(wk_dir):
                rosters_dir = wk_dir
            else:
                rosters_dir = "data/current_rosters"
                if not week_tree_missing_warned:
                    print("  (week roster trees not found -- using season-long rosters. "
                          "Run build_season_week_rosters_v_0_1_0.py for week-aware sims.)")
                    week_tree_missing_warned = True

            print(f"Simulating game {idx+1}/{len(reg_games)}: {away} at {home} (wk {wk})...")
            batch = BatchSimulator(away, home, year=SIM_YEAR, rosters_dir=rosters_dir)
            game_df, player_df = batch.run_batch(iterations=iterations, vectorized=True)

            # Format game_df
            game_df = game_df.rename(columns={'game_id': 'iteration'})
            game_df['game_id'] = game_id
            game_df['away_team'] = away
            game_df['home_team'] = home
            game_df['div_game'] = row['div_game']
            all_games_list.append(game_df)

            # Format player_df
            if player_df is not None and not player_df.empty:
                player_df = player_df.rename(columns={'game_id': 'iteration'})
                player_df['game_id'] = game_id
                all_players_list.append(player_df)

        print(f"Matchup simulation phase complete in {time.time() - start_time:.2f} seconds.")

        all_games_df = pd.concat(all_games_list, ignore_index=True)
        all_players_df = pd.concat(all_players_list, ignore_index=True)

        os.makedirs(os.path.dirname(games_cache_path), exist_ok=True)
        all_games_df.to_parquet(games_cache_path, index=False)
        all_players_df.to_parquet(players_cache_path, index=False)
        print("Saved simulation results to Parquet cache.")

    # 3. RE-STRUCTURE DATA FOR FAST RETRIEVAL
    # Convert games df to a nested dictionary grouped by game_id for sampling
    matchup_sim_results = {}
    for game_id, group in all_games_df.groupby('game_id'):
        matchup_sim_results[game_id] = {
            'away': group['away_team'].iloc[0],
            'home': group['home_team'].iloc[0],
            'div_game': group['div_game'].iloc[0],
            'outcomes': group[['away_score', 'home_score', 'winner']].to_dict(orient='records')
        }

    # 4. INITIALIZE SEASON-LONG STATISTICS TRACKING
    playoff_tracking = {
        team: {
            'won_division': 0,
            'made_playoffs': 0,
            'made_divisional': 0,
            'made_conference': 0,
            'made_superbowl': 0,
            'won_superbowl': 0,
            'season_wins': [],
            'season_losses': [],
            'season_ties': [],
            'season_pf': [],
            'season_pa': [],
            'season_div_wins': [],
            'season_div_losses': [],
            'season_div_ties': [],
        }
        for team in TEAM_DIVISIONS.keys()
    }

    # 5. RUN SEASON MONTE CARLO LOOPS (100 Seasons)
    print(f"\nSimulating {num_seasons} season runs...")
    for s in range(num_seasons):
        if (s + 1) % 10 == 0 or s == 0:
            print(f" - Simulating season {s+1}/{num_seasons}...")

        # Standings for this specific season
        season_standings = {
            team: {
                'wins': 0.0, 'losses': 0.0, 'ties': 0.0,
                'pf': 0.0, 'pa': 0.0,
                'div_wins': 0.0, 'div_losses': 0.0, 'div_ties': 0.0,
            }
            for team in TEAM_DIVISIONS.keys()
        }

        # Simulating regular season via 11-game random sampling
        for game_id, data in matchup_sim_results.items():
            away = data['away']
            home = data['home']
            div_game = data['div_game']
            outcomes = data['outcomes']

            # Sample 11 outcomes
            samples = random.sample(outcomes, 11)

            away_wins = sum(1 for r in samples if r['away_score'] > r['home_score'])
            home_wins = sum(1 for r in samples if r['home_score'] > r['away_score'])

            # Determine game winner and update record
            if away_wins > home_wins:
                season_standings[away]['wins'] += 1
                season_standings[home]['losses'] += 1
                if div_game == 1:
                    season_standings[away]['div_wins'] += 1
                    season_standings[home]['div_losses'] += 1
            elif home_wins > away_wins:
                season_standings[home]['wins'] += 1
                season_standings[away]['losses'] += 1
                if div_game == 1:
                    season_standings[home]['div_wins'] += 1
                    season_standings[away]['div_losses'] += 1
            else:
                # 5-5-1 or equal wins is a tie
                season_standings[away]['ties'] += 0.5
                season_standings[home]['ties'] += 0.5
                season_standings[away]['wins'] += 0.5
                season_standings[away]['losses'] += 0.5
                season_standings[home]['wins'] += 0.5
                season_standings[home]['losses'] += 0.5
                if div_game == 1:
                    season_standings[away]['div_ties'] += 0.5
                    season_standings[home]['div_ties'] += 0.5
                    season_standings[away]['div_wins'] += 0.5
                    season_standings[away]['div_losses'] += 0.5
                    season_standings[home]['div_wins'] += 0.5
                    season_standings[home]['div_losses'] += 0.5

            # Accumulate scores (expected PF/PA for this season iteration)
            avg_away_score = sum(r['away_score'] for r in samples) / 11.0
            avg_home_score = sum(r['home_score'] for r in samples) / 11.0

            season_standings[away]['pf'] += avg_away_score
            season_standings[away]['pa'] += avg_home_score
            season_standings[home]['pf'] += avg_home_score
            season_standings[home]['pa'] += avg_away_score

        # Update cumulative standings averages and list of records
        for team, stats in season_standings.items():
            playoff_tracking[team]['season_wins'].append(stats['wins'])
            playoff_tracking[team]['season_losses'].append(stats['losses'])
            playoff_tracking[team]['season_ties'].append(stats['ties'])
            playoff_tracking[team]['season_pf'].append(stats['pf'])
            playoff_tracking[team]['season_pa'].append(stats['pa'])
            playoff_tracking[team]['season_div_wins'].append(stats['div_wins'])
            playoff_tracking[team]['season_div_losses'].append(stats['div_losses'])
            playoff_tracking[team]['season_div_ties'].append(stats['div_ties'])

        # 6. PLAYOFF SEEDING FOR THIS SEASON
        # Seeds for AFC and NFC
        conference_seeds = {'AFC': [], 'NFC': []}

        for conf in ['AFC', 'NFC']:
            conf_teams = [t for t, (c, _) in TEAM_DIVISIONS.items() if c == conf]

            # Find Division Winners
            div_winners = []
            for division in ['East', 'North', 'South', 'West']:
                div_teams = [t for t in conf_teams if TEAM_DIVISIONS[t][1] == division]
                # Sort division teams by record key
                div_teams_sorted = sorted(div_teams, key=lambda t: get_seeding_sort_key(season_standings[t]), reverse=True)
                div_winners.append(div_teams_sorted[0])

            # Track Won Division -- div_winners here is the 4 division champs for
            # this conference (one per division), before the divisional-round-reached
            # tracking below reuses the same variable name for a different list.
            for team in div_winners:
                playoff_tracking[team]['won_division'] += 1

            # Assign seeds 1-4 to division winners (sorted by record)
            div_winners_sorted = sorted(div_winners, key=lambda t: get_seeding_sort_key(season_standings[t]), reverse=True)

            # Wild Cards: Next 3 best records in conference (excluding division winners)
            wildcards = [t for t in conf_teams if t not in div_winners]
            wildcards_sorted = sorted(wildcards, key=lambda t: get_seeding_sort_key(season_standings[t]), reverse=True)

            # Combine Seeds 1-7
            seeds_list = div_winners_sorted + wildcards_sorted[:3]
            conference_seeds[conf] = seeds_list

            # Track Made Playoffs
            for team in seeds_list:
                playoff_tracking[team]['made_playoffs'] += 1

        # 7. RUN PLAYOFF SIMULATOR FOR THIS SEASON
        for conf in ['AFC', 'NFC']:
            seeds = conference_seeds[conf]

            # Wild Card Round
            # Seed 1 Bye. Seed 2 vs 7, Seed 3 vs 6, Seed 4 vs 5
            wc_winner_2 = run_playoff_game(seeds[6], seeds[1]) # 7 at 2
            wc_winner_3 = run_playoff_game(seeds[5], seeds[2]) # 6 at 3
            wc_winner_4 = run_playoff_game(seeds[4], seeds[3]) # 5 at 4

            wc_winners = [seeds[0], wc_winner_2, wc_winner_3, wc_winner_4]
            wc_winners.sort(key=lambda t: seeds.index(t))

            # Track Divisional Reached
            for team in wc_winners:
                playoff_tracking[team]['made_divisional'] += 1

            # Divisional Round
            div_winner_1 = run_playoff_game(wc_winners[3], wc_winners[0]) # Lowest remaining at 1
            div_winner_2 = run_playoff_game(wc_winners[2], wc_winners[1]) # Other two

            div_winners = [div_winner_1, div_winner_2]
            div_winners.sort(key=lambda t: seeds.index(t))

            # Track Conference Reached
            for team in div_winners:
                playoff_tracking[team]['made_conference'] += 1

            # Conference Championship
            conf_champ = run_playoff_game(div_winners[1], div_winners[0])
            playoff_tracking[conf_champ]['made_superbowl'] += 1
            conference_seeds[f"{conf}_champ"] = conf_champ

        # Super Bowl
        afc_champ = conference_seeds["AFC_champ"]
        nfc_champ = conference_seeds["NFC_champ"]

        sb_winner = run_playoff_game(afc_champ, nfc_champ)
        playoff_tracking[sb_winner]['won_superbowl'] += 1

    print("\nAll seasons simulated successfully.")

    # 8. POST-PROCESS PLAYER STATS (Season Cumulative Totals and Percentiles)
    print("\nCalculating player stats cumulative season percentiles...")
    metrics = ['pAtt', 'pCmp', 'pYds', 'pTD', 'int', 'rAtt', 'rYds', 'rTD', 'rec', 'recYds', 'recTD', 'targets', 'fumbles', 'fumbles_lost', 'sacks_taken', 'air_yards', 'dk_score', 'fd_score', 'std_score', 'def_sack', 'def_int', 'def_fumble_rec', 'def_td', 'pts_allowed']

    # Sum stats per player, team, slot, pos, and iteration to get season-long totals
    season_player_grouped = all_players_df.groupby(['Player', 'Team', 'Pos', 'Slot', 'iteration'])[metrics].sum().reset_index()

    # Completion % = season-summed completions / season-summed attempts, NOT
    # the average of per-game rates -- computed here, after the summation
    # above, so each iteration's value is a real ratio-of-sums. NaN (not 0%)
    # for zero-attempt player/iterations so a bench QB's untouched iterations
    # don't drag the percentile distribution toward 0 -- pandas' quantile()
    # skips NaN by default.
    season_player_grouped['cmp_pct'] = np.where(
        season_player_grouped['pAtt'] > 0,
        season_player_grouped['pCmp'] / season_player_grouped['pAtt'] * 100,
        np.nan
    )
    metrics = metrics + ['cmp_pct']

    # Calculate percentiles (min=0%, 5%, 25%, median=50%, 75%, 95%, max=100%)
    quantiles = [0.0, 0.05, 0.25, 0.50, 0.75, 0.95, 1.0]
    player_summary = season_player_grouped.groupby(['Player', 'Team', 'Pos', 'Slot'])[metrics].quantile(quantiles)
    player_summary_unstacked = player_summary.unstack()
    player_summary_unstacked.columns = [f"{col[0]}_p{int(col[1]*100):02d}" for col in player_summary_unstacked.columns]
    player_summary_df = player_summary_unstacked.reset_index()

    # 9. BUILD SYSTEMATIC DIRECTORIES AND GENERATE REPORTS
    # (Team, Player) -> True for anyone with a real target_share or carry_share
    # on the roster -- used below to include every real usage player in the
    # per-team RB/WR/TE report, not just the Slot1-3 bucket that
    # _build_slot_map's naming caps out at (Cam's call, 2026-08-12: "anyone
    # with a target share or rush share" should get stats, deep bench
    # committee players included).
    usage_lookup = {}
    for path in glob.glob(f"data/current_rosters/*_traits_{SIM_YEAR}.json"):
        data = json.load(open(path))
        team_abbr = data["team"]
        for name, traits in data["traits"].items():
            if traits.get("target_share", 0) > 0 or traits.get("carry_share", 0) > 0:
                usage_lookup[(team_abbr, name)] = True

    # Per-game win rate (raw win % across the 1,000 pre-simulated iterations
    # for that specific matchup -- NOT the 11-game-sample season methodology
    # above) + week number, used below to write each team's own matchups
    # report. matchup_sim_results already has every game's real per-iteration
    # outcomes, no extra computation needed.
    week_by_game_id = dict(zip(reg_games['game_id'], reg_games['week']))
    game_win_rates = {}
    for game_id, data in matchup_sim_results.items():
        outcomes = data['outcomes']
        n = len(outcomes)
        away_wins = sum(1 for r in outcomes if r['away_score'] > r['home_score'])
        home_wins = sum(1 for r in outcomes if r['home_score'] > r['away_score'])
        game_win_rates[game_id] = {
            'away': data['away'], 'home': data['home'],
            'week': week_by_game_id.get(game_id),
            'away_win_pct': away_wins / n * 100, 'home_win_pct': home_wins / n * 100,
            'avg_away_score': sum(r['away_score'] for r in outcomes) / n,
            'avg_home_score': sum(r['home_score'] for r in outcomes) / n,
        }

    # Expected wins/losses via direct win-probability summation -- for each
    # team, sum that team's own win_pct (from game_win_rates above, the raw
    # per-game rate across the 1,000 pre-simulated iterations) across its 17
    # real games, ties split 50/50. Distinct from Wins_Mean/Wins_Median
    # above: those come from the 100-season Monte Carlo, which samples 11
    # discrete outcomes per matchup and derives a win/loss from majority --
    # a proxy for the true win probability with its own sampling noise. This
    # is the more direct sum-of-win-probabilities expectation, no sampling
    # involved (Cam's request, 2026-08-16).
    team_expected_wins = {team: 0.0 for team in TEAM_DIVISIONS}
    team_game_counts = {team: 0 for team in TEAM_DIVISIONS}
    for g in game_win_rates.values():
        tie_pct = max(0.0, 100.0 - g['away_win_pct'] - g['home_win_pct'])
        team_expected_wins[g['away']] += (g['away_win_pct'] + 0.5 * tie_pct) / 100.0
        team_expected_wins[g['home']] += (g['home_win_pct'] + 0.5 * tie_pct) / 100.0
        team_game_counts[g['away']] += 1
        team_game_counts[g['home']] += 1

    docs_report_dir = "docs/reports"
    os.makedirs(docs_report_dir, exist_ok=True)

    # Prepare expected/median stats dataframe for CSV and standings md
    summary_data = []
    # Per-team usage boards + matchups, accumulated below and written once as
    # one combined JSON after the team loop -- for the analytics dev site
    # (frontend_analysis Season2026 page), same underlying data as each
    # team's matchups.md/Usage Leaderboard section, just structured.
    teams_data = {}

    for team, tracking in playoff_tracking.items():
        conf, div = TEAM_DIVISIONS[team]

        # Calculate percentiles for wins
        wins_dist = tracking['season_wins']
        losses_dist = tracking['season_losses']
        ties_dist = tracking['season_ties']
        pf_dist = tracking['season_pf']
        pa_dist = tracking['season_pa']

        med_wins = np.percentile(wins_dist, 50)
        med_losses = np.percentile(losses_dist, 50)
        med_ties = np.percentile(ties_dist, 50)
        mean_wins = np.mean(wins_dist)
        mean_losses = np.mean(losses_dist)
        mean_ties = np.mean(ties_dist)

        games_played = team_game_counts[team]
        wins_expected = team_expected_wins[team]

        summary_data.append({
            'Team': team,
            'Conference': conf,
            'Division': div,
            'Wins_Expected': wins_expected,
            'Losses_Expected': games_played - wins_expected,
            'Wins_Mean': mean_wins,
            'Losses_Mean': mean_losses,
            'Ties_Mean': mean_ties,
            'Wins_Median': med_wins,
            'Losses_Median': med_losses,
            'Ties_Median': med_ties,
            'Wins_Min': np.min(wins_dist),
            'Wins_P05': np.percentile(wins_dist, 5),
            'Wins_P25': np.percentile(wins_dist, 25),
            'Wins_P75': np.percentile(wins_dist, 75),
            'Wins_P95': np.percentile(wins_dist, 95),
            'Wins_Max': np.max(wins_dist),
            'PF_Avg': np.mean(pf_dist),
            'PA_Avg': np.mean(pa_dist),
            'Div_Wins_Avg': np.mean(tracking['season_div_wins']),
            'Div_Losses_Avg': np.mean(tracking['season_div_losses']),
            'Div_Ties_Avg': np.mean(tracking['season_div_ties']),
            'Division_%': (tracking['won_division'] / num_seasons) * 100,
            'Playoffs_%': (tracking['made_playoffs'] / num_seasons) * 100,
            'Divional_%': (tracking['made_divisional'] / num_seasons) * 100,
            'Conference_%': (tracking['made_conference'] / num_seasons) * 100,
            'SuperBowl_%': (tracking['made_superbowl'] / num_seasons) * 100,
            'Champion_%': (tracking['won_superbowl'] / num_seasons) * 100,
        })

        # --- TEAM DIRECTORY CREATION ---
        team_dir = f"{docs_report_dir}/{SIM_YEAR}/teams/{team}"
        os.makedirs(team_dir, exist_ok=True)

        # 1. Save Win Distribution Histogram
        plt.figure(figsize=(7, 4.5))
        plt.hist(wins_dist, bins=np.arange(0, 19) - 0.5, rwidth=0.8, color='#1f77b4', edgecolor='black', alpha=0.85)
        plt.title(f"{TEAM_FULL_NAMES[team]} ({team}) Simulated Win Distribution\n(100 Monte Carlo Seasons)", fontsize=11, fontweight='bold')
        plt.xlabel("Simulated Wins", fontsize=10)
        plt.ylabel("Number of Seasons", fontsize=10)
        plt.xticks(np.arange(0, 18))

        # Add percentile lines
        plt.axvline(med_wins, color='red', linestyle='--', linewidth=1.5, label=f'Median: {med_wins:.1f}')
        plt.axvline(np.percentile(wins_dist, 25), color='orange', linestyle=':', linewidth=1.2, label=f'25th Pct: {np.percentile(wins_dist, 25):.1f}')
        plt.axvline(np.percentile(wins_dist, 75), color='green', linestyle=':', linewidth=1.2, label=f'75th Pct: {np.percentile(wins_dist, 75):.1f}')
        plt.legend(loc='upper right', framealpha=0.9, fontsize=9)
        plt.grid(axis='y', linestyle='--', alpha=0.5)
        plt.tight_layout()
        plt.savefig(f"{team_dir}/win_distribution.png", dpi=150)
        plt.close()

        # 1b. Save this team's own matchup win probabilities (subset of the
        # same 272-game report, scoped to this team's 17 games, sorted by
        # week). Raw win rate across the 1,000 pre-simulated iterations for
        # each matchup -- not the 11-game-sample season methodology used for
        # standings above.
        team_games = [g for g in game_win_rates.values() if g['away'] == team or g['home'] == team]
        team_games.sort(key=lambda g: (g['week'] if g['week'] is not None else 99))
        matchups_md = f"# {TEAM_FULL_NAMES[team]} {SIM_YEAR} Matchup Win Probabilities\n"
        matchups_md += "**Raw win rate across the 1,000 pre-simulated iterations for each game.**\n\n"
        matchups_md += "| Week | Matchup | " + team + " Win % | Opponent Win % | " + team + " Avg Score | Opp Avg Score |\n"
        matchups_md += "| :---: | :--- | :---: | :---: | :---: | :---: |\n"
        for g in team_games:
            is_away = g['away'] == team
            opp = g['home'] if is_away else g['away']
            matchup_str = f"{team} @ {opp}" if is_away else f"{opp} @ {team}"
            team_win = g['away_win_pct'] if is_away else g['home_win_pct']
            opp_win = g['home_win_pct'] if is_away else g['away_win_pct']
            team_score = g['avg_away_score'] if is_away else g['avg_home_score']
            opp_score = g['avg_home_score'] if is_away else g['avg_away_score']
            week_str = int(g['week']) if g['week'] is not None else '?'
            matchups_md += f"| {week_str} | {matchup_str} | **{team_win:.1f}%** | {opp_win:.1f}% | {team_score:.1f} | {opp_score:.1f} |\n"
        with open(f"{team_dir}/matchups.md", "w", encoding="utf-8") as f:
            f.write(matchups_md)

        # 2. Save Roster and Cumulative Player Stats Report
        team_players = player_summary_df[player_summary_df['Team'] == team]

        roster_md = f"# 📋 {TEAM_FULL_NAMES[team]} {SIM_YEAR} Roster & Simulated Season Stats\n"
        roster_md += f"**Cumulative season statistics computed over 1,000 simulations**\n"
        roster_md += f"*Shows the floor (min/p05), median (p50), and ceiling (p95/max) projections*\n\n"

        # Usage leaderboards -- every player with real touches, ranked purely
        # by volume (median season total), separate from the full percentile
        # tables below so team usage reads at a glance without digging
        # through every position group (Cam's call, 2026-08-13).
        has_usage_lb = team_players.apply(lambda p: (team, p['Player']) in usage_lookup, axis=1)
        rush_board = team_players[(team_players['Pos'] != 'DST') & (team_players['rAtt_p50'] > 0)].sort_values('rAtt_p50', ascending=False)
        target_board = team_players[has_usage_lb & (team_players['targets_p50'] > 0)].sort_values('targets_p50', ascending=False)
        qb_board = team_players[team_players['Pos'] == 'QB'].sort_values('pYds_p50', ascending=False)

        teams_data[team] = {
            'usage_rushing': rush_board[['Player', 'Pos', 'rAtt_p50', 'rYds_p50', 'rTD_p50', 'std_score_p50']].round(1).to_dict('records'),
            'usage_targets': target_board[['Player', 'Pos', 'targets_p50', 'rec_p50', 'recYds_p50', 'recTD_p50', 'std_score_p50']].round(1).to_dict('records'),
            'usage_qb': qb_board[['Player', 'Slot', 'pAtt_p50', 'pCmp_p50', 'cmp_pct_p50', 'pYds_p50', 'pTD_p50', 'int_p50', 'rAtt_p50', 'rYds_p50', 'rTD_p50', 'std_score_p50', 'dk_score_p50']].round(1).to_dict('records'),
            'matchups': [
                {
                    'week': int(g['week']) if g['week'] is not None else None,
                    'opponent': g['home'] if g['away'] == team else g['away'],
                    'is_away': g['away'] == team,
                    'team_win_pct': round(g['away_win_pct'] if g['away'] == team else g['home_win_pct'], 1),
                    'opp_win_pct': round(g['home_win_pct'] if g['away'] == team else g['away_win_pct'], 1),
                    'team_avg_score': round(g['avg_away_score'] if g['away'] == team else g['avg_home_score'], 1),
                    'opp_avg_score': round(g['avg_home_score'] if g['away'] == team else g['avg_away_score'], 1),
                }
                for g in team_games
            ],
        }

        roster_md += "## Usage Leaderboard\n\n"
        roster_md += "**By Rushing Attempts (season median)**\n\n"
        roster_md += "| Player | Pos | Rush Att | Rush Yds | Rush TDs |\n"
        roster_md += "| :--- | :---: | :---: | :---: | :---: |\n"
        for _, p in rush_board.iterrows():
            roster_md += f"| **{p['Player']}** | {p['Pos']} | {p['rAtt_p50']:.1f} | {p['rYds_p50']:.1f} | {p['rTD_p50']:.1f} |\n"
        # Column sum of the rows shown above -- a plain sum of per-player
        # medians (volume stats are additive; the median-vs-mean gap on these
        # counting stats is <1%). A per-iteration-summed team total is in the
        # percentile tables below.
        roster_md += (f"| **Total** | — | {rush_board['rAtt_p50'].sum():.1f} | "
                      f"{rush_board['rYds_p50'].sum():.1f} | {rush_board['rTD_p50'].sum():.1f} |\n")
        roster_md += "\n**By Targets (season median)**\n\n"
        roster_md += "| Player | Pos | Targets | Rec | Rec Yds | Rec TDs |\n"
        roster_md += "| :--- | :---: | :---: | :---: | :---: | :---: |\n"
        for _, p in target_board.iterrows():
            roster_md += f"| **{p['Player']}** | {p['Pos']} | {p['targets_p50']:.1f} | {p['rec_p50']:.1f} | {p['recYds_p50']:.1f} | {p['recTD_p50']:.1f} |\n"
        roster_md += (f"| **Total** | — | {target_board['targets_p50'].sum():.1f} | "
                      f"{target_board['rec_p50'].sum():.1f} | {target_board['recYds_p50'].sum():.1f} | "
                      f"{target_board['recTD_p50'].sum():.1f} |\n")
        roster_md += "\n"

        # Sort players by position (QBs, RBs, WRs, TEs, DST) and projection value.
        # RB/WR/TE are filtered by real roster usage share (usage_lookup, see
        # above), not a fixed Slot1-3 whitelist -- a deep-bench committee
        # player with a real, if small, target_share/carry_share still shows.
        has_usage = team_players.apply(lambda p: (team, p['Player']) in usage_lookup, axis=1)
        for pos_name, pos_group in [('Quarterbacks', ['QB']), ('Running Backs', ['RB']), ('Wide Receivers & Tight Ends', ['WR', 'TE']), ('Defense & Special Teams', ['DST'])]:
            roster_md += f"## {pos_name}\n\n"
            if pos_group == ['QB'] or pos_group == ['DST']:
                sub_players = team_players[team_players['Pos'].isin(pos_group)]
            else:
                sub_players = team_players[team_players['Pos'].isin(pos_group) & has_usage]
            if sub_players.empty:
                roster_md += "*No active players in this category.*\n\n"
                continue

            if 'QB' in pos_group:
                roster_md += "| Player | Slot | Stat | Min | 5th Pct | 25th Pct | Median | 75th Pct | 95th Pct | Max |\n"
                roster_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
                stat_list = [('Pass Yds', 'pYds'), ('Pass TDs', 'pTD'), ('Cmp %', 'cmp_pct'), ('INTs', 'int'), ('Std Fantasy', 'std_score'), ('DK Fantasy', 'dk_score')]
                for _, p in sub_players.iterrows():
                    for stat_lbl, stat_col in stat_list:
                        roster_md += f"| **{p['Player']}** | {p['Slot']} | {stat_lbl} | {p[f'{stat_col}_p00']:.1f} | {p[f'{stat_col}_p05']:.1f} | {p[f'{stat_col}_p25']:.1f} | **{p[f'{stat_col}_p50']:.1f}** | {p[f'{stat_col}_p75']:.1f} | {p[f'{stat_col}_p95']:.1f} | {p[f'{stat_col}_p100']:.1f} |\n"
                totals = _team_group_totals(season_player_grouped, team, pos_group, ['pYds', 'pTD', 'int', 'std_score', 'dk_score', 'pAtt', 'pCmp'])
                if totals:
                    for stat_lbl, stat_col in stat_list:
                        roster_md += f"| **Team Total** | — | {stat_lbl} | {totals[f'{stat_col}_p00']:.1f} | {totals[f'{stat_col}_p05']:.1f} | {totals[f'{stat_col}_p25']:.1f} | **{totals[f'{stat_col}_p50']:.1f}** | {totals[f'{stat_col}_p75']:.1f} | {totals[f'{stat_col}_p95']:.1f} | {totals[f'{stat_col}_p100']:.1f} |\n"
            elif 'RB' in pos_group:
                roster_md += "| Player | Slot | Stat | Min | 5th Pct | 25th Pct | Median | 75th Pct | 95th Pct | Max |\n"
                roster_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
                stat_list = [('Rush Att', 'rAtt'), ('Rush Yds', 'rYds'), ('Rush TDs', 'rTD'), ('Receptions', 'rec'), ('Std Fantasy', 'std_score'), ('DK Fantasy', 'dk_score')]
                for _, p in sub_players.iterrows():
                    for stat_lbl, stat_col in stat_list:
                        roster_md += f"| **{p['Player']}** | {p['Slot']} | {stat_lbl} | {p[f'{stat_col}_p00']:.1f} | {p[f'{stat_col}_p05']:.1f} | {p[f'{stat_col}_p25']:.1f} | **{p[f'{stat_col}_p50']:.1f}** | {p[f'{stat_col}_p75']:.1f} | {p[f'{stat_col}_p95']:.1f} | {p[f'{stat_col}_p100']:.1f} |\n"
                totals = _team_group_totals(season_player_grouped, team, pos_group, ['rAtt', 'rYds', 'rTD', 'rec', 'std_score', 'dk_score'])
                if totals:
                    for stat_lbl, stat_col in stat_list:
                        roster_md += f"| **Team Total** | — | {stat_lbl} | {totals[f'{stat_col}_p00']:.1f} | {totals[f'{stat_col}_p05']:.1f} | {totals[f'{stat_col}_p25']:.1f} | **{totals[f'{stat_col}_p50']:.1f}** | {totals[f'{stat_col}_p75']:.1f} | {totals[f'{stat_col}_p95']:.1f} | {totals[f'{stat_col}_p100']:.1f} |\n"
            elif 'WR' in pos_group:
                roster_md += "| Player | Slot | Stat | Min | 5th Pct | 25th Pct | Median | 75th Pct | 95th Pct | Max |\n"
                roster_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
                stat_list = [('Targets', 'targets'), ('Receptions', 'rec'), ('Rec Yds', 'recYds'), ('Rec TDs', 'recTD'), ('Std Fantasy', 'std_score'), ('DK Fantasy', 'dk_score')]
                for _, p in sub_players.iterrows():
                    for stat_lbl, stat_col in stat_list:
                        roster_md += f"| **{p['Player']}** | {p['Slot']} | {stat_lbl} | {p[f'{stat_col}_p00']:.1f} | {p[f'{stat_col}_p05']:.1f} | {p[f'{stat_col}_p25']:.1f} | **{p[f'{stat_col}_p50']:.1f}** | {p[f'{stat_col}_p75']:.1f} | {p[f'{stat_col}_p95']:.1f} | {p[f'{stat_col}_p100']:.1f} |\n"
                totals = _team_group_totals(season_player_grouped, team, pos_group, ['targets', 'rec', 'recYds', 'recTD', 'std_score', 'dk_score'])
                if totals:
                    for stat_lbl, stat_col in stat_list:
                        roster_md += f"| **Team Total** | — | {stat_lbl} | {totals[f'{stat_col}_p00']:.1f} | {totals[f'{stat_col}_p05']:.1f} | {totals[f'{stat_col}_p25']:.1f} | **{totals[f'{stat_col}_p50']:.1f}** | {totals[f'{stat_col}_p75']:.1f} | {totals[f'{stat_col}_p95']:.1f} | {totals[f'{stat_col}_p100']:.1f} |\n"
            else:
                roster_md += "| DST Team | Stat | Min | 5th Pct | 25th Pct | Median | 75th Pct | 95th Pct | Max |\n"
                roster_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
                for _, p in sub_players.iterrows():
                    for stat_lbl, stat_col in [('Sacks', 'def_sack'), ('INTs', 'def_int'), ('Fumble Rec', 'def_fumble_rec'), ('TDs', 'def_td'), ('Pts Allowed', 'pts_allowed'), ('Std Fantasy', 'std_score'), ('DK Fantasy', 'dk_score')]:
                        roster_md += f"| **{p['Player']}** | {stat_lbl} | {p[f'{stat_col}_p00']:.1f} | {p[f'{stat_col}_p05']:.1f} | {p[f'{stat_col}_p25']:.1f} | **{p[f'{stat_col}_p50']:.1f}** | {p[f'{stat_col}_p75']:.1f} | {p[f'{stat_col}_p95']:.1f} | {p[f'{stat_col}_p100']:.1f} |\n"
                # No Team Total row here -- DST is already one row per team,
                # a "total" of a single entry would just repeat it.
            roster_md += "\n"

        with open(f"{team_dir}/player_stats_and_roster.md", "w", encoding="utf-8") as f:
            f.write(roster_md)

    teams_data_path = f"{docs_report_dir}/{SIM_YEAR}/teams_data.json"
    with open(teams_data_path, "w", encoding="utf-8") as f:
        json.dump(teams_data, f)
    print(f"Wrote {teams_data_path}")

    summary_df = pd.DataFrame(summary_data)

    # Save CSV summary
    summary_df.to_csv(f"{docs_report_dir}/season_summaries_{SIM_YEAR}.csv", index=False)
    print(f"Saved season summaries CSV to {docs_report_dir}/season_summaries_{SIM_YEAR}.csv")

    # 10. RENDER STANDINGS MD BY DIVISION/CONFERENCE (Using Median Records)
    standings_md = f"# 🏆 NFL {SIM_YEAR} Expected Standings & Playoff Probabilities\n"
    standings_md += f"**Calculated from expected records & playoff brackets simulated across {num_seasons} seasons**\n"
    standings_md += f"*(Traditional W/L outcomes sampled via 11-game sets per matchup. Standing records show Medians)*\n\n"

    for conf in ['AFC', 'NFC']:
        standings_md += f"## {conf} Conference\n\n"

        for div in ['East', 'North', 'South', 'West']:
            standings_md += f"### {conf} {div}\n"
            standings_md += "| Team | Win% Expected Record | Mean (Expected) Record | Median Record | Range (5%-95%) | Expected Div Rec | Points For (Avg) | Points Against (Avg) | Diff | Playoffs % | Made Divisional % | Made Conf Champ % | Made Super Bowl % | Super Bowl Winner % |\n"
            standings_md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"

            div_df = summary_df[(summary_df['Conference'] == conf) & (summary_df['Division'] == div)]
            # Sort by expected wins
            div_df = div_df.sort_values(by=['Wins_Median', 'Wins_P75', 'PF_Avg'], ascending=False)

            for _, r in div_df.iterrows():
                win_pct_record_str = f"{r['Wins_Expected']:.1f} - {r['Losses_Expected']:.1f}"

                mean_record_str = f"{r['Wins_Mean']:.1f} - {r['Losses_Mean']:.1f}"
                if r['Ties_Mean'] > 0:
                    mean_record_str += f" - {r['Ties_Mean']:.1f}"

                record_str = f"{r['Wins_Median']:.1f} - {r['Losses_Median']:.1f}"
                if r['Ties_Median'] > 0:
                    record_str += f" - {r['Ties_Median']:.1f}"

                range_str = f"{r['Wins_P05']:.0f} to {r['Wins_P95']:.0f} wins"

                div_record_str = f"{r['Div_Wins_Avg']:.1f} - {r['Div_Losses_Avg']:.1f}"
                if r['Div_Ties_Avg'] > 0:
                    div_record_str += f" - {r['Div_Ties_Avg']:.1f}"

                diff = r['PF_Avg'] - r['PA_Avg']
                diff_indicator = f"+{diff:.1f}" if diff > 0 else f"{diff:.1f}"

                standings_md += f"| **{r['Team']}** | {win_pct_record_str} | {mean_record_str} | {record_str} | {range_str} | {div_record_str} | {r['PF_Avg']:.1f} | {r['PA_Avg']:.1f} | `{diff_indicator}` | {r['Playoffs_%']:.1f}% | {r['Divional_%']:.1f}% | {r['Conference_%']:.1f}% | {r['SuperBowl_%']:.1f}% | **{r['Champion_%']:.1f}%** |\n"
            standings_md += "\n"

    with open(f"{docs_report_dir}/season_standings_{SIM_YEAR}.md", "w", encoding="utf-8") as f:
        f.write(standings_md)

    print(f"Saved season standings markdown report to {docs_report_dir}/season_standings_{SIM_YEAR}.md")

if __name__ == "__main__":
    simulate_full_season_and_playoffs(iterations=1000, num_seasons=100)
    print("\nFull season simulation and playoff trees completed successfully!")
