import os
import pandas as pd
import numpy as np
import nfl_data_py as nfl


def analyze_plays_per_game():
    """
    Quick EDA: offensive plays/game and defensive plays faced/game, per team,
    2025 vs. pooled 2016-2024. Triggered by a manual observation that Bills
    games look light on offensive plays in 2025 game logs.
    """
    print("==================================================")
    print("Plays Per Game EDA: 2025 vs 2016-2024")
    print("==================================================")

    output_dir = "docs/eda_outputs/ppg"
    os.makedirs(output_dir, exist_ok=True)

    seasons = list(range(2016, 2026))
    cols = ['game_id', 'season', 'season_type', 'posteam', 'defteam', 'play_type']
    try:
        df_raw = nfl.import_pbp_data(seasons, columns=cols)
    except Exception as e:
        print(f"Column-limited import failed ({e}), falling back to full import...")
        df_raw = nfl.import_pbp_data(seasons)
        df_raw = df_raw[[c for c in cols if c in df_raw.columns]]

    print(f"Loaded {len(df_raw)} rows.")
    df = df_raw[df_raw['season_type'] == 'REG'].copy()
    print(f"Filtered to Regular Season: {len(df)} rows.")

    scrimmage = df[df['play_type'].isin(['pass', 'run'])].copy()

    # Offensive plays per team-game
    off_game = scrimmage.dropna(subset=['posteam']).groupby(['season', 'game_id', 'posteam']).size().reset_index(name='off_plays')
    off_game = off_game.rename(columns={'posteam': 'team'})

    # Defensive plays FACED per team-game
    def_game = scrimmage.dropna(subset=['defteam']).groupby(['season', 'game_id', 'defteam']).size().reset_index(name='def_plays_faced')
    def_game = def_game.rename(columns={'defteam': 'team'})

    # Team-season averages (plays/game)
    off_team_season = off_game.groupby(['season', 'team'])['off_plays'].mean().reset_index()
    def_team_season = def_game.groupby(['season', 'team'])['def_plays_faced'].mean().reset_index()
    team_season = off_team_season.merge(def_team_season, on=['season', 'team'], how='outer')

    # -------------------------------------------------------------
    # 2025 full team table
    # -------------------------------------------------------------
    t2025 = team_season[team_season['season'] == 2025].sort_values('off_plays', ascending=False).reset_index(drop=True)

    # -------------------------------------------------------------
    # League summary: 2025 vs pooled 2016-2024
    # -------------------------------------------------------------
    pooled_prior = team_season[team_season['season'].between(2016, 2024)]

    summary = {
        '2025_off_mean': float(t2025['off_plays'].mean()),
        '2025_off_median': float(t2025['off_plays'].median()),
        '2025_def_mean': float(t2025['def_plays_faced'].mean()),
        '2025_def_median': float(t2025['def_plays_faced'].median()),
        '2016_2024_off_mean': float(pooled_prior['off_plays'].mean()),
        '2016_2024_off_median': float(pooled_prior['off_plays'].median()),
        '2016_2024_def_mean': float(pooled_prior['def_plays_faced'].mean()),
        '2016_2024_def_median': float(pooled_prior['def_plays_faced'].median()),
    }

    print("\n2025 vs 2016-2024 (league-wide, team-season averages):")
    for k, v in summary.items():
        print(f"  {k}: {v:.2f}")

    # Per-season trend (helps see if 2025 is an outlier year or part of a longer trend)
    season_trend = team_season.groupby('season').agg(
        off_mean=('off_plays', 'mean'),
        off_median=('off_plays', 'median'),
        def_mean=('def_plays_faced', 'mean'),
        def_median=('def_plays_faced', 'median'),
    ).reset_index()

    # -------------------------------------------------------------
    # Save outputs
    # -------------------------------------------------------------
    t2025.to_csv(os.path.join(output_dir, "team_plays_per_game_2025.csv"), index=False)
    season_trend.to_csv(os.path.join(output_dir, "season_trend_2016_2025.csv"), index=False)

    buf_2025 = t2025[t2025['team'] == 'BUF']
    buf_row = buf_2025.iloc[0] if len(buf_2025) else None

    md_path = os.path.join(output_dir, "README.md")
    with open(md_path, 'w', encoding='utf-8') as f:
        f.write("# Plays Per Game — 2025 vs. 2016-2024\n\n")
        f.write("Triggered by a manual observation from game logs that Bills games look light on offensive plays in 2025. ")
        f.write("`off_plays` = offensive scrimmage plays (pass + run) per game, averaged across a team's season. ")
        f.write("`def_plays_faced` = same count from the opposing offense's perspective (plays this team's defense faced).\n\n")

        f.write("## League-Wide Summary\n\n")
        f.write("| | 2025 Mean | 2025 Median | 2016-2024 Mean | 2016-2024 Median |\n")
        f.write("|---|---:|---:|---:|---:|\n")
        f.write(f"| Offensive plays/game | {summary['2025_off_mean']:.2f} | {summary['2025_off_median']:.1f} | {summary['2016_2024_off_mean']:.2f} | {summary['2016_2024_off_median']:.1f} |\n")
        f.write(f"| Defensive plays faced/game | {summary['2025_def_mean']:.2f} | {summary['2025_def_median']:.1f} | {summary['2016_2024_def_mean']:.2f} | {summary['2016_2024_def_median']:.1f} |\n\n")

        if buf_row is not None:
            f.write(f"**Bills (BUF) 2025:** {buf_row['off_plays']:.2f} offensive plays/game, {buf_row['def_plays_faced']:.2f} defensive plays faced/game.\n\n")

        f.write("## Per-Season Trend (2016-2025)\n\n")
        f.write("| Season | Off Mean | Off Median | Def Mean | Def Median |\n")
        f.write("|---|---:|---:|---:|---:|\n")
        for _, r in season_trend.iterrows():
            f.write(f"| {int(r['season'])} | {r['off_mean']:.2f} | {r['off_median']:.1f} | {r['def_mean']:.2f} | {r['def_median']:.1f} |\n")
        f.write("\n")

        f.write("## Full 2025 Team Table (sorted by offensive plays/game)\n\n")
        f.write("| Team | Off Plays/Game | Def Plays Faced/Game |\n")
        f.write("|---|---:|---:|\n")
        for _, r in t2025.iterrows():
            f.write(f"| {r['team']} | {r['off_plays']:.2f} | {r['def_plays_faced']:.2f} |\n")

    print(f"\nSaved report to {md_path}")
    print(f"Saved team table to {os.path.join(output_dir, 'team_plays_per_game_2025.csv')}")
    print(f"Saved season trend to {os.path.join(output_dir, 'season_trend_2016_2025.csv')}")


if __name__ == '__main__':
    analyze_plays_per_game()
