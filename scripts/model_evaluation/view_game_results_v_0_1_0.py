import pandas as pd
import sys

def view_game(week, game_id):
    player_path = f'reports/season_sim/week_{week}_player_projections.csv'
    game_path = f'reports/season_sim/week_{week}_game_summary.csv'
    
    if not pd.io.common.file_exists(player_path):
        print(f"No reports found for Week {week}. Run the simulation first.")
        return

    players = pd.read_csv(player_path)
    games = pd.read_csv(game_path)
    
    # Filter for the specific game
    game_stats = games[games['game_id'] == game_id].iloc[0]
    game_players = players[players['game_id'] == game_id].sort_values('Avg_DK', ascending=False)
    
    print("\n" + "="*60)
    print(f" SIMULATED BOX SCORE: {game_id}")
    print("="*60)
    print(f"RESULT: {game_stats['away_team']} {game_stats['avg_away_score']} - {game_stats['home_team']} {game_stats['avg_home_score']}")
    print(f"WIN RATE: {game_stats['away_team']} {game_stats['away_win_rate']*100:.1f}% | TOTAL: {game_stats['avg_total']}")
    print("-"*60)
    print(f"{'PLAYER':<20} {'TEAM':<6} {'AVG DK':<8} {'CEILING':<8} {'YDS':<8}")
    print("-"*60)
    
    for _, p in game_players.head(15).iterrows():
        print(f"{p['Player']:<20} {p['Team']:<6} {p['Avg_DK']:<8} {p['Ceiling_DK']:<8} {p['Avg_Yds']:<8}")
    print("="*60)

if __name__ == "__main__":
    # Example: Detroit vs Kansas City
    view_game(week=1, game_id="2023_01_DET_KC")
