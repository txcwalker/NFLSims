"""Formats weekly simulation CSVs into human-readable box-score text.

Reads a week's game-summary and player-projection CSVs, groups players by game,
sorts by projected DraftKings points, and writes an aligned plain-text box score
(game header with scores/plays/win-rate; top-15 players with position-tailored
usage — comp/att for QBs, rush/targets for skill players).

Entry point: generate_weekly_report(week).
Full design rationale: see report_generator.md.
"""

import pandas as pd
import os

def generate_weekly_report(week):
    player_path = f'reports/season_sim/week_{week}_hybrid_player_projections.csv'
    game_path = f'reports/season_sim/week_{week}_hybrid_game_summary.csv'
    output_path = f'reports/season_sim/week_{week}_full_box_scores.txt'
    
    if not os.path.exists(player_path):
        print(f"No hybrid reports found for Week {week}.")
        return

    players = pd.read_csv(player_path)
    games = pd.read_csv(game_path)
    
    with open(output_path, 'w') as f:
        f.write(f"{'='*100}\n")
        f.write(f" WEEK {week} HYBRID UNLEASHED SIMULATION REPORT (PASS 1b - CORRECTED)\n")
        f.write(f"{'='*100}\n\n")
        
        for _, g in games.iterrows():
            game_id = g['game_id']
            game_players = players[players['game_id'] == game_id].sort_values('Avg_DK', ascending=False)
            
            f.write(f"{'='*100}\n")
            f.write(f" GAME: {game_id}  |  TOTAL PLAYS: {g['avg_plays']}\n")
            f.write(f"{'='*100}\n")
            f.write(f" RESULT: {g['away_team']} {g['avg_away_score']:.1f} - {g['home_team']} {g['avg_home_score']:.1f}\n")
            f.write(f" WIN RATE: {g['away_team']} {g['away_win_rate']*100:.1f}% | TOTAL: {g['avg_total']:.1f}\n")
            f.write(f"{'-'*100}\n")
            f.write(f"{'PLAYER':<20} {'POS':<6} {'TEAM':<6} {'AVG DK':<8} {'CEIL':<8} {'YDS':<8} {'TD':<6} {'USAGE':<15}\n")
            f.write(f"{'-'*100}\n")
            
            for _, p in game_players.head(15).iterrows():
                # Correct Position Labeling
                pos = str(p['Pos'])
                
                # Format Usage
                if 'QB' in pos:
                    usage = f"{p['Avg_pCmp']:.1f}/{p['Avg_pAtt']:.1f}, {p['Avg_rAtt']:.1f}r"
                else:
                    usage = f"{p['Avg_rAtt']:.1f}r, {p['Avg_Rec']:.1f}/{p['Avg_Targets']:.1f}t"
                
                f.write(f"{p['Player']:<20} {pos:<6} {p['Team']:<6} {p['Avg_DK']:<8.2f} {p['Ceiling_DK']:<8.2f} {p['Avg_Yds']:<8.2f} {p['Avg_TDs']:<6.2f} {usage:<15}\n")
            f.write("\n\n")
            
    print(f"Master Weekly Report (Pass 1b Corrected) regenerated at: {output_path}")

if __name__ == "__main__":
    generate_weekly_report(week=1)
