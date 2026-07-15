import sys
import os
import numpy as np

# Adjust path to import from src
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from legacy.game_engine_sequential import SequentialNFLGameEngine as NFLGameEngine
from src.nfl_sim.models.fourth_down_conversion_v_0_1_0.inference import FourthDownConversionModelV010
from src.nfl_sim.proe_overlay_v_0_1_0 import apply_proe_overlay

def print_separator(char="=", length=70):
    print(char * length)

def get_ordinal(n):
    return f"{n}th" if 11 <= (n % 100) <= 13 else ["th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th"][n % 10]

def run_diagnostic_game():
    print_separator()
    print("      NFLSims Play-by-Play Real-Time Diagnostic & Decision Runner")
    print_separator()
    
    # Initialize engine
    away_team = "KC"
    home_team = "SF"
    engine = NFLGameEngine(away_team=away_team, home_team=home_team, year=2025)
    
    # Load Fourth Down Conversion Model directly for diagnostics
    fd_conv_model = None
    try:
        fd_conv_dir = "src/nfl_sim/models/fourth_down_conversion_v_0_1_0"
        if os.path.exists(fd_conv_dir):
            fd_conv_model = FourthDownConversionModelV010(fd_conv_dir)
            print("[DIAGNOSTIC] Loaded FourthDownConversionModelV010 successfully.")
    except Exception as e:
        print(f"[DIAGNOSTIC] Warning: Could not load 4th down conversion model: {e}")
    
    print(f"[DIAGNOSTIC] Game initialized: {away_team} at {home_team}")
    print("Press Enter after each play to advance. Type 'q' and press Enter to quit.")
    print_separator()
    
    play_count = 0
    while not engine.game_over:
        # Pre-play state parameters
        pos = engine.possession
        defending = engine.defending
        score_diff = engine.scores[pos] - engine.scores[defending]
        game_sec = engine._get_game_sec()
        
        # Format clock
        min_rem = engine.time_remaining // 60
        sec_rem = engine.time_remaining % 60
        
        # =====================================================================
        # 1. PRE-PLAY SITUATIONAL DASHBOARD
        # =====================================================================
        print("\n" + "=" * 70)
        print(f" PLAY {play_count + 1:3d}  |  Q{engine.quarter} - {min_rem:02d}:{sec_rem:02d}  |  Possession: {pos}")
        print("=" * 70)
        
        if engine.yardline_100 <= 50:
            situation = f"{get_ordinal(engine.down)} & {engine.distance} at {defending} {engine.yardline_100}"
        else:
            situation = f"{get_ordinal(engine.down)} & {engine.distance} at {pos} {100 - engine.yardline_100}"
            
        print(f" Situation : {situation}")
        print(f" Score     : KC {engine.scores['KC']} - SF {engine.scores['SF']} (Margin: {score_diff:3d})")
        print(f" Timeouts  : KC {engine.timeouts['KC']} - SF {engine.timeouts['SF']}")
        
        # =====================================================================
        # 2. WIN PROBABILITY DIAGNOSTICS
        # =====================================================================
        wp_state = {
            'score_differential': score_diff,
            'game_seconds_remaining': game_sec,
            'down': engine.down,
            'ydstogo': engine.distance,
            'yardline_100': engine.yardline_100,
            'posteam_timeouts_remaining': engine.timeouts[pos],
            'defteam_timeouts_remaining': engine.timeouts[defending],
            'receive_2h_ko': 1.0 if engine.quarter <= 2 else 0.0
        }
        current_wp = engine.registry.predict_win_probability(wp_state)
        print(f" Win Prob  : P(Win for {pos}) = {current_wp * 100:.2f}% (Model: win_probability_v_0_1_0)")
        
        # =====================================================================
        # 3. SPECIAL TEAM / 4th DOWN BOT EV REASONING
        # =====================================================================
        is_kickoff = engine.needs_kickoff
        is_4th = (engine.down == 4 and not is_kickoff)
        
        if is_4th:
            print("\n>>> [4th DOWN BOT DECISION ENGINE]")
            
            # Field Goal success probability
            fg_prob = engine.registry.predict_fg_success([engine.yardline_100])
            kick_dist = engine.yardline_100 + 17
            print(f"  * Field Goal Success : P(Make from {kick_dist} yds) = {fg_prob * 100:.2f}% (Model: fg_v_0_1_0)")
            if kick_dist > 65:
                print("    --> Note: Distance > 65 yards. Field goal attempts are prohibited under normal rules.")
            
            # Fourth down conversion probability
            fd_prob = 0.54
            if fd_conv_model:
                fd_state = {
                    'ydstogo': engine.distance,
                    'yardline_100': engine.yardline_100,
                    'score_differential': score_diff,
                    'game_seconds_remaining': game_sec
                }
                fd_prob = fd_conv_model.predict_conversion_probability(fd_state)
                print(f"  * Conversion Success : P(Convert) = {fd_prob * 100:.2f}% (Model: fourth_down_conversion_v_0_1_0)")
            
            # Base engine probas
            features = [engine.yardline_100, engine.distance, game_sec, score_diff, engine.timeouts[pos]]
            probas = engine.registry.predict_4th_down_probas(features)
            print(f"  * Engine Choice Prior: P(Punt)={probas[0]*100:.1f}%, P(FG)={probas[1]*100:.1f}%, P(Go)={probas[2]*100:.1f}%")
            
            # Coach Aggression Overlays
            coach_name = engine.team_coaches.get(pos, "Unknown")
            coach_dna = engine.dna['coach'].get(coach_name, {})
            aggression = (coach_dna.get('deep_shot_rate', 0.12) - 0.12) * 2.0
            adjusted_probas = probas.copy()
            adjusted_probas[2] += aggression
            decision_list = ["PUNT", "FIELD_GOAL", "GO"]
            chosen = decision_list[np.argmax(adjusted_probas)]
            print(f"  * Coach Aggression   : {coach_name} (Aggression bias: {aggression:+.4f})")
            print(f"  * Final Choice       : {chosen} (Highest biased probability)")
            print("-" * 70)
            
        elif not is_kickoff:
            # =====================================================================
            # 4. PLAY SELECTION / STRATEGIC INTENT DIAGNOSTICS
            # =====================================================================
            print("\n>>> [STRATEGIC PLAY SELECTION]")
            leverage = score_diff * game_sec
            ps_features = [
                float(engine.yardline_100), 
                float(game_sec), 
                float(score_diff), 
                float(engine.timeouts[pos]), 
                float(engine.timeouts[defending]), 
                float(leverage)
            ]
            ps_state = {
                'down': engine.down, 
                'distance': engine.distance,
                'active_qb': engine._get_starter('QB'),
                'active_rb': engine._get_starter('RB'),
                'posteam_coach': engine.team_coaches.get(pos, "Unknown"),
                'yardline_100': engine.yardline_100
            }
            base_pass_prob = engine.registry.predict_play_selection_proba(ps_state, ps_features)
            adjusted_pass_prob = apply_proe_overlay(base_pass_prob, team=pos)
            
            print(f"  * Model Base Pass Prob : {base_pass_prob * 100:.2f}% (Bucket: {engine.registry.get_bucket_name(engine.down, engine.distance)})")
            print(f"  * Coach Strategic DNA  : {ps_state['posteam_coach']} (PROE Adjusted Pass Prob: {adjusted_pass_prob * 100:.2f}%)")
            
            # =====================================================================
            # 5. DEFENSIVE MATCHUP & TRENCH PHYSICS
            # =====================================================================
            print(f"\n>>> [DEFENSIVE MATCHUP & TRENCHES]")
            t_dna_off = engine.dna['trench'].get(str(engine.year), {}).get(pos, {})
            t_dna_def = engine.dna['trench'].get(str(engine.year), {}).get(defending, {})
            
            off_t = engine.trench_tiers.get(pos, {})
            def_t = engine.trench_tiers.get(defending, {})
            
            print(f"  * Offense Trench : OL Pass Block Tier = {off_t.get('pass_block_tier', 3)} | OL Run Block Tier = {off_t.get('run_block_tier', 3)}")
            print(f"  * Defense Trench : DL Pass Rush Tier = {def_t.get('pass_rush_tier', 3)} | DL Run Defense Tier = {def_t.get('run_defense_tier', 3)}")
            
            # Passing Trench Pressure & Sack probabilities
            avg_ttt = engine.dna['qb'].get(ps_state['active_qb'], {}).get('avg_time_to_throw_sec', 2.7)
            prob_pressure = 0.22 + (off_t.get('pass_block_tier', 3) - def_t.get('pass_rush_tier', 3)) * 0.05
            print(f"  * Trench Matchup : P(Pass Pressure) = {prob_pressure * 100:.1f}%")
            print(f"  * Defender DNA   : P(Sack if Pressure) = 18.00% (Adjusted by QB DNA Sack Allowed Rate: {t_dna_off.get('sack_rate_allowed', 0.06) * 100:.1f}%)")
            
            # Rushing parameters
            run_block_z = 3.0 - off_t.get('run_block_tier', 3.0)
            box_density_z = off_t.get('qb_cpoe_z', 0.0) * -0.5
            print(f"  * Rushing Trench : Run Block Z-Score = {run_block_z:+.2f} | Defensive Box Density Z-Score = {box_density_z:+.2f}")
            print("-" * 70)

        # Wait for user input to step play
        inp = input("Press Enter to execute the play (or 'q' to quit)... ")
        if inp.strip().lower() == 'q':
            print("Exiting diagnostic simulation.")
            break
            
        # Simulate and print
        play_desc = engine.simulate_play()
        print(f"\n[PLAY RESOLUTION]\n  {play_desc}")
        print("=" * 70)
        
        play_count += 1
        
    if engine.game_over:
        print_separator()
        print("                  GAME COMPLETED")
        print_separator()
        print(f"Final Score: KC {engine.scores['KC']} - SF {engine.scores['SF']}")
        winner = "KC" if engine.scores["KC"] > engine.scores["SF"] else "SF" if engine.scores["SF"] > engine.scores["KC"] else "TIE"
        print(f"Winner: {winner}")
        print_separator()

if __name__ == "__main__":
    run_diagnostic_game()
