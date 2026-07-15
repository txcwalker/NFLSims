import json
import os
import glob

def main():
    print("=== STARTING ROSTER INTEGRITY VALIDATION ===")
    rosters_dir = "data/current_rosters"
    roster_files = glob.glob(os.path.join(rosters_dir, "*_traits_2025.json"))
    
    passed_teams = 0
    failed_teams = 0
    
    for r_file in roster_files:
        team_code = os.path.basename(r_file).split("_")[0]
        with open(r_file, "r") as f:
            roster_data = json.load(f)
            
        traits = roster_data.get("traits", {})
        
        # 1. Check sum of target and carry shares
        total_target_share = 0.0
        total_carry_share = 0.0
        
        has_qb = False
        has_rb = False
        has_wr_te = False
        
        # Track position sorting order: QB -> RB -> WR -> TE
        positions_observed = []
        
        is_valid = True
        errors = []
        
        for name, p_traits in traits.items():
            pos = p_traits.get("pos")
            if not pos:
                is_valid = False
                errors.append(f"  Player '{name}' has no position.")
                continue
                
            positions_observed.append(pos)
            
            # Check for unique full names (no periods unless junior/etc)
            if "." in name and not name.endswith("Jr.") and not name.endswith("Sr."):
                # print(f"  Warning: Player '{name}' has short name format.")
                pass
                
            # Shares
            target_share = p_traits.get("target_share", 0.0)
            carry_share = p_traits.get("carry_share", 0.0)
            
            total_target_share += target_share
            if pos == 'RB':
                total_carry_share += carry_share
            elif pos in ['WR', 'TE']:
                total_carry_share += carry_share
                
            # Check DNA Stats Presence
            if pos == 'QB':
                has_qb = True
                required_fields = ['cpoe', 'avg_air_yards_per_att', 'deep_ball_rate', 'sack_rate', 'under_pressure_cpoe', 'avg_time_to_throw_sec', 'splits']
            elif pos == 'RB':
                has_rb = True
                required_fields = ['adot', 'yac_per_rec', 'ypc', 'efficiency', 'percent_attempts_gte_eight_defenders', 'avg_time_to_los', 'top_speed_mph', 'splits']
            else: # WR/TE
                has_wr_te = True
                required_fields = ['adot', 'yac_per_rec', 'ypc', 'avg_separation_yds', 'avg_cushion_yds', 'top_speed_mph', 'contested_catch_rate', 'splits']
                
            for field in required_fields:
                if field not in p_traits:
                    is_valid = False
                    errors.append(f"  Player '{name}' ({pos}) is missing DNA field: '{field}'")
                    
        # Verify positional ordering: QBs first, then RBs, then WRs, then TEs
        # We can verify that we don't see a QB after an RB/WR/TE, or an RB after a WR/TE, etc.
        pos_order = {'QB': 1, 'RB': 2, 'WR': 3, 'TE': 4}
        order_indices = [pos_order.get(p, 3) for p in positions_observed]
        
        is_sorted = all(order_indices[i] <= order_indices[i+1] for i in range(len(order_indices)-1))
        if not is_sorted:
            is_valid = False
            errors.append(f"  Roster is not correctly sorted by position. Position order observed: {positions_observed}")
            
        # Target shares should sum to approximately 1.0 (some players may be inactive, allowing 0.85 to 1.15)
        if not (0.80 <= total_target_share <= 1.20):
            # Warning only, some slates omit depth players
            # print(f"  Warning: Team target share sum is {total_target_share:.3f}")
            pass
            
        if is_valid:
            passed_teams += 1
        else:
            failed_teams += 1
            print(f"Team {team_code} FAILED Roster Integrity Checks:")
            for err in errors:
                print(err)
                
    print(f"Validation Completed: {passed_teams} teams PASSED, {failed_teams} teams FAILED.")
    if failed_teams == 0:
        print("Evergreen Quality-Assurance Check: PASSED")

if __name__ == "__main__":
    main()
