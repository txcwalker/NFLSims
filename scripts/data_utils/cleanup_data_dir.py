import os
import shutil

def main():
    print("=== STARTING DATA DIRECTORY CLEANUP ===")
    data_dir = "data"
    legacy_dir = os.path.join(data_dir, "legacy")
    os.makedirs(legacy_dir, exist_ok=True)
    
    # List of obsolete/archived files to move to data/legacy/
    legacy_files = [
        "chaos_training_data.csv",
        "chaos_training_data_with_macro.csv",
        "coach_aggression_map.json",
        "coach_proe_map.json",
        "team_to_dc_2025.json",
        "elite_10yr_play_selection.csv",
        "group_b_history_vector_training.csv",
        "group_b_honest_training.csv",
        "hardened_pass_training_master_v2.csv",
        "hardened_pass_training_master_v2_2.csv",
        "hardened_pass_training_master_v2_3.csv",
        "hardened_run_training_master.csv",
        "hardened_run_training_master_v2.csv",
        "historical_secondary_tiers.csv",
        "model_1_training_data.csv",
        "model_1_training_data_with_macro.csv",
        "personnel_talent_atlas.json",
        "rolling_10yr_play_selection.csv",
        "starting_position_pool_v2.csv",
        "temporal_aggression_atlas.json",
        "temporal_cpoe_atlas.json",
        "temporal_strategic_atlas.json",
        "timing_eda_results.json",
        "ultimate_10yr_play_selection.csv",
        "v2_1_script_training_set.csv",
        "v2_2_air_yards_training.csv",
        "v2_2_scaling_params.json"
    ]
    
    moved_count = 0
    missing_count = 0
    
    for filename in legacy_files:
        src_path = os.path.join(data_dir, filename)
        dest_path = os.path.join(legacy_dir, filename)
        
        if os.path.exists(src_path):
            shutil.move(src_path, dest_path)
            print(f"  Moved: {filename} -> data/legacy/")
            moved_count += 1
        else:
            # print(f"  Not found: {filename}")
            missing_count += 1
            
    print("=== DIRECTORY CLEANUP COMPLETE ===")
    print(f"  Moved files: {moved_count}")
    print(f"  Files already archived or not found: {missing_count}")

if __name__ == "__main__":
    main()
