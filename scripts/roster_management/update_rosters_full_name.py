import nfl_data_py as nfl
import json
import os
import glob
import pandas as pd
import numpy as np

def clean_name(name):
    if not isinstance(name, str):
        return ""
    name = name.strip()
    suffixes = [" Jr.", " Sr.", " III", " II", " IV", " V"]
    for s in suffixes:
        if name.endswith(s):
            name = name[:-len(s)]
    name = name.replace(".", "")
    return name

def get_short_name(full_name):
    parts = full_name.split(' ')
    if len(parts) < 2:
        return full_name
    first_initial = parts[0][0]
    last_name = parts[-1]
    return f"{first_initial}.{last_name}"

def main():
    print("=== STARTING ROSTER ENRICHMENT & SORTING ===")
    dna_dir = "data/dna"
    rosters_dir = "data/current_rosters"
    
    # 1. LOAD NEW POSITION-SPECIFIC DNA FILES
    try:
        with open(os.path.join(dna_dir, "qb_dna.json"), "r") as f:
            qb_dna = json.load(f)
        with open(os.path.join(dna_dir, "rb_dna.json"), "r") as f:
            rb_dna = json.load(f)
        with open(os.path.join(dna_dir, "wr_dna.json"), "r") as f:
            wr_dna = json.load(f)
        with open(os.path.join(dna_dir, "te_dna.json"), "r") as f:
            te_dna = json.load(f)
    except FileNotFoundError as e:
        print(f"Error loading DNA JSON files: {e}. Please build the DNA files first.")
        return
        
    # Combine skill players lookup
    skill_dna = {}
    skill_dna.update(rb_dna)
    skill_dna.update(wr_dna)
    skill_dna.update(te_dna)
    
    # 2. LOAD 2025 ROSTERS FOR NAME MAPPING
    print("Loading 2025 rosters from nfl_data_py...")
    rosters_2025 = nfl.import_seasonal_rosters([2025])
    
    # Build mapping for each team: short_name -> full_name_clean
    # We will use this to resolve roster entries like "K.Murray" -> "Kyler Murray"
    team_mappings = {}
    
    # Add manual overrides for rookie players or players with naming variances
    manual_overrides = {
        "ARI": {
            "M.Harrison": "Marvin Harrison",
            "Mi.Wilson": "Michael Wilson",
        },
        "KC": {
            "P.Mahomes": "Patrick Mahomes",
            "M.Brown": "Marquise Brown",
        },
        "PHI": {
            "A.Brown": "AJ Brown",
        }
    }
    
    # Team abbreviation conversion mapping (PBP vs. Rosters)
    team_abbr_map = {
        "OAK": "LV", "SD": "LAC", "LAR": "LA", "ARI": "ARI", "ATL": "ATL", "BAL": "BAL", "BUF": "BUF",
        "CAR": "CAR", "CHI": "CHI", "CIN": "CIN", "CLE": "CLE", "DAL": "DAL", "DEN": "DEN", "DET": "DET",
        "GB": "GB", "HOU": "HOU", "IND": "IND", "JAX": "JAX", "KC": "KC", "MIA": "MIA", "MIN": "MIN",
        "NE": "NE", "NO": "NO", "NYG": "NYG", "NYJ": "NYJ", "PHI": "PHI", "PIT": "PIT", "SEA": "SEA",
        "SF": "SF", "TB": "TB", "TEN": "TEN", "WAS": "WAS"
    }

    for idx, row in rosters_2025.iterrows():
        raw_team = row['team']
        team = team_abbr_map.get(raw_team, raw_team)
        if team not in team_mappings:
            team_mappings[team] = {}
            
        full_name = row['player_name']
        if pd.isna(full_name):
            continue
            
        clean_full = clean_name(full_name)
        short_name = get_short_name(clean_full)
        
        # Add to mapping
        team_mappings[team][short_name] = clean_full
        
        # Also register first-initial last-name variant
        # (e.g. M.Harrison -> Marvin Harrison)
        first_init = row['first_name'][0] if not pd.isna(row['first_name']) else ""
        last = row['last_name'] if not pd.isna(row['last_name']) else ""
        if first_init and last:
            team_mappings[team][f"{first_init}.{last}"] = clean_full

    # Apply manual overrides
    for team, overrides in manual_overrides.items():
        if team not in team_mappings:
            team_mappings[team] = {}
        for short, long in overrides.items():
            team_mappings[team][short] = long

    # 3. PROCESS TEAM ROSTER FILES
    roster_files = glob.glob(os.path.join(rosters_dir, "*_traits_2025.json"))
    print(f"Processing {len(roster_files)} team roster files...")
    
    for r_file in roster_files:
        team_code = os.path.basename(r_file).split("_")[0] # e.g. "ARI"
        with open(r_file, "r") as f:
            roster_data = json.load(f)
            
        traits = roster_data.get("traits", {})
        team_map = team_mappings.get(team_code, {})
        
        # We will build a new flat traits dict with full names, fully enriched
        enriched_traits = {}
        
        # Separate players by position to sort them
        qbs = []
        rbs = []
        wrs = []
        tes = []
        
        for short_name, stats in traits.items():
            # Resolve full display name
            full_name = team_map.get(short_name)
            if full_name is None:
                # Deduce name from key
                full_name = short_name
                # Look up in DNA files to find best match
                # (If exact short name matches DNA key or similar)
                if short_name in qb_dna:
                    full_name = short_name
                elif short_name in skill_dna:
                    full_name = short_name
                    
            pos = stats.get("pos", "WR/TE")
            
            # Map position to exact RB/WR/TE if WR/TE
            if pos == "WR/TE":
                # Look up in DNA
                p_dna = skill_dna.get(full_name, {})
                pos = p_dna.get("position", "WR")
                
            # Keep carry and target shares from current roster (which reflect active roles)
            target_share = stats.get("target_share", 0.0)
            carry_share = stats.get("carry_share", 0.0)
            ypc = stats.get("ypc", 4.2)
            
            # Look up career DNA attributes
            enriched_player = {
                "pos": pos,
                "target_share": target_share,
                "carry_share": carry_share,
                "status": "active"
            }
            
            # Enrich based on position
            if pos == "QB":
                # Get QB DNA
                p_dna = qb_dna.get(full_name, {})
                enriched_player.update({
                    "cpoe": p_dna.get("cpoe", 0.0),
                    "avg_air_yards_per_att": p_dna.get("avg_air_yards_per_att", 8.0),
                    "deep_ball_rate": p_dna.get("deep_ball_rate", 0.12),
                    "scramble_rate": p_dna.get("scramble_rate", 0.05),
                    "sack_rate": p_dna.get("sack_rate", 0.06),
                    "play_action_rate": p_dna.get("play_action_rate", 0.20),
                    "under_pressure_cpoe": p_dna.get("under_pressure_cpoe", -2.5),
                    "pressure_rate": p_dna.get("pressure_rate", 0.20),
                    "avg_time_to_throw_sec": p_dna.get("avg_time_to_throw_sec", 2.7),
                    "ngs_aggressiveness_index": p_dna.get("ngs_aggressiveness_index", 15.0),
                    "splits": p_dna.get("splits", {
                        "primary": {"cpoe": 0.0, "sack_rate": 0.06, "play_action_rate": 0.20},
                        "redzone": {"cpoe": 0.0, "sack_rate": 0.06, "play_action_rate": 0.20},
                        "goalline": {"cpoe": 0.0, "sack_rate": 0.06, "play_action_rate": 0.20}
                    })
                })
                qbs.append((full_name, enriched_player))
                
            elif pos == "RB":
                p_dna = skill_dna.get(full_name, {})
                enriched_player.update({
                    "adot": p_dna.get("avg_target_depth_yds", 1.0),
                    "yac_per_rec": p_dna.get("yac_per_reception", 6.5),
                    "elusiveness": p_dna.get("elusiveness", 0.0),
                    "broken_tackle_rate": p_dna.get("broken_tackle_rate", 0.15),
                    "ypc": ypc,
                    "efficiency": p_dna.get("efficiency", 3.8),
                    "percent_attempts_gte_eight_defenders": p_dna.get("percent_attempts_gte_eight_defenders", 0.22),
                    "avg_time_to_los": p_dna.get("avg_time_to_los", 2.75),
                    "rush_yards_over_expected_per_att": p_dna.get("rush_yards_over_expected_per_att", 0.0),
                    "rush_pct_over_expected": p_dna.get("rush_pct_over_expected", 0.35),
                    "catch_rate": p_dna.get("catch_rate", 0.75),
                    "top_speed_mph": p_dna.get("top_speed_mph", 20.3),
                    "contested_catch_rate": p_dna.get("contested_catch_rate", 0.38),
                    "splits": p_dna.get("splits", {
                        "primary": {"target_share": target_share, "carry_share": carry_share, "catch_rate": 0.75, "yac_per_reception": 6.5},
                        "redzone": {"target_share": target_share, "carry_share": carry_share, "catch_rate": 0.75, "yac_per_reception": 6.5},
                        "goalline": {"target_share": target_share, "carry_share": carry_share, "catch_rate": 0.75, "yac_per_reception": 6.5}
                    })
                })
                rbs.append((full_name, enriched_player))
                
            elif pos == "WR":
                p_dna = skill_dna.get(full_name, {})
                enriched_player.update({
                    "adot": p_dna.get("avg_target_depth_yds", 11.5),
                    "yac_per_rec": p_dna.get("yac_per_reception", 4.2),
                    "elusiveness": p_dna.get("elusiveness", 0.0),
                    "broken_tackle_rate": p_dna.get("broken_tackle_rate", 0.15),
                    "ypc": ypc,
                    "avg_separation_yds": p_dna.get("avg_separation_yds", 2.8),
                    "avg_cushion_yds": p_dna.get("avg_cushion_yds", 5.8),
                    "catch_rate": p_dna.get("catch_rate", 0.62),
                    "route_profile": p_dna.get("route_profile", "intermediate"),
                    "top_speed_mph": p_dna.get("top_speed_mph", 21.0),
                    "deep_target_rate": p_dna.get("deep_target_rate", 0.15),
                    "contested_catch_rate": p_dna.get("contested_catch_rate", 0.48),
                    "splits": p_dna.get("splits", {
                        "primary": {"target_share": target_share, "carry_share": carry_share, "catch_rate": 0.62, "yac_per_reception": 4.2},
                        "redzone": {"target_share": target_share, "carry_share": carry_share, "catch_rate": 0.62, "yac_per_reception": 4.2},
                        "goalline": {"target_share": target_share, "carry_share": carry_share, "catch_rate": 0.62, "yac_per_reception": 4.2}
                    })
                })
                wrs.append((full_name, enriched_player))
                
            elif pos == "TE":
                p_dna = skill_dna.get(full_name, {})
                enriched_player.update({
                    "adot": p_dna.get("avg_target_depth_yds", 7.5),
                    "yac_per_rec": p_dna.get("yac_per_reception", 4.5),
                    "elusiveness": p_dna.get("elusiveness", 0.0),
                    "broken_tackle_rate": p_dna.get("broken_tackle_rate", 0.15),
                    "ypc": ypc,
                    "avg_separation_yds": p_dna.get("avg_separation_yds", 3.0),
                    "avg_cushion_yds": p_dna.get("avg_cushion_yds", 5.6),
                    "catch_rate": p_dna.get("catch_rate", 0.68),
                    "route_profile": p_dna.get("route_profile", "short"),
                    "top_speed_mph": p_dna.get("top_speed_mph", 19.8),
                    "deep_target_rate": p_dna.get("deep_target_rate", 0.06),
                    "contested_catch_rate": p_dna.get("contested_catch_rate", 0.52),
                    "splits": p_dna.get("splits", {
                        "primary": {"target_share": target_share, "carry_share": carry_share, "catch_rate": 0.68, "yac_per_reception": 4.5},
                        "redzone": {"target_share": target_share, "carry_share": carry_share, "catch_rate": 0.68, "yac_per_reception": 4.5},
                        "goalline": {"target_share": target_share, "carry_share": carry_share, "catch_rate": 0.68, "yac_per_reception": 4.5}
                    })
                })
                tes.append((full_name, enriched_player))
                
        # 4. SORT BY POSITION AND SHARE
        # QBs sorted by carry share (or just keep order)
        qbs.sort(key=lambda x: x[1]['carry_share'], reverse=True)
        # RBs sorted by carry share descending
        rbs.sort(key=lambda x: x[1]['carry_share'], reverse=True)
        # WRs sorted by target share descending
        wrs.sort(key=lambda x: x[1]['target_share'], reverse=True)
        # TEs sorted by target share descending
        tes.sort(key=lambda x: x[1]['target_share'], reverse=True)
        
        # Combine back into a single flat traits dictionary in the sorted order
        for p, d in qbs + rbs + wrs + tes:
            enriched_traits[p] = d
            
        # Update roster file
        roster_data["traits"] = enriched_traits
        
        with open(r_file, "w") as f:
            json.dump(roster_data, f, indent=4)
            
    print("=== ROSTERS SUCCESSFULLY ENRICHED AND SORTED ===")

if __name__ == "__main__":
    main()
