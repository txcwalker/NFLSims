library(nflfastR)
library(dplyr)
library(tidyr)
library(nflreadr)

cat("--- Preparing Chaos Model Training Data ---\n")
current_year <- as.integer(format(Sys.Date(), "%Y"))
years <- (current_year - 11):(current_year - 1)

# 1. Load PBP with specific columns for chaos modeling
pbp <- load_pbp(years)

# Determine posteam coach (re-using logic from model_1)
pbp <- pbp %>%
  mutate(
    posteam_coach = ifelse(posteam == home_team, home_coach, away_coach),
    defteam_coach = ifelse(defteam == home_team, home_coach, away_coach)
  )

cat("Defining Chaos Targets...\n")

# 2. Define Chaos Targets based on your logic
chaos_data <- pbp %>%
  filter(!is.na(down), !is.na(ydstogo), !is.na(posteam)) %>%
  # Include no_play to capture pre-snap penalties
  filter(play_type %in% c("run", "pass", "punt", "field_goal", "extra_point", "no_play")) %>%
  mutate(
    # Pass Chaos
    is_sack = ifelse(sack == 1 | (fumble == 1 & play_type == "pass" & yards_gained < 0), 1, 0),
    is_interception = interception,
    
    # Run Chaos
    is_tfl = ifelse(play_type == "run" & yards_gained < 0 & fumble == 0 & penalty == 0, 1, 0),
    is_fumble = fumble,
    
    # Penalties
    is_off_penalty = ifelse(penalty == 1 & penalty_team == posteam, 1, 0),
    is_def_penalty = ifelse(penalty == 1 & penalty_team == defteam, 1, 0),
    
    # Create the Multi-class Labels
    pass_chaos_label = case_when(
        is_sack == 1 ~ "sack",
        is_interception == 1 ~ "interception",
        is_off_penalty == 1 ~ "off_penalty",
        is_def_penalty == 1 ~ "def_penalty",
        TRUE ~ "clean"
    ),
    
    run_chaos_label = case_when(
        is_fumble == 1 ~ "fumble",
        is_tfl == 1 ~ "tfl",
        is_off_penalty == 1 ~ "off_penalty",
        is_def_penalty == 1 ~ "def_penalty",
        TRUE ~ "clean"
    )
  )

# 3. Clean and Select Columns
chaos_clean <- chaos_data %>%
  select(
    game_id, play_id, season, week, posteam, defteam, posteam_coach, defteam_coach,
    play_type, down, ydstogo, yardline_100, qtr, game_seconds_remaining,
    wp, ep, pass_oe, score_differential, posteam_type, roof,
    pass_chaos_label, run_chaos_label,
    yards_gained, incomplete_pass, touchdown,
    pass_location, run_location, air_yards, yards_after_catch, pass_length,
    rusher_player_name, receiver_player_name,
    penalty, penalty_type, penalty_yards, penalty_team,
    fumble_lost, series_result
  )

cat("Saving Chaos Data to data/chaos_training_data.csv...\n")
dir.create("data", showWarnings = FALSE)
write.csv(chaos_clean, "data/chaos_training_data.csv", row.names = FALSE)
cat("Done! Total rows:", nrow(chaos_clean), "\n")
