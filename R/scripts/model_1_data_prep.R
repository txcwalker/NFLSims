library(nflfastR)
library(dplyr)
library(tidyr)
library(nflreadr)

cat("Loading play-by-play data dynamically...\n")
current_year <- as.integer(format(Sys.Date(), "%Y"))
years <- (current_year - 11):(current_year - 1)
cat("Ingesting seasons:", paste(years, collapse = ", "), "\n")
# 1. Load PBP
pbp <- load_pbp(years)

cat("Determining active QB and RB starters for each game...\n")
game_qb <- pbp %>%
  filter(!is.na(passer_id), !is.na(posteam)) %>%
  group_by(game_id, posteam, passer_id, passer) %>%
  summarize(pass_atts = n(), .groups = "drop") %>%
  arrange(game_id, posteam, desc(pass_atts)) %>%
  group_by(game_id, posteam) %>%
  slice(1) %>%
  select(game_id, posteam, active_qb = passer)

game_rb <- pbp %>%
  filter(!is.na(rusher_id), !is.na(posteam)) %>%
  group_by(game_id, posteam, rusher_id, rusher) %>%
  summarize(rush_atts = n(), .groups = "drop") %>%
  arrange(game_id, posteam, desc(rush_atts)) %>%
  group_by(game_id, posteam) %>%
  slice(1) %>%
  select(game_id, posteam, active_rb = rusher)

cat("Filtering data...\n")
plays <- pbp %>%
  # Determine posteam coach
  mutate(
    posteam_coach = ifelse(posteam == home_team, home_coach, away_coach),
    defteam_coach = ifelse(defteam == home_team, home_coach, away_coach)
  )

# 2. Filter to regular plays (run, pass, field_goal, punt)
plays <- plays %>%
  filter(play_type %in% c("run", "pass", "field_goal", "punt")) %>%
  filter(!is.na(down), !is.na(ydstogo))

cat("Merging active player identities...\n")
plays <- plays %>%
  left_join(game_qb, by = c("game_id", "posteam")) %>%
  left_join(game_rb, by = c("game_id", "posteam"))

cat("Engineering Game State and Momentum Features...\n")
# 3. Add Momentum Features
plays <- plays %>%
  arrange(game_id, play_id) %>%
  group_by(game_id, drive) %>%
  mutate(
    drive_play_count = row_number(),
    # previous play stats
    previous_play_type = lag(play_type, default = "first_play"),
    previous_play_yards = coalesce(lag(yards_gained), 0),
    # cumulative yards before this play
    drive_yards_gained_so_far = cumsum(coalesce(lag(yards_gained), 0)),
    
    # Streaks
    run_streak_id = cumsum(play_type != "run"),
    pass_streak_id = cumsum(play_type != "pass")
  ) %>%
  group_by(game_id, drive, run_streak_id) %>%
  mutate(consecutive_runs = row_number() - 1) %>%
  group_by(game_id, drive, pass_streak_id) %>%
  mutate(consecutive_passes = row_number() - 1) %>%
  ungroup() %>%
  # Shift the streak counts to represent the state *before* the snap
  arrange(game_id, play_id) %>%
  group_by(game_id, drive) %>%
  mutate(
    consecutive_runs_before_snap = lag(consecutive_runs, default = 0),
    consecutive_passes_before_snap = lag(consecutive_passes, default = 0)
  ) %>%
  ungroup()

# 4. Total points & differential & turnovers
plays <- plays %>%
  arrange(game_id, play_id) %>%
  group_by(game_id, posteam) %>%
  mutate(
    total_scored_points = total_home_score + total_away_score,
    score_differential = score_differential, # already exists in nflfastR
    posteam_turnovers_so_far = cumsum(coalesce(lag(interception), 0) + coalesce(lag(fumble_lost), 0))
  ) %>%
  ungroup()

cat("Saving dataset to data/model_1_training_data.csv...\n")
# Save to CSV
dir.create("data", showWarnings = FALSE)

plays_clean <- plays %>% 
  select(
    game_id, play_id, season, posteam, defteam, posteam_coach, defteam_coach,
    active_qb, active_rb, cpoe,
    play_type, down, ydstogo, yardline_100, qtr, half_seconds_remaining, game_seconds_remaining,
    posteam_timeouts_remaining, defteam_timeouts_remaining, roof, wind,
    wp, ep, pass_oe, score_differential, total_scored_points, posteam_turnovers_so_far,
    drive, drive_play_count, drive_yards_gained_so_far, previous_play_type, previous_play_yards,
    consecutive_runs_before_snap, consecutive_passes_before_snap,
    yards_gained, spread_line, posteam_type
  )

write.csv(plays_clean, "data/model_1_training_data.csv", row.names = FALSE)
cat("Data extraction complete! Row count:", nrow(plays_clean), "\n")
