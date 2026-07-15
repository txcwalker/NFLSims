library(nflfastR)
library(dplyr)
library(tidyr)
library(slider)

cat("Loading play-by-play data dynamically for rolling stats...\n")
current_year <- as.integer(format(Sys.Date(), "%Y"))
years <- (current_year - 11):(current_year - 1)
cat("Ingesting seasons:", paste(years, collapse = ", "), "\n")
# 1. Load PBP
pbp <- load_pbp(years)

# 1. Game-level Offensive Aggregation
cat("Aggregating Offensive Game Stats...\n")
off_game_stats <- pbp %>%
  filter(play_type %in% c("run", "pass")) %>%
  group_by(season, week, game_id, posteam) %>%
  summarize(
    off_plays = n(),
    off_pass_attempts = sum(play_type == "pass", na.rm = TRUE),
    off_rush_attempts = sum(play_type == "run", na.rm = TRUE),
    off_pass_rate = mean(play_type == "pass", na.rm = TRUE),
    off_proe = mean(pass_oe, na.rm = TRUE),
    off_shotgun_rate = mean(shotgun == 1, na.rm = TRUE),
    off_yards_per_play = mean(yards_gained, na.rm = TRUE),
    off_turnovers_committed = sum(interception, na.rm = TRUE) + sum(fumble_lost, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Game-level Defensive Aggregation
cat("Aggregating Defensive Game Stats...\n")
def_game_stats <- pbp %>%
  filter(play_type %in% c("run", "pass")) %>%
  group_by(season, week, game_id, defteam) %>%
  summarize(
    def_plays = n(),
    def_pass_yards = sum(ifelse(play_type == "pass", yards_gained, 0), na.rm = TRUE),
    def_rush_yards = sum(ifelse(play_type == "run", yards_gained, 0), na.rm = TRUE),
    def_pass_attempts = sum(play_type == "pass", na.rm = TRUE),
    def_rush_attempts = sum(play_type == "run", na.rm = TRUE),
    def_turnovers_forced = sum(interception, na.rm = TRUE) + sum(fumble_lost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    def_pass_yards_per_dropback = ifelse(def_pass_attempts > 0, def_pass_yards / def_pass_attempts, 0),
    def_rush_yards_per_carry = ifelse(def_rush_attempts > 0, def_rush_yards / def_rush_attempts, 0)
  )

# 3. Rolling Calculations
cat("Computing Rolling Averages (L4 and Season-to-Date)...\n")

# Offensive STD (grouped by team AND season)
off_std <- off_game_stats %>%
  arrange(posteam, season, week) %>%
  group_by(posteam, season) %>%
  mutate(across(where(is.numeric) & -c(week), 
                ~ slider::slide_dbl(.x, mean, .before = Inf, .after = -1, .na_rm = TRUE), 
                .names = "std_{.col}")) %>%
  ungroup()

# Offensive L4 (grouped by team only to cross seasons)
off_l4 <- off_game_stats %>%
  arrange(posteam, season, week) %>%
  group_by(posteam) %>%
  mutate(across(where(is.numeric) & -c(season, week), 
                ~ slider::slide_dbl(.x, mean, .before = 4, .after = -1, .na_rm = TRUE), 
                .names = "l4_{.col}")) %>%
  ungroup()

off_rolling <- off_std %>%
  left_join(off_l4 %>% select(game_id, posteam, starts_with("l4_")), by = c("game_id", "posteam"))

# Defensive STD
def_std <- def_game_stats %>%
  arrange(defteam, season, week) %>%
  group_by(defteam, season) %>%
  mutate(across(where(is.numeric) & -c(week), 
                ~ slider::slide_dbl(.x, mean, .before = Inf, .after = -1, .na_rm = TRUE), 
                .names = "std_{.col}")) %>%
  ungroup()

# Defensive L4
def_l4 <- def_game_stats %>%
  arrange(defteam, season, week) %>%
  group_by(defteam) %>%
  mutate(across(where(is.numeric) & -c(season, week), 
                ~ slider::slide_dbl(.x, mean, .before = 4, .after = -1, .na_rm = TRUE), 
                .names = "l4_{.col}")) %>%
  ungroup()

def_rolling <- def_std %>%
  left_join(def_l4 %>% select(game_id, defteam, starts_with("l4_")), by = c("game_id", "defteam"))

cat("Applying early season carry-over logic...\n")
# If STD is NA (Week 1), coalesce with L4 (which safely pulls from the end of last season!)
# Create a robust loop to coalesce matching pairs
off_cols <- grep("^std_", names(off_rolling), value = TRUE)
for(c in off_cols) {
  l4_c <- sub("^std_", "l4_", c)
  off_rolling[[c]] <- coalesce(off_rolling[[c]], off_rolling[[l4_c]])
}

def_cols <- grep("^std_", names(def_rolling), value = TRUE)
for(c in def_cols) {
  l4_c <- sub("^std_", "l4_", c)
  def_rolling[[c]] <- coalesce(def_rolling[[c]], def_rolling[[l4_c]])
}

# Select only the pre-game features
off_features <- off_rolling %>% select(season, game_id, posteam, starts_with("std_"), starts_with("l4_"))
def_features <- def_rolling %>% select(season, game_id, defteam, starts_with("std_"), starts_with("l4_"))

# 4. Merge back to individual plays
cat("Merging macro features back to play-by-play data...\n")
plays_df <- read.csv("data/model_1_training_data.csv")

plays_merged <- plays_df %>%
  left_join(off_features, by = c("season", "game_id", "posteam")) %>%
  left_join(def_features, by = c("season", "game_id", "defteam"))

write.csv(plays_merged, "data/model_1_training_data_with_macro.csv", row.names = FALSE)
cat("Successfully saved updated dataset to data/model_1_training_data_with_macro.csv\n")
