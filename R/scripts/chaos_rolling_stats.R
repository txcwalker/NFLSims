library(nflfastR)
library(dplyr)
library(tidyr)
library(slider)

cat("--- Computing Chaos-Specific Rolling Stats ---\n")
current_year <- as.integer(format(Sys.Date(), "%Y"))
years <- (current_year - 11):(current_year - 1)

pbp <- load_pbp(years)

# 1. Game-level Chaos Aggregation
cat("Aggregating Chaos Stats per Game...\n")
chaos_game_stats <- pbp %>%
  filter(play_type %in% c("run", "pass")) %>%
  group_by(season, week, game_id, posteam) %>%
  summarize(
    off_plays = n(),
    off_sacks_allowed = sum(sack, na.rm = TRUE),
    off_interceptions = sum(interception, na.rm = TRUE),
    off_fumbles = sum(fumble, na.rm = TRUE),
    off_penalties = sum(penalty == 1 & penalty_team == posteam, na.rm = TRUE),
    .groups = "drop"
  )

def_chaos_stats <- pbp %>%
  filter(play_type %in% c("run", "pass")) %>%
  group_by(season, week, game_id, defteam) %>%
  summarize(
    def_plays = n(),
    def_sacks = sum(sack, na.rm = TRUE),
    def_interceptions = sum(interception, na.rm = TRUE),
    def_fumbles_forced = sum(fumble, na.rm = TRUE),
    def_penalties_forced = sum(penalty == 1 & penalty_team == posteam, na.rm = TRUE), # posteam here is the offense they faced
    .groups = "drop"
  )

# 2. Rolling Calculations
compute_rolling <- function(df, team_col) {
  df %>%
    arrange(!!sym(team_col), season, week) %>%
    group_by(!!sym(team_col)) %>%
    mutate(across(where(is.numeric) & -c(season, week), 
                  ~ slider::slide_dbl(.x, mean, .before = 4, .after = -1, .na_rm = TRUE), 
                  .names = "l4_{.col}")) %>%
    ungroup()
}

off_rolling <- compute_rolling(chaos_game_stats, "posteam")
def_rolling <- compute_rolling(def_chaos_stats, "defteam")

# 3. Merge with Chaos Training Data
cat("Merging Rolling Chaos Stats with Training Data...\n")
chaos_df <- read.csv("data/chaos_training_data.csv")

final_chaos_df <- chaos_df %>%
  left_join(off_rolling %>% select(game_id, posteam, starts_with("l4_")), by = c("game_id", "posteam")) %>%
  left_join(def_rolling %>% select(game_id, defteam, starts_with("l4_")), by = c("game_id", "defteam"))

write.csv(final_chaos_df, "data/chaos_training_data_with_macro.csv", row.names = FALSE)
cat("Successfully saved Chaos Training Data with Macro Features!\n")
