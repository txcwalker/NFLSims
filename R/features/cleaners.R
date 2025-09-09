# cleaners.R
# Functions to clean raw pbp data

library(data.table)

clean_pbp_data <- function(df) {
  df <- as.data.table(df)

  # Remove games with ties
  df <- df[result != 0]

  # Create binary Winner column
  df[, Winner := ifelse(
    (result > 0 & posteam_type == 'home') | (result < 0 & posteam_type == 'away'),
    1, 0
  )]

  # Adjust weather for domes
  df[, temp := ifelse(roof %in% c("dome", "closed"), 72, temp)]
  df[, wind := ifelse(roof %in% c("dome", "closed"), 0, wind)]

  # Drop non-plays
  df <- df[!is.na(posteam_type)]
  df <- df[!is.na(game_seconds_remaining)]
  df <- df[!is.na(yardline_100)]

  # First play edge cases
  df[is.na(score_differential) & game_seconds_remaining == 3600, score_differential := 0]
  df[is.na(down) & game_seconds_remaining == 3600, down := 0]
  df[is.na(down) & ydstogo == 0, down := 0]

  return(df)
}
