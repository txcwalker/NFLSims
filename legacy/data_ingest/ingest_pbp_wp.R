# ingest_pbp_wp.R
# Load play-by-play data for win probability model

library(nflfastR)
library(dplyr)

years <- 1999:(as.integer(format(Sys.Date(), "%Y")) - 1)


raw_model_pbp_data <- load_pbp(years) %>%
  select(
    surface, wind, temp, roof,           # stadium conditions
    season_type, week, posteam_type,     # context
    game_seconds_remaining, score_differential, total, # game info
    drive, down, yardline_100, ydstogo,  # field position
    posteam_timeouts_remaining, defteam_timeouts_remaining, # timeouts
    spread_line, total_line,             # Vegas info
    result,                              # final result
    stadium_id, game_date                # for imputations
  )
