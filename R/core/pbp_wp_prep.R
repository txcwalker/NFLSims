# R/core/pbp_wp_prep.R
# One-stop loader + cleaner for WP training PBP
suppressPackageStartupMessages({
  library(nflfastR)
  library(dplyr)
  library(data.table)
})

# ---- PUBLIC ENTRYPOINT -------------------------------------------------------
# Returns a data.frame ready for WP model training.
# - start_year: first season to include (default 1999)
# - include_current: if FALSE (default), stops at last completed season.
#                    if TRUE, includes the current calendar year too.
prep_wp_pbp <- function(start_year = 1999, include_current = FALSE) {
  end_year <- as.integer(format(Sys.Date(), "%Y")) - ifelse(include_current, 0, 1)
  years <- seq.int(start_year, end_year)

  df <- nflfastR::load_pbp(years) |>
    dplyr::select(
      surface, wind, temp, roof,                     # stadium conditions
      season_type, week, posteam_type,               # context
      game_seconds_remaining, score_differential, total,  # game info
      drive, down, yardline_100, ydstogo,            # field position
      posteam_timeouts_remaining, defteam_timeouts_remaining, # timeouts
      spread_line, total_line,                       # Vegas info
      result,                                        # final result
      stadium_id, game_date                          # for imputations
    )

  # Clean + impute (within-game first, then by-date)
  df <- clean_pbp_data(df)
  df <- fill_missing_weather_within_game(df)
  df <- fill_missing_weather_by_date(df)

  return(df)
}



# Impute from other plays in the SAME game (closest in time)
fill_missing_weather_within_game <- function(df) {
  dt <- as.data.table(df)
  dt[, `:=`(temp_filled = temp, wind_filled = wind)]

  dt[is.na(temp) | is.na(wind),
     c("temp_filled", "wind_filled") := {
       complete_rows <- .SD[!is.na(temp) & !is.na(wind)]
       if (nrow(complete_rows) >= 1) {
         diffs <- abs(complete_rows$game_seconds_remaining - game_seconds_remaining)
         idx <- order(diffs)[1:min(10L, .N)]
         list(mean(complete_rows$temp[idx], na.rm = TRUE),
              mean(complete_rows$wind[idx], na.rm = TRUE))
       } else list(NA_real_, NA_real_)
     },
     by = .(game_date, stadium_id, game_seconds_remaining)]

  dt[is.na(temp), temp := temp_filled]
  dt[is.na(wind), wind := wind_filled]
  dt[, c("temp_filled", "wind_filled") := NULL]

  as.data.frame(dt)
}

# Impute remaining missing via stadium + day-of-year averages (±14-day window)
fill_missing_weather_by_date <- function(df) {
  dt <- as.data.table(df)
  dt[, day_of_year := as.integer(format(as.IDate(game_date), "%j"))]

  weather_complete <- dt[!is.na(temp) & !is.na(wind),
                         .(stadium_id, game_date, temp, wind)]
  weather_complete[, ref_day_of_year := as.integer(format(as.IDate(game_date), "%j"))]

  offsets <- -14:14
  weather_expanded <- weather_complete[, .(day_offset = offsets),
                                       by = .(stadium_id, temp, wind, ref_day_of_year)]
  weather_expanded[, day_of_year := (ref_day_of_year + day_offset) %% 366]
  weather_expanded[day_of_year == 0, day_of_year := 366]

  weather_lookup <- weather_expanded[, .(
    avg_temp = mean(temp, na.rm = TRUE),
    avg_wind = mean(wind, na.rm = TRUE)
  ), by = .(stadium_id, day_of_year)]

  dt <- merge(dt, weather_lookup, by = c("stadium_id", "day_of_year"), all.x = TRUE)

  dt[, weather_imputed := is.na(temp) | is.na(wind)]
  dt[is.na(temp), temp := fifelse(!is.na(avg_temp), avg_temp, 72)]
  dt[is.na(wind), wind := fifelse(!is.na(avg_wind), avg_wind, 0)]

  dt[, c("avg_temp", "avg_wind", "day_of_year") := NULL]
  as.data.frame(dt)
}

# ---- INTERNAL HELPERS --------------------------------------------------------
clean_pbp_data <- function(df) {
  dt <- as.data.table(df)

  # Remove ties (result == 0)
  dt <- dt[result != 0]

  # Binary winner from result & posteam_type
  dt[, Winner := fifelse(
    (result > 0 & posteam_type == "home") | (result < 0 & posteam_type == "away"),
    1L, 0L
  )]

  # Dome adjustments
  dt[, temp := fifelse(roof %in% c("dome", "closed"), 72, temp)]
  dt[, wind := fifelse(roof %in% c("dome", "closed"), 0,  wind)]

  # Basic validity filters
  dt <- dt[!is.na(posteam_type)]
  dt <- dt[!is.na(game_seconds_remaining)]
  dt <- dt[!is.na(yardline_100)]

  # First-play edge cases
  dt[is.na(score_differential) & game_seconds_remaining == 3600, score_differential := 0]
  dt[is.na(down) & game_seconds_remaining == 3600, down := 0]
  dt[is.na(down) & ydstogo == 0, down := 0]

  as.data.frame(dt)
}