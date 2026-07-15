# weather_impute.R
# Fill missing weather values with within-game or by-date lookups

library(data.table)

fill_missing_weather_within_game <- function(df) {
  dt <- as.data.table(df)
  dt[, `:=`(temp_filled = temp, wind_filled = wind)]

  dt[is.na(temp) | is.na(wind), c("temp_filled", "wind_filled") := {
    complete_rows <- .SD[!is.na(temp) & !is.na(wind)]
    if (nrow(complete_rows) >= 1) {
      diffs <- abs(complete_rows$game_seconds_remaining - game_seconds_remaining)
      idx <- order(diffs)[1:min(10, .N)]
      list(
        mean(complete_rows$temp[idx], na.rm = TRUE),
        mean(complete_rows$wind[idx], na.rm = TRUE)
      )
    } else list(NA_real_, NA_real_)
  }, by = .(game_date, stadium_id, game_seconds_remaining)]

  dt[is.na(temp), temp := temp_filled]
  dt[is.na(wind), wind := wind_filled]
  dt[, c("temp_filled", "wind_filled") := NULL]

  return(as.data.frame(dt))
}

fill_missing_weather_by_date <- function(df) {
  dt <- as.data.table(df)
  dt[, day_of_year := as.integer(format(as.IDate(game_date), "%j"))]

  weather_complete <- dt[!is.na(temp) & !is.na(wind), .(stadium_id, game_date, temp, wind)]
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
  return(as.data.frame(dt))
}
