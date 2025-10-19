# R/bots/run_live_today.R
# ------------------------------------------------------------------------------
# Run the live loop across today's kickoff window using ESPN SCOREBOARD times,
# not nflreadr. If no games today, exit immediately.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(lubridate); library(glue); library(jsonlite); library(purrr)
})
source("R/bots/run_live_loop.R")   # run_live()

`%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.atomic(a) && all(is.na(a)))) b else a

run_live_today <- function(poll_seconds = 20,
                           buffer_before_mins = 20,
                           max_game_hours = 4.5,
                           buffer_after_mins = 30,
                           tz = "America/Chicago") {
  Sys.setenv(TZ = tz)

  # 1) Pull ESPN scoreboard and extract today's kickoffs
  sb <- jsonlite::fromJSON("https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard", flatten = FALSE)
  evs <- sb$events %||% list()
  if (!length(evs)) { message("[LiveToday] No events on scoreboard. Exiting."); return(invisible(0L)) }

  # Each event has an ISO UTC `date` field
  kicks_utc <- purrr::map_chr(evs, ~ .x$date %||% NA_character_)
  kicks     <- lubridate::ymd_hms(kicks_utc, quiet = TRUE, tz = "UTC")
  kicks_loc <- with_tz(kicks, tz)
  today     <- as_date(Sys.time(), tz = tz)

  day_mask <- as_date(kicks_loc, tz) == today
  if (!any(day_mask, na.rm = TRUE)) {
    message(glue("[LiveToday] No games today ({today}). Exiting."))
    return(invisible(0L))
  }

  first_kick <- min(kicks_loc[day_mask], na.rm = TRUE)
  last_kick  <- max(kicks_loc[day_mask], na.rm = TRUE)

  start_at <- first_kick - minutes(buffer_before_mins)
  end_at   <- last_kick + hours(max_game_hours) + minutes(buffer_after_mins)

  now <- Sys.time()
  if (now < start_at) {
    wait <- as.numeric(difftime(start_at, now, units = "secs"))
    message(glue("[LiveToday] Sleeping {round(wait/60)} min until first kickoff window ({format(start_at, '%H:%M %Z')})."))
    Sys.sleep(wait)
  } else {
    message(glue("[LiveToday] Window already open (start_at={format(start_at, '%H:%M')}, now={format(now, '%H:%M')})."))
  }

  run_live(poll_seconds = poll_seconds, end_at = end_at)
  invisible(0L)
}
