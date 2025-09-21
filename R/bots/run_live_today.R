# R/bots/run_live_today.R
suppressPackageStartupMessages({
  library(dplyr); library(lubridate); library(glue)
  library(nflreadr); library(tibble)
})

# Load deps
source("R/bots/run_live_loop.R")   # run_live()
# (run_live_loop.R already sources post_targets.R, posting_policy.R, run_fd_live_once.R)

`%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.atomic(a) && all(is.na(a)))) b else a

# Compute today's kickoff window and run the live loop inside it.
# Behavior:
#  - If no games today: exit immediately (0).
#  - If now < first_kickoff - buffer: sleep until then.
#  - Run until end_at = last_kickoff + max_game_hours (+ buffer).
run_live_today <- function(poll_seconds = 20,
                           buffer_before_mins = 20,
                           max_game_hours = 4.5,
                           buffer_after_mins = 30,
                           tz = "America/Chicago") {
  Sys.setenv(TZ = tz)

  season <- nflreadr::most_recent_season()
  today  <- as_date(Sys.time(), tz = tz)

  sched <- nflreadr::load_schedules(seasons = season) %>%
    mutate(
      # nflreadr columns can vary; prefer 'gameday' (Date) and 'gametime' (string, local ET).
      gameday = as_date(.data$gameday),
      # Try multiple fields for kickoff; fall back to noon if unknown.
      kickoff_str = coalesce(.data$gametime, .data$kickoff, NA_character_)
    ) %>%
    filter(gameday == today)

  if (!nrow(sched)) {
    message(glue("[LiveToday] No games on {today}. Exiting."))
    return(invisible(0L))
  }

  # Parse kickoff times. If any fail, default to 12:00 local.
  parse_local <- function(x) {
    if (is.na(x) || !nzchar(x)) return(make_datetime(year(today), month(today), day(today), 12, 0, tz = tz))
    # Common forms: "12:00", "12:00:00", "12:00 PM", "7:15 PM"
    suppressWarnings({
      t1 <- try(as_datetime(paste(today, x), tz = tz), silent = TRUE)
      if (inherits(t1, "try-error") || is.na(t1)) {
        # Try lubridate hms clock-time
        clk <- suppressWarnings(lubridate::hms(x))
        if (is.na(clk)) {
          make_datetime(year(today), month(today), day(today), 12, 0, tz = tz)
        } else {
          make_datetime(year(today), month(today), day(today), hour(clk), minute(clk), second(clk), tz = tz)
        }
      } else t1
    })
  }

  kicks <- vapply(sched$kickoff_str, parse_local, as.POSIXct(Sys.time(), tz = tz))
  first_kick <- min(kicks, na.rm = TRUE)
  last_kick  <- max(kicks, na.rm = TRUE)

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

  # Run the live loop; it will exit automatically at end_at
  run_live(poll_seconds = poll_seconds, end_at = end_at)

  invisible(0L)
}
