# R/bots/fetch_upcoming_schedule.R
# -----------------------------------------------------------------------------
# Pull the upcoming week's NFL schedule (Thu→Mon window starting next Thu),
# write CSV + JSON for downstream jobs.
# Exits 0 with an empty artifact if out-of-season, so cron can run year-round.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(jsonlite); library(lubridate); library(stringr)
  library(nflreadr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.atomic(a) && all(is.na(a)))) b else a

# --- Helpers ------------------------------------------------------------------

first_thursday_sept <- function(year) {
  d <- as.Date(paste0(year, "-09-01"))
  d + ((5 - wday(d, week_start = 1)) %% 7)  # Thursday = 5 when week_start=1
}

second_sunday_feb <- function(year_plus_one) {
  d <- as.Date(paste0(year_plus_one, "-02-01"))
  first_sun <- d + ((7 - wday(d, week_start = 1)) %% 7)  # Sunday = 7
  first_sun + 7
}

is_in_nfl_season <- function(today = Sys.Date()) {
  yr <- year(today)
  # Season spans: from 1st Thu Sep (yr) through 2nd Sun Feb (yr+1)
  start <- first_thursday_sept(yr)
  end   <- second_sunday_feb(yr + 1)
  today >= start & today <= end
}

next_thursday <- function(d = Sys.Date()) {
  d + ((5 - wday(d, week_start = 1)) %% 7)
}

# safe POSIX parsing
as_utc <- function(x){
  if (inherits(x, "POSIXct")) {
    with_tz(x, "UTC")
  } else if (inherits(x, "Date")) {
    as_datetime(x, tz = "UTC")
  } else {
    suppressWarnings(ymd_hms(x, tz = "UTC"))
  }
}

# --- Main ---------------------------------------------------------------------

main <- function() {
  # where to drop artifacts so GH Actions can pick them up
  out_dir  <- Sys.getenv("SCHEDULE_OUT_DIR", unset = "artifacts")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  today <- Sys.Date()

  if (!is_in_nfl_season(today)) {
    message("[schedule] Out of NFL season. Writing empty artifacts and exiting.")
    empty <- tibble(
      season=integer(), week=integer(), season_type=character(),
      game_id=character(), away_team=character(), home_team=character(),
      start_time_utc=as.POSIXct(character()), start_time_et=character()
    )
    write_csv(empty, file.path(out_dir, "upcoming_week_schedule.csv"))
    write_json(list(game_ids = list()), file.path(out_dir, "upcoming_week_game_ids.json"), auto_unbox = TRUE, pretty = TRUE)
    quit(status = 0)
  }

  # Window: upcoming Thursday 00:00:00 UTC through next Tuesday 11:59:59 UTC
  # (covers Thu night → MNF and a bit of cushion)
  start_utc <- as_datetime(next_thursday(today), tz = "UTC")
  end_utc   <- start_utc + days(5) + hours(36)   # Thu→Tue 12:00 UTC

  # Seasons to load (handle Jan/Feb which belong to prior season)
  seasons <- sort(unique(c(year(start_utc), year(end_utc), year(today), year(today)+1)))

  sched <- nflreadr::load_schedules(seasons) %>%
    # Normalize common columns across nflreadr versions
    rename_with(~"start_time_utc", any_of(c("gameday", "game_datetime", "start_time_utc"))) %>%
    mutate(
      # If we got a Date, convert to midnight UTC; some sources also have "gametime" ET
      start_time_utc = as_utc(start_time_utc),
      start_time_et  = with_tz(start_time_utc, "America/New_York") %>% format("%Y-%m-%d %H:%M:%S"),
      away_team = .data$away_team %||% .data$away %||% NA_character_,
      home_team = .data$home_team %||% .data$home %||% NA_character_,
      season_type = .data$season_type %||% .data$game_type %||% NA_character_,
      week = as.integer(.data$week),
      season = as.integer(.data$season),
      game_id = .data$game_id %||% .data$g_id %||% .data$game_id
    ) %>%
    select(season, week, season_type, game_id, away_team, home_team, start_time_utc, start_time_et)

  upcoming <- sched %>%
    filter(!is.na(start_time_utc), start_time_utc >= start_utc, start_time_utc <= end_utc) %>%
    arrange(start_time_utc, home_team)

  # Write artifacts
  csv_path  <- file.path(out_dir, "upcoming_week_schedule.csv")
  json_path <- file.path(out_dir, "upcoming_week_game_ids.json")

  write_csv(upcoming, csv_path)
  write_json(list(game_ids = upcoming$game_id), json_path, auto_unbox = TRUE, pretty = TRUE)

  message(glue::glue("[schedule] Wrote {nrow(upcoming)} games ",
                     "({format(start_utc, '%Y-%m-%d')} → {format(end_utc, '%Y-%m-%d')})"))
  message(glue::glue("[schedule] CSV:  {csv_path}"))
  message(glue::glue("[schedule] JSON: {json_path}"))

  # For GH Actions composite or step outputs (new-style)
  gh_out <- Sys.getenv("GITHUB_OUTPUT", unset = NA_character_)
  if (!is.na(gh_out) && file.exists(dirname(gh_out))) {
    cat(sprintf("csv_path=%s\njson_path=%s\ncount=%d\n",
                csv_path, json_path, nrow(upcoming)),
        file = gh_out, append = TRUE)
  }
}

main()
