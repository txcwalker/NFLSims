# R/bots/fetch_live_plays_nflreadr.R
# -----------------------------------------------------------------------------
# Free fallback "live enough" fetcher using nflreadr mirrors.
# It clears the cache each poll and returns only 4th downs for TODAY's games.
# Emits the columns expected by run_fd_live_once() downstream.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(lubridate); library(nflreadr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.atomic(a) && all(is.na(a)))) b else a

# Helper: today in a chosen tz (default America/Chicago)
.today_local <- function(tz = Sys.getenv("FD_TZ", "America/Chicago")){
  as_date(with_tz(Sys.time(), tzone = tz))
}

# Helper: best-effort week detection for today
.current_week_games <- function(season, tz = Sys.getenv("FD_TZ", "America/Chicago")){
  sched <- nflreadr::load_schedules(seasons = season) %>%
    mutate(gameday = as_date(.data$gameday))
  today <- .today_local(tz)
  sched %>% filter(.data$gameday == today)
}

# Helper: clear nflreadr cache each poll to get fresh mirrors
.clear_nfl_cache <- function(){
  try(nflreadr::.clear_cache(), silent = TRUE)
  options(nflreadr.verbose = FALSE)
}

# Main fetcher
fetch_live_plays_nflreadr <- function(season = nflreadr::most_recent_season(),
                                      tz = Sys.getenv("FD_TZ", "America/Chicago")) {
  .clear_nfl_cache()

  games_today <- .current_week_games(season, tz)
  if (!nrow(games_today)) return(tibble())  # no games today

  # Restrict to today's game_ids to save work
  todays_ids <- games_today$game_id %||% unique(games_today$game_id)

  # Pull full-season PBP (fast, cached on mirror), then filter to games today
  pbp <- nflreadr::load_pbp(season) %>%
    filter(.data$game_id %in% todays_ids)

  if (!nrow(pbp)) return(tibble())

  # Keep fields we need; rename to match our downstream expectations
  out <- pbp %>%
    transmute(
      down,
      game_id,
      season = as.integer(season),
      week   = as.integer(week),
      qtr    = as.integer(qtr),
      game_seconds_remaining = as.numeric(game_seconds_remaining),
      posteam = .data$posteam,
      defteam = .data$defteam,
      yardline_100 = as.numeric(yardline_100),
      ydstogo = as.numeric(ydstogo),
      score_differential = as.numeric(score_differential),
      play_type = .data$play_type,
      # Environmental — nflreadr PBP generally won't have these; fill NAs
      roof = NA_character_,
      surface = NA_character_,
      wind = NA_real_,
      temp = NA_real_,
      spread_line = NA_real_,
      total_line  = NA_real_,
      posteam_timeouts_remaining = as.integer(posteam_timeouts_remaining %||% NA_integer_),
      defteam_timeouts_remaining = as.integer(defteam_timeouts_remaining %||% NA_integer_),
      wp = as.numeric(wp),

      # IDs useful for dedupe/cooldowns
      drive_id = .data$drive,
      play_id  = .data$play_id,

      # Scores: nflfastR columns are posteam_score & defteam_score at snap
      posteam_score = as.numeric(posteam_score %||% NA_real_),
      defteam_score = as.numeric(defteam_score %||% NA_real_),

      # Sometimes helpful for formatting/logs
      home_score = as.numeric(total_home_score %||% NA_real_),
      away_score = as.numeric(total_away_score %||% NA_real_),
      clock = .data$clock
    )

  out
}
