# R/bots/run_live_today.R
# Modified to support FORCE_FETCH for testing outside game windows

suppressPackageStartupMessages({
  library(dplyr); library(lubridate); library(glue); library(jsonlite); library(tibble)
  library(readr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.atomic(a) && all(is.na(a)))) b else a
.ensure_dir <- function(p){ if(!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE) }
.now_stamp <- function(){ format(Sys.time(), "%Y%m%d-%H%M%S") }

# IMPORTANT: Only source fetch/eval code (no posting files here)
.source_fetch_stack <- function(){
  source("R/bots/run_fd_live_once.R")        # returns tibble and can write artifacts
  # this file must define fetch_live_plays() for the ESPN path
  source("R/bots/fetch_live_plays_espn.R", local = TRUE)
}

# Decide whether we are currently "in window" for fetching
# Window = [first_kick - buffer_before, last_kick + max_game_hours + buffer_after]
.in_window_now <- function(tz = "America/Chicago",
                           buffer_before_mins = 20,
                           max_game_hours = 4.5,
                           buffer_after_mins = 30) {
  Sys.setenv(TZ = tz)

  # ESPN scoreboard (UTC ISO times per event)
  sb <- tryCatch(
    jsonlite::fromJSON("https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard", flatten = TRUE),
    error = function(e) NULL
  )
  if (is.null(sb) || is.null(sb$events) || !length(sb$events)) {
    return(list(in_window = FALSE, reason = "[LiveToday] No events on scoreboard."))
  }

  # Extract dates safely - handle both list and data.frame structures
  kicks_utc <- tryCatch({
    if (is.data.frame(sb$events)) {
      sb$events$date
    } else if (is.list(sb$events)) {
      vapply(sb$events, function(e) {
        if (is.list(e) && !is.null(e$date)) {
          as.character(e$date)
        } else if (is.character(e$date)) {
          e$date
        } else {
          NA_character_
        }
      }, character(1))
    } else {
      NA_character_
    }
  }, error = function(e) {
    warning("Error extracting dates from scoreboard: ", e$message)
    NA_character_
  })

  kicks     <- suppressWarnings(lubridate::ymd_hms(kicks_utc, tz = "UTC"))
  kicks_loc <- with_tz(kicks, tz)
  today     <- as_date(Sys.time(), tz = tz)

  day_mask  <- as_date(kicks_loc, tz) == today
  if (!any(day_mask, na.rm = TRUE)) {
    return(list(in_window = FALSE, reason = glue("[LiveToday] No games today ({today}).")))
  }

  first_kick <- min(kicks_loc[day_mask], na.rm = TRUE)
  last_kick  <- max(kicks_loc[day_mask], na.rm = TRUE)

  start_at <- first_kick - minutes(buffer_before_mins)
  end_at   <- last_kick + hours(max_game_hours) + minutes(buffer_after_mins)

  now <- Sys.time()
  list(
    in_window = (now >= start_at && now <= end_at),
    reason    = glue("[LiveToday] Window {format(start_at, '%H:%M')}–{format(end_at, '%H:%M')} local; now={format(now, '%H:%M')}"),
    start_at  = start_at, end_at = end_at
  )
}

run_live_today <- function(tz = "America/Chicago",
                           buffer_before_mins = 20,
                           max_game_hours = 4.5,
                           buffer_after_mins = 30,
                           quick_poll_count = as.integer(Sys.getenv("QUICK_POLL_COUNT", "1")),
                           poll_seconds = as.integer(Sys.getenv("POLL_SECONDS", "20"))) {
  .ensure_dir("R/bots/live_csv")
  ts <- .now_stamp()
  csv_path <- glue("R/bots/live_csv/plays_{ts}.csv")
  log_path <- glue("R/bots/live_csv/run_{ts}.log")

  # Only fetch/eval code — no posting
  .source_fetch_stack()

  # NEW: Check for FORCE_FETCH environment variable
  force_fetch <- identical(Sys.getenv("FORCE_FETCH", "false"), "true")

  window <- .in_window_now(
    tz = tz,
    buffer_before_mins = buffer_before_mins,
    max_game_hours = max_game_hours,
    buffer_after_mins = buffer_after_mins
  )

  # If outside window AND not forcing, write empty and exit
  if (!isTRUE(window$in_window) && !force_fetch) {
    readr::write_csv(tibble(), csv_path)
    msg <- paste0(window$reason, " (outside window; no fetch). Set FORCE_FETCH=true to override.")
    readr::write_lines(msg, log_path)
    message(msg)
    return(invisible(0L))
  }

  # NEW: Log when we're forcing
  if (force_fetch && !isTRUE(window$in_window)) {
    message("[LiveToday] FORCE_FETCH=true: overriding window check.")
    message(window$reason)
  } else if (isTRUE(window$in_window)) {
    message("[LiveToday] Inside game window. Fetching...")
  }

  # We're inside the window (or forcing) — do a quick fetch cycle
  out <- tibble()
  err <- NULL
  for (i in seq_len(max(1L, quick_poll_count))) {
    res <- tryCatch(
      run_fd_live_once(),   # this writes its own CSV+LOG too, but we also aggregate
      error = function(e) { err <<- e; tibble() }
    )
    if (nrow(res)) out <- dplyr::bind_rows(out, res)
    if (i < quick_poll_count) Sys.sleep(poll_seconds)
  }

  # De-dup if multiple polls ran
  if (nrow(out)) {
    # Prefer unique by (game_id, play_id) when available
    uniq_cols <- intersect(c("game_id","play_id"), names(out))
    if (length(uniq_cols) >= 2) {
      out <- out |> dplyr::distinct(dplyr::across(all_of(uniq_cols)), .keep_all = TRUE)
    }
  }

  readr::write_csv(out, csv_path)
  msg <- paste0(window$reason, " | fetched_rows=", nrow(out))
  if (!is.null(err)) msg <- paste0(msg, " | error=", err$message)
  readr::write_lines(msg, log_path)
  message(msg)
  invisible(nrow(out))
}

# Run if this file is executed directly
if (sys.nframe() == 0) {
  run_live_today()
}