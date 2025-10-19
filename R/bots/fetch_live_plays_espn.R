# R/bots/fetch_live_plays_espn.R
# ------------------------------------------------------------------------------
# ESPN live fetcher:
# - Enumerates active events (optionally filter to in-progress)
# - Fetches PBP + team map, normalizes to FD rows (guaranteed `down` column)
# - Dedups per-event via adapter's state file
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tibble)
})
source("R/live/espn_adapter.R")

# Optional: derive season/week if you need them in rows (here we keep NA)
.get_season_week <- function(){ list(season = NA_integer_, week = NA_integer_) }

fetch_live_plays_espn <- function(only_in_progress = TRUE){
  # Pull scoreboard once to optionally filter to "in" games
  ids <- tryCatch(espn_active_event_ids(), error = function(e) character())
  if (!length(ids)) return(tibble())

  sw <- .get_season_week()

  # Optional in-progress filter using summary$status.type.state
  if (isTRUE(only_in_progress)) {
    states <- purrr::map_chr(ids, function(id){
      sm <- tryCatch(espn_event(id), error = function(e) NULL)
      if (is.null(sm)) return(NA_character_)
      sm[["status"]][["type"]][["state"]] %||% NA_character_
    })
    ids <- ids[tolower(states) == "in"]
  }

  if (!length(ids)) return(tibble())

  rows <- purrr::map_dfr(ids, function(eid){
    pbp  <- tryCatch(espn_fetch_pbp(eid), error = function(e) NULL)
    if (is.null(pbp)) return(tibble())
    tmap <- tryCatch(espn_team_map(eid), error = function(e) NULL)
    if (is.null(tmap) || !nrow(tmap)) return(tibble())

    raw <- espn_plays_to_fd_rows(eid, pbp$plays, tmap, season = sw$season, week = sw$week)
    if (!nrow(raw)) return(tibble())

    # Guarantee `down` exists (adapter should have it; this is belt-and-suspenders)
    if (!("down" %in% names(raw))) {
      down_map <- tibble(
        play_id = pbp$plays$play_id,
        down    = suppressWarnings(as.integer(pbp$plays$start_down))
      )
      raw <- dplyr::left_join(raw, down_map, by = "play_id")
    }

    filter_new_plays(eid, raw)
  })

  rows
}

# Alias so run_live() will default to ESPN when no fetcher is passed
fetch_live_plays <- function(){ fetch_live_plays_espn() }
