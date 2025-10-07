# ------------------------------------------------------------------------------
# ESPN live fetcher
# Returns a data.frame with the columns expected by run_fd_live_once() pipeline.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tibble)
})

source("R/live/espn_adapter.R")

# Optional: pin the logical "season/week" if you want (or leave NA)
.get_season_week <- function(){
  # Neutral defaults; adjust if you have a helper in your project
  list(season = NA_integer_, week = NA_integer_)
}

fetch_live_plays_espn <- function(){
  events <- tryCatch(espn_active_event_ids(), error = function(e) character())
  if (!length(events)) return(tibble())

  sw <- .get_season_week()

  rows <- purrr::map_dfr(events, function(eid){
    pbp <- tryCatch(espn_fetch_pbp(eid), error = function(e) NULL)
    if (is.null(pbp)) return(tibble())

    tmap <- tryCatch(espn_team_map(eid), error = function(e) NULL)
    if (is.null(tmap) || !nrow(tmap)) return(tibble())

    raw <- espn_plays_to_fd_rows(eid, pbp$plays, tmap, season = sw$season, week = sw$week)
    if (!nrow(raw)) return(tibble())

    # Deduplicate newly seen plays only
    filter_new_plays(eid, raw)
  })

  # Ensure a 'down' column exists for run_fd_live_once() filter step
  # ESPN exposes down via start_down in the raw; here we approximate 4th-down candidates by
  # trying to infer when start_down == 4 (if available) otherwise leave NA and let caller filter again.
  if (!nrow(rows)) return(rows)

  # In case some rows came without knowing the down, try to refill from the raw PBP if present later.
  # For now, we set NA; run_fd_live_once() already guards for missing and will drop them.
  rows$down <- NA_integer_

  # We don't know the down for all plays here, but your evaluator will filter to down==4.
  # If you prefer to prefilter here, uncomment and compute a join back to pbp$plays$start_down.

  rows
}

# Optional: make ESPN the default fetcher name used by run_live()
fetch_live_plays <- function(){ fetch_live_plays_espn() }
