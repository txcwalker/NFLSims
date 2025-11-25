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
  cat("[ESPN] Starting fetch_live_plays_espn...\n")

  # Pull scoreboard once to optionally filter to "in" games
  ids <- tryCatch(
    espn_active_event_ids(),
    error = function(e) {
      cat("[ESPN] ERROR in espn_active_event_ids():", conditionMessage(e), "\n")
      character()
    }
  )

  cat("[ESPN] Found", length(ids), "active event IDs\n")
  if (!length(ids)) {
    cat("[ESPN] No active events found, returning empty tibble\n")
    return(tibble())
  }

  sw <- .get_season_week()

  # Optional in-progress filter using summary$status.type.state
  if (isTRUE(only_in_progress)) {
    cat("[ESPN] Filtering to in-progress games only...\n")
    states <- purrr::map_chr(ids, function(id){
      cat("[ESPN] Checking state for event", id, "\n")
      sm <- tryCatch(
        espn_event(id),
        error = function(e) {
          cat("[ESPN] ERROR fetching event", id, ":", conditionMessage(e), "\n")
          NULL
        }
      )
      if (is.null(sm)) {
        cat("[ESPN] Event", id, "returned NULL\n")
        return(NA_character_)
      }
      state <- sm[["status"]][["type"]][["state"]] %||% NA_character_
      cat("[ESPN] Event", id, "state:", state, "\n")
      state
    })

    cat("[ESPN] States before filter:", paste(states, collapse=", "), "\n")
    ids <- ids[tolower(states) == "in"]
    cat("[ESPN] Event IDs after in-progress filter:", paste(ids, collapse=", "), "\n")
  }

  if (!length(ids)) {
    cat("[ESPN] No in-progress events, returning empty tibble\n")
    return(tibble())
  }

  rows <- purrr::map_dfr(ids, function(eid){
    cat("[ESPN] Processing event", eid, "\n")

    pbp  <- tryCatch(
      espn_fetch_pbp(eid),
      error = function(e) {
        cat("[ESPN] ERROR fetching PBP for", eid, ":", conditionMessage(e), "\n")
        NULL
      }
    )
    if (is.null(pbp)) {
      cat("[ESPN] PBP is NULL for event", eid, "\n")
      return(tibble())
    }

    cat("[ESPN] Got", nrow(pbp$plays %||% tibble()), "plays for event", eid, "\n")

    tmap <- tryCatch(
      espn_team_map(eid),
      error = function(e) {
        cat("[ESPN] ERROR fetching team map for", eid, ":", conditionMessage(e), "\n")
        NULL
      }
    )
    if (is.null(tmap) || !nrow(tmap)) {
      cat("[ESPN] Team map is NULL or empty for event", eid, "\n")
      return(tibble())
    }

    cat("[ESPN] Converting", nrow(pbp$plays), "plays to FD rows for event", eid, "\n")
    raw <- tryCatch(
      espn_plays_to_fd_rows(eid, pbp$plays, tmap, season = sw$season, week = sw$week),
      error = function(e) {
        cat("[ESPN] ERROR converting to FD rows for", eid, ":", conditionMessage(e), "\n")
        tibble()
      }
    )

    cat("[ESPN] Got", nrow(raw), "FD rows for event", eid, "\n")
    if (!nrow(raw)) {
      cat("[ESPN] No FD rows for event", eid, "\n")
      return(tibble())
    }

    # Guarantee `down` exists (adapter should have it; this is belt-and-suspenders)
    if (!("down" %in% names(raw))) {
      cat("[ESPN] Adding down column for event", eid, "\n")
      down_map <- tibble(
        play_id = pbp$plays$play_id,
        down    = suppressWarnings(as.integer(pbp$plays$start_down))
      )
      raw <- dplyr::left_join(raw, down_map, by = "play_id")
    }

    cat("[ESPN] Filtering new plays for event", eid, "\n")
    filtered <- tryCatch(
      filter_new_plays(eid, raw),
      error = function(e) {
        cat("[ESPN] ERROR in filter_new_plays for", eid, ":", conditionMessage(e), "\n")
        raw
      }
    )

    cat("[ESPN] After filtering new plays:", nrow(filtered), "rows for event", eid, "\n")
    filtered
  })

  cat("[ESPN] Total rows returned:", nrow(rows), "\n")
  rows
}

# Alias so run_live() will default to ESPN when no fetcher is passed
fetch_live_plays <- function(){
  cat("[fetch_live_plays] Calling fetch_live_plays_espn()\n")
  fetch_live_plays_espn()
}