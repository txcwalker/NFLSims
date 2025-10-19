# R/live/test_espn_fetch.R
# ------------------------------------------------------------------------------
# Smoke test for ESPN adapter:
# - Picks a live game when possible (or ESPN_EVENT_ID override)
# - Fetches PBP (unflattened) and normalizes -> FD rows
# - Non-fatal when games are pregame (returns empty tibble)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(glue); library(readr)
  library(httr); library(jsonlite); library(purrr); library(stringr)
})
source("R/live/espn_adapter.R")

# No-flatten JSON fetch (local helper)
.espn_json_noflat <- function(url) {
  resp <- httr::GET(url, httr::user_agent("fd-bot/test-espn-fetch/0.5"), httr::timeout(10))
  httr::stop_for_status(resp)
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = FALSE)
}

.team_map_from_comps <- function(comps){
  if (is.null(comps) || !length(comps)) return(tibble())
  tibble(
    side   = purrr::map_chr(comps, ~ .x$homeAway %||% NA_character_),
    abbrev = purrr::map_chr(comps, ~ .x$team$abbreviation %||% .x$team$shortDisplayName %||% .x$team$displayName %||% NA_character_)
  )
}

# Try summary → pbp header → scoreboard for a competitors map
.team_map_any <- function(event_id){
  # 1) SUMMARY
  tm <- try({
    sm <- .espn_json_noflat(sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event=%s", event_id))
    .team_map_from_comps(purrr::pluck(sm, "header", "competitions", 1, "competitors"))
  }, silent = TRUE)
  if (inherits(tm, "tbl_df") && nrow(tm)) return(tm)

  # 2) PBP header
  tm <- try({
    pb <- .espn_json_noflat(sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/playbyplay?event=%s", event_id))
    .team_map_from_comps(purrr::pluck(pb, "header", "competitions", 1, "competitors"))
  }, silent = TRUE)
  if (inherits(tm, "tbl_df") && nrow(tm)) return(tm)

  # 3) SCOREBOARD
  tm <- try({
    sb <- .espn_json_noflat("https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard")
    ev <- purrr::detect(sb$events %||% list(), ~ (.x$id %||% "") == as.character(event_id))
    .team_map_from_comps(purrr::pluck(ev, "competitions", 1, "competitors"))
  }, silent = TRUE)
  if (inherits(tm, "tbl_df") && nrow(tm)) return(tm)

  tibble()
}

# Choose an event id (prefer live), or honor env override
pick_event_id <- function(strict_live = TRUE) {
  forced <- Sys.getenv("ESPN_EVENT_ID", unset = "")
  if (nzchar(forced)) return(forced)

  evs <- tryCatch(espn_list_events(), error = function(e) tibble())
  if (!nrow(evs)) stop("No events found on the ESPN NFL scoreboard.", call. = FALSE)

  in_prog <- evs |> filter(tolower(state) == "in") |> slice_head(n = 1)
  if (strict_live) {
    if (!nrow(in_prog)) {
      message("No in-progress events. Set ESPN_EVENT_ID to test a specific game.")
      return(evs$id[[1]])
    }
    return(in_prog$id[[1]])
  }
  if (nrow(in_prog)) return(in_prog$id[[1]])
  evs$id[[1]]
}

# --- Main smoke test ----------------------------------------------------------
test_espn_fetch <- function(limit_rows = 50, write_csv = TRUE, strict_live = FALSE) {
  event_id <- pick_event_id(strict_live = strict_live)
  message(glue("→ Using event_id: {event_id}"))

  pb  <- espn_fetch_pbp(event_id)
  raw <- pb$plays |> tibble::as_tibble()
  message(glue("Fetched {nrow(raw)} raw plays."))

  tm  <- .team_map_any(event_id)
  if (!nrow(tm) && nrow(raw) > 0) {
    # Last-resort: infer from plays
    abbrs <- raw$team_abbr %>% as.character() %>% toupper() %>% unique()
    abbrs <- abbrs[!is.na(abbrs) & nzchar(abbrs)]
    if (length(abbrs) >= 2) tm <- tibble(side = c("home","away"), abbrev = abbrs[1:2])
  }

  if (!nrow(tm)) {
    message("No team map and no plays (likely pregame). Returning empty tibble.")
    return(invisible(tibble()))
  }

  season_guess <- suppressWarnings(tryCatch(nflreadr::most_recent_season(), error = function(e) NA_integer_))
  rows <- espn_plays_to_fd_rows(event_id, raw, tm, season = season_guess, week = NA_integer_)
  sample_df <- rows |> arrange(desc(game_seconds_remaining)) |> slice_head(n = limit_rows)
  print(sample_df, n = min(nrow(sample_df), 20))

  if (isTRUE(write_csv) && nrow(sample_df)) {
    out_dir <- "data/live/samples"; dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    out_path <- file.path(out_dir, glue("espn_sample_{event_id}_{ts}.csv"))
    readr::write_csv(sample_df, out_path)
    message(glue("✓ Wrote sample to: {out_path}"))
  }

  invisible(sample_df)
}

# Optional autorun for local dev:
# try(test_espn_fetch(limit_rows = 50, write_csv = TRUE, strict_live = FALSE), silent = TRUE)
