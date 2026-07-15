suppressPackageStartupMessages({
  library(jsonlite); library(purrr); library(dplyr); library(tibble); library(stringr)
})
source("R/live/espn_adapter.R")

# Pretty-print first N keys/rows of a nested list/data.frame
pp <- function(x, n = 8){
  if (is.list(x) && !is.data.frame(x)) {
    cat("list names:", paste(head(names(x), n), collapse=", "), "\n")
  } else if (is.data.frame(x)) {
    print(utils::str(x[seq_len(min(n, nrow(x))), , drop = FALSE], give.attr = FALSE, vec.len = 3))
  } else {
    print(utils::str(x, give.attr = FALSE, vec.len = 3))
  }
}

# A) SCOREBOARD (NFL-only)
inspect_scoreboard <- function(){
  sb <- jsonlite::fromJSON(
    "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard",
    flatten = FALSE
  )
  cat("=== leagues ===\n"); pp(sb$leagues[[1]])
  cat("\n=== events (count) ===\n"); cat(length(sb$events), "events\n")
  if (length(sb$events)) {
    e <- sb$events[[1]]
    cat("\n=== one event (top-level keys) ===\n"); pp(e)
    cat("\nstatus.type:\n"); pp(purrr::pluck(e, "status", "type"))
    cat("\ncompetitions[1] keys:\n"); pp(purrr::pluck(e, "competitions", 1))
    cat("\ncompetitors team fields (home/away):\n")
    comp <- purrr::pluck(e, "competitions", 1, "competitors")
    if (length(comp)) pp(comp[[1]]$team)
  }
  invisible(sb)
}

# B) SUMMARY (rosters, injuries, leaders, winprob, etc.)
inspect_summary <- function(event_id){
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event=%s", event_id)
  sm <- jsonlite::fromJSON(url, flatten = FALSE)
  cat("=== header ===\n"); pp(sm$header)
  cat("\n=== winprobability (if present) ===\n"); pp(sm$winprobability)
  cat("\n=== gameInfo (stadium/weather) ===\n"); pp(sm$gameInfo)
  invisible(sm)
}

# C) PLAY-BY-PLAY (drives + plays)
inspect_pbp <- function(event_id, show = 6){
  pbp <- espn_fetch_pbp(event_id)   # uses unflattened, defensive parser from adapter
  cat("=== meta ===\n"); print(pbp$meta)
  cat("\n=== plays: columns ===\n"); print(names(pbp$plays))
  if (nrow(pbp$plays)) {
    keep <- intersect(c("play_id","text","type_text","period","clock_display",
                        "start_down","start_dist","start_sddt","start_poss",
                        "start_home","start_away","team_abbr"), names(pbp$plays))
    cat("\n=== sample plays ===\n"); print(dplyr::as_tibble(pbp$plays[seq_len(min(show, nrow(pbp$plays))), keep, drop = FALSE]))
  } else {
    cat("\n(no plays — game may be pregame or parser found none)\n")
  }
  invisible(pbp)
}

# D) What rows will our bot see after transform?
inspect_rows_for_event <- function(event_id, season = NA_integer_, week = NA_integer_){
  pbp <- espn_fetch_pbp(event_id)
  tm  <- espn_team_map(event_id)
  rows <- espn_plays_to_fd_rows(event_id, pbp$plays, tm, season, week)
  cat("=== transformed rows (columns) ===\n"); print(names(rows))
  cat("\n=== head ===\n"); print(utils::head(rows, 8))
  invisible(rows)
}
