# R/simulators/fourth_down/run_one_sim.R
# CLI subprocess bridge for Python live bot.
# Reads a game state JSON string from stdin, runs the simulator, and outputs JSON.
# Reference documentation: docs/database_guide.md
# ------------------------------------------------------------------------------

# Suppress all package messages to avoid polluting stdout
suppressPackageStartupMessages({
  library(jsonlite)
})

# Source the main simulator code
source("R/simulators/fourth_down/fourth_down_decision.R")

main <- function() {
  # Read JSON from stdin
  input_lines <- readLines(con = "stdin", warn = FALSE)
  if (length(input_lines) == 0 || !nzchar(input_lines)) {
    stop("No game state JSON provided on stdin.")
  }
  
  input_json <- paste(input_lines, collapse = "\n")
  game_state <- jsonlite::fromJSON(input_json, simplifyVector = TRUE)
  
  # Ensure all variables are scalar and correct type, mapping defaults for missing elements
  clean_state <- list(
    down = as.integer(game_state$down %||% 4L),
    yardline_100 = as.numeric(game_state$yardline_100 %||% NA_real_),
    ydstogo = as.integer(game_state$ydstogo %||% NA_integer_),
    game_seconds_remaining = as.numeric(game_state$game_seconds_remaining %||% NA_real_),
    score_differential = as.numeric(game_state$score_differential %||% 0),
    roof = as.character(game_state$roof %||% "outdoors"),
    surface = as.character(game_state$surface %||% "grass"),
    wind = as.numeric(game_state$wind %||% 0),
    temp = as.numeric(game_state$temp %||% 72),
    spread_line = as.numeric(game_state$spread_line %||% 0),
    total_line = as.numeric(game_state$total_line %||% 44),
    posteam_timeouts_remaining = as.integer(game_state$posteam_timeouts_remaining %||% 3L),
    defteam_timeouts_remaining = as.integer(game_state$defteam_timeouts_remaining %||% 3L)
  )
  
  # Run the simulation using the clean, isolated state list
  result <- simulate_fourth_down_decision(clean_state, reload_models = FALSE, verbose = FALSE)
  
  # Convert result to JSON and write to stdout
  cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = FALSE), "\n")
}

tryCatch({
  main()
}, error = function(e) {
  write(paste("R_ERROR:", conditionMessage(e)), stderr())
  err_res <- list(error = conditionMessage(e))
  cat(jsonlite::toJSON(err_res, auto_unbox = TRUE), "\n")
  quit(status = 1)
})
