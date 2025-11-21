# R/live/test_espn_simple.R (FIXED)
# Very simple test to see what ESPN returns

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(purrr)
})

cat("\n=== Simple ESPN Test ===\n\n")

# Step 1: Get scoreboard
cat("1. Fetching scoreboard...\n")
resp <- httr::GET(
  "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard",
  httr::user_agent("test/1.0"),
  httr::timeout(10)
)

sb <- jsonlite::fromJSON(httr::content(resp, "text"), flatten = FALSE, simplifyVector = FALSE)
cat("   Got", length(sb$events), "events\n\n")

# Step 2: Look at first few events
cat("2. Examining events structure...\n")
if (length(sb$events) > 0) {
  for (i in 1:min(3, length(sb$events))) {
    e <- sb$events[[i]]
    cat("\n   Event", i, ":\n")

    # Safe extraction
    id_direct <- e$id
    cat("      e$id =", id_direct, "\n")

    # Get competition
    comp <- e$competitions[[1]]
    if (!is.null(comp)) {
      id_comp <- comp$id
      cat("      comp$id =", id_comp, "\n")

      # Status is returned as a vector or list, extract safely
      status_vec <- comp$status$type
      status_str <- NA_character_

      # Try to extract the status name (usually 2nd element)
      if (is.list(status_vec)) {
        status_vec <- unlist(status_vec)
      }

      if (is.character(status_vec) && length(status_vec) > 1) {
        status_str <- status_vec[2]  # e.g., "STATUS_FINAL" or "STATUS_IN_PROGRESS"
      } else if (is.character(status_vec) && length(status_vec) > 0) {
        status_str <- status_vec[1]
      }

      cat("      status =", as.character(status_str), "\n")

      if (!is.null(comp$competitors) && length(comp$competitors) >= 2) {
        h <- comp$competitors[[1]]$team$abbreviation
        a <- comp$competitors[[2]]$team$abbreviation
        cat("      matchup =", a, "@", h, "\n")
      }
    }
  }
} else {
  cat("   No events to examine\n")
}

# Step 3: Test espn_active_event_ids function
cat("\n3. Testing espn_active_event_ids()...\n")
source("R/live/espn_adapter.R", local = TRUE)

ids <- tryCatch(
  espn_active_event_ids(only_live = FALSE),
  error = function(e) {
    cat("   ERROR:", conditionMessage(e), "\n")
    return(character())
  }
)
cat("   Found", length(ids), "event IDs (only_live=FALSE)\n")
if (length(ids) > 0) {
  cat("   IDs:", paste(head(ids, 3), collapse = ", "), "\n")
}

# Also test with only_live=TRUE
ids_live <- tryCatch(
  espn_active_event_ids(only_live = TRUE),
  error = function(e) {
    cat("   ERROR:", conditionMessage(e), "\n")
    return(character())
  }
)
cat("   Found", length(ids_live), "live event IDs (only_live=TRUE)\n")
if (length(ids_live) > 0) {
  cat("   IDs:", paste(head(ids_live, 3), collapse = ", "), "\n")
}

# Step 4: Test fetching PBP for first event (if any)
cat("\n4. Testing espn_fetch_pbp()...\n")
if (length(ids) > 0) {
  test_id <- ids[1]
  cat("   Using event ID:", test_id, "\n")

  pbp <- tryCatch(
    espn_fetch_pbp(test_id),
    error = function(e) {
      cat("   ERROR:", conditionMessage(e), "\n")
      return(NULL)
    }
  )

  if (!is.null(pbp)) {
    cat("   Got", nrow(pbp$plays), "plays\n")
    if (nrow(pbp$plays) > 0) {
      cat("   Columns:", paste(names(pbp$plays), collapse = ", "), "\n")
      cat("\n   Sample plays (first 5):\n")
      cols_to_show <- intersect(c("play_id", "text", "start_down", "team_abbr"), names(pbp$plays))
      print(head(pbp$plays[, cols_to_show, drop = FALSE], 5))
    }
  }
} else {
  cat("   No event IDs to test\n")
}

cat("\n=== Test Complete ===\n\n")