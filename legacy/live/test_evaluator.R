# R/live/test_evaluator.R
# Test the fourth-down evaluator with ESPN data

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

cat("\n=== Testing Fourth-Down Evaluator ===\n\n")

# Load the fetcher and evaluator
source("R/bots/fetch_live_plays_espn.R")
source("R/bots/run_fd_live_once.R")

# Step 1: Fetch plays (not just in-progress, include all)
cat("1. Fetching plays from ESPN...\n")
plays <- tryCatch(
  fetch_live_plays_espn(only_in_progress = FALSE),
  error = function(e) {
    cat("   ERROR:", conditionMessage(e), "\n")
    return(tibble())
  }
)

cat("   Fetched", nrow(plays), "total plays\n")

if (nrow(plays) > 0) {
  cat("   Columns:", paste(names(plays), collapse = ", "), "\n\n")

  # Step 2: Filter to 4th downs
  cat("2. Filtering to 4th downs...\n")
  plays_4th <- plays %>% filter(down == 4)
  cat("   Found", nrow(plays_4th), "4th down plays\n\n")

  if (nrow(plays_4th) > 0) {
    cat("3. Sample 4th down plays:\n")
    print(plays_4th %>%
          select(game_id, play_id, qtr, down, ydstogo, yardline_100) %>%
          head(5))

    # Step 3: Try running the evaluator
    cat("\n4. Running full evaluator (run_fd_live_once)...\n")
    result <- tryCatch(
      run_fd_live_once(fetcher = fetch_live_plays_espn, write_artifacts = TRUE),
      error = function(e) {
        cat("   ERROR:", conditionMessage(e), "\n")
        return(tibble())
      }
    )

    cat("   Evaluated", nrow(result), "rows\n")

    if (nrow(result) > 0) {
      cat("\n   Result columns:", paste(names(result), collapse = ", "), "\n")
      cat("\n   Sample evaluated rows:\n")
      print(result %>%
            select(game_id, off, def, down, ydstogo, best_action, called_action) %>%
            head(5))
    }
  } else {
    cat("   (No 4th downs in current data)\n")
  }
} else {
  cat("   (No plays available - games may be finished or out of season)\n")
}

cat("\n=== Test Complete ===\n\n")