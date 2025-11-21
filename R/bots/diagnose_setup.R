# R/bots/diagnose_setup.R
# Run this locally to check if everything is wired up correctly

cat("\n", strrep("=", 60), "\n")
cat("NFL Bot Diagnostic Check\n")
cat(strrep("=", 60), "\n\n")

# Check 1: Required packages
cat("1. Checking required packages...\n")
packages <- c("dplyr", "purrr", "tibble", "readr", "httr", "jsonlite",
              "lubridate", "glue", "stringr", "data.table")

missing <- c()
for (pkg in packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("   ✓", pkg, "\n")
  } else {
    cat("   ✗", pkg, "(MISSING)\n")
    missing <- c(missing, pkg)
  }
}

if (length(missing) > 0) {
  cat("\n   Install missing packages with:\n")
  cat("   install.packages(c('", paste(missing, collapse = "', '"), "'))\n\n")
}

# Check 2: File structure
cat("\n2. Checking required files...\n")
files <- c(
  "R/bots/run_live_today.R",
  "R/bots/run_fd_live_once.R",
  "R/bots/fetch_live_plays_espn.R",
  "R/bots/posting_policy.R",
  "R/bots/post_targets.R",
  "R/live/espn_adapter.R",
  "R/simulators/fourth_down/fourth_down_decision.R",
  "data/models/fg_model.rds",
  "data/models/fd_model.rds",
  "data/models/wp_model.rds"
)

for (f in files) {
  if (file.exists(f)) {
    cat("   ✓", f, "\n")
  } else {
    cat("   ✗", f, "(MISSING)\n")
  }
}

# Check 3: ESPN API connectivity
cat("\n3. Testing ESPN API connectivity...\n")
tryCatch({
  resp <- httr::GET(
    "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard",
    httr::user_agent("diagnostic/1.0"),
    httr::timeout(5)
  )

  if (httr::status_code(resp) == 200) {
    cat("   ✓ ESPN API is reachable (HTTP 200)\n")

    data <- jsonlite::fromJSON(httr::content(resp, "text"))
    n_events <- length(data$events %||% list())
    cat("   ✓ Events on scoreboard:", n_events, "\n")

    if (n_events > 0) {
      e <- data$events[[1]]
      comp <- e$competitions[[1]]
      home <- comp$competitors[[1]]$team$abbreviation
      away <- comp$competitors[[2]]$team$abbreviation
      cat("   → Example:", away, "@", home, "\n")
    }
  } else {
    cat("   ✗ ESPN API returned HTTP", httr::status_code(resp), "\n")
  }
}, error = function(e) {
  cat("   ✗ Error connecting to ESPN:", conditionMessage(e), "\n")
})

# Check 4: Try loading the adapter
cat("\n4. Testing ESPN adapter...\n")
tryCatch({
  source("R/live/espn_adapter.R", local = TRUE)
  cat("   ✓ ESPN adapter loaded successfully\n")

  # Try getting active event IDs
  ids <- espn_active_event_ids(only_live = TRUE)
  cat("   ✓ Found", length(ids), "live events\n")

  if (length(ids) > 0) {
    cat("   → First event ID:", ids[1], "\n")
  }
}, error = function(e) {
  cat("   ✗ Error loading adapter:", conditionMessage(e), "\n")
})

# Check 5: Try a test fetch
cat("\n5. Testing fetch_live_plays_espn()...\n")
tryCatch({
  source("R/bots/fetch_live_plays_espn.R", local = TRUE)
  plays <- fetch_live_plays_espn(only_in_progress = TRUE)
  cat("   ✓ fetch_live_plays_espn() executed\n")
  cat("   → Returned", nrow(plays), "plays\n")

  if (nrow(plays) > 0) {
    cat("   → Columns:", paste(names(plays), collapse = ", "), "\n")
  }
}, error = function(e) {
  cat("   ✗ Error:", conditionMessage(e), "\n")
})

# Check 6: Output directory
cat("\n6. Checking output directories...\n")
out_dirs <- c("R/bots/live_csv", "R/bots/logs", "data/live")
for (d in out_dirs) {
  if (dir.exists(d)) {
    cat("   ✓", d, "exists\n")
  } else {
    cat("   ✓", d, "(will be created on first run)\n")
  }
}

cat("\n", strrep("=", 60), "\n")
cat("Diagnostic complete!\n")
cat(strrep("=", 60), "\n\n")