# R/live/test_espn_direct.R
# Minimal test: just check if ESPN API is accessible

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

cat("\n=== ESPN Scoreboard API Test ===\n\n")

# Test 1: Can we reach ESPN?
cat("1. Testing ESPN scoreboard endpoint...\n")
resp <- tryCatch(
  httr::GET(
    "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard",
    httr::user_agent("fd-bot/test/1.0"),
    httr::timeout(10)
  ),
  error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    return(NULL)
  }
)

if (is.null(resp)) {
  cat("FAILED: Could not reach ESPN API\n")
  quit(status = 1)
}

cat("   Status:", resp$status_code, "\n")

# Test 2: Parse response
cat("\n2. Parsing JSON response...\n")
txt <- httr::content(resp, "text", encoding = "UTF-8")
cat("   Response size:", nchar(txt), "bytes\n")

data <- tryCatch(
  jsonlite::fromJSON(txt, flatten = FALSE),
  error = function(e) {
    cat("   ERROR parsing JSON:", conditionMessage(e), "\n")
    return(NULL)
  }
)

if (is.null(data)) {
  cat("FAILED: Could not parse ESPN response\n")
  quit(status = 1)
}

cat("   ✓ JSON parsed successfully\n")

# Test 3: Check structure
cat("\n3. Checking response structure...\n")
cat("   Top-level keys:", paste(head(names(data), 10), collapse = ", "), "\n")

if (!is.null(data$events)) {
  cat("   Events found:", length(data$events), "\n")

  if (length(data$events) > 0) {
    e <- data$events[[1]]
    cat("   First event keys:", paste(head(names(e), 8), collapse = ", "), "\n")

    comp <- e$competitions[[1]]
    if (!is.null(comp)) {
      cat("   First competition has", length(comp$competitors), "competitors\n")
      if (length(comp$competitors) >= 2) {
        home <- comp$competitors[[1]]$team$abbreviation
        away <- comp$competitors[[2]]$team$abbreviation
        cat("   Sample matchup:", away, "@", home, "\n")
      }

      status <- comp$status$type$state
      cat("   Status:", status, "\n")
    }
  }
} else {
  cat("   No events in response\n")
}

cat("\n=== Test Complete ===\n")
cat("ESPN API is reachable and returning valid data.\n")