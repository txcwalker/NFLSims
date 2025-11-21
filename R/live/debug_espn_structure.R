# R/live/debug_espn_structure.R
# Figure out what structure ESPN is actually returning

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

cat("\n=== ESPN JSON Structure Debug ===\n\n")

# Get raw response
cat("1. Fetching scoreboard (with flatten=FALSE)...\n")
resp <- httr::GET(
  "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard",
  httr::user_agent("debug/1.0"),
  httr::timeout(10)
)

txt <- httr::content(resp, "text", encoding = "UTF-8")
cat("   Response size:", nchar(txt), "bytes\n\n")

# Parse with flatten=FALSE (what adapter uses)
cat("2. Parsing with flatten=FALSE, simplifyVector=FALSE...\n")
sb <- jsonlite::fromJSON(txt, flatten = FALSE, simplifyVector = FALSE)

cat("   Top-level structure:\n")
cat("      typeof(sb) =", typeof(sb), "\n")
cat("      class(sb) =", paste(class(sb), collapse = ", "), "\n")
cat("      names(sb) =", paste(head(names(sb), 10), collapse = ", "), "\n\n")

cat("3. Events structure:\n")
cat("      length(sb$events) =", length(sb$events), "\n")
cat("      typeof(sb$events) =", typeof(sb$events), "\n")
cat("      class(sb$events) =", paste(class(sb$events), collapse = ", "), "\n\n")

if (length(sb$events) > 0) {
  cat("4. First event structure:\n")
  e1 <- sb$events[[1]]
  cat("      typeof(e1) =", typeof(e1), "\n")
  cat("      class(e1) =", paste(class(e1), collapse = ", "), "\n")
  cat("      length(e1) =", length(e1), "\n")

  if (is.list(e1)) {
    cat("      names(e1)[1:10] =", paste(head(names(e1), 10), collapse = ", "), "\n\n")

    cat("5. Trying to extract basic fields:\n")
    cat("      e1$id =", ifelse(is.null(e1$id), "NULL", e1$id), "\n")
    cat("      e1$uid =", ifelse(is.null(e1$uid), "NULL", e1$uid), "\n")
    cat("      e1$date =", ifelse(is.null(e1$date), "NULL", e1$date), "\n")

    if (!is.null(e1$competitions)) {
      cat("      typeof(e1$competitions) =", typeof(e1$competitions), "\n")
      cat("      length(e1$competitions) =", length(e1$competitions), "\n")

      if (length(e1$competitions) > 0) {
        comp1 <- e1$competitions[[1]]
        cat("      typeof(comp1) =", typeof(comp1), "\n")
        cat("      class(comp1) =", paste(class(comp1), collapse = ", "), "\n")

        if (is.list(comp1)) {
          cat("      names(comp1)[1:15] =", paste(head(names(comp1), 15), collapse = ", "), "\n")

          # Try to get status
          cat("\n6. Status info:\n")
          if (!is.null(comp1$status)) {
            cat("      typeof(status) =", typeof(comp1$status), "\n")
            cat("      names(status) =", paste(names(comp1$status), collapse = ", "), "\n")
            if (!is.null(comp1$status$type)) {
              cat("      status$type =", paste(comp1$status$type, collapse = ", "), "\n")
            }
          }

          # Try to get competitors
          cat("\n7. Competitors info:\n")
          if (!is.null(comp1$competitors)) {
            cat("      typeof(competitors) =", typeof(comp1$competitors), "\n")
            cat("      length(competitors) =", length(comp1$competitors), "\n")

            if (length(comp1$competitors) >= 2) {
              c1 <- comp1$competitors[[1]]
              cat("      typeof(comp[1]) =", typeof(c1), "\n")
              if (is.list(c1)) {
                cat("      names(comp[1])[1:10] =", paste(head(names(c1), 10), collapse = ", "), "\n")
                cat("      comp[1]$homeAway =", ifelse(is.null(c1$homeAway), "NULL", c1$homeAway), "\n")
                if (!is.null(c1$team)) {
                  cat("      comp[1]$team$abbreviation =", ifelse(is.null(c1$team$abbreviation), "NULL", c1$team$abbreviation), "\n")
                }
              }
            }
          }
        }
      }
    }
  } else {
    cat("      (First event is not a list!)\n")
    cat("      First 200 chars:\n")
    cat("      ", substr(as.character(e1), 1, 200), "\n")
  }
}

cat("\n=== Debug Complete ===\n\n")