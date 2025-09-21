# R/bots/run_live_chunk.R
# Run a short polling window (e.g., 8 minutes) and exit — ideal for GitHub Actions.

suppressPackageStartupMessages({
  library(tibble); library(glue)
})

# Load env if available
if (requireNamespace("dotenv", quietly = TRUE)) {
  try(dotenv::load_dot_env(".env", override = FALSE), silent = TRUE)
}

# Project sources
source("R/bots/post_targets.R")      # post_everywhere()
source("R/bots/posting_policy.R")    # should_post_decision(), format_post(), optional mark_posted()
source("R/bots/run_fd_live_once.R")  # returns standardized rows
source("R/bots/run_live_loop.R")     # reuses .process_batch()

`%||%` <- function(a,b) if (is.null(a) || length(a)==0 || (is.atomic(a) && all(is.na(a)))) b else a
.dir_ok <- function(p){ if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE) }

run_live_chunk <- function(duration_minutes = 8, poll_seconds = 20) {
  log_dir <- Sys.getenv("LIVE_LOG_DIR", unset = "R/bots/logs")
  csv_dir <- Sys.getenv("LIVE_CSV_DIR",  unset = "R/bots/live_csv")
  .dir_ok(log_dir); .dir_ok(csv_dir)

  deadline <- Sys.time() + duration_minutes*60
  total_posted <- 0L

  repeat {
    if (Sys.time() >= deadline) break
    t0 <- Sys.time()

    df <- tryCatch(run_fd_live_once(), error = function(e) {
      message("[run_fd_live_once ERROR] ", conditionMessage(e)); tibble()
    })

    n_posted <- tryCatch(.process_batch(df, csv_dir), error = function(e) {
      message("[Batch ERROR] ", conditionMessage(e)); 0L
    })
    total_posted <- total_posted + as.integer(n_posted)

    # Append a simple log line
    cat(glue("[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] chunk: rows={nrow(df %||% tibble())}, posted={n_posted}\n"),
        file = file.path(log_dir, paste0("live_", format(Sys.Date(), "%Y%m%d"), ".log")),
        append = TRUE)

    # Sleep to maintain cadence
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    Sys.sleep(max(0, poll_seconds - elapsed))
  }

  message(glue("[Chunk finished] Total posted this chunk: {total_posted}"))
  invisible(total_posted)
}

# Local smoke test:
# source("R/bots/run_live_chunk.R"); run_live_chunk(duration_minutes = 2, poll_seconds = 10)
