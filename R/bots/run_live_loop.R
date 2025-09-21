# R/bots/run_live_loop.R
# -----------------------------------------------------------------------------
# Live posting loop:
#  - polls run_fd_live_once() every N seconds
#  - gates with should_post_decision(row, game_meta, now)
#  - formats via format_post()
#  - posts via post_everywhere()
#  - logs to CSV + .log file
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(glue); library(tibble)
})

# Load env if available
if (requireNamespace("dotenv", quietly = TRUE)) {
  try(dotenv::load_dot_env(".env", override = FALSE), silent = TRUE)
}

# Project sources
source("R/bots/post_targets.R")     # post_everywhere()
source("R/bots/posting_policy.R")   # should_post_decision(), format_post(), optional mark_posted()
source("R/bots/run_fd_live_once.R") # run_fd_live_once()

`%||%` <- function(a,b) if (is.null(a)||length(a)==0||(is.atomic(a)&&all(is.na(a)))) b else a
.dir_ok <- function(p){ if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE) }

# --- Policy adapter: build meta + call with expected args ---------------------
.build_game_meta <- function(row){
  list(
    game_id = as.character(row$game_id %||% NA),
    season  = as.integer(row$season %||% NA),
    week    = as.integer(row$week %||% NA),
    posteam = as.character(row$posteam %||% NA),
    defteam = as.character(row$defteam %||% NA),
    qtr     = as.integer(row$qtr %||% NA),
    clock_s = as.numeric(row$game_seconds_remaining %||% NA)
  )
}

.policy_args <- function(row){
  list(
    row       = row,
    game_meta = .build_game_meta(row),
    now       = Sys.time()
  )
}

# One pass: take 0+ candidate rows, gate + post each
.process_batch <- function(df, csv_dir) {
  if (is.null(df) || !nrow(df)) return(invisible(0L))

  posted_ct <- 0L
  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]

    # Gate with your policy (supports row, game_meta, now)
    gate <- tryCatch(
      do.call(should_post_decision, .policy_args(row)),
      error = function(e) {
        message("[Policy ERROR] ", conditionMessage(e))
        list(should_post = FALSE)
      }
    )
    if (!isTRUE(gate$should_post)) next

    # Compose the message
    txt <- tryCatch(format_post(row), error = function(e) {
      message("[Format ERROR] ", conditionMessage(e)); NA_character_
    })
    if (!nzchar(txt)) next

    # Post
    res <- tryCatch(post_everywhere(txt), error = function(e) {
      message("[Post ERROR] ", conditionMessage(e)); list(any = FALSE)
    })

    # Record success & write CSV audit
    if (isTRUE(res$any)) {
      posted_ct <- posted_ct + 1L
      if (exists("mark_posted", mode = "function")) {
        try(mark_posted(row$game_id), silent = TRUE)
      }
      .dir_ok(csv_dir)
      csv_path <- file.path(csv_dir, paste0("live_", format(Sys.Date(), "%Y%m%d"), ".csv"))
      readr::write_csv(row, csv_path, append = file.exists(csv_path))
    }
  }
  invisible(posted_ct)
}

# Public entrypoint
run_live <- function(poll_seconds = 20) {
  log_dir <- Sys.getenv("LIVE_LOG_DIR", unset = "R/bots/logs")
  csv_dir <- Sys.getenv("LIVE_CSV_DIR", unset = "R/bots/live_csv")
  .dir_ok(log_dir); .dir_ok(csv_dir)

  poll_seconds <- as.integer(poll_seconds %||% 20)
  if (is.na(poll_seconds) || poll_seconds < 5) poll_seconds <- 5

  message(glue("Live loop started (poll={poll_seconds}s, DRY_RUN={Sys.getenv('DRY_RUN', '0')})"))

  repeat {
    t0 <- Sys.time()

    df <- tryCatch(run_fd_live_once(), error = function(e) {
      message("[run_fd_live_once ERROR] ", conditionMessage(e)); tibble()
    })

    n_posted <- tryCatch(.process_batch(df, csv_dir), error = function(e) {
      message("[Batch ERROR] ", conditionMessage(e)); 0L
    })

    log_line <- glue("[{format(Sys.time(), '%Y-%m-%d %H:%M:%S')}] polled: rows={nrow(df %||% tibble())}, posted={n_posted}\n")
    cat(log_line, file = file.path(log_dir, paste0("live_", format(Sys.Date(), "%Y%m%d"), ".log")), append = TRUE)

    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    Sys.sleep(max(0, poll_seconds - elapsed))
  }
}

# --- Quick smoke test (manual) -----------------------------------------------
# source("R/bots/run_live_loop.R"); run_live(5)
