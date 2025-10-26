# R/bots/run_fd_live_once.R
# ------------------------------------------------------------------------------
# One-shot “fetch → filter to 4th downs → simulate → policy row” generator.
# - Agnostic to source (ESPN, nflreadr, custom) via a public fetcher:
#       fetch_live_plays()
# - Writes artifacts (CSV + LOG) every run to R/bots/live_csv/.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tibble); library(rlang); library(tidyr)
  library(readr); library(glue)
})

`%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.atomic(a) && all(is.na(a)))) b else a
.ensure_dir <- function(p){ if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE) }
.now_stamp <- function(){ format(Sys.time(), "%Y%m%d-%H%M%S") }

# --- Weather/context defaults (bot stability) ---------------------------------
.apply_weather_defaults <- function(df){
  df %>%
    mutate(
      roof    = as.character(roof %||% "outdoors"),
      surface = as.character(surface %||% "grass"),
      wind    = suppressWarnings(as.numeric(wind %||% NA_real_)),
      temp    = suppressWarnings(as.numeric(temp %||% NA_real_)),
      wind    = ifelse(is.na(wind), 0, wind),
      temp    = ifelse(is.na(temp), 72, temp)
    )
}

# --- Called action from text/type (best-effort) -------------------------------
.infer_called_action <- function(r){
  pt <- tolower(as.character(r$play_type %||% ""))
  if (grepl("punt", pt)) return("punt")
  if (grepl("field_goal|kick", pt)) return("fg")
  if (grepl("run|pass|qb_kneel|qb_spike|sack|scramble", pt)) return("go")
  NA_character_
}

# --- Build the game_state expected by the simulator ---------------------------
.build_game_state <- function(r){
  list(
    down = 4L,
    yardline_100 = suppressWarnings(as.numeric(r$yardline_100 %||% NA_real_)),
    ydstogo      = suppressWarnings(as.numeric(r$ydstogo      %||% NA_real_)),
    game_seconds_remaining = suppressWarnings(as.numeric(r$game_seconds_remaining %||% NA_real_)),
    score_differential     = suppressWarnings(as.numeric(r$score_differential     %||% 0)),
    roof     = as.character(r$roof     %||% "outdoors"),
    surface  = as.character(r$surface  %||% "grass"),
    wind     = suppressWarnings(as.numeric(r$wind %||% 0)),
    temp     = suppressWarnings(as.numeric(r$temp %||% 72)),
    spread_line = suppressWarnings(as.numeric(r$spread_line %||% 0)),
    total_line  = suppressWarnings(as.numeric(r$total_line  %||% 44)),
    posteam_timeouts_remaining = suppressWarnings(as.integer(r$posteam_timeouts_remaining %||% 3L)),
    defteam_timeouts_remaining = suppressWarnings(as.integer(r$defteam_timeouts_remaining %||% 3L))
  )
}

# --- Model loader (fixed: no self-recursion). Allow project override ----------
.load_models <- function(){
  if (exists("project_load_models", mode = "function")) {
    return(project_load_models())
  }
  list(
    fg = readRDS("data/models/fg_model.rds"),
    fd = readRDS("data/models/fd_model.rds"),
    wp = readRDS("data/models/wp_model.rds")
  )
}

# --- Simulator wrapper: unify to a single return shape ------------------------
# Expect a function `simulate_fourth_down_decision(gs, fg, fd, wp)` or an older
# variant `simulate_fourth_down_decision(gs, reload_models=FALSE)`.
.simulate_fd <- function(gs, models){
  if (exists("simulate_fourth_down_decision", mode = "function")) {
    # First, try explicit models signature
    out <- tryCatch(
      simulate_fourth_down_decision(gs, models$fg, models$fd, models$wp),
      error = function(e) NULL
    )
    if (!is.null(out)) return(out)

    # Fallback to reload_models signature
    out2 <- tryCatch(
      simulate_fourth_down_decision(gs, reload_models = FALSE),
      error = function(e) NULL
    )
    if (!is.null(out2)) {
      return(list(
        wp_go = out2$go_for_it_ev,
        wp_punt = out2$punt_wp,
        wp_fg = out2$field_goal_ev,
        punt_suppressed = isTRUE(out2$punt_suppressed)
      ))
    }
  }
  NULL
}

# --- Derive quarter clock string from game-seconds-remaining ------------------
# (bugfix) return quarter seconds, not total game seconds.
.sec_and_clock <- function(qtr, gsr){
  q <- suppressWarnings(as.integer(qtr)); s <- suppressWarnings(as.numeric(gsr))
  if (is.na(q) || is.na(s)) return(list(sec_left = NA_integer_, clock = NA_character_))
  q_seconds_left <- pmax(0, ifelse(q <= 4, s - pmax(0, (4 - q) * 900), s))
  mm <- floor(q_seconds_left / 60); ss <- round(q_seconds_left %% 60)
  list(sec_left = as.integer(q_seconds_left), clock = sprintf("%d:%02d", mm, ss))
}

# --- Evaluate ONE 4th-down row to the posting-policy schema -------------------
.eval_one_row <- function(r, models){
  sim <- .simulate_fd(.build_game_state(r), models)
  if (is.null(sim)) return(NULL)

  wp_go   <- suppressWarnings(as.numeric(sim$wp_go   %||% NA_real_))
  wp_punt <- suppressWarnings(as.numeric(sim$wp_punt %||% NA_real_))
  wp_fg   <- suppressWarnings(as.numeric(sim$wp_fg   %||% NA_real_))

  if (all(is.na(c(wp_go, wp_punt, wp_fg)))) return(NULL)

  best_idx <- names(c(go = wp_go, punt = wp_punt, fg = wp_fg))[which.max(c(wp_go, wp_punt, wp_fg))]
  sc <- .sec_and_clock(r$qtr, r$game_seconds_remaining)
  called <- .infer_called_action(r)

  off_score <- r$posteam_score %||% r$home_score %||% NA_real_
  def_score <- r$defteam_score %||% r$away_score %||% NA_real_

  tibble(
    game_id  = r$game_id,
    drive_id = r$drive_id %||% NA,
    play_id  = r$play_id  %||% NA,
    season   = r$season, week = r$week,
    off = r$posteam %||% NA_character_,
    def = r$defteam %||% NA_character_,
    off_score = off_score,
    def_score = def_score,
    margin = ifelse(!is.na(off_score) && !is.na(def_score), abs(off_score - def_score), NA_real_),
    qtr = r$qtr,
    sec_left = sc$sec_left,   # quarter seconds remaining
    clock = sc$clock,         # quarter clock string (MM:SS)
    down = 4L,
    ydstogo = r$ydstogo,
    yardline_100 = r$yardline_100,
    roof = r$roof, surface = r$surface, wind = r$wind, temp = r$temp,
    pre_wp  = suppressWarnings(as.numeric(r$wp %||% NA_real_)),
    wp_go   = wp_go, wp_punt = wp_punt, wp_fg = wp_fg,
    best_action = best_idx,
    called_action = called,
    punt_suppressed = isTRUE(sim$punt_suppressed)
  )
}

# --- Evaluate a FRAME of candidate 4th-down rows ------------------------------
.evaluate_frame <- function(df){
  if (!nrow(df)) return(tibble())
  df <- .apply_weather_defaults(df)
  models <- .load_models()
  rows <- purrr::map_dfr(seq_len(nrow(df)), function(i) .eval_one_row(df[i, , drop = FALSE], models))
  rows %>%
    filter(!is.na(.data$wp_go) | !is.na(.data$wp_punt) | !is.na(.data$wp_fg)) %>%
    tidyr::replace_na(list(called_action = NA_character_, pre_wp = NA_real_))
}

# ------------------------------ PUBLIC API -----------------------------------
# run_fd_live_once(fetcher = NULL, write_artifacts = TRUE, ...)
# - If `fetcher` is NULL, it will call a global `fetch_live_plays()` if present.
# - Always writes CSV + LOG into R/bots/live_csv/ when write_artifacts = TRUE.
run_fd_live_once <- function(fetcher = NULL, write_artifacts = TRUE, ...) {
  out_dir <- "R/bots/live_csv"
  if (isTRUE(write_artifacts)) .ensure_dir(out_dir)
  ts <- .now_stamp()
  csv_path <- file.path(out_dir, glue("plays_{ts}.csv"))
  log_path <- file.path(out_dir, glue("run_{ts}.log"))

  # 0) Resolve fetcher
  if (is.null(fetcher)) {
    if (exists("fetch_live_plays", mode = "function")) {
      fetcher <- fetch_live_plays
    } else {
      msg <- "[run_fd_live_once] No live fetcher found; returning empty tibble."
      if (isTRUE(write_artifacts)) {
        readr::write_lines(msg, log_path)
        readr::write_csv(tibble(), csv_path)
      }
      message(msg)
      return(tibble())
    }
  }

  # 1) Fetch latest plays
  df <- tryCatch(fetcher(...), error = function(e) {
    msg <- paste0("[run_fd_live_once] fetcher() error: ", conditionMessage(e))
    if (isTRUE(write_artifacts)) {
      readr::write_lines(msg, log_path)
      readr::write_csv(tibble(), csv_path)
    }
    message(msg)
    tibble()
  })

  # 2) Exit early if nothing usable
  if (!is.data.frame(df) || !nrow(df)) {
    if (isTRUE(write_artifacts)) {
      readr::write_lines("[run_fd_live_once] fetch returned 0 rows.", log_path)
      readr::write_csv(tibble(), csv_path)
    }
    return(tibble())
  }

  # 3) Ensure a `down` column exists (belt-and-suspenders for sources)
  if (!"down" %in% names(df)) df$down <- NA_integer_
  if (all(is.na(df$down)) && "text" %in% names(df)) {
    df$down <- ifelse(grepl("\\b4th\\b", tolower(df$text %||% "")), 4L, NA_integer_)
  }

  # 4) Keep only 4th downs, then evaluate
  df4 <- tryCatch(dplyr::filter(df, .data$down == 4), error = function(e) tibble())
  res <- if (nrow(df4)) .evaluate_frame(df4) else tibble()

  # 5) Artifacts
  if (isTRUE(write_artifacts)) {
    readr::write_csv(res, csv_path)
    readr::write_lines(glue("[run_fd_live_once] wrote {nrow(res)} rows."), log_path)
  }

  res
}
