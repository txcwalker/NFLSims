# R/bots/run_fd_live_once.R
# -----------------------------------------------------------------------------
# LIVE ONLY: fetch current plays, evaluate 4th downs via your models/sim,
# and return rows ready for should_post_decision() + format_post() + post_everywhere().
# This version standardizes the row schema to match posting_policy.R requirements.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tibble); library(rlang); library(tidyr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a)==0 || (is.atomic(a) && all(is.na(a)))) b else a

# Weather/context defaults so the sim never crashes
.apply_weather_defaults <- function(df){
  df %>%
    mutate(
      roof    = as.character(roof %||% NA_character_),
      surface = as.character(surface %||% NA_character_),
      wind    = suppressWarnings(as.numeric(wind %||% NA_real_)),
      temp    = suppressWarnings(as.numeric(temp %||% NA_real_)),
      wind    = ifelse(is.na(wind), 0, wind),
      temp    = ifelse(is.na(temp), 72, temp)
    )
}

# Infer what the team actually did on 4th down (best-effort)
.infer_called_action <- function(r){
  pt <- tolower(as.character(r$play_type %||% ""))
  if (grepl("punt", pt)) return("punt")
  if (grepl("field_goal|kick", pt)) return("fg")
  if (grepl("run|pass|qb_kneel|qb_spike|sack|scramble", pt)) return("go")
  NA_character_
}

# Build the minimal game_state your sim needs
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

# Load models (use your project loader if it exists)
.load_models <- function(){
  if (exists(".load_models", mode = "function")) return(.load_models())
  list(
    fg = readRDS("data/models/fg_model.rds"),
    fd = readRDS("data/models/fd_model.rds"),
    wp = readRDS("data/models/wp_model.rds")
  )
}

# derive sec_left and a human clock string for the current quarter
.sec_and_clock <- function(qtr, gsr){
  q <- suppressWarnings(as.integer(qtr)); s <- suppressWarnings(as.numeric(gsr))
  if (is.na(q) || is.na(s)) return(list(sec_left = NA_integer_, clock = NA_character_))
  # For regulation, sec_left == game_seconds_remaining (Q1-4). For OT we'll just format the visible clock.
  q_seconds_left <- pmax(0L, ifelse(q <= 4, s - pmax(0, (4 - q) * 900), s))
  mm <- floor(q_seconds_left / 60); ss <- round(q_seconds_left %% 60)
  list(sec_left = as.integer(s), clock = sprintf("%d:%02d", mm, ss))
}

# Evaluate ONE 4th-down row to the policy schema
.eval_one_row <- function(r, models){
  # Prefer your project’s native row evaluator if present
  if (exists(".evaluate_row", mode = "function")) {
    out <- tryCatch(.evaluate_row(r), error = function(e) NULL)
    if (!is.null(out)) return(out)
  }

  # Otherwise drive via your simulator
  if (!exists("simulate_fourth_down_decision", mode = "function")) return(NULL)

  gs  <- .build_game_state(r)
  sim <- tryCatch(
    simulate_fourth_down_decision(gs, models$fg, models$fd, models$wp),
    error = function(e) NULL
  )
  if (is.null(sim)) return(NULL)

  wp_go   <- suppressWarnings(as.numeric(sim$wp_go   %||% NA_real_))
  wp_punt <- suppressWarnings(as.numeric(sim$wp_punt %||% NA_real_))
  wp_fg   <- suppressWarnings(as.numeric(sim$wp_fg   %||% NA_real_))

  wp_vec <- c(go = wp_go, punt = wp_punt, fg = wp_fg)
  if (all(is.na(wp_vec))) return(NULL)
  best_idx  <- names(wp_vec)[which.max(wp_vec)]
  best_wp   <- max(wp_vec, na.rm = TRUE)

  sc <- .sec_and_clock(r$qtr, r$game_seconds_remaining)
  called <- .infer_called_action(r)

  # Scores: try posteam/defteam scores first; fall back to home/away if available
  off_score <- r$posteam_score %||% r$home_score %||% NA_real_
  def_score <- r$defteam_score %||% r$away_score %||% NA_real_

  tibble(
    # identity
    game_id  = r$game_id,
    drive_id = r$drive_id %||% NA,
    play_id  = r$play_id  %||% NA,
    season   = r$season, week = r$week,

    # teams + scores (policy expects off/def fields)
    off = r$posteam %||% NA_character_,
    def = r$defteam %||% NA_character_,
    off_score = off_score,
    def_score = def_score,
    margin = ifelse(!is.na(off_score) && !is.na(def_score), abs(off_score - def_score), NA_real_),

    # situation
    qtr = r$qtr,
    sec_left = sc$sec_left,
    clock = sc$clock,
    down = 4L,
    ydstogo = r$ydstogo,
    yardline_100 = r$yardline_100,

    # environmental (debug convenience)
    roof = r$roof, surface = r$surface, wind = r$wind, temp = r$temp,

    # model fields used by formatter/policy
    pre_wp  = suppressWarnings(as.numeric(r$wp %||% NA_real_)),
    wp_go   = wp_go, wp_punt = wp_punt, wp_fg = wp_fg,
    best_action = best_idx,
    called_action = called,

    # posting niceties
    punt_suppressed = is.na(wp_punt)
  )
}

# Evaluate a FRAME of current plays (expects down==4 rows)
.evaluate_frame <- function(df){
  if (!nrow(df)) return(tibble())
  df <- .apply_weather_defaults(df)
  models <- .load_models()
  rows <- purrr::map_dfr(seq_len(nrow(df)), function(i){
    r <- df[i, , drop = FALSE]
    .eval_one_row(r, models)
  })
  rows %>%
    filter(!is.na(.data$wp_go) | !is.na(.data$wp_punt) | !is.na(.data$wp_fg)) %>%
    tidyr::replace_na(list(called_action = NA_character_, pre_wp = NA_real_))
}

# ------------------------------ PUBLIC API -----------------------------------
# Live-only entry point. `fetcher` must return a data.frame of the latest plays.
# It can be your NFL API fetcher. We filter to 4th downs and evaluate.
run_fd_live_once <- function(fetcher = NULL, ...) {
  # 1) Pick a fetcher (prefer the global fetch_live_plays if none passed)
  if (is.null(fetcher)) {
    if (exists("fetch_live_plays", mode = "function")) {
      fetcher <- fetch_live_plays
    } else {
      # Warn only once per session to avoid log spam
      if (!isTRUE(getOption("fd_live.fetcher_warned", FALSE))) {
        message("[run_fd_live_once] No live fetcher found; returning empty tibble.")
        options(fd_live.fetcher_warned = TRUE)
      }
      return(tibble::tibble())
    }
  }

  # 2) Call the fetcher (catch errors once, return empty tibble on failure)
  df <- tryCatch(
    fetcher(...),
    error = function(e) {
      if (!isTRUE(getOption("fd_live.fetch_error_warned", FALSE))) {
        message("[run_fd_live_once] fetcher() error: ", conditionMessage(e))
        options(fd_live.fetch_error_warned = TRUE)
      }
      tibble::tibble()
    }
  )

  # 3) Fast exits if nothing usable came back
  if (!is.data.frame(df) || !nrow(df)) return(tibble::tibble())
  if (!"down" %in% names(df)) df$down <- NA_integer_

  # 4) Keep only 4th downs; guard against filter errors
  df <- tryCatch(
    dplyr::filter(df, .data$down == 4),
    error = function(e) tibble::tibble()
  )
  if (!nrow(df)) return(tibble::tibble())

  # 5) Evaluate rows to policy schema
  .evaluate_frame(df)
}
