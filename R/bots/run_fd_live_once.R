# R/bots/run_fd_live_once.R
# -----------------------------------------------------------------------------
# LIVE ONLY: fetch current plays, evaluate 4th downs via your models/sim,
# and return rows ready for should_post_decision() + post_everywhere().
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tibble); library(rlang)
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
    fg = readRDS("R/models/field_goal/fg_model.rds"),
    fd = readRDS("R/models/fourth_down/fd_model.rds"),
    wp = readRDS("R/models/win_probability/wp_model.rds")
  )
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
  second_wp <- max(wp_vec[names(wp_vec) != best_idx], na.rm = TRUE)
  margin    <- ifelse(is.finite(best_wp - second_wp), best_wp - second_wp, NA_real_)

  tibble(
    game_id  = r$game_id, season = r$season, week = r$week, qtr = r$qtr,
    game_seconds_remaining = r$game_seconds_remaining,
    posteam = r$posteam, defteam = r$defteam,
    yardline_100 = r$yardline_100, ydstogo = r$ydstogo,
    pre_wp = suppressWarnings(as.numeric(r$wp %||% NA_real_)),  # if present in your feed
    wp_go = wp_go, wp_punt = wp_punt, wp_fg = wp_fg,
    best_action = best_idx,
    margin = margin,
    called_action = .infer_called_action(r)
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
    tidyr::replace_na(list(called_action = NA_character_, pre_wp = NA_real_, margin = NA_real_))
}

# ------------------------------ PUBLIC API -----------------------------------
# Live-only entry point. `fetcher` must return a data.frame of the latest plays.
# It can be your NFL API fetcher. We filter to 4th downs and evaluate.
run_fd_live_once <- function(fetcher = NULL, ...) {
  if (is.null(fetcher)) {
    if (!exists("fetch_live_plays", mode = "function")) {
      stop("No live fetcher found. Provide `fetcher=` or define `fetch_live_plays()`.")
    }
    fetcher <- fetch_live_plays
  }
  df <- fetcher(...)
  df <- tryCatch(df %>% dplyr::filter(.data$down == 4), error = function(e) tibble())
  .evaluate_frame(df)
}
