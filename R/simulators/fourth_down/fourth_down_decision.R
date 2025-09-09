# fourth_down_decision.R
# Self-contained simulation: loads .rds bundles internally and caches them.

source("R/core/fourth_down_helper.R")
suppressPackageStartupMessages(library(xgboost))

# ---- Hard-coded model paths ----
.FD_ENV <- new.env(parent = emptyenv())
.FD_ENV$FG_RDS     <- "R/models/field_goal/fg_model.rds"
.FD_ENV$FD_RDS     <- "R/models/fourth_down/fd_conversion_model.rds"  # fixed name
.FD_ENV$WP_RDS     <- "R/models/win_probability/wp_model.rds"
# optional categorical level maps (if you saved them)
.FD_ENV$FG_LEVELS  <- "R/models/field_goal/fg_levels.rds"
.FD_ENV$FD_LEVELS  <- "R/models/fourth_down/fd_levels.rds"

.load_bundles <- function(force = FALSE) {
  if (isTRUE(force) || is.null(.FD_ENV$fg_bundle)) {
    .FD_ENV$fg_bundle <- readRDS(.FD_ENV$FG_RDS)
    .FD_ENV$fd_bundle <- readRDS(.FD_ENV$FD_RDS)
    .FD_ENV$wp_bundle <- readRDS(.FD_ENV$WP_RDS)

    # load levels maps if available (optional)
    if (file.exists(.FD_ENV$FG_LEVELS)) .FD_ENV$fg_levels <- readRDS(.FD_ENV$FG_LEVELS)
    if (file.exists(.FD_ENV$FD_LEVELS)) .FD_ENV$fd_levels <- readRDS(.FD_ENV$FD_LEVELS)
  }
  invisible(NULL)
}

# Convert model WP (for team in possession) to OUR team WP.
# flipped = TRUE when possession switches to opponent.
for_us_wp <- function(prob, flipped) {
  prob <- as.numeric(prob)
  if (isTRUE(flipped)) 1 - prob else prob
}

# Make base DF without auto-factoring numerics
.make_base_df <- function(x) data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)

# Local prepare_state that tolerates missing columns and coerces types.
# If a levels_map is supplied, it will align factor levels to training.
.local_prepare_state <- function(state_df, model_features, levels_map = NULL, verbose = FALSE) {
  # 1) Apply categorical levels when supplied (exact mapping as training)
  if (!is.null(levels_map)) {
    for (nm in names(levels_map)) {
      if (!nm %in% names(state_df)) next
      lv <- levels_map[[nm]]
      state_df[[nm]] <- factor(as.character(state_df[[nm]]), levels = lv)
    }
  }

  # 2) Coerce char/factor -> integer codes (single-row safe)
  for (col in names(state_df)) {
    if (is.character(state_df[[col]]) || is.factor(state_df[[col]])) {
      state_df[[col]] <- as.integer(as.factor(state_df[[col]]))
    }
  }

  # 3) Add any missing model features as NA (xgboost can handle NA)
  missing <- setdiff(model_features, names(state_df))
  if (length(missing)) {
    for (m in missing) state_df[[m]] <- NA_real_
    if (isTRUE(verbose)) {
      message("[prepare_state] Added missing features: ", paste(missing, collapse = ", "))
    }
  }

  # 4) Reorder and drop extras to match model expectations
  state_df <- state_df[, model_features, drop = FALSE]

  # 5) Ensure all columns are numeric for xgb.DMatrix
  for (col in names(state_df)) {
    if (!is.numeric(state_df[[col]])) {
      state_df[[col]] <- as.numeric(state_df[[col]])
    }
  }

  state_df
}

# ----------------------------------------------
# Main simulation function (no model args needed)
# ----------------------------------------------
simulate_fourth_down_decision <- function(game_state, reload_models = FALSE) {
  .load_bundles(force = reload_models)

  fg_bundle <- .FD_ENV$fg_bundle
  fd_bundle <- .FD_ENV$fd_bundle
  wp_bundle <- .FD_ENV$wp_bundle

  # Feature lists + boosters
  fg_features <- fg_bundle$feature_names
  fd_features <- fd_bundle$feature_names
  wp_features <- wp_bundle$feature_names

  fg_boost <- fg_bundle$model
  fd_boost <- fd_bundle$model
  wp_boost <- wp_bundle$model

  # Optional level maps (may be NULL)
  fg_levels <- .FD_ENV$fg_levels
  fd_levels <- .FD_ENV$fd_levels

  # Base state
  base_state <- .make_base_df(game_state)

  # Base WP (we have the ball -> no flip)
  base_input <- .local_prepare_state(base_state, wp_features)  # WP levels not saved
  base_wp <- for_us_wp(predict(wp_boost, xgb.DMatrix(as.matrix(base_input))), flipped = FALSE)

  # ------------------------
  # Field Goal Outcomes
  # ------------------------
  fg_state <- .local_prepare_state(base_state, fg_features, levels_map = fg_levels)
  print(fg_state)
  fg_prob  <- as.numeric(predict(fg_boost, xgb.DMatrix(as.matrix(fg_state))))
  print(fg_prob)

  # Correct kick distance: yardline_100 + 17
  kick_distance <- base_state$yardline_100
  distance_penalty <- max(0, (kick_distance - 25) * 0.005)
  fg_prob <- max(0, min(1, fg_prob - distance_penalty))

  # FG Make (possession flips on kickoff -> flip = TRUE)
  fg_make_state <- base_state
  fg_make_state$score_differential <- -(fg_make_state$score_differential + 3)
  fg_make_state$yardline_100 <- 30
  fg_make_state$down <- 1
  fg_make_state$ydstogo <- 10
  tmp <- fg_make_state$posteam_timeouts_remaining
  fg_make_state$posteam_timeouts_remaining <- fg_make_state$defteam_timeouts_remaining
  fg_make_state$defteam_timeouts_remaining <- tmp
  fg_make_state$game_seconds_remaining <- fg_make_state$game_seconds_remaining - 5

  fg_make_input <- .local_prepare_state(fg_make_state, wp_features)
  fg_make_wp <- predict(wp_boost, xgb.DMatrix(as.matrix(fg_make_input)))


  # FG Miss (turnover on downs -> flip = TRUE)
  fg_miss_state <- base_state
  fg_miss_state$score_differential <- -(fg_miss_state$score_differential)
  fg_miss_state$yardline_100 <- (100 - fg_miss_state$yardline_100)
  fg_miss_state$down <- 1
  fg_miss_state$ydstogo <- 10
  tmp <- fg_miss_state$posteam_timeouts_remaining
  fg_miss_state$posteam_timeouts_remaining <- fg_miss_state$defteam_timeouts_remaining
  fg_miss_state$defteam_timeouts_remaining <- tmp
  fg_miss_state$game_seconds_remaining <- fg_miss_state$game_seconds_remaining - 5

  fg_miss_input <- .local_prepare_state(fg_miss_state, wp_features)
  fg_miss_wp <- 1- predict(wp_boost, xgb.DMatrix(as.matrix(fg_miss_input)))

  fg_ev <- fg_prob * fg_make_wp + (1 - fg_prob) * fg_miss_wp

  # ------------------------
  # Go For It Outcomes
  # ------------------------
  fd_state <- .local_prepare_state(base_state, fd_features, levels_map = fd_levels)
  fd_prob  <- as.numeric(predict(fd_boost, xgb.DMatrix(as.matrix(fd_state))))

  td_ev <- td_success_wp <- td_failure_wp <- td_prob <- NA_real_
  go_ev <- go_success_wp <- go_fail_wp   <- NA_real_

  if (base_state$yardline_100 != base_state$ydstogo) {
    # Convert success (keep ball -> no flip)
    go_success_state <- base_state
    go_success_state$yardline_100 <- base_state$yardline_100 - base_state$ydstogo - 1
    go_success_state$down <- 1
    go_success_state$ydstogo <- 10
    go_success_state$game_seconds_remaining <- go_success_state$game_seconds_remaining - 8

    go_success_input <- .local_prepare_state(go_success_state, wp_features)
    go_success_wp <- predict(wp_boost, xgb.DMatrix(as.matrix(go_success_input)))

    # Convert fail (opponent ball -> flip = TRUE)
    go_fail_state <- base_state
    go_fail_state$score_differential <- -go_fail_state$score_differential
    go_fail_state$yardline_100 <- 100 - go_fail_state$yardline_100
    go_fail_state$down <- 1
    go_fail_state$ydstogo <- 10
    tmp <- go_fail_state$posteam_timeouts_remaining
    go_fail_state$posteam_timeouts_remaining <- go_fail_state$defteam_timeouts_remaining
    go_fail_state$defteam_timeouts_remaining <- tmp
    go_fail_state$game_seconds_remaining <- go_fail_state$game_seconds_remaining - 8

    go_fail_input <- .local_prepare_state(go_fail_state, wp_features)
    go_fail_wp <- 1-predict(wp_boost, xgb.DMatrix(as.matrix(go_fail_input)))

    go_ev <- fd_prob * go_success_wp + (1 - fd_prob) * go_fail_wp

  } else {
    # Goal-to-go TD attempt
    td_state <- .local_prepare_state(base_state, fd_features, levels_map = fd_levels)
    td_prob  <- as.numeric(predict(fd_boost, xgb.DMatrix(as.matrix(td_state))))

    # TD success (kickoff -> flip = TRUE)
    td_success_state <- base_state
    td_success_state$score_differential <- -(td_success_state$score_differential + 7)
    td_success_state$yardline_100 <- 75
    td_success_state$down <- 1
    td_success_state$ydstogo <- 10
    tmp <- td_success_state$posteam_timeouts_remaining
    td_success_state$posteam_timeouts_remaining <- td_success_state$defteam_timeouts_remaining
    td_success_state$defteam_timeouts_remaining <- tmp
    td_success_state$game_seconds_remaining <- td_success_state$game_seconds_remaining - 6

    td_success_input <- .local_prepare_state(td_success_state, wp_features)
    td_success_wp <- predict(wp_boost, xgb.DMatrix(as.matrix(td_success_input)))

    # TD failure (opponent ball -> flip = TRUE)
    td_failure_state <- base_state
    td_failure_state$score_differential <- -td_failure_state$score_differential
    td_failure_state$yardline_100 <- 100 - base_state$yardline_100
    td_failure_state$down <- 1
    td_failure_state$ydstogo <- 10
    tmp <- td_failure_state$posteam_timeouts_remaining
    td_failure_state$posteam_timeouts_remaining <- td_failure_state$defteam_timeouts_remaining
    td_failure_state$defteam_timeouts_remaining <- tmp
    td_failure_state$game_seconds_remaining <- td_failure_state$game_seconds_remaining - 6

    td_failure_input <- .local_prepare_state(td_failure_state, wp_features)
    td_failure_wp <- predict(wp_boost, xgb.DMatrix(as.matrix(td_failure_input)))

    td_ev <- td_prob * td_success_wp + (1 - td_prob) * td_failure_wp
  }

  # ------------------------
  # Punt (opponent ball -> flip = TRUE)
  # ------------------------
  punt_state <- base_state
  punt_state$yardline_100 <- min(85, 100 + (45 - base_state$yardline_100))
  punt_state$score_differential <- -(punt_state$score_differential)
  punt_state$down <- 1
  punt_state$ydstogo <- 10
  tmp <- punt_state$posteam_timeouts_remaining
  punt_state$posteam_timeouts_remaining <- punt_state$defteam_timeouts_remaining
  punt_state$defteam_timeouts_remaining <- tmp
  punt_state$game_seconds_remaining <- punt_state$game_seconds_remaining - 10

  punt_input <- .local_prepare_state(punt_state, wp_features)
  punt_wp <- 1 - predict(wp_boost, xgb.DMatrix(as.matrix(punt_input)))

  # Deltas vs base
  delta_fg  <- if (is.na(fg_ev))  NA_real_ else fg_ev  - base_wp
  delta_go  <- if (is.na(go_ev))  NA_real_ else go_ev  - base_wp
  delta_td  <- if (is.na(td_ev))  NA_real_ else td_ev  - base_wp
  delta_pnt <- if (is.na(punt_wp)) NA_real_ else punt_wp - base_wp

  # Recommendation
  candidates <- c(FG = fg_ev, GO = go_ev, TD = td_ev, PUNT = punt_wp)
  candidates <- candidates[!is.na(candidates)]
  best_action <- names(which.max(candidates))
  best_wp <- unname(max(candidates))

  list(
    base_wp = base_wp,

    field_goal_ev = fg_ev,
    field_goal_make_wp = fg_make_wp,
    field_goal_miss_wp = fg_miss_wp,

    go_for_it_ev = go_ev,
    go_for_it_success_wp = go_success_wp,
    go_for_it_failure_wp = go_fail_wp,

    touchdown_ev = td_ev,
    touchdown_success_wp = td_success_wp,
    touchdown_failure_wp = td_failure_wp,

    punt_wp = punt_wp,

    # branch probabilities
    fg_prob = fg_prob,
    fd_prob = fd_prob,
    td_prob = td_prob,

    delta_fg = delta_fg,
    delta_go = delta_go,
    delta_td = delta_td,
    delta_punt = delta_pnt,

    recommendation = list(action = best_action, wp = best_wp)
  )
}

# ---- Pretty printer (with exact EV equations) -------------------------------

.num <- function(x, d = 3) ifelse(is.na(x), NA, round(as.numeric(x), d))
.pct <- function(x, d = 1) ifelse(is.na(x), NA, paste0(round(100*as.numeric(x), d), "%"))

print_fourth_down_summary <- function(result) {
  cat("\n================ FOURTH-DOWN DECISION SUMMARY ================\n")
  cat(sprintf("Base WP: %s\n", .pct(result$base_wp)))

  # FG
  fgp <- as.numeric(result$fg_prob)
  mk  <- as.numeric(result$field_goal_make_wp)
  ms  <- as.numeric(result$field_goal_miss_wp)
  fg_ev_calc <- fgp * mk + (1 - fgp) * ms
  cat("\n[FIELD GOAL]\n")
  cat(sprintf("  Make WP: %s | Miss WP: %s\n", .pct(mk), .pct(ms)))
  cat(sprintf("  FG EV  : %s  (Delta vs base: %s)\n",
              .pct(result$field_goal_ev), .pct(result$delta_fg)))
  cat(sprintf("    Equation: (%.3f * %.3f) + (1 - %.3f) * %.3f = %.3f\n",
              fgp, mk, fgp, ms, fg_ev_calc))

  # GO
  cat("\n[GO FOR IT]\n")
  if (!is.na(result$go_for_it_ev)) {
    fp  <- as.numeric(result$fd_prob)
    g_s <- as.numeric(result$go_for_it_success_wp)
    g_f <- as.numeric(result$go_for_it_failure_wp)
    go_ev_calc <- fp * g_s + (1 - fp) * g_f
    cat(sprintf("  Success WP: %s | Failure WP: %s\n", .pct(g_s), .pct(g_f)))
    cat(sprintf("  GO EV    : %s  (Delta vs base: %s)\n",
                .pct(result$go_for_it_ev), .pct(result$delta_go)))
    cat(sprintf("    Equation: (%.3f * %.3f) + (1 - %.3f) * %.3f = %.3f\n",
                fp, g_s, fp, g_f, go_ev_calc))
  } else {
    cat("  (Not applicable in goal-to-go branch)\n")
  }

  # TD
  cat("\n[TOUCHDOWN ATTEMPT]\n")
  if (!is.na(result$touchdown_ev)) {
    tp  <- as.numeric(result$td_prob)
    t_s <- as.numeric(result$touchdown_success_wp)
    t_f <- as.numeric(result$touchdown_failure_wp)
    td_ev_calc <- tp * t_s + (1 - tp) * t_f
    cat(sprintf("  Success WP: %s | Failure WP: %s\n", .pct(t_s), .pct(t_f)))
    cat(sprintf("  TD EV    : %s  (Delta vs base: %s)\n",
                .pct(result$touchdown_ev), .pct(result$delta_td)))
    cat(sprintf("    Equation: (%.3f * %.3f) + (1 - %.3f) * %.3f = %.3f\n",
                tp, t_s, tp, t_f, td_ev_calc))
  } else {
    cat("  (Not applicable unless yardline_100 == ydstogo)\n")
  }

  # Punt
  cat("\n[PUNT]\n")
  cat(sprintf("  Punt WP : %s  (Delta vs base: %s)\n",
              .pct(result$punt_wp), .pct(result$delta_punt)))

  # Recommendation
  rec <- result$recommendation
  cat("\n[RECOMMENDATION]\n")
  cat(sprintf("  %s  (WP: %s)\n", rec$action, .pct(rec$wp)))
  cat("================================================================\n\n")
}

simulate_and_print <- function(game_state, reload_models = FALSE) {
  res <- simulate_fourth_down_decision(game_state, reload_models = reload_models)
  print_fourth_down_summary(res)
  invisible(res)
}
