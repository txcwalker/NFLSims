# ------------------------------------------------------------------------------
# Posting Policy & Cache Helpers for Fourth-Down Bot
# ------------------------------------------------------------------------------
# This module decides whether to post a 4th-down evaluation, formats the text,
# and appends every evaluated 4th down (posted or not) to a CSV cache.
#
# EXPECTED USAGE:
# - Source this file from your main loop (e.g., run_bot.R).
# - For each 4th-down play, call `handle_fourth_down(gs, models, game_meta)`.
#   - `gs` is your game_state list/data.frame containing play context.
#   - `models` is your bundle of models; used by your own eval function.
#   - `game_meta` has window info (e.g., MNF/TNF/SNF/standalone flags).
#
# FILE LAYOUT:
# - Per-game CSV: data/cache/{season}/week_{wk}/decisions_{game_id}.csv
# - You can add a weekly rollup later if desired.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
  library(lubridate)
  library(readr)
})

# ------------------------------------------------------------------------------
# In-memory global state for cooldowns & rate-limiting
# - Reset on each fresh R session (intended for a single run of the bot).
# ------------------------------------------------------------------------------
.post_state <- new.env(parent = emptyenv())
.post_state$last_post_ts_by_game <- list()   # map: game_id -> POSIXct of last post
.post_state$last_post_drive_by_game <- list()# map: game_id -> last posted drive_id
.post_state$posts_last_hour_ts <- c()        # vector of POSIXct of all posts (global)

# ------------------------------------------------------------------------------
# Helper: Identify if this game is in a prime/standalone window.
# Expects `game_meta` to include a boolean `standalone` or a `window` code.
# ------------------------------------------------------------------------------
is_prime_window <- function(game_meta) {
  # Treats any standalone game as prime; else checks common prime window labels.
  isTRUE(game_meta$standalone) ||
    (is.character(game_meta$window) &&
       game_meta$window %in% c("MNF","TNF","SNF","HOL","INTL","SAT"))
}

# ------------------------------------------------------------------------------
# Core policy evaluator:
# - Decides if a post should be made and returns reasoning + rate-limit flags.
# - `row` must contain fields listed below (see comment).
# ------------------------------------------------------------------------------
should_post_decision <- function(row, game_meta, now = Sys.time()) {
  # REQUIRED FIELDS in `row` (list or 1-row data.frame):
  #   game_id, drive_id, play_id
  #   qtr (1–4/+OT), sec_left (int seconds left in game), clock (e.g., "12:34")
  #   off, def, off_score, def_score, margin (abs score diff)
  #   ydstogo, yardline_100, down
  #   pre_wp (0–1), wp_go, wp_punt, wp_fg (0–1 each)
  #   best_action, called_action  ("go","punt","fg")
  #
  #   Optional env/meta: roof, surface, wind, temp, precip, notes

  prime <- is_prime_window(game_meta)

  # Collect WPs by action
  wp_vec <- c(go = row$wp_go, punt = row$wp_punt, fg = row$wp_fg)

  # Best-case WP and called-action WP
  wp_best   <- suppressWarnings(max(wp_vec, na.rm = TRUE))
  wp_called <- wp_vec[[tolower(row$called_action)]]
  wp_gap    <- ifelse(is.na(wp_called), NA_real_, wp_best - wp_called)

  # "Leverage" = best action’s WP minus average of available actions
  leverage <- wp_best - mean(wp_vec[!is.na(wp_vec)], na.rm = TRUE)

  # Sunday posting gates (unchanged)
  one_score_2h <- (row$margin <= 8) && (row$qtr >= 3)
  clear_mistake <- (!is.na(wp_gap)) &&
                   (row$best_action != row$called_action) &&
                   (wp_gap >= 0.05)
  high_lev <- !is.na(row$pre_wp) &&
              row$pre_wp >= 0.35 && row$pre_wp <= 0.65 &&
              leverage >= 0.04

  # ---------------------------------------------------------------------------
  # NEW: Global "obvious situation" suppressor (overrides everything)
  # 1) If the spread between ANY options is ≥ 0.30 WP, don't post (too obvious)
  #    (implemented as max(option) - min(option) among non-NA options)
  # 2) "Must-go": if punt WP < 0.10, don't post (obvious go situation)
  # ---------------------------------------------------------------------------
  non_na <- wp_vec[!is.na(wp_vec)]
  spread <- if (length(non_na) >= 2) (max(non_na) - min(non_na)) else 0
  obvious_spread_block <- spread >= 0.30

  must_go_block <- !is.na(row$wp_punt) && (row$wp_punt < 0.10)

  obvious_block <- obvious_spread_block || must_go_block
  # ---------------------------------------------------------------------------

  # Per-game cooldown (shorter for prime windows)
  cd_secs <- if (prime) 45 else 60
  last_ts <- .post_state$last_post_ts_by_game[[row$game_id]]
  within_cd <- !is.null(last_ts) &&
               as.numeric(difftime(now, last_ts, units = "secs")) < cd_secs
  cooldown_block <- within_cd && !clear_mistake  # skip cooldown for clear mistakes

  # Per-drive throttle: at most one post per drive
  last_drive <- .post_state$last_post_drive_by_game[[row$game_id]]
  drive_block <- !is.null(last_drive) && identical(last_drive, row$drive_id)

  # Global rate-limit: soft cap of 12 posts per hour across all games
  recent <- .post_state$posts_last_hour_ts[
    as.numeric(difftime(now, .post_state$posts_last_hour_ts, units = "mins")) <= 60
  ]
  global_block <- length(recent) >= 12

  # Blowout guard (unchanged)
  blowout <- (row$margin >= 17) && !(row$qtr == 4 && row$sec_left <= 120)
  blowout_block <- blowout && !clear_mistake && (is.na(wp_gap) || wp_gap < 0.08)

  # Eligibility by window (unchanged), but will be overridden by obvious_block below
  reason <- NA_character_
  eligible <-
    if (prime) {
      reason <- "prime_all"; TRUE
    } else if (one_score_2h) {
      reason <- "one_score_2H"; TRUE
    } else if (clear_mistake) {
      reason <- "clear_mistake"; TRUE
    } else if (high_lev) {
      reason <- "high_leverage"; TRUE
    } else {
      FALSE
    }

  # FINAL: apply global "obvious" suppressor before other blocks
  eligible <- eligible && !obvious_block

  post_it <- eligible && !cooldown_block && !drive_block && !global_block && !blowout_block

  list(
    post = post_it,
    reason = ifelse(post_it, reason, NA_character_),
    cooldown_skipped = eligible && within_cd && clear_mistake,
    cooldown_block = cooldown_block,
    drive_block = drive_block,
    global_block = global_block,
    blowout_block = blowout_block,
    # new diagnostics:
    obvious_block = obvious_block,
    obvious_spread_block = obvious_spread_block,
    must_go_block = must_go_block,
    # metrics:
    wp_gap = wp_gap,
    leverage = leverage,
    prime = prime
  )
}


# ------------------------------------------------------------------------------
# Message formatter:
# - Creates text for live posts and revisionist posts (after the play).
# ------------------------------------------------------------------------------
format_post <- function(row, revisionist = FALSE) {
  # Helper to format percentages (e.g., 0.123 -> "12.3")
  pct <- function(x) sprintf("%.1f", 100 * x)

  # Human-readable score string at time of play.
  score_str <- sprintf("%s %d – %s %d", row$off, row$score_off, row$def, row$score_def)

  # If revisionist flag is set, use a look-back template.
  if (revisionist) {
    return(sprintf(
      "Looking back: 4th & %d at %s (%s %s). %s chose %s, model preferred %s (+%s%% WP). Score then: %s.",
      row$ydstogo, row$yardline, row$qtr_label, row$clock,
      row$off, toupper(row$called_action), toupper(row$best_action),
      pct(row$wp_gap), score_str
    ))
  }

  # If it was a clear mistake in real time, highlight the better action.
  if (!is.na(row$wp_gap) && row$best_action != row$called_action && row$wp_gap >= 0.05) {
    return(sprintf(
      "%s faced 4th & %d at %s (%s %s). They %s, model says %s (+%s%% WP).\nGo %s%% | Punt %s%% | FG %s%% | Pre-WP %s%%.",
      row$off, row$ydstogo, row$yardline, row$qtr_label, row$clock,
      toupper(row$called_action), toupper(row$best_action), pct(row$wp_gap),
      pct(row$wp_go), pct(row$wp_punt), pct(row$wp_fg), pct(row$pre_wp)
    ))
  }

  # Otherwise, neutral summary of the model’s recommendation.
  # “Next best” delta is best - second best among actions.
  sorted <- sort(c(row$wp_go, row$wp_punt, row$wp_fg), decreasing = TRUE, na.last = NA)
  next_gap <- if (length(sorted) >= 2) (sorted[1] - sorted[2]) else NA_real_

  sprintf(
    "4th & %d at %s | %s vs %s, %s %s.\nModel: %s (+%s%% WP vs next). Go %s%% | Punt %s%% | FG %s%% | Pre-WP %s%%.",
    row$ydstogo, row$yardline, row$off, row$def, row$qtr_label, row$clock,
    toupper(row$best_action), pct(next_gap),
    pct(row$wp_go), pct(row$wp_punt), pct(row$wp_fg), pct(row$pre_wp)
  )
}

# ------------------------------------------------------------------------------
# CSV cache appender:
# - Writes one row for every evaluated 4th down (regardless of posting).
# - Creates directories as needed.
# ------------------------------------------------------------------------------
append_cache <- function(row, policy, cache_path) {
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)

  out <- data.frame(
    season = row$season,
    week = row$week,
    game_id = row$game_id,
    drive_id = row$drive_id,
    play_id = row$play_id,
    post_time_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    posted_flag = policy$post,
    posted_reason = policy$reason,
    qtr = row$qtr,
    clock = row$clock,
    sec_left = row$sec_left,
    off = row$off,
    def = row$def,
    score_off = row$score_off,
    score_def = row$score_def,
    margin = row$margin,
    one_score_2h = (row$margin <= 8 && row$qtr >= 3),
    yardline_100 = row$yardline_100,
    down = row$down,
    ydstogo = row$ydstogo,
    roof = row$roof %||% NA,
    surface = row$surface %||% NA,
    wind = row$wind %||% NA,
    temp = row$temp %||% NA,
    precip = row$precip %||% NA,
    pre_wp = row$pre_wp,
    wp_go = row$wp_go,
    wp_punt = row$wp_punt,
    wp_fg = row$wp_fg,
    best_action = row$best_action,
    called_action = row$called_action,
    wp_gap = policy$wp_gap,
    leverage = policy$leverage,
    prime_window = policy$prime,
    cooldown_skipped = policy$cooldown_skipped,
    global_blocked = policy$global_block,
    notes = row$notes %||% NA_character_,
    stringsAsFactors = FALSE
  )

  # Append if file exists; create otherwise.
  is_new <- !file.exists(cache_path)
  readr::write_csv(out, cache_path, append = !is_new)
}

# ------------------------------------------------------------------------------
# Mark a successful post in in-memory state for cooldown and rate limiting.
# ------------------------------------------------------------------------------
mark_posted <- function(game_id, drive_id) {
  .post_state$last_post_ts_by_game[[game_id]] <- Sys.time()
  .post_state$last_post_drive_by_game[[game_id]] <- drive_id
  .post_state$posts_last_hour_ts <- c(.post_state$posts_last_hour_ts, Sys.time())
}

# ------------------------------------------------------------------------------
# Main handler to be called for each 4th down:
# - Expects you to have an `eval_fourth_down(gs, models)` function elsewhere
#   that returns WP by action plus a recommended action.
# - Decides whether to post, formats the message, writes cache, and (optionally)
#   calls your social poster.
# ------------------------------------------------------------------------------
handle_fourth_down <- function(gs, models, game_meta) {
  # Your own evaluator: must return a named list including:
  #   pre_wp, wp_go, wp_punt, wp_fg (0–1 each)
  #   best_action, called_action  ("go", "punt", or "fg")
  #   Any extra fields you want to carry along (optional).
  res <- eval_fourth_down(gs, models)

  # Merge the input `gs` and model results into a single row list.
  row <- c(gs, res) %>% as.list()

  # Human-readable labels used in formatted messages:
  row$qtr_label <- paste0("Q", row$qtr)
  row$yardline <- sprintf(
    "%s %d",
    ifelse(row$yardline_100 > 50, row$off, row$def),
    100 - row$yardline_100
  )
  row$score_off <- row$off_score
  row$score_def <- row$def_score
  row$margin   <- abs(row$off_score - row$def_score)

  # Evaluate policy & blocks.
  policy <- should_post_decision(row, game_meta)

  # Build cache file path and write the record.
  cache_file <- file.path(
    "data/cache", row$season,
    sprintf("week_%02d", as.integer(row$week)),
    sprintf("decisions_%s.csv", row$game_id)
  )
  append_cache(row, policy, cache_file)

  # If we should post, format the message and call your poster.
  if (isTRUE(policy$post)) {
    is_revisionist <- (!is.na(policy$wp_gap) &&
                       row$best_action != row$called_action &&
                       policy$wp_gap >= 0.05)

    msg <- format_post(row, revisionist = is_revisionist)

    # TODO: implement these in your project:
    # post_to_bluesky(msg) or post_to_mastodon(msg) or post_to_x(msg)
    # post_to_social(msg)

    # Update in-memory cooldown & rate-limits.
    mark_posted(row$game_id, row$drive_id)
  }

  invisible(policy)  # return policy object for logging/testing if desired
}

# ------------------------------------------------------------------------------
# Utility: "null-coalescing" operator for simple defaults.
# ------------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a
