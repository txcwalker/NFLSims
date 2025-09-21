# ------------------------------------------------------------------------------
# Posting Policy & Cache Helpers for Fourth-Down Bot
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
  library(lubridate)
  library(readr)
})

# Utility: null-coalescing
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

# ------------------------------------------------------------------------------
# In-memory global state for cooldowns & rate-limiting (reset each R session)
# ------------------------------------------------------------------------------
.post_state <- new.env(parent = emptyenv())
.post_state$last_post_ts_by_game <- list()    # game_id -> POSIXct of last post
.post_state$last_post_drive_by_game <- list() # game_id -> last posted drive_id
.post_state$posts_last_hour_ts <- c()         # POSIXct vector of recent posts

# ------------------------------------------------------------------------------
# Prime/standalone window?
# ------------------------------------------------------------------------------
is_prime_window <- function(game_meta) {
  isTRUE(game_meta$standalone) ||
    (is.character(game_meta$window) &&
       game_meta$window %in% c("MNF","TNF","SNF","HOL","INTL","SAT"))
}

# ------------------------------------------------------------------------------
# Core policy evaluator
# ------------------------------------------------------------------------------
should_post_decision <- function(row, game_meta, now = Sys.time()) {
  # REQUIRED FIELDS in `row`:
  # game_id, drive_id, play_id
  # qtr, sec_left, clock
  # off, def, off_score, def_score, margin
  # ydstogo, yardline_100, down
  # pre_wp, wp_go, wp_punt, wp_fg (0–1 each)
  # best_action, called_action ("go","punt","fg")
  prime <- is_prime_window(game_meta)

  wp_vec <- c(go = row$wp_go, punt = row$wp_punt, fg = row$wp_fg)

  wp_best   <- suppressWarnings(max(wp_vec, na.rm = TRUE))
  wp_called <- wp_vec[[tolower(row$called_action)]]
  wp_gap    <- ifelse(is.na(wp_called), NA_real_, wp_best - wp_called)

  leverage <- wp_best - mean(wp_vec[!is.na(wp_vec)], na.rm = TRUE)

  one_score_2h <- (row$margin <= 8) && (row$qtr >= 3)
  clear_mistake <- (!is.na(wp_gap)) &&
                   (row$best_action != row$called_action) &&
                   (wp_gap >= 0.05)
  high_lev <- !is.na(row$pre_wp) &&
              row$pre_wp >= 0.35 && row$pre_wp <= 0.65 &&
              leverage >= 0.04

  # ---- Global "obvious situation" suppressor ----
  non_na <- wp_vec[!is.na(wp_vec)]
  spread <- if (length(non_na) >= 2) (max(non_na) - min(non_na)) else 0
  obvious_spread_block <- spread >= 0.30              # any options differ by ≥30 pts
  must_go_block <- !is.na(row$wp_punt) && (row$wp_punt < 0.10)  # "must-go" if punt <10%
  obvious_block <- obvious_spread_block || must_go_block

  # ---- Rate limits / blocks ----
  cd_secs <- if (prime) 45 else 60
  last_ts <- .post_state$last_post_ts_by_game[[row$game_id]]
  within_cd <- !is.null(last_ts) &&
               as.numeric(difftime(now, last_ts, units = "secs")) < cd_secs
  cooldown_block <- within_cd && !clear_mistake

  last_drive <- .post_state$last_post_drive_by_game[[row$game_id]]
  drive_block <- !is.null(last_drive) && identical(last_drive, row$drive_id)

  recent <- .post_state$posts_last_hour_ts[
    as.numeric(difftime(now, .post_state$posts_last_hour_ts, units = "mins")) <= 60
  ]
  global_block <- length(recent) >= 12

  blowout <- (row$margin >= 17) && !(row$qtr == 4 && row$sec_left <= 120)
  blowout_block <- blowout && !clear_mistake && (is.na(wp_gap) || wp_gap < 0.08)

  # ---- Eligibility gates by window ----
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

  # Apply global "obvious" suppressor
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
    # diagnostics
    obvious_block = obvious_block,
    obvious_spread_block = obvious_spread_block,
    must_go_block = must_go_block,
    # metrics
    wp_gap = wp_gap,
    leverage = leverage,
    prime = prime
  )
}

# ------------------------------------------------------------------------------
# Message formatter (PUNT omitted if suppressed/NA)
# ------------------------------------------------------------------------------
.pct <- function(x) ifelse(is.na(x), "", paste0(round(100*as.numeric(x)), "%"))
.yard_str <- function(y100) {
  if (is.na(y100)) return("Own ?")
  if (y100 > 50) paste0("Own ", 100 - y100) else paste0("Opp ", y100)
}

format_post <- function(row, revisionist = FALSE) {
  # quarter label and clock
  qlabel <- if (!is.null(row$qtr) && row$qtr >= 5) "OT" else paste0("Q", row$qtr %||% "?")
  if (!is.null(row$clock) && !is.na(row$clock)) {
    qclock <- row$clock
  } else {
    q <- as.integer(row$qtr %||% 1L)
    s <- as.integer(row$sec_left %||% 0L)
    # seconds remaining in current quarter
    q_left <- s - pmax(0L, (4L - pmin(q, 4L)) * 900L)
    q_left <- max(0L, min(900L, q_left))
    qclock <- sprintf("%d:%02d", q_left %/% 60L, q_left %% 60L)
  }

  # score string
  score_str <- if (!is.null(row$off_score) && !is.null(row$def_score) &&
                   !is.na(row$off_score) && !is.na(row$def_score) &&
                   !is.null(row$off) && !is.null(row$def)) {
    paste0(row$off, " ", row$off_score, " vs ", row$def, " ", row$def_score)
  } else {
    "Score N/A"
  }

  # All-scenarios line: include only available actions; omit PUNT when suppressed/NA
  parts <- character(0)
  if (!is.null(row$wp_fg)   && !is.na(row$wp_fg))   parts <- c(parts, paste0("FG ",  .pct(row$wp_fg)))
  if (!is.null(row$wp_go)   && !is.na(row$wp_go))   parts <- c(parts, paste0("GO ",  .pct(row$wp_go)))
  if (!isTRUE(row$punt_suppressed) && !is.na(row$wp_punt %||% NA_real_)) {
    parts <- c(parts, paste0("PUNT ", .pct(row$wp_punt)))
  }
  all_line <- if (length(parts)) paste(parts, collapse = "  ") else ""

  # recommended action & WP
  ba <- tolower(row$best_action %||% "")
  rec_act <- toupper(ba %||% "—")
  rec_wp  <- {
    key <- paste0("wp_", ba)
    .pct(if (!is.null(row[[key]]) && !is.na(row[[key]])) row[[key]] else row$pre_wp)
  }

  header <- paste0(
    "4th & ", row$ydstogo, " at ", .yard_str(row$yardline_100),
    " | ", score_str, " | ", qlabel, " ", qclock,
    if (!is.null(row$off)) paste0(" (", row$off, " ball)") else ""
  )

  lines <- c(
    header,
    paste0(if (revisionist) "Model Says: " else "Model Recommendation: ",
           rec_act, " (WP ", rec_wp, ")")
  )
  if (nzchar(all_line)) lines <- c(lines, paste0("All Scenarios: ", all_line))

  paste0(paste(lines, collapse = "\n"), "\n#nfl #4thDown #analytics")
}

# ------------------------------------------------------------------------------
# CSV cache appender
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

  is_new <- !file.exists(cache_path)
  readr::write_csv(out, cache_path, append = !is_new)
}

# ------------------------------------------------------------------------------
# Mark a successful post (cooldown / rate limits)
# ------------------------------------------------------------------------------
mark_posted <- function(game_id, drive_id) {
  .post_state$last_post_ts_by_game[[game_id]] <- Sys.time()
  .post_state$last_post_drive_by_game[[game_id]] <- drive_id
  .post_state$posts_last_hour_ts <- c(.post_state$posts_last_hour_ts, Sys.time())
}

# ------------------------------------------------------------------------------
# Main handler (optional helper; uses a project-specific eval_fourth_down())
# ------------------------------------------------------------------------------
handle_fourth_down <- function(gs, models, game_meta) {
  res <- eval_fourth_down(gs, models)  # project-specific

  row <- c(gs, res) %>% as.list()
  row$qtr_label <- paste0("Q", row$qtr)
  row$yardline <- sprintf("%s %d",
                          ifelse(row$yardline_100 > 50, row$off, row$def),
                          100 - row$yardline_100)
  row$score_off <- row$off_score
  row$score_def <- row$def_score
  row$margin   <- abs(row$off_score - row$def_score)

  policy <- should_post_decision(row, game_meta)

  cache_file <- file.path(
    "data/cache", row$season,
    sprintf("week_%02d", as.integer(row$week)),
    sprintf("decisions_%s.csv", row$game_id)
  )
  append_cache(row, policy, cache_file)

  if (isTRUE(policy$post)) {
    is_revisionist <- (!is.na(policy$wp_gap) &&
                       row$best_action != row$called_action &&
                       policy$wp_gap >= 0.05)
    msg <- format_post(row, revisionist = is_revisionist)
    # post_to_social(msg)  # implement in your project
    mark_posted(row$game_id, row$drive_id)
  }

  invisible(policy)
}
