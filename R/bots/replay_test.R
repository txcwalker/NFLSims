# R/bots/replay_test.R
suppressPackageStartupMessages({
  library(dplyr); library(data.table); library(nflfastR); library(nflreadr); library(readr)
  library(glue)
})

source("R/simulators/fourth_down/fourth_down_decision.R")
source("R/bots/posting_policy.R")  # should_post_decision(), format_post()

`%||%` <- function(a,b) if (is.null(a) || is.na(a)) b else a

# Convert model HOME-WP to OFFENSE-WP using row context (posteam vs home_team)
wp_offense <- function(home_wp, row){
  if (is.null(home_wp) || is.na(home_wp)) return(NA_real_)
  offense_is_home <- NA
  if (!is.null(row$posteam) && !is.null(row$home_team)) {
    offense_is_home <- identical(as.character(row$posteam), as.character(row$home_team))
  } else if (!is.null(row$posteam_is_home)) {
    offense_is_home <- as.logical(row$posteam_is_home)
  }
  if (!is.na(offense_is_home) && !offense_is_home) {
    return(1 - as.numeric(home_wp))
  }
  as.numeric(home_wp)
}

# Derive *quarter* clock from game_seconds_remaining when pbp$clock is missing
qclock_from_gsr <- function(qtr, gsr){
  q <- as.integer(qtr %||% 1L)
  s <- as.integer(gsr %||% 0L)
  secs <- s - 900L * pmax(0L, (4L - pmin(q, 4L)))
  secs <- max(0L, min(900L, secs))
  sprintf("%d:%02d", secs %/% 60L, secs %% 60L)
}

# Robust called-action inference from a PBP row
called_action_from_row <- function(r){
  if (!is.null(r$field_goal_attempt) && isTRUE(r$field_goal_attempt == 1)) return("fg")
  if (!is.null(r$field_goal_result) && !is.na(r$field_goal_result)) return("fg")
  if (!is.null(r$punt) && isTRUE(r$punt == 1)) return("punt")
  if (!is.null(r$punt_attempt) && isTRUE(r$punt_attempt == 1)) return("punt")
  if (!is.null(r$rush) && isTRUE(r$rush == 1)) return("go")
  if (!is.null(r$pass) && isTRUE(r$pass == 1)) return("go")
  if (!is.null(r$qb_scramble) && isTRUE(r$qb_scramble == 1)) return("go")
  pt <- r$play_type %||% ""
  de <- r$desc %||% r$play_description %||% ""
  if (grepl("field[_ ]?goal", pt, ignore.case=TRUE) || grepl("field goal", de, ignore.case=TRUE)) return("fg")
  if (grepl("punt", pt, ignore.case=TRUE) || grepl(" punt", de, ignore.case=TRUE)) return("punt")
  if (grepl("pass|rush|scramble", pt, ignore.case=TRUE) || grepl("pass|rush|scramble", de, ignore.case=TRUE)) return("go")
  "go"
}

# Neutral meta for replays
game_meta_from_sched <- function(sched_row, todays_sched){
  list(standalone = TRUE, window = "REPLAY")
}

# --- Debug helper: dump the exact inputs/outputs for one row
trace_play <- function(row, reload_models = TRUE, verbose = TRUE, offense_perspective = TRUE){
  gs <- list(
    yardline_100 = row$yardline_100,
    ydstogo = row$ydstogo,
    down = row$down,
    game_seconds_remaining = row$game_seconds_remaining,
    score_differential = row$score_differential,
    roof = row$roof %||% "outdoors",
    surface = row$surface %||% "grass",
    wind = suppressWarnings(as.numeric(row$wind)),
    temp = suppressWarnings(as.numeric(row$temp)),
    spread_line = row$spread_line,
    total_line  = row$total_line,
    posteam_timeouts_remaining = row$posteam_timeouts_remaining %||% 3,
    defteam_timeouts_remaining  = row$defteam_timeouts_remaining  %||% 3
  )
  cat("\n--- GS to simulator ---\n"); print(gs)

  sim <- simulate_fourth_down_decision(gs, reload_models = reload_models, verbose = verbose)

  cat("\n--- RAW sim outputs ---\n")
  out <- list(
    base_wp = sim$base_wp,
    fg_prob = sim$fg_prob,
    fg_make_wp = sim$field_goal_make_wp,
    fg_miss_wp = sim$field_goal_miss_wp,
    fg_ev = sim$field_goal_ev,
    fd_prob = sim$fd_prob,
    go_success_wp = sim$go_for_it_success_wp,
    go_fail_wp = sim$go_for_it_failure_wp,
    go_ev = sim$go_for_it_ev,
    punt_wp = sim$punt_wp,
    punt_suppressed = sim$punt_suppressed
  )
  print(lapply(out, function(x) if (is.numeric(x)) round(x, 3) else x))

  pre_wp <- if (offense_perspective) wp_offense(sim$base_wp, row) else sim$base_wp
  cat(sprintf("\npre_wp (offense_perspective=%s): %s\n",
              offense_perspective,
              ifelse(is.na(pre_wp), "NA", sprintf("%.3f", pre_wp))))
  invisible(sim)
}

# -------------------------
# Replay a single game_id
# -------------------------
replay_game <- function(game_id,
                        out_dir = "data/test_out",
                        post_mode = c("none","mastodon"),
                        mastodon_visibility = c("private","direct","unlisted","public"),
                        reload_models = TRUE,             # default TRUE to match one-off tests
                        verbose = FALSE,                  # pass-through to simulator
                        offense_perspective = TRUE) {     # flip base_wp to offense if posteam != home
  post_mode <- match.arg(post_mode)
  mastodon_visibility <- match.arg(mastodon_visibility)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  pbp <- tryCatch(nflfastR::fast_scraper(game_id), error = function(e) NULL)
  if (is.null(pbp) || !nrow(pbp)) stop("No PBP for game_id: ", game_id)
  setDT(pbp)

  # -------- Schema-agnostic helper tags --------
  has <- function(n) n %in% names(pbp)
  pbp[, .ptype := if (has("play_type")) play_type else "" ]
  pbp[, .desc  := if (has("desc")) desc else if (has("play_description")) play_description else "" ]
  pbp[, .fg_tag := if (has("field_goal_attempt")) field_goal_attempt == 1
                    else if (has("field_goal_result")) !is.na(field_goal_result)
                    else grepl("field[_ ]?goal", .ptype, ignore.case=TRUE) ]
  pbp[, .punt_tag := if (has("punt")) punt == 1
                      else if (has("punt_attempt")) punt_attempt == 1
                      else grepl("punt", .ptype, ignore.case=TRUE) ]
  pbp[, .go_tag := (if (has("rush")) rush == 1 else FALSE) |
                   (if (has("pass")) pass == 1 else FALSE) |
                   (if (has("qb_scramble")) qb_scramble == 1 else FALSE) ]
  pbp[, .pen := if (has("penalty")) as.integer(penalty) else NA_integer_ ]
  pbp[, .no_play_flag := if (has("no_play")) no_play == 1 else FALSE ]

  # Candidates: any 4th down where a decision/action is evident
  cand <- pbp[
    down == 4 & (
      .fg_tag | .punt_tag | .go_tag |
      grepl("field[_ ]?goal|punt|pass|rush|scramble", .ptype, ignore.case = TRUE) |
      grepl("field goal|punt|pass|rush|scramble", .desc,  ignore.case = TRUE)
    ) &
    !(.ptype %in% c("no_play","timeout","quarter_end","end_period","end_half","end_game")) &
    (is.na(.pen) | .pen == 0 | !.no_play_flag)
  ]
  if (!nrow(cand)) stop("No 4th-down candidates in game: ", game_id)

  # Schedules (meta unused beyond neutral flags)
  sched <- nflreadr::load_schedules(seasons = unique(pbp$season))
  gm <- game_meta_from_sched(sched[sched$game_id == game_id, ], sched)

  rows <- vector("list", nrow(cand))
  posts <- list()

  for (i in seq_len(nrow(cand))) {
    row <- cand[i]

    # (Optional) trace a specific row for debugging
    if (isTRUE(verbose) && i == 1L) trace_play(row, reload_models = reload_models,
                                              verbose = TRUE, offense_perspective = offense_perspective)

    # Build simulator input from the PBP row (minimal state used by the sim)
    gs <- list(
      yardline_100 = row$yardline_100,
      ydstogo = row$ydstogo,
      down = row$down,
      game_seconds_remaining = row$game_seconds_remaining,
      score_differential = row$score_differential,
      roof = row$roof %||% "outdoors",
      surface = row$surface %||% "grass",
      wind = suppressWarnings(as.numeric(row$wind)),
      temp = suppressWarnings(as.numeric(row$temp)),
      spread_line = row$spread_line,
      total_line  = row$total_line,
      posteam_timeouts_remaining = row$posteam_timeouts_remaining %||% 3,
      defteam_timeouts_remaining  = row$defteam_timeouts_remaining  %||% 3
    )

    sim <- simulate_fourth_down_decision(gs, reload_models = reload_models, verbose = verbose)

    wp_go   <- sim$go_for_it_ev   %||% NA_real_
    wp_punt <- sim$punt_wp        %||% NA_real_
    wp_fg   <- sim$field_goal_ev  %||% NA_real_

    # pre-snap WP: offense perspective (or raw if offense_perspective = FALSE)
    pre_wp <- if (offense_perspective) wp_offense(sim$base_wp, row) else sim$base_wp
    if (is.na(pre_wp)) {
      pre_wp <- mean(c(wp_go, wp_punt, wp_fg), na.rm = TRUE)
      if (is.nan(pre_wp)) pre_wp <- NA_real_
    }

    # NA-safe best action
    opts <- c(go = wp_go, punt = wp_punt, fg = wp_fg)
    if (all(is.na(opts))) {
      best_action <- NA_character_
    } else {
      opts_rank <- opts
      opts_rank[is.na(opts_rank)] <- -Inf
      best_action <- names(which.max(opts_rank))
    }

    called_action <- called_action_from_row(row)

    # Prefer pbp clock; otherwise derive quarter clock
    row_clock <- row$clock %||% qclock_from_gsr(row$qtr, row$game_seconds_remaining)

    pol_row <- list(
      season = row$season, week = row$week, game_id = row$game_id,
      drive_id = row$drive, play_id = row$play_id,
      off = row$posteam %||% row$pos_team, def = row$defteam,
      off_score = row$posteam_score %||% 0L, def_score = row$defteam_score %||% 0L,
      margin = abs((row$posteam_score %||% 0L) - (row$defteam_score %||% 0L)),
      qtr = row$qtr, sec_left = row$game_seconds_remaining, clock = row_clock,
      yardline_100 = row$yardline_100, down = row$down, ydstogo = row$ydstogo,
      roof = row$roof, surface = row$surface, wind = row$wind, temp = row$temp, precip = row$precip,
      pre_wp = pre_wp, wp_go = wp_go, wp_punt = wp_punt, wp_fg = wp_fg,
      best_action = best_action, called_action = called_action,
      punt_suppressed = isTRUE(sim$punt_suppressed)
    )

    pol <- should_post_decision(pol_row, gm)

    wrong_call <- !is.na(best_action) && !is.na(called_action) && best_action != called_action
    would_post <- isTRUE(pol$post) && wrong_call

    # Build the would-post message (only if would_post == TRUE)
    post_text <- ""
    if (would_post) {
      msg <- format_post(pol_row, revisionist = (!is.na(pol$wp_gap) && wrong_call && pol$wp_gap >= 0.05))
      if (isTRUE(pol_row$punt_suppressed) || is.na(pol_row$wp_punt)) {
        msg <- sub("(All Scenarios:[^\n]*?)\\s+PUNT\\s+\\S+", "\\1", msg, perl = TRUE)
      }
      msg <- gsub("\u2014|\u2013|â€”|â€“", "-", msg) # normalize dashes for Windows consoles
      post_text <- enc2utf8(msg)
      posts[[length(posts)+1]] <- post_text
    }

    score_line <- paste0(pol_row$off, " ", pol_row$off_score, " vs ", pol_row$def, " ", pol_row$def_score)

    rows[[i]] <- data.frame(
      season = pol_row$season, week = pol_row$week, game_id = pol_row$game_id,
      play_id = pol_row$play_id, qtr = pol_row$qtr, clock = pol_row$clock,
      off = pol_row$off, def = pol_row$def,
      score_off = pol_row$off_score, score_def = pol_row$def_score,
      score_line = score_line,
      down = pol_row$down, ydstogo = pol_row$ydstogo, yardline_100 = pol_row$yardline_100,
      called_action = pol_row$called_action, best_action = pol_row$best_action,
      wp_go = wp_go, wp_punt = wp_punt, wp_fg = wp_fg, pre_wp = pre_wp,
      wp_gap = pol$wp_gap, leverage = pol$leverage, reason = pol$reason,
      obvious_block = pol$obvious_block %||% FALSE,
      must_go_block = pol$must_go_block %||% FALSE,
      would_post = would_post,
      post_text = post_text,
      stringsAsFactors = FALSE
    )
  }

  report <- dplyr::bind_rows(rows)

  out_csv <- file.path(out_dir, sprintf("replay_%s.csv", game_id))
  readr::write_csv(report, out_csv)

  out_txt <- file.path(out_dir, sprintf("replay_%s_posts.txt", game_id))
  if (length(posts)) writeLines(unlist(posts), out_txt) else writeLines("<<no posts>>", out_txt)

  message("[REPLAY] game=", game_id,
          " | 4th downs=", nrow(report),
          " | would-post=", sum(report$would_post, na.rm=TRUE),
          " | CSV=", out_csv)

  invisible(report)
}

# -------------------------
# Convenience: whole week
# -------------------------
replay_week <- function(season, week, ..., reload_models = TRUE, verbose = FALSE, offense_perspective = TRUE) {
  sched <- nflreadr::load_schedules(seasons = season) %>% dplyr::filter(week == !!week)
  lapply(sched$game_id, function(gid) replay_game(gid, ..., reload_models = reload_models,
                                                  verbose = verbose, offense_perspective = offense_perspective))
}
