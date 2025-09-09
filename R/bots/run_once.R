suppressPackageStartupMessages({
  library(dplyr); library(data.table)
  library(nflfastR); library(nflreadr)
  library(jsonlite); library(httr2)
  library(rtoot); library(glue)
})

# ------------------------------------------------------------------------------
# Sim + Policy (policy includes the "obvious" suppressor & wrong-only gating)
# ------------------------------------------------------------------------------
source("R/simulators/fourth_down/fourth_down_decision.R")
source("R/bots/posting_policy.R")   # uses: should_post_decision(), format_post(), append_cache(), mark_posted()

# ---------- Config via env vars ----------
BSKY_HANDLE   <- Sys.getenv("BSKY_HANDLE")
BSKY_APP_PASS <- Sys.getenv("BSKY_APP_PASSWORD")
MASTO_SERVER  <- Sys.getenv("MASTO_SERVER")
MASTO_TOKEN   <- Sys.getenv("MASTO_TOKEN")
DRY_RUN       <- identical(Sys.getenv("DRY_RUN"), "1")
DEDUP_FILE    <- "R/bots/.posted_keys.rds"   # moved from R/bot/ to R/bots/
NEAR_TOL_PCT  <- suppressWarnings(as.numeric(Sys.getenv("NEAR_TOL_PCT", "3")))

# ---------- Small utils ----------
`%||%` <- function(a,b) if (is.null(a) || is.na(a)) b else a
read_keys  <- function() if (file.exists(DEDUP_FILE)) readRDS(DEDUP_FILE) else character(0)
write_keys <- function(x) saveRDS(unique(x), DEDUP_FILE)

# ---------- Platform helpers ----------
bsky_login <- function(handle, app_password){
  req("https://bsky.social/xrpc/com.atproto.server.createSession") |>
    req_body_json(list(identifier = handle, password = app_password)) |>
    req_perform() |>
    resp_body_json()
}
bsky_post <- function(text, session){
  rec <- list(
    repo       = session$did,
    collection = "app.bsky.feed.post",
    record     = list(`$type`="app.bsky.feed.post", text=text,
                      createdAt=strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz="UTC"))
  )
  req("https://bsky.social/xrpc/com.atproto.repo.createRecord") |>
    req_headers(Authorization = paste("Bearer", session$accessJwt)) |>
    req_body_json(rec, auto_unbox = TRUE) |>
    req_perform() |>
    invisible(NULL)
}
masto_post <- function(text, server, token){
  post_toot(status = text, token = list(access_token = token, base_url = paste0("https://", server)))
}

# ---------- Helpers: build game state & human labels ----------
gs_from_row <- function(r){
  list(
    yardline_100 = r$yardline_100,
    ydstogo = r$ydstogo,
    down = r$down,
    game_seconds_remaining = r$game_seconds_remaining,
    score_differential = r$score_differential,
    roof = r$roof %||% "outdoors",
    surface = r$surface %||% "grass",
    wind = suppressWarnings(as.numeric(r$wind)),
    temp = suppressWarnings(as.numeric(r$temp)),
    spread_line = r$spread_line,
    total_line  = r$total_line,
    posteam_timeouts_remaining = r$posteam_timeouts_remaining %||% 3,
    defteam_timeouts_remaining  = r$defteam_timeouts_remaining  %||% 3
  )
}
yard_str <- function(y100){
  if (is.na(y100)) return("—")
  if (y100 > 50) paste0("Own ", 100 - y100) else paste0("Opp ", y100)
}
clock_str <- function(secs){
  paste0(secs %/% 60, ":", sprintf("%02d", secs %% 60))
}
pct <- function(x) ifelse(is.na(x), "—", paste0(round(100*as.numeric(x)), "%"))

score_line <- function(row){
  ht <- row$home_team; at <- row$away_team
  hs <- suppressWarnings(as.integer(row$home_score))
  as <- suppressWarnings(as.integer(row$away_score))
  if ((is.null(hs) || is.na(hs) || is.null(as) || is.na(as)) &&
      !is.null(row$posteam_score) && !is.null(row$defteam_score) &&
      !is.null(row$posteam) && !is.null(ht) && !is.null(at)) {
    if (row$posteam == ht) { hs <- as.integer(row$posteam_score); as <- as.integer(row$defteam_score) }
    else if (row$posteam == at) { hs <- as.integer(row$defteam_score); as <- as.integer(row$posteam_score) }
  }
  if (is.null(ht) || is.null(at) || is.na(hs) || is.na(as)) {
    return(paste0("Diff ", row$score_differential))
  }
  paste0(at, " ", as, " vs ", ht, " ", hs)
}

# ---------- Prefer policy formatter; fall back to quick text if needed ----------
fmt_post_quick <- function(row){
  format_post(row, revisionist = (!is.na(row$wp_gap) && row$best_action != row$called_action && row$wp_gap >= 0.05))
}

# ---------- Called action from PBP ----------
called_action_from_row <- function(r){
  if (isTRUE(r$field_goal_attempt == 1)) return("fg")
  if (isTRUE(r$punt == 1))               return("punt")
  if (isTRUE(r$rush == 1 || r$pass == 1 || r$qb_scramble == 1)) return("go")
  if (!is.null(r$play_type)) {
    if (grepl("field_goal", r$play_type, ignore.case=TRUE)) return("fg")
    if (grepl("punt", r$play_type, ignore.case=TRUE))       return("punt")
    if (grepl("pass|rush|scramble", r$play_type, ignore.case=TRUE)) return("go")
  }
  "go"
}

# ---------- Prime/standalone window info for policy ----------
game_meta_from_sched <- function(sched_row, todays_sched){
  standalone <- FALSE
  if (!is.null(sched_row$gameday) && !is.null(sched_row$gametime)) {
    same_kick <- todays_sched %>%
      filter(gameday == sched_row$gameday, gametime == sched_row$gametime)
    standalone <- nrow(same_kick) == 1
  }
  window <- if (!is.null(sched_row$weekday)) {
    wd <- tolower(sched_row$weekday)
    if (wd == "monday") "MNF" else if (wd == "thursday") "TNF" else if (wd == "saturday") "SAT" else NA_character_
  } else NA_character_
  list(standalone = standalone, window = window)
}

# ---------- Main: poll today's games once ----------
run_once <- function(){
  # Today's slate
  sched <- nflreadr::load_schedules(seasons = nflreadr::most_recent_season()) |>
    dplyr::filter(.data$gameday == as.character(Sys.Date()))
  if (!nrow(sched)) return(invisible(NULL))

  # Dedup cache
  posted <- read_keys()

  for (gid in sched$game_id) {
    # Live-ish PBP
    pbp <- tryCatch(nflfastR::fast_scraper(gid), error = function(e) NULL)
    if (is.null(pbp) || !nrow(pbp)) next

    setDT(pbp)

    # Evaluate ALL 4th downs (punts, FGs, and goes) to catch wrong calls of any type
    cand <- pbp[
      down == 4 & (
        # explicit tags
        field_goal_attempt == 1 |
        punt == 1 |
        rush == 1 | pass == 1 | qb_scramble == 1 |
        # fallback by play_type/desc if explicit flags are missing
        grepl("field_goal|punt|pass|rush|scramble", (play_type %||% ""), ignore.case = TRUE) |
        grepl("field goal|punt|pass|rush|scramble", (desc %||% ""),      ignore.case = TRUE)
      ) &
      # drop pure bookkeeping rows
      !(play_type %in% c("no_play","timeout","quarter_end","end_period","end_half","end_game")) &
      # keep rows unless an accepted penalty erased the snap entirely
      (is.na(penalty) | penalty == 0 | is.na(no_play) | no_play == 0)
    ]
    if (!nrow(cand)) next

    # Most recent 4th down
    row <- cand[.N]

    # Skip nonsensical states
    if (is.na(row$ydstogo) || is.na(row$yardline_100)) next

    # Robust dedup key
    pid <- if (!is.na(row$play_id)) row$play_id else paste0("row", nrow(pbp))
    key <- paste(row$game_id, pid, sep = "_")
    if (key %in% posted) next

    # Simulate
    gs  <- gs_from_row(row)
    sim <- simulate_fourth_down_decision(gs, reload_models = FALSE)

    # Map sim -> policy fields
    wp_go   <- sim$go_for_it_ev   %||% NA_real_
    wp_punt <- sim$punt_wp        %||% NA_real_
    wp_fg   <- sim$field_goal_ev  %||% NA_real_
    pre_wp  <- row$wp %||% row$posteam_wp %||% mean(c(wp_go, wp_punt, wp_fg), na.rm = TRUE)

    called <- called_action_from_row(row)
    opts <- c(go = wp_go, punt = wp_punt, fg = wp_fg)
    best_action <- names(opts)[which.max(opts)]

    pol_row <- list(
      season = row$season, week = row$week, game_id = row$game_id,
      drive_id = row$drive, play_id = row$play_id,
      off = row$posteam %||% row$pos_team,
      def = row$defteam,
      off_score = row$posteam_score %||% 0L, def_score = row$defteam_score %||% 0L,
      margin = abs((row$posteam_score %||% 0L) - (row$defteam_score %||% 0L)),
      qtr = row$qtr, sec_left = row$game_seconds_remaining, clock = clock_str(row$game_seconds_remaining),
      yardline_100 = row$yardline_100, down = row$down, ydstogo = row$ydstogo,
      roof = row$roof, surface = row$surface, wind = row$wind, temp = row$temp, precip = row$precip,
      pre_wp = pre_wp, wp_go = wp_go, wp_punt = wp_punt, wp_fg = wp_fg,
      best_action = best_action, called_action = called,
      notes = NA_character_
    )

    # Window meta for policy
    gm <- game_meta_from_sched(sched[sched$game_id == gid, ], sched)

    # Policy decision (contains obvious/must-go suppressor + thresholds)
    pol <- should_post_decision(pol_row, gm)

    # --- Always write the cache row, posted or not ---
    cache_file <- file.path(
      "data/cache", pol_row$season,
      sprintf("week_%02d", as.integer(pol_row$week)),
      sprintf("decisions_%s.csv", pol_row$game_id)
    )
    append_cache(pol_row, pol, cache_file)

    # Wrong-only: only post if the called action != best action
    wrong_call <- !is.na(pol_row$best_action) && !is.na(pol_row$called_action) &&
                  pol_row$best_action != pol_row$called_action

    if (isTRUE(pol$post) && isTRUE(wrong_call)) {
      # Build message (policy handles clear mistake vs neutral tone)
      text <- fmt_post_quick(c(pol_row, list(
        qtr_label = paste0("Q", pol_row$qtr),
        yardline  = sprintf("%s %d",
                            ifelse(pol_row$yardline_100 > 50, pol_row$off, pol_row$def),
                            100 - pol_row$yardline_100),
        score_off = pol_row$off_score,
        score_def = pol_row$def_score,
        wp_gap    = pol$wp_gap,
        wp_go     = pol_row$wp_go, wp_punt = pol_row$wp_punt, wp_fg = pol_row$wp_fg,
        pre_wp    = pol_row$pre_wp
      )))

      # Console preview
      message("[BOT] ", key, " -> ", substr(text, 1, 100), ifelse(nchar(text)>100,"...",""))

      if (DRY_RUN) {
        cat("\n--- DRY RUN (no posts) ---\n", text, "\n")
      } else {
        if (nzchar(BSKY_HANDLE) && nzchar(BSKY_APP_PASS)) {
          try({
            sess <- bsky_login(BSKY_HANDLE, BSKY_APP_PASS)
            bsky_post(text, sess)
          }, silent = TRUE)
        }
        if (nzchar(MASTO_SERVER) && nzchar(MASTO_TOKEN)) {
          try(masto_post(text, MASTO_SERVER, MASTO_TOKEN), silent = TRUE)
        }
      }

      # Update policy state for cooldown/rate limiting (kept for future use)
      mark_posted(pol_row$game_id, pol_row$drive_id)
    }

    # Dedup record (evaluated)
    posted <- c(posted, key)
    write_keys(posted)
  }

  invisible(NULL)
}

# If executed directly via Rscript (not sourced), run once immediately
if (sys.nframe() == 0) run_once()
