suppressPackageStartupMessages({
  library(dplyr); library(data.table)
  library(nflfastR); library(nflreadr)
  library(jsonlite); library(httr2)
  library(rtoot); library(glue)
})

# --------------------------------------------------------------------------
# Sim + Policy (policy includes the "obvious" suppressor & wrong-only gating)
# --------------------------------------------------------------------------
source("R/simulators/fourth_down/fourth_down_decision.R")
source("R/bots/posting_policy.R")   # should_post_decision(), format_post(), append_cache(), mark_posted()

# ---------- Config via env vars ----------
BSKY_HANDLE   <- Sys.getenv("BSKY_HANDLE")
BSKY_APP_PASS <- Sys.getenv("BSKY_APP_PASSWORD")
MASTO_SERVER  <- Sys.getenv("MASTO_SERVER")
MASTO_TOKEN   <- Sys.getenv("MASTO_TOKEN")
DRY_RUN       <- identical(Sys.getenv("DRY_RUN"), "1")
DEDUP_FILE    <- "R/bots/.posted_keys.rds"
NEAR_TOL_PCT  <- suppressWarnings(as.numeric(Sys.getenv("NEAR_TOL_PCT", "3")))

# ---------- Small utils ----------
`%||%` <- function(a,b) if (is.null(a) || is.na(a)) b else a
read_keys  <- function() if (file.exists(DEDUP_FILE)) readRDS(DEDUP_FILE) else character(0)
write_keys <- function(x) saveRDS(unique(x), DEDUP_FILE)

# Derive *quarter* clock from game_seconds_remaining when pbp$clock is missing
qclock_from_gsr <- function(qtr, gsr){
  q <- as.integer(qtr %||% 1L)
  s <- as.integer(gsr %||% 0L)
  secs <- s - 900L * pmax(0L, (4L - pmin(q, 4L)))
  secs <- max(0L, min(900L, secs))
  sprintf("%d:%02d", secs %/% 60L, secs %% 60L)
}

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

# ---------- Helpers: build game state ----------
gs_from_row <- function(r){
  safe_num <- function(x) suppressWarnings(as.numeric(x %||% NA_real_))
  list(
    yardline_100 = safe_num(r$yardline_100),
    ydstogo = safe_num(r$ydstogo),
    down = r$down,
    game_seconds_remaining = safe_num(r$game_seconds_remaining),
    score_differential = safe_num(r$score_differential),
    roof = r$roof %||% "outdoors",
    surface = r$surface %||% "grass",
    wind = safe_num(r$wind),
    temp = safe_num(r$temp),
    spread_line = safe_num(r$spread_line),
    total_line  = safe_num(r$total_line),
    posteam_timeouts_remaining = r$posteam_timeouts_remaining %||% 3,
    defteam_timeouts_remaining  = r$defteam_timeouts_remaining  %||% 3
  )
}

# ---------- Called action from PBP (schema-agnostic) ----------
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

# ---------- Prime/standalone window info for policy ----------
game_meta_from_sched <- function(sched_row, todays_sched){
  standalone <- FALSE
  if (!is.null(sched_row$gameday) && !is.null(sched_row$gametime)) {
    same_kick <- todays_sched %>% filter(gameday == sched_row$gameday, gametime == sched_row$gametime)
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
  sched <- nflreadr::load_schedules(seasons = nflreadr::most_recent_season()) |>
    dplyr::filter(.data$gameday == as.character(Sys.Date()))
  if (!nrow(sched)) return(invisible(NULL))

  posted <- read_keys()

  for (gid in sched$game_id) {
    pbp <- tryCatch(nflfastR::fast_scraper(gid), error = function(e) NULL)
    if (is.null(pbp) || !nrow(pbp)) next
    setDT(pbp)

    # ---------- Schema-agnostic helper tags ----------
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

    # ---------- 4th-down candidates ----------
    cand <- pbp[
      down == 4 & (
        .fg_tag | .punt_tag | .go_tag |
        grepl("field[_ ]?goal|punt|pass|rush|scramble", .ptype, ignore.case = TRUE) |
        grepl("field goal|punt|pass|rush|scramble", .desc,  ignore.case = TRUE)
      ) &
      !(.ptype %in% c("no_play","timeout","quarter_end","end_period","end_half","end_game")) &
      (is.na(.pen) | .pen == 0 | !.no_play_flag)
    ]
    if (!nrow(cand)) next

    # Most recent 4th down
    row <- cand[.N]
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

    # Better clock (prefer pbp clock, else derive quarter clock)
    row_clock <- row$clock %||% qclock_from_gsr(row$qtr, row$game_seconds_remaining)

    pol_row <- list(
      season = row$season, week = row$week, game_id = row$game_id,
      drive_id = row$drive, play_id = row$play_id,
      off = row$posteam %||% row$pos_team,
      def = row$defteam,
      off_score = row$posteam_score %||% 0L, def_score = row$defteam_score %||% 0L,
      margin = abs((row$posteam_score %||% 0L) - (row$defteam_score %||% 0L)),
      qtr = row$qtr, sec_left = row$game_seconds_remaining, clock = row_clock,
      yardline_100 = row$yardline_100, down = row$down, ydstogo = row$ydstogo,
      roof = row$roof, surface = row$surface, wind = row$wind, temp = row$temp, precip = row$precip,
      pre_wp = pre_wp, wp_go = wp_go, wp_punt = wp_punt, wp_fg = wp_fg,
      best_action = best_action, called_action = called,
      punt_suppressed = isTRUE(sim$punt_suppressed),   # <-- pass suppress flag
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
      # Use policy formatter (ASCII-only; omits PUNT when suppressed/NA)
      revisionist_flag <- (!is.na(pol$wp_gap) &&
                           pol_row$best_action != pol_row$called_action &&
                           pol$wp_gap >= 0.05)
      text <- format_post(pol_row, revisionist = revisionist_flag)
      # If punt is suppressed or NA, remove the PUNT chunk from the "All Scenarios" line.
      if (isTRUE(sim$punt_suppressed) || is.na(pol_row$wp_punt)) {
        text <- sub("(All Scenarios:[^\n]*?)\\s+PUNT\\s+[^\\s]+", "\\1", text, perl = TRUE)
      }

      # Normalize dashes so Windows consoles don't show â€” and friends
      text <- gsub("\u2014|\u2013|â€”|â€“", "-", text)
      text <- enc2utf8(text)

      # Console preview
      if (DRY_RUN) {
        cat("\n--- PREVIEW ---\n", text, "\nChars: ", nchar(text), "\n", sep = "")
      } else {
        if (nzchar(BSKY_HANDLE) && nzchar(BSKY_APP_PASS)) {
          try({ sess <- bsky_login(BSKY_HANDLE, BSKY_APP_PASS); bsky_post(text, sess) }, silent = TRUE)
        }
        if (nzchar(MASTO_SERVER) && nzchar(MASTO_TOKEN)) {
          try(masto_post(text, MASTO_SERVER, MASTO_TOKEN), silent = TRUE)
        }
      }

      # Update cooldown / rate limits
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
