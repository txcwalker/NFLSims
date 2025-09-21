# File: R/reports/run_fd_backfill.R
# -----------------------------------------------------------------------------
# Fourth Down Bot — Backfill (root‑relative paths, down==4 only)
# - Weather defaults only (temp=72, wind=0, surface="grass")
# - Saves to data/fd_backfill/<season>/<week>/<game_id>.csv
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(data.table); library(readr)
  library(glue); library(purrr); library(tidyr)
  library(nflfastR)
})

# --- Relative to project ROOT -------------------------------------------------
source("R/simulators/fourth_down/fourth_down_decision.R")  # simulate_fourth_down_decision()

# --- Output locations (root-relative) ----------------------------------------
BACKFILL_ROOT <- "data/fd_backfill"   # per requirement: data/fd_backfill/<season>/<week>/<file>

# ---- safe accessors & helpers -----------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
getv   <- function(r, nm, default = NA) { val <- r[[nm]]; if (is.null(val)) default else val }
is_one <- function(x) isTRUE(x == 1)
nzchr  <- function(x) is.character(x) && length(x) == 1 && nzchar(x)
.ensure_dir <- function(path) { if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE) }

# Apply simple weather defaults to a pbp data.frame (no external imputers)
.apply_weather_defaults <- function(df) {
  df %>% dplyr::mutate(
    wind    = dplyr::if_else(is.na(wind), 0, wind),
    temp    = dplyr::if_else(is.na(temp), 72, temp),
    surface = dplyr::coalesce(surface, "grass")
  )
}

# Map nflfastR row -> game_state expected by simulate_fourth_down_decision()
# NOTE: per your guidance, model does NOT use team ids; omit posteam/defteam/home_team
.row_to_game_state <- function(r) {
  list(
    yardline_100               = as.numeric(getv(r, "yardline_100")),
    ydstogo                    = as.numeric(getv(r, "ydstogo")),
    down                       = as.integer(getv(r, "down")),
    qtr                        = as.integer(getv(r, "qtr")),
    game_seconds_remaining     = as.numeric(getv(r, "game_seconds_remaining")),
    score_differential         = as.numeric(getv(r, "score_differential")),
    roof                       = as.character(getv(r, "roof", NA_character_)),
    surface                    = as.character(getv(r, "surface", NA_character_)),
    wind                       = suppressWarnings(as.numeric(getv(r, "wind"))),
    temp                       = suppressWarnings(as.numeric(getv(r, "temp"))),
    spread_line                = suppressWarnings(as.numeric(getv(r, "spread_line"))),
    total_line                 = suppressWarnings(as.numeric(getv(r, "total_line"))),
    posteam_timeouts_remaining = as.integer(getv(r, "posteam_timeouts_remaining")),
    defteam_timeouts_remaining = as.integer(getv(r, "defteam_timeouts_remaining"))
  )
}

# Derive called action from PBP (robust to missing columns)
.called_action <- function(r){
  pt <- tolower(as.character(getv(r, "play_type", "")))
  if (is_one(getv(r, "punt")) || pt %in% c("punt")) return("punt")
  if (!is.na(getv(r, "field_goal_result")) || pt %in% c("field_goal")) return("fg")
  if (is_one(getv(r, "pass")) || is_one(getv(r, "rush")) || pt %in% c("run","rush","pass","qb_kneel","qb_spike")) return("go")
  NA_character_
}

# Clock string (mm:ss), tolerant to different schemas
.clock_str <- function(r){
  clk <- getv(r, "clock"); if (nzchr(clk)) return(clk)
  qsr <- suppressWarnings(as.integer(getv(r, "quarter_seconds_remaining")))
  if (!is.na(qsr)) return(sprintf("%d:%02d", qsr %/% 60L, qsr %% 60L))
  gsr <- suppressWarnings(as.integer(getv(r, "game_seconds_remaining")))
  q   <- suppressWarnings(as.integer(getv(r, "qtr", 1L)))
  if (!is.na(gsr) && !is.na(q)) {
    per <- 900L
    within_q <- gsr - max(0L, (4L - min(q, 4L)) * per)
    within_q <- max(0L, min(per, within_q))
    return(sprintf("%d:%02d", within_q %/% 60L, within_q %% 60L))
  }
  NA_character_
}

# Evaluate a single play row -> tidy output row (GUARDS down==4)
.evaluate_row <- function(r){
  if (as.integer(getv(r, "down")) != 4L) return(NULL)

  gs  <- .row_to_game_state(r)
  sim <- simulate_fourth_down_decision(gs, reload_models = FALSE, verbose = FALSE)

  best <- tolower(sim$recommendation$action %||% NA_character_)
  best <- dplyr::case_when(
    best == "td" ~ "go",
    best %in% c("go","fg","punt") ~ best,
    TRUE ~ best
  )

  tibble::tibble(
    season          = getv(r, "season"),
    week            = getv(r, "week"),
    game_id         = getv(r, "game_id"),
    play_id         = getv(r, "play_id"),
    qtr             = getv(r, "qtr"),
    clock           = .clock_str(r),
    sec_left        = getv(r, "game_seconds_remaining"),
    off             = getv(r, "posteam"),
    def             = getv(r, "defteam"),
    off_score       = getv(r, "posteam_score"),
    def_score       = getv(r, "defteam_score"),
    margin          = abs(getv(r, "score_differential")),
    yardline_100    = getv(r, "yardline_100"),
    down            = getv(r, "down"),
    ydstogo         = getv(r, "ydstogo"),
    roof            = getv(r, "roof", NA_character_),
    surface         = getv(r, "surface", NA_character_),
    wind            = getv(r, "wind"),
    temp            = getv(r, "temp"),
    spread_line     = getv(r, "spread_line"),
    total_line      = getv(r, "total_line"),
    pre_wp          = sim$base_wp,
    wp_fg           = sim$field_goal_ev,
    wp_go           = sim$go_for_it_ev %||% sim$touchdown_ev,
    wp_punt         = sim$punt_wp,
    best_action     = best,
    called_action   = .called_action(r),
    punt_suppressed = isTRUE(sim$punt_suppressed),
    fg_suppressed   = isTRUE(sim$fg_suppressed)
  )
}

# Write/append per-game CSV under data/fd_backfill/<season>/<week>/<game_id>.csv
.write_game_csv <- function(row_df){
  season_chr <- as.character(row_df$season[[1]])
  week_chr   <- as.character(row_df$week[[1]])
  gdir <- file.path(BACKFILL_ROOT, season_chr, week_chr)
  .ensure_dir(gdir)
  csv_path <- file.path(gdir, glue("{row_df$game_id[[1]]}.csv"))
  if (!file.exists(csv_path)) readr::write_csv(row_df, csv_path) else readr::write_csv(row_df, csv_path, append = TRUE)
  csv_path
}

# -----------------------------------------------------------------------------
# PUBLIC: Backfill runner (down==4 enforced; fixed weather defaults applied)
# -----------------------------------------------------------------------------
run_fd_backfill <- function(season, week = NULL, games = NULL) {
  pbp <- nflfastR::load_pbp(season)
  if (!is.null(week))  pbp <- dplyr::filter(pbp, week %in% !!week)
  if (!is.null(games)) pbp <- dplyr::filter(pbp, game_id %in% !!games)
  if (!nrow(pbp)) return(invisible(NULL))

  # Apply hard weather defaults (temp=72, wind=0, surface="grass")
  pbp <- .apply_weather_defaults(pbp)

  # Filter to legitimate 4th downs up-front (extra guards inside .evaluate_row)
  fds <- pbp %>%
    dplyr::filter(
      down == 4,
      is.na(penalty) | penalty == 0,
      !isTRUE(two_point_attempt),
      !isTRUE(qb_kneel)
    ) %>%
    dplyr::arrange(season, week, game_id, qtr, dplyr::desc(game_seconds_remaining))

  if (!nrow(fds)) return(invisible(NULL))

  rows <- purrr::map_dfr(seq_len(nrow(fds)), function(i){
    r <- fds[i, , drop = FALSE]
    out <- .evaluate_row(r)
    if (!is.null(out)) .write_game_csv(out)
    out
  })

  # Also save a week-level aggregate if a single week was requested
  if (!is.null(week)) {
    week_chr <- paste(sort(unique(week)), collapse = "-")
    wdir <- file.path(BACKFILL_ROOT, as.character(unique(rows$season)[1]), as.character(week_chr))
    .ensure_dir(wdir)
    wk_csv <- file.path(wdir, glue("_week.csv"))
    readr::write_csv(rows, wk_csv)
  }

  invisible(rows)
}

# --- Usage (from project root) -----------------------------------------------
# source("R/reports/run_fd_backfill.R")
# run_fd_backfill(season = 2025, week = 2)
