# R/live/espn_adapter.R
# ------------------------------------------------------------------------------
# ESPN (unofficial) adapter: fetches scoreboard/summary/pbp and normalizes plays
# to a stable schema for the 4th-down bot. ALWAYS returns a `down` column.
# Includes light retry/backoff, one %||% helper, and a small stateful dedup.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(dplyr); library(purrr)
  library(stringr); library(tibble); library(lubridate)
})

# Single, project-wide null/empty coalescer
`%||%` <- function(a,b) if (is.null(a)||length(a)==0||(is.atomic(a)&&all(is.na(a)))) b else a

# --- HTTP JSON with light retry/backoff --------------------------------------
.espm_json <- function(url, flatten = TRUE){
  ua <- httr::user_agent("fd-bot/espn-adapter/0.5")
  for (i in 1:3) {
    resp <- try(httr::GET(url, ua, timeout(10)), silent = TRUE)
    if (inherits(resp, "try-error")) { Sys.sleep(runif(1, 0.2, 0.8)); next }
    if (httr::http_error(resp)) {
      if (i == 3) httr::stop_for_status(resp) else { Sys.sleep(runif(1, 0.2, 0.8)); next }
    }
    txt <- httr::content(resp, "text", encoding = "UTF-8")
    return(jsonlite::fromJSON(txt, flatten = flatten))
  }
  stop("ESPN request failed after retries.")
}

# --- Time & yardline helpers --------------------------------------------------
.parse_clock_secs <- function(display){
  if (is.null(display) || is.na(display) || !nzchar(display)) return(0L)
  mm <- suppressWarnings(as.integer(sub(":.*$", "", display)))
  ss <- suppressWarnings(as.integer(sub("^.*:", "", display)))
  mm*60L + ss
}

.game_seconds_remaining <- function(period_number, clock_display){
  sec_left_qtr <- .parse_clock_secs(clock_display)
  if (is.na(period_number)) return(NA_integer_)
  if (period_number <= 4L) (4L - period_number) * 900L + sec_left_qtr else sec_left_qtr
}

.parse_possession_text <- function(txt){
  if (is.null(txt) || is.na(txt) || !nzchar(txt)) return(list(team = NA_character_, yard = NA_integer_))
  parts <- str_split(txt, "\\s+", n = 2)[[1]]
  if (length(parts) < 2) return(list(team = NA_character_, yard = NA_integer_))
  yd <- gsub("[^0-9]", "", parts[2])
  list(team = toupper(parts[1]), yard = suppressWarnings(as.integer(yd)))
}

.compute_yardline_100 <- function(posteam, mark_team, yard){
  if (is.na(yard)) return(NA_real_)
  if (isTRUE(toupper(posteam) == toupper(mark_team))) 100 - yard else yard
}

# --- Public: ids/map ----------------------------------------------------------
espn_active_event_ids <- function(league = "nfl") {
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/%s/scoreboard", league)
  json <- .espm_json(url, flatten = FALSE)
  purrr::map_chr(json$events %||% list(), "id")
}

espn_event <- function(event_id) {
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event=%s", event_id)
  .espm_json(url, flatten = FALSE)
}

espn_list_events <- function() {
  ids <- espn_active_event_ids()
  purrr::map_dfr(ids, function(id) {
    dat <- espn_event(id)
    tibble::tibble(
      id = id,
      state = dat[["status"]][["type"]][["state"]] %||% NA_character_,
      detail = dat[["status"]][["type"]][["detail"]] %||% NA_character_,
      home = purrr::pluck(dat, "competitions", 1, "competitors", 1, "team", "displayName") %||% NA_character_,
      away = purrr::pluck(dat, "competitions", 1, "competitors", 2, "team", "displayName") %||% NA_character_
    )
  })
}

espn_team_map <- function(event_id){
  sm <- .espm_json(sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event=%s", event_id), flatten = FALSE)
  comps <- purrr::pluck(sm, "header", "competitions", 1, "competitors")
  if (is.null(comps) || !length(comps)) return(NULL)
  tibble(
    side    = purrr::map_chr(comps, ~ .x$homeAway %||% NA_character_),
    abbrev  = purrr::map_chr(comps, ~ .x$team$abbreviation %||% .x$team$shortDisplayName %||% NA_character_),
    team_id = purrr::map_chr(comps, ~ .x$team$id %||% NA_character_)
  )
}

# --- Robust down inference ----------------------------------------------------
.infer_down_from_text <- function(x){
  xx <- tolower(x %||% "")
  if (grepl("\\b1st\\b", xx)) return(1L)
  if (grepl("\\b2nd\\b", xx)) return(2L)
  if (grepl("\\b3rd\\b", xx)) return(3L)
  if (grepl("\\b4th\\b", xx)) return(4L)
  NA_integer_
}

.infer_down_coalesce <- function(start_down, sddt, text){
  d1 <- suppressWarnings(as.integer(start_down))
  if (!is.na(d1)) return(d1)
  d2 <- suppressWarnings(as.integer(str_extract(sddt %||% "", "^[1234](?=\\s*&\\s*)")))
  if (!is.na(d2)) return(d2)
  .infer_down_from_text(text)
}

# safe pluck
.pluck <- function(x, ...) purrr::pluck(x, ..., .default = NULL)

# --- Play-by-play fetch (unflattened) ----------------------------------------
espn_fetch_pbp <- function(event_id){
  pb <- .espm_json(
    sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/playbyplay?event=%s", event_id),
    flatten = FALSE
  )

  # Collect plays from previous drives + current
  plays <- list()
  prev <- .pluck(pb, "drives", "previous")
  if (!is.null(prev) && length(prev)) plays <- c(plays, purrr::flatten(purrr::map(prev, ~ .pluck(.x, "plays") %||% list())))
  cur  <- .pluck(pb, "drives", "current", "plays")
  if (!is.null(cur) && length(cur)) plays <- c(plays, cur)
  plays <- purrr::compact(purrr::flatten(plays))

  if (!length(plays)) {
    return(list(plays = tibble::tibble(), meta = list(period = NA_integer_, clock = NA_character_)))
  }

  df <- purrr::map_dfr(plays, function(p){
    tibble::tibble(
      play_id       = as.character(.pluck(p, "id") %||% NA_character_),
      text          = as.character(.pluck(p, "text") %||% .pluck(p, "description") %||% NA_character_),
      type_text     = as.character(.pluck(p, "type", "text") %||% NA_character_),
      type_abbr     = as.character(.pluck(p, "type", "abbreviation") %||% NA_character_),
      period        = suppressWarnings(as.integer(.pluck(p, "period", "number") %||% .pluck(p, "periodNumber") %||% NA_integer_)),
      clock_display = as.character(.pluck(p, "clock", "displayValue") %||% .pluck(p, "clockDisplay") %||% NA_character_),
      start_down    = suppressWarnings(as.integer(.pluck(p, "start", "down") %||% NA_integer_)),
      start_dist    = suppressWarnings(as.integer(.pluck(p, "start", "distance") %||% NA_integer_)),
      start_sddt    = as.character(.pluck(p, "start", "shortDownDistanceText") %||% NA_character_),
      start_poss    = as.character(
        .pluck(p, "start", "possession", "displayValue") %||%
        .pluck(p, "start", "possessionText") %||%
        .pluck(p, "start", "team", "abbreviation") %||%
        .pluck(p, "start", "team", "shortDisplayName") %||% NA_character_
      ),
      start_home    = suppressWarnings(as.integer(.pluck(p, "start", "homeScore") %||% NA_integer_)),
      start_away    = suppressWarnings(as.integer(.pluck(p, "start", "awayScore") %||% NA_integer_)),
      team_abbr     = toupper(as.character(.pluck(p, "team", "abbreviation") %||% .pluck(p, "team", "shortDisplayName") %||% NA_character_)),
      scoring_play  = isTRUE(.pluck(p, "scoringPlay"))
    )
  })

  meta <- list(
    period = suppressWarnings(as.integer(.pluck(cur, 1, "period", "number") %||% NA_integer_)),
    clock  = as.character(.pluck(cur, 1, "clock", "displayValue") %||% NA_character_)
  )
  list(plays = df, meta = meta)
}

# --- Normalize to FD schema (ALWAYS includes `down`) --------------------------
espn_plays_to_fd_rows <- function(event_id, plays_df, team_map, season = NA_integer_, week = NA_integer_){
  if (nrow(plays_df) == 0 || is.null(team_map) || !nrow(team_map)) return(tibble())

  home_abbr <- toupper(team_map$abbrev[team_map$side == "home"] %||% NA_character_)
  away_abbr <- toupper(team_map$abbrev[team_map$side == "away"] %||% NA_character_)

  out <- plays_df %>%
    mutate(
      poss = purrr::map(start_poss, .parse_possession_text),
      poss_team = purrr::map_chr(poss, "team"),
      poss_yard = purrr::map_int(poss, "yard"),
      posteam   = toupper(coalesce(team_abbr, poss_team)),
      defteam   = ifelse(posteam == home_abbr, away_abbr,
                  ifelse(posteam == away_abbr, home_abbr, NA_character_)),
      yardline_100 = .compute_yardline_100(posteam, poss_team, poss_yard),
      qtr   = as.integer(period),
      gsr   = .game_seconds_remaining(period, clock_display),
      posteam_score = ifelse(posteam == home_abbr, start_home, start_away),
      defteam_score = ifelse(posteam == home_abbr, start_away, start_home),
      home_score    = start_home,
      away_score    = start_away,
      score_differential = as.integer((posteam_score %||% 0) - (defteam_score %||% 0)),
      play_type = tolower(type_text %||% NA_character_),
      down = .infer_down_coalesce(start_down, start_sddt, text)
    ) %>%
    transmute(
      game_id = as.character(event_id),
      play_id,
      season = as.integer(season),
      week   = as.integer(week),
      qtr    = as.integer(qtr),
      game_seconds_remaining = as.integer(gsr),
      posteam, defteam,
      yardline_100 = as.numeric(yardline_100),
      ydstogo      = as.integer(start_dist),
      score_differential,
      play_type,
      posteam_score = as.integer(posteam_score),
      defteam_score = as.integer(defteam_score),
      home_score = as.integer(home_score),
      away_score = as.integer(away_score),
      text = text %||% NA_character_,
      down = as.integer(down)
    )
  out
}

# --- Stateful de-dup per event_id --------------------------------------------
.espm_state_path <- function(){
  dir.create("data/live", recursive = TRUE, showWarnings = FALSE)
  file.path("data/live", "espn_state.json")
}
.read_espm_state <- function(){
  p <- .espm_state_path()
  if (!file.exists(p)) return(list())
  jsonlite::read_json(p, simplifyVector = TRUE)
}
.write_espm_state <- function(state){
  jsonlite::write_json(state, path = .espm_state_path(), auto_unbox = TRUE, pretty = TRUE)
}
filter_new_plays <- function(event_id, plays_df){
  st <- .read_espm_state(); key <- as.character(event_id)
  seen <- st[[key]] %||% character()
  new_df <- plays_df %>% filter(!(play_id %in% seen), !is.na(play_id))
  st[[key]] <- unique(c(seen, new_df$play_id))
  if (length(st[[key]]) > 300) st[[key]] <- tail(st[[key]], 300)
  .write_espm_state(st)
  new_df
}
