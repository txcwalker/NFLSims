# R/live/espn_adapter.R
# ------------------------------------------------------------------------------
# ESPN (unofficial) adapter: fetches scoreboard/summary/pbp and normalizes plays
# to a stable schema for the 4th-down bot. ALWAYS returns a `down` column.
#
# VECTORIZED VERSION - All helper functions now work with vectors (157+ plays)
#
# Key points:
# - Keep JSON as lists (no unwanted data.frame simplification).
# - Scoreboard helper (works with/without explicit date; seasontype/groups supported).
# - Robust extraction of event IDs even if `events` is a data.frame.
# - espn_list_events(...) accepts a pre-fetched scoreboard to avoid extra calls.
# - espn_fetch_pbp(...) returns a tibble of plays plus a small meta list.
# - espn_plays_to_fd_rows(...) expects that tibble, so dplyr never mutates a raw list.
# - filter_new_plays(event_id, plays_df) only operates on plays (not meta).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(dplyr); library(purrr)
  library(stringr); library(tibble); library(lubridate)
})

`%||%` <- function(a,b) if (is.null(a)||length(a)==0||(is.atomic(a)&&all(is.na(a)))) b else a
.pluck <- function(x, ...) purrr::pluck(x, ..., .default = NULL)

# --- HTTP JSON with light retry/backoff
.espm_json <- function(url, flatten = FALSE, simplify = FALSE){
  ua <- httr::user_agent("fd-bot/espn-adapter/0.7")
  for (i in 1:3) {
    resp <- try(httr::GET(url, ua, timeout(10)), silent = TRUE)
    if (inherits(resp, "try-error")) { Sys.sleep(runif(1, 0.2, 0.8)); next }
    if (httr::http_error(resp)) {
      if (i == 3) httr::stop_for_status(resp) else { Sys.sleep(runif(1, 0.2, 0.8)); next }
    }
    txt <- httr::content(resp, "text", encoding = "UTF-8")
    return(jsonlite::fromJSON(txt, flatten = flatten, simplifyVector = simplify))
  }
  stop("ESPN request failed after retries.")
}

# --- Time & yardline helpers (ALL VECTORIZED) ---

.parse_clock_secs <- function(display){
  display <- as.character(display)
  is_empty <- is.na(display) | !nzchar(display)

  mm <- suppressWarnings(as.integer(sub(":.*$", "", display)))
  ss <- suppressWarnings(as.integer(sub("^.*:", "", display)))
  result <- mm * 60L + ss

  result[is_empty] <- 0L
  as.integer(result)
}

.game_seconds_remaining <- function(period_number, clock_display){
  sec_left_qtr <- .parse_clock_secs(clock_display)
  period_number <- as.integer(period_number)

  result <- ifelse(
    is.na(period_number),
    NA_integer_,
    ifelse(
      period_number <= 4L,
      (4L - period_number) * 900L + sec_left_qtr,
      sec_left_qtr
    )
  )
  as.integer(result)
}

.parse_possession_text <- function(txt){
  txt <- as.character(txt)
  is_empty <- is.na(txt) | !nzchar(txt)

  # Initialize result vectors
  teams <- character(length(txt))
  yards <- integer(length(txt))

  # Process non-empty entries
  if (any(!is_empty)) {
    idx <- which(!is_empty)
    for (i in idx) {
      parts <- stringr::str_split(txt[i], "\\s+", n = 2)[[1]]
      if (length(parts) >= 2) {
        teams[i] <- toupper(parts[1])
        yd <- as.integer(gsub("[^0-9]", "", parts[2]))
        yards[i] <- if (is.na(yd)) NA_integer_ else yd
      } else {
        teams[i] <- NA_character_
        yards[i] <- NA_integer_
      }
    }
  }

  # Set empty entries to NA
  teams[is_empty] <- NA_character_
  yards[is_empty] <- NA_integer_

  list(team = teams, yard = yards)
}

.infer_down_from_text <- function(x){
  x <- tolower(as.character(x))
  result <- rep(NA_integer_, length(x))

  result[grepl("\\b1st\\b", x)] <- 1L
  result[grepl("\\b2nd\\b", x)] <- 2L
  result[grepl("\\b3rd\\b", x)] <- 3L
  result[grepl("\\b4th\\b", x)] <- 4L

  result
}

.infer_down_coalesce <- function(start_down, sddt, text){
  # Try start_down first
  d1 <- suppressWarnings(as.integer(start_down))
  result <- d1

  # Where d1 is NA, try sddt extraction
  na_idx <- is.na(result)
  if (any(na_idx)) {
    d2 <- suppressWarnings(as.integer(stringr::str_extract(sddt[na_idx], "^[1234](?=\\s*&\\s*)")))
    result[na_idx] <- d2
  }

  # Where still NA, infer from text
  na_idx <- is.na(result)
  if (any(na_idx)) {
    d3 <- .infer_down_from_text(text[na_idx])
    result[na_idx] <- d3
  }

  as.integer(result)
}

.compute_yardline_100 <- function(posteam, mark_team, yard){
  result <- ifelse(
    is.na(yard),
    NA_real_,
    ifelse(
      toupper(posteam) == toupper(mark_team),
      100 - yard,
      yard
    )
  )
  as.numeric(result)
}

# --- Scoreboard fetcher
espn_scoreboard <- function(league = "nfl", date = NULL, seasontype = NULL, groups = NULL) {
  base <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/%s/scoreboard", league)
  qs <- list()
  if (!is.null(date))       qs$dates <- format(as.Date(date), "%Y%m%d")
  if (!is.null(seasontype)) qs$seasontype <- as.character(seasontype)
  if (!is.null(groups))     qs$groups <- as.character(groups)

  url <- if (length(qs)) paste0(base, "?", paste(sprintf("%s=%s", names(qs), qs), collapse="&")) else base
  .espm_json(url, flatten = FALSE, simplify = FALSE)
}

# --- Extract status state robustly (ESPN returns it as a vector sometimes)
.get_status_state <- function(comp) {
  st <- purrr::pluck(comp, "status", "type")
  if (is.null(st)) return(NA_character_)

  # status$type might be a vector like: c(3, "STATUS_FINAL", "post", TRUE, "Final", ...)
  # or a list. Extract the second element which is usually the state name
  if (is.character(st) && length(st) > 1) {
    return(tolower(st[2]))  # e.g., "STATUS_FINAL", "STATUS_IN_PROGRESS"
  } else if (is.character(st)) {
    return(tolower(st[1]))
  }
  NA_character_
}

# --- Robust event id getter
espn_active_event_ids <- function(league = "nfl",
                                  date = NULL,
                                  only_live = TRUE,
                                  seasontype = NULL,
                                  groups = NULL,
                                  scoreboard = NULL) {
  sb <- scoreboard %||% espn_scoreboard(league = league, date = date,
                                        seasontype = seasontype, groups = groups)

  evs <- sb$events %||% list()
  if (is.data.frame(evs)) {
    if (nrow(evs) == 0) return(character())
    evs <- split(evs, seq_len(nrow(evs)))
  }
  if (!length(evs)) return(character())

  first_comp <- function(e) purrr::pluck(e, "competitions", 1)
  evs <- purrr::keep(evs, ~ !is.null(first_comp(.x)))

  if (isTRUE(only_live)) {
    evs <- purrr::keep(evs, function(e) {
      st <- .get_status_state(first_comp(e))
      # Check for "in_progress" or "in" or similar
      grepl("in_progress|in|live", st, ignore.case = TRUE)
    })
  }

  .scalar_chr <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    if (length(x) > 1) x <- x[[1]]
    as.character(x)
  }

  ids <- vapply(evs, function(e) {
    id1 <- .scalar_chr(purrr::pluck(e, "id"))
    if (!is.na(id1) && nzchar(id1)) return(id1)

    id2 <- .scalar_chr(purrr::pluck(first_comp(e), "id"))
    if (!is.na(id2) && nzchar(id2)) return(id2)

    uid <- .scalar_chr(purrr::pluck(e, "uid"))
    if (!is.na(uid) && nzchar(uid)) {
      m <- regmatches(uid, regexpr("e:([0-9]+)", uid))
      if (length(m) && nzchar(m)) return(sub("^e:", "", m))
    }
    NA_character_
  }, character(1))

  unique(ids[!is.na(ids) & nzchar(ids)])
}

# --- Summary/event
espn_event <- function(event_id) {
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event=%s", event_id)
  .espm_json(url, flatten = FALSE, simplify = FALSE)
}

# --- List events helper
espn_list_events <- function(scoreboard = NULL,
                             league = "nfl",
                             date = NULL,
                             only_live = FALSE,
                             seasontype = NULL,
                             groups = NULL) {
  sb <- scoreboard %||% espn_scoreboard(league = league, date = date,
                                        seasontype = seasontype, groups = groups)

  evs <- sb$events %||% list()
  if (is.data.frame(evs)) evs <- split(evs, seq_len(nrow(evs)))
  if (!length(evs)) {
    return(tibble::tibble(id=character(), state=character(), detail=character(),
                          home=character(), away=character()))
  }

  rows <- purrr::map_dfr(evs, function(e) {
    comp <- purrr::pluck(e, "competitions", 1)
    state <- .get_status_state(comp)

    tibble::tibble(
      id     = as.character((e$id %||% purrr::pluck(comp, "id")) %||% NA_character_),
      state  = state,
      detail = purrr::pluck(comp, "status", "type") %>%
               {if (is.character(.) && length(.) > 2) .[3] else NA_character_},
      home   = purrr::pluck(comp, "competitors", 1, "team", "displayName") %||% NA_character_,
      away   = purrr::pluck(comp, "competitors", 2, "team", "displayName") %||% NA_character_
    )
  })

  if (isTRUE(only_live)) {
    rows <- dplyr::filter(rows, grepl("in_progress|in|live", state, ignore.case = TRUE))
  }
  rows
}

# --- Team map
espn_team_map <- function(event_id){
  sm <- .espm_json(sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event=%s", event_id),
                   flatten = FALSE, simplify = FALSE)
  comps <- purrr::pluck(sm, "header", "competitions", 1, "competitors")
  if (is.null(comps) || !length(comps)) return(NULL)
  tibble::tibble(
    side    = purrr::map_chr(comps, ~ .x$homeAway %||% NA_character_),
    abbrev  = purrr::map_chr(comps, ~ .x$team$abbreviation %||% .x$team$shortDisplayName %||% NA_character_),
    team_id = purrr::map_chr(comps, ~ .x$team$id %||% NA_character_)
  )
}

# --- Play-by-play fetch
espn_fetch_pbp <- function(event_id){
  pb <- .espm_json(
    sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/playbyplay?event=%s", event_id),
    flatten = FALSE, simplify = FALSE
  )

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

# --- Normalize to FD schema
espn_plays_to_fd_rows <- function(event_id, plays_df, team_map, season = NA_integer_, week = NA_integer_){
  if (NROW(plays_df) == 0 || is.null(team_map) || NROW(team_map) == 0) {
    return(tibble::tibble())
  }

  home_abbr <- toupper(team_map$abbrev[team_map$side == "home"] %||% NA_character_)
  away_abbr <- toupper(team_map$abbrev[team_map$side == "away"] %||% NA_character_)

  out <- plays_df %>%
    dplyr::mutate(
      poss = purrr::map(start_poss, .parse_possession_text),
      poss_team = purrr::map_chr(poss, "team"),
      poss_yard = purrr::map_int(poss, "yard"),
      posteam   = toupper(dplyr::coalesce(team_abbr, poss_team)),
      defteam   = dplyr::if_else(posteam == home_abbr, away_abbr,
                         dplyr::if_else(posteam == away_abbr, home_abbr, NA_character_)),
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
    dplyr::transmute(
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

# --- Stateful de-dup
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
  if (NROW(plays_df) == 0) return(tibble::tibble())
  if (!inherits(plays_df, "data.frame")) plays_df <- tibble::as_tibble(plays_df)
  if (!"play_id" %in% names(plays_df))  return(tibble::tibble())

  st <- .read_espm_state(); key <- as.character(event_id)
  seen <- st[[key]] %||% character()

  new_df <- dplyr::filter(plays_df, !(play_id %in% seen), !is.na(play_id))

  st[[key]] <- unique(c(seen, new_df$play_id))
  if (length(st[[key]]) > 300) st[[key]] <- tail(st[[key]], 300)

  .write_espm_state(st)
  new_df
}