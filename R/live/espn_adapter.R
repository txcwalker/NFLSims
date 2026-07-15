# R/live/espn_adapter.R
# UPDATED: Now uses ESPN Core API instead of playbyplay endpoint
# Core API: https://sports.core.api.espn.com/v2/.../plays
# Benefits: Better structure, paginated, more reliable
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(dplyr); library(purrr)
  library(stringr); library(tibble); library(lubridate)
})

`%||%` <- function(a,b) if (is.null(a)||length(a)==0||(is.atomic(a)&&all(is.na(a)))) b else a
.pluck <- function(x, ...) purrr::pluck(x, ..., .default = NULL)

# --- HTTP JSON with light retry/backoff
.espm_json <- function(url, flatten = FALSE, simplify = FALSE){
  ua <- httr::user_agent("fd-bot/espn-core-api/1.0")
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

# --- Scoreboard fetcher (unchanged)
espn_scoreboard <- function(league = "nfl", date = NULL, seasontype = NULL, groups = NULL) {
  base <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/%s/scoreboard", league)
  qs <- list()
  if (!is.null(date))       qs$dates <- format(as.Date(date), "%Y%m%d")
  if (!is.null(seasontype)) qs$seasontype <- as.character(seasontype)
  if (!is.null(groups))     qs$groups <- as.character(groups)

  url <- if (length(qs)) paste0(base, "?", paste(sprintf("%s=%s", names(qs), qs), collapse="&")) else base
  .espm_json(url, flatten = FALSE, simplify = FALSE)
}

# --- Event ID extraction (from previous fix)
espn_active_event_ids <- function(league = "nfl",
                                  date = NULL,
                                  only_live = TRUE,
                                  seasontype = NULL,
                                  groups = NULL,
                                  scoreboard = NULL) {
  sb <- scoreboard %||% espn_scoreboard(league = league, date = date,
                                        seasontype = seasontype, groups = groups)

  evs <- sb$events %||% list()

  cat("[espn_active_event_ids] Found", length(evs), "events\n")

  if (is.data.frame(evs)) {
    if (nrow(evs) == 0) return(character())
    evs <- lapply(seq_len(nrow(evs)), function(i) as.list(evs[i, ]))
  }

  if (!length(evs)) return(character())

  extract_id <- function(e) {
    if (!is.null(e$id) && length(e$id) > 0) {
      id <- as.character(e$id)
      if (!is.na(id) && nzchar(id)) return(id)
    }

    comp <- .pluck(e, "competitions")
    if (!is.null(comp) && length(comp) > 0) {
      comp1 <- if (is.list(comp[[1]])) comp[[1]] else comp
      if (!is.null(comp1$id)) {
        id <- as.character(comp1$id)
        if (!is.na(id) && nzchar(id)) return(id)
      }
    }

    if (!is.null(e$uid)) {
      uid <- as.character(e$uid)
      m <- regmatches(uid, regexpr("[0-9]{9,}", uid))
      if (length(m) > 0 && nzchar(m)) return(m)
    }

    return(NA_character_)
  }

  extract_status <- function(e) {
    comp <- .pluck(e, "competitions")
    if (is.null(comp) || length(comp) == 0) return(NA_character_)

    comp1 <- if (is.list(comp[[1]])) comp[[1]] else comp
    status_type <- .pluck(comp1, "status", "type")

    if (is.null(status_type)) return(NA_character_)

    state <- NA_character_
    if (is.list(status_type)) {
      state <- status_type$state %||% status_type$name %||% NA_character_
    } else if (is.character(status_type) && length(status_type) >= 2) {
      state <- status_type[2]
    } else if (is.character(status_type)) {
      state <- status_type[1]
    }

    return(tolower(as.character(state)))
  }

  ids <- character(length(evs))
  statuses <- character(length(evs))

  for (i in seq_along(evs)) {
    ids[i] <- extract_id(evs[[i]])
    statuses[i] <- extract_status(evs[[i]])
  }

  valid_mask <- !is.na(ids) & nzchar(ids)
  ids <- ids[valid_mask]
  statuses <- statuses[valid_mask]

  if (isTRUE(only_live)) {
    live_mask <- grepl("in_progress|in|live|playing", statuses, ignore.case = TRUE)
    ids <- ids[live_mask]
  }

  unique(ids)
}

# --- NEW: Core API Play-by-Play Fetcher (Paginated)
espn_fetch_pbp <- function(event_id, max_pages = 20){
  cat("[espn_fetch_pbp] Fetching plays for event", event_id, "from Core API\n")

  all_plays <- list()
  page <- 1

  repeat {
    url <- sprintf(
      "https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/events/%s/competitions/%s/plays?page=%d&limit=100",
      event_id, event_id, page
    )

    cat("[espn_fetch_pbp] Fetching page", page, "\n")

    data <- tryCatch(
      .espm_json(url, flatten = FALSE, simplify = FALSE),
      error = function(e) {
        cat("[espn_fetch_pbp] ERROR on page", page, ":", conditionMessage(e), "\n")
        return(NULL)
      }
    )

    if (is.null(data) || is.null(data$items) || length(data$items) == 0) {
      cat("[espn_fetch_pbp] No more plays on page", page, "\n")
      break
    }

    all_plays <- c(all_plays, data$items)
    cat("[espn_fetch_pbp] Got", length(data$items), "plays from page", page, "(total so far:", length(all_plays), ")\n")

    # Check if there are more pages
    total_pages <- data$pageCount %||% 1
    if (page >= total_pages || page >= max_pages) {
      cat("[espn_fetch_pbp] Reached end (page", page, "of", total_pages, ")\n")
      break
    }

    page <- page + 1
  }

  cat("[espn_fetch_pbp] Total plays fetched:", length(all_plays), "\n")

  if (length(all_plays) == 0) {
    return(list(plays = tibble::tibble(), meta = list(period = NA_integer_, clock = NA_character_)))
  }

  # Convert to tibble
  df <- purrr::map_dfr(all_plays, function(p){
    tibble::tibble(
      play_id       = as.character(p$id %||% NA_character_),
      text          = as.character(p$text %||% NA_character_),
      type_text     = as.character(.pluck(p, "type", "text") %||% NA_character_),
      type_abbr     = as.character(.pluck(p, "type", "abbreviation") %||% NA_character_),
      period        = suppressWarnings(as.integer(.pluck(p, "period", "number") %||% NA_integer_)),
      clock_display = as.character(.pluck(p, "clock", "displayValue") %||% NA_character_),
      start_down    = suppressWarnings(as.integer(.pluck(p, "start", "down") %||% NA_integer_)),
      start_dist    = suppressWarnings(as.integer(.pluck(p, "start", "distance") %||% NA_integer_)),
      start_yardline = suppressWarnings(as.integer(.pluck(p, "start", "yardLine") %||% NA_integer_)),
      start_yardsToEndzone = suppressWarnings(as.integer(.pluck(p, "start", "yardsToEndzone") %||% NA_integer_)),
      end_down      = suppressWarnings(as.integer(.pluck(p, "end", "down") %||% NA_integer_)),
      end_yardline  = suppressWarnings(as.integer(.pluck(p, "end", "yardLine") %||% NA_integer_)),
      away_score    = suppressWarnings(as.integer(p$awayScore %||% NA_integer_)),
      home_score    = suppressWarnings(as.integer(p$homeScore %||% NA_integer_)),
      team_id       = as.character(.pluck(p, "team", "$ref") %||% NA_character_),
      scoring_play  = isTRUE(p$scoringPlay)
    )
  })

  # Meta from last play
  meta <- list(
    period = if (nrow(df) > 0) df$period[nrow(df)] else NA_integer_,
    clock  = if (nrow(df) > 0) df$clock_display[nrow(df)] else NA_character_
  )

  list(plays = df, meta = meta)
}

# --- Team map (unchanged)
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

# --- Time helpers (vectorized)
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

# --- NEW: Convert Core API plays to FD schema
espn_plays_to_fd_rows <- function(event_id, plays_df, team_map, season = NA_integer_, week = NA_integer_){
  if (NROW(plays_df) == 0 || is.null(team_map) || NROW(team_map) == 0) {
    return(tibble::tibble())
  }

  home_abbr <- toupper(team_map$abbrev[team_map$side == "home"] %||% NA_character_)
  away_abbr <- toupper(team_map$abbrev[team_map$side == "away"] %||% NA_character_)
  home_id <- team_map$team_id[team_map$side == "home"] %||% NA_character_
  away_id <- team_map$team_id[team_map$side == "away"] %||% NA_character_

  # Extract team ID from play to determine possession
  plays_df <- plays_df %>%
    dplyr::mutate(
      # Extract numeric team ID from $ref URL
      team_num = as.character(stringr::str_extract(team_id, "[0-9]+$")),
      # Determine which team has possession
      posteam = dplyr::case_when(
        team_num == home_id ~ home_abbr,
        team_num == away_id ~ away_abbr,
        TRUE ~ NA_character_
      ),
      defteam = dplyr::if_else(posteam == home_abbr, away_abbr,
                      dplyr::if_else(posteam == away_abbr, home_abbr, NA_character_)),
      # Use yardsToEndzone directly as yardline_100
      yardline_100 = as.numeric(start_yardsToEndzone),
      qtr = as.integer(period),
      gsr = .game_seconds_remaining(period, clock_display),
      # Scores
      posteam_score = dplyr::if_else(posteam == home_abbr, home_score, away_score),
      defteam_score = dplyr::if_else(posteam == home_abbr, away_score, home_score),
      score_differential = as.integer(posteam_score - defteam_score),
      # Play type
      play_type = tolower(type_text %||% NA_character_),
      # Down
      down = as.integer(start_down)
    ) %>%
    dplyr::transmute(
      game_id = as.character(event_id),
      play_id,
      season = as.integer(season),
      week   = as.integer(week),
      qtr,
      game_seconds_remaining = as.integer(gsr),
      posteam, defteam,
      yardline_100 = as.numeric(yardline_100),
      ydstogo = as.integer(start_dist),
      score_differential,
      play_type,
      posteam_score = as.integer(posteam_score),
      defteam_score = as.integer(defteam_score),
      home_score = as.integer(home_score),
      away_score = as.integer(away_score),
      text = text %||% NA_character_,
      down = as.integer(down)
    )

  plays_df
}

# --- Stateful de-dup (unchanged)
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

# --- Summary/event (unchanged)
espn_event <- function(event_id) {
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event=%s", event_id)
  .espm_json(url, flatten = FALSE, simplify = FALSE)
}

# --- List events helper (unchanged)
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
    status_type <- .pluck(comp, "status", "type")

    state <- NA_character_
    if (is.list(status_type)) {
      state <- status_type$state %||% status_type$name %||% NA_character_
    } else if (is.character(status_type) && length(status_type) >= 2) {
      state <- status_type[2]
    } else if (is.character(status_type)) {
      state <- status_type[1]
    }

    tibble::tibble(
      id     = as.character((e$id %||% purrr::pluck(comp, "id")) %||% NA_character_),
      state  = tolower(as.character(state)),
      detail = ifelse(is.character(status_type) && length(status_type) > 2, status_type[3], NA_character_),
      home   = purrr::pluck(comp, "competitors", 1, "team", "displayName") %||% NA_character_,
      away   = purrr::pluck(comp, "competitors", 2, "team", "displayName") %||% NA_character_
    )
  })

  if (isTRUE(only_live)) {
    rows <- dplyr::filter(rows, grepl("in_progress|in|live", state, ignore.case = TRUE))
  }
  rows
}