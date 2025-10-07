# ------------------------------------------------------------------------------
# ESPN (unofficial) live adapter
#  - Discovers active event IDs
#  - Pulls play-by-play per game
#  - Converts plays to minimal "pbp rows" your bot expects
#  - Tracks last seen play per game to avoid duplicates
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(dplyr); library(purrr); library(stringr); library(tibble)
})

.espm_json <- function(url){
  resp <- httr::GET(url, httr::user_agent("fd-bot/espn-adapter"))
  stop_for_status(resp)
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, flatten = TRUE)
}

# --- Helpers ------------------------------------------------------------------

.parse_clock_secs <- function(display){
  # display like "12:34" or "0:05"
  if (is.null(display) || is.na(display) || !nzchar(display)) return(0L)
  mmss <- str_split_fixed(display, ":", 2)
  as.integer(mmss[,1]) * 60L + as.integer(mmss[,2])
}

.game_seconds_remaining <- function(period_number, clock_display){
  # NFL regulation: 4 * 900 sec; OT period_number==5 (10 min). We treat OT as 0 remaining for WP if you prefer.
  sec_left_qtr <- .parse_clock_secs(clock_display)
  if (is.na(period_number)) return(0L)
  if (period_number <= 4) {
    (4L - period_number) * 900L + sec_left_qtr
  } else {
    # Overtime: conservative 0 remaining beyond current
    sec_left_qtr
  }
}

# From possession text like "BUF 43" -> list(team="BUF", yard=43)
.parse_possession_text <- function(txt){
  if (is.null(txt) || is.na(txt) || !nzchar(txt)) return(list(team = NA_character_, yard = NA_integer_))
  parts <- str_split(txt, "\\s+", n = 2)[[1]]
  if (length(parts) < 2) return(list(team = NA_character_, yard = NA_integer_))
  yd <- suppressWarnings(as.integer(parts[2]))
  list(team = parts[1], yard = yd)
}

# Compute yardline_100 given posteam + "TEAM YARD" marker
.compute_yardline_100 <- function(posteam, mark_team, yard){
  if (is.na(yard)) return(NA_real_)
  if (isTRUE(toupper(posteam) == toupper(mark_team))) 100 - yard else yard
}

# --- Public: active ESPN event IDs (today/week) --------------------------------
espn_active_event_ids <- function(){
  url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard"
  sb <- .espm_json(url)
  ev <- sb$events
  if (is.null(ev) || !nrow(as.data.frame(ev))) return(character())
  # keep in-progress + pre (you may restrict to in-progress)
  ids <- purrr::map_chr(ev, ~ .x$id %||% NA_character_)
  ids[!is.na(ids)]
}

# Build team map (id -> abbrev/home/away) for a game
espn_team_map <- function(event_id){
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event=%s", event_id)
  sm <- .espm_json(url)
  comp <- sm$header$competitions[[1]]
  if (is.null(comp)) return(NULL)

  comps <- comp$competitors[[1]]
  if (is.null(comps)) return(NULL)

  tibble(
    side = purrr::map_chr(comps, ~ .x$homeAway %||% NA_character_),
    team_id = purrr::map_chr(comps, ~ .x$team$id %||% NA_character_),
    abbrev  = purrr::map_chr(comps, ~ .x$team$abbreviation %||% .x$team$shortDisplayName %||% NA_character_)
  )
}

# Fetch PBP for an event; returns list(plays=tibble, meta=list(period,clock))
espn_fetch_pbp <- function(event_id){
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/playbyplay?event=%s", event_id)
  pb <- .espm_json(url)

  # Drives can be under $drives$current$plays and $drives$previous
  plays <- list()
  cur <- pb$drives$current
  prv <- pb$drives$previous
  if (!is.null(cur$plays[[1]])) plays <- c(plays, cur$plays)
  if (!is.null(prv)) {
    prv_plays <- purrr::map(prv, ~ .x$plays)
    plays <- c(plays, purrr::flatten(prv_plays))
  }
  plays <- purrr::compact(plays)

  # Flatten to tibble; guard missing fields
  if (!length(plays)) return(list(plays = tibble(), meta = list(period = NA_integer_, clock = NA_character_)))

  df <- purrr::map_dfr(plays, function(p){
    # some keys may be missing
    tibble(
      play_id       = p$id %||% NA_character_,
      text          = p$text %||% NA_character_,
      type_text     = p$type$text %||% NA_character_,
      type_abbr     = p$type$abbreviation %||% NA_character_,
      period        = p$period$number %||% NA_integer_,
      clock_display = p$clock$displayValue %||% NA_character_,
      start_down    = p$start$down %||% NA_integer_,
      start_dist    = p$start$distance %||% NA_integer_,
      start_poss    = p$start$possession$displayValue %||% p$start$possessionText %||% NA_character_,
      start_home    = p$start$homeScore %||% NA_integer_,
      start_away    = p$start$awayScore %||% NA_integer_,
      team_abbr     = p$team$abbreviation %||% NA_character_,
      scoring_play  = isTRUE(p$scoringPlay)
    )
  })

  meta <- list(
    period = cur$plays[[1]]$period$number %||% NA_integer_,
    clock  = cur$plays[[1]]$clock$displayValue %||% NA_character_
  )
  list(plays = df, meta = meta)
}

# Convert ESPN plays to the minimal columns your bot uses
# Returns tibble with: game_id, play_id, season, week (NA if unknown), qtr, game_seconds_remaining,
#                      posteam, defteam, yardline_100, ydstogo, score_diff
espn_plays_to_fd_rows <- function(event_id, plays_df, team_map, season = NA_integer_, week = NA_integer_){
  if (nrow(plays_df) == 0 || is.null(team_map) || !nrow(team_map)) return(tibble())

  # Use team map to map offence/defence abbreviations reliably
  # If play team_abbr missing, fallback to possession text prefix
  rows <- plays_df %>%
    mutate(
      poss_parsed = purrr::map(start_poss, .parse_possession_text),
      poss_team   = toupper(purrr::map_chr(poss_parsed, "team")),
      poss_yard   = purrr::map_int(poss_parsed, "yard"),
      posteam     = toupper(coalesce(team_abbr, poss_team)),
      # defteam: the other side
      defteam     = {
        home_abbr <- team_map$abbrev[team_map$side == "home"] %||% NA_character_
        away_abbr <- team_map$abbrev[team_map$side == "away"] %||% NA_character_
        ifelse(toupper(posteam) == toupper(home_abbr), toupper(away_abbr),
               ifelse(toupper(posteam) == toupper(away_abbr), toupper(home_abbr), NA_character_))
      },
      yardline_100 = .compute_yardline_100(posteam, poss_team, poss_yard),
      qtr          = as.integer(period),
      gsr          = .game_seconds_remaining(period, clock_display),
      # score diff from posteam perspective
      score_diff   = {
        home_abbr <- team_map$abbrev[team_map$side == "home"] %||% NA_character_
        # map team score by side at start of play
        ifelse(toupper(posteam) == toupper(home_abbr),
               (start_home %||% 0) - (start_away %||% 0),
               (start_away %||% 0) - (start_home %||% 0))
      }
    ) %>%
    transmute(
      game_id = as.character(event_id),
      play_id,
      season = as.integer(season),
      week   = as.integer(week),
      qtr    = as.integer(qtr),
      game_seconds_remaining = as.integer(gsr),
      posteam = as.character(posteam),
      defteam = as.character(defteam),
      yardline_100 = as.numeric(yardline_100),
      ydstogo = as.integer(start_dist),
      score_differential = as.integer(score_diff),
      type_text = type_text %||% NA_character_,
      text = text %||% NA_character_,
      scoring_play = scoring_play
    )
  rows
}

# --- State management: last seen play per event --------------------------------

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

# Given a tibble of plays, drop ones we've already processed for this game
filter_new_plays <- function(event_id, plays_df){
  st <- .read_espm_state()
  seen <- st[[as.character(event_id)]] %||% character()
  new_df <- plays_df %>% filter(!(play_id %in% seen), !is.na(play_id))
  # Update state with cumulative set but bound size
  st[[as.character(event_id)]] <- unique(c(seen, new_df$play_id))
  # keep last ~300 ids per game to avoid file bloat
  if (length(st[[as.character(event_id)]]) > 300) {
    st[[as.character(event_id)]] <- tail(st[[as.character(event_id)]], 300)
  }
  .write_espm_state(st)
  new_df
}

# "Is this a legitimate 4th-down decision candidate?"
is_fd_candidate <- function(row){
  if (is.na(row$ydstogo) || is.na(row$yardline_100) || is.na(row$qtr)) return(FALSE)
  # exclude housekeeping plays
  bad_types <- c("TIMEOUT", "END PERIOD", "END GAME", "TWO-MINUTE WARNING", "PENALTY (NO PLAY)")
  if (any(str_detect(toupper(row$type_text %||% ""), bad_types))) return(FALSE)
  TRUE
}
