# R/bots/fetch_live_plays.R
suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(tibble); library(dplyr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (is.atomic(a) && all(is.na(a)))) b else a

# Optional helper: derive game_seconds_remaining if feed gives "clock" like "12:34"
.clock_to_gsr <- function(qtr, clock){
  mm <- suppressWarnings(as.integer(sub(":.*", "", clock)))
  ss <- suppressWarnings(as.integer(sub(".*:", "", clock)))
  secs_left <- pmax(0L, 60L*mm + ss)
  q <- suppressWarnings(as.integer(qtr))
  base <- dplyr::case_when(
    q == 1L ~ 3L*900L,
    q == 2L ~ 2L*900L,
    q == 3L ~ 1L*900L,
    q >= 4L ~ 0L,
    TRUE    ~ NA_integer_
  )
  as.numeric(base + secs_left)
}

# --- MAIN: fetch current live plays from your endpoint -----------------------
fetch_live_plays <- function(){
  url   <- Sys.getenv("NFL_LIVE_URL", "")
  token <- Sys.getenv("NFL_LIVE_TOKEN", "")  # optional

  if (!nzchar(url)) {
    message("[fetch_live_plays] NFL_LIVE_URL not set; returning empty frame.")
    return(tibble())
  }

  req <- request(url) |>
    req_user_agent("FourthDownBot/1.0 (+github actions)") |>
    req_timeout(10) |>
    req_retry(max_tries = 3)

  if (nzchar(token)) req <- req |> req_headers(Authorization = paste("Bearer", token))

  resp  <- req_perform(req)
  body  <- resp_body_json(resp, simplifyVector = TRUE)
  plays <- body$plays %||% body$data %||% body
  if (is.null(plays) || length(plays) == 0) return(tibble())

  df <- tibble::as_tibble(plays)

  # Ensure required columns exist (fill with NA if your feed doesn’t include them)
  needed <- c(
    "down","game_id","season","week","qtr","game_seconds_remaining",
    "posteam","defteam","yardline_100","ydstogo","score_differential",
    "play_type","roof","surface","wind","temp","spread_line","total_line",
    "posteam_timeouts_remaining","defteam_timeouts_remaining","wp"
  )
  for (nm in needed) if (!nm %in% names(df)) df[[nm]] <- NA

  # If only "clock" exists, compute game_seconds_remaining
  if ("clock" %in% names(df) && all(is.na(df$game_seconds_remaining))) {
    df$game_seconds_remaining <- .clock_to_gsr(df$qtr, df$clock)
  }

  # Best-effort type coercions so the sim doesn’t choke
  df <- df %>%
    mutate(
      down  = suppressWarnings(as.integer(down)),
      qtr   = suppressWarnings(as.integer(qtr)),
      week  = suppressWarnings(as.integer(week)),
      season = suppressWarnings(as.integer(season)),
      game_seconds_remaining = suppressWarnings(as.numeric(game_seconds_remaining)),
      yardline_100 = suppressWarnings(as.numeric(yardline_100)),
      ydstogo      = suppressWarnings(as.numeric(ydstogo)),
      score_differential = suppressWarnings(as.numeric(score_differential)),
      wind = suppressWarnings(as.numeric(wind)),
      temp = suppressWarnings(as.numeric(temp)),
      spread_line = suppressWarnings(as.numeric(spread_line)),
      total_line  = suppressWarnings(as.numeric(total_line)),
      posteam_timeouts_remaining = suppressWarnings(as.integer(posteam_timeouts_remaining)),
      defteam_timeouts_remaining = suppressWarnings(as.integer(defteam_timeouts_remaining))
    )

  df
}
