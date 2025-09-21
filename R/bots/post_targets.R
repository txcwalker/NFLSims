# R/bots/post_targets.R
# Posting helpers for Mastodon + Bluesky with DRY_RUN support
# Requires: rtoot (Mastodon), glue; optional: atrrr (Bluesky)
# Env vars used (from .env or runner secrets):
# - DRY_RUN = "0" | "1"
# - MASTO_SERVER, MASTO_TOKEN
# - BSKY_HANDLE, BSKY_APP_PASSWORD

suppressPackageStartupMessages({
  library(glue)
  library(rtoot)
  library(atrrr)
  library(dotenv)
})

# Optional Bluesky support
.BSKY_AVAILABLE <- requireNamespace("atrrr", quietly = TRUE)

# Optional dotenv (don’t fail if not installed)
if (requireNamespace("dotenv", quietly = TRUE)) {
  try(dotenv::load_dot_env(".env", override = FALSE), silent = TRUE)
}

# -- small utils ---------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (is.atomic(a) && all(is.na(a)))) b else a

.get_env <- function(key, default = "") {
  val <- Sys.getenv(key, unset = NA_character_)
  if (is.na(val) || identical(val, "")) return(default)
  val
}

.is_dry_run <- function() identical(.get_env("DRY_RUN", "0"), "1")

.trim_to_limit <- function(txt, limit) {
  if (nchar(txt, type = "chars", allowNA = FALSE) <= limit) return(txt)
  # Reserve 1 char for ellipsis
  paste0(substr(txt, 1, max(0, limit - 1)), "…")
}

# -- Mastodon ------------------------------------------------------------------

# Returns TRUE/FALSE; prints errors but doesn't stop your loop
post_masto <- function(text, char_limit = 500) {
  srv <- .get_env("MASTO_SERVER")
  tok <- .get_env("MASTO_TOKEN")

  if (!nzchar(srv) || !nzchar(tok)) return(FALSE)   # not configured
  if (.is_dry_run()) {
    message("[DRY_RUN:Mastodon] ", text)
    return(TRUE)
  }

  safe_txt <- .trim_to_limit(text, char_limit)
  # rtoot wants instance without scheme
  instance <- sub("^https?://", "", srv)

  ok <- tryCatch({
    rt <- rtoot::rt_connect(token = tok, instance = instance)
    rtoot::post_toot(status = safe_txt, token = rt)
    TRUE
  }, error = function(e) {
    message("[Mastodon ERROR] ", conditionMessage(e))
    FALSE
  })
  ok
}

# -- Bluesky (via atrrr) -------------------------------------------------------

# One-time cached session
.bsky_session_env <- new.env(parent = emptyenv())

.bsky_login <- function(){
  h <- .get_env("BSKY_HANDLE")
  p <- .get_env("BSKY_APP_PASSWORD")
  if (!nzchar(h) || !nzchar(p)) return(NULL)
  if (!.BSKY_AVAILABLE) return(NULL)
  if (!is.null(.bsky_session_env$session)) return(.bsky_session_env$session)

  sess <- tryCatch(
    atrrr::login(identifier = h, password = p),
    error = function(e){ message("[Bluesky login ERROR] ", conditionMessage(e)); NULL }
  )
  .bsky_session_env$session <- sess
  sess
}

# Returns TRUE/FALSE; prints errors but doesn't stop your loop
post_bsky <- function(text, char_limit = 300) {
  if (!.BSKY_AVAILABLE) return(FALSE)              # package not installed
  sess <- .bsky_login()
  if (is.null(sess)) return(FALSE)                 # not configured or login failed

  if (.is_dry_run()) {
    message("[DRY_RUN:Bluesky] ", text)
    return(TRUE)
  }

  safe_txt <- .trim_to_limit(text, char_limit)

  ok <- tryCatch({
    atrrr::post(safe_txt, session = sess)
    TRUE
  }, error = function(e) {
    message("[Bluesky ERROR] ", conditionMessage(e))
    FALSE
  })
  ok
}

# -- One call to post to everything -------------------------------------------

# Returns a named logical list: list(mastodon=TRUE/FALSE, bluesky=TRUE/FALSE, any=TRUE/FALSE)
post_everywhere <- function(text) {
  masto_ok <- post_masto(text)
  bsky_ok  <- post_bsky(text)

  list(mastodon = isTRUE(masto_ok),
       bluesky  = isTRUE(bsky_ok),
       any      = isTRUE(masto_ok) || isTRUE(bsky_ok))
}

# -- Quick smoke tests ---------------------------------------------------------

# Run once in console to verify wiring (will not post when DRY_RUN=1)
smoke_post_targets <- function(){
  msg <- glue("Hello from NFL bot at {format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')}")
  res <- post_everywhere(msg)
  print(res)
  invisible(res)
}
