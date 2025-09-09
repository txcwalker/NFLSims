# bluesky_bot.R
library(httr)
library(jsonlite)

post_to_bluesky <- function(message) {
  username <- Sys.getenv("BSKY_USERNAME")
  password <- Sys.getenv("BSKY_APP_PASSWORD")

  # Step 1: Login
  res <- POST(
    url = "https://bsky.social/xrpc/com.atproto.server.createSession",
    body = list(identifier = username, password = password),
    encode = "json"
  )

  if (http_error(res)) {
    stop("Bluesky login failed: ", content(res, "text"))
  }

  session <- content(res)
  token <- session$accessJwt
  did <- session$did

  # Step 2: Post message
  post_body <- list(
    repo = did,
    collection = "app.bsky.feed.post",
    record = list(
      text = message,
      createdAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      `$type` = "app.bsky.feed.post"
    )
  )

  post_res <- POST(
    url = "https://bsky.social/xrpc/com.atproto.repo.createRecord",
    add_headers(Authorization = paste("Bearer", token)),
    body = post_body,
    encode = "json"
  )

  if (http_error(post_res)) {
    warning("Bluesky post failed: ", content(post_res, "text"))
  } else {
    message("✅ Posted to Bluesky")
  }
}


