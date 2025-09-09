# Script for automatically tweeting

# mastodon_bot.R
library(Rtoot)

post_to_mastodon <- function(message) {
  token <- Sys.getenv("MASTODON_TOKEN")
  instance <- Sys.getenv("MASTODON_INSTANCE")  # e.g. "https://mastodon.social"

  if (token == "" || instance == "") {
    stop("Missing Mastodon token or instance.")
  }

  set_token(token, base_url = instance)

  post_toot(status = message)
  message("✅ Posted to Mastodon")
}
