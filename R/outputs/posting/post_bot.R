# post_bot.R

post_bot <- function(message, platforms = c("mastodon", "bluesky")) {
  if ("mastodon" %in% platforms) {
    source("R/outputs/posting/mastodon_bot.R")
    post_to_mastodon(message)
  }
  if ("bluesky" %in% platforms) {
    source("R/outputs/posting/bluesky_bot.R")
    post_to_bluesky(message)
  }
}
