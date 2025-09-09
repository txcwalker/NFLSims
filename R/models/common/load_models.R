# Loading Models used in the fourth down bot

load_models <- function() {
  list(
    wp_model = readRDS("models/wp_model.rds"),
    fg_model = readRDS("models/fg_model.rds"),
    fd_model = readRDS("models/fd_model.rds")
  )
}

