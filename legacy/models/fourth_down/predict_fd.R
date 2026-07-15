# legacy/models/fourth_down/predict_fd.R
# RETIRED: superseded by R/simulators/fourth_down/fourth_down_decision.R, which
# loads the FD conversion model self-contained. Kept for reference only.

suppressPackageStartupMessages({
  library(xgboost)
  library(data.table)
})
source("legacy/models/fourth_down/load_fd_model.R")

.encode_with_levels <- function(x, lv) {
  as.integer(factor(as.character(x), levels = lv))
}

predict_fd_conversion <- function(df) {
  load_fd_models()
  
  if (is.null(.fd_env$conv_model)) stop("FD Conversion model is not loaded.")
  
  dt <- as.data.table(df)
  model <- .fd_env$conv_model
  features <- .fd_env$conv_features
  levels_map <- .fd_env$conv_levels
  
  if (!"temp" %in% names(dt)) dt[, temp := 72]
  if (!"wind" %in% names(dt)) dt[, wind := 0]
  if (!"roof" %in% names(dt)) dt[, roof := "outdoors"]
  
  dt[is.na(temp), temp := 72]
  dt[is.na(wind), wind := 0]
  
  cat_cols <- names(levels_map)
  for (f in features) {
    if (f %in% names(dt)) {
      if (f %in% cat_cols) {
        dt[[f]] <- .encode_with_levels(dt[[f]], levels_map[[f]])
      } else {
        dt[[f]] <- suppressWarnings(as.numeric(dt[[f]]))
      }
    } else {
      dt[[f]] <- NA_real_
    }
  }
  
  x_pred <- as.matrix(dt[, features, with = FALSE])
  probs <- predict(model, newdata = x_pred)
  
  return(probs)
}
