# fourth_down_helper.R
# Helper function
prepare_state <- function(state_df, model_features) {
  for (col in names(state_df)) {
    if (is.character(state_df[[col]]) || is.factor(state_df[[col]])) {
      state_df[[col]] <- as.integer(as.factor(state_df[[col]]))
    }
  }

  # Fill missing features with 0s if needed
  missing <- setdiff(model_features, names(state_df))
  if (length(missing) > 0) {
    for (col in missing) {
      state_df[[col]] <- 0
    }
  }

  state_df <- state_df[, model_features, drop = FALSE]
  return(state_df)
}