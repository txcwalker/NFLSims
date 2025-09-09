# Resolve per-model hparams with fallbacks
get_grid <- function(model_key, default_grid) {
  # Prefer model-specific grid if defined (e.g., xgb_grid_fg)
  model_grid <- get0(paste0("xgb_grid_", model_key), ifnotfound = NULL, inherits = TRUE)
  if (!is.null(model_grid)) return(model_grid)
  # Else a shared global grid if provided
  shared <- get0("xgb_grid", ifnotfound = NULL, inherits = TRUE)
  if (!is.null(shared)) return(shared)
  # Else script-local default
  default_grid
}

get_ctrl <- function(model_key, default_ctrl) {
  model_ctrl <- get0(paste0("ctrl_", model_key), ifnotfound = NULL, inherits = TRUE)
  if (!is.null(model_ctrl)) return(model_ctrl)
  shared <- get0("ctrl", ifnotfound = NULL, inherits = TRUE)
  if (!is.null(shared)) return(shared)
  default_ctrl
}
