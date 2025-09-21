# ------------------------------------------------------------------------------
# Win Probability model (uses prep_wp_pbp from R/core/pbp_wp_prep.R)
# 70/15/15 stratified splits: train / valid / test
# - Train: used for caret CV training
# - Test : quick post-train metric check (AUC)
# - Valid: held-out for calibration/other eval (untouched here)
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(data.table); library(caret); library(pROC)
})
source("R/core/pbp_wp_prep.R")  # prep_wp_pbp(), fill helpers, clean helpers

set.seed(42)

# --- Paths --------------------------------------------------------------------
MODEL_DIR   <- "R/models/win_probability"
MODEL_RDS   <- file.path(MODEL_DIR, "wp_model.rds")
LEVELS_RDS  <- file.path(MODEL_DIR, "wp_levels.rds")
SPLIT_DIR   <- "data/wp"
if (!dir.exists(MODEL_DIR)) dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(SPLIT_DIR)) dir.create(SPLIT_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Data (single source of truth) --------------------------------------------
wp_df <- prep_wp_pbp(start_year = 1999, include_current = FALSE) |> as.data.table()

# Winner already created by clean_pbp_data() inside prep (binary int 0/1)
# Drop non-predictive / leakage cols
drop_cols <- c("game_date","result","posteam_type","drive","stadium_id",
               "weather_imputed","total","Winner")
features <- setdiff(names(wp_df), drop_cols)

# --- 70/15/15 stratified split -------------------------------------------------
idx_train <- caret::createDataPartition(wp_df$Winner, p = 0.70, list = FALSE)
train     <- wp_df[idx_train]
rest      <- wp_df[-idx_train]
idx_valid <- caret::createDataPartition(rest$Winner, p = 0.50, list = FALSE) # half of 30% = 15%
valid     <- rest[idx_valid]
test      <- rest[-idx_valid]

# ---- Save factor levels from TRAIN only --------------------------------------
is_cat    <- vapply(train[, ..features], \(x) is.character(x) || is.factor(x), logical(1))
cat_cols  <- names(is_cat)[is_cat]
wp_levels <- lapply(train[, ..cat_cols], \(col) if (is.factor(col)) levels(col) else sort(unique(as.character(col))))
saveRDS(wp_levels, LEVELS_RDS)

# ---- Integer encode categoricals (using TRAIN levels) ------------------------
encode_with_levels <- function(x, lv) as.integer(factor(as.character(x), levels = lv))
for (f in features) {
  if (f %in% cat_cols) {
    lv          <- wp_levels[[f]]
    train[[f]]  <- encode_with_levels(train[[f]], lv)
    valid[[f]]  <- encode_with_levels(valid[[f]], lv)
    test[[f]]   <- encode_with_levels(test[[f]],  lv)
  } else {
    train[[f]] <- suppressWarnings(as.numeric(train[[f]]))
    valid[[f]] <- suppressWarnings(as.numeric(valid[[f]]))
    test[[f]]  <- suppressWarnings(as.numeric(test[[f]]))
  }
}

# ---- (Optional) Save splits for downstream evaluation ------------------------
saveRDS(train[, c(features, "Winner"), with = FALSE], file.path(SPLIT_DIR, "train.rds"))
saveRDS(valid[, c(features, "Winner"), with = FALSE], file.path(SPLIT_DIR, "valid.rds"))
saveRDS(test [, c(features, "Winner"), with = FALSE], file.path(SPLIT_DIR, "test.rds"))
saveRDS(features, file.path(SPLIT_DIR, "feature_names.rds"))

# ---- Matrices / labels -------------------------------------------------------
x_train <- as.matrix(as.data.frame(train)[, features, drop = FALSE])
x_test  <- as.matrix(as.data.frame(test)[ , features, drop = FALSE])
y_train <- factor(ifelse(train$Winner == 1, "Win", "Loss"), levels = c("Win","Loss"))
y_test  <- test$Winner

# ---- Grid / Control ----------------------------------------------------------
xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6),
  eta = c(0.05, 0.10),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)
ctrl <- trainControl(
  method = "cv", number = 5, verboseIter = TRUE,
  classProbs = TRUE, summaryFunction = twoClassSummary, allowParallel = TRUE
)

# ---- Train -------------------------------------------------------------------
wp_model <- caret::train(
  x = x_train, y = y_train,
  method = "xgbTree", trControl = ctrl, tuneGrid = xgb_grid, metric = "ROC"
)

# ---- Evaluate on TEST (VALID remains untouched) ------------------------------
test_probs <- predict(wp_model, newdata = x_test, type = "prob")[, "Win"]
auc_test   <- as.numeric(pROC::auc(y_test, test_probs))

# ---- Bundle & Save -----------------------------------------------------------
bundle <- list(
  model         = wp_model$finalModel,
  feature_names = colnames(x_train),
  trained_at    = Sys.time(),
  params        = wp_model$bestTune,
  source        = "R/models/win_probability/train_wp.R",
  meta          = list(n_train = nrow(x_train), n_valid = nrow(valid), n_test = nrow(x_test), metric = "ROC"),
  eval          = list(auc_test = auc_test)
)
saveRDS(bundle, MODEL_RDS)
cat(sprintf("[WP] Saved %s | AUC(test)=%.4f\n", MODEL_RDS, auc_test))
cat(sprintf("[WP] Saved levels map -> %s\n", LEVELS_RDS))
