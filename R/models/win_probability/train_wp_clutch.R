# ------------------------------------------------------------------------------
# WP "Clutch" model (4Q/close; uses prep_wp_pbp)
# 70/15/15 stratified splits with TRAIN-frozen levels; VALID held for calibration
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(data.table); library(caret); library(pROC)
})
source("R/core/pbp_wp_prep.R")

set.seed(42)

MODEL_DIR   <- "R/models/win_probability"
MODEL_RDS   <- file.path(MODEL_DIR, "wp_clutch_model.rds")
LEVELS_RDS  <- file.path(MODEL_DIR, "wp_clutch_levels.rds")
SPLIT_DIR   <- "data/wp_clutch"
if (!dir.exists(MODEL_DIR)) dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(SPLIT_DIR)) dir.create(SPLIT_DIR, recursive = TRUE, showWarnings = FALSE)

wp_df     <- prep_wp_pbp(start_year = 1999, include_current = FALSE) |> as.data.table()
clutch_df <- wp_df[game_seconds_remaining <= 900 & abs(score_differential) <= 10]

drop_cols <- c("game_date","result","posteam_type","drive","stadium_id",
               "weather_imputed","total","Winner")
features  <- setdiff(names(clutch_df), drop_cols)

# --- 70/15/15 stratified split -------------------------------------------------
idx_train <- caret::createDataPartition(clutch_df$Winner, p = 0.70, list = FALSE)
train     <- clutch_df[idx_train]
rest      <- clutch_df[-idx_train]
idx_valid <- caret::createDataPartition(rest$Winner, p = 0.50, list = FALSE)
valid     <- rest[idx_valid]
test      <- rest[-idx_valid]

# ---- Levels from TRAIN only ---------------------------------------------------
is_cat      <- vapply(train[, ..features], \(x) is.character(x) || is.factor(x), logical(1))
cat_cols    <- names(is_cat)[is_cat]
levels_map  <- lapply(train[, ..cat_cols], \(col) if (is.factor(col)) levels(col) else sort(unique(as.character(col))))
saveRDS(levels_map, LEVELS_RDS)

encode_with_levels <- function(x, lv) as.integer(factor(as.character(x), levels = lv))
for (f in features) {
  if (f %in% cat_cols) {
    lv          <- levels_map[[f]]
    train[[f]]  <- encode_with_levels(train[[f]], lv)
    valid[[f]]  <- encode_with_levels(valid[[f]], lv)
    test[[f]]   <- encode_with_levels(test[[f]],  lv)
  } else {
    train[[f]] <- suppressWarnings(as.numeric(train[[f]]))
    valid[[f]] <- suppressWarnings(as.numeric(valid[[f]]))
    test[[f]]  <- suppressWarnings(as.numeric(test[[f]]))
  }
}

# ---- (Optional) Save splits ---------------------------------------------------
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

# ---- Evaluate on TEST --------------------------------------------------------
test_probs <- predict(wp_model, newdata = x_test, type = "prob")[, "Win"]
auc_test   <- as.numeric(pROC::auc(y_test, test_probs))

bundle <- list(
  model         = wp_model$finalModel,
  feature_names = colnames(x_train),
  trained_at    = Sys.time(),
  params        = wp_model$bestTune,
  source        = "R/models/win_probability/train_wp_clutch.R",
  meta          = list(n_train = nrow(x_train), n_valid = nrow(valid), n_test = nrow(x_test), metric = "ROC",
                       clutch_filter = "gsec<=900 & |score_diff|<=10"),
  eval          = list(auc_test = auc_test)
)
saveRDS(bundle, MODEL_RDS)
cat(sprintf("[WP-CLUTCH] Saved %s | AUC(test)=%.4f\n", MODEL_RDS, auc_test))
cat(sprintf("[WP-CLUTCH] Saved levels map -> %s\n", LEVELS_RDS))
