# ------------------------------------------------------------------------------
# Field Goal model (uses fill_missing_weather_* from R/core/pbp_wp_prep.R)
# 70/15/15 stratified splits: train / valid / test
# - Train: used for caret CV training
# - Test : quick post-train metric check (AUC here)
# - Valid: held-out for calibration/other eval plots later (untouched here)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
  library(caret)
  library(pROC)
  library(nflfastR)
})

source("R/core/pbp_wp_prep.R")

options(stringsAsFactors = FALSE)
set.seed(42)

YEARS      <- 1999:(as.integer(format(Sys.Date(), "%Y")) - 1)
MODEL_DIR  <- "R/models/field_goal"
MODEL_RDS  <- file.path(MODEL_DIR, "fg_model.rds")
LEVELS_RDS <- file.path(MODEL_DIR, "fg_levels.rds")
SPLIT_DIR  <- "data/fg"  # <-- optional: where we save train/valid/test for later eval
if (!dir.exists(MODEL_DIR)) dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(SPLIT_DIR)) dir.create(SPLIT_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Load and basic filter -----------------------------------------------------
fg_data <- nflfastR::load_pbp(YEARS) %>%
  dplyr::filter(!is.na(field_goal_result)) %>%
  dplyr::select(
    stadium_id, surface, wind, temp, roof,
    season_type, week, game_date,
    game_seconds_remaining, total,
    yardline_100, ydstogo,
    spread_line, total_line,
    field_goal_result
  )

# ensure data.table
fg_data <- as.data.table(fg_data)

# --- Weather cleanup + fills ---------------------------------------------------
# Make weather numeric before fills (avoid trunc warnings)
fg_data[, `:=`(temp = as.numeric(temp), wind = as.numeric(wind))]

# These helpers may return data.frame/tibble; reassert DT after each
fg_data <- fill_missing_weather_within_game(fg_data); data.table::setDT(fg_data)
fg_data <- fill_missing_weather_by_date(fg_data);   data.table::setDT(fg_data)

# --- Outcome: robust derivation + labels --------------------------------------
fg_data[, fg_made := {
  res_chr <- tolower(as.character(field_goal_result))
  as.integer(res_chr == "made")
}]
fg_data <- fg_data[!is.na(fg_made)]

# Positive class first for caret's twoClassSummary
fg_data[, label := factor(ifelse(fg_made == 1, "Made", "Miss"), levels = c("Made","Miss"))]

# --- Feature set ---------------------------------------------------------------
drop_cols <- c("game_date","weather_imputed","field_goal_result","fg_made","label","stadium_id","total")
features  <- setdiff(names(fg_data), drop_cols)

# --- 70/15/15 stratified split -------------------------------------------------
# 1) 70% train (stratified by label)
idx_train <- caret::createDataPartition(fg_data$label, p = 0.70, list = FALSE)
train_fg  <- fg_data[idx_train, ]
rest_fg   <- fg_data[-idx_train, ]

# 2) Split remaining 30% into 15% valid, 15% test (i.e., 50/50 of "rest"), stratified
idx_valid <- caret::createDataPartition(rest_fg$label, p = 0.50, list = FALSE)
valid_fg  <- rest_fg[idx_valid, ]
test_fg   <- rest_fg[-idx_valid, ]

# Drop unused levels just in case
train_fg[, label := droplevels(label)]
valid_fg[, label := droplevels(label)]
test_fg[,  label := droplevels(label)]

# Quick counts for visibility
cat("[FG] Class counts:\n")
print(list(
  train = table(train_fg$label),
  valid = table(valid_fg$label),
  test  = table(test_fg$label)
))

# --- Freeze categorical levels from TRAIN and encode across splits -------------
is_cat   <- vapply(train_fg[, ..features], function(x) is.character(x) || is.factor(x), logical(1))
cat_cols <- names(is_cat)[is_cat]

# Build levels map from TRAIN only
levels_map <- lapply(train_fg[, ..cat_cols], function(col) {
  if (is.factor(col)) levels(col) else sort(unique(as.character(col)))
})
saveRDS(levels_map, LEVELS_RDS)
cat(sprintf("[FG] Saved levels map -> %s\n", LEVELS_RDS))

encode_with_levels <- function(x, lv) {
  as.integer(factor(as.character(x), levels = lv))
}

# Apply encoding to all splits using train levels
for (f in features) {
  if (f %in% cat_cols) {
    lv <- levels_map[[f]]
    train_fg[[f]] <- encode_with_levels(train_fg[[f]], lv)
    valid_fg[[f]] <- encode_with_levels(valid_fg[[f]], lv)
    test_fg[[f]]  <- encode_with_levels(test_fg[[f]],  lv)
  } else {
    # Ensure numeric for safety
    train_fg[[f]] <- suppressWarnings(as.numeric(train_fg[[f]]))
    valid_fg[[f]] <- suppressWarnings(as.numeric(valid_fg[[f]]))
    test_fg[[f]]  <- suppressWarnings(as.numeric(test_fg[[f]]))
  }
}

# --- (Optional) Save the three splits for downstream evaluation ----------------
saveRDS(train_fg[, c(features, "label"), with = FALSE], file.path(SPLIT_DIR, "train.rds"))
saveRDS(valid_fg[, c(features, "label"), with = FALSE], file.path(SPLIT_DIR, "valid.rds"))
saveRDS(test_fg [, c(features, "label"), with = FALSE], file.path(SPLIT_DIR, "test.rds"))
saveRDS(features, file.path(SPLIT_DIR, "feature_names.rds"))
cat(sprintf("[FG] Saved splits to %s (train/valid/test + feature_names)\n", SPLIT_DIR))

# --- Matrices for caret::train (TRAIN only) ------------------------------------
train_df <- as.data.frame(train_fg)
test_df  <- as.data.frame(test_fg)   # used for a quick AUC sanity check
valid_df <- as.data.frame(valid_fg)  # reserved for later calibration/eval

x_train  <- as.matrix(train_df[, features, drop = FALSE])
y_train  <- train_df[["label"]]

x_test   <- as.matrix(test_df[,  features, drop = FALSE])
y_test   <- test_df[["label"]]

# --- Tuning + training (unchanged; CV happens inside caret on TRAIN) -----------
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

fg_model <- caret::train(
  x = x_train, y = y_train,
  method = "xgbTree", trControl = ctrl, tuneGrid = xgb_grid, metric = "ROC"
)

# --- Eval on TEST only (VALID remains pristine for later plots) ----------------
test_probs <- predict(fg_model, newdata = x_test, type = "prob")[, "Made"]
auc_test   <- as.numeric(pROC::auc(y_test, test_probs))

# --- Bundle + save -------------------------------------------------------------
fg_bundle <- list(
  model         = fg_model$finalModel,
  feature_names = colnames(x_train),
  trained_at    = Sys.time(),
  params        = fg_model$bestTune,
  source        = "R/models/field_goal/train_fg.R",
  meta          = list(
    years = c(min(YEARS), max(YEARS)),
    n_train = nrow(x_train),
    n_valid = nrow(valid_df),
    n_test  = nrow(x_test),
    metric = "ROC"
  ),
  eval          = list(auc_test = auc_test)
)

saveRDS(fg_bundle, MODEL_RDS)
cat(sprintf("[FG] Saved %s | AUC(test)=%.4f\n", MODEL_RDS, auc_test))
