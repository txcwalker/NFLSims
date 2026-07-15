###############################################################################
# Play Type Selection — Model Training Script
# Version: V.0.1.0
# Last Updated: May 2026
#
# Purpose:
#   Train 11 XGBoost binary classifiers (one per down/distance bucket) to
#   predict whether an offensive play will be a run or a pass.
#   See docs/models/play_selection_v_0_1_0.md for full design rationale.
#
# Architecture: 11 submodels
#   1_10, 1_short, 1_long
#   2_short, 2_med, 2_long
#   3_short, 3_med, 3_long
#   4_short, 4_med_long  (4_med + 4_long merged per EDA Q2 decision)
#
# Features (Section 5.1 of model doc):
#   yardline_100, game_seconds_remaining, score_differential,
#   posteam_timeouts_remaining, defteam_timeouts_remaining,
#   leverage  (= score_differential * game_seconds_remaining)
#
# Input:  data/model_1_training_data.csv
# Output: src/nfl_sim/models/play_selection_v2_1_0/ (one .json per bucket)
#         docs/eda_outputs/play_selection/training_summary_v_0_1_0.csv
#
# Split Strategy: Season-based 70/15/15
#   Seasons 2015-2022 → Train (8 seasons, 72.7%)
#   Season  2023      → Validation (1 season, ~9.1%)
#   Season  2024-2025 → Test (2 seasons, ~18.2%)
#   Seasons assigned deterministically to prevent data leakage across games.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(xgboost)
})

# ── Config ────────────────────────────────────────────────────────────────────
DATA_PATH   <- "data/model_1_training_data.csv"
MODEL_DIR   <- "src/nfl_sim/models/play_selection_v_0_1_0"
SUMMARY_OUT <- "docs/eda_outputs/play_selection/training_summary_v_0_1_0.csv"

dir.create(MODEL_DIR,   recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(SUMMARY_OUT), recursive = TRUE, showWarnings = FALSE)

TRAIN_SEASONS <- 2015:2022   # 8 seasons
VALID_SEASONS <- 2023        # 1 season — used for early stopping
TEST_SEASONS  <- 2024:2025   # 2 seasons — held out for final evaluation

FEATURES <- c(
  "yardline_100",
  "game_seconds_remaining",
  "score_differential",
  "posteam_timeouts_remaining",
  "defteam_timeouts_remaining",
  "leverage"
)
TARGET <- "is_pass"

# XGBoost hyperparameters — conservative starting point for V.0.1.0
# These will be tuned in a future version after evaluating baseline performance.
XGB_PARAMS <- list(
  objective        = "binary:logistic",
  eval_metric      = "logloss",     # Log loss: correct loss for probability output
  eta              = 0.05,          # Learning rate — slow and steady to avoid overfitting
  max_depth        = 5,             # Shallow enough to avoid overfitting on smaller buckets
  subsample        = 0.8,           # Row subsampling per tree
  colsample_bytree = 0.8,           # Feature subsampling per tree
  min_child_weight = 10,            # Require 10+ plays in each leaf — important for smaller buckets
  nthread          = 4
)
MAX_ROUNDS     <- 500
EARLY_STOP     <- 30               # Stop if validation logloss doesn't improve for 30 rounds

cat("=============================================================\n")
cat("  NFLSims — Play Type Selection Training V.0.1.0\n")
cat("=============================================================\n\n")

# ── Load & Engineer Features ──────────────────────────────────────────────────
cat("Loading data...\n")
raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE)
cat("  Raw rows:", nrow(raw), "\n")

plays <- raw %>%
  filter(
    play_type %in% c("run", "pass"),
    !is.na(down), !is.na(ydstogo),
    !is.na(game_seconds_remaining), !is.na(score_differential),
    !is.na(yardline_100),
    !is.na(posteam_timeouts_remaining), !is.na(defteam_timeouts_remaining),
    !is.na(season)
  ) %>%
  mutate(
    is_pass  = as.numeric(play_type == "pass"),
    # Leverage interaction term: encodes urgency (score × time remaining)
    # Trailing with little time = large negative value → high pass rate
    leverage = score_differential * game_seconds_remaining,
    # ── Bucket Assignment (11 submodels per EDA Q2 decision) ──────────────
    dist_bucket = case_when(
      ydstogo == 10 & down == 1               ~ "1_10",
      ydstogo >  10 & down == 1               ~ "1_long",
      ydstogo <  10 & down == 1               ~ "1_short",
      down == 2 & ydstogo <= 3                ~ "2_short",
      down == 2 & ydstogo >= 4 & ydstogo <= 6 ~ "2_med",
      down == 2 & ydstogo >= 7                ~ "2_long",
      down == 3 & ydstogo <= 3                ~ "3_short",
      down == 3 & ydstogo >= 4 & ydstogo <= 6 ~ "3_med",
      down == 3 & ydstogo >= 7                ~ "3_long",
      down == 4 & ydstogo <= 3                ~ "4_short",
      down == 4 & ydstogo >= 4                ~ "4_med_long",  # 4_med + 4_long merged
      TRUE                                    ~ "other"
    )
  ) %>%
  filter(dist_bucket != "other")

cat("  Filtered plays:", nrow(plays), "\n")
cat("  Train seasons:", paste(TRAIN_SEASONS, collapse=", "), "\n")
cat("  Valid season: ", VALID_SEASONS, "\n")
cat("  Test seasons: ", paste(TEST_SEASONS, collapse=", "), "\n\n")

# ── Train All 11 Submodels ────────────────────────────────────────────────────
buckets <- sort(unique(plays$dist_bucket))
cat("Buckets to train:", paste(buckets, collapse=", "), "\n\n")

summary_rows <- list()

for (bucket in buckets) {
  cat("────────────────────────────────────────────────\n")
  cat("  Training bucket:", bucket, "\n")

  bdata <- plays %>% filter(dist_bucket == bucket)

  train_df <- bdata %>% filter(season %in% TRAIN_SEASONS)
  valid_df  <- bdata %>% filter(season %in% VALID_SEASONS)
  test_df   <- bdata %>% filter(season %in% TEST_SEASONS)

  cat("  n_train:", nrow(train_df),
      "| n_valid:", nrow(valid_df),
      "| n_test:", nrow(test_df), "\n")

  if (nrow(train_df) < 100 || nrow(valid_df) < 20) {
    cat("  ⚠️  Insufficient data — skipping bucket\n\n")
    next
  }

  dtrain <- xgb.DMatrix(
    data  = as.matrix(train_df[, FEATURES]),
    label = train_df[[TARGET]]
  )
  dvalid <- xgb.DMatrix(
    data  = as.matrix(valid_df[, FEATURES]),
    label = valid_df[[TARGET]]
  )
  dtest <- xgb.DMatrix(
    data  = as.matrix(test_df[, FEATURES]),
    label = test_df[[TARGET]]
  )

  # ── Train with early stopping on validation logloss ──────────────────────
  model <- xgb.train(
    params    = XGB_PARAMS,
    data      = dtrain,
    nrounds   = MAX_ROUNDS,
    watchlist = list(train = dtrain, valid = dvalid),
    early_stopping_rounds = EARLY_STOP,
    verbose   = 0   # Set to 1 to see round-by-round output
  )

  # ── Evaluate on test set ──────────────────────────────────────────────────
  test_preds <- predict(model, dtest)
  test_labels <- test_df[[TARGET]]

  # Log Loss
  eps      <- 1e-15
  logloss  <- -mean(test_labels * log(pmax(test_preds, eps)) +
                    (1 - test_labels) * log(pmax(1 - test_preds, eps)))
  # Brier Score
  brier    <- mean((test_preds - test_labels)^2)
  # Accuracy (threshold 0.5)
  accuracy <- mean((test_preds > 0.5) == test_labels)
  # Baseline logloss (always predict overall pass rate)
  base_rate <- mean(train_df[[TARGET]])
  base_loss <- -mean(test_labels * log(base_rate) +
                     (1 - test_labels) * log(1 - base_rate))

  cat("  Best round:   ", model$best_iteration, "\n")
  cat("  Test LogLoss: ", round(logloss, 4),
      "  (Baseline:", round(base_loss, 4), ")\n")
  cat("  Test Brier:   ", round(brier, 4), "\n")
  cat("  Test Accuracy:", round(accuracy, 4), "\n\n")

  # ── Save model ────────────────────────────────────────────────────────────
  model_path <- file.path(MODEL_DIR, paste0(bucket, ".json"))
  xgb.save(model, model_path)
  cat("  Saved:", model_path, "\n\n")

  summary_rows[[bucket]] <- data.frame(
    bucket       = bucket,
    n_train      = nrow(train_df),
    n_valid      = nrow(valid_df),
    n_test       = nrow(test_df),
    best_round   = model$best_iteration,
    test_logloss = round(logloss, 4),
    baseline_logloss = round(base_loss, 4),
    logloss_improvement = round(base_loss - logloss, 4),
    test_brier   = round(brier, 4),
    test_accuracy = round(accuracy, 4),
    stringsAsFactors = FALSE
  )
}

# ── Write Training Summary ────────────────────────────────────────────────────
summary_df <- do.call(rbind, summary_rows)
write.csv(summary_df, SUMMARY_OUT, row.names = FALSE)

cat("=============================================================\n")
cat("  TRAINING COMPLETE\n")
cat("=============================================================\n\n")
cat("  Models saved to:", MODEL_DIR, "\n")
cat("  Summary written to:", SUMMARY_OUT, "\n\n")
cat("  Per-Bucket Results:\n")
print(summary_df[, c("bucket","n_train","test_logloss","baseline_logloss",
                     "logloss_improvement","test_brier","test_accuracy")],
      row.names = FALSE)
