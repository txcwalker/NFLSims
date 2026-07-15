suppressPackageStartupMessages({
  library(dplyr)
  library(xgboost)
  library(pROC)   # for ROC-AUC
})

MODEL_DIR <- "src/nfl_sim/models/play_selection_v_0_1_0"
DATA_PATH <- "data/model_1_training_data.csv"

TRAIN_SEASONS <- 2015:2022
TEST_SEASONS  <- 2024:2025

FEATURES <- c(
  "yardline_100", "game_seconds_remaining", "score_differential",
  "posteam_timeouts_remaining", "defteam_timeouts_remaining", "leverage"
)

raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE)

plays <- raw %>%
  filter(play_type %in% c("run", "pass"),
         !is.na(down), !is.na(ydstogo), !is.na(game_seconds_remaining),
         !is.na(score_differential), !is.na(yardline_100),
         !is.na(posteam_timeouts_remaining), !is.na(defteam_timeouts_remaining),
         !is.na(season)) %>%
  mutate(
    is_pass  = as.numeric(play_type == "pass"),
    leverage = score_differential * game_seconds_remaining,
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
      down == 4 & ydstogo >= 4                ~ "4_med_long",
      TRUE                                    ~ "other"
    )
  ) %>% filter(dist_bucket != "other")

bucket_order <- c("1_10","1_short","1_long","2_short","2_med","2_long",
                  "3_short","3_med","3_long","4_short","4_med_long")

cat("\n=== Play Type Selection V.0.1.0 — Evaluation Summary ===\n\n")
cat(sprintf("%-12s  %6s  %8s  %8s  %8s  %8s  %9s\n",
            "Bucket","N_Test","LogLoss","Baseline","Improve","Brier","ROC-AUC"))
cat(strrep("-", 72), "\n")

rows <- list()

for (bucket in bucket_order) {
  model_path <- file.path(MODEL_DIR, paste0(bucket, ".json"))
  if (!file.exists(model_path)) next

  bdata     <- plays %>% filter(dist_bucket == bucket)
  train_df  <- bdata %>% filter(season %in% TRAIN_SEASONS)
  test_df   <- bdata %>% filter(season %in% TEST_SEASONS)
  if (nrow(test_df) < 10) next

  model  <- xgb.load(model_path)
  dtest  <- xgb.DMatrix(as.matrix(test_df[, FEATURES]), label = test_df$is_pass)
  preds  <- predict(model, dtest)
  labels <- test_df$is_pass

  base_rate <- mean(train_df$is_pass)
  eps       <- 1e-15
  logloss   <- -mean(labels * log(pmax(preds, eps)) + (1 - labels) * log(pmax(1 - preds, eps)))
  base_loss <- -mean(labels * log(base_rate) + (1 - labels) * log(1 - base_rate))
  improve   <- base_loss - logloss
  brier     <- mean((preds - labels)^2)
  roc_auc   <- as.numeric(auc(roc(labels, preds, quiet = TRUE)))

  cat(sprintf("%-12s  %6d  %8.4f  %8.4f  %+8.4f  %8.4f  %9.4f\n",
              bucket, nrow(test_df), logloss, base_loss, improve, brier, roc_auc))

  rows[[bucket]] <- data.frame(bucket, n_test = nrow(test_df),
    logloss = round(logloss,4), baseline = round(base_loss,4),
    improvement = round(improve,4), brier = round(brier,4),
    roc_auc = round(roc_auc,4), stringsAsFactors = FALSE)
}

cat(strrep("-", 72), "\n")
out <- do.call(rbind, rows)
write.csv(out, "docs/eda_outputs/play_selection/evaluation_summary_v_0_1_0.csv", row.names = FALSE)
cat("\nSaved to: docs/eda_outputs/play_selection/evaluation_summary_v_0_1_0.csv\n")
