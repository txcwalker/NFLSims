suppressPackageStartupMessages({ library(dplyr); library(xgboost); library(pROC) })

MODEL_DIR <- "src/nfl_sim/models/play_selection_v_0_1_0"
DATA_PATH <- "data/model_1_training_data.csv"
TRAIN_SEASONS <- 2015:2022
TEST_SEASONS  <- 2024:2025
FEATURES <- c("yardline_100","game_seconds_remaining","score_differential",
              "posteam_timeouts_remaining","defteam_timeouts_remaining","leverage")

raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE)
plays <- raw %>%
  filter(play_type %in% c("run","pass"), !is.na(down), !is.na(ydstogo),
         !is.na(game_seconds_remaining), !is.na(score_differential),
         !is.na(yardline_100), !is.na(posteam_timeouts_remaining),
         !is.na(defteam_timeouts_remaining), !is.na(season)) %>%
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
      TRUE ~ "other")) %>%
  filter(dist_bucket != "other")

# ── Collect test predictions across all buckets ───────────────────────────────
all_preds   <- c()
all_labels  <- c()
train_pass_rate <- mean(plays %>% filter(season %in% TRAIN_SEASONS) %>% pull(is_pass))

for (bucket in unique(plays$dist_bucket)) {
  model_path <- file.path(MODEL_DIR, paste0(bucket, ".json"))
  if (!file.exists(model_path)) next
  test_df <- plays %>% filter(dist_bucket == bucket, season %in% TEST_SEASONS)
  if (nrow(test_df) < 10) next
  model  <- xgb.load(model_path)
  dtest  <- xgb.DMatrix(as.matrix(test_df[, FEATURES]))
  preds  <- predict(model, dtest)
  all_preds  <- c(all_preds, preds)
  all_labels <- c(all_labels, test_df$is_pass)
}

# ── Compute overall metrics ───────────────────────────────────────────────────
eps        <- 1e-15
logloss    <- -mean(all_labels * log(pmax(all_preds, eps)) + (1-all_labels) * log(pmax(1-all_preds, eps)))
base_loss  <- -mean(all_labels * log(train_pass_rate) + (1-all_labels) * log(1-train_pass_rate))
brier      <- mean((all_preds - all_labels)^2)
brier_base <- train_pass_rate * (1 - train_pass_rate)   # Brier baseline = p*(1-p)
roc_auc    <- as.numeric(auc(roc(all_labels, all_preds, quiet = TRUE)))
accuracy   <- mean((all_preds > 0.5) == all_labels)
n_test     <- length(all_labels)

cat("\n=== Play Type Selection V.0.1.0 — OVERALL Test Set Performance ===\n")
cat("  Test plays (2024-2025):", n_test, "\n")
cat("  Overall pass rate in test set:", round(mean(all_labels), 4), "\n\n")
cat(sprintf("  %-22s %8s %8s %8s\n", "Metric", "Model", "Baseline", "Better?"))
cat(strrep("-", 52), "\n")
cat(sprintf("  %-22s %8.4f %8.4f %8s\n", "LogLoss",   logloss,   base_loss,  ifelse(logloss < base_loss, "✅ Yes", "❌ No")))
cat(sprintf("  %-22s %8.4f %8.4f %8s\n", "Brier Score", brier,   brier_base, ifelse(brier   < brier_base,"✅ Yes", "❌ No")))
cat(sprintf("  %-22s %8.4f %8s %8s\n",   "ROC-AUC",  roc_auc,   "  0.5000", ifelse(roc_auc > 0.5,      "✅ Yes", "❌ No")))
cat(sprintf("  %-22s %8.4f\n",            "Accuracy",  accuracy))
cat(strrep("-", 52), "\n")
