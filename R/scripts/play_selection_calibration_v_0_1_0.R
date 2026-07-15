suppressPackageStartupMessages({
  library(dplyr); library(xgboost); library(ggplot2)
})

MODEL_DIR    <- "src/nfl_sim/models/play_selection_v_0_1_0"
DATA_PATH    <- "data/model_1_training_data.csv"
PLOT_DIR     <- "docs/eda_outputs/play_selection/calibration_plots"
TEST_SEASONS <- 2024:2025
FEATURES     <- c("yardline_100","game_seconds_remaining","score_differential",
                  "posteam_timeouts_remaining","defteam_timeouts_remaining","leverage")

dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

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

# Helper: build calibration data frame from predictions + labels
calibration_df <- function(preds, labels, n_bins = 10) {
  df <- data.frame(pred = preds, actual = labels)
  df$bin <- cut(df$pred, breaks = seq(0, 1, length.out = n_bins + 1),
                include.lowest = TRUE)
  df %>%
    group_by(bin) %>%
    summarise(
      n           = n(),
      mean_pred   = mean(pred),
      actual_rate = mean(actual),
      .groups     = "drop"
    ) %>%
    filter(!is.na(bin))
}

# Helper: build and save a calibration ggplot
save_cal_plot <- function(cal_df, title, filename) {
  p <- ggplot(cal_df, aes(x = mean_pred, y = actual_rate)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "grey50", linewidth = 0.8) +
    geom_point(aes(size = n), color = "#2c7bb6", alpha = 0.85) +
    geom_line(color = "#2c7bb6", linewidth = 0.7) +
    scale_size_continuous(name = "# Plays", range = c(2, 8)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1),
                       labels = scales::percent_format()) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1),
                       labels = scales::percent_format()) +
    labs(
      title    = title,
      subtitle = "Dashed line = perfect calibration. Points above line = model under-predicts passes.",
      x        = "Predicted Pass Probability",
      y        = "Actual Pass Rate"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      panel.grid.minor = element_blank()
    )
  ggsave(file.path(PLOT_DIR, filename), p, width = 7, height = 5.5, dpi = 150)
  cat("  Saved:", filename, "\n")
}

# ── 1. Overall Calibration (all buckets combined) ─────────────────────────────
cat("Building overall calibration...\n")
all_preds <- all_labels <- c()
for (bucket in unique(plays$dist_bucket)) {
  mp <- file.path(MODEL_DIR, paste0(bucket, ".json"))
  if (!file.exists(mp)) next
  td  <- plays %>% filter(dist_bucket == bucket, season %in% TEST_SEASONS)
  if (nrow(td) < 10) next
  m   <- xgb.load(mp)
  all_preds  <- c(all_preds,  predict(m, xgb.DMatrix(as.matrix(td[, FEATURES]))))
  all_labels <- c(all_labels, td$is_pass)
}
cal_overall <- calibration_df(all_preds, all_labels)
save_cal_plot(cal_overall,
              "Overall Calibration — Play Type Selection V.0.1.0\n(All 11 Submodels, 2024–2025 Test Set)",
              "calibration_overall.png")

# ── 2. Submodel: 1_10 (largest, near 50/50 — most interesting) ───────────────
cat("Building 1_10 calibration...\n")
td  <- plays %>% filter(dist_bucket == "1_10", season %in% TEST_SEASONS)
m   <- xgb.load(file.path(MODEL_DIR, "1_10.json"))
p   <- predict(m, xgb.DMatrix(as.matrix(td[, FEATURES])))
save_cal_plot(calibration_df(p, td$is_pass),
              "Calibration: 1st & 10 Submodel (n = 27,147)",
              "calibration_1_10.png")

# ── 3. Submodel: 3_short (weakest model — most likely to show drift) ──────────
cat("Building 3_short calibration...\n")
td  <- plays %>% filter(dist_bucket == "3_short", season %in% TEST_SEASONS)
m   <- xgb.load(file.path(MODEL_DIR, "3_short.json"))
p   <- predict(m, xgb.DMatrix(as.matrix(td[, FEATURES])))
save_cal_plot(calibration_df(p, td$is_pass),
              "Calibration: 3rd & Short Submodel (n = 4,195)\n⚠️  Weakest ROC-AUC (0.611) — watch for miscalibration",
              "calibration_3_short.png")

# ── 4. Submodel: 2_short (best logloss improvement — a model doing real work) ─
cat("Building 2_short calibration...\n")
td  <- plays %>% filter(dist_bucket == "2_short", season %in% TEST_SEASONS)
m   <- xgb.load(file.path(MODEL_DIR, "2_short.json"))
p   <- predict(m, xgb.DMatrix(as.matrix(td[, FEATURES])))
save_cal_plot(calibration_df(p, td$is_pass),
              "Calibration: 2nd & Short Submodel (n = 3,785)\nBest LogLoss Improvement (+0.071)",
              "calibration_2_short.png")

cat("\nAll calibration plots saved to:", PLOT_DIR, "\n")
