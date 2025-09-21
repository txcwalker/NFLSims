# R/eval/evaluate_wp_model.R
# Evaluation tailored for Win Probability bundles (your saved RDS lists).
# Uses bundle$model (xgb.Booster), bundle$feature_names, and a holdout df
# (defaults to data/wp/valid.rds) to compute metrics and plots:
# Brier, LogLoss, ECE, Calibration slope/intercept, ROC/AUC, PR/AUPRC,
# and a decile reliability table.

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(pROC); library(PRROC); library(broom)
  library(xgboost); library(readr)
})

# ---------- Utils ----------
`%||%` <- function(x, y) if (is.null(x)) y else x

.dir_create <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

load_bundle <- function(bundle_or_path) {
  if (is.list(bundle_or_path)) return(bundle_or_path)
  stopifnot(file.exists(bundle_or_path))
  readRDS(bundle_or_path)
}

pick_holdout <- function(df, split_dir, default_split = "valid.rds") {
  if (!is.null(df)) return(df)
  if (!is.null(split_dir)) {
    cand <- file.path(split_dir, default_split)
    if (file.exists(cand)) return(readRDS(cand))
  }
  stop("No data provided: pass df=... or a valid split_dir containing ", default_split)
}

brier_score <- function(y_true, y_prob) mean((y_prob - y_true)^2, na.rm = TRUE)

log_loss <- function(y_true, y_prob, eps = 1e-15) {
  p <- pmin(pmax(y_prob, eps), 1 - eps)
  -mean(y_true * log(p) + (1 - y_true) * log(1 - p), na.rm = TRUE)
}

expected_calibration_error <- function(y_true, y_prob, n_bins = 10) {
  df <- data.frame(y_true = y_true, y_prob = y_prob) %>%
    mutate(bin = cut(y_prob, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE))
  byb <- df %>% group_by(bin) %>% summarise(n = n(), conf = mean(y_prob), acc = mean(y_true), .groups = "drop")
  sum(byb$n / sum(byb$n) * abs(byb$acc - byb$conf))
}

plot_calibration <- function(y_true, y_prob, out_file = NULL, n_bins = 10, title = "WP Calibration") {
  df <- data.frame(y_true = y_true, y_prob = y_prob) %>%
    mutate(bin = cut(y_prob, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE))
  by_bin <- df %>% group_by(bin) %>% summarise(conf = mean(y_prob), acc = mean(y_true), n = n(), .groups = "drop")
  g <- ggplot(by_bin, aes(conf, acc)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    geom_point(aes(size = n), alpha = 0.85) +
    geom_line() +
    coord_equal(xlim = c(0,1), ylim = c(0,1)) +
    labs(title = title, x = "Mean predicted WP", y = "Empirical win rate", size = "Bin N") +
    theme_minimal(base_size = 12)
  if (!is.null(out_file)) ggsave(out_file, g, width = 6, height = 6, dpi = 150)
  g
}

# ---------- Main ----------
# Args:
#   bundle_or_path : list (bundle) or path to bundle .rds with $model and $feature_names
#   df             : (optional) data.frame holding features + 'Winner' (0/1)
#   split_dir      : (optional) directory holding 'valid.rds' (default) or 'test.rds'
#   outcome_col    : name of binary outcome column (default "Winner")
#   output_dir     : where to write plots/summary (defaults near bundle if path; else "R/eval/outputs")
#   tag            : label suffix for files
#   n_calib_bins   : calibration bins
#   split_file     : which file inside split_dir to use (e.g., "valid.rds" or "test.rds")
evaluate_wp_model <- function(
  bundle_or_path,
  df = NULL,
  split_dir = "data/wp",
  outcome_col = "Winner",
  output_dir = NULL,
  tag = "wp_eval",
  n_calib_bins = 10,
  split_file = "valid.rds"
) {
  bundle <- load_bundle(bundle_or_path)

  if (is.null(df)) df <- pick_holdout(NULL, split_dir, default_split = split_file)
  stopifnot(outcome_col %in% names(df))

  # Ensure predictable subsetting even if holdout is a data.table
  df <- as.data.frame(df)

  feature_names <- bundle$feature_names
  stopifnot(all(feature_names %in% names(df)))

  # Numeric matrix for xgb
  X <- as.matrix(df[, feature_names, drop = FALSE])
  y_true <- as.integer(df[[outcome_col]])

  # Predict probabilities
  if (inherits(bundle$model, "xgb.Booster")) {
    y_prob <- predict(bundle$model, newdata = xgboost::xgb.DMatrix(X))
  } else {
    # Fallback for other model types saved into the bundle
    y_prob <- as.numeric(predict(bundle$model, newdata = df[, feature_names, drop = FALSE], type = "prob")[, 2])
  }

  # Metrics
  brier <- brier_score(y_true, y_prob)
  ll    <- log_loss(y_true, y_prob)
  ece   <- expected_calibration_error(y_true, y_prob, n_calib_bins)

  # Calibration slope/intercept
  df_fit <- data.frame(y_true = y_true, logit = qlogis(pmin(pmax(y_prob, 1e-6), 1 - 1e-6)))
  glm_fit <- glm(y_true ~ logit, data = df_fit, family = binomial())
  cal_coefs <- broom::tidy(glm_fit)

  # ROC / PR
  roc_obj <- pROC::roc(response = y_true, predictor = y_prob, quiet = TRUE, direction = "<")
  auc_val <- as.numeric(pROC::auc(roc_obj))
  pr      <- PRROC::pr.curve(scores.class0 = y_prob[y_true == 1], scores.class1 = y_prob[y_true == 0], curve = TRUE)
  auprc   <- as.numeric(pr$auc.integral)

  # Output dir
  if (is.null(output_dir)) {
    if (is.character(bundle_or_path) && file.exists(bundle_or_path)) {
      output_dir <- file.path(dirname(bundle_or_path), "eval_outputs")
    } else {
      output_dir <- "R/eval/outputs"
    }
  }
  .dir_create(output_dir)

  # Plots
  plot_calibration(y_true, y_prob,
                   file.path(output_dir, paste0("WP_Calibration_", tag, ".png")),
                   n_bins = n_calib_bins,
                   title = "Win Probability Calibration")

  roc_df <- data.frame(tpr = rev(roc_obj$sensitivities), fpr = rev(1 - roc_obj$specificities))
  g_roc <- ggplot(roc_df, aes(fpr, tpr)) +
    geom_abline(linetype = 2) + geom_line() +
    labs(title = paste0("WP ROC (AUC = ", round(auc_val, 4), ")"), x = "FPR", y = "TPR") +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0("WP_ROC_", tag, ".png")), g_roc, width = 6, height = 6, dpi = 150)

  pr_df <- data.frame(recall = pr$curve[,1], precision = pr$curve[,2])
  g_pr <- ggplot(pr_df, aes(recall, precision)) + geom_line() +
    labs(title = paste0("WP PR (AUPRC = ", round(auprc, 4), ")"), x = "Recall", y = "Precision") +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0("WP_PR_", tag, ".png")), g_pr, width = 6, height = 6, dpi = 150)

  # Decile reliability
  rel_tbl <- data.frame(y_true = y_true, y_prob = y_prob) %>%
    mutate(decile = ntile(y_prob, 10)) %>%
    group_by(decile) %>%
    summarise(n = n(), mean_wp = mean(y_prob), win_rate = mean(y_true), .groups = "drop") %>%
    arrange(decile)

  # Summary text
  sink(file.path(output_dir, paste0("WP_Summary_", tag, ".txt")))
  cat("=== Win Probability Model Evaluation ===\n")
  cat("Tag: ", tag, "\n\n")
  cat("AUC:   ", round(auc_val, 6), "\n")
  cat("AUPRC: ", round(auprc, 6), "\n")
  cat("Brier: ", round(brier, 6), "\n")
  cat("LogLoss:", round(ll, 6), "\n")
  cat("ECE:   ", round(ece, 6), " (", n_calib_bins, " bins)\n\n", sep = "")
  cat("-- Calibration Slope/Intercept (glm(y ~ logit(p))) --\n")
  print(cal_coefs); cat("\n")
  cat("-- Decile Reliability --\n")
  print(rel_tbl)
  sink()

  invisible(list(
    auc = auc_val, auprc = auprc, brier = brier, logloss = ll, ece = ece,
    calibration_glm = glm_fit, reliability = rel_tbl,
    output_dir = output_dir
  ))
}

# -----------------------------
# Example usage:
# -----------------------------
# wp_bundle_path <- "R/models/win_probability/wp_model.rds"          # or "wp_clutch_model.rds"
# res_wp <- evaluate_wp_model(
#   bundle_or_path = wp_bundle_path,
#   split_dir = "data/wp",                    # or "data/wp_clutch"
#   outcome_col = "Winner",
#   tag = "wp_valid",
#   split_file = "valid.rds"                  # or "test.rds"
# )
