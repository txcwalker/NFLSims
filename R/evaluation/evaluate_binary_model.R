# R/eval/evaluate_binary_model.R
# Generic evaluator for your binary bundles (Field Goal, Fourth Down).
# Works with bundle RDS that contains $model (xgb.Booster) + $feature_names.
# Accepts a holdout df (or loads data/*/valid.rds by default) and computes:
# ROC/AUC, PR/AUPRC, Calibration (+ECE), Confusion Matrices (0.50 & Youden),
# LogLoss, Brier, and Feature Importance (xgboost-native if available).

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(pROC); library(caret)
  library(PRROC); library(scales); library(xgboost); library(readr)
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

youden_threshold <- function(y_true, y_prob) {
  roc_obj <- pROC::roc(response = y_true, predictor = y_prob, quiet = TRUE, direction = "<")
  coords <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")
  list(threshold = as.numeric(coords["threshold"]),
       sensitivity = as.numeric(coords["sensitivity"]),
       specificity = as.numeric(coords["specificity"]),
       roc = roc_obj)
}

plot_calibration <- function(y_true, y_prob, out_file = NULL, n_bins = 10, title = "Calibration (Reliability)") {
  df <- data.frame(y_true = y_true, y_prob = y_prob) %>%
    mutate(bin = cut(y_prob, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE))
  byb <- df %>% group_by(bin) %>%
    summarise(conf = mean(y_prob), acc = mean(y_true), n = n(), .groups = "drop")
  g <- ggplot(byb, aes(x = conf, y = acc)) +
    geom_abline(linetype = 2) + geom_point(aes(size = n), alpha = 0.8) + geom_line() +
    coord_equal(xlim = c(0,1), ylim = c(0,1)) +
    labs(title = title, x = "Mean predicted probability", y = "Empirical outcome rate", size = "Bin N") +
    theme_minimal(base_size = 12)
  if (!is.null(out_file)) ggsave(out_file, g, width = 6, height = 6, dpi = 150)
  g
}

plot_roc <- function(roc_obj, out_file = NULL, title_prefix = "ROC Curve") {
  df <- data.frame(tpr = rev(roc_obj$sensitivities), fpr = rev(1 - roc_obj$specificities))
  auc_val <- pROC::auc(roc_obj)
  g <- ggplot(df, aes(x = fpr, y = tpr)) +
    geom_abline(linetype = 2) + geom_line() +
    labs(title = paste0(title_prefix, " (AUC = ", round(as.numeric(auc_val), 4), ")"),
         x = "False Positive Rate", y = "True Positive Rate") +
    theme_minimal(base_size = 12)
  if (!is.null(out_file)) ggsave(out_file, g, width = 6, height = 6, dpi = 150)
  list(auc = as.numeric(auc_val), plot = g)
}

plot_pr <- function(y_true, y_prob, out_file = NULL, title_prefix = "Precision-Recall Curve") {
  pr <- PRROC::pr.curve(scores.class0 = y_prob[y_true == 1],
                        scores.class1 = y_prob[y_true == 0],
                        curve = TRUE)
  df <- data.frame(recall = pr$curve[,1], precision = pr$curve[,2])
  g <- ggplot(df, aes(x = recall, y = precision)) +
    geom_line() +
    labs(title = paste0(title_prefix, " (AUPRC = ", round(pr$auc.integral, 4), ")"),
         x = "Recall", y = "Precision") +
    theme_minimal(base_size = 12)
  if (!is.null(out_file)) ggsave(out_file, g, width = 6, height = 6, dpi = 150)
  list(auprc = as.numeric(pr$auc.integral), plot = g)
}

compute_feature_importance <- function(model, feature_names, top_n = 30, out_file = NULL) {
  imp <- NULL
  if (inherits(model, "xgb.Booster")) {
    imp <- xgb.importance(model = model, feature_names = feature_names) %>%
      as.data.frame() %>%
      dplyr::rename(feature = Feature, gain = Gain, cover = Cover, freq = Frequency)
  } else {
    vi <- tryCatch(varImp(model)$importance, error = function(e) NULL)
    if (!is.null(vi)) {
      imp <- vi %>%
        tibble::rownames_to_column("feature") %>%
        arrange(desc(Overall)) %>%
        rename(gain = Overall)
    }
  }
  if (is.null(imp) || !nrow(imp)) return(NULL)
  top <- imp %>% head(top_n)
  g <- ggplot(top, aes(x = reorder(feature, gain), y = gain)) +
    geom_col() + coord_flip() +
    labs(title = "Feature Importance", x = NULL, y = "Importance (gain or overall)") +
    theme_minimal(base_size = 12)
  if (!is.null(out_file)) ggsave(out_file, g, width = 7, height = 8, dpi = 150)
  list(table = imp, plot = g)
}

# ---------- Main ----------
# Arguments documented in your version.
evaluate_binary_model <- function(
  bundle_or_path,
  df = NULL,
  split_dir = NULL,
  feature_names = NULL,
  outcome_col = "label",
  positive_label = NULL,
  output_dir = NULL,
  tag = "model_eval",
  n_calib_bins = 10,
  split_file = "valid.rds"
) {
  bundle <- load_bundle(bundle_or_path)

  if (is.null(df)) {
    if (is.null(split_dir)) {
      # try to infer from common patterns
      split_dir <- if (grepl("field_goal", tolower(bundle$source %||% ""))) "data/fg"
      else if (grepl("fourth_down", tolower(bundle$source %||% ""))) "data/fd"
      else "data"
    }
    df <- pick_holdout(NULL, split_dir, default_split = split_file)
  }
  stopifnot(outcome_col %in% names(df))

  # Make sure downstream subsetting behaves even if the split was a data.table
  df <- as.data.frame(df)

  if (is.null(feature_names)) feature_names <- bundle$feature_names
  stopifnot(all(feature_names %in% names(df)))

  X <- as.matrix(df[, feature_names, drop = FALSE])

  # Standardize y_true to 0/1
  y_col <- df[[outcome_col]]
  if (is.factor(y_col) || is.character(y_col)) {
    y_fac <- factor(as.character(y_col))
    if (!is.null(positive_label)) {
      stopifnot(positive_label %in% levels(y_fac))
      y_true <- as.integer(y_fac == positive_label)
    } else {
      # fall back: first level as positive (matches your training convention)
      y_true <- as.integer(y_fac == levels(y_fac)[1])
      positive_label <- levels(y_fac)[1]
    }
  } else {
    y_true <- as.integer(y_col)
    if (is.null(positive_label)) positive_label <- "1"
  }

  # Predict proba
  if (inherits(bundle$model, "xgb.Booster")) {
    y_prob <- predict(bundle$model, newdata = xgboost::xgb.DMatrix(X))
  } else {
    y_prob <- as.numeric(predict(bundle$model, newdata = df[, feature_names, drop = FALSE], type = "prob")[, 2])
  }

  # Metrics
  brier <- brier_score(y_true, y_prob)
  ll    <- log_loss(y_true, y_prob)
  ece   <- expected_calibration_error(y_true, y_prob, n_calib_bins)

  yt <- youden_threshold(y_true, y_prob)
  thr_opt <- yt$threshold
  roc_obj <- yt$roc

  cm_50  <- caret::confusionMatrix(as.factor(as.integer(y_prob >= 0.5)),
                                   as.factor(y_true), positive = "1")
  cm_opt <- caret::confusionMatrix(as.factor(as.integer(y_prob >= thr_opt)),
                                   as.factor(y_true), positive = "1")

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
  roc_out <- plot_roc(roc_obj, file.path(output_dir, paste0("ROC_", tag, ".png")))
  pr_out  <- plot_pr(y_true, y_prob, file.path(output_dir, paste0("PR_", tag, ".png")))
  plot_calibration(y_true, y_prob,
                   file.path(output_dir, paste0("Calibration_", tag, ".png")),
                   n_bins = n_calib_bins)

  # Feature Importance
  fi <- compute_feature_importance(bundle$model, feature_names,
                                   out_file = file.path(output_dir, paste0("FeatureImportance_", tag, ".png")))

  # Summary
  sink(file.path(output_dir, paste0("Summary_", tag, ".txt")))
  cat("=== Binary Model Evaluation ===\n")
  cat("Tag: ", tag, "\n\n")
  cat("Positive class: ", positive_label, "\n\n", sep = "")
  cat("AUC:   ", round(roc_out$auc, 6), "\n")
  cat("AUPRC: ", round(pr_out$auprc, 6), "\n")
  cat("Brier: ", round(brier, 6), "\n")
  cat("LogLoss:", round(ll, 6), "\n")
  cat("ECE:   ", round(ece, 6), " (", n_calib_bins, " bins)\n\n", sep = "")
  cat("-- Thresholds --\n")
  cat("0.50 Confusion Matrix:\n"); print(cm_50); cat("\n")
  cat(sprintf("Youden-opt threshold: %.6f\n", thr_opt))
  cat("Youden Confusion Matrix:\n"); print(cm_opt); cat("\n")
  if (!is.null(fi)) {
    cat("-- Top Feature Importance (head) --\n")
    print(utils::head(fi$table, 20))
  } else {
    cat("-- Feature importance not available for this model type --\n")
  }
  sink()

  invisible(list(
    auc = roc_out$auc,
    auprc = pr_out$auprc,
    brier = brier,
    logloss = ll,
    ece = ece,
    threshold_opt = thr_opt,
    cm_50 = cm_50, cm_opt = cm_opt,
    feature_importance = fi,
    output_dir = output_dir
  ))
}

# -----------------------------
# Example usage:
# -----------------------------
# # Field Goal (positive = "Made")
# fg_bundle_path <- "R/models/field_goal/fg_model.rds"
# res_fg <- evaluate_binary_model(
#   bundle_or_path = fg_bundle_path,
#   split_dir = "data/fg",
#   outcome_col = "label",
#   positive_label = "Made",
#   tag = "fg_valid"
# )
#
# # Fourth Down (positive = "Converted")
# fd_bundle_path <- "R/models/fourth_down/fd_conversion_model.rds"
# res_fd <- evaluate_binary_model(
#   bundle_or_path = fd_bundle_path,
#   split_dir = "data/fd",
#   outcome_col = "label",
#   positive_label = "Converted",
#   tag = "fd_valid"
# )
