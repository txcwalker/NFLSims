# ------------------------------------------------------------------------------
# Field Goal model (uses fill_missing_weather_* from R/core/pbp_wp_prep.R)
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
if (!dir.exists(MODEL_DIR)) dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)

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
# Derive a clean binary from the raw text first, then build a 2-level factor.
fg_data[, fg_made := {
  res_chr <- tolower(as.character(field_goal_result))
  as.integer(res_chr == "made")
}]

# Sanity: drop any rows that somehow became NA in fg_made (shouldn't happen after filter)
fg_data <- fg_data[!is.na(fg_made)]

# Label with positive class first (caret's twoClassSummary treats the first level as the "event")
fg_data[, label := factor(ifelse(fg_made == 1, "Made", "Miss"), levels = c("Made","Miss"))]

# --- Features -----------------------------------------------------------------
drop_cols <- c("game_date","weather_imputed","field_goal_result","fg_made","label","stadium_id","total")
features  <- setdiff(names(fg_data), drop_cols)

# Save categorical levels for deployment encoding
is_cat   <- vapply(fg_data[, ..features], function(x) is.character(x) || is.factor(x), logical(1))
cat_cols <- names(is_cat)[is_cat]
levels_map <- lapply(fg_data[, ..cat_cols], function(col) {
  if (is.factor(col)) levels(col) else sort(unique(as.character(col)))
})
saveRDS(levels_map, LEVELS_RDS)

# Integer encode categoricals (simple label-encoding)
for (f in features) {
  if (is.character(fg_data[[f]]) || is.factor(fg_data[[f]])) {
    fg_data[[f]] <- as.integer(as.factor(fg_data[[f]]))
  }
}

# --- Train/test split (STRATIFY on the factor label) ---------------------------
# This guarantees both classes are represented in the training set.
idx      <- caret::createDataPartition(fg_data$label, p = 0.8, list = FALSE)
train_fg <- fg_data[idx, ]
test_fg  <- fg_data[-idx, ]

# Extra guardrail: if a class is missing (extremely unlikely), re-split once.
if (length(unique(train_fg$label)) < 2L) {
  warning("Resplitting because a class was missing in training set.")
  idx      <- caret::createDataPartition(fg_data$label, p = 0.8, list = FALSE)
  train_fg <- fg_data[idx, ]
  test_fg  <- fg_data[-idx, ]
}
# Drop unused levels just in case
train_fg[, label := droplevels(label)]
test_fg[,  label := droplevels(label)]

# Quick counts for visibility
cat("[FG] Class counts: train=", paste(capture.output(print(table(train_fg$label))), collapse=" "),
    " | test=", paste(capture.output(print(table(test_fg$label))), collapse=" "), "\n")

# --- Matrices for caret::train -------------------------------------------------
train_df <- as.data.frame(train_fg)
test_df  <- as.data.frame(test_fg)
x_train  <- as.matrix(train_df[, features, drop = FALSE])
x_test   <- as.matrix(test_df [ , features, drop = FALSE])
y_train  <- train_df[["label"]]
y_test   <- test_df[["label"]]

# --- Tuning + training ---------------------------------------------------------
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

# --- Eval ----------------------------------------------------------------------
test_probs <- predict(fg_model, newdata = x_test, type = "prob")[, "Made"]
auc_test   <- as.numeric(pROC::auc(y_test, test_probs))

# --- Bundle + save -------------------------------------------------------------
fg_bundle <- list(
  model         = fg_model$finalModel,
  feature_names = colnames(x_train),
  trained_at    = Sys.time(),
  params        = fg_model$bestTune,
  source        = "R/models/field_goal/train_fg.R",
  meta          = list(years = c(min(YEARS), max(YEARS)), n_train = nrow(x_train), n_test = nrow(x_test), metric = "ROC"),
  eval          = list(auc_test = auc_test)
)

saveRDS(fg_bundle, MODEL_RDS)
cat(sprintf("[FG] Saved %s | AUC(test)=%.4f\n", MODEL_RDS, auc_test))
cat(sprintf("[FG] Saved levels map -> %s\n", LEVELS_RDS), "\n")
