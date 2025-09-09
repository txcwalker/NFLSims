# ------------------------------------------------------------------------------
# Fourth-Down Conversion model (uses fill_missing_weather_* from R/core/pbp_wp_prep.R)
# Predicts whether a 4th-down attempt is converted.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
  library(caret)
  library(pROC)
  library(nflfastR)
})
source("R/core/pbp_wp_prep.R")

set.seed(42)

YEARS      <- 1999:(as.integer(format(Sys.Date(), "%Y")) - 1)
MODEL_DIR  <- "R/models/fourth_down"
MODEL_RDS  <- file.path(MODEL_DIR, "fd_conversion_model.rds")
LEVELS_RDS <- file.path(MODEL_DIR, "fd_levels.rds")
if (!dir.exists(MODEL_DIR)) dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Load and derive 4th-down GO attempts -------------------------------------
# nflfastR does NOT have 'fourth_down_attempt' column. We derive attempts as:
#   - down == 4
#   - the offense actually ran a play (rush/pass/scramble)
#   - exclude spike/kneel
# Conversion is derived from first_down == 1 (covers penalties that awarded a new first down)
fd_data <- nflfastR::load_pbp(YEARS) %>%
  dplyr::filter(
    down == 4,
    (rush == 1 | pass == 1 | qb_scramble == 1),
    (is.na(qb_spike) | qb_spike == 0),
    (is.na(qb_kneel) | qb_kneel == 0),
    !is.na(first_down)
  ) %>%
  dplyr::select(
    stadium_id, surface, wind, temp, roof,
    season_type, week, game_date,
    game_seconds_remaining,
    yardline_100, ydstogo,
    score_differential,
    spread_line, total_line,
    play_type, rush, pass, qb_scramble, qb_spike, qb_kneel,
    first_down
  )

# ensure data.table
fd_data <- as.data.table(fd_data)

# --- Weather cleanup + fills ---------------------------------------------------
fd_data[, `:=`(temp = as.numeric(temp), wind = as.numeric(wind))]

fd_data <- fill_missing_weather_within_game(fd_data); data.table::setDT(fd_data)
fd_data <- fill_missing_weather_by_date(fd_data);    data.table::setDT(fd_data)

# --- Normalize/clean some categorical values ----------------------------------
fd_data[, roof := ifelse(roof %in% c("outdoor","outdoors"), "outdoors",
                  ifelse(roof %in% c("indoor","indoors","dome"), "indoors", roof))]

# --- Target + labels (derive conversion from first_down) ----------------------
fd_data[, fd_converted := as.integer(first_down == 1)]
fd_data <- fd_data[!is.na(fd_converted)]

# Positive class first to match twoClassSummary convention
fd_data[, label := factor(ifelse(fd_converted == 1, "Converted", "Failed"),
                          levels = c("Converted","Failed"))]

# --- Features -----------------------------------------------------------------
drop_cols <- c("game_date","weather_imputed","first_down","fd_converted","label","stadium_id")
features  <- setdiff(names(fd_data), drop_cols)

# Save categorical levels for deployment encoding
is_cat   <- vapply(fd_data[, ..features], function(x) is.character(x) || is.factor(x), logical(1))
cat_cols <- names(is_cat)[is_cat]
levels_map <- lapply(fd_data[, ..cat_cols], function(col) {
  if (is.factor(col)) levels(col) else sort(unique(as.character(col)))
})
saveRDS(levels_map, LEVELS_RDS)

# Integer encode categoricals
for (f in features) {
  if (is.character(fd_data[[f]]) || is.factor(fd_data[[f]])) {
    fd_data[[f]] <- as.integer(as.factor(fd_data[[f]]))
  }
}

# --- Train/test split (stratify on factor label) -------------------------------
idx      <- caret::createDataPartition(fd_data$label, p = 0.8, list = FALSE)
train_fd <- fd_data[idx, ]
test_fd  <- fd_data[-idx, ]

# Extra guardrail: if a class is missing (unlikely), re-split once
if (length(unique(train_fd$label)) < 2L) {
  warning("Resplitting because a class was missing in training set.")
  idx      <- caret::createDataPartition(fd_data$label, p = 0.8, list = FALSE)
  train_fd <- fd_data[idx, ]
  test_fd  <- fd_data[-idx, ]
}
train_fd[, label := droplevels(label)]
test_fd[,  label := droplevels(label)]

cat("[FD] Class counts: train=", paste(capture.output(print(table(train_fd$label))), collapse=" "),
    " | test=", paste(capture.output(print(table(test_fd$label))), collapse=" "), "\n")

# --- Matrices for caret::train -------------------------------------------------
train_df <- as.data.frame(train_fd)
test_df  <- as.data.frame(test_fd)
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

fd_model <- caret::train(
  x = x_train, y = y_train,
  method = "xgbTree", trControl = ctrl, tuneGrid = xgb_grid, metric = "ROC"
)

# --- Eval ----------------------------------------------------------------------
test_probs <- predict(fd_model, newdata = x_test, type = "prob")[, "Converted"]
auc_test   <- as.numeric(pROC::auc(y_test, test_probs))

# --- Bundle + save -------------------------------------------------------------
fd_bundle <- list(
  model         = fd_model$finalModel,
  feature_names = colnames(x_train),
  trained_at    = Sys.time(),
  params        = fd_model$bestTune,
  source        = "R/models/fourth_down/train_fd_conversion.R",
  meta          = list(years = c(min(YEARS), max(YEARS)), n_train = nrow(x_train), n_test = nrow(x_test), metric = "ROC"),
  eval          = list(auc_test = auc_test)
)

saveRDS(fd_bundle, MODEL_RDS)
cat(sprintf("[FD] Saved %s | AUC(test)=%.4f\n", MODEL_RDS, auc_test))
cat(sprintf("[FD] Saved levels map -> %s\n", LEVELS_RDS), "\n")
