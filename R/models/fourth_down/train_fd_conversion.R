# ------------------------------------------------------------------------------
# Fourth-Down Conversion model (uses fill_missing_weather_* from R/core/pbp_wp_prep.R)
# Predicts whether a 4th-down attempt is converted.
# 70/15/15 stratified splits; TRAIN-frozen levels; VALID held for calibration
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
SPLIT_DIR  <- "data/fd"
if (!dir.exists(MODEL_DIR)) dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(SPLIT_DIR)) dir.create(SPLIT_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Load and derive 4th-down GO attempts -------------------------------------
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
  ) |> as.data.table()

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
fd_data[, label := factor(ifelse(fd_converted == 1, "Converted", "Failed"),
                          levels = c("Converted","Failed"))]

# --- Features -----------------------------------------------------------------
drop_cols <- c("game_date","weather_imputed","first_down","fd_converted","label","stadium_id")
features  <- setdiff(names(fd_data), drop_cols)

# --- 70/15/15 stratified split -------------------------------------------------
idx_train <- caret::createDataPartition(fd_data$label, p = 0.70, list = FALSE)
train_fd  <- fd_data[idx_train]
rest_fd   <- fd_data[-idx_train]
idx_valid <- caret::createDataPartition(rest_fd$label, p = 0.50, list = FALSE)
valid_fd  <- rest_fd[idx_valid]
test_fd   <- rest_fd[-idx_valid]

# ---- Freeze categorical levels from TRAIN and encode across splits -----------
is_cat    <- vapply(train_fd[, ..features], function(x) is.character(x) || is.factor(x), logical(1))
cat_cols  <- names(is_cat)[is_cat]
levels_map <- lapply(train_fd[, ..cat_cols], function(col) {
  if (is.factor(col)) levels(col) else sort(unique(as.character(col)))
})
saveRDS(levels_map, LEVELS_RDS)

encode_with_levels <- function(x, lv) as.integer(factor(as.character(x), levels = lv))
for (f in features) {
  if (f %in% cat_cols) {
    lv              <- levels_map[[f]]
    train_fd[[f]]   <- encode_with_levels(train_fd[[f]], lv)
    valid_fd[[f]]   <- encode_with_levels(valid_fd[[f]], lv)
    test_fd[[f]]    <- encode_with_levels(test_fd[[f]],  lv)
  } else {
    train_fd[[f]] <- suppressWarnings(as.numeric(train_fd[[f]]))
    valid_fd[[f]] <- suppressWarnings(as.numeric(valid_fd[[f]]))
    test_fd[[f]]  <- suppressWarnings(as.numeric(test_fd[[f]]))
  }
}

# ---- (Optional) Save splits for downstream evaluation ------------------------
saveRDS(train_fd[, c(features, "label"), with = FALSE], file.path(SPLIT_DIR, "train.rds"))
saveRDS(valid_fd[, c(features, "label"), with = FALSE], file.path(SPLIT_DIR, "valid.rds"))
saveRDS(test_fd [, c(features, "label"), with = FALSE], file.path(SPLIT_DIR, "test.rds"))
saveRDS(features, file.path(SPLIT_DIR, "feature_names.rds"))

# --- Matrices for caret::train (TRAIN only) -----------------------------------
train_df <- as.data.frame(train_fd)
test_df  <- as.data.frame(test_fd)   # quick AUC sanity check
valid_df <- as.data.frame(valid_fd)  # reserved for later calibration/eval

x_train  <- as.matrix(train_df[, features, drop = FALSE])
y_train  <- train_df[["label"]]

x_test   <- as.matrix(test_df[,  features, drop = FALSE])
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

# --- Eval on TEST (VALID remains pristine) ------------------------------------
test_probs <- predict(fd_model, newdata = x_test, type = "prob")[, "Converted"]
auc_test   <- as.numeric(pROC::auc(y_test, test_probs))

# --- Bundle + save -------------------------------------------------------------
fd_bundle <- list(
  model         = fd_model$finalModel,
  feature_names = colnames(x_train),
  trained_at    = Sys.time(),
  params        = fd_model$bestTune,
  source        = "R/models/fourth_down/train_fd_conversion.R",
  meta          = list(years = c(min(YEARS), max(YEARS)),
                       n_train = nrow(x_train), n_valid = nrow(valid_df), n_test = nrow(x_test), metric = "ROC"),
  eval          = list(auc_test = auc_test)
)

saveRDS(fd_bundle, MODEL_RDS)
cat(sprintf("[FD] Saved %s | AUC(test)=%.4f\n", MODEL_RDS, auc_test))
cat(sprintf("[FD] Saved levels map -> %s\n", LEVELS_RDS), "\n")
