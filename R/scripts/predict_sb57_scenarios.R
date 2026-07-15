# R/scripts/predict_sb57_scenarios.R
# One-off: get model WP for two SB LVII (Feb 12, 2023) scenario candidates
# to replace the fabricated scenario 17 in wp_quiz.html.
#
# Option A: PHI possessing, trailing 27-35, Q4 ~9:00, 1st & 10 at own 25
# Option B: KC  possessing, tied 35-35,    Q4 ~5:15, 1st & 10 at own 25

suppressPackageStartupMessages({
  library(xgboost)
  library(data.table)
})

# ---- Load model + levels -------------------------------------------------------
wrapper    <- readRDS("R/models/win_probability/wp_model.rds")
booster    <- wrapper$model          # xgb.Booster lives here
features   <- wrapper$feature_names  # 15 features
levels_map <- readRDS("R/models/win_probability/wp_levels.rds")

encode_with_levels <- function(x, lv) as.integer(factor(as.character(x), levels = lv))

# ---- Helper: encode a single-row data.table and predict -----------------------
predict_wp <- function(scenario_dt) {
  dt <- copy(scenario_dt)
  cat_cols <- names(levels_map)
  for (f in features) {
    if (f %in% names(dt)) {
      if (f %in% cat_cols) {
        dt[[f]] <- encode_with_levels(dt[[f]], levels_map[[f]])
      } else {
        dt[[f]] <- suppressWarnings(as.numeric(dt[[f]]))
      }
    } else {
      dt[[f]] <- NA_real_
    }
  }
  x <- as.matrix(dt[, features, with = FALSE])
  round(predict(booster, newdata = x) * 100, 2)
}

# ---- SB LVII shared context ---------------------------------------------------
# State Farm Stadium, Glendale AZ — retractable roof (closed for SB), FieldTurf
# KC was -1.5 favourite; total line ~49.5; Super Bowl = week 22 (POST)
base <- list(
  surface     = "fieldturf",
  roof        = "dome",
  temp        = 68L,
  wind        = 0L,
  season_type = "POST",
  week        = 22L,
  total_line  = 49.5
)

# ---- Option A: PHI possessing, down 8, Q4 9:00 --------------------------------
# score_differential = posteam_score - defteam_score = 27 - 35 = -8
# spread_line in nflfastR = home_team_spread; PHI is "away" so their spread = +1.5
sc_a <- as.data.table(c(base, list(
  game_seconds_remaining     = 540L,
  score_differential         = -8L,
  down                       = 1L,
  yardline_100               = 75L,   # own 25 = 75 yards from scoring end zone
  ydstogo                    = 10L,
  posteam_timeouts_remaining = 3L,
  defteam_timeouts_remaining = 3L,
  spread_line                = 1.5    # PHI +1.5 from home-team perspective
)))

wp_a <- predict_wp(sc_a)
cat(sprintf("Option A — PHI down 35-27, Q4 9:00, 1st & 10 Own 25:  %.2f%%\n", wp_a))

# ---- Option B: KC possessing, tied, Q4 5:15 -----------------------------------
# score_differential = 35 - 35 = 0
# spread_line = -1.5 (KC was favourite)
sc_b <- as.data.table(c(base, list(
  game_seconds_remaining     = 315L,
  score_differential         = 0L,
  down                       = 1L,
  yardline_100               = 75L,
  ydstogo                    = 10L,
  posteam_timeouts_remaining = 3L,
  defteam_timeouts_remaining = 2L,
  spread_line                = -1.5   # KC was -1.5 favourite
)))

wp_b <- predict_wp(sc_b)
cat(sprintf("Option B — KC tied 35-35,   Q4 5:15, 1st & 10 Own 25:  %.2f%%\n", wp_b))
