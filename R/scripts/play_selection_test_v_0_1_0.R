suppressPackageStartupMessages({ library(dplyr); library(xgboost) })

MODEL_DIR <- "src/nfl_sim/models/play_selection_v_0_1_0"
DATA_PATH <- "data/model_1_training_data.csv"
set.seed(42)

FEATURES <- c("yardline_100","game_seconds_remaining","score_differential",
              "posteam_timeouts_remaining","defteam_timeouts_remaining","leverage")

# Helper: assign bucket from down + ydstogo
assign_bucket <- function(down, ydstogo) {
  dplyr::case_when(
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
    TRUE ~ "other"
  )
}

# Helper: run a single scenario through the right submodel
predict_pass_prob <- function(down, ydstogo, yardline_100, game_seconds_remaining,
                               score_differential, timeouts_off = 3, timeouts_def = 3) {
  bucket <- assign_bucket(down, ydstogo)
  if (bucket == "other") return(NA_real_)
  mp <- file.path(MODEL_DIR, paste0(bucket, ".json"))
  if (!file.exists(mp)) return(NA_real_)
  m <- xgb.load(mp)
  leverage <- score_differential * game_seconds_remaining
  feat <- matrix(c(yardline_100, game_seconds_remaining, score_differential,
                   timeouts_off, timeouts_def, leverage), nrow = 1,
                 dimnames = list(NULL, FEATURES))
  predict(m, xgb.DMatrix(feat))
}

# ── Section 8.1: 500 Random Realistic Game States ────────────────────────────
cat("=== Section 8.1: 500 Random Game State Distribution Test ===\n\n")

# Load real data to sample realistic joint distributions
raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE) %>%
  filter(play_type %in% c("run","pass"), !is.na(down), !is.na(ydstogo),
         !is.na(game_seconds_remaining), !is.na(score_differential), !is.na(yardline_100),
         !is.na(posteam_timeouts_remaining), !is.na(defteam_timeouts_remaining))

# Sample 500 rows from real data — realistic by construction
sample500 <- raw %>% sample_n(500) %>%
  mutate(
    is_pass     = as.numeric(play_type == "pass"),
    leverage    = score_differential * game_seconds_remaining,
    dist_bucket = assign_bucket(down, ydstogo)
  ) %>% filter(dist_bucket != "other")

# Get predicted pass prob for each
sample500$pred_pass_prob <- mapply(
  predict_pass_prob,
  down                  = sample500$down,
  ydstogo               = sample500$ydstogo,
  yardline_100          = sample500$yardline_100,
  game_seconds_remaining = sample500$game_seconds_remaining,
  score_differential    = sample500$score_differential,
  timeouts_off          = sample500$posteam_timeouts_remaining,
  timeouts_def          = sample500$defteam_timeouts_remaining
)

sample500 <- sample500 %>% filter(!is.na(pred_pass_prob))

overall_pred_pass <- mean(sample500$pred_pass_prob)
overall_act_pass  <- mean(sample500$is_pass)

cat(sprintf("  EDA overall pass rate:          57%%  (approx)\n"))
cat(sprintf("  Sample actual pass rate:        %.1f%%\n", overall_act_pass * 100))
cat(sprintf("  Model mean predicted pass prob: %.1f%%\n", overall_pred_pass * 100))
cat(sprintf("  Match within tolerance (±3%%):   %s\n\n",
            ifelse(abs(overall_pred_pass - 0.57) < 0.03, "✅ YES", "⚠️  CHECK")))

# Check: trailing + time short → high pass rate
trail_short <- sample500 %>%
  filter(score_differential <= -7, game_seconds_remaining <= 120)
cat(sprintf("  Trailing 7+ pts, <2 min left (n=%d): mean pred pass = %.1f%%  (expect >75%%): %s\n",
            nrow(trail_short), mean(trail_short$pred_pass_prob) * 100,
            ifelse(mean(trail_short$pred_pass_prob) > 0.75, "✅", "⚠️")))

# Check: leading + late → run heavy
lead_late <- sample500 %>%
  filter(score_differential >= 7, game_seconds_remaining <= 600, down == 1)
cat(sprintf("  Leading 7+, <10 min left, 1st down (n=%d): mean pred pass = %.1f%%  (expect <50%%): %s\n",
            nrow(lead_late), mean(lead_late$pred_pass_prob) * 100,
            ifelse(mean(lead_late$pred_pass_prob) < 0.50, "✅", "⚠️")))

# Check: long yardage → high pass rate
long_yard <- sample500 %>% filter(ydstogo >= 7, down %in% c(2,3))
cat(sprintf("  2nd/3rd and long 7+ (n=%d): mean pred pass = %.1f%%  (expect >65%%): %s\n",
            nrow(long_yard), mean(long_yard$pred_pass_prob) * 100,
            ifelse(mean(long_yard$pred_pass_prob) > 0.65, "✅", "⚠️")))

# Check: short yardage → low pass rate
short_yard <- sample500 %>% filter(ydstogo <= 3, down %in% c(2,3,4))
cat(sprintf("  2nd/3rd/4th and short 1-3 (n=%d): mean pred pass = %.1f%%  (expect <55%%): %s\n",
            nrow(short_yard), mean(short_yard$pred_pass_prob) * 100,
            ifelse(mean(short_yard$pred_pass_prob) < 0.55, "✅", "⚠️")))

# ── Section 8.2: Edge Case Checklist ─────────────────────────────────────────
cat("\n=== Section 8.2: Edge Case Checklist ===\n\n")

edge_cases <- list(
  list(label="3rd & 15, down 14, 2:00 left (near-certain pass)",
       down=3, ydstogo=15, yl=50, secs=120, sd=-14, expect_gt=0.85),
  list(label="1st & 10, up 10, 6:00 Q4 (run-heavy)",
       down=1, ydstogo=10, yl=50, secs=360, sd=10, expect_lt=0.49),
  list(label="4th & 1, short yardage (run-elevated)",
       down=4, ydstogo=1, yl=50, secs=1800, sd=0, expect_lt=0.55),
  list(label="1st & 10, own 1-yard line (backed up, suppressed pass)",
       down=1, ydstogo=10, yl=99, secs=1800, sd=0, expect_lt=0.49),
  list(label="2nd & 7, red zone 5-yd line (RZ effect)",
       down=2, ydstogo=7, yl=5, secs=1800, sd=0, expect_lt=0.68)
)

for (ec in edge_cases) {
  p <- predict_pass_prob(ec$down, ec$ydstogo, ec$yl, ec$secs, ec$sd)
  if (!is.null(ec$expect_gt)) {
    pass <- p > ec$expect_gt
    cat(sprintf("  [%s] %s\n       Predicted: %.1f%%  |  Threshold: >%.0f%%\n\n",
                ifelse(pass,"✅","⚠️ "), ec$label, p*100, ec$expect_gt*100))
  } else {
    pass <- p < ec$expect_lt
    cat(sprintf("  [%s] %s\n       Predicted: %.1f%%  |  Threshold: <%.0f%%\n\n",
                ifelse(pass,"✅","⚠️ "), ec$label, p*100, ec$expect_lt*100))
  }
}

cat("=== Testing Complete ===\n")
