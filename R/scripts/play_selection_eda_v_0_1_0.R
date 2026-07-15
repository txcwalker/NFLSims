###############################################################################
# Play Type Selection — EDA Script
# Version: V.0.1.0
# Author: NFLSims / Antigravity
# Last Updated: May 2026
#
# Purpose:
#   Run the Exploratory Data Analysis defined in docs/models/play_selection_v_0_1_0.md.
#   All outputs are written to docs/eda_outputs/play_selection/ for review and
#   for pasting findings into the model documentation file.
#
# Input:
#   data/model_1_training_data.csv — Preprocessed NFL play-by-play (2015-2024)
#   Columns used: play_type, down, ydstogo, yardline_100, qtr,
#                 game_seconds_remaining, score_differential,
#                 posteam_timeouts_remaining, defteam_timeouts_remaining, season
#
# Output:
#   Console summaries + CSV tables written to docs/eda_outputs/play_selection/
###############################################################################

library(dplyr)
library(tidyr)
library(ggplot2)

# ── Config ────────────────────────────────────────────────────────────────────
DATA_PATH   <- "data/model_1_training_data.csv"
OUTPUT_DIR  <- "docs/eda_outputs/play_selection"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=============================================================\n")
cat("  NFLSims — Play Type Selection EDA V.0.1.0\n")
cat("=============================================================\n\n")

# ── Load & Filter ─────────────────────────────────────────────────────────────
cat("Loading data...\n")
raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE)
cat("  Raw rows:", nrow(raw), "\n")

# Scope to confirmed offensive snaps only (run or pass).
# Field goals and punts are excluded — those decisions happen upstream.
plays <- raw %>%
  filter(
    play_type %in% c("run", "pass"),
    !is.na(down),
    !is.na(ydstogo),
    !is.na(game_seconds_remaining),
    !is.na(score_differential),
    !is.na(yardline_100)
  ) %>%
  mutate(
    is_pass    = as.integer(play_type == "pass"),
    # ── Engineered leverage interaction term ──────────────────────────────
    # Captures urgency: score_diff × seconds_remaining. A team down 7 with
    # 45 seconds is in a different universe than down 7 with 8 minutes.
    # Negative leverage = trailing; positive leverage = leading.
    leverage   = score_differential * game_seconds_remaining,
    # ── Down / Distance Bucket Assignment ─────────────────────────────────
    dist_bucket = case_when(
      ydstogo == 10 & down == 1             ~ "1_10",
      ydstogo >  10 & down == 1             ~ "1_long",
      ydstogo <  10 & down == 1             ~ "1_short",
      down == 2 & ydstogo <= 3              ~ "2_short",
      down == 2 & ydstogo >= 4 & ydstogo <= 6 ~ "2_med",
      down == 2 & ydstogo >= 7              ~ "2_long",
      down == 3 & ydstogo <= 3              ~ "3_short",
      down == 3 & ydstogo >= 4 & ydstogo <= 6 ~ "3_med",
      down == 3 & ydstogo >= 7              ~ "3_long",
      down == 4 & ydstogo <= 3              ~ "4_short",
      down == 4 & ydstogo >= 4 & ydstogo <= 6 ~ "4_med",
      down == 4 & ydstogo >= 7              ~ "4_long",
      TRUE                                  ~ "other"
    ),
    # ── Yardline Zones ────────────────────────────────────────────────────
    yardline_zone = case_when(
      yardline_100 <= 10            ~ "Red Zone (1-10)",
      yardline_100 <= 20            ~ "Scoring Zone (11-20)",
      yardline_100 <= 50            ~ "Own Half (21-50)",
      TRUE                          ~ "Backed Up (51-99)"
    ),
    # ── Score Differential Buckets ────────────────────────────────────────
    score_bucket = case_when(
      score_differential <= -14     ~ "Down 14+",
      score_differential <= -7      ~ "Down 7-13",
      score_differential <= -1      ~ "Down 1-6",
      score_differential == 0       ~ "Tied",
      score_differential <= 6       ~ "Up 1-6",
      score_differential <= 13      ~ "Up 7-13",
      TRUE                          ~ "Up 14+"
    ),
    # ── 2-Minute Drill Flag ───────────────────────────────────────────────
    # Defined as: inside final 2 minutes of either half
    two_min_drill = as.integer(
      (qtr == 2 & half_seconds_remaining <= 120) |
      (qtr == 4 & game_seconds_remaining <= 120)
    )
  )

cat("  Filtered to run/pass plays:", nrow(plays), "\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# EDA 1: Overall Run / Pass Split
# ──────────────────────────────────────────────────────────────────────────────
cat("── EDA 1: Overall Run/Pass Split ─────────────────────────────\n")
overall <- plays %>%
  summarise(
    total_plays = n(),
    runs        = sum(is_pass == 0),
    passes      = sum(is_pass == 1),
    pass_rate   = round(mean(is_pass), 4)
  )
print(overall)
write.csv(overall, file.path(OUTPUT_DIR, "1_overall_split.csv"), row.names = FALSE)
cat("\n")

# ──────────────────────────────────────────────────────────────────────────────
# EDA 2: Observation Count + Run/Pass by Down/Distance Bucket
# ──────────────────────────────────────────────────────────────────────────────
cat("── EDA 2: Observation Count + Pass Rate by Bucket ────────────\n")
bucket_summary <- plays %>%
  filter(dist_bucket != "other") %>%
  group_by(dist_bucket) %>%
  summarise(
    n_plays   = n(),
    n_runs    = sum(is_pass == 0),
    n_passes  = sum(is_pass == 1),
    pass_rate = round(mean(is_pass), 4)
  ) %>%
  arrange(desc(n_plays))
print(bucket_summary, n = 20)
write.csv(bucket_summary, file.path(OUTPUT_DIR, "2_bucket_obs_and_pass_rate.csv"), row.names = FALSE)

# ── Flag: buckets with low observations ──────────────────────────────────────
LOW_OBS_THRESHOLD <- 3000
low_obs <- bucket_summary %>% filter(n_plays < LOW_OBS_THRESHOLD)
if (nrow(low_obs) > 0) {
  cat("\n  ⚠️  LOW OBSERVATION BUCKETS (< ", LOW_OBS_THRESHOLD, " plays):\n")
  print(low_obs)
} else {
  cat("  ✅  All buckets exceed the", LOW_OBS_THRESHOLD, "play minimum.\n")
}
cat("\n")

# ──────────────────────────────────────────────────────────────────────────────
# EDA 3: Pass Rate by Year — NFL Meta Drift
# ──────────────────────────────────────────────────────────────────────────────
cat("── EDA 3: Pass Rate by Year (NFL Meta Drift) ─────────────────\n")
by_year <- plays %>%
  group_by(season) %>%
  summarise(
    n_plays   = n(),
    pass_rate = round(mean(is_pass), 4)
  ) %>%
  arrange(season)
print(by_year, n = 20)
write.csv(by_year, file.path(OUTPUT_DIR, "3_pass_rate_by_year.csv"), row.names = FALSE)
cat("\n")

# ──────────────────────────────────────────────────────────────────────────────
# EDA 4: Pass Rate by Year AND Bucket (Meta Drift per Situation)
# ──────────────────────────────────────────────────────────────────────────────
cat("── EDA 4: Pass Rate by Year × Bucket ─────────────────────────\n")
by_year_bucket <- plays %>%
  filter(dist_bucket != "other") %>%
  group_by(season, dist_bucket) %>%
  summarise(
    n_plays   = n(),
    pass_rate = round(mean(is_pass), 4),
    .groups   = "drop"
  ) %>%
  arrange(dist_bucket, season)
write.csv(by_year_bucket, file.path(OUTPUT_DIR, "4_pass_rate_by_year_and_bucket.csv"), row.names = FALSE)
cat("  Written to CSV (wide table — see 4_pass_rate_by_year_and_bucket.csv)\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# EDA 5: Pass Rate by Quarter (Game Flow Effect)
# ──────────────────────────────────────────────────────────────────────────────
cat("── EDA 5: Pass Rate by Quarter ────────────────────────────────\n")
by_quarter <- plays %>%
  group_by(qtr) %>%
  summarise(
    n_plays   = n(),
    pass_rate = round(mean(is_pass), 4)
  )
print(by_quarter)
write.csv(by_quarter, file.path(OUTPUT_DIR, "5_pass_rate_by_quarter.csv"), row.names = FALSE)
cat("\n")

# ──────────────────────────────────────────────────────────────────────────────
# EDA 6: Pass Rate by Quarter AND Bucket
# ──────────────────────────────────────────────────────────────────────────────
cat("── EDA 6: Pass Rate by Quarter × Bucket ──────────────────────\n")
by_qtr_bucket <- plays %>%
  filter(dist_bucket != "other") %>%
  group_by(qtr, dist_bucket) %>%
  summarise(
    n_plays   = n(),
    pass_rate = round(mean(is_pass), 4),
    .groups   = "drop"
  ) %>%
  arrange(dist_bucket, qtr)
write.csv(by_qtr_bucket, file.path(OUTPUT_DIR, "6_pass_rate_by_quarter_and_bucket.csv"), row.names = FALSE)
cat("  Written to CSV — see 6_pass_rate_by_quarter_and_bucket.csv\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# EDA 7: Pass Rate by Score Differential Bucket
# ──────────────────────────────────────────────────────────────────────────────
cat("── EDA 7: Pass Rate by Score Differential ─────────────────────\n")
score_levels <- c("Down 14+", "Down 7-13", "Down 1-6", "Tied", "Up 1-6", "Up 7-13", "Up 14+")
by_score <- plays %>%
  mutate(score_bucket = factor(score_bucket, levels = score_levels)) %>%
  group_by(score_bucket) %>%
  summarise(
    n_plays   = n(),
    pass_rate = round(mean(is_pass), 4)
  ) %>%
  arrange(score_bucket)
print(by_score)
write.csv(by_score, file.path(OUTPUT_DIR, "7_pass_rate_by_score_bucket.csv"), row.names = FALSE)
cat("\n")

# ──────────────────────────────────────────────────────────────────────────────
# EDA 8: Pass Rate by Yardline Zone
# ──────────────────────────────────────────────────────────────────────────────
cat("── EDA 8: Pass Rate by Yardline Zone ──────────────────────────\n")
by_zone <- plays %>%
  group_by(yardline_zone) %>%
  summarise(
    n_plays   = n(),
    pass_rate = round(mean(is_pass), 4)
  ) %>%
  arrange(pass_rate)
print(by_zone)
write.csv(by_zone, file.path(OUTPUT_DIR, "8_pass_rate_by_yardline_zone.csv"), row.names = FALSE)
cat("\n")

# ──────────────────────────────────────────────────────────────────────────────
# EDA 9: 2-Minute Drill Effect
# ──────────────────────────────────────────────────────────────────────────────
cat("── EDA 9: 2-Minute Drill Effect ───────────────────────────────\n")
two_min <- plays %>%
  group_by(two_min_drill) %>%
  summarise(
    n_plays   = n(),
    pass_rate = round(mean(is_pass), 4)
  ) %>%
  mutate(situation = ifelse(two_min_drill == 1, "2-Min Drill", "Normal"))
print(select(two_min, situation, n_plays, pass_rate))
write.csv(two_min, file.path(OUTPUT_DIR, "9_two_minute_drill.csv"), row.names = FALSE)
cat("\n")

# ──────────────────────────────────────────────────────────────────────────────
# EDA 10: Leverage Interaction Variable Validation
# Confirms the engineered (score_diff × game_seconds_remaining) term
# is monotonically related to pass rate — teams with high negative leverage
# (trailing, time running out) should pass at near 100%.
# ──────────────────────────────────────────────────────────────────────────────
cat("── EDA 10: Leverage Variable Validation ───────────────────────\n")
# Use fixed decile-style breaks to avoid non-unique quantile issue
leverage_summary <- plays %>%
  mutate(
    leverage_bin = cut(
      leverage,
      breaks = c(-Inf, -100000, -50000, -20000, -5000, 0, 5000, 20000, 50000, 100000, Inf),
      labels = c("Trailing-Desperate","Trailing-Critical","Trailing-Serious",
                 "Trailing-Moderate","Trailing-Slight","Leading-Slight",
                 "Leading-Moderate","Leading-Serious","Leading-Critical","Leading-Comfortable"),
      include.lowest = TRUE
    )
  ) %>%
  group_by(leverage_bin) %>%
  summarise(
    n_plays      = n(),
    pass_rate    = round(mean(is_pass), 4),
    avg_score_diff = round(mean(score_differential), 1),
    avg_secs_rem   = round(mean(game_seconds_remaining), 0)
  ) %>%
  arrange(leverage_bin)
print(leverage_summary, n = 20)
write.csv(leverage_summary, file.path(OUTPUT_DIR, "10_leverage_validation.csv"), row.names = FALSE)
cat("\n")

# ──────────────────────────────────────────────────────────────────────────────
# Summary Report
# ──────────────────────────────────────────────────────────────────────────────
cat("=============================================================\n")
cat("  EDA COMPLETE — Outputs written to:", OUTPUT_DIR, "\n")
cat("=============================================================\n")
cat("\n  Files produced:\n")
cat("   1_overall_split.csv\n")
cat("   2_bucket_obs_and_pass_rate.csv     ← CHECK LOW OBS BUCKETS\n")
cat("   3_pass_rate_by_year.csv            ← META DRIFT CHECK\n")
cat("   4_pass_rate_by_year_and_bucket.csv\n")
cat("   5_pass_rate_by_quarter.csv\n")
cat("   6_pass_rate_by_quarter_and_bucket.csv\n")
cat("   7_pass_rate_by_score_bucket.csv\n")
cat("   8_pass_rate_by_yardline_zone.csv\n")
cat("   9_two_minute_drill.csv\n")
cat("  10_leverage_validation.csv          ← CONFIRM MONOTONIC RELATIONSHIP\n")
cat("\n")
cat("  Next Step: Review outputs and update docs/models/play_selection_v_0_1_0.md\n")
cat("  with EDA findings before proceeding to model training.\n\n")
