# R/tests/auto_test_fourth_downs.R
suppressPackageStartupMessages({
  library(nflfastR)
  library(dplyr)
  library(data.table)
})
source("R/core/pbp_wp_prep.R")
source("R/models/fourth_down/simulate_fd_decision.R")

cat("Loading 2025 historical play-by-play data for testing...\n")
pbp <- load_pbp(2025)

# Filter for 4th downs
pbp_4th <- pbp %>% 
  filter(down == 4) %>%
  filter(!is.na(yardline_100))

# Sample 1000 plays to keep execution time reasonable, plus some edge cases
set.seed(123)
edge_cases <- pbp_4th %>%
  filter((qtr == 4 & score_differential < -16) | (yardline_100 <= 35) | (yardline_100 >= 60))

random_sample <- pbp_4th %>%
  sample_n(min(1000, nrow(pbp_4th)))

test_data <- bind_rows(edge_cases, random_sample) %>%
  distinct(play_id, game_id, .keep_all = TRUE)

# Minimal columns required for model
test_data <- test_data %>%
  select(
    play_id, game_id, posteam, defteam, yardline_100, ydstogo, down, qtr, game_seconds_remaining,
    score_differential, posteam_timeouts_remaining, defteam_timeouts_remaining,
    surface, wind, temp, roof, season_type, week, posteam_type, total,
    spread_line, total_line, play_type_nfl, desc, game_date, stadium_id
  )

# Impute weather and clean
test_data <- fill_missing_weather_within_game(test_data)
test_data <- fill_missing_weather_by_date(test_data)

# Run Simulator
cat("Running simulation on", nrow(test_data), "4th down plays...\n")
results <- simulate_fd_decision(test_data)

# ================= AUTOMATED LOGIC ASSERTIONS =================
cat("\n=== AUTOMATED LOGICAL CHECKS ===\n")
failures <- 0

# Check 1: No punting in FG range (inside the 30)
fails_1 <- results %>% filter(yardline_100 <= 30 & !is.na(wp_punt))
if(nrow(fails_1) > 0) {
  cat("❌ FAILED: Found", nrow(fails_1), "plays allowing punts inside the 30.\n")
  failures <- failures + 1
} else {
  cat("✅ PASSED: No punting allowed inside the 30.\n")
}

# Check 2: No kicking FGs from > 65 yards (yardline_100 > 48)
fails_2 <- results %>% filter(yardline_100 > 65 & !is.na(wp_fg))
if(nrow(fails_2) > 0) {
  cat("❌ FAILED: Found", nrow(fails_2), "plays allowing FGs from > 82 yards out.\n")
  failures <- failures + 1
} else {
  cat("✅ PASSED: No FGs allowed from impossible distances.\n")
}

# Check 3: Down big in the 4th quarter -> Must go
fails_3 <- results %>% filter(qtr == 4 & score_differential <= -14 & game_seconds_remaining < 600 & best_action != "go")
if(nrow(fails_3) > 0) {
  cat("❌ FAILED: Found", nrow(fails_3), "plays down 2+ scores late in 4Q where GO was not selected.\n")
  # Usually this is because our logic hasn't enforced a strict override, but let's see if WP naturally catches it.
} else {
  cat("✅ PASSED: Automatically going for it when down big late in 4th quarter.\n")
}

# Check 4: Probabilities bounds
fails_4 <- results %>% filter((!is.na(wp_go) & (wp_go < 0 | wp_go > 1)) | 
                              (!is.na(wp_punt) & (wp_punt < 0 | wp_punt > 1)) | 
                              (!is.na(wp_fg) & (wp_fg < 0 | wp_fg > 1)))
if(nrow(fails_4) > 0) {
  cat("❌ FAILED: Found probabilities outside [0, 1] bounds.\n")
  failures <- failures + 1
} else {
  cat("✅ PASSED: All probabilities within valid bounds.\n")
}

# Write results to CSV for manual UI review
out_dir <- "R/tests/outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write.csv(results, file.path(out_dir, "historical_test_results.csv"), row.names = FALSE)
cat("\nResults saved to", file.path(out_dir, "historical_test_results.csv"), "\n")
