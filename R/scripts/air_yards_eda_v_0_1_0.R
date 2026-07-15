# =============================================================================
# Air Yards EDA — V.0.1.0
# Script: R/scripts/air_yards_eda_v_0_1_0.R
# Purpose: Exploratory Data Analysis for the Air Yards model.
#   Performs all required EDA tasks per MODEL_DEVELOPMENT_STANDARD.md:
#   target distribution, multimodality detection, conditional distributions
#   by Quarter/Down/Score Differential, and feature correlation analysis.
# Inputs:
#   - nflfastR PBP data: seasons 2015-2024 (loaded via nflreadr)
# Outputs:
#   - docs/eda_outputs/air_yards/ — density plots, conditional breakdown CSVs,
#     correlation matrices, summary JSON
#   - Console summary printed at end of each section for documentation
# Author: NFLSims Engine
# =============================================================================

library(nflreadr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(jsonlite)
library(scales)

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------

SEASONS       <- 2015:2024
OUTPUT_DIR    <- "docs/eda_outputs/air_yards"
DEEP_SHOT_THRESH <- 20   # air_yards >= 20 = "deep shot"
SCREEN_THRESH    <- 1    # air_yards <= 0  = "screen / behind LOS"

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# SECTION 1 — DATA LOAD & FILTER
# ==============================================================================
#' load_and_filter_pbp
#'
#' @description Loads nflfastR play-by-play for the given seasons and applies
#'   standard Air Yards EDA filters: pass plays only, excluding spikes and
#'   aborted snaps. NA air_yards rows are flagged and excluded from distribution
#'   analysis but counted in the missing data audit.
#'
#' @param seasons Integer vector. Seasons to load (e.g., 2015:2024).
#' @return A tibble of filtered pass plays with key columns retained.
load_and_filter_pbp <- function(seasons) {
  message("Loading PBP for seasons: ", paste(range(seasons), collapse = "-"))

  pbp_raw <- load_pbp(seasons)

  # Flag: total raw rows
  n_raw <- nrow(pbp_raw)
  message("  Raw rows: ", format(n_raw, big.mark = ","))

  # Filter to pass plays, exclude spikes and aborted snaps
  # play_type == "pass" covers all dropbacks including sacks
  # qb_spike == 1 and aborted_play == 1 are exclusion flags in nflfastR
  pbp_pass <- pbp_raw %>%
    filter(
      play_type == "pass",
      is.na(qb_spike)     | qb_spike     == 0,
      is.na(aborted_play) | aborted_play == 0
    ) %>%
    select(
      # Identifiers
      game_id, play_id, season, week, posteam, defteam,
      # Target variable
      air_yards,
      # Situational context
      down, ydstogo, yardline_100, qtr,
      game_seconds_remaining, half_seconds_remaining,
      score_differential,
      # Player DNA join keys
      passer_player_name, receiver_player_name,
      # QB DNA features available in PBP
      cpoe, qb_scramble, sack,
      # Receiver DNA features
      any_of("yards_after_catch"),
      # Trench context
      any_of(c("qb_hit", "qb_hurry")),
      # Coach DNA join key
      any_of(c("home_coach", "away_coach")),
      # NGS-sourced timing — will be absent from raw PBP (triggers join flag)
      any_of("time_to_throw")
    )

  n_pass <- nrow(pbp_pass)
  n_missing_ay <- sum(is.na(pbp_pass$air_yards))
  message("  Pass plays after filter: ", format(n_pass, big.mark = ","))
  message("  Plays with NA air_yards: ", format(n_missing_ay, big.mark = ","),
          " (", round(100 * n_missing_ay / n_pass, 1), "%) — excluded from distribution EDA")

  pbp_pass
}

pbp <- load_and_filter_pbp(SEASONS)

# Working dataset: valid air yards only
pbp_ay <- pbp %>% filter(!is.na(air_yards))

# ==============================================================================
# SECTION 2 — TARGET DISTRIBUTION
# ==============================================================================
#' plot_ay_density
#'
#' @description Generates and saves kernel density plots of air_yards for
#'   three time windows: all years, 2020-2024, and 2023-2024.
#'   Annotates the zero-inflated region (screens) and the deep shot threshold.
#'
#' @param data Tibble. Filtered PBP data with valid air_yards.
#' @param output_dir Character. Path to write plots.
#' @return Named list of ggplot objects.
plot_ay_density <- function(data, output_dir) {
  windows <- list(
    "all_years"   = data,
    "last_5yr"    = data %>% filter(season >= 2020),
    "last_2yr"    = data %>% filter(season >= 2023)
  )

  plots <- list()
  for (nm in names(windows)) {
    d <- windows[[nm]]
    n <- nrow(d)
    mean_ay <- mean(d$air_yards)
    median_ay <- median(d$air_yards)

    p <- ggplot(d, aes(x = air_yards)) +
      geom_density(fill = "#1a73e8", alpha = 0.4, color = "#0d47a1", linewidth = 0.8) +
      geom_vline(xintercept = 0,                color = "red",    linetype = "dashed", linewidth = 0.7) +
      geom_vline(xintercept = DEEP_SHOT_THRESH, color = "orange", linetype = "dashed", linewidth = 0.7) +
      geom_vline(xintercept = mean_ay,          color = "black",  linetype = "dotted", linewidth = 0.6) +
      annotate("text", x = 0,                y = Inf, label = "Screen\nthreshold",
               vjust = 1.5, hjust = -0.1, size = 3, color = "red") +
      annotate("text", x = DEEP_SHOT_THRESH, y = Inf, label = "Deep shot\nthreshold",
               vjust = 1.5, hjust = -0.1, size = 3, color = "orange") +
      scale_x_continuous(limits = c(-15, 60), breaks = seq(-15, 60, 5)) +
      labs(
        title = paste0("Air Yards Distribution — ", gsub("_", " ", nm)),
        subtitle = paste0("n = ", format(n, big.mark = ","),
                          " | Mean = ", round(mean_ay, 1),
                          " yds | Median = ", round(median_ay, 1), " yds"),
        x = "Air Yards", y = "Density"
      ) +
      theme_minimal(base_size = 12)

    fname <- file.path(output_dir, paste0("ay_density_", nm, ".png"))
    ggsave(fname, p, width = 10, height = 5, dpi = 150)
    message("  Saved: ", fname)
    plots[[nm]] <- p
  }
  plots
}

message("\n--- SECTION 2: TARGET DISTRIBUTION ---")
density_plots <- plot_ay_density(pbp_ay, OUTPUT_DIR)

# Key stats for documentation
ay_stats <- pbp_ay %>%
  summarise(
    n               = n(),
    mean_ay         = mean(air_yards),
    median_ay       = median(air_yards),
    sd_ay           = sd(air_yards),
    pct_screen      = mean(air_yards <= SCREEN_THRESH),
    pct_short       = mean(air_yards > SCREEN_THRESH & air_yards < 10),
    pct_intermediate = mean(air_yards >= 10 & air_yards < DEEP_SHOT_THRESH),
    pct_deep        = mean(air_yards >= DEEP_SHOT_THRESH),
    pct_negative    = mean(air_yards < 0)
  )
print(ay_stats)

# ==============================================================================
# SECTION 3 — MULTIMODALITY / ZERO-INFLATION ANALYSIS
# ==============================================================================
#' analyze_multimodality
#'
#' @description Quantifies zero-inflation and identifies cluster boundaries.
#'   Computes the proportion of plays at air_yards <= 0 (screen/behind-LOS),
#'   and where the main "deep shot" cluster begins using a histogram-based
#'   valley-detection approach. These findings directly inform model architecture
#'   selection (hurdle vs. mixture vs. PDF sampling).
#'
#' @param data Tibble with air_yards column.
#' @return Named list of findings.
analyze_multimodality <- function(data) {
  # Histogram with 1-yard bins to find valleys
  ay_min <- floor(min(data$air_yards, na.rm = TRUE))
  ay_max <- ceiling(max(data$air_yards, na.rm = TRUE))
  h <- hist(data$air_yards, breaks = seq(ay_min, ay_max + 1, 1), plot = FALSE)

  findings <- list(
    pct_zero_inflated  = mean(data$air_yards <= 0),
    pct_negative       = mean(data$air_yards < 0),
    pct_exactly_zero   = mean(data$air_yards == 0),
    pct_positive_short = mean(data$air_yards > 0 & data$air_yards < 10),
    pct_intermediate   = mean(data$air_yards >= 10 & data$air_yards < 20),
    pct_deep_shot      = mean(data$air_yards >= 20),
    mean_ay_excl_screen = mean(data$air_yards[data$air_yards > 1]),
    # Valley between short and intermediate cluster typically at 3-5 yds
    histogram_counts   = h$counts,
    histogram_mids     = h$mids
  )

  message("  Zero-inflated (<=0): ", round(100 * findings$pct_zero_inflated, 1), "%")
  message("  Negative air yards:  ", round(100 * findings$pct_negative, 1), "%")
  message("  Deep shots (>=20):   ", round(100 * findings$pct_deep_shot, 1), "%")

  findings
}

message("\n--- SECTION 3: MULTIMODALITY ANALYSIS ---")
modal_findings <- analyze_multimodality(pbp_ay)

# ==============================================================================
# SECTION 4 — CONDITIONAL DISTRIBUTIONS
# ==============================================================================
#' compute_conditional_distributions
#'
#' @description Breaks down mean air yards and deep-shot rate by:
#'   (a) Quarter (1-4 + OT), (b) Down (1-4),
#'   (c) Score Differential buckets (-14+, -7 to -1, 0, +1 to +7, +14+)
#'   This answers whether the distribution is stationary or game-situation-dependent,
#'   which is critical for deciding if a single model or situational submodels are needed.
#'
#' @param data Tibble with air_yards, qtr, down, score_differential columns.
#' @param output_dir Character. Path to write CSV outputs.
#' @return Named list of summary tibbles.
compute_conditional_distributions <- function(data, output_dir) {

  # Score differential buckets
  data <- data %>%
    mutate(
      score_bucket = case_when(
        score_differential <= -14  ~ "Trailing 14+",
        score_differential <= -7   ~ "Trailing 7-13",
        score_differential <= -1   ~ "Trailing 1-6",
        score_differential == 0    ~ "Tied",
        score_differential <= 7    ~ "Leading 1-7",
        score_differential <= 13   ~ "Leading 8-13",
        TRUE                       ~ "Leading 14+"
      ),
      is_deep_shot = air_yards >= DEEP_SHOT_THRESH,
      is_screen    = air_yards <= SCREEN_THRESH
    )

  # (a) By Quarter
  by_qtr <- data %>%
    group_by(qtr) %>%
    summarise(
      n = n(), mean_ay = mean(air_yards),
      deep_shot_rate = mean(is_deep_shot),
      screen_rate    = mean(is_screen),
      .groups = "drop"
    )

  # (b) By Down
  by_down <- data %>%
    filter(!is.na(down)) %>%
    group_by(down) %>%
    summarise(
      n = n(), mean_ay = mean(air_yards),
      deep_shot_rate = mean(is_deep_shot),
      screen_rate    = mean(is_screen),
      .groups = "drop"
    )

  # (c) By Score Bucket
  bucket_order <- c("Trailing 14+", "Trailing 7-13", "Trailing 1-6",
                    "Tied", "Leading 1-7", "Leading 8-13", "Leading 14+")
  by_score <- data %>%
    filter(!is.na(score_bucket)) %>%
    mutate(score_bucket = factor(score_bucket, levels = bucket_order)) %>%
    group_by(score_bucket) %>%
    summarise(
      n = n(), mean_ay = mean(air_yards),
      deep_shot_rate = mean(is_deep_shot),
      screen_rate    = mean(is_screen),
      .groups = "drop"
    ) %>%
    arrange(score_bucket)

  # (d) By Down x Score Bucket (interaction — key for model architecture)
  by_down_score <- data %>%
    filter(!is.na(down), !is.na(score_bucket)) %>%
    mutate(score_bucket = factor(score_bucket, levels = bucket_order)) %>%
    group_by(down, score_bucket) %>%
    summarise(
      n = n(), mean_ay = mean(air_yards),
      deep_shot_rate = mean(is_deep_shot),
      .groups = "drop"
    )

  # Write CSVs
  write.csv(by_qtr,        file.path(output_dir, "ay_by_quarter.csv"),        row.names = FALSE)
  write.csv(by_down,       file.path(output_dir, "ay_by_down.csv"),           row.names = FALSE)
  write.csv(by_score,      file.path(output_dir, "ay_by_score_bucket.csv"),   row.names = FALSE)
  write.csv(by_down_score, file.path(output_dir, "ay_by_down_x_score.csv"),   row.names = FALSE)

  message("\n  --- By Quarter ---"); print(by_qtr)
  message("\n  --- By Down ---");   print(by_down)
  message("\n  --- By Score Bucket ---"); print(by_score)

  list(by_qtr = by_qtr, by_down = by_down,
       by_score = by_score, by_down_score = by_down_score)
}

message("\n--- SECTION 4: CONDITIONAL DISTRIBUTIONS ---")
cond_results <- compute_conditional_distributions(pbp_ay, OUTPUT_DIR)

# ==============================================================================
# SECTION 5 — FEATURE CORRELATION ANALYSIS
# ==============================================================================
#' analyze_feature_correlations
#'
#' @description Analyzes the relationship between air_yards and candidate
#'   features: Time to Throw (TTT), CPOE, yardline_100, down, ydstogo,
#'   score_differential, qtr. If TTT (time_to_throw) is absent from PBP,
#'   flags the NGS join as a required pre-training step.
#'
#' @param data Tibble with air_yards and candidate feature columns.
#' @param output_dir Character. Write path for correlation table CSV.
#' @return Named list with correlation table and NGS join flag status.
analyze_feature_correlations <- function(data, output_dir) {

  # Check TTT availability
  ttt_available <- "time_to_throw" %in% names(data) &&
                   sum(!is.na(data$time_to_throw)) > 1000

  if (!ttt_available) {
    message("  [FLAG] time_to_throw column absent or < 1000 non-NA values.")
    message("  [FLAG] NGS join is a REQUIRED PRE-TRAINING STEP for TTT feature.")
    message("  [FLAG] Continuing EDA with CPOE as TTT proxy.")
  } else {
    message("  time_to_throw available: n = ", sum(!is.na(data$time_to_throw)))
  }

  # Numeric features to correlate with air_yards
  numeric_features <- c("yardline_100", "down", "ydstogo",
                        "score_differential", "qtr",
                        "game_seconds_remaining", "cpoe")
  if (ttt_available) numeric_features <- c(numeric_features, "time_to_throw")

  # Pearson + Spearman correlations
  corr_results <- lapply(numeric_features, function(feat) {
    vals <- data %>%
      select(air_yards, all_of(feat)) %>%
      drop_na()
    pearson  <- cor(vals$air_yards, vals[[feat]], method = "pearson")
    spearman <- cor(vals$air_yards, vals[[feat]], method = "spearman")
    tibble(feature = feat, pearson = round(pearson, 3),
           spearman = round(spearman, 3), n_obs = nrow(vals))
  }) %>% bind_rows() %>% arrange(desc(abs(pearson)))

  write.csv(corr_results, file.path(output_dir, "ay_feature_correlations.csv"), row.names = FALSE)
  message("\n  --- Feature Correlations with air_yards ---")
  print(corr_results)

  # Binned analysis: air_yards by ydstogo buckets
  ay_by_distance <- data %>%
    mutate(distance_bucket = case_when(
      ydstogo <= 3  ~ "Short (1-3)",
      ydstogo <= 6  ~ "Medium (4-6)",
      ydstogo <= 10 ~ "Standard (7-10)",
      TRUE          ~ "Long (11+)"
    )) %>%
    group_by(distance_bucket) %>%
    summarise(mean_ay = mean(air_yards), deep_shot_rate = mean(air_yards >= DEEP_SHOT_THRESH),
              n = n(), .groups = "drop")
  write.csv(ay_by_distance, file.path(output_dir, "ay_by_distance_bucket.csv"), row.names = FALSE)
  message("\n  --- Air Yards by Distance Bucket ---"); print(ay_by_distance)

  list(
    correlations    = corr_results,
    ttt_available   = ttt_available,
    ngs_join_needed = !ttt_available,
    by_distance     = ay_by_distance
  )
}

message("\n--- SECTION 5: FEATURE CORRELATION ANALYSIS ---")
corr_results <- analyze_feature_correlations(pbp_ay, OUTPUT_DIR)

# ==============================================================================
# SECTION 6 — TEMPORAL DRIFT
# ==============================================================================
#' analyze_temporal_drift
#'
#' @description Checks if the air yards distribution has shifted across seasons.
#'   A stable distribution supports a flat training window (like play_selection).
#'   A drifting distribution may require recency weighting.
#'
#' @param data Tibble with air_yards and season columns.
#' @param output_dir Character. Write path.
#' @return Summary tibble by season.
analyze_temporal_drift <- function(data, output_dir) {
  by_season <- data %>%
    group_by(season) %>%
    summarise(
      n = n(),
      mean_ay        = round(mean(air_yards), 2),
      median_ay      = round(median(air_yards), 2),
      deep_shot_rate = round(mean(air_yards >= DEEP_SHOT_THRESH), 4),
      screen_rate    = round(mean(air_yards <= SCREEN_THRESH), 4),
      pct_negative   = round(mean(air_yards < 0), 4),
      .groups = "drop"
    )
  write.csv(by_season, file.path(output_dir, "ay_by_season.csv"), row.names = FALSE)
  message("\n  --- Air Yards Drift by Season ---"); print(by_season)
  by_season
}

message("\n--- SECTION 6: TEMPORAL DRIFT ---")
season_drift <- analyze_temporal_drift(pbp_ay, OUTPUT_DIR)

# ==============================================================================
# SECTION 7 — WRITE EDA SUMMARY JSON
# ==============================================================================
#' write_eda_summary
#'
#' @description Writes a machine-readable JSON summary of all key EDA findings.
#'   This file is consumed by the model architecture decision process and
#'   referenced in air_yards_v_0_1_0.md Section 2.
#'
#' @param ay_stats Named list of overall distribution statistics.
#' @param modal Named list of multimodality findings.
#' @param corr_results Named list of correlation outputs.
#' @param output_dir Character. Write path.
write_eda_summary <- function(ay_stats, modal, corr_results, output_dir) {
  summary <- list(
    version           = "V.0.1.0",
    generated         = as.character(Sys.time()),
    seasons_analyzed  = paste(range(SEASONS), collapse = "-"),
    n_plays           = ay_stats$n,
    distribution = list(
      mean_ay           = round(ay_stats$mean_ay, 2),
      median_ay         = round(ay_stats$median_ay, 2),
      sd_ay             = round(ay_stats$sd_ay, 2),
      pct_screen        = round(ay_stats$pct_screen, 4),
      pct_intermediate  = round(ay_stats$pct_intermediate, 4),
      pct_deep_shot     = round(ay_stats$pct_deep, 4),
      pct_negative      = round(ay_stats$pct_negative, 4)
    ),
    multimodality = list(
      zero_inflated_pct  = round(modal$pct_zero_inflated, 4),
      deep_shot_cluster_start_yds = DEEP_SHOT_THRESH,
      is_zero_inflated   = modal$pct_zero_inflated > 0.10
    ),
    ngs_join_required  = corr_results$ngs_join_needed,
    key_correlations   = as.list(setNames(corr_results$correlations$pearson,
                                          corr_results$correlations$feature))
  )
  write_json(summary, file.path(output_dir, "ay_eda_summary_v_0_1_0.json"),
             pretty = TRUE, auto_unbox = TRUE)
  message("\n  EDA summary written to: ", file.path(output_dir, "ay_eda_summary_v_0_1_0.json"))
}

write_eda_summary(ay_stats, modal_findings, corr_results, OUTPUT_DIR)

message("\n=== EDA COMPLETE ===")
message("All outputs saved to: ", OUTPUT_DIR)
