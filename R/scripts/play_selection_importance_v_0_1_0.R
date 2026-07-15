suppressPackageStartupMessages({ library(dplyr); library(xgboost); library(ggplot2) })

MODEL_DIR <- "src/nfl_sim/models/play_selection_v_0_1_0"
PLOT_DIR  <- "docs/eda_outputs/play_selection"

FEATURES <- c("yardline_100","game_seconds_remaining","score_differential",
              "posteam_timeouts_remaining","defteam_timeouts_remaining","leverage")

buckets <- c("1_10","1_short","1_long","2_short","2_med","2_long",
             "3_short","3_med","3_long","4_short","4_med_long")

# ── Aggregate Gain importance across all submodels ────────────────────────────
# Gain = fractional improvement in loss for splits on this feature.
# We weight each model's importance by its training set size so larger buckets
# contribute proportionally more to the overall picture.
all_importance <- list()

for (bucket in buckets) {
  mp <- file.path(MODEL_DIR, paste0(bucket, ".json"))
  if (!file.exists(mp)) next
  m   <- xgb.load(mp)
  # Pass feature names explicitly so importance shows real names, not f0/f1/...
  imp <- xgb.importance(feature_names = FEATURES, model = m)
  if (nrow(imp) == 0) next
  imp$bucket <- bucket
  all_importance[[bucket]] <- imp
}

combined <- bind_rows(all_importance)

# Aggregate: mean Gain per feature across all submodels (unweighted for simplicity)
agg <- combined %>%
  group_by(Feature) %>%
  summarise(mean_gain = mean(Gain), .groups = "drop") %>%
  arrange(desc(mean_gain)) %>%
  mutate(
    Feature = case_when(
      Feature == "game_seconds_remaining"    ~ "Game Seconds Remaining",
      Feature == "score_differential"        ~ "Score Differential",
      Feature == "leverage"                  ~ "Leverage (Score × Time)",
      Feature == "yardline_100"              ~ "Yardline (yards to EZ)",
      Feature == "posteam_timeouts_remaining" ~ "Offensive Timeouts",
      Feature == "defteam_timeouts_remaining" ~ "Defensive Timeouts",
      TRUE ~ Feature
    ),
    Feature = factor(Feature, levels = rev(Feature))
  )

p <- ggplot(agg, aes(x = Feature, y = mean_gain)) +
  geom_col(fill = "#2c7bb6", width = 0.65, alpha = 0.88) +
  geom_text(aes(label = sprintf("%.3f", mean_gain)),
            hjust = -0.1, size = 3.8, color = "grey30") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Feature Importance (Mean Gain) — Play Type Selection V.0.1.0",
    subtitle = "Averaged across all 11 submodels. Gain = fractional improvement in model loss from splits on this feature.",
    x        = NULL,
    y        = "Mean Gain"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

out_path <- file.path(PLOT_DIR, "feature_importance_v_0_1_0.png")
ggsave(out_path, p, width = 8, height = 5, dpi = 150)
cat("Saved:", out_path, "\n\n")

cat("=== Feature Importance (Mean Gain across submodels) ===\n")
print(agg %>% mutate(mean_gain = round(mean_gain, 4)), row.names = FALSE)
