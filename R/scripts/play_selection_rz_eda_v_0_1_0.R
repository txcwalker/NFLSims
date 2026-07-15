suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

plays <- read.csv("data/model_1_training_data.csv", stringsAsFactors = FALSE)

plays <- plays %>%
  filter(play_type %in% c("run", "pass"), !is.na(down), !is.na(ydstogo), !is.na(yardline_100)) %>%
  mutate(
    is_pass = as.integer(play_type == "pass"),
    dist_bucket = case_when(
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
      down == 4 & ydstogo >= 4 & ydstogo <= 6 ~ "4_med",
      down == 4 & ydstogo >= 7                ~ "4_long",
      TRUE                                    ~ "other"
    )
  ) %>%
  filter(dist_bucket != "other")

rz     <- plays %>% filter(yardline_100 <= 20)
non_rz <- plays %>% filter(yardline_100 >  20)

bucket_order <- c("1_10","1_short","1_long","2_short","2_med","2_long",
                  "3_short","3_med","3_long","4_short","4_med","4_long")

overall_s <- plays %>%
  group_by(dist_bucket) %>%
  summarise(overall_n = n(), overall_pass = round(mean(is_pass), 3), .groups = "drop")

rz_s <- rz %>%
  group_by(dist_bucket) %>%
  summarise(rz_n = n(), rz_pass = round(mean(is_pass), 3), .groups = "drop")

non_rz_s <- non_rz %>%
  group_by(dist_bucket) %>%
  summarise(non_rz_n = n(), non_rz_pass = round(mean(is_pass), 3), .groups = "drop")

result <- overall_s %>%
  left_join(rz_s,     by = "dist_bucket") %>%
  left_join(non_rz_s, by = "dist_bucket") %>%
  mutate(
    dist_bucket = factor(dist_bucket, levels = bucket_order),
    delta = round(rz_pass - non_rz_pass, 3)
  ) %>%
  arrange(dist_bucket)

cat("\n=== Red Zone EDA: Pass Rate by Bucket (Red Zone = yardline <= 20) ===\n")
cat(sprintf("%-10s  %8s %8s   %8s %8s   %8s\n",
            "Bucket", "All_N", "All_%", "RZ_N", "RZ_%", "Delta"))
cat(strrep("-", 62), "\n")

for (i in seq_len(nrow(result))) {
  r <- result[i, ]
  rz_n_val   <- ifelse(is.na(r$rz_n),   0,  r$rz_n)
  rz_p_val   <- ifelse(is.na(r$rz_pass), NA, r$rz_pass)
  delta_val  <- ifelse(is.na(r$delta),  NA, r$delta)
  cat(sprintf("%-10s  %8d %8.3f   %8d %8s   %8s\n",
              as.character(r$dist_bucket),
              r$overall_n,
              r$overall_pass,
              rz_n_val,
              ifelse(is.na(rz_p_val), "  NA", sprintf("%.3f", rz_p_val)),
              ifelse(is.na(delta_val), "  NA", sprintf("%+.3f", delta_val))))
}

cat("\nDelta = RZ pass rate minus non-RZ pass rate\n")
cat("Positive delta = team passes MORE often in the red zone for that bucket\n")
cat("Negative delta = team runs MORE often in the red zone for that bucket\n")

cat("\n--- Red Zone Overall Summary ---\n")
cat("Total RZ plays:    ", nrow(rz), "\n")
cat("RZ overall pass %: ", round(mean(rz$is_pass), 3), "\n")
cat("Non-RZ pass %:     ", round(mean(non_rz$is_pass), 3), "\n")
