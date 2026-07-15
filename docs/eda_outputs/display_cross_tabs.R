library(dplyr)
library(tidyr)

OUT <- "docs/eda_outputs/play_selection"

cat("\n=== EDA 4: Pass Rate by Year x Bucket (pivoted wide) ===\n")
df4 <- read.csv(file.path(OUT, "4_pass_rate_by_year_and_bucket.csv"))
wide4 <- df4 %>%
  select(season, dist_bucket, pass_rate) %>%
  pivot_wider(names_from = season, values_from = pass_rate)
print(as.data.frame(wide4), row.names = FALSE)

cat("\n=== EDA 4: Observation Count by Year x Bucket (pivoted wide) ===\n")
wide4n <- df4 %>%
  select(season, dist_bucket, n_plays) %>%
  pivot_wider(names_from = season, values_from = n_plays)
print(as.data.frame(wide4n), row.names = FALSE)

cat("\n=== EDA 6: Pass Rate by Quarter x Bucket (pivoted wide) ===\n")
df6 <- read.csv(file.path(OUT, "6_pass_rate_by_quarter_and_bucket.csv"))
wide6 <- df6 %>%
  select(qtr, dist_bucket, pass_rate) %>%
  pivot_wider(names_from = qtr, values_from = pass_rate, names_prefix = "Q")
print(as.data.frame(wide6), row.names = FALSE)

cat("\n=== EDA 8: Pass Rate by Yardline Zone ===\n")
df8 <- read.csv(file.path(OUT, "8_pass_rate_by_yardline_zone.csv"))
print(as.data.frame(df8 %>% arrange(pass_rate)), row.names = FALSE)
