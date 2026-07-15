library(nflreadr)
library(dplyr)

cat("--- Extracting PFR Advanced Pressure Stats (2018-2024) ---\n")
years <- 2018:2024

pfr_adv <- load_pfr_advstats(years, stat_type = "pass")

# Clean and aggregate
pfr_clean <- pfr_adv %>%
  select(
    season, week, team, player = pfr_player_name, 
    times_pressured, times_sacked, times_hurried, times_hit, times_blitzed,
    passing_bad_throws, passing_bad_throw_pct
  )

# Save to data/external
dir.create("data/external", showWarnings = FALSE)
write.csv(pfr_clean, "data/external/pfr_adv_passing_stats.csv", row.names = FALSE)

cat("Successfully saved PFR Advanced Stats for", nrow(pfr_clean), "player-weeks.\n")
