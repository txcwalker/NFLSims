library(nflreadr)
library(dplyr)

cat("--- Extracting PFR Advanced Rushing Stats (2018-2024) ---\n")
years <- 2018:2024

# Load Advanced Rushing Stats
pfr_rush <- load_pfr_advstats(years, stat_type = "rush")

# Clean and aggregate
pfr_rush_clean <- pfr_rush %>%
  select(
    season, week, team, player = pfr_player_name, 
    ybc = rushing_yards_before_contact, 
    ybc_avg = rushing_yards_before_contact_avg,
    yac = rushing_yards_after_contact, 
    yac_avg = rushing_yards_after_contact_avg,
    broken_tackles = rushing_broken_tackles
  )

# Save to data/external
write.csv(pfr_rush_clean, "data/external/pfr_adv_rushing_stats.csv", row.names = FALSE)

cat("Successfully saved PFR Advanced Rushing Stats for", nrow(pfr_rush_clean), "player-weeks.\n")
