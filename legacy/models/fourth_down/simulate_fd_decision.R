# R/models/fourth_down/simulate_fd_decision.R

suppressPackageStartupMessages({
  library(data.table)
})
source("R/models/win_probability/predict_wp.R")
source("R/models/field_goal/predict_fg.R")
source("R/models/fourth_down/predict_fd.R")

simulate_fd_decision <- function(row_df) {
  dt <- as.data.table(row_df)
  if (nrow(dt) == 0) return(dt)
  
  # Calculate pre-snap Win Probability
  dt[, pre_wp := predict_wp(dt)]
  
  # Helper to flip possession
  flip_possession <- function(d) {
    d2 <- copy(d)
    d2[, posteam_type := fifelse(posteam_type == "home", "away", "home")]
    d2[, score_differential := -score_differential]
    # Swap timeouts
    tmp <- d2$posteam_timeouts_remaining
    d2[, posteam_timeouts_remaining := defteam_timeouts_remaining]
    d2[, defteam_timeouts_remaining := tmp]
    d2
  }
  
  # 1. PUNT
  dt_punt <- flip_possession(dt)
  # Assume 40 net yards. Opponent takes over at 100 - yardline_100 + 40
  opp_y100 <- 100 - dt$yardline_100 + 40
  opp_y100 <- fifelse(opp_y100 >= 100, 80, opp_y100) # Touchback to the 20
  dt_punt[, yardline_100 := opp_y100]
  dt_punt[, down := 1]
  dt_punt[, ydstogo := 10]
  wp_punt <- 1 - predict_wp(dt_punt)
  
  # 2. FIELD GOAL
  prob_fg <- predict_fg(dt)
  
  # FG Made
  dt_fg_made <- flip_possession(dt)
  dt_fg_made[, score_differential := score_differential - 3] # opponent is now down an extra 3
  dt_fg_made[, yardline_100 := 75] # Kickoff touchback
  dt_fg_made[, down := 1]
  dt_fg_made[, ydstogo := 10]
  wp_fg_made <- 1 - predict_wp(dt_fg_made)
  
  # FG Missed
  dt_fg_miss <- flip_possession(dt)
  # Spot of kick is approx LOS + 7. Opponent yardline is 100 - (LOS + 7)
  miss_y100 <- 100 - (dt$yardline_100 + 7)
  miss_y100 <- fifelse(dt$yardline_100 + 7 < 20, 80, miss_y100) # Miss inside 20 goes to 20
  dt_fg_miss[, yardline_100 := miss_y100]
  dt_fg_miss[, down := 1]
  dt_fg_miss[, ydstogo := 10]
  wp_fg_miss <- 1 - predict_wp(dt_fg_miss)
  
  wp_fg <- prob_fg * wp_fg_made + (1 - prob_fg) * wp_fg_miss
  
  # 3. GO FOR IT
  prob_go <- predict_fd_conversion(dt)
  
  # Go Converted
  dt_go_made <- copy(dt)
  dt_go_made[, yardline_100 := yardline_100 - ydstogo]
  dt_go_made[, down := 1]
  dt_go_made[, ydstogo := 10]
  
  # Check if Touchdown
  is_td <- dt_go_made$yardline_100 <= 0
  
  # Handle TD case
  dt_td <- flip_possession(dt_go_made)
  dt_td[, score_differential := score_differential - 7] # opponent down extra 7 (assume PAT)
  dt_td[, yardline_100 := 75]
  
  wp_go_made <- fifelse(is_td, 1 - predict_wp(dt_td), predict_wp(dt_go_made))
  
  # Go Failed
  dt_go_miss <- flip_possession(dt)
  dt_go_miss[, yardline_100 := 100 - dt$yardline_100]
  dt_go_miss[, down := 1]
  dt_go_miss[, ydstogo := 10]
  wp_go_miss <- 1 - predict_wp(dt_go_miss)
  
  wp_go <- prob_go * wp_go_made + (1 - prob_go) * wp_go_miss
  
  # Suppress punt and FG if impossible/illogical
  dt[, wp_punt := fifelse(yardline_100 <= 30, NA_real_, wp_punt)]
  dt[, wp_fg := fifelse(yardline_100 > 65, NA_real_, wp_fg)]
  
  # Logical Override: Must GO if down 2+ scores late in 4th quarter
  dt[, wp_punt := fifelse(qtr == 4 & score_differential <= -14 & game_seconds_remaining < 600, NA_real_, wp_punt)]
  dt[, wp_fg := fifelse(qtr == 4 & score_differential <= -14 & game_seconds_remaining < 600, NA_real_, wp_fg)]
  
  dt[, wp_go := wp_go]
  
  # Best action
  dt[, best_action := apply(dt[, .(wp_go, wp_punt, wp_fg)], 1, function(x) {
    names(x)[which.max(x)]
  })]
  dt[, best_action := sub("wp_", "", best_action)]
  
  return(as.data.frame(dt))
}
