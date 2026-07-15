library(xgboost); library(data.table)
wrapper <- readRDS("R/models/win_probability/wp_model.rds")
booster <- wrapper$model
features <- wrapper$feature_names
lv <- readRDS("R/models/win_probability/wp_levels.rds")
enc <- function(x, l) as.integer(factor(as.character(x), levels = l))

run <- function(sc) {
  dt <- copy(sc)
  for (f in features) {
    if (f %in% names(lv)) dt[[f]] <- enc(dt[[f]], lv[[f]])
    else dt[[f]] <- suppressWarnings(as.numeric(dt[[f]]))
  }
  round(predict(booster, as.matrix(dt[, features, with = FALSE])) * 100, 2)
}

# 1) Up 7, 1 min left — should be ~90%+
s1 <- data.table(surface="fieldturf", roof="dome", temp=68, wind=0,
  season_type="POST", week=22,
  game_seconds_remaining=60, score_differential=7,
  down=1, yardline_100=60, ydstogo=10,
  posteam_timeouts_remaining=3, defteam_timeouts_remaining=0,
  spread_line=-1.5, total_line=49.5)
cat("Up 7, 1 min left, spread=-1.5:", run(s1), "%\n")

# 2) Down 7, 1 min left — should be ~10% or less
s2 <- copy(s1); s2[, score_differential := -7]
cat("Down 7, 1 min left, spread=-1.5:", run(s2), "%\n")

# 3) Tied, ball, 5 min left, spread=0 (neutral) — expect ~55%
s3 <- data.table(surface="fieldturf", roof="dome", temp=68, wind=0,
  season_type="POST", week=22,
  game_seconds_remaining=315, score_differential=0,
  down=1, yardline_100=75, ydstogo=10,
  posteam_timeouts_remaining=3, defteam_timeouts_remaining=2,
  spread_line=0, total_line=49.5)
cat("Tied, ball, 5min, spread=0:", run(s3), "%\n")

# 4) Same but possessing team favoured (spread_line=-1.5)
s4 <- copy(s3); s4[, spread_line := -1.5]
cat("Tied, ball, 5min, spread=-1.5 (posteam fav):", run(s4), "%\n")

# 5) Same but possessing team is underdog (spread_line=+1.5)
s5 <- copy(s3); s5[, spread_line := 1.5]
cat("Tied, ball, 5min, spread=+1.5 (posteam dog):", run(s5), "%\n")
