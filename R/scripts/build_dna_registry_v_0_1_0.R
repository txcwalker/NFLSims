# =============================================================================
# DNA Registry Population Build — V.0.1.0
# Script: R/scripts/build_dna_registry_v_0_1_0.R
#
# Purpose: Computes all nflfastR-derivable DNA fields for QB, Skill, Coach,
#   and Trench DNA registries. NGS-only fields are written as null and flagged.
#   Output overwrites the seed JSON files in data/dna/.
#
# Inputs:
#   - nflfastR PBP 2015-2024 (wider window for career avg reliability)
#   - nflreadr rosters 2015-2024 (for player position mapping)
#
# Outputs:
#   - data/dna/qb_dna.json
#   - data/dna/skill_dna.json
#   - data/dna/coach_dna.json
#   - data/dna/trench_dna.json
#
# Thresholds:
#   QB    : >= 100 pass attempts in a season to count as observed
#   Skill : >= 30 targets in a season to count as observed
#   Coach : >= 100 offensive pass plays in a season
#   Trench: All 32 teams, all seasons
# =============================================================================

library(nflreadr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(purrr)

# --- CONFIG ------------------------------------------------------------------
SEASONS        <- 2015:2024
DNA_DIR        <- "data/dna"
QB_MIN_ATT     <- 100   # min pass attempts per season
SKILL_MIN_TGT  <- 30    # min targets per season
COACH_MIN_PLAYS <- 100  # min pass plays per season
DEEP_THRESH    <- 20    # air_yards >= 20 = deep shot
SCREEN_THRESH  <- 0     # air_yards <= 0  = screen/behind LOS

dir.create(DNA_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOAD DATA
# =============================================================================
message("Loading PBP ", min(SEASONS), "-", max(SEASONS), "...")
pbp_raw <- load_pbp(SEASONS)

message("Loading rosters...")
rosters <- load_rosters(SEASONS) |>
  select(season, full_name, gsis_id, position, depth_chart_position) |>
  distinct()

# Base pass play filter — used across all DNA builds
pbp_pass <- pbp_raw |>
  filter(
    play_type  == "pass",
    is.na(qb_spike)     | qb_spike     == 0,
    is.na(aborted_play) | aborted_play == 0
  ) |>
  mutate(
    # Identify offensive coach per play
    off_coach = if_else(posteam == home_team, home_coach, away_coach),
    is_deep   = !is.na(air_yards) & air_yards >= DEEP_THRESH,
    is_screen = !is.na(air_yards) & air_yards <= SCREEN_THRESH,
    # play_action: encoded in play_type_nfl as 'play_action_pass'
    # no_huddle: standalone 0/1 column exists
    play_action = as.integer(grepl('play_action', coalesce(play_type_nfl, ''), ignore.case = TRUE)),
    no_huddle   = coalesce(as.integer(no_huddle), 0L)
  )

message("Pass plays loaded: ", format(nrow(pbp_pass), big.mark = ","))

# League-average air yards by season (for coach tendency delta)
league_ay <- pbp_pass |>
  filter(!is.na(air_yards)) |>
  group_by(season) |>
  summarise(league_mean_ay = mean(air_yards), .groups = "drop")

# =============================================================================
# HELPER: weighted career average
# Weight by n_obs so large seasons dominate over small samples.
# =============================================================================
#' weighted_career_avg
#'
#' @param df Tibble with columns: value, n_obs (weight).
#' @return Single weighted mean.
weighted_career_avg <- function(df) {
  if (nrow(df) == 0 || sum(df$n_obs, na.rm = TRUE) == 0) return(NA_real_)
  sum(df$value * df$n_obs, na.rm = TRUE) / sum(df$n_obs, na.rm = TRUE)
}

# =============================================================================
# SECTION 1 — QB DNA
# =============================================================================
#' build_qb_dna
#'
#' @description Computes per-QB career-weighted averages for all nflfastR-
#'   derivable DNA fields. Career average is weighted by pass attempts so that
#'   high-volume seasons carry more influence. QBs with < QB_MIN_ATT attempts
#'   in every season are excluded (insufficient sample).
#'
#' Fields computed:
#'   cpoe                  - Completion % Over Expected (nflfastR)
#'   avg_air_yards_per_att - Mean air yards per pass attempt
#'   deep_ball_rate        - Proportion of attempts with air_yards >= 20
#'   scramble_rate         - Proportion of pass plays with qb_scramble == 1
#'   sack_rate             - Sacks / (Sacks + Attempts)
#'   play_action_rate      - Proportion of pass plays with play_action == 1
#'   under_pressure_cpoe   - Mean CPOE when QB is hit (qb_hit == 1)
#'   seasons_observed      - Seasons with >= QB_MIN_ATT attempts
#'
#' NGS fields (null in this build):
#'   ngs_aggressiveness_index, avg_time_to_throw_sec
#'
#' @param pbp Tibble. Filtered pass play PBP.
#' @return Named list ready for JSON serialization.
build_qb_dna <- function(pbp) {
  message("  Building QB DNA...")

  # Per-season QB stats
  qb_season <- pbp |>
    filter(!is.na(passer_player_name)) |>
    group_by(season, passer_player_name) |>
    summarise(
      n_att         = sum(pass_attempt == 1, na.rm = TRUE),
      n_sack        = sum(sack == 1, na.rm = TRUE),
      cpoe          = mean(cpoe, na.rm = TRUE),
      avg_ay        = mean(air_yards, na.rm = TRUE),
      deep_rate     = mean(is_deep, na.rm = TRUE),
      scramble_rate = mean(qb_scramble == 1, na.rm = TRUE),
      pa_rate       = mean(play_action == 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(sack_rate = n_sack / (n_sack + n_att))

  # Under-pressure CPOE (career, unweighted — small sample per season)
  qb_pressure <- pbp |>
    filter(!is.na(passer_player_name), qb_hit == 1, !is.na(cpoe)) |>
    group_by(passer_player_name) |>
    summarise(
      under_pressure_cpoe = mean(cpoe, na.rm = TRUE),
      n_pressure_plays    = n(),
      .groups = "drop"
    ) |>
    # Require at least 20 pressure plays for a reliable estimate
    filter(n_pressure_plays >= 20)

  # Career-weighted averages (weight by n_att)
  qb_career <- qb_season |>
    filter(n_att >= QB_MIN_ATT) |>
    group_by(passer_player_name) |>
    summarise(
      seasons_observed      = n(),
      total_attempts        = sum(n_att),
      cpoe                  = weighted_career_avg(data.frame(value = cpoe, n_obs = n_att)),
      avg_air_yards_per_att = weighted_career_avg(data.frame(value = avg_ay, n_obs = n_att)),
      deep_ball_rate        = weighted_career_avg(data.frame(value = deep_rate, n_obs = n_att)),
      scramble_rate         = weighted_career_avg(data.frame(value = scramble_rate, n_obs = n_att)),
      sack_rate             = sum(n_sack) / (sum(n_sack) + sum(n_att)),
      play_action_rate      = weighted_career_avg(data.frame(value = pa_rate, n_obs = n_att)),
      .groups = "drop"
    ) |>
    left_join(qb_pressure, by = "passer_player_name")

  # Build named list
  qb_list <- list(
    `_metadata` = list(
      version        = "V.0.1.0",
      created        = as.character(Sys.Date()),
      source         = "nflfastR PBP 2015-2024",
      seasons        = paste(range(SEASONS), collapse = "-"),
      min_attempts   = QB_MIN_ATT,
      ngs_fields_null = list("ngs_aggressiveness_index", "avg_time_to_throw_sec"),
      deprecation_notice = paste(
        "temporal_cpoe_atlas.json superseded for Air Yards modeling by this file.",
        "Do NOT delete — still active for play_selection overlay."
      )
    )
  )

  for (i in seq_len(nrow(qb_career))) {
    row <- qb_career[i, ]
    nm  <- row$passer_player_name
    qb_list[[nm]] <- list(
      cpoe                  = round(row$cpoe, 4),
      avg_air_yards_per_att = round(row$avg_air_yards_per_att, 3),
      deep_ball_rate        = round(row$deep_ball_rate, 4),
      scramble_rate         = round(row$scramble_rate, 4),
      sack_rate             = round(row$sack_rate, 4),
      play_action_rate      = round(row$play_action_rate, 4),
      under_pressure_cpoe   = if (!is.na(row$under_pressure_cpoe))
                                round(row$under_pressure_cpoe, 4) else NULL,
      ngs_aggressiveness_index = NULL,
      avg_time_to_throw_sec    = NULL,
      seasons_observed      = as.integer(row$seasons_observed),
      total_attempts        = as.integer(row$total_attempts)
    )
  }

  message("    QBs written: ", nrow(qb_career))
  qb_list
}

# =============================================================================
# SECTION 2 — SKILL DNA (WR / TE / RB)
# =============================================================================
#' build_skill_dna
#'
#' @description Computes per-receiver career-weighted DNA for WR, TE, and RB.
#'   Weighted by target count. Requires a position lookup from rosters.
#'
#' Fields computed:
#'   position           - WR | TE | RB
#'   target_share       - Targets / Team pass attempts (career avg)
#'   catch_rate         - Receptions / Targets
#'   avg_target_depth   - Mean air yards on targets (avg_target_depth_yds)
#'   deep_target_rate   - Proportion of own targets that are >= 20 air yards
#'   air_yards_share    - Player's air yards / Team total air yards (career avg)
#'   yac_per_reception  - Mean yards_after_catch per reception
#'   route_profile      - Derived: short(<7) / intermediate(7-15) / deep(>15)
#'   seasons_observed   - Seasons with >= SKILL_MIN_TGT targets
#'
#' NGS fields (null): avg_separation_yds, top_speed_mph
#'
#' @param pbp Tibble. Filtered pass play PBP.
#' @param rosters Tibble. Player roster data with positions.
#' @return Named list ready for JSON serialization.
build_skill_dna <- function(pbp, rosters) {
  message("  Building Skill DNA...")

  # Team-season totals for share calculations
  team_season_totals <- pbp |>
    filter(!is.na(receiver_player_name)) |>
    group_by(season, posteam) |>
    summarise(
      team_targets  = n(),
      team_air_yards = sum(air_yards, na.rm = TRUE),
      .groups = "drop"
    )

  # Per-player per-season
  skill_season <- pbp |>
    filter(!is.na(receiver_player_name)) |>
    group_by(season, posteam, receiver_player_name) |>
    summarise(
      n_targets     = n(),
      n_receptions  = sum(complete_pass == 1, na.rm = TRUE),
      sum_air_yards = sum(air_yards, na.rm = TRUE),
      avg_tgt_depth = mean(air_yards, na.rm = TRUE),
      deep_tgt_rate = mean(is_deep, na.rm = TRUE),
      yac_per_rec   = mean(yards_after_catch[complete_pass == 1], na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(team_season_totals, by = c("season", "posteam")) |>
    mutate(
      target_share    = n_targets / team_targets,
      air_yards_share = sum_air_yards / team_air_yards,
      catch_rate      = n_receptions / n_targets
    )

  # Position lookup — use most common position across seasons
  pos_lookup <- rosters |>
    filter(position %in% c("WR", "TE", "RB")) |>
    group_by(full_name, position) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(full_name) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    select(receiver_player_name = full_name, position)

  # Career-weighted averages (weight by n_targets)
  skill_career <- skill_season |>
    filter(n_targets >= SKILL_MIN_TGT) |>
    group_by(receiver_player_name) |>
    summarise(
      seasons_observed  = n(),
      total_targets     = sum(n_targets),
      target_share      = weighted_career_avg(data.frame(value = target_share,    n_obs = n_targets)),
      catch_rate        = weighted_career_avg(data.frame(value = catch_rate,       n_obs = n_targets)),
      avg_target_depth  = weighted_career_avg(data.frame(value = avg_tgt_depth,   n_obs = n_targets)),
      deep_target_rate  = weighted_career_avg(data.frame(value = deep_tgt_rate,   n_obs = n_targets)),
      air_yards_share   = weighted_career_avg(data.frame(value = air_yards_share, n_obs = n_targets)),
      yac_per_reception = weighted_career_avg(data.frame(value = yac_per_rec,     n_obs = n_targets)),
      .groups = "drop"
    ) |>
    left_join(pos_lookup, by = "receiver_player_name") |>
    mutate(
      position     = coalesce(position, "WR"),  # default WR if lookup misses
      route_profile = case_when(
        avg_target_depth <  7  ~ "short",
        avg_target_depth <= 15 ~ "intermediate",
        TRUE                   ~ "deep"
      )
    )

  # Build named list
  skill_list <- list(
    `_metadata` = list(
      version   = "V.0.1.0",
      created   = as.character(Sys.Date()),
      source    = "nflfastR PBP 2015-2024",
      min_targets = SKILL_MIN_TGT,
      ngs_fields_null = list("avg_separation_yds", "top_speed_mph"),
      deprecation_notice = paste(
        "personnel_talent_atlas.json (alpha_idx, reliability_idx) remains active",
        "for chaos model. This file supersedes it for Air Yards modeling only."
      )
    )
  )

  for (i in seq_len(nrow(skill_career))) {
    row <- skill_career[i, ]
    nm  <- row$receiver_player_name
    skill_list[[nm]] <- list(
      position          = row$position,
      target_share      = round(row$target_share, 4),
      catch_rate        = round(row$catch_rate, 4),
      avg_target_depth_yds = round(row$avg_target_depth, 3),
      deep_target_rate  = round(row$deep_target_rate, 4),
      air_yards_share   = round(row$air_yards_share, 4),
      yac_per_reception = round(row$yac_per_reception, 3),
      route_profile     = row$route_profile,
      avg_separation_yds = NULL,
      top_speed_mph      = NULL,
      seasons_observed  = as.integer(row$seasons_observed),
      total_targets     = as.integer(row$total_targets)
    )
  }

  message("    Skill players written: ", nrow(skill_career))
  skill_list
}

# =============================================================================
# SECTION 3 — COACH DNA
# =============================================================================
#' build_coach_dna
#'
#' @description Computes per-coach career air-yards-specific DNA.
#'   Keyed by coach name (offensive play-caller). Weighted by pass plays.
#'
#' Fields computed:
#'   proe                 - Mean pass rate delta vs league avg (existing atlas)
#'   air_yards_tendency   - Mean air yards minus league mean for that season
#'   deep_shot_rate       - Proportion of pass plays with air_yards >= 20
#'   screen_rate          - Proportion of pass plays with air_yards <= 0
#'   play_action_rate     - Proportion of pass plays with play_action == 1
#'   no_huddle_rate       - Proportion of pass plays with no_huddle == 1
#'   rpo_rate             - Proportion of pass plays that are RPOs
#'   conservative_score_bias - Pearson r(score_diff, air_yards) per coach
#'                             Negative = leads depresses air yards (conservative)
#'   seasons_observed     - Seasons with >= COACH_MIN_PLAYS pass plays
#'   total_pass_plays     - Career pass play count
#'
#' @param pbp Tibble. Filtered pass play PBP.
#' @param league_ay Tibble. League mean air yards per season.
#' @return Named list ready for JSON serialization.
build_coach_dna <- function(pbp, league_ay) {
  message("  Building Coach DNA...")

  pbp_coach <- pbp |>
    filter(!is.na(off_coach), !is.na(air_yards)) |>
    left_join(league_ay, by = "season") |>
    mutate(ay_vs_league = air_yards - league_mean_ay)

  # Conservative score bias: correlation between score_diff and air_yards
  # Negative = when leading (positive score_diff), coach throws shorter
  coach_bias <- pbp_coach |>
    filter(!is.na(score_differential)) |>
    group_by(off_coach) |>
    summarise(
      conservative_score_bias = cor(score_differential, air_yards,
                                    use = "complete.obs"),
      .groups = "drop"
    )

  # Per-season per-coach
  coach_season <- pbp_coach |>
    group_by(season, off_coach) |>
    summarise(
      n_plays       = n(),
      mean_ay_vs_lg = mean(ay_vs_league, na.rm = TRUE),
      deep_rate     = mean(is_deep, na.rm = TRUE),
      screen_rate   = mean(is_screen, na.rm = TRUE),
      pa_rate       = mean(play_action == 1, na.rm = TRUE),
      nh_rate       = mean(no_huddle   == 1, na.rm = TRUE),
      # RPO: nflfastR has 'rush_attempt' + 'pass_attempt' combos or play_type_nfl
      rpo_rate      = mean(grepl("run_pass_option|rpo",
                                 coalesce(play_type_nfl, ""), ignore.case = TRUE),
                           na.rm = TRUE),
      .groups = "drop"
    )

  # Career-weighted averages
  coach_career <- coach_season |>
    filter(n_plays >= COACH_MIN_PLAYS) |>
    group_by(off_coach) |>
    summarise(
      seasons_observed   = n(),
      total_pass_plays   = sum(n_plays),
      air_yards_tendency = weighted_career_avg(data.frame(value = mean_ay_vs_lg, n_obs = n_plays)),
      deep_shot_rate     = weighted_career_avg(data.frame(value = deep_rate,     n_obs = n_plays)),
      screen_rate        = weighted_career_avg(data.frame(value = screen_rate,   n_obs = n_plays)),
      play_action_rate   = weighted_career_avg(data.frame(value = pa_rate,       n_obs = n_plays)),
      no_huddle_rate     = weighted_career_avg(data.frame(value = nh_rate,       n_obs = n_plays)),
      rpo_rate           = weighted_career_avg(data.frame(value = rpo_rate,      n_obs = n_plays)),
      .groups = "drop"
    ) |>
    left_join(coach_bias, by = "off_coach")

  # Build named list
  coach_list <- list(
    `_metadata` = list(
      version   = "V.0.1.0",
      created   = as.character(Sys.Date()),
      source    = "nflfastR PBP 2015-2024",
      key       = "Coach name (offensive play-caller, HC-level)",
      min_pass_plays = COACH_MIN_PLAYS,
      deprecation_notice = paste(
        "coordinator_atlas.json (off_proe, def_proe_allowed) remains active for",
        "play_selection overlay. This file supersedes it for Air Yards modeling."
      )
    )
  )

  for (i in seq_len(nrow(coach_career))) {
    row <- coach_career[i, ]
    nm  <- row$off_coach
    coach_list[[nm]] <- list(
      air_yards_tendency   = round(row$air_yards_tendency, 4),
      deep_shot_rate       = round(row$deep_shot_rate, 4),
      screen_rate          = round(row$screen_rate, 4),
      play_action_rate     = round(row$play_action_rate, 4),
      no_huddle_rate       = round(row$no_huddle_rate, 4),
      rpo_rate             = round(row$rpo_rate, 4),
      conservative_score_bias = round(row$conservative_score_bias, 4),
      seasons_observed     = as.integer(row$seasons_observed),
      total_pass_plays     = as.integer(row$total_pass_plays)
    )
  }

  message("    Coaches written: ", nrow(coach_career))
  coach_list
}

# =============================================================================
# SECTION 4 — TRENCH DNA
# =============================================================================
#' build_trench_dna
#'
#' @description Computes team-season trench DNA. Stored by season > team.
#'   Offensive metrics computed from own pass plays; defensive metrics from
#'   plays where this team is the defense.
#'
#' Fields computed (offensive team):
#'   sack_rate_allowed    - Sacks / (Sacks + Pass attempts)
#'   avg_air_yards_allowed_by_def - Mean air yards allowed by opposing defense
#'
#' Fields computed (defensive team — join as defteam):
#'   def_pressure_rate    - Proportion of pass plays with qb_hit == 1
#'   def_sack_rate        - Sacks generated / opposing pass attempts
#'   blitz_rate           - Proportion of pass plays with blitz == 1 (if available)
#'
#' NGS fields (null): off_pass_block_win_rate, times_to_pressure_sec
#'
#' @param pbp Tibble. Filtered pass play PBP.
#' @return Named list ready for JSON serialization, keyed by season > team.
build_trench_dna <- function(pbp) {
  message("  Building Trench DNA...")

  # Offensive trench — own pass plays
  off_trench <- pbp |>
    group_by(season, posteam) |>
    summarise(
      n_pass_att        = sum(pass_attempt == 1, na.rm = TRUE),
      n_sacks_allowed   = sum(sack == 1, na.rm = TRUE),
      sack_rate_allowed = sum(sack == 1, na.rm = TRUE) /
                          (sum(sack == 1, na.rm = TRUE) + sum(pass_attempt == 1, na.rm = TRUE)),
      avg_air_yards     = mean(air_yards, na.rm = TRUE),
      .groups = "drop"
    )

  # Defensive trench — plays where this team is defense
  def_trench <- pbp |>
    group_by(season, defteam) |>
    summarise(
      n_opp_attempts    = sum(pass_attempt == 1, na.rm = TRUE),
      def_pressure_rate = mean(qb_hit == 1, na.rm = TRUE),
      def_sack_rate     = sum(sack == 1, na.rm = TRUE) /
                          (sum(sack == 1, na.rm = TRUE) + sum(pass_attempt == 1, na.rm = TRUE)),
      blitz_rate        = if ("blitz" %in% names(pbp))
                            mean(blitz == 1, na.rm = TRUE)
                          else NA_real_,
      avg_ay_allowed    = mean(air_yards, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(posteam = defteam)

  trench_combined <- off_trench |>
    left_join(def_trench, by = c("season", "posteam"))

  # Build nested list: season -> team -> metrics
  trench_list <- list(
    `_metadata` = list(
      version   = "V.0.1.0",
      created   = as.character(Sys.Date()),
      source    = "nflfastR PBP 2015-2024",
      structure = "season > team_abbreviation > metrics",
      ngs_fields_null = list("off_pass_block_win_rate", "times_to_pressure_sec"),
      blitz_note = "blitz_rate is NA if nflfastR version does not include blitz column",
      deprecation_notice = paste(
        "trench_tiers_2025.json (1-5 tier grades) remains active for play_selection.",
        "This file supersedes it for Air Yards modeling with continuous metrics."
      )
    )
  )

  seasons_list <- split(trench_combined, trench_combined$season)
  for (szn in names(seasons_list)) {
    szn_df <- seasons_list[[szn]]
    team_list <- list()
    for (i in seq_len(nrow(szn_df))) {
      row <- szn_df[i, ]
      team_list[[row$posteam]] <- list(
        sack_rate_allowed  = round(row$sack_rate_allowed, 4),
        avg_air_yards      = round(row$avg_air_yards, 3),
        def_pressure_rate  = round(row$def_pressure_rate, 4),
        def_sack_rate      = round(row$def_sack_rate, 4),
        blitz_rate         = if (!is.na(row$blitz_rate)) round(row$blitz_rate, 4) else NULL,
        avg_ay_allowed     = round(row$avg_ay_allowed, 3),
        off_pass_block_win_rate  = NULL,
        times_to_pressure_sec    = NULL
      )
    }
    trench_list[[szn]] <- team_list
  }

  message("    Trench season-teams written: ", nrow(trench_combined))
  trench_list
}

# =============================================================================
# MAIN — RUN ALL BUILDS AND WRITE JSON
# =============================================================================
message("\n=== STARTING DNA REGISTRY BUILD ===\n")

qb_dna     <- build_qb_dna(pbp_pass)
skill_dna  <- build_skill_dna(pbp_pass, rosters)
coach_dna  <- build_coach_dna(pbp_pass, league_ay)
trench_dna <- build_trench_dna(pbp_pass)

# Write JSON files
write_json(qb_dna,     file.path(DNA_DIR, "qb_dna.json"),     pretty = TRUE, auto_unbox = TRUE, na = "null")
write_json(skill_dna,  file.path(DNA_DIR, "skill_dna.json"),  pretty = TRUE, auto_unbox = TRUE, na = "null")
write_json(coach_dna,  file.path(DNA_DIR, "coach_dna.json"),  pretty = TRUE, auto_unbox = TRUE, na = "null")
write_json(trench_dna, file.path(DNA_DIR, "trench_dna.json"), pretty = TRUE, auto_unbox = TRUE, na = "null")

message("\n=== DNA BUILD COMPLETE ===")
message("Files written to: ", DNA_DIR)
message("  qb_dna.json     — ", length(qb_dna)    - 1, " QBs")
message("  skill_dna.json  — ", length(skill_dna) - 1, " skill players")
message("  coach_dna.json  — ", length(coach_dna) - 1, " coaches")
message("  trench_dna.json — ", length(trench_dna) - 1, " seasons")
