# =============================================================================
# NGS Augmentation — DNA Registry V.0.1.0
# Script: R/scripts/augment_dna_ngs_v_0_1_0.R
#
# Purpose: Fills the null NGS fields in the DNA registry files using
#   nflreadr::load_nextgen_stats(). Reads existing JSON files, patches null
#   fields in-place, and writes updated JSON back to data/dna/.
#
# Must run AFTER build_dna_registry_v_0_1_0.R.
#
# NGS fields populated by this script:
#   qb_dna.json:    avg_time_to_throw_sec, ngs_aggressiveness_index
#   skill_dna.json: avg_separation_yds
#
# NGS fields that REMAIN null (not available via nflreadr):
#   qb_dna:     (none remaining after this script)
#   skill_dna:  top_speed_mph          -- NGS speed tracking, not in nflreadr
#   trench_dna: off_pass_block_win_rate -- ESPN/PFF proprietary
#               times_to_pressure_sec  -- NGS pass rush, not in nflreadr
#
# Data source: nflreadr::load_nextgen_stats()
#   Passing NGS:   avg_time_to_throw, aggressiveness (per QB per week)
#   Receiving NGS: avg_separation (per receiver per week)
#   Seasons loaded: 2016-2024 (NGS data begins 2016 in nflreadr)
# =============================================================================

library(nflreadr)
library(dplyr)
library(jsonlite)

DNA_DIR   <- "data/dna"
NGS_START <- 2016   # NGS data begins 2016 in nflreadr
NGS_END   <- 2024
QB_MIN_WEEKS   <- 4    # minimum weeks with NGS data to compute a career avg
SKILL_MIN_WEEKS <- 4

# --- HELPER: weighted career average (same as build script) ------------------
#' weighted_career_avg
#'
#' @description Computes a weighted mean where each observation is weighted by
#'   n_obs (e.g., pass attempts or targets). Prevents small-sample seasons
#'   from distorting the career estimate.
#'
#' @param df Data.frame with columns value (numeric) and n_obs (weight).
#' @return Single weighted mean, or NA if no valid data.
weighted_career_avg <- function(df) {
  df <- df[!is.na(df$value) & !is.na(df$n_obs) & df$n_obs > 0, ]
  if (nrow(df) == 0) return(NA_real_)
  sum(df$value * df$n_obs) / sum(df$n_obs)
}

# =============================================================================
# SECTION 1 — LOAD NGS DATA
# =============================================================================
message("Loading NGS passing stats (", NGS_START, "-", NGS_END, ")...")
ngs_pass <- load_nextgen_stats(
  seasons   = NGS_START:NGS_END,
  stat_type = "passing"
) |>
  # Regular season only — postseason can skew career averages for backup QBs
  filter(season_type == "REG") |>
  select(
    season, week,
    player_display_name, player_short_name,
    team_abbr, attempts,
    avg_time_to_throw,
    aggressiveness
  )

message("  NGS passing rows: ", format(nrow(ngs_pass), big.mark = ","))

message("Loading NGS receiving stats (", NGS_START, "-", NGS_END, ")...")
ngs_recv <- load_nextgen_stats(
  seasons   = NGS_START:NGS_END,
  stat_type = "receiving"
) |>
  filter(season_type == "REG") |>
  select(
    season, week,
    player_display_name, player_short_name,
    team_abbr, targets,
    avg_separation,
    avg_cushion,
    avg_intended_air_yards,
    percent_share_of_intended_air_yards
  )

message("  NGS receiving rows: ", format(nrow(ngs_recv), big.mark = ","))

# =============================================================================
# SECTION 2 — COMPUTE QB NGS CAREER AVERAGES
# =============================================================================
#' compute_qb_ngs_averages
#'
#' @description Aggregates weekly NGS passing data into career-weighted averages
#'   per QB. Weighted by attempts per week so high-volume weeks dominate.
#'   Filters to QBs with >= QB_MIN_WEEKS weeks of data for reliability.
#'
#' @param ngs_pass Tibble. Weekly NGS passing data from load_nextgen_stats().
#' @return Tibble with one row per QB: player_display_name, avg_time_to_throw_sec,
#'   ngs_aggressiveness_index, ngs_weeks_observed.
compute_qb_ngs_averages <- function(ngs_pass) {
  message("  Computing QB NGS career averages...")

  qb_ngs <- ngs_pass |>
    filter(!is.na(avg_time_to_throw), !is.na(aggressiveness), attempts > 0) |>
    group_by(player_display_name) |>
    summarise(
      ngs_weeks_observed        = n(),
      total_ngs_attempts        = sum(attempts),
      avg_time_to_throw_sec     = weighted_career_avg(
                                    data.frame(value = avg_time_to_throw, n_obs = attempts)
                                  ),
      ngs_aggressiveness_index  = weighted_career_avg(
                                    data.frame(value = aggressiveness, n_obs = attempts)
                                  ),
      .groups = "drop"
    ) |>
    filter(ngs_weeks_observed >= QB_MIN_WEEKS)

  message("    QBs with NGS data: ", nrow(qb_ngs))
  qb_ngs
}

qb_ngs_avgs <- compute_qb_ngs_averages(ngs_pass)

# =============================================================================
# SECTION 3 — COMPUTE RECEIVER NGS CAREER AVERAGES
# =============================================================================
#' compute_receiver_ngs_averages
#'
#' @description Aggregates weekly NGS receiving data into career-weighted
#'   averages per receiver. Weighted by targets per week. avg_separation is
#'   the key field — it measures how much space the receiver creates, which
#'   directly conditions deep route viability and air yards distribution.
#'
#' @param ngs_recv Tibble. Weekly NGS receiving data.
#' @return Tibble with one row per receiver: player_display_name,
#'   avg_separation_yds, ngs_weeks_observed.
compute_receiver_ngs_averages <- function(ngs_recv) {
  message("  Computing receiver NGS career averages...")

  recv_ngs <- ngs_recv |>
    filter(!is.na(avg_separation), targets > 0) |>
    group_by(player_display_name) |>
    summarise(
      ngs_weeks_observed = n(),
      total_ngs_targets  = sum(targets),
      avg_separation_yds = weighted_career_avg(
                             data.frame(value = avg_separation, n_obs = targets)
                           ),
      avg_cushion_yds    = weighted_career_avg(
                             data.frame(value = avg_cushion, n_obs = targets)
                           ),
      .groups = "drop"
    ) |>
    filter(ngs_weeks_observed >= SKILL_MIN_WEEKS)

  message("    Receivers with NGS data: ", nrow(recv_ngs))
  recv_ngs
}

recv_ngs_avgs <- compute_receiver_ngs_averages(ngs_recv)

# =============================================================================
# SECTION 4 — PATCH QB DNA JSON
# =============================================================================
#' patch_qb_dna
#'
#' @description Reads qb_dna.json, fills null NGS fields with computed values
#'   where a name match exists between DNA keys and NGS player names.
#'
#'   Name matching challenge: nflfastR uses "F.Lastname" format
#'   (e.g., "P.Mahomes") while NGS uses full display names
#'   ("Patrick Mahomes"). The match is done by converting both to a
#'   standardized "F.Lastname" format for comparison.
#'
#' @param qb_ngs_avgs Tibble. QB NGS career averages from compute_qb_ngs_averages().
#' @param dna_dir Character. Path to data/dna/.
#' @return Invisibly returns the count of QBs patched.
patch_qb_dna <- function(qb_ngs_avgs, dna_dir) {
  message("  Patching qb_dna.json with NGS fields...")

  qb_dna <- read_json(file.path(dna_dir, "qb_dna.json"))

  # Build a lookup: "F.Lastname" -> NGS row
  # Convert NGS display name "Patrick Mahomes" -> "P.Mahomes"
  ngs_lookup <- qb_ngs_avgs |>
    mutate(
      short_key = paste0(
        substr(sapply(strsplit(player_display_name, " "), `[[`, 1), 1, 1),
        ".",
        sapply(strsplit(player_display_name, " "), function(x) paste(x[-1], collapse = ""))
      )
    )

  patched <- 0
  for (nm in names(qb_dna)) {
    if (startsWith(nm, "_")) next   # skip metadata keys

    # Try exact match on short_key
    match_row <- ngs_lookup |> filter(short_key == nm)

    # Fallback: try case-insensitive match
    if (nrow(match_row) == 0) {
      match_row <- ngs_lookup |>
        filter(tolower(short_key) == tolower(nm))
    }

    if (nrow(match_row) > 0) {
      row <- match_row[1, ]
      qb_dna[[nm]]$avg_time_to_throw_sec    <- round(row$avg_time_to_throw_sec, 3)
      qb_dna[[nm]]$ngs_aggressiveness_index <- round(row$ngs_aggressiveness_index, 2)
      qb_dna[[nm]]$ngs_weeks_observed       <- as.integer(row$ngs_weeks_observed)
      patched <- patched + 1
    }
  }

  write_json(qb_dna, file.path(dna_dir, "qb_dna.json"),
             pretty = TRUE, auto_unbox = TRUE, na = "null")
  message("    QBs patched with NGS: ", patched, " / ", length(qb_dna) - 1)
  invisible(patched)
}

# =============================================================================
# SECTION 5 — PATCH SKILL DNA JSON
# =============================================================================
#' patch_skill_dna
#'
#' @description Reads skill_dna.json, fills null avg_separation_yds with NGS
#'   values where a name match exists. top_speed_mph remains null — not
#'   available via nflreadr NGS.
#'
#' @param recv_ngs_avgs Tibble. Receiver NGS averages.
#' @param dna_dir Character. Path to data/dna/.
#' @return Invisibly returns count of players patched.
patch_skill_dna <- function(recv_ngs_avgs, dna_dir) {
  message("  Patching skill_dna.json with NGS fields...")

  skill_dna <- read_json(file.path(dna_dir, "skill_dna.json"))

  # NGS uses full names; skill_dna uses nflfastR "F.Lastname" format.
  # Build the same short_key lookup.
  ngs_lookup <- recv_ngs_avgs |>
    mutate(
      short_key = paste0(
        substr(sapply(strsplit(player_display_name, " "), `[[`, 1), 1, 1),
        ".",
        sapply(strsplit(player_display_name, " "), function(x) paste(x[-1], collapse = ""))
      )
    )

  patched <- 0
  for (nm in names(skill_dna)) {
    if (startsWith(nm, "_")) next

    match_row <- ngs_lookup |> filter(short_key == nm)
    if (nrow(match_row) == 0) {
      match_row <- ngs_lookup |>
        filter(tolower(short_key) == tolower(nm))
    }

    if (nrow(match_row) > 0) {
      row <- match_row[1, ]
      skill_dna[[nm]]$avg_separation_yds    <- round(row$avg_separation_yds, 3)
      skill_dna[[nm]]$avg_cushion_yds       <- round(row$avg_cushion_yds, 3)
      skill_dna[[nm]]$ngs_weeks_observed    <- as.integer(row$ngs_weeks_observed)
      # top_speed_mph stays null — not available in nflreadr
      patched <- patched + 1
    }
  }

  write_json(skill_dna, file.path(dna_dir, "skill_dna.json"),
             pretty = TRUE, auto_unbox = TRUE, na = "null")
  message("    Skill players patched with NGS: ", patched, " / ", length(skill_dna) - 1)
  invisible(patched)
}

# =============================================================================
# SECTION 6 — AUDIT: REMAINING NULLS
# =============================================================================
#' audit_null_fields
#'
#' @description After patching, scans each DNA file and reports which fields
#'   still contain null values and for what proportion of records. This drives
#'   the decision of which features to include vs. exclude in V.0.1.0 training.
#'
#' @param dna_dir Character. Path to data/dna/.
audit_null_fields <- function(dna_dir) {
  message("\n  --- NULL FIELD AUDIT POST-NGS PATCH ---")

  for (fname in c("qb_dna.json", "skill_dna.json")) {
    dna <- read_json(file.path(dna_dir, fname))
    records <- dna[!startsWith(names(dna), "_")]
    n_total <- length(records)

    # Collect all field names from first non-meta record
    all_fields <- names(records[[1]])

    null_counts <- sapply(all_fields, function(fld) {
      sum(sapply(records, function(r) is.null(r[[fld]]) || is.na(r[[fld]])))
    })

    cat("\n", fname, " (n=", n_total, "):\n", sep = "")
    for (fld in names(null_counts)) {
      n_null <- null_counts[[fld]]
      pct <- round(100 * n_null / n_total, 1)
      flag <- if (pct > 50) " *** HIGH NULL RATE — exclude from training ***" else ""
      cat(sprintf("  %-32s %4d/%d null (%5.1f%%)%s\n",
                  fld, n_null, n_total, pct, flag))
    }
  }
}

# =============================================================================
# MAIN
# =============================================================================
message("\n=== STARTING NGS DNA AUGMENTATION ===\n")

patch_qb_dna(qb_ngs_avgs, DNA_DIR)
patch_skill_dna(recv_ngs_avgs, DNA_DIR)
audit_null_fields(DNA_DIR)

message("\n=== NGS AUGMENTATION COMPLETE ===")
message("Updated files: data/dna/qb_dna.json, data/dna/skill_dna.json")
message("Remaining nulls: top_speed_mph (speed tracking, not in nflreadr NGS)")
message("                 off_pass_block_win_rate (ESPN/PFF proprietary)")
message("                 times_to_pressure_sec   (NGS pass rush, not in nflreadr)")
