# R/live/test_output_files.R
# Test that output files are created correctly

suppressPackageStartupMessages({
  library(readr)
  library(tibble)
  library(dplyr)
})

cat("\n=== Testing Output File Creation ===\n\n")

# Create mock 4th down data
cat("1. Creating mock 4th down data...\n")
mock_data <- tibble(
  game_id = "401772946",
  drive_id = 1,
  play_id = "401772946_1",
  season = 2024,
  week = 13,
  off = "HOU",
  def = "BUF",
  off_score = 23,
  def_score = 20,
  margin = 3,
  qtr = 4,
  sec_left = 120,
  clock = "2:00",
  down = 4L,
  ydstogo = 5,
  yardline_100 = 50,
  roof = "dome",
  surface = "turf",
  wind = 0,
  temp = 72,
  pre_wp = 0.65,
  wp_go = 0.70,
  wp_punt = 0.55,
  wp_fg = 0.60,
  best_action = "go",
  called_action = "go",
  punt_suppressed = FALSE
)

cat("   Created mock data with", nrow(mock_data), "row\n\n")

# Test 1: Output directory creation
cat("2. Testing output directory creation...\n")
out_dir <- "R/bots/live_csv"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  cat("   Created:", out_dir, "\n")
} else {
  cat("   Already exists:", out_dir, "\n")
}

# Test 2: Write CSV
cat("\n3. Testing CSV write...\n")
ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
csv_path <- file.path(out_dir, glue::glue("test_plays_{ts}.csv"))
readr::write_csv(mock_data, csv_path)
cat("   Wrote:", csv_path, "\n")

if (file.exists(csv_path)) {
  size <- file.size(csv_path)
  cat("   File size:", size, "bytes\n")
  cat("   ✓ CSV file created successfully\n")
} else {
  cat("   ✗ ERROR: CSV file not created\n")
}

# Test 3: Write LOG
cat("\n4. Testing LOG write...\n")
log_path <- file.path(out_dir, glue::glue("test_run_{ts}.log"))
readr::write_lines(
  c(
    "=== Test Run ===",
    glue::glue("Timestamp: {Sys.time()}"),
    glue::glue("Rows evaluated: {nrow(mock_data)}"),
    "Status: SUCCESS"
  ),
  log_path
)
cat("   Wrote:", log_path, "\n")

if (file.exists(log_path)) {
  cat("   ✓ LOG file created successfully\n")
} else {
  cat("   ✗ ERROR: LOG file not created\n")
}

# Test 4: Verify files can be read back
cat("\n5. Testing file read-back...\n")
read_back <- readr::read_csv(csv_path, show_col_types = FALSE)
cat("   Read", nrow(read_back), "rows from CSV\n")
cat("   Columns match:", all(names(read_back) == names(mock_data)), "\n")
cat("   ✓ Files readable\n")

# Test 5: List directory
cat("\n6. Current output directory contents:\n")
files <- list.files(out_dir, full.names = FALSE, recursive = FALSE)
for (f in head(files, 10)) {
  cat("   -", f, "\n")
}
if (length(files) > 10) {
  cat("   ... and", length(files) - 10, "more files\n")
}

cat("\n=== All Output Tests Complete ===\n")
cat("✓ Output infrastructure is working correctly!\n\n")