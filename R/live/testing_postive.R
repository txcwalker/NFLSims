# R/live/test_posting.R
# Test posting infrastructure in dry-run mode

cat("\n=== Testing Posting Infrastructure (DRY RUN) ===\n\n")

# Set dry-run mode
Sys.setenv(DRY_RUN = "1")

# Load posting functions
source("R/bots/post_targets.R")

cat("1. Testing post_masto (Mastodon)...\n")
result_masto <- post_masto("Test message to Mastodon #nfl")
cat("   Result:", result_masto, "\n")

cat("\n2. Testing post_bsky (Bluesky)...\n")
result_bsky <- post_bsky("Test message to Bluesky #nfl")
cat("   Result:", result_bsky, "\n")

cat("\n3. Testing post_everywhere...\n")
result_all <- post_everywhere("Test message to all platforms! #nfl #analytics")
cat("   Mastodon:", result_all$mastodon, "\n")
cat("   Bluesky:", result_all$bluesky, "\n")
cat("   Any posted:", result_all$any, "\n")

cat("\n=== Summary ===\n")
cat("All posting functions loaded and executed successfully!\n")
cat("(DRY_RUN=1 so messages were printed, not actually posted)\n\n")