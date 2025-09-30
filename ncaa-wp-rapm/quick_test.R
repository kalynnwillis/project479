# Quick test script for NCAA RAPM analysis
# This runs a simplified version with limited data for testing purposes
# Run this first to verify everything works before processing full season data

cat("=====================================\n")
cat("NCAA Basketball RAPM - Quick Test\n")
cat("=====================================\n\n")

# Set CRAN mirror first
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Set working directory
if (basename(getwd()) != "ncaa-wp-rapm") {
  if (dir.exists("ncaa-wp-rapm")) {
    setwd("ncaa-wp-rapm")
  }
}

# Step 0: Setup
cat("Installing required packages...\n")
source("R/00_setup.R")

# For quick testing, we'll use a very small sample
cat("\n\nNOTE: This is a QUICK TEST with limited data.\n")
cat("For full analysis, use run_analysis.R\n\n")

# Manually set a small limit in the data acquisition script
# by creating a test version

library(tidyverse)
library(ncaahoopR)

cat("Fetching small sample of games for testing...\n")

# Get a few teams
test_teams <- c("Duke", "North Carolina", "Gonzaga", "Kansas", "UCLA")

cat(paste("Test teams:", paste(test_teams, collapse = ", "), "\n\n"))

# Get schedules for test teams
all_games <- map_df(test_teams, function(team) {
  tryCatch({
    cat(paste("Fetching schedule for", team, "...\n"))
    get_schedule(team, season = "2023-24") %>%
      mutate(team_queried = team)
  }, error = function(e) {
    cat(paste("Error fetching", team, ":", e$message, "\n"))
    tibble()
  })
})

# Get first 50 unique games
game_ids <- all_games %>%
  filter(!is.na(game_id)) %>%
  distinct(game_id) %>%
  head(50) %>%
  pull(game_id)

cat(paste("\nDownloading play-by-play for", length(game_ids), "games...\n"))

# Download play-by-play
pbp_data <- map_df(game_ids, function(gid) {
  tryCatch({
    if (which(game_ids == gid) %% 10 == 0) {
      cat(paste("Game", which(game_ids == gid), "of", length(game_ids), "\n"))
    }
    get_pbp_game(gid) %>%
      mutate(game_id = gid)
  }, error = function(e) {
    tibble()
  })
})

# Clean data
pbp_clean <- pbp_data %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  mutate(
    score_diff = home_score - away_score,
    time_remaining_sec = secs_remaining,
    half = ifelse(half == 1, 1, 2)
  ) %>%
  group_by(game_id) %>%
  mutate(home_win = ifelse(last(score_diff) > 0, 1, 0)) %>%
  ungroup()

# Save test data
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
saveRDS(pbp_clean, "data/raw/pbp_clean.rds")

cat(paste("\n✓ Downloaded", nrow(pbp_clean), "plays from", 
          n_distinct(pbp_clean$game_id), "games\n\n"))

# Run remaining steps
cat("Running win probability model...\n")
source("R/02_build_wp_model.R")

cat("\nRunning shift creation...\n")
source("R/03_build_shifts.R")

cat("\nFitting RAPM...\n")
source("R/04_fit_rapm.R")

cat("\nGenerating plots...\n")
source("R/05_eval_plots.R")

cat("\n=====================================\n")
cat("Quick test complete!\n")
cat("=====================================\n\n")

cat("Check outputs in:\n")
cat("- tables/rapm_rankings.csv\n")
cat("- figs/*.png\n\n")

cat("For full analysis with all games, edit 01_get_data.R\n")
cat("to set LIMIT_GAMES = NULL, then run run_analysis.R\n")
