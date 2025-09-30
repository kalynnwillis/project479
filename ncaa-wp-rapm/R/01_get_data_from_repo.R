# Load pre-scraped NCAA data from ncaahoopR_data repository
# This is MUCH more reliable than real-time scraping from ESPN
# Source: https://github.com/lbenz730/ncaahoopR_data

source("R/utils.R")

library(tidyverse)

message("Loading NCAA basketball data from ncaahoopR_data repository...")
message("Source: https://github.com/lbenz730/ncaahoopR_data\n")

# Configuration
SEASON <- "2022-23"  # Seasons available: 2002-03 through 2024-25
LIMIT_GAMES <- 500   # NULL for all games, or set a number for testing

# GitHub raw content base URL
base_url <- "https://raw.githubusercontent.com/lbenz730/ncaahoopR_data/master"

message(paste("Season:", SEASON))
message(paste("Limit:", ifelse(is.null(LIMIT_GAMES), "ALL games", paste(LIMIT_GAMES, "games"))))

# The repository structure according to load_data.R example:
# - PBP logs are in: season/pbp_logs/*.csv (one file per game)
# - Box scores are in: season/team/game_id.csv
# - Schedules are in: season/schedule/team_schedule.csv

# Step 1: Get a master schedule by loading one team's schedule to get game list
message("\nStep 1: Loading schedule to find available games...")

# Pick a major team to get a good sample of games
test_team <- "Duke"
schedule_url <- paste0(base_url, "/", SEASON, "/schedule/", test_team, "_schedule.csv")

duke_schedule <- tryCatch({
  read_csv(schedule_url, show_col_types = FALSE)
}, error = function(e) {
  message("Error loading schedule. Season may not be available.")
  message("Available seasons: 2002-03 through 2024-25")
  message("Seasons with best PBP coverage: 2016-17 onwards")
  stop(e)
})

message(paste("Found", nrow(duke_schedule), "games on Duke's schedule"))

# Get game IDs to download
game_ids <- duke_schedule$game_id

if (!is.null(LIMIT_GAMES)) {
  game_ids <- head(game_ids, LIMIT_GAMES)
  message(paste("Limiting to", length(game_ids), "games for testing"))
}

# Step 2: Download PBP data
# Files are at: season/pbp_logs/game_id.csv
message("\nStep 2: Downloading play-by-play data...")

pbp_data <- map_df(1:length(game_ids), function(i) {
  gid <- game_ids[i]
  
  if (i %% 50 == 0) {
    message(paste("  Downloaded", i, "of", length(game_ids), "games"))
  }
  
  # URL pattern: SEASON/pbp_logs/game_id.csv
  pbp_url <- paste0(base_url, "/", SEASON, "/pbp_logs/", gid, ".csv")
  
  tryCatch({
    read_csv(pbp_url, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
      mutate(game_id = as.character(gid))
  }, error = function(e) {
    # File doesn't exist - that's OK, skip it
    tibble()
  })
})

message(paste("Downloaded", nrow(pbp_data), "total plays from", 
              n_distinct(pbp_data$game_id), "games"))

# Step 3: Clean and process
message("\nStep 3: Cleaning and processing data...")

pbp_clean <- pbp_data %>%
  mutate(
    # Convert to numeric
    home_score = as.numeric(home_score),
    away_score = as.numeric(away_score),
    secs_remaining = as.numeric(secs_remaining),
    half = as.numeric(half),
    
    # Calculate features
    score_diff = home_score - away_score
  ) %>%
  filter(!is.na(home_score), !is.na(away_score), !is.na(secs_remaining)) %>%
  # Add game outcome
  group_by(game_id) %>%
  arrange(desc(secs_remaining)) %>%
  mutate(
    home_win = ifelse(last(score_diff) > 0, 1, 0)
  ) %>%
  ungroup()

message(paste("Cleaned:", nrow(pbp_clean), "plays from", 
              n_distinct(pbp_clean$game_id), "games"))

# Save
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
saveRDS(pbp_clean, "data/raw/pbp_clean.rds")
message("Saved to: data/raw/pbp_clean.rds")

# Step 4: Download box scores for player information
message("\nStep 4: Downloading box scores for player data...")

# Get unique teams from the games we have
teams_in_pbp <- unique(c(pbp_clean$home, pbp_clean$away))
message(paste("Found", length(teams_in_pbp), "unique teams in play-by-play data"))

# Limit teams for faster processing
TEAMS_LIMIT <- 50
if (length(teams_in_pbp) > TEAMS_LIMIT) {
  message(paste("Limiting to first", TEAMS_LIMIT, "teams for testing"))
  teams_in_pbp <- head(teams_in_pbp, TEAMS_LIMIT)
}

box_scores <- map_df(teams_in_pbp, function(team) {
  if (which(teams_in_pbp == team) %% 10 == 0) {
    message(paste("  Processing team", which(teams_in_pbp == team), "of", length(teams_in_pbp)))
  }
  
  team_encoded <- URLencode(team, reserved = TRUE)
  
  # Get team schedule to find their game IDs
  schedule_url <- paste0(base_url, "/", SEASON, "/schedule/", team_encoded, "_schedule.csv")
  
  team_schedule <- tryCatch({
    read_csv(schedule_url, show_col_types = FALSE)
  }, error = function(e) tibble())
  
  if (nrow(team_schedule) == 0) return(tibble())
  
  # Download box scores for this team
  # URL pattern: SEASON/TEAM/game_id.csv
  team_boxes <- map_df(team_schedule$game_id, function(gid) {
    box_url <- paste0(base_url, "/", SEASON, "/", team_encoded, "/", gid, ".csv")
    
    tryCatch({
      read_csv(box_url, show_col_types = FALSE) %>%
        mutate(
          game_id = as.character(gid),
          team = team,
          min = as.numeric(min)
        )
    }, error = function(e) tibble())
  })
  
  return(team_boxes)
})

message(paste("Downloaded", nrow(box_scores), "box score records"))

# Save box scores
dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)
saveRDS(box_scores, "data/interim/box_scores.rds")
message("Saved to: data/interim/box_scores.rds")

# Summary
message("\n========================================")
message("=== Data Acquisition Complete ===")
message("========================================")
message(paste("Season:", SEASON))
message(paste("Games:", n_distinct(pbp_clean$game_id)))
message(paste("Plays:", nrow(pbp_clean)))
message(paste("Box score records:", nrow(box_scores)))
message(paste("Unique players:", n_distinct(box_scores$player)))
message("\nData files saved:")
message("  - data/raw/pbp_clean.rds")
message("  - data/interim/box_scores.rds")
message("\nReady to run win probability model!")
message("Next: source('R/02_build_wp_model.R')")