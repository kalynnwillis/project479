# Data acquisition: Fetch NCAA basketball play-by-play data
source("R/utils.R")

library(tidyverse)
library(ncaahoopR)

message("Downloading NCAA basketball data...")

# Set season (change as needed)
SEASON <- "2023-24"

# Get all Division I teams
teams <- get_teams()
d1_teams <- teams %>% 
  filter(!is.na(conference)) %>%
  pull(team)

message(paste("Found", length(d1_teams), "D1 teams"))

# Get schedule for all teams
# Note: This can take a while, so we'll cache it
schedule_file <- "data/raw/schedule.rds"

if (file.exists(schedule_file)) {
  message("Loading cached schedule...")
  all_games <- readRDS(schedule_file)
} else {
  message("Fetching game schedules (this may take a while)...")
  
  all_games <- map_df(d1_teams, function(team) {
    tryCatch({
      message(paste("Fetching schedule for", team))
      get_schedule(team, season = SEASON) %>%
        mutate(team_queried = team)
    }, error = function(e) {
      message(paste("Error fetching", team, ":", e$message))
      tibble()
    })
  })
  
  saveRDS(all_games, schedule_file)
  message(paste("Saved schedule:", nrow(all_games), "game records"))
}

# Get unique game IDs (each game appears twice in schedule)
game_ids <- all_games %>%
  filter(!is.na(game_id)) %>%
  distinct(game_id) %>%
  pull(game_id)

message(paste("Found", length(game_ids), "unique games"))

# Download play-by-play data
# Note: This is time-intensive, so we'll do a subset or use cached data
pbp_file <- "data/raw/pbp_data.rds"

if (file.exists(pbp_file)) {
  message("Loading cached play-by-play data...")
  pbp_data <- readRDS(pbp_file)
} else {
  message("Downloading play-by-play data...")
  message("Note: For full season, this can take hours. Consider using a subset for testing.")
  
  # For development/testing, limit to first N games
  # For production, remove the head() call
  LIMIT_GAMES <- 500  # Set to NULL for all games
  
  if (!is.null(LIMIT_GAMES)) {
    message(paste("Limiting to first", LIMIT_GAMES, "games for testing"))
    game_ids <- head(game_ids, LIMIT_GAMES)
  }
  
  pbp_data <- map_df(game_ids, function(gid) {
    tryCatch({
      if (which(game_ids == gid) %% 50 == 0) {
        message(paste("Processing game", which(game_ids == gid), "of", length(game_ids)))
      }
      get_pbp_game(gid) %>%
        mutate(game_id = gid)
    }, error = function(e) {
      message(paste("Error fetching game", gid, ":", e$message))
      tibble()
    })
  })
  
  saveRDS(pbp_data, pbp_file)
  message(paste("Saved play-by-play data:", nrow(pbp_data), "plays"))
}

# Clean and prepare data
pbp_clean <- pbp_data %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  mutate(
    # Calculate game state variables
    score_diff = home_score - away_score,  # From home team perspective
    time_remaining_sec = secs_remaining,
    half = ifelse(half == 1, 1, 2),
    # Game outcome (home team wins)
    home_win = NA  # Will be filled in later
  )

# Add game outcomes
pbp_clean <- pbp_clean %>%
  group_by(game_id) %>%
  mutate(
    home_win = ifelse(last(score_diff) > 0, 1, 0)
  ) %>%
  ungroup()

# Save cleaned data
saveRDS(pbp_clean, "data/raw/pbp_clean.rds")

message(paste("Data acquisition complete:", 
              nrow(pbp_clean), "plays from",
              n_distinct(pbp_clean$game_id), "games"))