# IMPROVED Data acquisition using ncaahoopR
# Focuses on major conference teams with better data availability

source("R/utils.R")

library(tidyverse)
library(ncaahoopR)

message("==============================================")
message("NCAA Basketball Data Acquisition - IMPROVED")
message("==============================================\n")

# Configuration
SEASON <- "2021-22"  # 2021-22 has better data availability than 2023-24
LIMIT_GAMES <- NULL  # NULL for all games, or set number for testing

# Focus on major conference teams (better data coverage)
major_teams <- c(
  # ACC
  "Duke", "North Carolina", "Virginia", "Miami FL", "Wake Forest",
  # Big Ten  
  "Michigan St.", "Illinois", "Purdue", "Indiana", "Ohio St.",
  # Big 12
  "Kansas", "Baylor", "Texas", "Iowa St.", "Texas Tech",
  # Big East
  "Villanova", "UConn", "Creighton", "Providence", "Xavier",
  # SEC
  "Kentucky", "Tennessee", "Alabama", "Arkansas", "Auburn",
  # West Coast / Pac-12
  "UCLA", "Arizona", "Gonzaga", "Oregon", "USC"
)

message(paste("Season:", SEASON))
message(paste("Teams:", length(major_teams)))
message(paste("Game limit:", ifelse(is.null(LIMIT_GAMES), "ALL", LIMIT_GAMES)))

# Step 1: Get schedules
message("\n=== Step 1: Getting schedules ===")

schedule_file <- paste0("data/raw/schedules_", SEASON, ".rds")

if (file.exists(schedule_file)) {
  message("Loading cached schedules...")
  all_schedules <- readRDS(schedule_file)
} else {
  message("Fetching schedules from ESPN (this may take a few minutes)...")
  
  all_schedules <- map_df(major_teams, function(team) {
    message(paste("  -", team))
    tryCatch({
      get_schedule(team, season = SEASON) %>%
        mutate(team_queried = team)
    }, error = function(e) {
      message(paste("    Error:", e$message))
      tibble()
    })
  })
  
  saveRDS(all_schedules, schedule_file)
}

message(paste("Total schedule entries:", nrow(all_schedules)))

# Get unique game IDs (exclude non-games)
game_ids <- all_schedules %>%
  filter(!is.na(game_id), game_id != "") %>%
  distinct(game_id) %>%
  pull(game_id)

message(paste("Unique games:", length(game_ids)))

# Apply limit if specified
if (!is.null(LIMIT_GAMES)) {
  game_ids <- head(game_ids, LIMIT_GAMES)
  message(paste("Limited to:", length(game_ids), "games"))
}

# Step 2: Download play-by-play
message("\n=== Step 2: Downloading play-by-play ===")

pbp_file <- paste0("data/raw/pbp_", SEASON, "_raw.rds")

if (file.exists(pbp_file)) {
  message("Loading cached play-by-play data...")
  pbp_data <- readRDS(pbp_file)
} else {
  message(paste("Downloading", length(game_ids), "games..."))
  message("Note: This will take time. Expect ~30-60 minutes for 1000 games.")
  
  pbp_data <- map_df(1:length(game_ids), function(i) {
    gid <- game_ids[i]
    
    # Progress updates
    if (i %% 50 == 0) {
      message(paste("  Progress:", i, "/", length(game_ids), 
                    sprintf("(%.1f%%)", 100*i/length(game_ids))))
    }
    
    tryCatch({
      get_pbp_game(gid) %>%
        mutate(game_id = gid)
    }, error = function(e) {
      tibble()  # Skip failed games silently
    })
  })
  
  saveRDS(pbp_data, pbp_file)
}

successful_games <- n_distinct(pbp_data$game_id)
success_rate <- round(100 * successful_games / length(game_ids), 1)

message(paste("Downloaded:", nrow(pbp_data), "plays"))
message(paste("Successful games:", successful_games, "/", length(game_ids),
              sprintf("(%.1f%%)", success_rate)))

# Step 3: Clean data
message("\n=== Step 3: Cleaning data ===")

pbp_clean <- pbp_data %>%
  filter(!is.na(home_score), !is.na(away_score), !is.na(secs_remaining)) %>%
  mutate(
    score_diff = home_score - away_score,
    half = as.numeric(half)
  ) %>%
  group_by(game_id) %>%
  arrange(desc(secs_remaining)) %>%
  mutate(
    home_win = ifelse(last(score_diff) > 0, 1, 0)
  ) %>%
  ungroup()

message(paste("Cleaned:", nrow(pbp_clean), "plays from", 
              n_distinct(pbp_clean$game_id), "games"))

saveRDS(pbp_clean, "data/raw/pbp_clean.rds")

# Step 4: Download box scores
message("\n=== Step 4: Downloading box scores ===")

box_file <- paste0("data/interim/boxscores_", SEASON, ".rds")

if (file.exists(box_file)) {
  message("Loading cached box scores...")
  box_scores <- readRDS(box_file)
} else {
  message("Fetching box scores for player data...")
  
  # Use games we successfully got PBP for
  successful_game_ids <- unique(pbp_clean$game_id)
  
  box_scores <- map_df(1:length(successful_game_ids), function(i) {
    gid <- successful_game_ids[i]
    
    if (i %% 100 == 0) {
      message(paste("  Progress:", i, "/", length(successful_game_ids)))
    }
    
    tryCatch({
      get_boxscore(gid) %>%
        mutate(
          game_id = gid,
          min = as.numeric(min)
        )
    }, error = function(e) {
      tibble()
    })
  })
  
  saveRDS(box_scores, box_file)
}

message(paste("Box score records:", nrow(box_scores)))
message(paste("Unique players:", n_distinct(box_scores$player)))

saveRDS(box_scores, "data/interim/box_scores.rds")

# Step 5: Summary statistics
message("\n==============================================")
message("=== DATA ACQUISITION COMPLETE ===")
message("==============================================")

summary_stats <- list(
  season = SEASON,
  teams = length(major_teams),
  games_attempted = length(game_ids),
  games_successful = n_distinct(pbp_clean$game_id),
  success_rate = success_rate,
  total_plays = nrow(pbp_clean),
  total_boxscore_records = nrow(box_scores),
  unique_players = n_distinct(box_scores$player),
  unique_teams = n_distinct(c(pbp_clean$home, pbp_clean$away))
)

# Print summary
cat("\nSUMMARY:\n")
cat(paste("  Season:", summary_stats$season, "\n"))
cat(paste("  Teams queried:", summary_stats$teams, "\n"))
cat(paste("  Games attempted:", summary_stats$games_attempted, "\n"))
cat(paste("  Games successful:", summary_stats$games_successful, 
          sprintf("(%.1f%%)\n", summary_stats$success_rate)))
cat(paste("  Total plays:", format(summary_stats$total_plays, big.mark = ","), "\n"))
cat(paste("  Box score records:", format(summary_stats$total_boxscore_records, big.mark = ","), "\n"))
cat(paste("  Unique players:", summary_stats$unique_players, "\n"))
cat(paste("  Unique teams:", summary_stats$unique_teams, "\n"))

cat("\nFILES SAVED:\n")
cat("  - data/raw/pbp_clean.rds\n")
cat("  - data/interim/box_scores.rds\n")

cat("\nNEXT STEPS:\n")
cat("  Run: source('R/02_build_wp_model.R')\n")
cat("  Or: source('run_analysis.R') to run full pipeline\n")

# Save summary
saveRDS(summary_stats, "data/raw/data_summary.rds")

message("\nDone!")
