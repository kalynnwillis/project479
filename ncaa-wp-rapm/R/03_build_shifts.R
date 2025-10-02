# Build player shifts: Track win probability changes for each lineup combination
# A "shift" is a period where the same 10 players are on the court

source("R/utils.R")

library(tidyverse)
library(Matrix)
library(gbm)  # Needed for gbm.perf()

message("Building player shifts...")

# Load data
model_list <- readRDS("data/interim/wp_models.rds")

# Load model-specific libraries based on best model
best_model_name <- model_list$best_model_name
if (best_model_name == "XGBoost") {
  library(xgboost)
} else if (best_model_name == "Random Forest") {
  library(ranger)
}

# Use only the games that were in the WP model (if sampled)
pbp_data_full <- readRDS("data/raw/pbp_clean.rds")

if (!is.null(model_list$sampled_games)) {
  message(paste("Using sampled games from WP model:", length(model_list$sampled_games), "games"))
  pbp_data <- pbp_data_full %>% filter(game_id %in% model_list$sampled_games)
} else {
  pbp_data <- pbp_data_full
}

message(paste("Play-by-play data:", nrow(pbp_data), "plays from", n_distinct(pbp_data$game_id), "games"))

# Get the best model
best_model <- switch(
  best_model_name,
  "Logistic" = model_list$logistic,
  "GBM" = model_list$gbm,
  "Random Forest" = model_list$random_forest,
  "XGBoost" = model_list$xgboost
)

message(paste("Using", best_model_name, "for win probability calculation"))

# Prepare features for WP prediction
pbp_with_features <- pbp_data %>%
  mutate(
    score_diff = home_score - away_score,
    time_remaining_min = secs_remaining / 60,
    time_elapsed_min = (half - 1) * 20 + (20 - time_remaining_min),
    score_diff_x_time = score_diff * time_remaining_min,
    score_diff_sq = score_diff^2,
    has_ball = if("possession" %in% names(.)) {
      ifelse(possession == "home", 1, 0)
    } else {
      0.5  # Unknown possession = 50/50
    },
    is_first_half = ifelse(half == 1, 1, 0),
    is_second_half = ifelse(half == 2, 1, 0),
    is_clutch = ifelse(time_remaining_min < 5 & abs(score_diff) < 10, 1, 0)
  ) %>%
  filter(!is.na(score_diff), !is.na(time_remaining_min))

# Predict win probability for each play
pred_data <- pbp_with_features %>%
  select(all_of(model_list$predictors))

# Handle missing values
pred_data[is.na(pred_data)] <- 0

# Predict based on model type
if (best_model_name == "Logistic") {
  wp <- predict(best_model, pred_data, type = "response")
} else if (best_model_name == "GBM") {
  best_iter <- gbm.perf(best_model, method = "cv", plot.it = FALSE)
  wp <- predict(best_model, pred_data, n.trees = best_iter, type = "response")
} else if (best_model_name == "Random Forest") {
  # ranger returns predictions in $predictions, column 2 is probability of win
  wp <- predict(best_model, pred_data)$predictions[, 2]
} else if (best_model_name == "XGBoost") {
  pred_matrix <- xgb.DMatrix(data = as.matrix(pred_data))
  wp <- predict(best_model, pred_matrix)
}

pbp_with_wp <- pbp_with_features %>%
  mutate(home_wp = wp)

# Build shifts
# Note: ncaahoopR provides player information in some formats
# We'll create a simplified version using substitution events

# Identify substitution events and create shifts
# For simplicity, we'll use a possession-level approach
# Each possession is treated as a mini-shift

shifts <- pbp_with_wp %>%
  # Filter for plays with player information
  filter(!is.na(home_score), !is.na(away_score)) %>%
  arrange(game_id, half, desc(secs_remaining)) %>%
  group_by(game_id, half) %>%
  mutate(
    play_id = row_number(),
    next_wp = lead(home_wp, default = last(ifelse(home_win == 1, 1, 0))),
    wp_change = next_wp - home_wp,
    duration = secs_remaining - lead(secs_remaining, default = 0)
  ) %>%
  ungroup()

# For RAPM, we need to identify which players were on court
# Since ncaahoopR doesn't always have lineup data, we'll use a proxy:
# Extract players from play description or use team-level analysis

# Create team-level shifts (as proxy when player data unavailable)
team_shifts <- shifts %>%
  filter(duration > 0, duration < 300) %>%  # Filter reasonable durations
  mutate(
    shift_id = paste(game_id, half, play_id, sep = "_"),
    home_team = home,
    away_team = away
  ) %>%
  select(
    shift_id, game_id, home_team, away_team, 
    home_wp, next_wp, wp_change, duration,
    score_diff, time_remaining_min, half
  )

message(paste("Created", nrow(team_shifts), "team-level shifts"))

# Extract unique players from play-by-play descriptions
# This is a heuristic approach - in real implementation, 
# you'd use actual lineup data if available

# Check if player columns exist
has_home_player <- "home_player" %in% names(pbp_with_wp)
has_away_player <- "away_player" %in% names(pbp_with_wp)

if (has_home_player) {
  players_home <- pbp_with_wp %>%
    filter(!is.na(home_player)) %>%
    distinct(game_id, home, home_player) %>%
    rename(team = home, player = home_player)
} else {
  players_home <- tibble()
  message("Warning: home_player column not found in play-by-play data")
}

if (has_away_player) {
  players_away <- pbp_with_wp %>%
    filter(!is.na(away_player)) %>%
    distinct(game_id, away, away_player) %>%
    rename(team = away, player = away_player)
} else {
  players_away <- tibble()
  message("Warning: away_player column not found in play-by-play data")
}

all_players <- bind_rows(players_home, players_away) %>%
  distinct()

# Only filter if player column exists
if (nrow(all_players) > 0 && "player" %in% names(all_players)) {
  all_players <- all_players %>%
    filter(!is.na(player), player != "TEAM")
  message(paste("Identified", n_distinct(all_players$player), "unique players"))
} else {
  all_players <- tibble()
  message("Warning: No player information available in play-by-play data")
}

# For demonstration, assign players to shifts
# In production, you'd use actual lineup data from box scores or lineup tracking

# Get box scores to identify which players played in which games
box_scores_file <- "data/interim/box_scores.rds"

if (file.exists(box_scores_file)) {
  box_scores <- readRDS(box_scores_file)
} else {
  message("Fetching box scores for player-game associations...")
  message("Note: This may take a while and some games may not have box scores available.")
  
  game_ids <- unique(team_shifts$game_id)
  
  box_scores <- map_df(game_ids, function(gid) {
    tryCatch({
      if (which(game_ids == gid) %% 10 == 0) {
        message(paste("Processing box score", which(game_ids == gid), "of", length(game_ids)))
      }
      bs <- get_boxscore(gid)
      if (!is.null(bs) && nrow(bs) > 0) {
        bs %>% mutate(game_id = gid)
      } else {
        tibble()
      }
    }, error = function(e) {
      message(paste("  Error fetching box score for game", gid, "- skipping"))
      tibble()
    })
  })
  
  saveRDS(box_scores, box_scores_file)
  message(paste("Retrieved box scores:", nrow(box_scores), "player-game records"))
}

# Create player-shift associations - ENHANCED with starter data
# Improved heuristic: prioritize starters and high-minute players

if (nrow(box_scores) > 0 && "min" %in% names(box_scores)) {
  
  # ENHANCEMENT 1: Create player skill profiles from shooting/defensive stats
  message("Creating player skill profiles from enhanced box scores...")
  
  player_profiles <- box_scores %>%
    group_by(player, team) %>%
    summarise(
      games_played = n(),
      avg_min = mean(min, na.rm = TRUE),
      is_regular_starter = mean(starter, na.rm = TRUE) >= 0.5,
      
      # Shooting efficiency
      ft_made = sum(ftm, na.rm = TRUE),
      ft_att = sum(fta, na.rm = TRUE),
      ft_pct = ifelse(ft_att > 0, ft_made / ft_att, NA),
      
      fg3_made = sum(fg3m, na.rm = TRUE),
      fg3_att = sum(fg3a, na.rm = TRUE),
      fg3_pct = ifelse(fg3_att > 0, fg3_made / fg3_att, NA),
      
      fg_made = sum(fgm, na.rm = TRUE),
      fg_att = sum(fga, na.rm = TRUE),
      fg_pct = ifelse(fg_att > 0, fg_made / fg_att, NA),
      
      # Volume rates (per 40 minutes)
      fta_per_40 = (ft_att / sum(min, na.rm = TRUE)) * 40,
      fg3a_per_40 = (fg3_att / sum(min, na.rm = TRUE)) * 40,
      
      # Defensive stats (per 40 minutes)
      stl_per_40 = (sum(stl, na.rm = TRUE) / sum(min, na.rm = TRUE)) * 40,
      blk_per_40 = (sum(blk, na.rm = TRUE) / sum(min, na.rm = TRUE)) * 40,
      tov_per_40 = (sum(tov, na.rm = TRUE) / sum(min, na.rm = TRUE)) * 40,
      
      # Overall production
      pts_per_min = sum(pts, na.rm = TRUE) / sum(min, na.rm = TRUE),
      
      # Position (most common)
      position = names(sort(table(position), decreasing = TRUE))[1],
      
      .groups = 'drop'
    )
  
  message(paste("Created profiles for", n_distinct(player_profiles$player), "players"))
  
  # ENHANCEMENT 2: Smarter player-game filtering using starter status
  player_games <- box_scores %>%
    # Improved filter: starters OR players with significant minutes
    filter(starter == TRUE | min >= 15) %>%  
    select(game_id, team, player, min, starter) %>%
    distinct() %>%
    # Add a weight based on minutes and starter status
    mutate(
      lineup_weight = case_when(
        starter == TRUE & min >= 20 ~ 1.0,   # Clear starters with good minutes
        starter == TRUE & min >= 10 ~ 0.9,   # Starters with lower minutes
        starter == FALSE & min >= 25 ~ 0.8,  # Key bench players
        starter == FALSE & min >= 15 ~ 0.6,  # Rotation players
        TRUE ~ 0.3                            # Others
      )
    )
  
  # Assign players to shifts based on game participation
  # This is a simplification - real RAPM would use exact lineup data
  shifts_with_players <- team_shifts %>%
    left_join(
      player_games %>% select(game_id, team, player),
      by = c("game_id", "home_team" = "team"),
      relationship = "many-to-many"
    ) %>%
    rename(home_player = player) %>%
    left_join(
      player_games %>% select(game_id, team, player),
      by = c("game_id", "away_team" = "team"),
      relationship = "many-to-many"
    ) %>%
    rename(away_player = player) %>%
    filter(!is.na(home_player), !is.na(away_player))
  
  message(paste("Created", nrow(shifts_with_players), "player-shift observations"))
  message(paste("Unique players:", n_distinct(c(shifts_with_players$home_player, 
                                                 shifts_with_players$away_player))))
} else {
  message("Warning: No box score data available. Creating dummy player data for demonstration.")
  # Create dummy player data so the RAPM step can still run
  player_games <- team_shifts %>%
    select(game_id, home_team) %>%
    distinct() %>%
    mutate(
      team = home_team,
      player = paste0(home_team, "_Player1"),
      min = 30
    ) %>%
    select(game_id, team, player, min)
  
  shifts_with_players <- team_shifts %>%
    mutate(
      home_player = paste0(home_team, "_Player1"),
      away_player = paste0(away_team, "_Player1")
    )
  
  message("Note: Using team-level dummy data. Results will represent team effects, not individual players.")
}

# Save shifts data
saveRDS(team_shifts, "data/interim/team_shifts.rds")
saveRDS(shifts_with_players, "data/interim/player_shifts.rds")
saveRDS(player_games, "data/interim/player_games.rds")

# Save player profiles for use in RAPM
if (exists("player_profiles")) {
  saveRDS(player_profiles, "data/interim/player_profiles.rds")
  message(paste("Saved player profiles for", nrow(player_profiles), "players"))
}

message("Shift building complete.")
message(paste("Total team shifts:", nrow(team_shifts)))
message(paste("Total player-shift observations:", nrow(shifts_with_players)))
if (exists("player_profiles")) {
  message(paste("Player profiles created:", nrow(player_profiles)))
}