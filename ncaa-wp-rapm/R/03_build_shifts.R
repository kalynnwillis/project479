# Build player shifts with PROPER lineup tracking
# This version uses starters from box scores to build credible shifts
# DATA REALITY: ESPN doesn't provide sub events, so we use starters + possession-level shifts

source("R/utils.R")

library(tidyverse)
library(Matrix)
library(gbm) # Needed for gbm.perf()

message("Building player shifts with STARTER-BASED lineup tracking...")

# Load data
model_list <- readRDS("data/interim/wp_models.rds")
box_scores <- readRDS("data/interim/box_scores.rds")

# Load model-specific libraries based on best model
best_model_name <- model_list$best_model_name
if (best_model_name == "XGBoost") {
  library(xgboost)
} else if (best_model_name == "Random Forest") {
  library(ranger)
}

# CRITICAL FIX: Use ALL games for RAPM (don't restrict to WP sample)
# WP model was trained on a sample, but we can apply it to ALL games
pbp_data <- readRDS("data/raw/pbp_clean.rds")

message(paste("Play-by-play data:", nrow(pbp_data), "plays from", n_distinct(pbp_data$game_id), "games"))
message("Note: Using ALL games for RAPM (WP model applied to full dataset)")

# Get the best model
best_model <- switch(best_model_name,
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
    has_ball = if ("possession" %in% names(.)) {
      ifelse(possession == "home", 1, 0)
    } else {
      0.5 # Unknown possession = 50/50
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

# ============================================================================
# CRITICAL FIX: Proper lineup tracking using starters
# ============================================================================

message("\n=== LINEUP RECONSTRUCTION ===")

# Step 1: Identify starters for each game (ALL games in dataset)
# A player is a starter if they have starter==TRUE in box scores
games_in_analysis <- unique(pbp_data$game_id) # Use ALL games, not just WP sample

starters <- box_scores %>%
  filter(starter == TRUE, game_id %in% games_in_analysis) %>%
  select(game_id, team, player_id, player, min, position) %>%
  arrange(game_id, team, desc(min))

# Check starter quality: we need exactly 5 starters per team per game
starters_per_game <- starters %>%
  group_by(game_id, team) %>%
  summarise(n_starters = n(), .groups = "drop")

# Identify "good" games where both teams have exactly 5 starters
good_games <- starters_per_game %>%
  group_by(game_id) %>%
  summarise(
    n_teams = n(),
    min_starters = min(n_starters),
    max_starters = max(n_starters),
    .groups = "drop"
  ) %>%
  filter(n_teams == 2, min_starters == 5, max_starters == 5) %>%
  pull(game_id)

message(paste("Games with clean starter data:", length(good_games), "/", length(games_in_analysis)))
message(paste("Coverage:", round(100 * length(good_games) / length(games_in_analysis), 1), "%"))

# Filter to good games only for RAPM analysis
pbp_for_rapm <- pbp_with_wp %>%
  filter(game_id %in% good_games)

message(paste("Plays for RAPM:", nrow(pbp_for_rapm), "plays"))

# Step 2: Build possession-level shifts
# Sort by ascending time (forward in time)
# Use half instead of period_number since it may not exist in older data
pbp_sorted <- pbp_for_rapm %>%
  arrange(game_id, half, desc(secs_remaining)) %>%
  group_by(game_id, half) %>%
  mutate(
    # Calculate FORWARD time change (next play - current play)
    # We're going forward in time, so next play has LESS time remaining
    # duration = current time - next time = current secs_remaining - next secs_remaining
    # (since secs_remaining decreases as game progresses)
    next_secs_remaining = lead(secs_remaining, default = 0),
    duration = secs_remaining - next_secs_remaining,

    # Fix data leakage: don't use final outcome for next_wp
    # On terminal plays (last play of half), we DON'T have next_wp
    # So we drop these rows later rather than guessing
    next_wp = lead(home_wp, default = NA),

    # Calculate WP change
    wp_change = next_wp - home_wp
  ) %>%
  ungroup() %>%
  # Remove terminal plays where we don't have valid next_wp
  filter(!is.na(next_wp))

# Create HALF-LEVEL stints (lecture-aligned "stints" = between subs)
# Since we don't have sub events, half = one stint
# This is the defensible proxy given data limitations

# First: Calculate leverage for each possession
pbp_with_leverage <- pbp_sorted %>%
  filter(duration > 0, duration < 300) %>%
  mutate(
    is_close = abs(score_diff) <= 10,
    is_clutch = time_remaining_min <= 5,
    leverage = case_when(
      is_clutch & is_close ~ 2.0,
      is_close ~ 1.5,
      abs(score_diff) > 20 ~ 0.5,
      TRUE ~ 1.0
    )
  )

# Then: Aggregate to half-level stints (one row per game-half)
team_shifts <- pbp_with_leverage %>%
  group_by(game_id, half, home, away) %>%
  summarise(
    season = first(season), # NEW: Preserve season
    start_wp = first(home_wp),
    end_wp = last(next_wp),
    wp_change = end_wp - start_wp,
    duration = sum(duration),
    leverage = mean(leverage),
    avg_score_diff = mean(score_diff),
    avg_time_remaining = mean(time_remaining_min),
    .groups = "drop"
  ) %>%
  filter(duration > 60) %>%
  mutate(
    stint_id = paste(game_id, half, sep = "_stint_"),
    home_team = home,
    away_team = away
  ) %>%
  select(
    stint_id, game_id, season, half, home_team, away_team,
    wp_change, duration, leverage,
    avg_score_diff, avg_time_remaining
  )

message(paste("Created", nrow(team_shifts), "HALF-LEVEL stints (lecture-aligned)"))
message(paste("Median stint duration:", round(median(team_shifts$duration)), "seconds"))
message(paste("Mean leverage weight:", round(mean(team_shifts$leverage), 2)))

# Step 4: Prepare starter lists for sparse matrix (NO materialization)
message("\n=== PREPARING STARTER LISTS FOR RAPM ===")

# Get starters for each game (5 per team)
# CRITICAL: Use player_id for joins, keep player name for display
game_starters <- starters %>%
  filter(game_id %in% good_games) %>%
  group_by(game_id, team) %>%
  slice_max(order_by = min, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  select(game_id, team, player_id, player, position, min)

# Create starter lists for each stint (no Cartesian product!)
# CRITICAL: Use player_id for matrix building, keep player name for display
stint_starters <- team_shifts %>%
  left_join(
    game_starters %>% select(game_id, team, player_id, player),
    by = c("game_id", "home_team" = "team"),
    relationship = "many-to-many"
  ) %>%
  rename(home_starter_id = player_id, home_starter = player) %>%
  left_join(
    game_starters %>% select(game_id, team, player_id, player),
    by = c("game_id", "away_team" = "team"),
    relationship = "many-to-many"
  ) %>%
  rename(away_starter_id = player_id, away_starter = player) %>%
  filter(!is.na(home_starter_id), !is.na(away_starter_id)) %>%
  # Keep one row per stint with lists of starter IDs (for matrix) and names (for display)
  group_by(stint_id, game_id, season, half, home_team, away_team, wp_change, duration, leverage) %>%
  summarise(
    home_starters_id = list(unique(home_starter_id)),
    home_starters = list(unique(home_starter)),
    away_starters_id = list(unique(away_starter_id)),
    away_starters = list(unique(away_starter)),
    n_home = n_distinct(home_starter_id),
    n_away = n_distinct(away_starter_id),
    .groups = "drop"
  ) %>%
  # Keep only stints with full 5v5 lineups
  filter(n_home == 5, n_away == 5)

message(paste("Stints with full 5v5 lineups:", nrow(stint_starters)))
message(paste(
  "Total unique players (by ID):",
  n_distinct(c(
    unlist(stint_starters$home_starters_id),
    unlist(stint_starters$away_starters_id)
  ))
))

# Step 5: Create player profiles for context
message("\n=== CREATING PLAYER PROFILES ===")

# CRITICAL: Group by player_id, keep latest player name for display
player_profiles <- box_scores %>%
  filter(game_id %in% good_games) %>%
  group_by(player_id, team) %>%
  summarise(
    player = dplyr::last(player), # Most recent name for this player_id
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
    .groups = "drop"
  )

message(paste("Created profiles for", nrow(player_profiles), "players"))

# Step 6: Save all data (NO shifts_with_players!)
saveRDS(stint_starters, "data/interim/player_shifts.rds") # Stints with starter lists
saveRDS(game_starters, "data/interim/player_games.rds")
saveRDS(player_profiles, "data/interim/player_profiles.rds")

# Save metadata about data quality
data_quality <- list(
  total_games_in_analysis = length(games_in_analysis),
  games_with_clean_lineups = length(good_games),
  coverage_pct = 100 * length(good_games) / length(games_in_analysis),
  total_stints = nrow(stint_starters),
  unique_players = n_distinct(c(
    unlist(stint_starters$home_starters_id),
    unlist(stint_starters$away_starters_id)
  )),
  median_stint_duration_sec = median(stint_starters$duration),
  mean_leverage = mean(stint_starters$leverage),
  pct_high_leverage = 100 * mean(stint_starters$leverage > 1.0)
)

saveRDS(data_quality, "data/interim/lineup_quality.rds")

message("\n===================================")
message("=== STINT BUILDING COMPLETE ===")
message("===================================")
message(paste("Total HALF-LEVEL stints:", nrow(stint_starters)))
message(paste("Unique players:", data_quality$unique_players))
message(paste("Game coverage:", round(data_quality$coverage_pct, 1), "%"))
message(paste("Median stint duration:", round(data_quality$median_stint_duration_sec), "sec"))
message("\nMETHODOLOGY (lecture-aligned):")
message("- Lineup tracking: STARTERS ONLY (no substitution data)")
message("- Stint definition: HALF-LEVEL (defensible proxy for between-sub stints)")
message("- Data leakage: FIXED (next_wp does NOT use realized outcomes)")
message("- Weighting: Duration × Leverage (WAPM from lecture)")
message("- Sparse matrix: Built directly from starter lists (no materialization)")
message("\nLIMITATION: Without substitution data, player RAPM is EXPLORATORY.")
message("           Team-level APM is more reliable for this dataset.")
