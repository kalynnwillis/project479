# Utility functions for NCAA RAPM analysis

library(tidyverse)

# Function to calculate win probability for a game state
calculate_wp <- function(score_diff, time_remaining, model, ...) {
  # Create prediction data frame
  pred_data <- tibble(
    score_diff = score_diff,
    time_remaining_min = time_remaining,
    ...
  )
  
  # Predict based on model type
  if (inherits(model, "glm")) {
    predict(model, pred_data, type = "response")
  } else if (inherits(model, "gbm")) {
    predict(model, pred_data, type = "response", n.trees = model$n.trees)
  } else {
    stop("Unsupported model type")
  }
}

# Function to get team conference
get_team_conference <- function(teams_data) {
  teams_data %>%
    select(team, conference) %>%
    distinct()
}

# Function to calculate adjusted stats per 100 possessions
per_100_possessions <- function(stat, possessions) {
  (stat / possessions) * 100
}

# Function to create a shift from play-by-play segment
create_shift <- function(pbp_segment) {
  tibble(
    start_time = first(pbp_segment$secs_remaining),
    end_time = last(pbp_segment$secs_remaining),
    duration = start_time - end_time,
    start_wp = first(pbp_segment$home_wp),
    end_wp = last(pbp_segment$home_wp),
    wp_change = end_wp - start_wp,
    home_players = list(unique(pbp_segment$home_player[!is.na(pbp_segment$home_player)])),
    away_players = list(unique(pbp_segment$away_player[!is.na(pbp_segment$away_player)]))
  )
}

# Function to normalize RAPM values
normalize_rapm <- function(rapm_values, scale = 100) {
  (rapm_values - mean(rapm_values, na.rm = TRUE)) / sd(rapm_values, na.rm = TRUE) * scale
}

# Function to get player rankings
rank_players <- function(rapm_table, metric = "ridge_rapm", min_games = 10) {
  rapm_table %>%
    filter(games_played >= min_games) %>%
    arrange(desc(.data[[metric]])) %>%
    mutate(rank = row_number()) %>%
    select(rank, player, everything())
}

# Function to calculate Brier score
brier_score <- function(predicted_prob, actual_outcome) {
  mean((predicted_prob - actual_outcome)^2)
}

# Function to calculate log loss
log_loss <- function(predicted_prob, actual_outcome, eps = 1e-15) {
  predicted_prob <- pmax(pmin(predicted_prob, 1 - eps), eps)
  -mean(actual_outcome * log(predicted_prob) + (1 - actual_outcome) * log(1 - predicted_prob))
}

# Function to create sparse player matrix efficiently
create_player_matrix_sparse <- function(shifts_df, player_col_home, player_col_away, all_players) {
  require(Matrix)
  
  n_rows <- nrow(shifts_df)
  n_cols <- length(all_players)
  
  # Create triplet format (i, j, x) for sparse matrix
  i_indices <- c()
  j_indices <- c()
  values <- c()
  
  for (idx in 1:n_rows) {
    home_player <- shifts_df[[player_col_home]][idx]
    away_player <- shifts_df[[player_col_away]][idx]
    
    if (!is.na(home_player) && home_player %in% all_players) {
      i_indices <- c(i_indices, idx)
      j_indices <- c(j_indices, which(all_players == home_player))
      values <- c(values, 1)
    }
    
    if (!is.na(away_player) && away_player %in% all_players) {
      i_indices <- c(i_indices, idx)
      j_indices <- c(j_indices, which(all_players == away_player))
      values <- c(values, -1)
    }
  }
  
  sparseMatrix(i = i_indices, j = j_indices, x = values, 
               dims = c(n_rows, n_cols))
}

# Function to validate RAPM results
validate_rapm <- function(rapm_results, shifts_data, player_col = "ridge_rapm") {
  # Check for extreme values
  extreme_count <- sum(abs(rapm_results[[player_col]]) > 0.1, na.rm = TRUE)
  
  # Check for missing values
  missing_count <- sum(is.na(rapm_results[[player_col]]))
  
  # Check variance
  rapm_var <- var(rapm_results[[player_col]], na.rm = TRUE)
  
  list(
    extreme_values = extreme_count,
    missing_values = missing_count,
    variance = rapm_var,
    mean = mean(rapm_results[[player_col]], na.rm = TRUE),
    median = median(rapm_results[[player_col]], na.rm = TRUE)
  )
}

# Function to format player names consistently
format_player_name <- function(player_names) {
  player_names %>%
    str_trim() %>%
    str_to_title()
}

# Function to get sample size recommendations
sample_size_check <- function(n_players, n_shifts) {
  min_shifts_per_player <- n_shifts / n_players
  
  if (min_shifts_per_player < 10) {
    warning("Low average shifts per player. Consider increasing sample size or filtering players.")
  }
  
  list(
    n_players = n_players,
    n_shifts = n_shifts,
    avg_shifts_per_player = min_shifts_per_player,
    sufficient = min_shifts_per_player >= 10
  )
}

message("Utility functions loaded.")