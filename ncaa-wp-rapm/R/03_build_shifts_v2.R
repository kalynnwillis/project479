# ESPN doesn't provide sub events, so we use top 7 players by minutes (V2: includes bench)

source("R/utils.R")

library(tidyverse)
library(Matrix)
library(gbm)

model_list <- readRDS("new_data/interim/wp_models.rds")
box_scores <- readRDS("new_data/interim/box_scores.rds")

best_model_name <- model_list$best_model_name
if (best_model_name == "XGBoost") {
    library(xgboost)
} else if (best_model_name == "Random Forest") {
    library(ranger)
}

pbp_data <- readRDS("new_data/raw/pbp_clean.rds")

# Get the best model
best_model <- switch(best_model_name,
    "Logistic" = model_list$logistic,
    "GBM" = model_list$gbm,
    "Random Forest" = model_list$random_forest,
    "XGBoost" = model_list$xgboost
)

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
            0.5
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
    wp <- predict(best_model, pred_data)$predictions[, 2]
} else if (best_model_name == "XGBoost") {
    pred_matrix <- xgb.DMatrix(data = as.matrix(pred_data))
    wp <- predict(best_model, pred_matrix)
}

pbp_with_wp <- pbp_with_features %>%
    mutate(home_wp = wp)

games_in_analysis <- unique(pbp_data$game_id)

# MODIFIED: Use top 7 players by minutes instead of just starters
rotation_players <- box_scores %>%
    filter(game_id %in% games_in_analysis) %>%
    group_by(game_id, team) %>%
    arrange(desc(min)) %>%
    slice_head(n = 7) %>% # Top 7 by minutes (includes key bench players)
    ungroup() %>%
    select(game_id, team, player_id, player, min, position) %>%
    arrange(game_id, team, desc(min))

players_per_game <- rotation_players %>%
    group_by(game_id, team) %>%
    summarise(n_players = n(), .groups = "drop")

# MODIFIED: Identify "good" games where both teams have exactly 7 rotation players
good_games <- players_per_game %>%
    group_by(game_id) %>%
    summarise(
        n_teams = n(),
        min_players = min(n_players),
        max_players = max(n_players),
        .groups = "drop"
    ) %>%
    filter(n_teams == 2, min_players == 7, max_players == 7) %>%
    pull(game_id)


# Filter to good games only for RAPM analysis
pbp_for_rapm <- pbp_with_wp %>%
    filter(game_id %in% good_games)


# Step 2: Build possession-level shifts
# Sort by ascending time (forward in time)
# Use half instead of period_number since it may not exist in older data
pbp_sorted <- pbp_for_rapm %>%
    arrange(game_id, half, desc(secs_remaining)) %>%
    group_by(game_id, half) %>%
    mutate(
        next_secs_remaining = lead(secs_remaining, default = 0),
        duration = secs_remaining - next_secs_remaining,
        next_wp = lead(home_wp, default = NA),

        # Calculate WP change
        wp_change = next_wp - home_wp
    ) %>%
    ungroup() %>%
    # Remove terminal plays where we don't have valid next_wp
    filter(!is.na(next_wp))

# Create HALF-LEVEL stints (lecture-aligned "stints" = between subs)
# Since we don't have sub events, half = one stint

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


# Step 4: Prepare rotation player lists for sparse matrix (NO materialization)
# MODIFIED: Use top 7 instead of top 5
game_rotation <- rotation_players %>%
    filter(game_id %in% good_games) %>%
    group_by(game_id, team) %>%
    slice_max(order_by = min, n = 7, with_ties = FALSE) %>%
    ungroup() %>%
    select(game_id, team, player_id, player, position, min)

stint_players <- team_shifts %>%
    left_join(
        game_rotation %>% select(game_id, team, player_id, player),
        by = c("game_id", "home_team" = "team"),
        relationship = "many-to-many"
    ) %>%
    rename(home_player_id = player_id, home_player = player) %>%
    left_join(
        game_rotation %>% select(game_id, team, player_id, player),
        by = c("game_id", "away_team" = "team"),
        relationship = "many-to-many"
    ) %>%
    rename(away_player_id = player_id, away_player = player) %>%
    filter(!is.na(home_player_id), !is.na(away_player_id)) %>%
    group_by(stint_id, game_id, season, half, home_team, away_team, wp_change, duration, leverage) %>%
    summarise(
        home_starters_id = list(unique(home_player_id)),
        home_starters = list(unique(home_player)),
        away_starters_id = list(unique(away_player_id)),
        away_starters = list(unique(away_player)),
        n_home = n_distinct(home_player_id),
        n_away = n_distinct(away_player_id),
        .groups = "drop"
    ) %>%
    # MODIFIED: Keep only stints with full 7v7 lineups
    filter(n_home == 7, n_away == 7)

message(paste("Stints with full 7v7 lineups:", nrow(stint_players)))
message(paste(
    "Total unique players (by ID):",
    n_distinct(c(
        unlist(stint_players$home_starters_id),
        unlist(stint_players$away_starters_id)
    ))
))

# Step 5: Create player profiles for context

player_profiles <- box_scores %>%
    filter(game_id %in% good_games) %>%
    group_by(player_id, team) %>%
    summarise(
        player = dplyr::last(player),
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


# Step 6: Save all data (MODIFIED: save to new_data/)
saveRDS(stint_players, "new_data/interim/player_shifts.rds")
saveRDS(game_rotation, "new_data/interim/player_games.rds")
saveRDS(player_profiles, "new_data/interim/player_profiles.rds")

data_quality <- list(
    total_games_in_analysis = length(games_in_analysis),
    games_with_clean_lineups = length(good_games),
    coverage_pct = 100 * length(good_games) / length(games_in_analysis),
    total_stints = nrow(stint_players),
    unique_players = n_distinct(c(
        unlist(stint_players$home_starters_id),
        unlist(stint_players$away_starters_id)
    )),
    median_stint_duration_sec = median(stint_players$duration),
    mean_leverage = mean(stint_players$leverage),
    pct_high_leverage = 100 * mean(stint_players$leverage > 1.0)
)

saveRDS(data_quality, "new_data/interim/lineup_quality.rds")

message("\n=== V2 Build Complete: Using Top 7 Players by Minutes ===")
message(paste("Games analyzed:", length(good_games)))
message(paste("Unique players:", data_quality$unique_players))
