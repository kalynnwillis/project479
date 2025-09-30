# Fit RAPM using regularized regression (Ridge/Lasso)
# Regularized Adjusted Plus-Minus on Win Probability scale

source("R/utils.R")

library(tidyverse)
library(glmnet)
library(Matrix)

message("Fitting RAPM model...")

# Load shifts data
shifts <- readRDS("data/interim/player_shifts.rds")
player_games <- readRDS("data/interim/player_games.rds")

message(paste("Loaded", nrow(shifts), "shift observations"))

# Get unique players
all_players <- unique(c(shifts$home_player, shifts$away_player))
all_teams <- unique(c(shifts$home_team, shifts$away_team))

message(paste("Unique players:", length(all_players)))
message(paste("Unique teams:", length(all_teams)))

# Create design matrix for RAPM
# Each row is a shift, each column is a player
# Value is +1 if player is on home team, -1 if on away team, 0 otherwise

# For computational efficiency with large datasets, use sparse matrices
create_rapm_matrix <- function(shifts_data, players) {
  n_shifts <- nrow(shifts_data)
  n_players <- length(players)
  
  message("Creating sparse design matrix...")
  message(paste("Dimensions:", n_shifts, "shifts x", n_players, "players"))
  
  # Initialize sparse matrix
  X <- Matrix(0, nrow = n_shifts, ncol = n_players, sparse = TRUE)
  colnames(X) <- players
  
  # Fill in player effects
  for (i in 1:n_shifts) {
    if (i %% 10000 == 0) {
      message(paste("Processing shift", i, "of", n_shifts))
    }
    
    home_player <- shifts_data$home_player[i]
    away_player <- shifts_data$away_player[i]
    
    if (!is.na(home_player) && home_player %in% players) {
      player_idx <- which(players == home_player)
      X[i, player_idx] <- 1
    }
    
    if (!is.na(away_player) && away_player %in% players) {
      player_idx <- which(players == away_player)
      X[i, player_idx] <- -1
    }
  }
  
  return(X)
}

X <- create_rapm_matrix(shifts, all_players)

# Response variable: win probability change
y <- shifts$wp_change

# Remove rows/columns with no variation
valid_shifts <- which(rowSums(X != 0) > 0)
valid_players <- which(colSums(X != 0) > 0)

X_valid <- X[valid_shifts, valid_players]
y_valid <- y[valid_shifts]

message(paste("After filtering:", nrow(X_valid), "shifts and", ncol(X_valid), "players"))

# Add team effects to control for team strength
# Create team indicators
teams_matrix <- Matrix(0, nrow = nrow(shifts), ncol = length(all_teams), sparse = TRUE)
colnames(teams_matrix) <- all_teams

for (i in 1:nrow(shifts)) {
  if (i %% 10000 == 0) {
    message(paste("Adding team effects:", i, "of", nrow(shifts)))
  }
  
  home_team <- shifts$home_team[i]
  away_team <- shifts$away_team[i]
  
  if (!is.na(home_team) && home_team %in% all_teams) {
    team_idx <- which(all_teams == home_team)
    teams_matrix[i, team_idx] <- 1
  }
  
  if (!is.na(away_team) && away_team %in% all_teams) {
    team_idx <- which(all_teams == away_team)
    teams_matrix[i, team_idx] <- -1
  }
}

# Combine player and team effects
X_combined <- cbind(X, teams_matrix)
X_combined_valid <- X_combined[valid_shifts, ]

# Fit Ridge Regression (L2 regularization)
message("Fitting Ridge regression model...")
set.seed(479)

# Use cross-validation to select lambda
cv_ridge <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  alpha = 0,  # Ridge regression
  nfolds = 10,
  standardize = TRUE,
  parallel = FALSE
)

# Fit model with best lambda
ridge_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  alpha = 0,
  lambda = cv_ridge$lambda.min,
  standardize = TRUE
)

ridge_coefs <- coef(ridge_model)
ridge_player_effects <- ridge_coefs[2:(length(all_players) + 1)]
names(ridge_player_effects) <- colnames(X_valid)

# Fit Lasso Regression (L1 regularization)
message("Fitting Lasso regression model...")
cv_lasso <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  alpha = 1,  # Lasso regression
  nfolds = 10,
  standardize = TRUE,
  parallel = FALSE
)

lasso_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  alpha = 1,
  lambda = cv_lasso$lambda.min,
  standardize = TRUE
)

lasso_coefs <- coef(lasso_model)
lasso_player_effects <- lasso_coefs[2:(length(all_players) + 1)]
names(lasso_player_effects) <- colnames(X_valid)

# Fit Elastic Net (combination)
message("Fitting Elastic Net model...")
cv_elastic <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  alpha = 0.5,  # Elastic net
  nfolds = 10,
  standardize = TRUE,
  parallel = FALSE
)

elastic_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  alpha = 0.5,
  lambda = cv_elastic$lambda.min,
  standardize = TRUE
)

elastic_coefs <- coef(elastic_model)
elastic_player_effects <- elastic_coefs[2:(length(all_players) + 1)]
names(elastic_player_effects) <- colnames(X_valid)

# Compile RAPM results
rapm_results <- tibble(
  player = colnames(X_valid),
  ridge_rapm = as.numeric(ridge_player_effects),
  lasso_rapm = as.numeric(lasso_player_effects),
  elastic_rapm = as.numeric(elastic_player_effects)
) %>%
  arrange(desc(ridge_rapm))

# Add player statistics from box scores
player_stats <- player_games %>%
  group_by(player) %>%
  summarise(
    games_played = n(),
    total_minutes = sum(min, na.rm = TRUE),
    avg_minutes = mean(min, na.rm = TRUE)
  )

rapm_results <- rapm_results %>%
  left_join(player_stats, by = "player")

# Compute percentile ranks
rapm_results <- rapm_results %>%
  mutate(
    ridge_percentile = percent_rank(ridge_rapm) * 100,
    lasso_percentile = percent_rank(lasso_rapm) * 100,
    elastic_percentile = percent_rank(elastic_rapm) * 100
  )

# Model evaluation metrics
message("\n=== Model Evaluation ===")
message(paste("Ridge lambda:", round(cv_ridge$lambda.min, 6)))
message(paste("Lasso lambda:", round(cv_lasso$lambda.min, 6)))
message(paste("Elastic lambda:", round(cv_elastic$lambda.min, 6)))

message(paste("\nRidge MSE:", round(min(cv_ridge$cvm), 6)))
message(paste("Lasso MSE:", round(min(cv_lasso$cvm), 6)))
message(paste("Elastic MSE:", round(min(cv_elastic$cvm), 6)))

# Save results
rapm_output <- list(
  rapm_table = rapm_results,
  ridge_model = ridge_model,
  lasso_model = lasso_model,
  elastic_model = elastic_model,
  cv_ridge = cv_ridge,
  cv_lasso = cv_lasso,
  cv_elastic = cv_elastic,
  design_matrix = X_combined_valid,
  response = y_valid
)

saveRDS(rapm_output, "data/processed/rapm_results.rds")
write_csv(rapm_results, "tables/rapm_rankings.csv")

# Print top players
message("\n=== Top 20 Players (Ridge RAPM) ===")
print(rapm_results %>% 
        filter(!is.na(games_played)) %>%
        arrange(desc(ridge_rapm)) %>% 
        select(player, ridge_rapm, games_played, avg_minutes) %>%
        head(20))

message("\nRAPM fitting complete.")