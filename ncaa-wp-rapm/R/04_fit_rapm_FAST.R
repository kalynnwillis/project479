# Fit RAPM using regularized regression - FAST OPTIMIZED VERSION
# Regularized Adjusted Plus-Minus on Win Probability scale

source("R/utils.R")

library(tidyverse)
library(glmnet)
library(Matrix)

message("Fitting RAPM model (FAST VERSION)...")

# Load shifts data
shifts <- readRDS("data/interim/player_shifts.rds")
player_games <- readRDS("data/interim/player_games.rds")

message(paste("Loaded", nrow(shifts), "shift observations"))

# OPTIMIZATION 1: Sample shifts if dataset is too large
MAX_SHIFTS <- 500000  # Limit for speed
if (nrow(shifts) > MAX_SHIFTS) {
  message(paste("Sampling", MAX_SHIFTS, "shifts from", nrow(shifts), "for faster computation..."))
  set.seed(479)
  shifts <- shifts %>% slice_sample(n = MAX_SHIFTS)
  message(paste("Using", nrow(shifts), "sampled shifts"))
}

# Get unique players
all_players <- unique(c(shifts$home_player, shifts$away_player))
all_teams <- unique(c(shifts$home_team, shifts$away_team))

message(paste("Unique players:", length(all_players)))
message(paste("Unique teams:", length(all_teams)))

# OPTIMIZATION 2: Vectorized sparse matrix creation
create_rapm_matrix_fast <- function(shifts_data, players) {
  n_shifts <- nrow(shifts_data)
  n_players <- length(players)
  
  message("Creating sparse design matrix (vectorized)...")
  message(paste("Dimensions:", n_shifts, "shifts x", n_players, "players"))
  
  # Create player index lookup for fast matching
  player_lookup <- setNames(1:n_players, players)
  
  # Find indices for home and away players
  home_indices <- match(shifts_data$home_player, players)
  away_indices <- match(shifts_data$away_player, players)
  
  # Remove NAs
  valid_home <- !is.na(home_indices)
  valid_away <- !is.na(away_indices)
  
  # Build sparse matrix using triplet format (i, j, x)
  i_indices <- c(which(valid_home), which(valid_away))
  j_indices <- c(home_indices[valid_home], away_indices[valid_away])
  values <- c(rep(1, sum(valid_home)), rep(-1, sum(valid_away)))
  
  # Create sparse matrix
  X <- sparseMatrix(
    i = i_indices,
    j = j_indices,
    x = values,
    dims = c(n_shifts, n_players)
  )
  
  colnames(X) <- players
  
  message("✓ Matrix created")
  return(X)
}

# Create player design matrix
X <- create_rapm_matrix_fast(shifts, all_players)

# Response variable: win probability change
y <- shifts$wp_change

# Remove rows/columns with no variation
message("Filtering valid shifts and players...")
valid_shifts <- which(rowSums(X != 0) > 0)
valid_players <- which(colSums(X != 0) > 0)

X_valid <- X[valid_shifts, valid_players]
y_valid <- y[valid_shifts]

message(paste("After filtering:", nrow(X_valid), "shifts and", ncol(X_valid), "players"))

# OPTIMIZATION 3: Vectorized team matrix
create_team_matrix_fast <- function(shifts_data, teams) {
  n_shifts <- nrow(shifts_data)
  n_teams <- length(teams)
  
  message("Creating team matrix (vectorized)...")
  
  # Create team index lookup
  team_lookup <- setNames(1:n_teams, teams)
  
  # Find indices
  home_indices <- match(shifts_data$home_team, teams)
  away_indices <- match(shifts_data$away_team, teams)
  
  # Remove NAs
  valid_home <- !is.na(home_indices)
  valid_away <- !is.na(away_indices)
  
  # Build sparse matrix
  i_indices <- c(which(valid_home), which(valid_away))
  j_indices <- c(home_indices[valid_home], away_indices[valid_away])
  values <- c(rep(1, sum(valid_home)), rep(-1, sum(valid_away)))
  
  teams_matrix <- sparseMatrix(
    i = i_indices,
    j = j_indices,
    x = values,
    dims = c(n_shifts, n_teams)
  )
  
  colnames(teams_matrix) <- teams
  message("✓ Team matrix created")
  return(teams_matrix)
}

teams_matrix <- create_team_matrix_fast(shifts, all_teams)

# Combine player and team effects
message("Combining matrices...")
X_combined <- cbind(X, teams_matrix)
X_combined_valid <- X_combined[valid_shifts, ]

message(paste("Combined matrix dimensions:", 
              nrow(X_combined_valid), "x", ncol(X_combined_valid)))

# OPTIMIZATION 4: Reduce CV folds for speed
N_FOLDS <- 5  # Instead of 10

# Fit Ridge Regression (L2 regularization)
message("\nFitting Ridge regression model...")
set.seed(479)

cv_ridge <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  alpha = 0,  # Ridge regression
  nfolds = N_FOLDS,  # Reduced from 10
  standardize = TRUE,
  parallel = FALSE,
  type.measure = "mse"
)

ridge_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  alpha = 0,
  lambda = cv_ridge$lambda.min,
  standardize = TRUE
)

ridge_coefs <- coef(ridge_model)
ridge_player_effects <- ridge_coefs[2:(length(valid_players) + 1)]
names(ridge_player_effects) <- colnames(X_valid)

message("✓ Ridge complete")

# Fit Lasso Regression (L1 regularization)
message("Fitting Lasso regression model...")
cv_lasso <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  alpha = 1,  # Lasso regression
  nfolds = N_FOLDS,
  standardize = TRUE,
  parallel = FALSE,
  type.measure = "mse"
)

lasso_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  alpha = 1,
  lambda = cv_lasso$lambda.min,
  standardize = TRUE
)

lasso_coefs <- coef(lasso_model)
lasso_player_effects <- lasso_coefs[2:(length(valid_players) + 1)]
names(lasso_player_effects) <- colnames(X_valid)

message("✓ Lasso complete")

# Fit Elastic Net (combination)
message("Fitting Elastic Net model...")
cv_elastic <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  alpha = 0.5,  # Elastic net
  nfolds = N_FOLDS,
  standardize = TRUE,
  parallel = FALSE,
  type.measure = "mse"
)

elastic_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  alpha = 0.5,
  lambda = cv_elastic$lambda.min,
  standardize = TRUE
)

elastic_coefs <- coef(elastic_model)
elastic_player_effects <- elastic_coefs[2:(length(valid_players) + 1)]
names(elastic_player_effects) <- colnames(X_valid)

message("✓ Elastic Net complete")

# Compile RAPM results
message("\nCompiling results...")
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
    avg_minutes = mean(min, na.rm = TRUE),
    .groups = "drop"
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
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
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

message("\n✓ RAPM fitting complete!")
message(paste("Total players analyzed:", nrow(rapm_results)))

