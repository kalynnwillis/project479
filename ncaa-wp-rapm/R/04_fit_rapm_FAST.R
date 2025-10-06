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
box_scores <- readRDS("data/interim/box_scores.rds")

message(paste("Loaded", nrow(shifts), "shift observations"))

# Load player profiles (if available from enhanced data extraction)
player_profiles_file <- "data/interim/player_profiles.rds"
if (file.exists(player_profiles_file)) {
  player_profiles <- readRDS(player_profiles_file)
  message(paste("Loaded player profiles for", nrow(player_profiles), "players"))
  message("Player profiles will be used to enhance RAPM estimates")
} else {
  player_profiles <- NULL
  message("No player profiles found - using basic RAPM")
}

# OPTIMIZATION 1: Sample shifts if dataset is too large
MAX_SHIFTS <- 500000 # Limit for speed
if (nrow(shifts) > MAX_SHIFTS) {
  message(paste("Sampling", MAX_SHIFTS, "shifts from", nrow(shifts), "for faster computation..."))
  set.seed(479)
  shifts <- shifts %>% slice_sample(n = MAX_SHIFTS)
  message(paste("Using", nrow(shifts), "sampled shifts"))
}

# FIX B: Build minutes/games per player from FULL box scores (not just starters)
minutes_per_player <- box_scores %>%
  group_by(player) %>%
  summarise(
    total_minutes = sum(min, na.rm = TRUE),
    games_played = n(),
    avg_minutes = total_minutes / games_played,
    .groups = "drop"
  )

# Filter to players with sufficient sample (lecture-aligned thresholds)
MIN_MINUTES <- 300 # ~10 games × 30 min
MIN_GAMES <- 10
keep_players <- minutes_per_player %>%
  filter(total_minutes >= MIN_MINUTES, games_played >= MIN_GAMES) %>%
  pull(player)

message(paste("Players meeting thresholds (≥", MIN_MINUTES, "min, ≥", MIN_GAMES, "games):", length(keep_players)))

# Get unique players from starter lists, filtered by sample size
all_players_raw <- sort(unique(c(unlist(shifts$home_starters), unlist(shifts$away_starters))))
all_players <- intersect(all_players_raw, keep_players) # FILTER HERE
all_teams <- unique(c(shifts$home_team, shifts$away_team))

message(paste("Players in RAPM (after filtering):", length(all_players)))
message(paste("Teams:", length(all_teams)))

# CRITICAL: Build sparse matrix directly from starter lists (no materialization!)
create_rapm_matrix_from_lists <- function(stints_data, players) {
  n_stints <- nrow(stints_data)
  n_players <- length(players)

  message("Creating sparse design matrix from starter lists...")
  message(paste("Dimensions:", n_stints, "stints x", n_players, "players"))

  # Create player index lookup
  player_lookup <- setNames(1:n_players, players)

  # Build triplet format (stint_idx, player_idx, value)
  i_indices <- integer()
  j_indices <- integer()
  values <- numeric()

  for (stint_idx in 1:n_stints) {
    # Home starters: +1
    home_players <- stints_data$home_starters[[stint_idx]]
    home_idx <- player_lookup[home_players]
    home_idx <- home_idx[!is.na(home_idx)]

    if (length(home_idx) > 0) {
      i_indices <- c(i_indices, rep(stint_idx, length(home_idx)))
      j_indices <- c(j_indices, home_idx)
      values <- c(values, rep(1, length(home_idx)))
    }

    # Away starters: -1
    away_players <- stints_data$away_starters[[stint_idx]]
    away_idx <- player_lookup[away_players]
    away_idx <- away_idx[!is.na(away_idx)]

    if (length(away_idx) > 0) {
      i_indices <- c(i_indices, rep(stint_idx, length(away_idx)))
      j_indices <- c(j_indices, away_idx)
      values <- c(values, rep(-1, length(away_idx)))
    }

    if (stint_idx %% 10000 == 0) {
      message(paste("  Processed", stint_idx, "/", n_stints, "stints"))
    }
  }

  # Create sparse matrix
  X <- sparseMatrix(
    i = i_indices,
    j = j_indices,
    x = values,
    dims = c(n_stints, n_players)
  )

  colnames(X) <- players

  message("✓ Matrix created (10 non-zeros per stint)")
  return(X)
}

# Create player design matrix
X <- create_rapm_matrix_from_lists(shifts, all_players)

# FIX A: Response variable on PER-MINUTE scale (not per-half)
# This makes coefficients interpretable as ΔWP per minute
y <- shifts$wp_change / (shifts$duration / 60) # ΔWP per minute

message(paste("Response: ΔWP per minute (mean =", round(mean(y), 5), ")"))

# CRITICAL: Weight shifts by duration AND leverage (from lecture - WAPM)
# 1. Duration weighting: longer shifts = more reliable
# 2. Leverage weighting: close games = more important (down-weight blowouts)
if ("leverage" %in% names(shifts)) {
  weights <- sqrt(shifts$duration) * shifts$leverage
  message("Using WEIGHTED APM: Duration × Leverage (close games up-weighted, blowouts down-weighted)")
} else {
  # Fallback to duration only
  weights <- sqrt(shifts$duration)
  message("Using duration-weighted shifts (no leverage data)")
}

# Normalize weights to sum to number of observations
weights <- weights * (nrow(shifts) / sum(weights))

# Remove rows/columns with no variation
message("Filtering valid shifts and players...")
valid_shifts <- which(rowSums(X != 0) > 0)
valid_players <- which(colSums(X != 0) > 0)

X_valid <- X[valid_shifts, valid_players]
y_valid <- y[valid_shifts]
weights_valid <- weights[valid_shifts]

message(paste("After filtering:", nrow(X_valid), "shifts and", ncol(X_valid), "players"))
message(paste("Median shift weight:", round(median(weights_valid), 2)))

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

message(paste(
  "Combined matrix dimensions:",
  nrow(X_combined_valid), "x", ncol(X_combined_valid)
))

# OPTIMIZATION 4: Reduce CV folds for speed
N_FOLDS <- 5 # Instead of 10

# Fit Ridge Regression (L2 regularization) WITH WEIGHTING
message("\nFitting Ridge regression model (WEIGHTED by shift duration)...")
set.seed(479)

cv_ridge <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid, # CRITICAL: Weight by duration
  alpha = 0, # Ridge regression
  nfolds = N_FOLDS, # Reduced from 10
  standardize = TRUE,
  parallel = FALSE,
  type.measure = "mse"
)

ridge_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid, # CRITICAL: Weight by duration
  alpha = 0,
  lambda = cv_ridge$lambda.min,
  standardize = TRUE
)

ridge_coefs <- coef(ridge_model)
ridge_player_effects <- ridge_coefs[2:(length(valid_players) + 1)]
names(ridge_player_effects) <- colnames(X_valid)

message("✓ Ridge complete")

# Fit Lasso Regression (L1 regularization) WITH WEIGHTING
message("Fitting Lasso regression model (WEIGHTED by shift duration)...")
cv_lasso <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid, # CRITICAL: Weight by duration
  alpha = 1, # Lasso regression
  nfolds = N_FOLDS,
  standardize = TRUE,
  parallel = FALSE,
  type.measure = "mse"
)

lasso_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid, # CRITICAL: Weight by duration
  alpha = 1,
  lambda = cv_lasso$lambda.min,
  standardize = TRUE
)

lasso_coefs <- coef(lasso_model)
lasso_player_effects <- lasso_coefs[2:(length(valid_players) + 1)]
names(lasso_player_effects) <- colnames(X_valid)

message("✓ Lasso complete")

# Fit Elastic Net (combination) WITH WEIGHTING
message("Fitting Elastic Net model (WEIGHTED by shift duration)...")
cv_elastic <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid, # CRITICAL: Weight by duration
  alpha = 0.5, # Elastic net
  nfolds = N_FOLDS,
  standardize = TRUE,
  parallel = FALSE,
  type.measure = "mse"
)

elastic_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid, # CRITICAL: Weight by duration
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

# FIX C: Use full box scores for games_played (not just starts)
# This join brings in: total_minutes, games_played, avg_minutes
rapm_results <- rapm_results %>%
  left_join(minutes_per_player, by = "player")

# Add player profiles if available (NEW!)
if (!is.null(player_profiles)) {
  # Aggregate profiles by player (some players may have played for multiple teams)
  player_profile_summary <- player_profiles %>%
    group_by(player) %>%
    summarise(
      position = first(position),
      is_regular_starter = any(is_regular_starter),
      ft_pct = weighted.mean(ft_pct, w = games_played, na.rm = TRUE),
      fg3_pct = weighted.mean(fg3_pct, w = games_played, na.rm = TRUE),
      fg_pct = weighted.mean(fg_pct, w = games_played, na.rm = TRUE),
      fta_per_40 = weighted.mean(fta_per_40, w = games_played, na.rm = TRUE),
      fg3a_per_40 = weighted.mean(fg3a_per_40, w = games_played, na.rm = TRUE),
      stl_per_40 = weighted.mean(stl_per_40, w = games_played, na.rm = TRUE),
      blk_per_40 = weighted.mean(blk_per_40, w = games_played, na.rm = TRUE),
      pts_per_min = weighted.mean(pts_per_min, w = games_played, na.rm = TRUE),
      .groups = "drop"
    )

  rapm_results <- rapm_results %>%
    left_join(player_profile_summary, by = "player")

  message("✓ Added player shooting and defensive profiles to RAPM results")
}

# Compute percentile ranks and PER-40 versions for readability
rapm_results <- rapm_results %>%
  mutate(
    # Per-40 minute versions (easier to interpret)
    ridge_per40 = ridge_rapm * 40,
    lasso_per40 = lasso_rapm * 40,
    elastic_per40 = elastic_rapm * 40,
    # Percentiles
    ridge_percentile = percent_rank(ridge_rapm) * 100,
    lasso_percentile = percent_rank(lasso_rapm) * 100,
    elastic_percentile = percent_rank(elastic_rapm) * 100
  )

# ============================================================================
# MINUTES-BASED SENSITIVITY ANALYSIS (from lecture)
# ============================================================================
message("\n=== MINUTES-BASED SENSITIVITY ===")

# Create filtered versions at different minutes thresholds
rapm_250min <- rapm_results %>%
  filter(total_minutes >= 250) %>%
  arrange(desc(ridge_rapm))

rapm_400min <- rapm_results %>%
  filter(total_minutes >= 400) %>%
  arrange(desc(ridge_rapm))

message(paste("Players with ≥250 minutes:", nrow(rapm_250min)))
message(paste("Players with ≥400 minutes:", nrow(rapm_400min)))

# Compare top 10 lists
top10_all <- rapm_results %>%
  arrange(desc(ridge_rapm)) %>%
  head(10) %>%
  pull(player)
top10_250 <- rapm_250min %>%
  head(10) %>%
  pull(player)
top10_400 <- rapm_400min %>%
  head(10) %>%
  pull(player)

message("\nTop 10 overlap:")
message(paste("  All vs ≥250min:", length(intersect(top10_all, top10_250)), "/ 10"))
message(paste("  All vs ≥400min:", length(intersect(top10_all, top10_400)), "/ 10"))
message(paste("  ≥250 vs ≥400min:", length(intersect(top10_250, top10_400)), "/ 10"))

# Model evaluation metrics
message("\n=== Model Evaluation ===")
message(paste("Ridge lambda:", round(cv_ridge$lambda.min, 6)))
message(paste("Lasso lambda:", round(cv_lasso$lambda.min, 6)))
message(paste("Elastic lambda:", round(cv_elastic$lambda.min, 6)))

message(paste("\nRidge MSE:", round(min(cv_ridge$cvm), 6)))
message(paste("Lasso MSE:", round(min(cv_lasso$cvm), 6)))
message(paste("Elastic MSE:", round(min(cv_elastic$cvm), 6)))

# Save results (directories created by 00_setup.R)
rapm_output <- list(
  rapm_table = rapm_results,
  rapm_250min = rapm_250min, # Sensitivity: ≥250 minutes
  rapm_400min = rapm_400min, # Sensitivity: ≥400 minutes
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
write_csv(rapm_250min, "tables/rapm_rankings_250min.csv")
write_csv(rapm_400min, "tables/rapm_rankings_400min.csv")

# Print top players
message("\n=== Top 20 Players (Ridge RAPM per 40 min) ===")
print(rapm_results %>%
  filter(!is.na(games_played)) %>%
  arrange(desc(ridge_rapm)) %>%
  select(player, ridge_rapm, ridge_per40, games_played, total_minutes) %>%
  head(20))

message("\n✓ RAPM fitting complete!")
message(paste("Total players analyzed:", nrow(rapm_results)))
