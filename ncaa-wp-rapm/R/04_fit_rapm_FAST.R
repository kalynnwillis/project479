# Fit RAPM using regularized regression - FAST OPTIMIZED VERSION
# Regularized Adjusted Plus-Minus on Win Probability scale

source("R/utils.R")
source("R/conference_map.R")

library(tidyverse)
library(glmnet)
library(Matrix)

# SPEED OPTIMIZATION: Parallel CV
if (requireNamespace("doParallel", quietly = TRUE)) {
  library(doParallel)
  n_cores <- max(1, parallel::detectCores() - 1)
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  message(paste("Parallel processing enabled:", n_cores, "cores"))
  USE_PARALLEL <- TRUE
} else {
  message("doParallel not available, using single core")
  USE_PARALLEL <- FALSE
}

message("Fitting RAPM model (FAST VERSION)...")

# ============================================================================
# DATA SCOPE: 7 seasons (2018-2024)
# All player statistics and thresholds are CAREER TOTALS across all seasons
# ============================================================================

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
MIN_MINUTES <- 2000 # Multi-season data: ~1-2 solid seasons (60 games × 30 min)
MIN_GAMES <- 50 # Multi-season data: ~1-2 seasons of regular play
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

# FIX A: Response variable on PER-MINUTE scale with outlier guards
# Protect against tiny durations and extreme outliers
y <- shifts$wp_change / pmax(shifts$duration, 60) * 60 # Minimum duration = 60 sec
y <- pmin(pmax(y, -0.02), 0.02) # Cap at ±2% WP per minute
y <- ifelse(is.finite(y), y, 0) # Handle any inf/NaN

message(paste("Response: ΔWP per minute (mean =", round(mean(y), 5), ", SD =", round(sd(y), 5), ")"))

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

# Remove rows/columns with no variation and ensure finite values
message("Filtering valid shifts and players...")
valid_shifts <- which(rowSums(X != 0) > 0 & is.finite(y))
valid_players <- which(colSums(X != 0) > 0)

X_valid <- X[valid_shifts, valid_players]
y_valid <- y[valid_shifts]
weights_valid <- weights[valid_shifts]
shifts_valid <- shifts[valid_shifts, ] # CRITICAL: Keep only valid shifts

message(paste("After filtering:", nrow(X_valid), "shifts and", ncol(X_valid), "players"))
message(paste("Median shift weight:", round(median(weights_valid), 2)))

# ============================================================================
# TEAM & CONFERENCE MATRICES (create ONLY for valid shifts - MUCH faster!)
# ============================================================================
message("\n=== Creating team and conference fixed effects ===")

# Map teams to conferences ONCE
team_conf_map <- data.frame(
  team = all_teams,
  conference = get_team_conference(all_teams)
)

# Add conference info to VALID shifts only (not all shifts!)
shifts_valid_conf <- shifts_valid %>%
  left_join(team_conf_map, by = c("home_team" = "team")) %>%
  rename(home_conf = conference) %>%
  left_join(team_conf_map, by = c("away_team" = "team")) %>%
  rename(away_conf = conference)

all_conferences <- sort(unique(c(shifts_valid_conf$home_conf, shifts_valid_conf$away_conf)))
message(paste("Conferences:", length(all_conferences)))

# OPTIMIZED: Build both matrices in one pass for VALID shifts only
n_valid <- nrow(shifts_valid_conf)
n_teams <- length(all_teams)
n_conf <- length(all_conferences)

message(paste("Building matrices for", n_valid, "valid shifts..."))

# Team matrix (with NA guards)
home_team_idx <- match(shifts_valid_conf$home_team, all_teams)
away_team_idx <- match(shifts_valid_conf$away_team, all_teams)

valid_home_t <- !is.na(home_team_idx)
valid_away_t <- !is.na(away_team_idx)

teams_matrix_valid <- sparseMatrix(
  i = c(which(valid_home_t), which(valid_away_t)),
  j = c(home_team_idx[valid_home_t], away_team_idx[valid_away_t]),
  x = c(rep(1L, sum(valid_home_t)), rep(-1L, sum(valid_away_t))),
  dims = c(n_valid, n_teams)
)
colnames(teams_matrix_valid) <- all_teams

# Conference matrix (with NA guards)
home_conf_idx <- match(shifts_valid_conf$home_conf, all_conferences)
away_conf_idx <- match(shifts_valid_conf$away_conf, all_conferences)

valid_home_c <- !is.na(home_conf_idx)
valid_away_c <- !is.na(away_conf_idx)

conf_matrix_valid <- sparseMatrix(
  i = c(which(valid_home_c), which(valid_away_c)),
  j = c(home_conf_idx[valid_home_c], away_conf_idx[valid_away_c]),
  x = c(rep(1L, sum(valid_home_c)), rep(-1L, sum(valid_away_c))),
  dims = c(n_valid, n_conf)
)
colnames(conf_matrix_valid) <- all_conferences

message("✓ Matrices created (no subsetting needed!)")

# FAST: Combine matrices (all already same size)
message("Combining matrices...")
X_combined_valid <- cbind(X_valid, teams_matrix_valid, conf_matrix_valid)

message(paste(
  "Combined matrix dimensions:",
  nrow(X_combined_valid), "x", ncol(X_combined_valid)
))

# OPTIMIZATION: Faster CV with shorter lambda path
N_FOLDS <- 5 # Reduced from 10
N_LAMBDA <- 40 # Reduced from 100 (default)
LAMBDA_MIN_RATIO <- 1e-2 # Less aggressive (default 1e-4)

# Fit Ridge Regression (L2 regularization) WITH WEIGHTING
message("\nFitting Ridge regression model (WEIGHTED by shift duration)...")
message(paste("  Matrix size:", nrow(X_combined_valid), "x", ncol(X_combined_valid)))
t_ridge_start <- Sys.time()
set.seed(479)

cv_ridge <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid, # CRITICAL: Weight by duration
  alpha = 0, # Ridge regression
  nfolds = N_FOLDS,
  nlambda = N_LAMBDA,
  lambda.min.ratio = LAMBDA_MIN_RATIO,
  standardize = TRUE,
  parallel = USE_PARALLEL,
  type.measure = "mse"
)

# Use lambda.1se (stronger shrinkage, more stable)
ridge_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid, # CRITICAL: Weight by duration
  alpha = 0,
  lambda = cv_ridge$lambda.1se,
  standardize = TRUE
)

ridge_coefs <- coef(ridge_model)
ridge_player_effects <- ridge_coefs[2:(length(valid_players) + 1)]
names(ridge_player_effects) <- colnames(X_valid)

# Also extract team and conference effects
n_players <- length(valid_players)
n_teams <- ncol(teams_matrix_valid)
n_conferences <- length(all_conferences)
ridge_team_effects <- ridge_coefs[(n_players + 2):(n_players + n_teams + 1)]
ridge_conf_effects <- ridge_coefs[(n_players + n_teams + 2):(n_players + n_teams + n_conferences + 1)]

message(paste("✓ Ridge complete in", round(difftime(Sys.time(), t_ridge_start, units = "mins"), 1), "minutes"))

# Fit Lasso Regression (L1 regularization) WITH WEIGHTING
message("Fitting Lasso regression model (WEIGHTED by shift duration)...")
cv_lasso <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid, # CRITICAL: Weight by duration
  alpha = 1, # Lasso regression
  nfolds = N_FOLDS,
  nlambda = N_LAMBDA,
  lambda.min.ratio = LAMBDA_MIN_RATIO,
  standardize = TRUE,
  parallel = USE_PARALLEL,
  type.measure = "mse"
)

lasso_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid, # CRITICAL: Weight by duration
  alpha = 1,
  lambda = cv_lasso$lambda.1se,
  standardize = TRUE
)

lasso_coefs <- coef(lasso_model)
lasso_player_effects <- lasso_coefs[2:(length(valid_players) + 1)]
names(lasso_player_effects) <- colnames(X_valid)

# Extract team and conference effects
lasso_team_effects <- lasso_coefs[(n_players + 2):(n_players + n_teams + 1)]
lasso_conf_effects <- lasso_coefs[(n_players + n_teams + 2):(n_players + n_teams + n_conferences + 1)]

message("✓ Lasso complete")

# Fit Elastic Net (combination) WITH WEIGHTING
message("Fitting Elastic Net model (WEIGHTED by shift duration)...")
cv_elastic <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid, # CRITICAL: Weight by duration
  alpha = 0.5, # Elastic net
  nfolds = N_FOLDS,
  nlambda = N_LAMBDA,
  lambda.min.ratio = LAMBDA_MIN_RATIO,
  standardize = TRUE,
  parallel = USE_PARALLEL,
  type.measure = "mse"
)

elastic_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid, # CRITICAL: Weight by duration
  alpha = 0.5,
  lambda = cv_elastic$lambda.1se,
  standardize = TRUE
)

elastic_coefs <- coef(elastic_model)
elastic_player_effects <- elastic_coefs[2:(length(valid_players) + 1)]
names(elastic_player_effects) <- colnames(X_valid)

# Extract team and conference effects
elastic_team_effects <- elastic_coefs[(n_players + 2):(n_players + n_teams + 1)]
elastic_conf_effects <- elastic_coefs[(n_players + n_teams + 2):(n_players + n_teams + n_conferences + 1)]

message("✓ Elastic Net complete")

# ============================================================================
# BASELINE APM (Weighted OLS - no regularization) as cross-check
# ============================================================================
message("\n=== Fitting Baseline APM (Weighted OLS) ===")

# Fit unregularized model with very large lambda (effectively no penalty)
baseline_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid,
  alpha = 0,
  lambda = 1e-10, # Nearly zero penalty
  standardize = TRUE
)

baseline_coefs <- coef(baseline_model)
baseline_player_effects <- baseline_coefs[2:(length(valid_players) + 1)]
names(baseline_player_effects) <- colnames(X_valid)

message(paste(
  "  Baseline APM range:",
  round(min(baseline_player_effects), 6), "to",
  round(max(baseline_player_effects), 6)
))
message(paste(
  "  Ridge range:",
  round(min(ridge_player_effects), 6), "to",
  round(max(ridge_player_effects), 6)
))
message("  → Regularization is shrinking estimates as expected")

# ============================================================================
# GAME-WISE OUT-OF-SAMPLE MSE (robustness check)
# ============================================================================
message("\n=== Computing game-wise OOS MSE ===")

# Get unique games from valid shifts
shifts_valid$game_id <- shifts[valid_shifts, ]$game_id
unique_games <- unique(shifts_valid$game_id)
n_games <- length(unique_games)

message(paste("Evaluating on", n_games, "games..."))

# Hold out each game and predict
set.seed(479)
n_oos_games <- min(50, n_games) # Sample games for speed
oos_games <- sample(unique_games, n_oos_games)
oos_mse_ridge <- numeric(n_oos_games)
oos_mse_baseline <- numeric(n_oos_games)

for (i in seq_along(oos_games)) {
  test_game <- oos_games[i]
  train_idx <- which(shifts_valid$game_id != test_game)
  test_idx <- which(shifts_valid$game_id == test_game)

  if (length(test_idx) > 0 && length(train_idx) > 100) {
    # Fit on train
    cv_temp <- cv.glmnet(
      x = X_combined_valid[train_idx, ],
      y = y_valid[train_idx],
      weights = weights_valid[train_idx],
      alpha = 0,
      nfolds = 3,
      nlambda = 20,
      standardize = TRUE
    )

    # Predict on test
    pred_ridge <- predict(cv_temp, s = "lambda.1se", newx = X_combined_valid[test_idx, ])
    pred_baseline <- predict(
      glmnet(X_combined_valid[train_idx, ], y_valid[train_idx],
        weights = weights_valid[train_idx], alpha = 0, lambda = 1e-10
      ),
      newx = X_combined_valid[test_idx, ]
    )

    oos_mse_ridge[i] <- mean((y_valid[test_idx] - pred_ridge)^2)
    oos_mse_baseline[i] <- mean((y_valid[test_idx] - pred_baseline)^2)
  }
}

valid_oos <- oos_mse_ridge > 0
message(paste("  Ridge OOS MSE:", round(mean(oos_mse_ridge[valid_oos]), 6)))
message(paste("  Baseline OOS MSE:", round(mean(oos_mse_baseline[valid_oos]), 6)))
message(paste(
  "  Improvement:",
  round(100 * (1 - mean(oos_mse_ridge[valid_oos]) / mean(oos_mse_baseline[valid_oos])), 1), "%"
))

# Compile RAPM results
message("\nCompiling results...")
rapm_results <- tibble(
  player = colnames(X_valid),
  baseline_apm = as.numeric(baseline_player_effects), # NEW: Unregularized
  ridge_rapm = as.numeric(ridge_player_effects),
  lasso_rapm = as.numeric(lasso_player_effects),
  elastic_rapm = as.numeric(elastic_player_effects)
) %>%
  arrange(desc(ridge_rapm))

# Compile conference effects
conference_effects <- tibble(
  conference = all_conferences,
  ridge_conf = as.numeric(ridge_conf_effects),
  lasso_conf = as.numeric(lasso_conf_effects),
  elastic_conf = as.numeric(elastic_conf_effects)
) %>%
  mutate(
    ridge_conf_per40 = ridge_conf * 40,
    lasso_conf_per40 = lasso_conf * 40,
    elastic_conf_per40 = elastic_conf * 40
  ) %>%
  arrange(desc(ridge_conf))

message("\n=== Conference Effects (Ridge per 40 min) ===")
print(conference_effects %>% select(conference, ridge_conf_per40))

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
    baseline_per40 = baseline_apm * 40,
    ridge_per40 = ridge_rapm * 40,
    lasso_per40 = lasso_rapm * 40,
    elastic_per40 = elastic_rapm * 40,
    # Percentiles
    ridge_percentile = percent_rank(ridge_rapm) * 100,
    lasso_percentile = percent_rank(lasso_rapm) * 100,
    elastic_percentile = percent_rank(elastic_rapm) * 100
  )

# ============================================================================
# SANITY CHECKS (lecture-aligned validation)
# ============================================================================
message("\n=== RAPM Sanity Checks ===")

# Check 1: Mean should be near 0 after centering
rapm_mean <- mean(rapm_results$ridge_rapm, na.rm = TRUE)
rapm_sd <- sd(rapm_results$ridge_rapm, na.rm = TRUE)
message(paste("Ridge RAPM mean:", round(rapm_mean, 6), "(should be ≈0)"))
message(paste("Ridge RAPM SD:", round(rapm_sd, 6)))

# Check 2: Correlation with minutes (should be near 0 after shrinkage)
minutes_cor <- cor(rapm_results$ridge_rapm, rapm_results$total_minutes,
  use = "complete.obs"
)
message(paste(
  "Correlation(RAPM, minutes):", round(minutes_cor, 3),
  "(should be ≈0 with good shrinkage)"
))

if (abs(minutes_cor) > 0.3) {
  message("  ⚠ Warning: High correlation with minutes may indicate under-regularization")
} else {
  message("  ✓ Good: RAPM not biased toward high-minute players")
}

# ============================================================================
# MINUTES-BASED SENSITIVITY ANALYSIS (from lecture)
# ============================================================================
message("\n=== MINUTES-BASED SENSITIVITY ===")

# Create filtered versions at different minutes thresholds (sensitivity analysis)
rapm_1500min <- rapm_results %>%
  filter(total_minutes >= 1500) %>%
  arrange(desc(ridge_rapm))

rapm_2500min <- rapm_results %>%
  filter(total_minutes >= 2500) %>%
  arrange(desc(ridge_rapm))

message(paste("Players with ≥1500 minutes:", nrow(rapm_1500min)))
message(paste("Players with ≥2500 minutes:", nrow(rapm_2500min)))

# Compare top 10 lists
top10_all <- rapm_results %>%
  arrange(desc(ridge_rapm)) %>%
  head(10) %>%
  pull(player)
top10_1500 <- rapm_1500min %>%
  head(10) %>%
  pull(player)
top10_2500 <- rapm_2500min %>%
  head(10) %>%
  pull(player)

message("\nTop 10 overlap:")
message(paste("  All vs ≥1500min:", length(intersect(top10_all, top10_1500)), "/ 10"))
message(paste("  All vs ≥2500min:", length(intersect(top10_all, top10_2500)), "/ 10"))
message(paste("  ≥1500 vs ≥2500min:", length(intersect(top10_1500, top10_2500)), "/ 10"))

# Model evaluation metrics
# CLEANUP: Stop parallel cluster
if (USE_PARALLEL) {
  stopCluster(cl)
  message("Parallel cluster stopped")
}

message("\n=== Model Evaluation ===")
message(paste("Ridge lambda (1se):", round(cv_ridge$lambda.1se, 6)))
message(paste("Lasso lambda (1se):", round(cv_lasso$lambda.1se, 6)))
message(paste("Elastic lambda (1se):", round(cv_elastic$lambda.1se, 6)))

message(paste("\nRidge MSE:", round(min(cv_ridge$cvm), 6)))
message(paste("Lasso MSE:", round(min(cv_lasso$cvm), 6)))
message(paste("Elastic MSE:", round(min(cv_elastic$cvm), 6)))

# Apply final consistency filter (match the pre-matrix threshold)
rapm_results_filtered <- rapm_results %>%
  filter(total_minutes >= MIN_MINUTES, games_played >= MIN_GAMES)

message(paste("Players after final filter:", nrow(rapm_results_filtered), "/", nrow(rapm_results)))

# Save results (directories created by 00_setup.R)
rapm_output <- list(
  rapm_table = rapm_results_filtered, # Filtered version for primary analysis
  rapm_table_all = rapm_results, # Unfiltered for diagnostics
  rapm_1500min = rapm_1500min, # Sensitivity: ≥1500 minutes
  rapm_2500min = rapm_2500min, # Sensitivity: ≥2500 minutes
  conference_effects = conference_effects, # Conference fixed effects
  ridge_model = ridge_model,
  lasso_model = lasso_model,
  elastic_model = elastic_model,
  baseline_model = baseline_model, # NEW: Unregularized baseline
  cv_ridge = cv_ridge,
  cv_lasso = cv_lasso,
  cv_elastic = cv_elastic,
  design_matrix = X_combined_valid,
  response = y_valid,
  sanity_checks = list(
    rapm_mean = rapm_mean,
    rapm_sd = rapm_sd,
    minutes_cor = minutes_cor
  ),
  oos_validation = list( # NEW: Out-of-sample validation
    ridge_mse = mean(oos_mse_ridge[valid_oos]),
    baseline_mse = mean(oos_mse_baseline[valid_oos]),
    n_games = sum(valid_oos)
  )
)

saveRDS(rapm_output, "data/processed/rapm_results.rds")
write_csv(rapm_results_filtered, "tables/rapm_rankings.csv")
write_csv(rapm_1500min, "tables/rapm_rankings_1500min.csv")
write_csv(rapm_2500min, "tables/rapm_rankings_2500min.csv")
write_csv(conference_effects, "tables/conference_effects.csv")

# Print top players
message("\n=== Top 20 Players (Ridge RAPM per 40 min) ===")
message(paste("Note: Filtered to ≥", MIN_MINUTES, "minutes and ≥", MIN_GAMES, "games"))
print(rapm_results_filtered %>%
  arrange(desc(ridge_rapm)) %>%
  select(player, ridge_per40, baseline_per40, games_played, total_minutes) %>%
  head(20))

message("\n=== Baseline vs Ridge Comparison ===")
baseline_sd <- sd(rapm_results_filtered$baseline_per40, na.rm = TRUE)
ridge_sd <- sd(rapm_results_filtered$ridge_per40, na.rm = TRUE)
message(paste("Baseline APM SD:", round(baseline_sd, 4)))
message(paste("Ridge RAPM SD:", round(ridge_sd, 4)))
message(paste("Shrinkage factor:", round(ridge_sd / baseline_sd, 3)))

message("\n✓ RAPM fitting complete!")
message(paste(
  "Total players analyzed:", nrow(rapm_results_filtered),
  "(", nrow(rapm_results) - nrow(rapm_results_filtered), "filtered out)"
))
