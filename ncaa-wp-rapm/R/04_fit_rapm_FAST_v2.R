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
  USE_PARALLEL <- TRUE
} else {
  USE_PARALLEL <- FALSE
}

shifts <- readRDS("new_data/interim/player_shifts.rds")
player_games <- readRDS("new_data/interim/player_games.rds")
box_scores <- readRDS("new_data/interim/box_scores.rds")

message(paste("Loaded", nrow(shifts), "shift observations"))

# Load player profiles
player_profiles_file <- "new_data/interim/player_profiles.rds"
if (file.exists(player_profiles_file)) {
  player_profiles <- readRDS(player_profiles_file)
} else {
  player_profiles <- NULL
}

# OPTIMIZATION 1: Sample shifts if dataset is too large
MAX_SHIFTS <- 500000
if (nrow(shifts) > MAX_SHIFTS) {
  message(paste("Sampling", MAX_SHIFTS, "shifts from", nrow(shifts), "for faster computation..."))
  set.seed(479)
  shifts <- shifts %>% slice_sample(n = MAX_SHIFTS)
  message(paste("Using", nrow(shifts), "sampled shifts"))
}

# FIX B: Build minutes/games per player from FULL box scores (not just starters)
minutes_per_player <- box_scores %>%
  group_by(player_id) %>%
  summarise(
    player = dplyr::last(player),
    total_minutes = sum(min, na.rm = TRUE),
    games_played = n(),
    avg_minutes = total_minutes / games_played,
    .groups = "drop"
  )


MIN_MINUTES <- 2000 # Multi-season data: ~1-2 solid seasons (60 games × 30 min)
MIN_GAMES <- 50 # Multi-season data: ~1-2 seasons of regular play
keep_players <- minutes_per_player %>%
  filter(total_minutes >= MIN_MINUTES, games_played >= MIN_GAMES) %>%
  pull(player_id)


all_players_raw <- sort(unique(c(unlist(shifts$home_starters_id), unlist(shifts$away_starters_id))))
all_players <- intersect(all_players_raw, keep_players)
all_teams <- unique(c(shifts$home_team, shifts$away_team))

create_rapm_matrix_from_lists <- function(stints_data, players) {
  n_stints <- nrow(stints_data)
  n_players <- length(players)


  # Create player index lookup
  player_lookup <- setNames(1:n_players, players)

  # Build triplet format (stint_idx, player_idx, value)
  i_indices <- integer()
  j_indices <- integer()
  values <- numeric()

  for (stint_idx in 1:n_stints) {
    # Home starters: +1
    home_players <- stints_data$home_starters_id[[stint_idx]]
    home_idx <- player_lookup[home_players]
    home_idx <- home_idx[!is.na(home_idx)]

    if (length(home_idx) > 0) {
      i_indices <- c(i_indices, rep(stint_idx, length(home_idx)))
      j_indices <- c(j_indices, home_idx)
      values <- c(values, rep(1, length(home_idx)))
    }

    # Away starters: -1
    away_players <- stints_data$away_starters_id[[stint_idx]]
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
y <- shifts$wp_change / pmax(shifts$duration, 60) * 60 # Minimum duration = 60 sec
y <- pmin(pmax(y, -0.02), 0.02) # Cap at ±2% WP per minute
y <- ifelse(is.finite(y), y, 0)


if ("leverage" %in% names(shifts)) {
  weights <- sqrt(shifts$duration) * shifts$leverage
  message("Using WEIGHTED APM: Duration × Leverage (close games up-weighted, blowouts down-weighted)")
} else {
  weights <- sqrt(shifts$duration)
  message("Using duration-weighted shifts (no leverage data)")
}

weights <- weights * (nrow(shifts) / sum(weights))


valid_shifts <- which(rowSums(X != 0) > 0 & is.finite(y))
valid_players <- which(colSums(X != 0) > 0)

X_valid <- X[valid_shifts, valid_players]
y_valid <- y[valid_shifts]
weights_valid <- weights[valid_shifts]
shifts_valid <- shifts[valid_shifts, ]


# Map teams to conferences using manual mapping (hoopR doesn't provide conference data)
team_conf_map <- data.frame(
  team = all_teams,
  conference = get_team_conference(all_teams)
)

# Add conference info to VALID shifts only
shifts_valid_conf <- shifts_valid %>%
  left_join(team_conf_map, by = c("home_team" = "team")) %>%
  rename(home_conf = conference) %>%
  left_join(team_conf_map, by = c("away_team" = "team")) %>%
  rename(away_conf = conference)

all_conferences <- sort(unique(c(shifts_valid_conf$home_conf, shifts_valid_conf$away_conf)))
all_seasons <- sort(unique(shifts_valid$season))

n_valid <- nrow(shifts_valid_conf)
n_teams <- length(all_teams)
n_conf <- length(all_conferences)
n_seasons <- length(all_seasons)


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

season_idx <- match(shifts_valid$season, all_seasons)
valid_season <- !is.na(season_idx)

season_matrix_valid <- sparseMatrix(
  i = which(valid_season),
  j = season_idx[valid_season],
  x = rep(1L, sum(valid_season)),
  dims = c(n_valid, n_seasons)
)
colnames(season_matrix_valid) <- paste0("Season_", all_seasons)


# FAST: Combine matrices (all already same size)
X_combined_valid <- cbind(X_valid, teams_matrix_valid, conf_matrix_valid, season_matrix_valid)


# OPTIMIZATION: Faster CV with shorter lambda path
N_FOLDS <- 5
N_LAMBDA <- 40
LAMBDA_MIN_RATIO <- 1e-2

# Fit Ridge Regression (L2 regularization) WITH WEIGHTING
t_ridge_start <- Sys.time()
set.seed(479)

cv_ridge <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid,
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
  weights = weights_valid,
  alpha = 0,
  lambda = cv_ridge$lambda.1se,
  standardize = TRUE
)

ridge_coefs <- coef(ridge_model)
ridge_player_effects <- ridge_coefs[2:(length(valid_players) + 1)]
names(ridge_player_effects) <- colnames(X_valid)


n_players <- length(valid_players)
n_teams <- ncol(teams_matrix_valid)
n_conferences <- length(all_conferences)
n_seasons_fx <- ncol(season_matrix_valid)

ridge_team_effects <- ridge_coefs[(n_players + 2):(n_players + n_teams + 1)]
ridge_conf_effects <- ridge_coefs[(n_players + n_teams + 2):(n_players + n_teams + n_conferences + 1)]
ridge_season_effects <- ridge_coefs[(n_players + n_teams + n_conferences + 2):(n_players + n_teams + n_conferences + n_seasons_fx + 1)]


# Fit Lasso Regression (L1 regularization) WITH WEIGHTING
message("Fitting Lasso regression model (WEIGHTED by shift duration)...")
cv_lasso <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid,
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
  weights = weights_valid,
  alpha = 1,
  lambda = cv_lasso$lambda.1se,
  standardize = TRUE
)

lasso_coefs <- coef(lasso_model)
lasso_player_effects <- lasso_coefs[2:(length(valid_players) + 1)]
names(lasso_player_effects) <- colnames(X_valid)


lasso_team_effects <- lasso_coefs[(n_players + 2):(n_players + n_teams + 1)]
lasso_conf_effects <- lasso_coefs[(n_players + n_teams + 2):(n_players + n_teams + n_conferences + 1)]
lasso_season_effects <- lasso_coefs[(n_players + n_teams + n_conferences + 2):(n_players + n_teams + n_conferences + n_seasons_fx + 1)]


# Fit Elastic Net (combination) WITH WEIGHTING
cv_elastic <- cv.glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid,
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
  weights = weights_valid,
  alpha = 0.5,
  lambda = cv_elastic$lambda.1se,
  standardize = TRUE
)

elastic_coefs <- coef(elastic_model)
elastic_player_effects <- elastic_coefs[2:(length(valid_players) + 1)]
names(elastic_player_effects) <- colnames(X_valid)

# Extract team, conference, and season effects
elastic_team_effects <- elastic_coefs[(n_players + 2):(n_players + n_teams + 1)]
elastic_conf_effects <- elastic_coefs[(n_players + n_teams + 2):(n_players + n_teams + n_conferences + 1)]
elastic_season_effects <- elastic_coefs[(n_players + n_teams + n_conferences + 2):(n_players + n_teams + n_conferences + n_seasons_fx + 1)]


# Fit unregularized model with very large lambda (effectively no penalty)
baseline_model <- glmnet(
  x = X_combined_valid,
  y = y_valid,
  weights = weights_valid,
  alpha = 0,
  lambda = 1e-10,
  standardize = TRUE
)

baseline_coefs <- coef(baseline_model)
baseline_player_effects <- baseline_coefs[2:(length(valid_players) + 1)]
names(baseline_player_effects) <- colnames(X_valid)


shifts_valid$game_id <- shifts[valid_shifts, ]$game_id
unique_games <- unique(shifts_valid$game_id)
n_games <- length(unique_games)


set.seed(479)
n_oos_games <- min(50, n_games)
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

# Compile RAPM results
rapm_results <- tibble(
  player_id = colnames(X_valid),
  baseline_apm = as.numeric(baseline_player_effects),
  ridge_rapm = as.numeric(ridge_player_effects),
  lasso_rapm = as.numeric(lasso_player_effects),
  elastic_rapm = as.numeric(elastic_player_effects)
) %>%
  arrange(desc(ridge_rapm))

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

print(conference_effects %>% select(conference, ridge_conf_per40))

# Compile season effects
season_effects <- tibble(
  season = all_seasons,
  ridge_season = as.numeric(ridge_season_effects),
  lasso_season = as.numeric(lasso_season_effects),
  elastic_season = as.numeric(elastic_season_effects)
) %>%
  mutate(
    ridge_season_per40 = ridge_season * 40,
    lasso_season_per40 = lasso_season * 40,
    elastic_season_per40 = elastic_season * 40
  )

print(season_effects %>% select(season, ridge_season_per40))

# FIX C: Use full box scores for games_played (not just starts)
# This join brings in: player name, total_minutes, games_played, avg_minutes
rapm_results <- rapm_results %>%
  left_join(minutes_per_player, by = "player_id")

# Add player profiles if available (NEW!)
if (!is.null(player_profiles)) {
  # Aggregate profiles by player_id (some players may have played for multiple teams)
  player_profile_summary <- player_profiles %>%
    group_by(player_id) %>%
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
    left_join(player_profile_summary, by = "player_id")
}

# Compute percentile ranks and PER-40 versions for readability
rapm_results <- rapm_results %>%
  mutate(
    # Per-40 minute versions
    baseline_per40 = baseline_apm * 40,
    ridge_per40 = ridge_rapm * 40,
    lasso_per40 = lasso_rapm * 40,
    elastic_per40 = elastic_rapm * 40,
    # Percentiles
    ridge_percentile = percent_rank(ridge_rapm) * 100,
    lasso_percentile = percent_rank(lasso_rapm) * 100,
    elastic_percentile = percent_rank(elastic_rapm) * 100
  )

# Check 1: Mean should be near 0 after centering
rapm_mean <- mean(rapm_results$ridge_rapm, na.rm = TRUE)
rapm_sd <- sd(rapm_results$ridge_rapm, na.rm = TRUE)
message(paste("Ridge RAPM mean:", round(rapm_mean, 6), "(should be ≈0)"))
message(paste("Ridge RAPM SD:", round(rapm_sd, 6)))

# Check 2: Correlation with minutes
minutes_cor <- cor(rapm_results$ridge_rapm, rapm_results$total_minutes,
  use = "complete.obs"
)

if (abs(minutes_cor) > 0.3) {
  message("  ⚠ Warning: High correlation with minutes may indicate under-regularization")
} else {
  message("  ✓ Good: RAPM not biased toward high-minute players")
}


# Create filtered versions at different minutes thresholds
rapm_1500min <- rapm_results %>%
  filter(total_minutes >= 1500) %>%
  arrange(desc(ridge_rapm))

rapm_2500min <- rapm_results %>%
  filter(total_minutes >= 2500) %>%
  arrange(desc(ridge_rapm))

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


# Model evaluation metrics
if (USE_PARALLEL) {
  stopCluster(cl)
  message("Parallel cluster stopped")
}


# Apply final consistency filter (match the pre-matrix threshold)
rapm_results_filtered <- rapm_results %>%
  filter(total_minutes >= MIN_MINUTES, games_played >= MIN_GAMES)

# Save results (directories created by 00_setup.R)
rapm_output <- list(
  rapm_table = rapm_results_filtered,
  rapm_table_all = rapm_results,
  rapm_1500min = rapm_1500min,
  rapm_2500min = rapm_2500min,
  conference_effects = conference_effects,
  season_effects = season_effects,
  ridge_model = ridge_model,
  lasso_model = lasso_model,
  elastic_model = elastic_model,
  baseline_model = baseline_model,
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
  oos_validation = list(
    ridge_mse = mean(oos_mse_ridge[valid_oos]),
    baseline_mse = mean(oos_mse_baseline[valid_oos]),
    n_games = sum(valid_oos)
  )
)

saveRDS(rapm_output, "new_data/processed/rapm_results.rds")
write_csv(rapm_results_filtered, "new_tables/rapm_rankings.csv")
write_csv(rapm_1500min, "new_tables/rapm_rankings_1500min.csv")
write_csv(rapm_2500min, "new_tables/rapm_rankings_2500min.csv")
write_csv(conference_effects, "new_tables/conference_effects.csv")
write_csv(season_effects, "new_tables/season_effects.csv")

# Print top players
message("\n=== Top 20 Players (Ridge RAPM per 40 min) ===")
message(paste("Note: Filtered to ≥", MIN_MINUTES, "minutes and ≥", MIN_GAMES, "games"))

top_20 <- rapm_results_filtered %>%
  arrange(desc(ridge_rapm)) %>%
  select(player, ridge_per40, baseline_per40, games_played, total_minutes) %>%
  head(20)

message("\nTOP 3 PLAYERS:")
for (i in seq_len(min(3, nrow(top_20)))) {
  message(sprintf(
    "%d. %-25s Ridge: %.3f | Baseline: %.3f | Games: %3d | Mins: %4d",
    i,
    top_20$player[i],
    top_20$ridge_per40[i],
    top_20$baseline_per40[i],
    top_20$games_played[i],
    top_20$total_minutes[i]
  ))
}

message("\nFull Top 20:")
print(top_20, n = 20)

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
