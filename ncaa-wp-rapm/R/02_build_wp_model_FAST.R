# Build win probability models - FAST VERSION with sampling

source("R/utils.R")

library(tidyverse)
library(caret)
library(gbm)
library(randomForest)
library(xgboost)
library(pROC)

message("Building win probability models (FAST VERSION)...")

# Load cleaned play-by-play data
pbp_data <- readRDS("data/raw/pbp_clean.rds")

message(paste("Total data:", nrow(pbp_data), "plays"))

# SAMPLE to manageable size (100K plays instead of 4M)
set.seed(479)
SAMPLE_SIZE <- 100000

if (nrow(pbp_data) > SAMPLE_SIZE) {
  message(paste("Sampling", SAMPLE_SIZE, "plays for faster training..."))
  # Sample games (not individual plays) to maintain game structure
  sample_games <- sample(unique(pbp_data$game_id), 
                        size = ceiling(SAMPLE_SIZE / (nrow(pbp_data)/n_distinct(pbp_data$game_id))))
  pbp_data <- pbp_data %>% filter(game_id %in% sample_games)
  message(paste("Sampled data:", nrow(pbp_data), "plays from", length(sample_games), "games"))
}

# Feature engineering (same as before)
wp_features <- pbp_data %>%
  mutate(
    score_diff = home_score - away_score,
    time_remaining_min = secs_remaining / 60,
    time_elapsed_min = (half - 1) * 20 + (20 - time_remaining_min),
    score_diff_x_time = score_diff * time_remaining_min,
    score_diff_sq = score_diff^2,
    has_ball = if("possession" %in% names(.)) {
      ifelse(possession == "home", 1, 0)
    } else {
      0.5
    },
    is_first_half = ifelse(half == 1, 1, 0),
    is_second_half = ifelse(half == 2, 1, 0),
    is_clutch = ifelse(time_remaining_min < 5 & abs(score_diff) < 10, 1, 0),
    home_win = as.factor(home_win)
  ) %>%
  filter(!is.na(home_win), !is.na(score_diff), !is.na(time_remaining_min)) %>%
  select(game_id, home_win, score_diff, time_remaining_min, time_elapsed_min,
         score_diff_x_time, score_diff_sq, has_ball, is_first_half, 
         is_second_half, is_clutch)

message(paste("Prepared", nrow(wp_features), "observations for modeling"))

# Split data
set.seed(479)
train_games <- sample(unique(wp_features$game_id), 
                      size = floor(0.8 * n_distinct(wp_features$game_id)))

train_data <- wp_features %>% filter(game_id %in% train_games)
test_data <- wp_features %>% filter(!game_id %in% train_games)

message(paste("Training:", nrow(train_data), "| Testing:", nrow(test_data)))

predictors <- c("score_diff", "time_remaining_min", "time_elapsed_min",
                "score_diff_x_time", "score_diff_sq", "has_ball", 
                "is_first_half", "is_second_half", "is_clutch")

# Model 1: Logistic Regression
message("Training Model 1: Logistic Regression...")
model_logit <- glm(
  home_win ~ score_diff + time_remaining_min + score_diff_x_time + 
    score_diff_sq + has_ball + is_first_half + is_clutch,
  data = train_data,
  family = binomial(link = "logit")
)

pred_logit_train <- predict(model_logit, train_data, type = "response")
pred_logit_test <- predict(model_logit, test_data, type = "response")

# Model 2: GBM (reduced complexity for speed)
message("Training Model 2: GBM (simplified)...")
model_gbm <- gbm(
  home_win ~ .,
  data = train_data %>% select(home_win, all_of(predictors)) %>%
    mutate(home_win = as.numeric(home_win) - 1),
  distribution = "bernoulli",
  n.trees = 300,  # Reduced from 500
  interaction.depth = 3,  # Reduced from 4
  shrinkage = 0.01,
  cv.folds = 3,  # Reduced from 5
  n.cores = 1
)

best_iter <- gbm.perf(model_gbm, method = "cv", plot.it = FALSE)
pred_gbm_train <- predict(model_gbm, train_data, n.trees = best_iter, type = "response")
pred_gbm_test <- predict(model_gbm, test_data, n.trees = best_iter, type = "response")

# SKIP Random Forest and XGBoost for speed (they're slowest)
message("Skipping Random Forest and XGBoost for speed...")

# Evaluation
evaluate_model <- function(pred_probs, actual, model_name, dataset) {
  actual_numeric <- as.numeric(actual) - 1
  roc_obj <- roc(actual_numeric, pred_probs, quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))
  brier <- mean((pred_probs - actual_numeric)^2)
  log_loss <- -mean(actual_numeric * log(pred_probs + 1e-15) + 
                      (1 - actual_numeric) * log(1 - pred_probs + 1e-15))
  tibble(Model = model_name, Dataset = dataset, AUC = auc_val, 
         Brier_Score = brier, Log_Loss = log_loss)
}

results <- bind_rows(
  evaluate_model(pred_logit_train, train_data$home_win, "Logistic", "Train"),
  evaluate_model(pred_logit_test, test_data$home_win, "Logistic", "Test"),
  evaluate_model(pred_gbm_train, train_data$home_win, "GBM", "Train"),
  evaluate_model(pred_gbm_test, test_data$home_win, "GBM", "Test")
)

print(results)

best_model_name <- results %>%
  filter(Dataset == "Test") %>%
  arrange(desc(AUC)) %>%
  dplyr::slice(1) %>%
  pull(Model)

message(paste("Best model:", best_model_name))

# Save models AND the sampled games list
model_list <- list(
  logistic = model_logit,
  gbm = model_gbm,
  random_forest = NULL,  # Skipped
  xgboost = NULL,  # Skipped
  best_model_name = best_model_name,
  evaluation_results = results,
  predictors = predictors,
  sampled_games = unique(wp_features$game_id)  # Save which games were used
)

dir.create("data/interim", showWarnings = FALSE, recursive = TRUE)
saveRDS(model_list, "data/interim/wp_models.rds")
write_csv(results, "tables/wp_model_evaluation.csv")

message("✓ WP models complete!")
message(paste("Best:", best_model_name, "| AUC =", 
              round(results %>% filter(Model == best_model_name, Dataset == "Test") %>% pull(AUC), 4)))
