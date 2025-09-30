# Build multiple win probability models and select the best one
# Models will predict home team win probability based on game state

source("R/utils.R")

library(tidyverse)
library(caret)
library(gbm)
library(randomForest)
library(xgboost)
library(pROC)

message("Building win probability models...")

# Load cleaned play-by-play data
pbp_data <- readRDS("data/raw/pbp_clean.rds")

# Feature engineering for WP model
wp_features <- pbp_data %>%
  mutate(
    # Core features
    score_diff = home_score - away_score,
    time_remaining_min = secs_remaining / 60,
    time_elapsed_min = (half - 1) * 20 + (20 - time_remaining_min),
    
    # Interaction features
    score_diff_x_time = score_diff * time_remaining_min,
    score_diff_sq = score_diff^2,
    
    # Possession indicator (approximation) - only if column exists
    has_ball = if("possession" %in% names(.)) {
      ifelse(possession == "home", 1, 0)
    } else {
      0.5  # Unknown possession = 50/50
    },
    
    # Period indicators
    is_first_half = ifelse(half == 1, 1, 0),
    is_second_half = ifelse(half == 2, 1, 0),
    
    # Critical time indicators
    is_clutch = ifelse(time_remaining_min < 5 & abs(score_diff) < 10, 1, 0),
    
    # Outcome
    home_win = as.factor(home_win)
  ) %>%
  filter(!is.na(home_win), !is.na(score_diff), !is.na(time_remaining_min)) %>%
  select(game_id, home_win, score_diff, time_remaining_min, time_elapsed_min,
         score_diff_x_time, score_diff_sq, has_ball, is_first_half, 
         is_second_half, is_clutch)

message(paste("Prepared", nrow(wp_features), "observations for modeling"))

# Split data: 80% train, 20% test
set.seed(479)
train_games <- sample(unique(wp_features$game_id), 
                      size = floor(0.8 * n_distinct(wp_features$game_id)))

train_data <- wp_features %>% filter(game_id %in% train_games)
test_data <- wp_features %>% filter(!game_id %in% train_games)

message(paste("Training set:", nrow(train_data), "observations"))
message(paste("Test set:", nrow(test_data), "observations"))

# Define predictors
predictors <- c("score_diff", "time_remaining_min", "time_elapsed_min",
                "score_diff_x_time", "score_diff_sq", "has_ball", 
                "is_first_half", "is_second_half", "is_clutch")

# Model 1: Logistic Regression (baseline)
message("Training Model 1: Logistic Regression...")
model_logit <- glm(
  home_win ~ score_diff + time_remaining_min + score_diff_x_time + 
    score_diff_sq + has_ball + is_first_half + is_clutch,
  data = train_data,
  family = binomial(link = "logit")
)

pred_logit_train <- predict(model_logit, train_data, type = "response")
pred_logit_test <- predict(model_logit, test_data, type = "response")

# Model 2: Gradient Boosting Machine
message("Training Model 2: Gradient Boosting Machine...")
model_gbm <- gbm(
  home_win ~ .,
  data = train_data %>% select(home_win, all_of(predictors)) %>%
    mutate(home_win = as.numeric(home_win) - 1),
  distribution = "bernoulli",
  n.trees = 500,
  interaction.depth = 4,
  shrinkage = 0.01,
  cv.folds = 5,
  n.cores = 1
)

best_iter <- gbm.perf(model_gbm, method = "cv", plot.it = FALSE)
pred_gbm_train <- predict(model_gbm, train_data, n.trees = best_iter, type = "response")
pred_gbm_test <- predict(model_gbm, test_data, n.trees = best_iter, type = "response")

# Model 3: Random Forest
message("Training Model 3: Random Forest...")
model_rf <- randomForest(
  home_win ~ .,
  data = train_data %>% select(home_win, all_of(predictors)),
  ntree = 300,
  mtry = 3,
  importance = TRUE
)

pred_rf_train <- predict(model_rf, train_data, type = "prob")[, 2]
pred_rf_test <- predict(model_rf, test_data, type = "prob")[, 2]

# Model 4: XGBoost
message("Training Model 4: XGBoost...")
train_matrix <- xgb.DMatrix(
  data = as.matrix(train_data %>% select(all_of(predictors))),
  label = as.numeric(train_data$home_win) - 1
)
test_matrix <- xgb.DMatrix(
  data = as.matrix(test_data %>% select(all_of(predictors))),
  label = as.numeric(test_data$home_win) - 1
)

model_xgb <- xgb.train(
  data = train_matrix,
  nrounds = 300,
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)

pred_xgb_train <- predict(model_xgb, train_matrix)
pred_xgb_test <- predict(model_xgb, test_matrix)

# Evaluate models using ROC AUC and Brier Score
evaluate_model <- function(pred_probs, actual, model_name, dataset) {
  actual_numeric <- as.numeric(actual) - 1
  
  # ROC AUC
  roc_obj <- roc(actual_numeric, pred_probs, quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))  # Convert to numeric
  
  # Brier Score
  brier <- mean((pred_probs - actual_numeric)^2)
  
  # Log Loss
  log_loss <- -mean(actual_numeric * log(pred_probs + 1e-15) + 
                      (1 - actual_numeric) * log(1 - pred_probs + 1e-15))
  
  tibble(
    Model = model_name,
    Dataset = dataset,
    AUC = auc_val,
    Brier_Score = brier,
    Log_Loss = log_loss
  )
}

# Evaluate all models
results <- bind_rows(
  evaluate_model(pred_logit_train, train_data$home_win, "Logistic", "Train"),
  evaluate_model(pred_logit_test, test_data$home_win, "Logistic", "Test"),
  evaluate_model(pred_gbm_train, train_data$home_win, "GBM", "Train"),
  evaluate_model(pred_gbm_test, test_data$home_win, "GBM", "Test"),
  evaluate_model(pred_rf_train, train_data$home_win, "Random Forest", "Train"),
  evaluate_model(pred_rf_test, test_data$home_win, "Random Forest", "Test"),
  evaluate_model(pred_xgb_train, train_data$home_win, "XGBoost", "Train"),
  evaluate_model(pred_xgb_test, test_data$home_win, "XGBoost", "Test")
)

print(results)

# Select best model based on test AUC
best_model_name <- results %>%
  filter(Dataset == "Test") %>%
  arrange(desc(AUC)) %>%
  dplyr::slice(1) %>%  # Explicitly use dplyr::slice to avoid conflict with xgboost
  pull(Model)

message(paste("Best model:", best_model_name))

# Save all models and select best
model_list <- list(
  logistic = model_logit,
  gbm = model_gbm,
  random_forest = model_rf,
  xgboost = model_xgb,
  best_model_name = best_model_name,
  evaluation_results = results,
  predictors = predictors
)

saveRDS(model_list, "data/interim/wp_models.rds")

# Save the best model predictions for verification
best_predictions <- switch(
  best_model_name,
  "Logistic" = list(train = pred_logit_test, test = pred_logit_test),
  "GBM" = list(train = pred_gbm_test, test = pred_gbm_test),
  "Random Forest" = list(train = pred_rf_test, test = pred_rf_test),
  "XGBoost" = list(train = pred_xgb_test, test = pred_xgb_test)
)

# Save evaluation results
write_csv(results, "tables/wp_model_evaluation.csv")

message("Win probability model training complete.")
message(paste("Best model:", best_model_name, 
              "with test AUC =", 
              round(results %>% 
                      filter(Model == best_model_name, Dataset == "Test") %>% 
                      pull(AUC), 4)))