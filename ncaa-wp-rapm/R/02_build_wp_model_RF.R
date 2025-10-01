# Build win probability models - RANDOM FOREST OPTIMIZED VERSION
# Multiple strategies to handle large datasets without memory errors

source("R/utils.R")

library(tidyverse)
library(caret)
library(gbm)
library(ranger)  # Memory-efficient alternative to randomForest
library(xgboost)
library(pROC)

message("Building win probability models with OPTIMIZED Random Forest...")

# Load cleaned play-by-play data
pbp_data <- readRDS("data/raw/pbp_clean.rds")
message(paste("Total data:", nrow(pbp_data), "plays"))

# STRATEGY 1: Smart sampling (maintain game structure)
set.seed(479)
SAMPLE_SIZE <- 150000  # Larger sample than FAST version

if (nrow(pbp_data) > SAMPLE_SIZE) {
  message(paste("Sampling", SAMPLE_SIZE, "plays for training..."))
  sample_games <- sample(unique(pbp_data$game_id), 
                        size = ceiling(SAMPLE_SIZE / (nrow(pbp_data)/n_distinct(pbp_data$game_id))))
  pbp_data <- pbp_data %>% filter(game_id %in% sample_games)
  message(paste("Sampled data:", nrow(pbp_data), "plays from", length(sample_games), "games"))
}

# Feature engineering
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

# STRATEGY 2: Increase R memory limit (if on Windows/macOS with limits)
if (.Platform$OS.type == "windows") {
  memory.limit(size = 32000)  # 32GB
}

# Try to increase vector memory limit
options(expressions = 500000)

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

# Model 2: GBM (lighter weight)
message("Training Model 2: GBM...")
model_gbm <- gbm(
  home_win ~ .,
  data = train_data %>% select(home_win, all_of(predictors)) %>%
    mutate(home_win = as.numeric(home_win) - 1),
  distribution = "bernoulli",
  n.trees = 300,
  interaction.depth = 3,
  shrinkage = 0.01,
  cv.folds = 3,
  n.cores = 1
)

best_iter <- gbm.perf(model_gbm, method = "cv", plot.it = FALSE)
pred_gbm_train <- predict(model_gbm, train_data, n.trees = best_iter, type = "response")
pred_gbm_test <- predict(model_gbm, test_data, n.trees = best_iter, type = "response")

# STRATEGY 3A: Use ranger (memory-efficient Random Forest)
message("Training Model 3: Random Forest (ranger - memory optimized)...")

# Prepare data for ranger
train_rf <- train_data %>% 
  select(home_win, all_of(predictors))

test_rf <- test_data %>% 
  select(home_win, all_of(predictors))

# Train with ranger (much more memory efficient than randomForest)
model_rf <- ranger(
  home_win ~ .,
  data = train_rf,
  num.trees = 300,           # Reduced from typical 500
  max.depth = 10,            # Limit tree depth
  min.node.size = 100,       # Larger minimum node size
  mtry = 3,                  # Number of variables to try at each split
  probability = TRUE,        # For probability predictions
  num.threads = 1,           # Single thread to control memory
  verbose = TRUE,
  write.forest = TRUE,       # Keep the forest for predictions
  replace = TRUE,            # Bootstrap sampling
  sample.fraction = 0.7      # Use 70% of data per tree (saves memory)
)

pred_rf_train <- predict(model_rf, train_rf)$predictions[, 2]
pred_rf_test <- predict(model_rf, test_rf)$predictions[, 2]

message("✓ Random Forest complete!")

# STRATEGY 3B: Batch Random Forest (if ranger still fails)
# This approach trains multiple smaller forests and averages predictions
batch_random_forest <- function(train_data, test_data, n_batches = 5, trees_per_batch = 100) {
  message(paste("Training", n_batches, "Random Forest batches with", trees_per_batch, "trees each..."))
  
  all_train_preds <- matrix(0, nrow = nrow(train_data), ncol = n_batches)
  all_test_preds <- matrix(0, nrow = nrow(test_data), ncol = n_batches)
  
  for (i in 1:n_batches) {
    message(paste("  Batch", i, "of", n_batches))
    
    # Sample subset for this batch
    batch_idx <- sample(1:nrow(train_data), size = floor(nrow(train_data) * 0.6))
    batch_data <- train_data[batch_idx, ]
    
    # Train small forest
    rf_batch <- ranger(
      home_win ~ .,
      data = batch_data %>% select(home_win, all_of(predictors)),
      num.trees = trees_per_batch,
      max.depth = 8,
      min.node.size = 50,
      probability = TRUE,
      num.threads = 1,
      verbose = FALSE
    )
    
    # Predict
    all_train_preds[, i] <- predict(rf_batch, train_data %>% select(all_of(predictors)))$predictions[, 2]
    all_test_preds[, i] <- predict(rf_batch, test_data %>% select(all_of(predictors)))$predictions[, 2]
    
    # Clean up
    rm(rf_batch, batch_data)
    gc()
  }
  
  # Average predictions across batches
  list(
    train = rowMeans(all_train_preds),
    test = rowMeans(all_test_preds)
  )
}

# Model 4: XGBoost (memory efficient with sparse matrices)
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
  params = list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 4,          # Reduced depth
    eta = 0.1,              # Learning rate
    subsample = 0.7,        # Use 70% of data per tree
    colsample_bytree = 0.7  # Use 70% of features per tree
  ),
  data = train_matrix,
  nrounds = 200,            # Reduced rounds
  verbose = 0,
  early_stopping_rounds = 20,
  watchlist = list(test = test_matrix)
)

pred_xgb_train <- predict(model_xgb, train_matrix)
pred_xgb_test <- predict(model_xgb, test_matrix)

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
  evaluate_model(pred_gbm_test, test_data$home_win, "GBM", "Test"),
  evaluate_model(pred_rf_train, train_data$home_win, "Random Forest", "Train"),
  evaluate_model(pred_rf_test, test_data$home_win, "Random Forest", "Test"),
  evaluate_model(pred_xgb_train, train_data$home_win, "XGBoost", "Train"),
  evaluate_model(pred_xgb_test, test_data$home_win, "XGBoost", "Test")
)

print(results)

best_model_name <- results %>%
  filter(Dataset == "Test") %>%
  arrange(desc(AUC)) %>%
  dplyr::slice(1) %>%
  pull(Model)

message(paste("Best model:", best_model_name))

# Save all models
model_list <- list(
  logistic = model_logit,
  gbm = model_gbm,
  random_forest = model_rf,
  xgboost = model_xgb,
  best_model_name = best_model_name,
  evaluation_results = results,
  predictors = predictors,
  sampled_games = unique(wp_features$game_id)
)

dir.create("data/interim", showWarnings = FALSE, recursive = TRUE)
saveRDS(model_list, "data/interim/wp_models.rds")
write_csv(results, "tables/wp_model_evaluation.csv")

message("✓ ALL models complete (including Random Forest)!")
message(paste("Best:", best_model_name, "| AUC =", 
              round(results %>% filter(Model == best_model_name, Dataset == "Test") %>% pull(AUC), 4)))

