# Build win probability models - RANDOM FOREST OPTIMIZED VERSION
source("R/utils.R")

library(tidyverse)
library(caret)
library(gbm)
library(ranger)
library(xgboost)
library(pROC)

message("Building win probability models with OPTIMIZED Random Forest...")

# Load cleaned play-by-play data
pbp_data <- readRDS("data/raw/pbp_clean.rds")

# STRATEGY 1: Smart sampling (maintain game structure)
set.seed(479)
SAMPLE_SIZE <- 150000

if (nrow(pbp_data) > SAMPLE_SIZE) {
  message(paste("Sampling", SAMPLE_SIZE, "plays for training..."))
  sample_games <- sample(unique(pbp_data$game_id),
    size = ceiling(SAMPLE_SIZE / (nrow(pbp_data) / n_distinct(pbp_data$game_id)))
  )
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
    has_ball = if ("possession" %in% names(.)) {
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
  select(
    game_id, home_win, score_diff, time_remaining_min, time_elapsed_min,
    score_diff_x_time, score_diff_sq, has_ball, is_first_half,
    is_second_half, is_clutch
  )


# Split data
set.seed(479)
train_games <- sample(unique(wp_features$game_id),
  size = floor(0.8 * n_distinct(wp_features$game_id))
)

train_data <- wp_features %>% filter(game_id %in% train_games)
test_data <- wp_features %>% filter(!game_id %in% train_games)


predictors <- c(
  "score_diff", "time_remaining_min", "time_elapsed_min",
  "score_diff_x_time", "score_diff_sq", "has_ball",
  "is_first_half", "is_second_half", "is_clutch"
)

# Model 1: Logistic Regression
model_logit <- glm(
  home_win ~ score_diff + time_remaining_min + score_diff_x_time +
    score_diff_sq + has_ball + is_first_half + is_clutch,
  data = train_data,
  family = binomial(link = "logit")
)

pred_logit_train <- predict(model_logit, train_data, type = "response")
pred_logit_test <- predict(model_logit, test_data, type = "response")

# Model 2: GBM
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


# Model 3: Random Forest (ranger)
# Prepare data for ranger
train_rf <- train_data %>%
  select(home_win, all_of(predictors))

test_rf <- test_data %>%
  select(home_win, all_of(predictors))

# Train with ranger (much more memory efficient than randomForest)
model_rf <- ranger(
  home_win ~ .,
  data = train_rf,
  num.trees = 300,
  max.depth = 10,
  min.node.size = 100,
  mtry = 3, # Number of variables to try at each split
  probability = TRUE,
  num.threads = 1,
  verbose = TRUE,
  write.forest = TRUE,
  replace = TRUE,
  sample.fraction = 0.7
)

pred_rf_train <- predict(model_rf, train_rf)$predictions[, 2]
pred_rf_test <- predict(model_rf, test_rf)$predictions[, 2]



# Model 4: XGBoost

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
    max_depth = 4,
    eta = 0.1,
    subsample = 0.7,
    colsample_bytree = 0.7
  ),
  data = train_matrix,
  nrounds = 200,
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
  brier <- brier_score(pred_probs, actual_numeric)
  ll <- log_loss(pred_probs, actual_numeric)

  # Perfect calibration: intercept ≈ 0, slope ≈ 1
  calib_data <- data.frame(pred = pred_probs, actual = actual_numeric)
  calib_data$pred <- pmax(pmin(calib_data$pred, 0.9999), 0.0001)
  calib_model <- glm(actual ~ qlogis(pred), data = calib_data, family = binomial())
  calib_intercept <- coef(calib_model)[1]
  calib_slope <- coef(calib_model)[2]

  tibble(
    Model = model_name,
    Dataset = dataset,
    AUC = auc_val,
    Brier_Score = brier,
    Log_Loss = ll,
    Calib_Intercept = calib_intercept,
    Calib_Slope = calib_slope
  )
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


test_results <- results %>%
  filter(Dataset == "Test") %>%
  mutate(
    calib_error = abs(Calib_Intercept) + abs(Calib_Slope - 1),
    is_well_calibrated = (abs(Calib_Intercept) < 0.5 & abs(Calib_Slope - 1) < 0.3)
  )
print(test_results %>% select(Model, AUC, Brier_Score, Calib_Intercept, Calib_Slope, is_well_calibrated))

best_model_name <- test_results %>%
  arrange(Brier_Score, calib_error) %>%
  dplyr::slice(1) %>%
  pull(Model)

# Create calibration data for all models on test set
calibration_data <- tibble(
  actual = as.numeric(test_data$home_win) - 1,
  Logistic = pred_logit_test,
  GBM = pred_gbm_test,
  `Random Forest` = pred_rf_test,
  XGBoost = pred_xgb_test
)

# Function to compute calibration bins
compute_calibration_bins <- function(pred_probs, actual, n_bins = 10) {
  # Create bins
  breaks <- seq(0, 1, length.out = n_bins + 1)
  bin_ids <- cut(pred_probs, breaks = breaks, include.lowest = TRUE, labels = FALSE)

  # Compute observed vs expected in each bin
  calib_df <- tibble(
    bin = bin_ids,
    pred = pred_probs,
    actual = actual
  ) %>%
    filter(!is.na(bin)) %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      mean_pred = mean(pred),
      mean_actual = mean(actual),
      .groups = "drop"
    ) %>%
    mutate(bin_center = (bin - 0.5) / n_bins)

  return(calib_df)
}

# Compute calibration for each model
calibration_curves <- list(
  Logistic = compute_calibration_bins(calibration_data$Logistic, calibration_data$actual),
  GBM = compute_calibration_bins(calibration_data$GBM, calibration_data$actual),
  `Random Forest` = compute_calibration_bins(calibration_data$`Random Forest`, calibration_data$actual),
  XGBoost = compute_calibration_bins(calibration_data$XGBoost, calibration_data$actual)
)

print(results %>%
  filter(Dataset == "Test") %>%
  select(Model, Calib_Intercept, Calib_Slope, AUC, Brier_Score))

# Save all models (directories created by 00_setup.R)
model_list <- list(
  logistic = model_logit,
  gbm = model_gbm,
  random_forest = model_rf,
  xgboost = model_xgb,
  best_model_name = best_model_name,
  evaluation_results = results,
  calibration_curves = calibration_curves,
  calibration_data = calibration_data,
  predictors = predictors,
  sampled_games = unique(wp_features$game_id)
)

saveRDS(model_list, "data/interim/wp_models.rds")
write_csv(results, "tables/wp_model_evaluation.csv")

message("✓ ALL models complete (including Random Forest)!")
message(paste(
  "Best:", best_model_name, "| AUC =",
  round(results %>% filter(Model == best_model_name, Dataset == "Test") %>% pull(AUC), 4)
))
message("✓ Calibration analysis complete!")
message(paste(
  "Best model calibration slope:",
  round(results %>% filter(Model == best_model_name, Dataset == "Test") %>% pull(Calib_Slope), 3)
))
