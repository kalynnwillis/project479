# Utility functions for NCAA RAPM analysis

library(tidyverse)

# Function to calculate Brier score
brier_score <- function(predicted_prob, actual_outcome) {
  mean((predicted_prob - actual_outcome)^2)
}

# Function to calculate log loss
log_loss <- function(predicted_prob, actual_outcome, eps = 1e-15) {
  predicted_prob <- pmax(pmin(predicted_prob, 1 - eps), eps)
  -mean(actual_outcome * log(predicted_prob) + (1 - actual_outcome) * log(1 - predicted_prob))
}

message("Utility functions loaded.")
