# Setup script: Install and load required packages
# Create necessary directories

# Set CRAN mirror first
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Create directories
required_dirs <- c(
  "data/raw",
  "data/interim",
  "data/processed",
  "figs",
  "tables"
)

for (d in required_dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Required packages (excluding ncaahoopR which comes from GitHub)
required_packages <- c(
  "tidyverse",      # Data manipulation and visualization
  "glmnet",         # Regularized regression for RAPM
  "caret",          # Model training and evaluation
  "Matrix",         # Sparse matrices for efficiency
  "gbm",            # Gradient boosting for WP model
  "xgboost",        # Alternative boosting for WP model
  "randomForest",   # Random forest for WP model
  "pROC",           # ROC curves for model evaluation
  "knitr",          # Tables
  "kableExtra",     # Better tables
  "ggplot2",        # Plotting
  "cowplot",        # Plot arrangements
  "viridis"         # Color scales
)

# Function to install if missing
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing", pkg, "..."))
    install.packages(pkg)
  } else {
    message(paste(pkg, "already installed"))
  }
}

# Install missing packages
message("Checking and installing required packages from CRAN...")
for (pkg in required_packages) {
  install_if_missing(pkg)
}

# Install devtools if needed (for GitHub packages)
if (!requireNamespace("devtools", quietly = TRUE)) {
  message("Installing devtools...")
  install.packages("devtools")
}

# Install ncaahoopR from GitHub
message("\nInstalling ncaahoopR from GitHub...")
if (!requireNamespace("ncaahoopR", quietly = TRUE)) {
  message("Installing ncaahoopR from lbenz730/ncaahoopR...")
  devtools::install_github("lbenz730/ncaahoopR", force = FALSE)
} else {
  message("ncaahoopR already installed")
}

# Load key packages
library(tidyverse)
library(ncaahoopR)
library(glmnet)
library(Matrix)

message("Setup complete. All required packages installed.")