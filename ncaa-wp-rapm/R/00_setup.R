# Setup script: Install and load required packages
# Create necessary directories

library(tidyverse)
library(ncaahoopR)
library(glmnet)
library(Matrix)

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
  "tidyverse",      
  "glmnet",         
  "caret",          
  "Matrix",         
  "gbm",            
  "xgboost",        
  "randomForest",   
  "pROC",           
  "knitr",          
  "kableExtra",     
  "ggplot2",        
  "cowplot",        
  "viridis"         
)

# Function to install if missing
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  } else {
    message(paste(pkg, "already installed"))
  }
}

# Install missing packages
for (pkg in required_packages) {
  install_if_missing(pkg)
}

# Install devtools if needed - Github packages
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install ncaahoopR from GitHub
if (!requireNamespace("ncaahoopR", quietly = TRUE)) {
  devtools::install_github("lbenz730/ncaahoopR", force = FALSE)
} else {
  message("ncaahoopR already installed")
}


message("Setup complete. All required packages installed.")