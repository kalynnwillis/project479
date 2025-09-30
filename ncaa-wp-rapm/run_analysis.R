# Main script to run the complete NCAA RAPM analysis pipeline
# Run this script to execute the entire analysis from start to finish

# Set CRAN mirror first
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Set working directory to the project root
if (basename(getwd()) != "ncaa-wp-rapm") {
  if (dir.exists("ncaa-wp-rapm")) {
    setwd("ncaa-wp-rapm")
  } else {
    stop("Please run this script from the project root or ncaa-wp-rapm directory")
  }
}

cat("=====================================\n")
cat("NCAA Basketball RAPM Analysis\n")
cat("Regularized Adjusted Plus-Minus on Win Probability Scale\n")
cat("=====================================\n\n")

# Track timing
start_time <- Sys.time()

# Step 0: Setup
cat("Step 0: Setting up environment...\n")
source("R/00_setup.R")
cat("✓ Setup complete\n\n")

# Step 1: Get data
cat("Step 1: Acquiring NCAA basketball data...\n")
cat("Note: This step can take several hours for a full season.\n")
cat("Consider using cached data if available.\n")
tryCatch({
  source("R/01_get_data.R")
  cat("✓ Data acquisition complete\n\n")
}, error = function(e) {
  cat("Error in data acquisition:", e$message, "\n")
  cat("Continuing with existing data if available...\n\n")
})

# Step 2: Build win probability model
cat("Step 2: Building win probability models...\n")
tryCatch({
  source("R/02_build_wp_model.R")
  cat("✓ Win probability model complete\n\n")
}, error = function(e) {
  cat("Error in WP model building:", e$message, "\n")
  cat("Analysis cannot continue without WP model.\n")
  stop(e)
})

# Step 3: Build shifts
cat("Step 3: Building player shifts...\n")
tryCatch({
  source("R/03_build_shifts.R")
  cat("✓ Shift building complete\n\n")
}, error = function(e) {
  cat("Error in shift building:", e$message, "\n")
  cat("Analysis cannot continue without shifts.\n")
  stop(e)
})

# Step 4: Fit RAPM
cat("Step 4: Fitting RAPM models...\n")
cat("This step uses regularized regression to estimate player effects.\n")
tryCatch({
  source("R/04_fit_rapm.R")
  cat("✓ RAPM fitting complete\n\n")
}, error = function(e) {
  cat("Error in RAPM fitting:", e$message, "\n")
  cat("Analysis cannot continue without RAPM results.\n")
  stop(e)
})

# Step 5: Generate evaluation plots
cat("Step 5: Generating evaluation plots and tables...\n")
tryCatch({
  source("R/05_eval_plots.R")
  cat("✓ Evaluation complete\n\n")
}, error = function(e) {
  cat("Error in evaluation:", e$message, "\n")
  cat("Continuing anyway...\n\n")
})

# Summary
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")

cat("=====================================\n")
cat("Analysis Complete!\n")
cat("=====================================\n")
cat(sprintf("Total time: %.2f minutes\n\n", as.numeric(elapsed)))

cat("Output locations:\n")
cat("- Data: data/raw/, data/interim/, data/processed/\n")
cat("- Figures: figs/\n")
cat("- Tables: tables/\n\n")

cat("Key outputs:\n")
cat("- RAPM rankings: tables/rapm_rankings.csv\n")
cat("- Top/bottom players: tables/top_bottom_players.csv\n")
cat("- WP model evaluation: tables/wp_model_evaluation.csv\n")
cat("- Visualizations: figs/*.png\n\n")

cat("To view results:\n")
cat("- Open tables/rapm_rankings.csv for player rankings\n")
cat("- Check figs/ folder for all visualizations\n")
cat("- Review report.qmd or slides.qmd for analysis writeup\n\n")

# Load and display top 10 players
if (file.exists("data/processed/rapm_results.rds")) {
  rapm_data <- readRDS("data/processed/rapm_results.rds")
  top10 <- rapm_data$rapm_table %>%
    dplyr::filter(!is.na(games_played), games_played >= 10) %>%
    dplyr::arrange(desc(ridge_rapm)) %>%
    dplyr::select(player, ridge_rapm, games_played, avg_minutes) %>%
    head(10)
  
  cat("Top 10 Players (Ridge RAPM):\n")
  print(top10)
}

cat("\nDone!\n")
