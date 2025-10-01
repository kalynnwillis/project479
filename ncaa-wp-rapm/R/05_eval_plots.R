# Evaluation plots and tables for RAPM results

source("R/utils.R")

library(tidyverse)
library(ggplot2)
library(viridis)
library(cowplot)
library(pROC)

message("Generating evaluation plots...")

# Load results
rapm_results <- readRDS("data/processed/rapm_results.rds")
wp_models <- readRDS("data/interim/wp_models.rds")

# Extract RAPM table
rapm_table <- rapm_results$rapm_table

# Plot 1: Top 30 Players by RAPM
top_players <- rapm_table %>%
  filter(!is.na(games_played), games_played >= 10) %>%
  arrange(desc(ridge_rapm)) %>%
  head(30)

p1 <- ggplot(top_players, aes(x = reorder(player, ridge_rapm), y = ridge_rapm)) +
  geom_col(aes(fill = ridge_rapm), show.legend = FALSE) +
  scale_fill_viridis(option = "plasma") +
  coord_flip() +
  labs(
    title = "Top 30 Players by RAPM (Ridge)",
    subtitle = "Regularized Adjusted Plus-Minus on Win Probability Scale",
    x = "Player",
    y = "RAPM (WP Change per Possession)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 8)
  )

ggsave("figs/top_players_rapm.png", p1, width = 10, height = 8, dpi = 300)
message("Saved: figs/top_players_rapm.png")

# Plot 2: RAPM Distribution
p2 <- ggplot(rapm_table %>% filter(!is.na(games_played)), 
             aes(x = ridge_rapm)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Distribution of RAPM Values",
    x = "RAPM (Ridge)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("figs/rapm_distribution.png", p2, width = 8, height = 6, dpi = 300)
message("Saved: figs/rapm_distribution.png")

# Plot 3: RAPM vs Minutes Played
p3 <- ggplot(rapm_table %>% filter(!is.na(games_played), games_played >= 5), 
             aes(x = avg_minutes, y = ridge_rapm)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 2) +
  geom_smooth(method = "loess", color = "red", se = TRUE, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "RAPM vs. Average Minutes Played",
    subtitle = paste0("Range: ", 
                      round(min(rapm_table$ridge_rapm, na.rm=TRUE), 5), " to ",
                      round(max(rapm_table$ridge_rapm, na.rm=TRUE), 5)),
    x = "Average Minutes per Game",
    y = "RAPM (Ridge) - Win Probability Impact"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("figs/rapm_vs_minutes.png", p3, width = 8, height = 6, dpi = 300)
message("Saved: figs/rapm_vs_minutes.png")

# Plot 4: Ridge vs Lasso vs Elastic Net comparison
# Check if Lasso has variation
lasso_var <- var(rapm_table$lasso_rapm, na.rm = TRUE)
lasso_max <- max(abs(rapm_table$lasso_rapm), na.rm = TRUE)

if (lasso_var < 1e-10 || lasso_max < 1e-10) {
  # Lasso shrunk everything to zero - create informative plot
  p4 <- ggplot(rapm_table %>% filter(!is.na(games_played)) %>% head(200), 
               aes(x = ridge_rapm, y = lasso_rapm)) +
    geom_point(alpha = 0.6, color = "steelblue", size = 2) +
    geom_hline(yintercept = 0, linetype = "solid", color = "red", linewidth = 1) +
    annotate("text", x = mean(rapm_table$ridge_rapm, na.rm=TRUE), 
             y = max(rapm_table$lasso_rapm, na.rm=TRUE) * 0.5,
             label = "Lasso shrunk all coefficients to ~0\n(Strong regularization)",
             color = "red", size = 4, fontface = "italic") +
    labs(
      title = "Ridge RAPM vs. Lasso RAPM",
      subtitle = "Lasso regularization too strong for noisy player-shift data",
      x = "Ridge RAPM (varying)",
      y = "Lasso RAPM (all ≈ 0)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
} else {
  # Normal Ridge vs Lasso plot
  p4 <- ggplot(rapm_table %>% filter(!is.na(games_played)), 
               aes(x = ridge_rapm, y = lasso_rapm)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Ridge RAPM vs. Lasso RAPM",
      x = "Ridge RAPM",
      y = "Lasso RAPM"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
}

ggsave("figs/ridge_vs_lasso.png", p4, width = 8, height = 6, dpi = 300)
message("Saved: figs/ridge_vs_lasso.png")

# Plot 5: Cross-validation curves
tryCatch({
  png("figs/cv_lambda_selection.png", width = 12, height = 4, units = "in", res = 300)
  par(mfrow = c(1, 3))
  plot(rapm_results$cv_ridge, main = "Ridge CV")
  plot(rapm_results$cv_lasso, main = "Lasso CV")
  plot(rapm_results$cv_elastic, main = "Elastic Net CV")
  dev.off()
  message("Saved: figs/cv_lambda_selection.png")
}, error = function(e) {
  dev.off()  # Close any open graphics device
  message("Note: CV plots skipped (not critical for analysis)")
})

# Plot 6: Win Probability Model Performance
# Load test predictions and compare to actual outcomes
if (!is.null(wp_models$evaluation_results)) {
  p6 <- ggplot(wp_models$evaluation_results, 
               aes(x = Model, y = AUC, fill = Dataset)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(aes(label = round(AUC, 3)), 
              position = position_dodge(width = 0.7),
              vjust = -0.5, size = 3) +
    scale_fill_manual(values = c("Train" = "lightblue", "Test" = "darkblue")) +
    labs(
      title = "Win Probability Model Performance",
      subtitle = "Area Under ROC Curve (AUC) - Higher is Better",
      x = "Model",
      y = "AUC",
      fill = "Dataset"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    ) +
    coord_cartesian(ylim = c(0.8, 1.0))  # Zoom to data range
  
  ggsave("figs/wp_model_performance.png", p6, width = 10, height = 6, dpi = 300)
  message("Saved: figs/wp_model_performance.png")
}

# Create summary table
summary_stats <- rapm_table %>%
  filter(!is.na(games_played), games_played >= 10) %>%
  summarise(
    n_players = n(),
    mean_rapm = mean(ridge_rapm, na.rm = TRUE),
    median_rapm = median(ridge_rapm, na.rm = TRUE),
    sd_rapm = sd(ridge_rapm, na.rm = TRUE),
    min_rapm = min(ridge_rapm, na.rm = TRUE),
    max_rapm = max(ridge_rapm, na.rm = TRUE)
  )

write_csv(summary_stats, "tables/rapm_summary_stats.csv")
message("Saved: tables/rapm_summary_stats.csv")

# Create top/bottom players table
top_bottom <- bind_rows(
  rapm_table %>%
    filter(!is.na(games_played), games_played >= 10) %>%
    arrange(desc(ridge_rapm)) %>%
    head(25) %>%
    mutate(rank_type = "Top 25"),
  rapm_table %>%
    filter(!is.na(games_played), games_played >= 10) %>%
    arrange(ridge_rapm) %>%
    head(25) %>%
    mutate(rank_type = "Bottom 25")
) %>%
  select(rank_type, player, ridge_rapm, lasso_rapm, elastic_rapm, 
         games_played, avg_minutes, ridge_percentile)

write_csv(top_bottom, "tables/top_bottom_players.csv")
message("Saved: tables/top_bottom_players.csv")

# Plot 7: Regularization path
tryCatch({
  png("figs/regularization_path.png", width = 10, height = 6, units = "in", res = 300)
  plot(rapm_results$ridge_model, xvar = "lambda", label = TRUE, 
       main = "Ridge Regularization Path")
  abline(v = log(rapm_results$cv_ridge$lambda.min), lty = 2, col = "red")
  dev.off()
  message("Saved: figs/regularization_path.png")
}, error = function(e) {
  dev.off()  # Close any open graphics device
  message("Note: Regularization path plot skipped (not critical for analysis)")
})

# Combined plot for paper/presentation
combined_plot <- plot_grid(
  p1 + theme(legend.position = "none"),
  p2,
  p3,
  p4,
  ncol = 2,
  labels = c("A", "B", "C", "D")
)

ggsave("figs/combined_analysis.png", combined_plot, width = 14, height = 10, dpi = 300)
message("Saved: figs/combined_analysis.png")

message("\nEvaluation complete. All plots and tables saved.")
message("Check the 'figs/' and 'tables/' directories for outputs.")