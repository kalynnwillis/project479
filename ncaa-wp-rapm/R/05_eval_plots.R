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

# ============================================================================
# WP MODEL CALIBRATION PLOTS (CRITICAL!)
# ============================================================================
message("\n=== Creating WP Model Calibration Plots ===")

if (!is.null(wp_models$calibration_curves)) {
  # Create calibration plot for all models
  calib_plot_data <- bind_rows(
    wp_models$calibration_curves$Logistic %>% mutate(Model = "Logistic"),
    wp_models$calibration_curves$GBM %>% mutate(Model = "GBM"),
    wp_models$calibration_curves$`Random Forest` %>% mutate(Model = "Random Forest"),
    wp_models$calibration_curves$XGBoost %>% mutate(Model = "XGBoost")
  )

  p_calib <- ggplot(calib_plot_data, aes(x = mean_pred, y = mean_actual, color = Model)) +
    geom_line(linewidth = 1) +
    geom_point(aes(size = n), alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 0.8) +
    scale_color_viridis_d(option = "plasma", end = 0.9) +
    labs(
      title = "Win Probability Model Calibration",
      subtitle = "Predicted vs Observed Win Rate (Perfect calibration = diagonal line)",
      x = "Predicted Win Probability",
      y = "Observed Win Rate",
      size = "N Observations"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    ) +
    coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))

  ggsave("figs/wp_calibration.png", p_calib, width = 10, height = 8, dpi = 300)
  message("Saved: figs/wp_calibration.png")

  # Create individual calibration plot for best model
  best_model <- wp_models$best_model_name
  best_calib <- wp_models$calibration_curves[[best_model]]

  p_calib_best <- ggplot(best_calib, aes(x = mean_pred, y = mean_actual)) +
    geom_line(color = "#440154FF", linewidth = 1.5) +
    geom_point(aes(size = n), color = "#440154FF", alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
    labs(
      title = paste("Calibration:", best_model, "Model"),
      subtitle = "Predicted vs Observed Win Rate (Red line = perfect calibration)",
      x = "Predicted Win Probability",
      y = "Observed Win Rate",
      size = "N Observations"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))

  ggsave("figs/wp_calibration_best.png", p_calib_best, width = 8, height = 8, dpi = 300)
  message("Saved: figs/wp_calibration_best.png")
} else {
  message("Warning: No calibration data found in wp_models")
}

# Plot 1: Top 30 Players by RAPM
# Filter: only players with sufficient data in our RAPM analysis
top_players <- rapm_table %>%
  filter(!is.na(games_played), games_played >= 5, !is.na(ridge_rapm)) %>%
  arrange(desc(ridge_rapm)) %>%
  head(30)

if (nrow(top_players) == 0) {
  warning("No players meet the games_played >= 5 threshold. Skipping top players plot.")
} else {
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
}

# Plot 2: RAPM Distribution
rapm_for_dist <- rapm_table %>% filter(!is.na(games_played), !is.na(ridge_rapm))

if (nrow(rapm_for_dist) == 0) {
  warning("No players with valid RAPM. Skipping distribution plot.")
} else {
  p2 <- ggplot(rapm_for_dist, aes(x = ridge_rapm)) +
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
}

# Plot 3: RAPM vs Minutes Played
rapm_for_minutes <- rapm_table %>%
  # Ensure avg_minutes exists (compute if missing)
  mutate(avg_minutes = ifelse(is.na(avg_minutes) & !is.na(total_minutes) & !is.na(games_played),
    total_minutes / pmax(games_played, 1), avg_minutes
  )) %>%
  filter(!is.na(games_played), games_played >= 5, !is.na(ridge_rapm), !is.na(avg_minutes))

if (nrow(rapm_for_minutes) == 0) {
  warning("No players meet threshold for RAPM vs minutes plot. Skipping.")
} else {
  p3 <- ggplot(
    rapm_for_minutes,
    aes(x = avg_minutes, y = ridge_rapm)
  ) +
    geom_point(alpha = 0.6, color = "steelblue", size = 2) +
    geom_smooth(method = "loess", color = "red", se = TRUE, linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    labs(
      title = "RAPM vs. Average Minutes Played",
      subtitle = paste0(
        "Range: ",
        round(min(rapm_table$ridge_rapm, na.rm = TRUE), 5), " to ",
        round(max(rapm_table$ridge_rapm, na.rm = TRUE), 5)
      ),
      x = "Average Minutes per Game",
      y = "RAPM (Ridge) - Win Probability Impact"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  ggsave("figs/rapm_vs_minutes.png", p3, width = 8, height = 6, dpi = 300)
  message("Saved: figs/rapm_vs_minutes.png")
}

# NEW PLOT: RAPM vs Shooting Efficiency (uses enhanced data!)
if ("ft_pct" %in% names(rapm_table) && "fg3_pct" %in% names(rapm_table)) {
  message("Creating shooting efficiency plots with enhanced data...")

  # Plot: RAPM vs Free Throw Percentage
  # UPDATED: Lower threshold for starter-based RAPM
  p_shooting1 <- rapm_table %>%
    filter(games_played >= 10, !is.na(ft_pct), !is.na(fg3_pct)) %>%
    ggplot(aes(x = ft_pct, y = ridge_rapm, color = fg3_pct)) +
    geom_point(alpha = 0.6, size = 2.5) +
    scale_color_viridis(option = "plasma", name = "3P%") +
    geom_smooth(method = "lm", color = "red", se = TRUE, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    labs(
      title = "RAPM vs. Free Throw Shooting",
      subtitle = "Color shows 3-point percentage (qualified players)",
      x = "Free Throw %",
      y = "RAPM (Ridge)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  ggsave("figs/rapm_vs_shooting.png", p_shooting1, width = 10, height = 6, dpi = 300)
  message("Saved: figs/rapm_vs_shooting.png")
}

# NEW PLOT: RAPM by Position (uses enhanced data!)
if ("position" %in% names(rapm_table)) {
  message("Creating position analysis plots...")

  # UPDATED: Lower threshold for starter-based RAPM
  p_position <- rapm_table %>%
    filter(games_played >= 10, !is.na(position), position %in% c("G", "F", "C")) %>%
    ggplot(aes(x = position, y = ridge_rapm, fill = position)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_fill_manual(values = c("G" = "#1f77b4", "F" = "#ff7f0e", "C" = "#2ca02c")) +
    labs(
      title = "RAPM Distribution by Position",
      subtitle = "Guards vs Forwards vs Centers",
      x = "Position",
      y = "RAPM (Ridge)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")

  ggsave("figs/rapm_by_position.png", p_position, width = 8, height = 6, dpi = 300)
  message("Saved: figs/rapm_by_position.png")
}

# NEW PLOT: Starters vs Bench (uses enhanced data!)
if ("is_starter" %in% names(rapm_table)) {
  message("Creating starter vs bench comparison...")

  # UPDATED: Lower threshold for starter-based RAPM
  p_starter <- rapm_table %>%
    filter(games_played >= 10, !is.na(is_starter)) %>%
    mutate(role = ifelse(is_starter, "Starter", "Bench")) %>%
    ggplot(aes(x = role, y = ridge_rapm, fill = role)) +
    geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
    geom_jitter(alpha = 0.2, width = 0.2, size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_fill_manual(values = c("Starter" = "#2ecc71", "Bench" = "#e74c3c")) +
    labs(
      title = "RAPM: Starters vs Bench Players",
      subtitle = "Violin plot showing distribution with quartiles",
      x = "Player Role",
      y = "RAPM (Ridge)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")

  ggsave("figs/rapm_starters_vs_bench.png", p_starter, width = 8, height = 6, dpi = 300)
  message("Saved: figs/rapm_starters_vs_bench.png")
}

# NEW PLOT: Elite Shooters with High RAPM
if ("ft_pct" %in% names(rapm_table) && "fg3_pct" %in% names(rapm_table)) {
  message("Creating elite shooters visualization...")

  # UPDATED: Lower games threshold since we use starter-only RAPM on sampled games
  # Elite criteria: strong shooting efficiency (FT% and 3P%) + enough games for reliability
  elite_shooters <- rapm_table %>%
    filter(
      games_played >= 10, # At least 10 games in our analysis (down from 20)
      !is.na(ft_pct), !is.na(fg3_pct),
      ft_pct > 0.75, # Slightly lower threshold (75% FT)
      fg3_pct > 0.35 # Keep 35% 3PT
    ) %>%
    arrange(desc(ridge_rapm)) %>%
    head(20)

  if (nrow(elite_shooters) > 0) {
    p_elite <- ggplot(elite_shooters, aes(x = reorder(player, ridge_rapm), y = ridge_rapm)) +
      geom_col(aes(fill = ridge_rapm), show.legend = FALSE) +
      scale_fill_viridis(option = "plasma") +
      coord_flip() +
      labs(
        title = "Top 20 Elite Shooters by RAPM",
        subtitle = "Starters with FT% > 75% AND 3P% > 35% (10+ games in analysis)",
        x = "Player",
        y = "RAPM (Ridge)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 9)
      )

    ggsave("figs/elite_shooters_rapm.png", p_elite, width = 10, height = 8, dpi = 300)
    message("Saved: figs/elite_shooters_rapm.png")
  }
}

# Plot 4: Ridge vs Lasso vs Elastic Net comparison
# Check if Lasso has variation
lasso_var <- var(rapm_table$lasso_rapm, na.rm = TRUE)
lasso_max <- max(abs(rapm_table$lasso_rapm), na.rm = TRUE)

if (lasso_var < 1e-10 || lasso_max < 1e-10) {
  # Lasso shrunk everything to zero - create informative plot
  p4 <- ggplot(
    rapm_table %>% filter(!is.na(games_played)) %>% head(200),
    aes(x = ridge_rapm, y = lasso_rapm)
  ) +
    geom_point(alpha = 0.6, color = "steelblue", size = 2) +
    geom_hline(yintercept = 0, linetype = "solid", color = "red", linewidth = 1) +
    annotate("text",
      x = mean(rapm_table$ridge_rapm, na.rm = TRUE),
      y = max(rapm_table$lasso_rapm, na.rm = TRUE) * 0.5,
      label = "Lasso shrunk all coefficients to ~0\n(Strong regularization)",
      color = "red", size = 4, fontface = "italic"
    ) +
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
  p4 <- ggplot(
    rapm_table %>% filter(!is.na(games_played)),
    aes(x = ridge_rapm, y = lasso_rapm)
  ) +
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
tryCatch(
  {
    png("figs/cv_lambda_selection.png", width = 12, height = 4, units = "in", res = 300)
    par(mfrow = c(1, 3))
    plot(rapm_results$cv_ridge, main = "Ridge CV")
    plot(rapm_results$cv_lasso, main = "Lasso CV")
    plot(rapm_results$cv_elastic, main = "Elastic Net CV")
    dev.off()
    message("Saved: figs/cv_lambda_selection.png")
  },
  error = function(e) {
    dev.off() # Close any open graphics device
    message("Note: CV plots skipped (not critical for analysis)")
  }
)

# Plot 6: Win Probability Model Performance
# Load test predictions and compare to actual outcomes
if (!is.null(wp_models$evaluation_results)) {
  p6 <- ggplot(
    wp_models$evaluation_results,
    aes(x = Model, y = AUC, fill = Dataset)
  ) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(aes(label = round(AUC, 3)),
      position = position_dodge(width = 0.7),
      vjust = -0.5, size = 3
    ) +
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
    coord_cartesian(ylim = c(0.8, 1.0)) # Zoom to data range

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
  select(
    rank_type, player, ridge_rapm, lasso_rapm, elastic_rapm,
    games_played, avg_minutes, ridge_percentile
  )

write_csv(top_bottom, "tables/top_bottom_players.csv")
message("Saved: tables/top_bottom_players.csv")

# Plot 7: Regularization path
tryCatch(
  {
    png("figs/regularization_path.png", width = 10, height = 6, units = "in", res = 300)
    plot(rapm_results$ridge_model,
      xvar = "lambda", label = TRUE,
      main = "Ridge Regularization Path"
    )
    abline(v = log(rapm_results$cv_ridge$lambda.min), lty = 2, col = "red")
    dev.off()
    message("Saved: figs/regularization_path.png")
  },
  error = function(e) {
    dev.off() # Close any open graphics device
    message("Note: Regularization path plot skipped (not critical for analysis)")
  }
)

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
