# NCAA Basketball RAPM Analysis

## Project Overview

This project implements **Regularized Adjusted Plus-Minus (RAPM)** for NCAA Division I basketball players on a **win probability scale**. The goal is to quantify individual player contributions to team success while controlling for teammate and opponent strength.

### Main Research Question

**How do certain NCAA basketball players contribute to their team winning above others in a measurable metric?**

### Key Features

- **Win Probability Model**: Builds and compares multiple ML models (Logistic, GBM, Random Forest, XGBoost) to predict game outcomes
- **Player Shifts**: Tracks win probability changes during specific lineup combinations  
- **Regularized Regression**: Uses Ridge, Lasso, and Elastic Net to estimate player effects with shrinkage
- **Team/Conference Controls**: Accounts for strength of schedule and conference effects
- **Comprehensive Evaluation**: Generates rankings, visualizations, and statistical summaries

## Data Source

NCAA basketball play-by-play data from the [`ncaahoopR`](https://github.com/lbenz730/ncaahoopR) package by Luke Benz.

## Methodology

### 1. Win Probability Model

Instead of traditional points per 100 possessions, we model RAPM on the **win probability scale**:

- **Features**: Home team lead, time remaining, score interactions, possession indicator
- **Models tested**: Logistic regression, GBM, Random Forest, XGBoost
- **Selection**: Best model chosen by cross-validated AUC on test set
- **Output**: Win probability for any game state

### 2. Player Shifts

A "shift" represents a period where a specific set of players are on the court:

- Extract lineup changes from play-by-play data
- Calculate win probability change (ΔWP) for each shift
- Associate players with their team's ΔWP when on court (+1 for home, -1 for away)

### 3. RAPM Estimation

**Model specification**:
```
ΔWP = β₁·Player₁ + β₂·Player₂ + ... + βₙ·Playerₙ + Team Effects + ε
```

**Regularization**:
- **Ridge (L2)**: Shrinks all coefficients toward zero
- **Lasso (L1)**: Can set coefficients exactly to zero (sparse)
- **Elastic Net**: Combination of Ridge and Lasso

**Why regularization?**
- Reduces noise and overfitting
- Prevents over-attribution to players with limited minutes
- Handles multicollinearity (players often play together)
- Accounts for teams not playing each other (sparse connectivity)

### 4. Interpretation

**RAPM value**: Expected change in win probability per possession when player is on court (vs. average player)

- **Positive RAPM**: Player increases team's chance of winning
- **Negative RAPM**: Player decreases team's chance of winning
- **Scale**: Typically ranges from -0.02 to +0.02 (i.e., ±2% WP change per possession)

## Project Structure

```
ncaa-wp-rapm/
├── R/
│   ├── 00_setup.R              # Install packages, create directories
│   ├── 01_get_data.R           # Download NCAA play-by-play data
│   ├── 02_build_wp_model.R     # Train win probability models
│   ├── 03_build_shifts.R       # Create player shifts from PBP
│   ├── 04_fit_rapm.R           # Fit regularized RAPM models
│   ├── 05_eval_plots.R         # Generate evaluation visualizations
│   └── utils.R                 # Helper functions
├── data/
│   ├── raw/                    # Raw play-by-play data
│   ├── interim/                # WP models, shifts
│   └── processed/              # Final RAPM results
├── figs/                       # Output visualizations
├── tables/                     # Output tables (CSV)
├── run_analysis.R              # Main script to run full pipeline
├── report.qmd                  # Quarto analysis report
├── slides.qmd                  # Quarto presentation
└── README.md                   # This file
```

## Usage

### Quick Start

1. **Install R dependencies**:
```r
source("R/00_setup.R")
```

2. **Run full analysis**:
```r
source("run_analysis.R")
```

This will execute all steps sequentially:
- Download NCAA data (can take hours for full season)
- Build and evaluate WP models
- Create player shifts
- Fit RAPM models
- Generate plots and tables

### Step-by-Step Execution

You can also run individual scripts:

```r
# Setup
source("R/00_setup.R")

# Get data (optional: modify SEASON and LIMIT_GAMES in script)
source("R/01_get_data.R")

# Build WP model
source("R/02_build_wp_model.R")

# Build shifts
source("R/03_build_shifts.R")

# Fit RAPM
source("R/04_fit_rapm.R")

# Generate plots
source("R/05_eval_plots.R")
```

### Configuration

**Data acquisition** (`01_get_data.R`):
- `SEASON`: Change to desired season (e.g., "2023-24")
- `LIMIT_GAMES`: Set to `NULL` for all games, or a number for testing

**Minimum games filter** (`04_fit_rapm.R`, `05_eval_plots.R`):
- Adjust `games_played >= 10` filter as needed

## Outputs

### Tables

- `rapm_rankings.csv`: All players with RAPM values and statistics
- `top_bottom_players.csv`: Top 25 and bottom 25 players
- `rapm_summary_stats.csv`: Descriptive statistics
- `wp_model_evaluation.csv`: Model comparison metrics

### Figures

- `top_players_rapm.png`: Bar chart of top 30 players
- `rapm_distribution.png`: Histogram of RAPM values
- `rapm_vs_minutes.png`: RAPM relationship with playing time
- `ridge_vs_lasso.png`: Comparison of regularization methods
- `cv_lambda_selection.png`: Cross-validation curves
- `wp_model_performance.png`: Model AUC comparison
- `regularization_path.png`: Coefficient paths
- `combined_analysis.png`: Multi-panel summary figure

## Key Dependencies

- **Data**: `ncaahoopR`, `tidyverse`
- **Modeling**: `glmnet`, `caret`, `gbm`, `xgboost`, `randomForest`
- **Evaluation**: `pROC`
- **Visualization**: `ggplot2`, `viridis`, `cowplot`

See `00_setup.R` for complete package list.

## Limitations & Extensions

### Current Limitations

1. **Lineup data**: If exact lineup data is unavailable, the script approximates player shifts from box scores (players with >10 minutes)
2. **Sample size**: Players with few games may have unreliable RAPM estimates despite regularization
3. **Possession tracking**: ncaahoopR doesn't always have perfect possession indicators

### Potential Extensions

1. **Multi-year RAPM**: Pool data across seasons with year effects
2. **Offensive/Defensive RAPM**: Separate impacts on offense vs defense
3. **Luck-adjusted RAPM**: Account for shot variance (e.g., opponent 3PT% on defense)
4. **Conference-specific models**: Build separate WP models per conference
5. **Player tracking**: Incorporate SportVU or tracking data if available
6. **Priors**: Use Bayesian approach with informative priors from recruiting rankings

## References

- [ncaahoopR Package](https://github.com/lbenz730/ncaahoopR)
- Adjusted Plus-Minus methodology: [Basketball-Reference](https://www.basketball-reference.com/about/glossary.html)
- Regularization in sports analytics: Engelmann (2017), Sill (2010)

## Authors

Developed for STAT 479 course project.

## License

This project uses data from `ncaahoopR`. Please cite appropriately if using this methodology.

---

For questions or issues, please open an issue on the project repository.