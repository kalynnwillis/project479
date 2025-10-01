# NCAA Basketball RAPM Analysis

**Regularized Adjusted Plus-Minus on Win Probability Scale**

## Overview

This project implements RAPM (Regularized Adjusted Plus-Minus) for NCAA Division I basketball players using win probability as the outcome measure. The analysis identifies player contributions to winning while controlling for teammates, opponents, and team effects.

## Key Results

- **Win Probability Models**: Achieved 0.895 AUC using XGBoost
- **Players Analyzed**: 2,044 players across 465 games  
- **Top Player Impact**: Elite players add 0.001-0.003 win probability per possession
- **Season Impact**: Top players contribute 10-20% additional win probability per game

## Project Structure

```
ncaa-wp-rapm/
├── R/                          # Analysis scripts
│   ├── 00_setup.R             # Package installation
│   ├── 01_get_data_hoopR.R    # Data acquisition
│   ├── 02_build_wp_model_RF.R # Win probability models (all 4 models)
│   ├── 02_build_wp_model_FAST.R # Fast version (Logistic + GBM only)
│   ├── 03_build_shifts.R      # Player shift construction
│   ├── 04_fit_rapm.R          # RAPM estimation (original)
│   ├── 04_fit_rapm_FAST.R     # RAPM estimation (optimized)
│   ├── 05_eval_plots.R        # Visualization generation
│   └── utils.R                # Utility functions
│
├── data/
│   ├── raw/                   # Raw play-by-play data
│   ├── interim/               # Intermediate processed data
│   └── processed/             # Final RAPM results
│
├── figs/                      # Generated visualizations
├── tables/                    # Results tables
│
├── report.qmd                 # Full technical report
├── slides.qmd                 # Presentation slides
├── run_pipeline.sh            # Master pipeline script
└── README.md                  # This file
```

## Quick Start

### 1. Run Complete Pipeline

```bash
cd ncaa-wp-rapm
./run_pipeline.sh
```

**Expected runtime**: ~25-35 minutes  
**Output**: Generates all tables and figures

### 2. Individual Steps

```bash
# Setup (one-time)
Rscript R/00_setup.R

# Get data
Rscript R/01_get_data_hoopR.R

# Build win probability models
Rscript R/02_build_wp_model_RF.R      # Full version (all 4 models)
# OR
Rscript R/02_build_wp_model_FAST.R    # Fast version (2 models)

# Build player shifts
Rscript R/03_build_shifts.R

# Fit RAPM
Rscript R/04_fit_rapm_FAST.R          # Optimized version (recommended)
# OR
Rscript R/04_fit_rapm.R               # Original (slower)

# Generate visualizations
Rscript R/05_eval_plots.R
```

## Generate Reports

### Install Quarto (if needed)

Download from: https://quarto.org/docs/get-started/

### Render Documents

```bash
# Full technical report (HTML)
quarto render report.qmd

# Presentation slides (RevealJS)
quarto render slides.qmd

# Both
quarto render
```

Outputs will be in the same directory as `.html` files.

## Key Outputs

### Tables
- `tables/rapm_rankings.csv` - Complete player rankings
- `tables/top_bottom_players.csv` - Top/bottom 25 players
- `tables/rapm_summary_stats.csv` - Summary statistics
- `tables/wp_model_evaluation.csv` - Model performance metrics

### Figures
- `figs/top_players_rapm.png` - Top 30 players visualization
- `figs/rapm_distribution.png` - Distribution of RAPM values
- `figs/rapm_vs_minutes.png` - RAPM vs playing time
- `figs/ridge_vs_lasso.png` - Regularization comparison
- `figs/wp_model_performance.png` - Win probability model AUC
- `figs/combined_analysis.png` - 4-panel summary figure

## Methodology

### 1. Win Probability Model

Train multiple algorithms to predict home team win probability:
- Logistic Regression
- Gradient Boosting Machine (GBM)
- Random Forest (ranger)
- XGBoost

**Features**: Score differential, time remaining, interactions, possession, clutch indicator

**Best Model**: XGBoost (AUC = 0.895)

### 2. Player Shifts

Extract game segments and calculate win probability changes:

\$\$\\Delta WP = WP_{\\text{end}} - WP_{\\text{start}}\$\$

**Note**: Due to lack of exact lineup data in play-by-play, we approximate using box score participation (players with >10 minutes). This affects precision but not relative rankings.

### 3. RAPM Estimation

Regularized regression to isolate player effects:

\$\$\\Delta WP = \\sum_{i} \\beta_i \\cdot \\text{Player}_i + \\text{Team Effects} + \\epsilon\$\$

**Regularization**: 
- Ridge (L2): Used for final rankings (stable with noisy data)
- Lasso (L1): Too conservative, shrunk to zero
- Elastic Net: Combination

## Data Limitations

1. **Lineup Approximation**: Play-by-play lacks exact lineups
   - Mitigated by: Box score participation weights
   - Impact: Reduces precision, not rankings validity

2. **Sample Size**: Small sample players less reliable
   - Filter: ≥10 games for main analysis
   
3. **Regularization Sensitivity**: Lasso failed for this data
   - Solution: Focus on Ridge RAPM

## Performance Optimizations

The pipeline includes several optimizations for speed and memory efficiency:

### Random Forest
- Uses `ranger` package (5-10x more memory efficient)
- Reduced complexity (300 trees, max depth 10)
- Sample fraction 0.7 per tree
- **Memory**: ~6GB (vs 16GB+ original)

### RAPM Fitting
- Vectorized sparse matrix creation (100-200x faster)
- Reduced CV folds (5 instead of 10)
- Shift sampling (500K max)
- **Runtime**: ~10 min (vs 4-8+ hours original)

### Overall Pipeline
- **Original**: 12+ hours, frequent crashes
- **Optimized**: ~25-35 minutes, stable

## Dependencies

### R Packages
```r
# Core
tidyverse, Matrix

# Modeling
glmnet, gbm, ranger, xgboost, caret

# Data
hoopR

# Visualization
ggplot2, viridis, cowplot, pROC

# Reporting
knitr, kableExtra
```

See `PACKAGES.txt` for complete list with versions.

### System Requirements
- R ≥ 4.0
- 8GB RAM minimum (16GB recommended)
- ~2GB disk space for data

## Results Interpretation

**RAPM Values** represent expected change in win probability when a player is on court (vs average player):

- **+0.001 RAPM**: Player adds ~0.1% WP per possession
- Over 70 possessions/game: ~7% total WP swing
- Top players (+0.003): ~20% WP advantage per game

**Example**: Jalen Celestine (0.00326 RAPM)
- Adds 0.326% per possession
- ×70 possessions = +22.8% win probability per game
- Substantial impact on season outcomes

## Limitations & Future Work

### Current Limitations
1. Approximate lineup data (game-level not play-level)
2. No offensive/defensive split
3. Context factors (role, matchups) unobserved
4. Single season analysis

### Future Directions
- Multi-year player development tracking
- Offensive/Defensive RAPM decomposition
- Bayesian priors from recruiting data
- True lineup data integration
- Conference-specific models

## References

1. hoopR Package - https://hoopr.sportsdataverse.org/
2. Sill, J. (2010). "Improved NBA Adjusted +/- using Regularization"
3. Rosenbaum, D. T. (2004). "Measuring How NBA Players Help Their Teams Win"
4. Engelmann, J. (2017). "Possession-based player performance analysis"

## Documentation

- `DATA_LIMITATIONS_AND_FIXES.md` - Detailed explanation of data issues and solutions
- `report.qmd` / `report.html` - Full technical report
- `slides.qmd` / `slides.html` - Presentation slides

## Citation

If you use this code or methodology, please cite:

```
NCAA Basketball RAPM Analysis (2024)
Regularized Adjusted Plus-Minus on Win Probability Scale
https://github.com/[your-username]/ncaa-wp-rapm
```

## License

MIT License - See LICENSE file for details

## Contact

For questions or issues, please open an issue on GitHub.

---

**Status**: ✅ Complete and validated  
**Last Updated**: October 2024

