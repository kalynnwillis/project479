# NCAA Basketball RAPM Implementation Summary

## What We Built

A complete **Regularized Adjusted Plus-Minus (RAPM)** analysis pipeline for NCAA Division I basketball using R, implementing your full analysis plan on a **win probability scale**.

---

## Key Features Implemented

### ✅ 1. Shrinkage Penalty (Regularization)
- **Ridge regression** (L2): Shrinks all coefficients toward zero
- **Lasso regression** (L1): Can zero out coefficients (sparse solutions)
- **Elastic Net**: Balanced combination of Ridge and Lasso
- **Cross-validation**: Automatically selects optimal penalty parameter (λ)

**Purpose**: Reduces noise, prevents over-attribution to single players, handles limited minutes

### ✅ 2. Control for Team and Opponent Effects
- **Player effects**: Individual contribution to win probability
- **Team effects**: Accounts for overall team strength
- **Opponent effects**: Built into the shift structure (±1 coding)
- **Sparse matrix implementation**: Efficient handling of large-scale data

### ✅ 3. Conference Structure
- Naturally handled through **regularization** and **team controls**
- Shrinkage prevents overfitting when teams don't play each other
- Schedule strength accounted for in team effects

### ✅ 4. Win Probability Model
Built **four different models** and automatically selects the best:

1. **Logistic Regression** (baseline)
2. **Gradient Boosting Machine (GBM)**
3. **Random Forest**
4. **XGBoost**

**Features**:
- Home team lead (score differential)
- Time remaining
- Interaction: score × time
- Possession indicator
- Game period (1st/2nd half)
- Clutch indicator (< 5 min, close game)

**Evaluation**: AUC, Brier Score, Log Loss on held-out test set

### ✅ 5. RAPM on Win Probability Scale
- **Output**: Change in win probability per possession
- **Interpretation**: Direct measure of winning impact
- **Scale**: Typically -0.02 to +0.02 (±2% per possession)

---

## Project Structure

```
ncaa-wp-rapm/
├── R/
│   ├── 00_setup.R              # Package installation, directory setup
│   ├── 01_get_data.R           # NCAA data acquisition (ncaahoopR)
│   ├── 02_build_wp_model.R     # Train 4 WP models, select best
│   ├── 03_build_shifts.R       # Create player shifts from PBP
│   ├── 04_fit_rapm.R           # Fit Ridge/Lasso/Elastic Net RAPM
│   ├── 05_eval_plots.R         # Generate visualizations & tables
│   └── utils.R                 # Helper functions
│
├── run_analysis.R              # Run full pipeline
├── quick_test.R                # Quick test with small sample
│
├── report.qmd                  # Quarto analysis report
├── slides.qmd                  # Quarto presentation slides
│
├── README.md                   # Full documentation
├── QUICKSTART.md               # 5-minute getting started guide
├── PACKAGES.txt                # Required R packages
├── IMPLEMENTATION_SUMMARY.md   # This file
│
├── data/
│   ├── raw/                    # Play-by-play data
│   ├── interim/                # WP models, shifts
│   └── processed/              # Final RAPM results
│
├── figs/                       # Output visualizations
└── tables/                     # Output CSV tables
```

---

## How It Works

### Pipeline Overview

```
Raw Data → Win Probability Model → Player Shifts → RAPM → Evaluation
```

### Step-by-Step Process

#### 1. Data Acquisition (`01_get_data.R`)
- Fetches NCAA play-by-play from `ncaahoopR` package
- Configurable season and game limit
- Caches data to avoid re-downloading
- Cleans and prepares game state variables

#### 2. Win Probability Model (`02_build_wp_model.R`)
- Trains 4 ML models on game state → outcome
- 80/20 train/test split
- Evaluates on AUC, Brier Score, Log Loss
- Selects best performer automatically
- Saves all models and metrics

#### 3. Build Shifts (`03_build_shifts.R`)
- Calculates WP for every play using best model
- Creates "shifts": periods with same players on court
- Tracks ΔWP (win probability change) for each shift
- Associates players: +1 (home), -1 (away), 0 (not on court)
- Fetches box scores for player-game associations

#### 4. Fit RAPM (`04_fit_rapm.R`)
- Creates sparse design matrix (shifts × players)
- Adds team effects as controls
- Fits regularized regression:
  - Ridge (α = 0)
  - Lasso (α = 1)
  - Elastic Net (α = 0.5)
- Cross-validates to select optimal λ
- Outputs player rankings with statistics

#### 5. Evaluation (`05_eval_plots.R`)
- Top/bottom player rankings
- RAPM distribution histogram
- RAPM vs. playing time scatter
- Ridge vs. Lasso comparison
- Cross-validation curves
- Multi-panel summary figure

---

## Outputs

### Tables (CSV format)
- `rapm_rankings.csv`: All players with Ridge/Lasso/Elastic RAPM
- `top_bottom_players.csv`: Top 25 and bottom 25 players
- `rapm_summary_stats.csv`: Descriptive statistics
- `wp_model_evaluation.csv`: Model comparison metrics

### Figures (PNG format)
- `top_players_rapm.png`: Top 30 players bar chart
- `rapm_distribution.png`: Histogram of RAPM values
- `rapm_vs_minutes.png`: RAPM vs average minutes played
- `ridge_vs_lasso.png`: Comparison of regularization methods
- `cv_lambda_selection.png`: Cross-validation curves (3 models)
- `wp_model_performance.png`: WP model AUC comparison
- `regularization_path.png`: Ridge coefficient paths
- `combined_analysis.png`: Multi-panel summary (4-panel)

### Reports (Quarto)
- `report.html`: Full analysis report with code
- `slides.html`: Presentation slides

---

## Usage

### Quick Start (5 minutes)
```r
source("quick_test.R")
```
Runs analysis on ~50 games to verify everything works.

### Full Analysis
```r
source("run_analysis.R")
```
Runs complete pipeline from data download to final plots.

### Customization

**Change season**:
```r
# In 01_get_data.R
SEASON <- "2022-23"
```

**Limit games for testing**:
```r
# In 01_get_data.R
LIMIT_GAMES <- 100  # or NULL for all
```

**Adjust minimum games filter**:
```r
# In 04_fit_rapm.R or 05_eval_plots.R
filter(games_played >= 10)  # Change threshold
```

---

## Key Dependencies

### Data
- `ncaahoopR`: NCAA play-by-play data
- `tidyverse`: Data manipulation

### Modeling
- `glmnet`: Regularized regression (Ridge/Lasso/Elastic Net)
- `caret`: ML framework
- `gbm`: Gradient boosting
- `xgboost`: Extreme gradient boosting
- `randomForest`: Random forests
- `Matrix`: Sparse matrices

### Evaluation
- `pROC`: ROC curves and AUC
- `ggplot2`: Visualization
- `viridis`: Color palettes
- `cowplot`: Multi-panel plots

Install all with:
```r
source("R/00_setup.R")
```

---

## Addressing Your Requirements

| Requirement | Implementation | File |
|------------|----------------|------|
| Shrinkage penalty | Ridge/Lasso/Elastic Net with CV | `04_fit_rapm.R` |
| Control for team/opponent | Team effects + shift coding | `04_fit_rapm.R` |
| Conference structure | Regularization handles sparse connectivity | `04_fit_rapm.R` |
| Win probability model | 4 models (Logistic/GBM/RF/XGBoost) | `02_build_wp_model.R` |
| Multiple WP models | Trains all, selects best by AUC | `02_build_wp_model.R` |
| Accurate WP features | Home lead + time remaining + interactions | `02_build_wp_model.R` |

---

## Example Results Interpretation

**Top Player with RAPM = +0.015**:
- Adds 1.5% to team's win probability per possession
- Over 70 possessions/game: +1.05 (105%) total WP impact
- Equivalent to ~3-5 extra wins per season

**Player with RAPM = -0.010**:
- Decreases team's win probability by 1% per possession  
- Should probably get fewer minutes

---

## Extensions & Future Work

Possible enhancements:
1. **Multi-year RAPM**: Track player development across seasons
2. **Offensive/Defensive RAPM**: Separate impacts on each end
3. **Lineup-level RAPM**: Five-player unit effects
4. **Bayesian RAPM**: Use recruiting rankings as priors
5. **Luck-adjusted**: Account for shot variance
6. **Player tracking**: Incorporate SportVU data

---

## Technical Notes

### Computational Efficiency
- **Sparse matrices**: Handles thousands of players efficiently
- **Vectorized operations**: Fast R implementations
- **Caching**: Saves intermediate results to avoid recomputation

### Statistical Rigor
- **Cross-validation**: Prevents overfitting in model selection
- **Regularization**: Stabilizes estimates with limited data
- **Train/test split**: Validates WP model on unseen games
- **Multiple models**: Robustness check across methods

### Data Quality
- **Missing data handling**: Filters and imputes appropriately
- **Outlier detection**: Removes unreasonable shift durations
- **Minimum games**: Filters players with insufficient data

---

## Citation

If using this methodology, please cite:

```
NCAA Basketball RAPM Analysis (2024)
Data source: ncaahoopR package by Luke Benz
https://github.com/lbenz730/ncaahoopR
```

---

## Support

- **Documentation**: See `README.md` for full details
- **Quick start**: See `QUICKSTART.md` for 5-minute guide
- **Code comments**: All scripts heavily documented
- **Example reports**: `report.qmd` and `slides.qmd` demonstrate usage

---

## Conclusion

This implementation provides a **production-ready, statistically rigorous RAPM analysis** for NCAA basketball that:

✅ Uses modern ML for win probability modeling  
✅ Implements proper regularization to handle sparse data  
✅ Controls for team/opponent/conference effects  
✅ Produces interpretable, actionable results  
✅ Generates publication-ready visualizations  
✅ Includes comprehensive documentation  

**The pipeline is fully automated** and can be run with a single command (`source("run_analysis.R")`), making it easy to:
- Analyze different seasons
- Update with new data
- Compare across years
- Generate reports for stakeholders

Enjoy your RAPM analysis!
