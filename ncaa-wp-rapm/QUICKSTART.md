# Quick Start Guide

## Getting Started in 5 Minutes

### Option 1: Quick Test (Recommended First)

Run a small test with limited data (~5-10 minutes):

```r
# In R console
setwd("ncaa-wp-rapm")  # or navigate to this folder in RStudio
source("quick_test.R")
```

This will:
- Install required packages
- Download ~50 games from 5 top teams
- Build WP models
- Calculate RAPM
- Generate plots

**Expected output**: Tables and figures in `tables/` and `figs/` folders

---

### Option 2: Full Analysis

For complete season analysis (several hours):

```r
source("run_analysis.R")
```

Or run steps individually:

```r
source("R/00_setup.R")       # Setup (5 min)
source("R/01_get_data.R")    # Data download (1-3 hours)
source("R/02_build_wp_model.R")  # WP models (10-20 min)
source("R/03_build_shifts.R")    # Build shifts (10-20 min)
source("R/04_fit_rapm.R")        # Fit RAPM (5-15 min)
source("R/05_eval_plots.R")      # Plots (2-5 min)
```

---

## Customization

### Change Season

Edit `01_get_data.R`:
```r
SEASON <- "2022-23"  # Change to desired season
```

### Limit Games (for testing)

Edit `01_get_data.R`:
```r
LIMIT_GAMES <- 100  # Set to NULL for all games
```

### Change Minimum Games Filter

Edit `04_fit_rapm.R` or `05_eval_plots.R`:
```r
filter(games_played >= 10)  # Adjust threshold
```

---

## View Results

### Tables
```r
# In R console
library(tidyverse)

# Load RAPM results
rapm <- read_csv("tables/rapm_rankings.csv")

# View top players
rapm %>% 
  filter(games_played >= 10) %>%
  arrange(desc(ridge_rapm)) %>%
  select(player, ridge_rapm, games_played) %>%
  head(20)
```

### Figures

Open `figs/` folder to view:
- `top_players_rapm.png` - Top 30 players bar chart
- `rapm_distribution.png` - Distribution histogram
- `rapm_vs_minutes.png` - RAPM vs playing time
- `combined_analysis.png` - Multi-panel summary
- And more...

---

## Generate Report

Create HTML report with Quarto:

```bash
# In terminal
quarto render report.qmd
```

Or slides:

```bash
quarto render slides.qmd
```

---

## Troubleshooting

### Package Installation Issues

```r
# Manually install troublesome packages
install.packages("ncaahoopR")  # If this fails:
devtools::install_github("lbenz730/ncaahoopR")
```

### Memory Issues

If running out of memory:
- Reduce `LIMIT_GAMES` in `01_get_data.R`
- Run on fewer teams
- Use a machine with more RAM

### Data Download Errors

Some games may fail to download. This is normal - the script continues with available data.

---

## Project Structure

```
ncaa-wp-rapm/
├── R/                    # Analysis scripts
│   ├── 00_setup.R
│   ├── 01_get_data.R
│   ├── 02_build_wp_model.R
│   ├── 03_build_shifts.R
│   ├── 04_fit_rapm.R
│   └── 05_eval_plots.R
├── data/                 # Data (gitignored)
├── figs/                 # Output figures
├── tables/               # Output tables
├── run_analysis.R        # Main pipeline
├── quick_test.R          # Quick test script
├── report.qmd            # Analysis report
└── README.md             # Full documentation
```

---

## Next Steps

1. **Run quick test** to verify setup
2. **Review outputs** in `tables/` and `figs/`
3. **Customize parameters** as needed
4. **Run full analysis** for complete results
5. **Generate report** with Quarto

---

## Getting Help

- Check `README.md` for detailed documentation
- Review inline comments in R scripts
- See `PACKAGES.txt` for dependencies
- Open an issue if you encounter bugs

---

**Estimated times** (varies by machine and internet speed):
- Quick test: 5-10 minutes
- Full season (500 games): 1-3 hours
- Full season (all games): 3-8 hours
