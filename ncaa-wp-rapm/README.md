# NCAA WP RAPM

A reproducible pipeline to compute NCAA women's basketball Regularized Adjusted Plus-Minus (RAPM), feed it with a win probability (WP) model, and produce a report and slides via Quarto.

## Structure
- `R/`: pipeline scripts
- `data/`: raw/interim/processed artifacts
- `figs/`: figures
- `tables/`: tables
- `report.qmd`: main report
- `slides.qmd`: presentation

## Getting Started
1. Install R and Quarto.
2. Restore dependencies with renv:
```r
install.packages("renv")
renv::restore()
```
3. Run pipeline:
```bash
Rscript R/00_setup.R
Rscript R/01_get_data.R
Rscript R/02_build_wp_model.R
Rscript R/03_build_shifts.R
Rscript R/04_fit_rapm.R
Rscript R/05_eval_plots.R
```
4. Render docs:
```bash
quarto render report.qmd
quarto render slides.qmd
```

