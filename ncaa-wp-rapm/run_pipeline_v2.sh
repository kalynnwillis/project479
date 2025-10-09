#!/bin/bash

# NCAA RAPM Pipeline V2 - Top 7 Players by Minutes
# Saves results to new_data/, new_figs/, new_tables/

echo "=========================================="
echo "NCAA RAPM Pipeline V2"
echo "Using top 7 players by minutes (includes bench)"
echo "=========================================="
echo ""

# Step 1: Build shifts (7v7 instead of 5v5)
echo "Step 1/3: Building shifts with top 7 players..."
Rscript R/03_build_shifts_v2.R
if [ $? -ne 0 ]; then
    echo "Error in build_shifts_v2. Stopping."
    exit 1
fi
echo "✓ Shifts built"
echo ""

# Step 2: Fit RAPM models
echo "Step 2/3: Fitting RAPM models..."
Rscript R/04_fit_rapm_FAST_v2.R
if [ $? -ne 0 ]; then
    echo "Error in fit_rapm_v2. Stopping."
    exit 1
fi
echo "✓ RAPM models fitted"
echo ""

# Step 3: Generate plots and tables
echo "Step 3/3: Creating evaluation plots..."
Rscript R/05_eval_plots_v2.R
if [ $? -ne 0 ]; then
    echo "Error in eval_plots_v2. Stopping."
    exit 1
fi
echo "✓ Plots created"
echo ""

echo "=========================================="
echo "V2 Pipeline Complete!"
echo "=========================================="
echo "Results saved to:"
echo "  - new_data/processed/rapm_results.rds"
echo "  - new_tables/*.csv"
echo "  - new_figs/*.png"
echo ""
echo "Compare with original results in:"
echo "  - data/processed/"
echo "  - tables/"
echo "  - figs/"
echo ""

