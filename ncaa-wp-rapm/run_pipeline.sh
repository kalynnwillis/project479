#!/bin/bash
# Run complete RAPM analysis pipeline

cd /Users/kalynnwillis/project479/ncaa-wp-rapm

echo "================================="
echo "NCAA RAPM Analysis Pipeline"
echo "================================="
echo ""

echo "Step 2: Building Win Probability Models..."
Rscript R/02_build_wp_model.R 2>&1 | tail -15
echo ""

echo "Step 3: Building Player Shifts..."
Rscript R/03_build_shifts.R 2>&1 | tail -15
echo ""

echo "Step 4: Fitting RAPM Models..."
Rscript R/04_fit_rapm.R 2>&1 | tail -25
echo ""

echo "Step 5: Generating Plots..."
Rscript R/05_eval_plots.R 2>&1 | tail -15
echo ""

echo "================================="
echo "✓ ANALYSIS COMPLETE!"
echo "================================="
echo "Check:"
echo "  - tables/rapm_rankings.csv"
echo "  - figs/*.png"
echo ""
