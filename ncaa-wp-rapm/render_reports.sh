#!/bin/bash
# Render Quarto documents (report and slides)

cd /Users/kalynnwillis/project479/ncaa-wp-rapm

echo "======================================"
echo "Rendering Quarto Documents"
echo "======================================"
echo ""

# Check if Quarto is installed
if ! command -v quarto &> /dev/null; then
    echo "❌ Quarto not found!"
    echo ""
    echo "Please install Quarto from:"
    echo "https://quarto.org/docs/get-started/"
    echo ""
    echo "Installation options:"
    echo "  - Download installer for macOS"
    echo "  - Or use Homebrew: brew install quarto"
    exit 1
fi

echo "✓ Quarto found: $(quarto --version)"
echo ""

# Render report
echo "Rendering technical report..."
quarto render report.qmd
if [ $? -eq 0 ]; then
    echo "✓ Report rendered: report.html"
else
    echo "❌ Report rendering failed"
    exit 1
fi

echo ""

# Render slides
echo "Rendering presentation slides..."
quarto render slides.qmd
if [ $? -eq 0 ]; then
    echo "✓ Slides rendered: slides.html"
else
    echo "❌ Slides rendering failed"
    exit 1
fi

echo ""
echo "======================================"
echo "✓ ALL DOCUMENTS RENDERED!"
echo "======================================"
echo ""
echo "View your documents:"
echo "  - Report: open report.html"
echo "  - Slides: open slides.html"
echo ""

