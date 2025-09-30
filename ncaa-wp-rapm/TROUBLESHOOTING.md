# Troubleshooting Guide

## Common Installation Issues

### Issue 1: "trying to use CRAN without setting a mirror"

**Error message**:
```
Error in contrib.url(repos, "source") : 
  trying to use CRAN without setting a mirror
```

**Solution**: ✅ **FIXED** - All scripts now set CRAN mirror at the beginning:
```r
options(repos = c(CRAN = "https://cloud.r-project.org/"))
```

**What to do**: Simply re-run the script. The updated version will work.

---

### Issue 2: "package 'ncaahoopR' is not available for this version of R"

**Error message**:
```
Warning message:
package 'ncaahoopR' is not available for this version of R
```

**Why**: `ncaahoopR` is only available on GitHub, not CRAN.

**Solution**: ✅ **FIXED** - Setup script now installs from GitHub:
```r
devtools::install_github("lbenz730/ncaahoopR")
```

**What to do**: Re-run the setup. It will now install from the correct source.

---

### Issue 3: Installation takes a long time

**Expected behavior**: First-time installation can take 10-30 minutes depending on:
- Your internet connection
- How many packages need to be compiled
- Your computer's speed

**Tips**:
- Be patient, especially with `tidyverse`, `xgboost`, and `ncaahoopR`
- Watch for compilation messages - this is normal
- Don't interrupt the installation

---

### Issue 4: devtools installation fails

**Error**: Issues installing `devtools` package

**Solution**:
```r
# Try installing from CRAN with dependencies
install.packages("devtools", dependencies = TRUE)

# If that fails, install required system libraries:
# On Mac (via Homebrew):
# brew install openssl libgit2

# On Ubuntu/Debian:
# sudo apt-get install libssl-dev libcurl4-openssl-dev libgit2-dev
```

---

### Issue 5: ncaahoopR GitHub installation fails

**Error**: `devtools::install_github()` fails

**Solutions to try**:

1. **Update R** to the latest version:
   ```r
   R.version.string  # Check current version
   # Download latest from: https://cran.r-project.org/
   ```

2. **Install with build vignettes disabled**:
   ```r
   devtools::install_github("lbenz730/ncaahoopR", build_vignettes = FALSE)
   ```

3. **Check GitHub rate limit**:
   ```r
   # If you hit rate limits, use a GitHub PAT
   usethis::create_github_token()
   # Follow prompts, then:
   usethis::edit_r_environ()
   # Add: GITHUB_PAT=your_token_here
   ```

---

### Issue 6: Specific package won't install

**General solution**:

1. **Check package dependencies**:
   ```r
   install.packages("PACKAGE_NAME", dependencies = TRUE)
   ```

2. **Clear package cache**:
   ```r
   # Remove and reinstall
   remove.packages("PACKAGE_NAME")
   install.packages("PACKAGE_NAME")
   ```

3. **Install from source**:
   ```r
   install.packages("PACKAGE_NAME", type = "source")
   ```

---

### Issue 7: Memory errors during analysis

**Error**: R runs out of memory during data processing

**Solutions**:

1. **Reduce game limit** in `01_get_data.R`:
   ```r
   LIMIT_GAMES <- 100  # Instead of 500 or NULL
   ```

2. **Increase R memory limit** (Mac/Linux):
   ```r
   # Not usually necessary on Mac/Linux
   ```

3. **Close other applications** to free up RAM

4. **Use fewer WP models** - edit `02_build_wp_model.R` to skip some models

---

### Issue 8: Data download fails

**Error**: `get_pbp_game()` or similar functions fail

**Reasons**:
- Internet connection issues
- NCAA website temporarily unavailable
- Game ID doesn't exist
- Rate limiting

**Solution**: 
- The scripts handle errors gracefully - they skip failed games
- Check error messages for specific game IDs
- Retry later if widespread failures
- Use cached data if available

---

### Issue 9: Can't load tidyverse

**Error**: 
```
Error: package or namespace load failed for 'tidyverse'
```

**Solution**:
```r
# Install individual components
install.packages(c("dplyr", "ggplot2", "tidyr", "readr", "purrr", "tibble"))

# Or force reinstall
remove.packages("tidyverse")
install.packages("tidyverse", dependencies = TRUE)
```

---

### Issue 10: Quarto rendering fails

**Error**: Can't render `.qmd` files

**Solution**:

1. **Install Quarto**: Download from https://quarto.org/docs/get-started/

2. **Check installation**:
   ```bash
   quarto --version
   ```

3. **Render from terminal**:
   ```bash
   cd ncaa-wp-rapm
   quarto render report.qmd
   ```

---

## Platform-Specific Issues

### macOS

**Issue**: Compilation errors for packages

**Solution**: Install Xcode Command Line Tools:
```bash
xcode-select --install
```

**Issue**: `gfortran` not found

**Solution**:
```bash
# Install gfortran via Homebrew
brew install gcc
```

### Windows

**Issue**: Rtools not found

**Solution**: Download and install Rtools from:
https://cran.r-project.org/bin/windows/Rtools/

### Linux

**Issue**: Missing system libraries

**Solution** (Ubuntu/Debian):
```bash
sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev
```

---

## Verification Steps

After fixing issues, verify installation:

```r
# Test package loading
library(tidyverse)
library(ncaahoopR)
library(glmnet)
library(caret)

# Check versions
sessionInfo()

# Run quick test
source("quick_test.R")
```

---

## Getting More Help

1. **Check R version**: Make sure you have R ≥ 4.0
   ```r
   R.version.string
   ```

2. **Update all packages**:
   ```r
   update.packages(ask = FALSE)
   ```

3. **Session info** for debugging:
   ```r
   sessionInfo()
   ```

4. **Check error logs**: Look for specific error messages

5. **ncaahoopR issues**: Check https://github.com/lbenz730/ncaahoopR/issues

---

## Still Having Issues?

If problems persist:

1. Share the **full error message**
2. Run `sessionInfo()` and share the output
3. Specify your operating system and R version
4. Note which script/line is failing

Most issues are resolved by:
- ✅ Setting CRAN mirror (now automatic)
- ✅ Installing ncaahoopR from GitHub (now automatic)
- ✅ Installing system dependencies
- ✅ Updating R and packages
