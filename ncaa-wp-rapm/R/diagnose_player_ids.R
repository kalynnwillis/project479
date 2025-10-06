# Diagnostic script to verify player ID fixes
# This script checks for duplicate player names and validates RAPM results

library(tidyverse)

message("=== PLAYER ID DIAGNOSTIC CHECKS ===\n")

# Check 1: How many unique athlete IDs per display name?
message("1. Checking for name collisions in box scores...")
box_scores <- readRDS("data/interim/box_scores.rds")

name_collisions <- box_scores %>%
    distinct(player, player_id) %>%
    count(player) %>%
    arrange(desc(n)) %>%
    filter(n > 1)

if (nrow(name_collisions) > 0) {
    message(sprintf("   Found %d display names with multiple athlete IDs:", nrow(name_collisions)))
    message("   Top 10 collisions:")
    print(head(name_collisions, 10))

    # Show specific examples
    example_name <- name_collisions$player[1]
    examples <- box_scores %>%
        filter(player == example_name) %>%
        distinct(player_id, player, team) %>%
        arrange(player_id)
    message(sprintf("\n   Example: '%s' maps to %d different athletes:", example_name, nrow(examples)))
    print(examples)
} else {
    message("   ✓ No name collisions found (all display names are unique)")
}

# Check 2: Verify RAPM table uses player_id correctly
message("\n2. Checking RAPM results structure...")
if (file.exists("data/processed/rapm_results.rds")) {
    rapm_output <- readRDS("data/processed/rapm_results.rds")
    rapm_table <- rapm_output$rapm_table

    if ("player_id" %in% names(rapm_table)) {
        message("   ✓ RAPM table contains player_id column")

        # Check for duplicates
        n_total <- nrow(rapm_table)
        n_unique_ids <- n_distinct(rapm_table$player_id)

        if (n_total == n_unique_ids) {
            message(sprintf("   ✓ All %d rows have unique player_ids (no duplicates)", n_total))
        } else {
            message(sprintf("   ⚠ WARNING: %d rows but only %d unique player_ids!", n_total, n_unique_ids))
            dups <- rapm_table %>%
                count(player_id) %>%
                filter(n > 1)
            message(sprintf("   Found %d duplicated player_ids:", nrow(dups)))
            print(head(dups, 10))
        }

        # Check that we have display names
        if ("player" %in% names(rapm_table)) {
            n_with_names <- sum(!is.na(rapm_table$player))
            message(sprintf("   ✓ %d/%d rows have display names", n_with_names, n_total))
        } else {
            message("   ⚠ WARNING: No 'player' column for display names!")
        }
    } else {
        message("   ⚠ WARNING: RAPM table does NOT contain player_id column!")
        message("   This means the fix was not applied correctly.")
    }
} else {
    message("   ℹ RAPM results not yet generated (run pipeline first)")
}

# Check 3: Verify shifts use player_id lists
message("\n3. Checking player shifts structure...")
if (file.exists("data/interim/player_shifts.rds")) {
    shifts <- readRDS("data/interim/player_shifts.rds")

    has_id_lists <- all(c("home_starters_id", "away_starters_id") %in% names(shifts))
    has_name_lists <- all(c("home_starters", "away_starters") %in% names(shifts))

    if (has_id_lists) {
        message("   ✓ Shifts contain player_id lists (home_starters_id, away_starters_id)")
        n_unique_ids <- n_distinct(c(unlist(shifts$home_starters_id), unlist(shifts$away_starters_id)))
        message(sprintf("   ✓ %d unique player IDs in shifts", n_unique_ids))
    } else {
        message("   ⚠ WARNING: Shifts do NOT contain player_id lists!")
    }

    if (has_name_lists) {
        message("   ✓ Shifts also contain player name lists for display")
    }
} else {
    message("   ℹ Player shifts not yet generated (run pipeline first)")
}

# Check 4: Compare before/after if old data exists
message("\n4. Summary:")
if (nrow(name_collisions) > 0) {
    message(sprintf("   → Found %d display names that map to multiple athletes", nrow(name_collisions)))
    message("   → Using player_id prevents merging different athletes with the same name")
    message("   → This should result in more accurate RAPM rankings")
} else {
    message("   → No name collisions detected in this dataset")
}

message("\n=== DIAGNOSTIC COMPLETE ===")
