# Check if NCAA hoopR data has lineup and substitution information
library(tidyverse)
library(hoopR)

# Load a small sample of raw data
message("Loading sample NCAA play-by-play data...")
pbp_sample <- load_mbb_pbp(seasons = 2024)

# Check column names
message("\n=== All columns in NCAA pbp data ===")
print(names(pbp_sample))

# Check for lineup-related columns
message("\n=== Checking for lineup fields (like NBA) ===")
lineup_cols <- grep("player|lineup|participant|athlete", names(pbp_sample),
    value = TRUE, ignore.case = TRUE
)
if (length(lineup_cols) > 0) {
    message("Found potential lineup columns:")
    print(lineup_cols)
} else {
    message("NO lineup columns found (home_player1, etc.)")
}

# Check for substitution event types
message("\n=== Checking for substitution event indicators ===")
if ("type_id" %in% names(pbp_sample)) {
    event_types <- pbp_sample %>%
        count(type_id, type_text) %>%
        arrange(type_id)
    message("Event types available:")
    print(event_types, n = 50)

    # Check if substitution event exists
    sub_events <- event_types %>%
        filter(str_detect(tolower(type_text), "sub|replace"))
    if (nrow(sub_events) > 0) {
        message("\n*** FOUND SUBSTITUTION EVENTS: ***")
        print(sub_events)
    } else {
        message("\n*** NO substitution events found in type_text ***")
    }
} else {
    message("No type_id column found")
}

# Check description fields
message("\n=== Checking for description fields ===")
desc_cols <- grep("description|text", names(pbp_sample), value = TRUE, ignore.case = TRUE)
if (length(desc_cols) > 0) {
    message("Found description columns:")
    print(desc_cols)

    # Sample some descriptions
    message("\n=== Sample play descriptions ===")
    sample_plays <- pbp_sample %>%
        select(any_of(c("type_text", "text", "home_description", "away_description"))) %>%
        slice_sample(n = 20)
    print(sample_plays)
} else {
    message("NO description columns found")
}

# Check if participants/athletes are embedded in another structure
message("\n=== Checking for athlete/participant data structure ===")
if ("participants" %in% names(pbp_sample)) {
    message("Found 'participants' column - checking structure:")
    print(str(pbp_sample$participants[1:5]))
}

if ("athletes_involved" %in% names(pbp_sample)) {
    message("Found 'athletes_involved' column - checking structure:")
    print(str(pbp_sample$athletes_involved[1:5]))
}

message("\n=== Summary ===")
message("Total plays in sample: ", nrow(pbp_sample))
message("Total columns: ", ncol(pbp_sample))

# Save sample for inspection
saveRDS(pbp_sample %>% slice(1:1000), "data/interim/pbp_raw_sample.rds")
message("\nSaved 1000-row sample to data/interim/pbp_raw_sample.rds for manual inspection")
