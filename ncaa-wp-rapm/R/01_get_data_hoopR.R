# Get NCAA data using hoopR package (alternative to ncaahoopR)
# hoopR uses ESPN's API directly and may have better data availability

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

library(tidyverse)

# Install hoopR if needed
if (!requireNamespace("hoopR", quietly = TRUE)) {
  message("Installing hoopR...")
  install.packages("hoopR")
}

library(hoopR)

message("===================================")
message("NCAA Data via hoopR Package")
message("===================================\n")

# Configuration
# hoopR uses numeric year (e.g., 2022 for 2021-22 season)
SEASON <- c(2023, 2024)  # 2 most recent seasons (prevents memory issues)
message(paste("Seasons:", paste(SEASON, collapse=", ")))

# Load play-by-play
message("Loading play-by-play data...")
pbp <- load_mbb_pbp(seasons = SEASON)

message(paste("Loaded", nrow(pbp), "plays"))

# Load player box scores  
message("\nLoading player box scores...")
player_box <- load_mbb_player_box(seasons = SEASON)

message(paste("Loaded", nrow(player_box), "player-game records"))
message(paste("Unique players:", n_distinct(player_box$athlete_display_name)))

# Convert to our format
message("\nConverting to analysis format...")

# PBP conversion
pbp_clean <- pbp %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  mutate(
    # Use hoopR's built-in seconds remaining (already calculated!)
    secs_remaining = as.numeric(start_game_seconds_remaining),
    # Game state
    score_diff = home_score - away_score,
    half = ifelse(period_number <= 2, period_number, 2),
    # Game outcome
    game_id = as.character(game_id),
    home = home_team_name,
    away = away_team_name
  ) %>%
  filter(!is.na(secs_remaining), secs_remaining >= 0) %>%
  group_by(game_id) %>%
  arrange(game_id, desc(secs_remaining)) %>%
  mutate(
    home_win = ifelse(last(score_diff) > 0, 1, 0)
  ) %>%
  ungroup() %>%
  select(game_id, home, away, home_score, away_score, score_diff, 
         secs_remaining, half, home_win)

# Box scores conversion
box_scores <- player_box %>%
  mutate(
    game_id = as.character(game_id),
    player = athlete_display_name,
    team = team_short_display_name,
    min = as.numeric(minutes),
    pts = as.numeric(points),
    reb = as.numeric(rebounds),
    ast = as.numeric(assists)
  ) %>%
  filter(!is.na(min), min > 0) %>%
  select(game_id, player, team, min, pts, reb, ast)

# Save
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)

saveRDS(pbp_clean, "data/raw/pbp_clean.rds")
saveRDS(box_scores, "data/interim/box_scores.rds")

# Summary
message("\n===================================")
message("=== DATA LOADED SUCCESSFULLY ===")
message("===================================")
message(paste("Season:", SEASON))
message(paste("Games:", n_distinct(pbp_clean$game_id)))
message(paste("Plays:", nrow(pbp_clean)))
message(paste("Player-game records:", nrow(box_scores)))
message(paste("Unique players:", n_distinct(box_scores$player)))
message(paste("Unique teams:", n_distinct(box_scores$team)))

message("\nFiles saved:")
message("  - data/raw/pbp_clean.rds")
message("  - data/interim/box_scores.rds")

message("\nNext: source('R/02_build_wp_model.R')")

# Show sample players
message("\nSample players:")
print(box_scores %>% 
        group_by(player) %>% 
        summarise(games = n(), avg_min = mean(min), avg_pts = mean(pts)) %>%
        arrange(desc(avg_pts)) %>%
        head(10))

message("\nDone!")
