# Get NCAA data using hoopR package (alternative to ncaahoopR)
# hoopR uses ESPN's API directly and may have better data availability

library(tidyverse)

if (!requireNamespace("hoopR", quietly = TRUE)) {
  message("Installing hoopR...")
  install.packages("hoopR")
}

library(hoopR)

SEASON <- c(2018, 2019, 2020, 2021, 2022, 2023, 2024)
pbp <- load_mbb_pbp(seasons = SEASON)
player_box <- load_mbb_player_box(seasons = SEASON)
pbp_clean <- pbp %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  mutate(
    secs_remaining = as.numeric(start_game_seconds_remaining),
    score_diff = home_score - away_score,
    half = ifelse(period_number <= 2, period_number, 2),
    game_id = as.character(game_id),
    home_team_id = as.character(home_team_id),
    away_team_id = as.character(away_team_id),
    home = home_team_name,
    away = away_team_name,
    season = as.integer(season),
    period_number = period_number,
    play_type = type_text,
    play_text = text
  ) %>%
  filter(!is.na(secs_remaining), secs_remaining >= 0) %>%
  group_by(game_id) %>%
  arrange(game_id, desc(secs_remaining)) %>%
  mutate(
    home_win = ifelse(last(score_diff) > 0, 1, 0)
  ) %>%
  ungroup() %>%
  # Note: Conference mapping done in RAPM script via conference_map.R
  select(
    game_id, season, home_team_id, away_team_id, home, away,
    home_score, away_score, score_diff,
    secs_remaining, half, period_number, play_type, play_text, home_win
  )

box_scores <- player_box %>%
  mutate(
    game_id = as.character(game_id),
    season = as.integer(season),
    team_id = as.character(team_id),
    player_id = as.character(athlete_id),
    player = athlete_display_name,
    team = team_short_display_name,
    min = as.numeric(minutes),
    pts = as.numeric(points),
    reb = as.numeric(rebounds),
    ast = as.numeric(assists),
    fgm = as.numeric(field_goals_made),
    fga = as.numeric(field_goals_attempted),
    fg_pct = ifelse(!is.na(fga) & fga > 0, fgm / fga, NA),
    fg3m = as.numeric(three_point_field_goals_made),
    fg3a = as.numeric(three_point_field_goals_attempted),
    fg3_pct = ifelse(!is.na(fg3a) & fg3a > 0, fg3m / fg3a, NA),
    ftm = as.numeric(free_throws_made),
    fta = as.numeric(free_throws_attempted),
    ft_pct = ifelse(!is.na(fta) & fta > 0, ftm / fta, NA),
    oreb = as.numeric(offensive_rebounds),
    dreb = as.numeric(defensive_rebounds),
    stl = as.numeric(steals),
    blk = as.numeric(blocks),
    tov = as.numeric(turnovers),
    pf = as.numeric(fouls),
    starter = as.logical(starter),
    position = athlete_position_abbreviation,
    home_away = home_away
  ) %>%
  filter(!is.na(min), min > 0) %>%
  select(
    game_id, season, player_id, player, team, min, starter, position, home_away,
    pts, reb, ast, oreb, dreb,
    fgm, fga, fg_pct, fg3m, fg3a, fg3_pct,
    ftm, fta, ft_pct, stl, blk, tov, pf
  )
saveRDS(pbp_clean, "data/raw/pbp_clean.rds")
saveRDS(box_scores, "data/interim/box_scores.rds")

# sample players
print(box_scores %>%
  group_by(player) %>%
  summarise(games = n(), avg_min = mean(min), avg_pts = mean(pts)) %>%
  arrange(desc(avg_pts)) %>%
  head(10))

message("\nDone!")
