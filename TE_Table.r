library(tidyverse)
library(nflreadr)
library(gt)
library(gtExtras)

combine   <- load_combine()
players   <- load_players()
contracts <- load_contracts()
teams     <- load_teams()

# Step 1: fastest TEs
te_fast <- combine %>%
  filter(pos == "TE", !is.na(forty), season >= 2016, season <= 2025) %>%
  arrange(forty) %>%
  slice_head(n = 10)

# Step 2: normalize names that differ between combine and players table, then join headshot
te_fast <- te_fast %>%
  mutate(lookup_name = case_when(
    player_name == "Chigoziem Okonkwo" ~ "Chig Okonkwo",
    TRUE ~ player_name
  )) %>%
  left_join(
    players %>% select(display_name, headshot),
    by = c("lookup_name" = "display_name")
  )

# Step 3: join contracts by player name
best_contracts <- contracts %>%
  filter(!is.na(inflated_apy), inflated_apy > 0) %>%
  group_by(player) %>%
  slice_max(inflated_apy, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player, inflated_apy)

te_fast <- te_fast %>%
  left_join(best_contracts, by = c("player_name" = "player"))

# Step 4: join team logos on full team name
te_fast <- te_fast %>%
  left_join(
    teams %>% select(team_name, team_logo_espn),
    by = c("draft_team" = "team_name")
  )

# Step 5: build table
table_data <- te_fast %>%
  arrange(forty) %>%
  transmute(
    year         = season,
    team_logo    = team_logo_espn,
    headshot     = headshot,
    player       = player_name,
    school       = school,
    forty        = forty,
    inflated_apy = inflated_apy
  )

# Step 6: render GT table
gt_table <- table_data %>%
  gt() %>%
  gt_theme_espn() %>%
  gt_img_rows(columns = team_logo, img_source = "web", height = 30) %>%
  gt_img_rows(columns = headshot,  img_source = "web", height = 50) %>%
  cols_align(align = "center", everything()) %>%
  fmt_number(
    columns  = inflated_apy,
    decimals = 2,
    pattern  = "${x}M"
  ) %>%
  cols_label(
    year         = "YEAR",
    team_logo    = "DRAFT TEAM",
    headshot     = "PLAYER",
    player       = "",
    school       = "SCHOOL",
    forty        = "FORTY",
    inflated_apy = "INFLATED APY"
  ) %>%
  tab_header(title = "TE Contracts and 40 Yard Dash Times")

print(gt_table)
gtsave(gt_table, "te_table.html")
