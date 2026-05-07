library(tidyverse)
library(nflreadr)
library(gt)
library(gtExtras)

combine   <- load_combine()
players   <- load_players()
contracts <- load_contracts()
teams     <- load_teams()

fast_te <- combine %>%
  filter(pos == "TE", !is.na(forty), season >= 2016, season <= 2025) %>%
  arrange(forty) %>%
  slice_head(n = 10)

# fix chig oknkwo name
fast_te <- fast_te %>%
  mutate(lookup_name = case_when(
    player_name == "Chigoziem Okonkwo" ~ "Chig Okonkwo",
    TRUE ~ player_name
  )) %>%
  left_join(
    players %>% select(display_name, headshot),
    by = c("lookup_name" = "display_name")
  )

apy <- contracts %>%
  filter(!is.na(inflated_apy), inflated_apy > 0) %>%
  group_by(player) %>%
  slice_max(inflated_apy, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player, inflated_apy)

fast_te <- fast_te %>%
  left_join(apy, by = c("player_name" = "player"))

fast_te <- fast_te %>%
  left_join(
    teams %>% select(team_name, team_logo_espn),
    by = c("draft_team" = "team_name")
  )

tbl <- fast_te %>%
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
gt_tbl <- tbl %>%
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

print(gt_tbl)
gtsave(gt_tbl, "te_table.html")
