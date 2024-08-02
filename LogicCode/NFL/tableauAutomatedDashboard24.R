library(nflfastR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(nflreadr)
library(writexl)

rm(list=ls())
options(nflreadr.verbose = FALSE)

pbp <- load_pbp(2023) |> 
  filter(play_type == "pass" | play_type == "run")

rushers <- pbp %>% group_by(p_id = rusher_player_id, posteam) %>% 
  summarize(epa = sum(epa),
            wpa = sum(wpa),
            vwpa = sum(vegas_wpa),
            rushes = n(),
            explosives = sum(yards_gained >=10),
            negatives = sum(yards_gained < 0),
            scoring_drive_perc = sum(fixed_drive_result == "Touchdown")/rushes,
            series_succ_rate = sum(series_success == 1)/rushes,
            opportunity = mean(td_prob)) %>% 
  filter(rushes >= 20)

receivingBacks <- pbp %>% filter %>% group_by(p_id = receiver_player_id, posteam) %>% 
  summarize(epa = sum(epa),
            wpa = sum(wpa),
            vwpa = sum(vegas_wpa),
            catches = n(),
            explosives = sum(yards_gained >=20),
            negatives = sum(yards_gained < 0),
            scoring_drive_perc = sum(fixed_drive_result == "Touchdown")/catches,
            series_succ_rate = sum(series_success == 1)/catches,
            opportunity = mean(td_prob))

quarterbacks <- pbp %>% group_by(id = passer_player_id) %>% 
  summarize(passes = n())%>% 
  filter(passes >= 20)

ids <- pbp %>% group_by(p_id = rusher_player_id) %>% 
  summarize(name = first(rusher_player_name), .groups = "drop") %>% 
  mutate(qb = sapply(p_id, function(x) any(grepl(x, quarterbacks$id))))

rushingids <- ids %>% filter(qb == FALSE)

rushing <- rushingids %>% 
  left_join(rushers, by = "p_id")

rushing <- rushing[!is.na(rushing$epa), ]

receiving <- rushingids %>% 
  left_join(receivingBacks, by = "p_id")

receiving <- receiving[!is.na(receiving$epa), ]

netProduction <- rushing %>% 
  left_join(receiving, by = "p_id")

netTable <- netProduction %>%
  mutate(
    touches = rushes + catches,
    rushes = rushes,
    catches = catches,
    name = name.x,
    team = posteam.x,
    negatives = (negatives.x + negatives.y)/touches,
    rush_epa_play = coalesce(epa.x, 0)/rushes,
    rec_epa_play = coalesce(epa.y, 0)/catches,
    total_epa_play = (coalesce(epa.x, 0) + coalesce(epa.y, 0))/touches,
    wpa = coalesce(wpa.x, 0) + coalesce(wpa.y, 0),
    vwpa = coalesce(vwpa.x, 0) + coalesce(vwpa.y, 0),
    rushing_explosive_rate = coalesce(explosives.x, 0)/rushes,
    receiving_explosive_rate = coalesce(explosives.y, 0)/catches,
    total_explosive_rate = (coalesce(explosives.x, 0) + coalesce(explosives.y, 0))/touches,
    scoring_drive_perc = (coalesce(scoring_drive_perc.x, 0)*rushes + coalesce(scoring_drive_perc.y, 0)*catches)/touches,
    series_succ_rate = (coalesce(series_succ_rate.x, 0)*rushes + coalesce(series_succ_rate.y, 0)*catches)/touches,
    opportunity = (coalesce(opportunity.x, 0)*rushes + coalesce(opportunity.y, 0)*catches)/touches
  ) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>% 
  filter(name != "D.Samuel", touches >= 50) |> 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

remove_last_word <- function(text) {
  sub("\\s+\\S+$", "", text)
}

# Apply the function to the column
netTable$city <- sapply(netTable$team_name, remove_last_word)

write_xlsx(netTable,"/Users/kylebratzel/SportsPortfolio/LogicCode/NFL/chaseBrownTableTableauTest.xlsx")
