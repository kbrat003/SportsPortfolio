library(nflfastR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(nflreadr)

rm(list=ls())

pbp <- load_pbp(2023) %>% 
  filter(week <= 18, play_type == "pass" | play_type == "run")

rushers <- pbp %>% group_by(p_id = rusher_player_id, posteam) %>% 
  summarize(epa = sum(epa),
            wpa = sum(wpa),
            vwpa = sum(vegas_wpa),
            rushes = n(),
            explosives = sum(yards_gained >=15),
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
  left_join(teams_colors_logos, by = c("team" = "team_abbr")) %>% 
  filter(name != "D.Samuel", touches >= 50)

# Deebo Samuel highly skews the graph and is an outlier for rushing production for a non-running back

# All touches
netTable %>% 
  ggplot(aes(x = total_epa_play, y = total_explosive_rate)) + 
  geom_point(aes(fill = team_color, color = team_color2, size = touches), shape = 21) + 
  geom_text_repel(aes(label = name))+
  geom_smooth(method = "lm", color = "black", size = 0.5, alpha = 0.2, se = FALSE) +
  theme_minimal() +
  labs(x = "EPA/Touch",
       y = "Net explosive Play Rate",
       title = "Total Explosiveness vs. Efficiency",
       subtitle = "Explosives: runs of 10+, catches of 20+ yards; 50+ touches, 20+ carries to qualify",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | July 29 2024") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_fill_identity() +  
  scale_color_identity() + 
  scale_size_continuous(range = c(1, 5)) +
  theme(legend.position = "none")

# Only Rushes
netTable %>% 
  ggplot(aes(x = rush_epa_play, y = rushing_explosive_rate)) + 
  geom_point(aes(fill = team_color, color = team_color2, size = rushes), shape = 21) + 
  geom_text_repel(aes(label = name)) +
  geom_smooth(method = "lm", color = "black", size = 0.5, alpha = 0.2, se = FALSE) +
  theme_minimal() +
  labs(x = "EPA/Rush",
       y = "Rushing explosive Play Rate",
       title = "Rushing Explosiveness+ vs. Efficiency",
       subtitle = "Explosives+ defined as rushes of 15+ yards; 50+ touches, 20+ carries to qualify",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | July 29 2024") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_fill_identity() +  
  scale_color_identity() + 
  scale_size_continuous(range = c(1, 5)) +
  theme(legend.position = "none")

recTable <- netTable %>% filter(catches >= 10)

# Only Catches
recTable %>% 
  ggplot(aes(x = rec_epa_play, y = receiving_explosive_rate)) + 
  geom_point(aes(fill = team_color, color = team_color2, size = catches), shape = 21) + 
  geom_text_repel(aes(label = name))+
  geom_smooth(color = "black", size = 0.5, alpha = 0.2) +
  theme_minimal() +
  labs(x = "EPA/Catch",
       y = "Receiving explosive Play Rate",
       title = "Receiving Explosiveness vs. Efficiency",
       subtitle = "Explosives defined as catches of 20+ yards; 50+ touches, 10+ catches to qualify",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | July 29 2024") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_fill_identity() +  
  scale_color_identity() + 
  scale_size_continuous(range = c(1, 5)) +
  theme(legend.position = "none")

netTable %>% 
  ggplot(aes(x = opportunity, y = scoring_drive_perc)) + 
  geom_point(aes(fill = team_color, color = team_color2, size = touches), shape = 21) + 
  geom_text_repel(aes(label = name)) +
  geom_smooth(color = "black", size = 0.5, alpha = 0.2) +
  theme_minimal() +
  labs(x = "Average Touchdown Probability per Touch",
       y = "Percentage of Drives Resulting in a Touchdown",
       title = "Players Contributing Towards Drives Resulting in a Touchdown",
       subtitle = "Insights into utilization contributing towards successful drives",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | July 29 2024") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_fill_identity() +  
  scale_color_identity() + 
  scale_size_continuous(range = c(1, 5)) +
  theme(legend.position = "none")



firstHalfScoring <- pbp %>% filter(week <= 12) %>% group_by(posteam)%>% 
  summarize(earlyScoring = mean(ifelse(posteam == home_team, home_score, away_score)),
            eplays = n())

secondHalfScoring <- pbp %>% filter(week > 12, week <= 18) %>% group_by(posteam)%>% 
  summarize(lateScoring = mean(ifelse(posteam == home_team, home_score, away_score)),
            lplays = n())

scoringSplits <- firstHalfScoring %>% 
  left_join(secondHalfScoring, by = "posteam") %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

scoringSplits %>% 
  ggplot(aes(x = earlyScoring, y = lateScoring)) + 
  geom_point(aes(fill = team_color, color = team_color2, size = eplays+lplays), shape = 21) + 
  geom_text_repel(aes(label = posteam)) +
  geom_smooth(method = "lm", color = "black", size = 0.5, alpha = 0.5, se = FALSE) +
  theme_minimal() +
  labs(x = "Scoring before Week 13",
       y = "Scoring Week 13 through 18",
       title = "Late Season Scoring Changes",
       subtitle = "Scoring normalized by points per play in games",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | July 29 2024") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_fill_identity() +  
  scale_color_identity() + 
  scale_size_continuous(range = c(1, 5)) +
  theme(legend.position = "none")
