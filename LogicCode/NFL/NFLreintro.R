library(nflfastR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(nflreadr)

rm(list=ls())

pbp <- load_pbp(2023) %>% 
  filter(week <= 18)

pbp <- pbp[!is.na(pbp$down), ]

nonSToffense <- pbp %>% 
  filter((play_type == "run" | play_type == "pass"))

nonSToffense$yardranges <- ifelse(nonSToffense$ydstogo > 10, "11+", ifelse(nonSToffense$ydstogo > 7, "8-10", ifelse(nonSToffense$ydstogo > 3, "4-8", "0-3")))

pr <- nonSToffense %>% 
  group_by(posteam, down, yardranges) %>% 
  summarize(pr = mean(play_type == "pass"), .groups = "drop")

pass_rates_wide <- nonSToffense %>%
  pivot_wider(names_from = down, values_from = pr, names_prefix = "down")

longEarlyDowns <- nonSToffense %>% filter(down != 3, down != 4, ydstogo >= 15) %>% 
  group_by(posteam, play_type, down)

badRuns <- longEarlyDowns %>% summarize(rr = mean(play_type == "rush"))

receiver_player_name

rusher_player_name

bearsO <- nonSToffense %>% filter (posteam == "CHI")
titansO <- nonSToffense %>% filter (posteam == "TEN")
bengalsO <- nonSToffense %>% filter (posteam == "CIN")

browning <- bengalsO %>% filter(passer_player_name == "J.Browning")
burrow <- bengalsO %>% filter(passer_player_name == "J.Burrow")
burrowHealthy <- burrow %>% filter(week > 3)

chase <- bengalsO %>% filter(play_type == "pass") %>% group_by(passer_player_name, receiver)  %>% 
  summarise(air_epa_play = mean(air_epa, na.rm = TRUE), .groups = "drop",
            epa_play = mean(epa, na.rm = TRUE),
            epaOE = epa_play - air_epa_play,
            plays = n())

chase <- chase[!is.na(chase$receiver), ] %>% filter (plays >= 5)


ids <- nonSToffense %>% group_by(receiver_id) %>% 
  summarize(Name = first(receiver_player_name), .groups = "drop")

passes <- nonSToffense %>% filter(play_type == "pass", !is.na(yac_epa)) %>% group_by(receiver_id, passer_player_name, posteam, ydstogo) %>% 
  summarize(air_epa_play = mean(air_epa, na.rm = TRUE), .groups = "drop",
            epa_play = mean(epa, na.rm = TRUE),
            epa = sum(epa),
            avg_yac_epa = epa_play - air_epa_play,
            yds = sum(yards_gained),
            targets = n(),
            rec = sum(!is.na(yards_after_catch)),
            ypc = yds/rec,
            target_to_catch_perc = 100*rec/targets)

receiving <- ids %>% 
  left_join(passes, by = "receiver_id") %>% 
  select(-receiver_id) 

receiving <- receiving[!is.na(receiving$Name), ] %>% filter(targets >= 5, rec > 0)

#receiving <- receiving[!is.na(receiving$Name), ] %>% filter(targets > 24)

downfield <- receiving %>% filter(ypc > 10)

downProduction <- receiving %>% group_by(down)

shortProduction <- receiving %>% filter(yardranges == "0-3")
deepProduction <- receiving %>% filter(yardranges == "11+")

jamarrrange <- rbind(data = receiving
                ) %>% filter(Name == "J.Chase") %>% group_by(passer_player_name) %>% select(-Name)

jamarrdown <- rbind(d1 = downProduction %>% filter(down == 1),
                    d2 = downProduction %>% filter(down == 2),
                    d3 = downProduction %>% filter(down == 3)
                    )%>% filter(Name == "J.Chase") %>% group_by(passer_player_name) %>% select(-Name)
