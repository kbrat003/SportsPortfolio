library(nflfastR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(nflreadr)

rm(list=ls())

pbp <- load_pbp(2023) %>% 
  filter(week <= 18)

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