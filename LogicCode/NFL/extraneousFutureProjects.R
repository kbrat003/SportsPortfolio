library(nflfastR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(nflreadr)

rm(list=ls())

pbp <- load_pbp(2023) %>% 
  filter(week <= 18, play_type == "pass" | play_type == "run")

pr <- pbp %>% 
  group_by(posteam, down, yardranges) %>% 
  summarize(pr = mean(play_type == "pass"), .groups = "drop")

pass_rates_wide <- pbp %>%
  pivot_wider(names_from = down, values_from = pr, names_prefix = "down")

longEarlyDowns <- pbp %>% filter(down != 3, down != 4, ydstogo >= 15) %>% 
  group_by(posteam, play_type, down)

badRuns <- longEarlyDowns %>% summarize(rr = mean(play_type == "rush"))

bearsO <- pbp %>% filter (posteam == "CHI")
titansO <- pbp %>% filter (posteam == "TEN")

openingDrives <- pbp %>% filter(ifelse(drive == 1, drive == 1, drive == 2)) %>% 
  group_by(game_id) %>% summarize(s = n())
