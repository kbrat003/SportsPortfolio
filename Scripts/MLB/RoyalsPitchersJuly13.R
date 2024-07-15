library(baseballr)
library(dplyr)
library(vip)
library(tidyverse)
library(ggimage)
library(ggrepel)
library(ggplot2)

rm(list=ls()) # optional clean-up

pitcherData <- bref_daily_pitcher("2024-01-01", "2024-12-31") %>% fip_plus()
pitcherData[is.na(pitcherData)] <- 0

royalsPitchers <- pitcherData %>% filter(Team == "Kansas City")

bullpenPlayer <- pitcherData %>% filter(IP - G < 10, G > 5, IP >= 15)
royalsBullpen <- royalsPitchers %>% filter(IP - G < 10, G > 3)

bullpen <- bullpenPlayer %>% 
  mutate(Team = paste(Team, str_sub(bullpenPlayer$Level, -3), sep = "")) %>% 
  group_by(Team) %>% 
  summarise(SOP = 100*sum(SO)/sum(BF),
            GB = mean(GB.FB))

bullpen <- bullpen[-(grep(",", bullpen$Team)), ]

avg <- mean(bullpen$SOP)

bullpen$SOPAA <- bullpen$SOP - avg

ggplot(bullpen, aes(x = SOPAA)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_text(aes(label = Team)) +
  labs(x = "SOP Above Average (SOPAA)", y = "Frequency", title = "Histogram of SOPAA with Team Labels") +
  theme_minimal()
