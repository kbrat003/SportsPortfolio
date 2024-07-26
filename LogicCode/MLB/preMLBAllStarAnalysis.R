library(baseballr)
library(dplyr)
library(vip)
library(tidyverse)
library(ggimage)
library(ggrepel)

rm(list=ls())

batting <- bref_daily_batter("2024-03-01", "2024-08-22")  %>% woba_plus()
pitching <- bref_daily_pitcher("2024-01-01", "2024-12-31") %>% fip_plus()
batting[is.na(batting)] <- 0
pitching[is.na(pitching)] <- 0

jmonbatting <- bref_daily_batter("2024-06-01", "2024-07-14")  %>% woba_plus()
jmonpitching <- bref_daily_pitcher("2024-06-01", "2024-07-14") %>% fip_plus()
jmonbatting[is.na(jmonbatting)] <- 0
jmonpitching[is.na(jmonpitching)] <- 0

jbatter <- jmonbatting %>% filter(PA > 35)
jpitcher <- jmonpitching %>% filter(G >= 5)