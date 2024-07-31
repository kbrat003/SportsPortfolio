library(nflfastR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(nflreadr)

rm(list=ls())

pbp <- load_pbp(2023) |> 
  filter(play_type == "pass" | play_type == "run")
