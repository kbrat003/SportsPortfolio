library(baseballr)
library(dplyr)
library(vip)
library(tidyverse)
library(ggimage)
library(ggrepel)

rm(list=ls()) # optional clean-up

batterData625 <- bref_daily_batter("2024-03-01", "2024-08-22")  %>% woba_plus()
pitcherData625 <- bref_daily_pitcher("2024-01-01", "2024-12-31") %>% fip_plus()

pitcherData625[is.na(pitcherData625)] <- 0
batterData625[is.na(batterData625)] <- 0

starters <- pitcherData625 %>% filter(IP > 30, GS > 5) 
everydayBatters <- batterData625 %>% filter(PA >= 100)


ggplot(starters, aes(x = ERA, y = FIP)) +
  theme_bw() +
  geom_point() +
  geom_text(label = starters$Name, nudge_x = 0.2, nudge_y = 0.2, check_overlap = T) + 
  geom_hline(yintercept = mean(starters$FIP), linetype = "dashed")+
  geom_vline(xintercept = mean(starters$ERA), linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", size = 0.5, alpha = 0.5, se = FALSE) +
  labs(x = "ERA",
       y = "FIP",
       title = "ERA vs FIP; individual pitcher's luck with defense",
       subtitle = "Clusters can be seen by proximity between datapoints",
       caption = "By Kyle Bratzel | kbrat003@ucr.edu | Jun 25 2024") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))

fipMinusERA <- starters %>% 
  select(GB.FB, ERA, FIP, Name, Team) %>% 
  summarize(
    Name = Name,
    Team = Team,
    GBpFP = GB.FB,
    FmE = FIP - ERA
  )

ggplot(fipMinusERA, aes(x = FmE, y = GBpFP)) +
  theme_bw() +
  geom_point() +
  geom_text(label = starters$Name, nudge_x = -0.01, nudge_y = 0.01, check_overlap = T) + 
  geom_hline(yintercept = mean(starters$GBpFP), linetype = "dashed")+
  geom_vline(xintercept = mean(starters$FmE), linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", size = 0.5, alpha = 0.5, se = FALSE) +
  labs(x = "Pitcher Luck",
       y = "Groundball to Flyball ratio",
       title = "Pitcher Luck vs Groundball Rate",
       subtitle = "Luck defined as difference between FIP and ERA",
       caption = "By Kyle Bratzel | kbrat003@ucr.edu | Jun 25 2024") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))

team <- fipMinusERA %>% filter(Team == "Philadelphia")

ggplot(team, aes(x = FmE, y = GBpFP)) +
  theme_minimal() +
  geom_point() +
  geom_text(label = team$Name, nudge_x = 0.1, nudge_y = 0.01, check_overlap = T) + 
  geom_text(aes(x=1.2, y=0.45, label="Lucky"), size = 5, color = "green")+
  geom_text(aes(x=-0.1, y=0.55, label="Unlucky"), size = 5, color = "red")+
  geom_hline(yintercept = mean(fipMinusERA$GBpFP), linetype = "dashed")+
  geom_vline(xintercept = mean(fipMinusERA$FmE), linetype = "dashed") +
  labs(x = "Pitcher Luck",
       y = "Groundball to Flyball ratio",
       title = "Pitcher Luck vs Groundballs per Flyball",
       subtitle = "Luck defined as difference between FIP and ERA",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | Jun 26 2024") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))


lowcalculate_percentile <- function(value, dataset) {
  100 - (ecdf(dataset)(value) * 100)
}

lowcalculate_percentiles_for_attributes <- function(data, row_index, attributes) {
  percentiles <- sapply(attributes, function(attribute) {
    value <- data[row_index, attribute]
    percentile <- lowcalculate_percentile(value, data[[attribute]])
    return(percentile)
  })
  return(percentiles)
}

highcalculate_percentile <- function(value, dataset) {
  ecdf(dataset)(value) * 100
}

highcalculate_percentiles_for_attributes <- function(data, row_index, attributes) {
  percentiles <- sapply(attributes, function(attribute) {
    value <- data[row_index, attribute]
    percentile <- highcalculate_percentile(value, data[[attribute]])
    return(percentile)
  })
  return(percentiles)
}

suarez <- 156

lowAttributes <- c("ERA", "FIP", "WHIP", "BAbip", "wOBA_against", "uBB_perc", "LD", "PU")
highAttributes <- c("GB.FB", "SO_perc")

suarezlowpercentiles <- lowcalculate_percentiles_for_attributes(starters, suarez, lowAttributes)
suarezhighpercentiles <- highcalculate_percentiles_for_attributes(starters, suarez, highAttributes)

suarezpercentiles <- c(suarezlowpercentiles, suarezhighpercentiles)

suarezpercentile_df <- data.frame(
  Attribute = names(suarezpercentiles),
  Percentile = as.numeric(suarezpercentiles)
)

# Plot the bar chart
ggplot(suarezpercentile_df, aes(x = Attribute, y = Percentile)) +
  geom_bar(stat = "identity", fill = rgb(232,24,40,maxColorValue = 255)) +
  ylim(0, 100) + # Set y-axis limit from 0 to 100
  labs(title = "Ranger Suarez Statistical Percentiles",
       x = "Statistic",
       y = "Percentile",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | Jun 26 2024") +
  theme_minimal()

turnbull <- 139

turnbulllowpercentiles <- lowcalculate_percentiles_for_attributes(starters, turnbull, lowAttributes)
turnbullhighpercentiles <- highcalculate_percentiles_for_attributes(starters, turnbull, highAttributes)

turnbullpercentiles <- c(turnbulllowpercentiles, turnbullhighpercentiles)

turnbullpercentile_df <- data.frame(
  Attribute = names(turnbullpercentiles),
  Percentile = as.numeric(turnbullpercentiles)
)

# Plot the bar chart
ggplot(turnbullpercentile_df, aes(x = Attribute, y = Percentile)) +
  geom_bar(stat = "identity", fill = rgb(232,24,40,maxColorValue = 255)) +
  ylim(0, 100) + # Set y-axis limit from 0 to 100
  labs(title = "Spencer Turnbull Statistical Percentiles",
       x = "Statistic",
       y = "Percentile",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | Jun 26 2024") +
  theme_minimal()

sanchez <- 144

sanchezlowpercentiles <- lowcalculate_percentiles_for_attributes(starters, sanchez, lowAttributes)
sanchezhighpercentiles <- highcalculate_percentiles_for_attributes(starters, sanchez, highAttributes)

sanchezpercentiles <- c(sanchezlowpercentiles, sanchezhighpercentiles)

sanchezpercentile_df <- data.frame(
  Attribute = names(sanchezpercentiles),
  Percentile = as.numeric(sanchezpercentiles)
)

# Plot the bar chart
ggplot(sanchezpercentile_df, aes(x = Attribute, y = Percentile)) +
  geom_bar(stat = "identity", fill = rgb(232,24,40,maxColorValue = 255)) +
  ylim(0, 100) + # Set y-axis limit from 0 to 100
  labs(title = "Cristopher Sanchez Statistical Percentiles",
       x = "Statistic",
       y = "Percentile",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | Jun 26 2024") +
  theme_minimal()