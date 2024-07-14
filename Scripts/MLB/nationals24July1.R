library(baseballr)
library(dplyr)
library(vip)
library(tidyverse)
library(ggimage)
library(ggrepel)

rm(list=ls()) # optional clean-up

batterData <- bref_daily_batter("2024-01-01", "2024-12-31")  %>% woba_plus()
pitcherData <- bref_daily_pitcher("2024-01-01", "2024-12-31") %>% fip_plus()

pitcherData[is.na(pitcherData)] <- 0
batterData[is.na(batterData)] <- 0

averages <- batterData %>% 
  summarise(avgOBP = mean(OBP),
            avgSLG = mean(SLG),
            avgOPS = mean(OPS),
            avgBA = mean(BA),
            avgSOP = mean(sum(SO)/sum(PA))
            )

batterData %>% filter


batters <- batterData %>% 
  mutate(Team = paste(Team, str_sub(batterData$Level, -3), sep = "")) %>% 
  filter(PA > 100) %>% 
  group_by(Team) %>% 
  summarise(OBPaa = mean(OBP) - averages$avgOBP,
            SLGaa = mean(SLG) - averages$avgSLG,
            OPSaa = mean(OPS) - averages$avgOPS,
            BAaa = mean(BA) - averages$avgBA,
            SOPaa = -100*(sum(SO)/sum(PA) - averages$avgSOP)
            )

batters <- batters[-(grep(",", batters$Team)), ]
nationalsBatters <- batterData %>% filter(Team == "Washington", PA > 100)
nationalsPitchers <- pitcherData %>% filter(Team == "Washington")

bullpenPlayer <- pitcherData %>% filter(IP - G < 10, G > 5, IP >= 15)
natsBullpen <- nationalsPitchers %>% filter(IP - G < 10, G > 3)

bullpen <- bullpenPlayer %>% 
  mutate(Team = paste(Team, str_sub(bullpenPlayer$Level, -3), sep = "")) %>% 
  group_by(Team) %>% 
  summarise(ERA = 9*sum(ER)/sum(IP),
            SOP = 100*sum(SO)/sum(BF))

bullpen <- bullpen[-(grep(",", bullpen$Team)), ]

ggplot(natsBullpen, aes(x = SO9, y = wOBA_CON_against)) +
  theme_minimal() +
  geom_point(color = rgb(171,0,3,maxColorValue = 255)) +
  geom_text(label = natsBullpen$Name, nudge_x = 0, nudge_y = 0.0075, check_overlap = T, , color = rgb(20,34,90,maxColorValue = 255)) +
  geom_text(aes(x=13.5, y=0.3, label="Strong"), size = 5, color = rgb(20,34,90,maxColorValue = 255))+
  geom_text(aes(x=6, y=0.4, label="Lit up"), size = 5, color = rgb(171,0,3,maxColorValue = 255))+
  geom_hline(yintercept = mean(bullpenPlayer$wOBA_CON_against), linetype = "dashed", color = rgb(20,34,90,maxColorValue = 255)) +
  geom_vline(xintercept = mean(bullpenPlayer$SO9), linetype = "dashed", color = rgb(20,34,90,maxColorValue = 255)) +
  geom_smooth(method = "lm", color = rgb(20,34,90,maxColorValue = 255), size = 0.5, alpha = 0.5, se = FALSE)+
  labs(x = "Strikeouts per 9",
       y = "wOBA on Balls in Play",
       title = "Strikeout Pitchers with Balls in Play",
       subtitle = "Nationals bullpen through June 2024",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | July 2 2024") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + 
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))



ggplot(nationalsBatters, aes(x = SLG, y = wOBA)) +
  theme_minimal() +
  geom_point(color = rgb(171,0,3,maxColorValue = 255)) +
  geom_text(label = nationalsBatters$Name, nudge_x = 0.005, nudge_y = 0.005, check_overlap = T, color = rgb(20,34,90,maxColorValue = 255)) +
  geom_text(aes(x=0.5, y=0.325, label="Crushing"), size = 5, color = rgb(20,34,90,maxColorValue = 255))+
  geom_text(aes(x=.28, y=0.3, label="Struggling"), size = 5, color = rgb(171,0,3,maxColorValue = 255))+
  geom_hline(yintercept = mean(batterData$wOBA), linetype = "dashed", color = rgb(20,34,90,maxColorValue = 255)) +
  geom_vline(xintercept = mean(batterData$SLG), linetype = "dashed", color = rgb(20,34,90,maxColorValue = 255)) +
  geom_smooth(method = "lm", color = rgb(20,34,90,maxColorValue = 255), size = 0.5, alpha = 0.5, se = FALSE)+
  labs(x = "SLG%",
       y = "wOBA",
       title = "Power hitters getting on base",
       subtitle = "Nationals hitters through June 2024",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | July 2 2024",
       color = rgb(20,34,90,maxColorValue = 255)) +
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

winker <- 59

pitcherLowAttributes <- c("ERA", "FIP", "WHIP", "BAbip", "wOBA_against", "uBB_perc", "LD", "PU")
pitcherHighAttributes <- c("GB.FB", "SO_perc")

batterHighAttributes <- c("OBP", "SLG", "OPS", "wOBA", "wOBA_CON", "BB_perc", "HR_perc", "SB_perc", "SB", "BA")
batterLowAttributes <- c("SO_perc")

everydayBats <- batterData %>% filter(PA >= 100) %>% 
  mutate(SO_perc = SO/PA,
         BB_perc = BB/PA,
         HR_perc = HR/PA,
         SB_perc = SB/(SB+CS))

winkerLowpercentiles <- lowcalculate_percentiles_for_attributes(everydayBats, winker, batterLowAttributes)
winkerHighpercentiles <- highcalculate_percentiles_for_attributes(everydayBats, winker, batterHighAttributes)

winkerpercentiles <- c(winkerLowpercentiles, winkerHighpercentiles)

winkerpercentile_df <- data.frame(
  Attribute = names(winkerpercentiles),
  Percentile = as.numeric(winkerpercentiles)
)

# Plot the bar chart
ggplot(winkerpercentile_df, aes(x = Attribute, y = Percentile)) +
  geom_bar(stat = "identity", fill = rgb(20,34,90,maxColorValue = 255)) +
  ylim(0, 100) + # Set y-axis limit from 0 to 100
  labs(title = "Jesse Winker Statistical Percentiles",
       x = "Statistic",
       y = "Percentile",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | July 2 2024") +
  theme_minimal()

finnegan <- 167

finneganlowpercentiles <- lowcalculate_percentiles_for_attributes(bullpenPlayer, finnegan, pitcherLowAttributes)
finneganhighpercentiles <- highcalculate_percentiles_for_attributes(bullpenPlayer, finnegan, pitcherHighAttributes)

finneganpercentiles <- c(finneganlowpercentiles, finneganhighpercentiles)

finneganpercentile_df <- data.frame(
  Attribute = names(finneganpercentiles),
  Percentile = as.numeric(finneganpercentiles)
)

finneganpercentile_df

# Plot the bar chart
ggplot(finneganpercentile_df, aes(x = Attribute, y = Percentile)) +
  geom_bar(stat = "identity", fill = rgb(171,0,3,maxColorValue = 255)) +
  ylim(0, 100) + # Set y-axis limit from 0 to 100
  labs(title = "Kyle Finnegan Statistical Percentiles",
       x = "Statistic",
       y = "Percentile",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | July 2 2024") +
  theme_minimal()

harvey <- 70

harveylowpercentiles <- lowcalculate_percentiles_for_attributes(bullpenPlayer, harvey, pitcherLowAttributes)
harveyhighpercentiles <- highcalculate_percentiles_for_attributes(bullpenPlayer, harvey, pitcherHighAttributes)

harveypercentiles <- c(harveylowpercentiles, harveyhighpercentiles)

harveypercentile_df <- data.frame(
  Attribute = names(harveypercentiles),
  Percentile = as.numeric(harveypercentiles)
)

harveypercentile_df

# Plot the bar chart
ggplot(harveypercentile_df, aes(x = Attribute, y = Percentile)) +
  geom_bar(stat = "identity", fill = rgb(171,0,3,maxColorValue = 255)) +
  ylim(0, 100) + # Set y-axis limit from 0 to 100
  labs(title = "Hunter Harvey Statistical Percentiles",
       x = "Statistic",
       y = "Percentile",
       caption = "By Kyle Bratzel | @kylebratzel | kbrat003@ucr.edu | July 13 2024") +
  theme_minimal()
