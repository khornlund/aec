library(tidyverse)
library(ggplot2)
library(ggthemes)
library(reshape2)
source("2-eda.R")

data_fn <- 'data/aec/aec-hor-tpp-results.rds'
tpp_df  <- readRDS(data_fn)

calc_waste <- function(n, p) {
  waste <- ifelse(p <= 0.5, n, n * (p - 0.5))
  return(waste)
}

favour_func <- function(x) {
  if (x > 0) {
    return('LIB') 
  }
  return('ALP')
}

gap_df <- tpp_df %>%
  mutate(
    LIB_Waste = calc_waste(LIB_N, LIB_P),
    ALP_Waste = calc_waste(ALP_N, ALP_P)) %>%
  group_by(
    Year, State) %>%
  summarise(
    LIB_N = sum(LIB_N),
    ALP_N = sum(ALP_N),
    Abs_Waste_Gap = sum(ALP_Waste) - sum(LIB_Waste),
    Total = sum(Total)) %>%
  mutate(
    `TPP Vote Spread` = (LIB_N - ALP_N) / Total,
    `Efficiency Gap` = Abs_Waste_Gap / Total)

aus_gap_df <- gap_df %>%
  group_by(Year) %>%
  summarise(
    LIB_N = sum(LIB_N),
    ALP_N = sum(ALP_N),
    Abs_Waste_Gap = sum(Abs_Waste_Gap),
    Total = sum(Total)) %>%
  mutate(
    `TPP Vote Spread` = (LIB_N - ALP_N) / Total,
    `Efficiency Gap` = Abs_Waste_Gap / Total,
    State = 'AUS')

gap_df <- bind_rows(gap_df, aus_gap_df) %>%
  mutate(State = order_aus(State))
gap_df$`TPP Favour` <- map_chr(.x = gap_df$`TPP Vote Spread`, .f = favour_func)

gap_df %>%
  filter(State %in% c('AUS', 'NSW', 'VIC', 'QLD', 'WA', 'SA')) %>%
  ggplot(aes(x=`TPP Vote Spread`, y=`Efficiency Gap`, color = `TPP Favour`)) +
  geom_point(size=2) +
  scale_color_manual(values=c('red', 'blue')) +
  xlim(c(-0.25, +0.25)) +
  ylim(c(-0.25, +0.25)) +
  theme_calc() +
  facet_wrap(State ~ .) +
  labs(title = 'TPP Vote Spread v Efficiency Gap',
       subtitle = subtitle_years(1996, 2016),
       x = 'TPP Vote Spread (%)',
       y = 'Efficiency Gap (%)')

