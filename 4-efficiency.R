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

melt_df <- bind_rows(gap_df, aus_gap_df) %>%
  melt(
    id.vars = c("Year", "State"), 
    measure.vars = c("TPP Vote Spread", "Efficiency Gap"),
    variable.name = "Measure",
    value.name = "Value")

melt_df$Favour <- map_chr(.x = melt_df$Value, .f = favour_func)
melt_df$State  <- order_aus(melt_df$State)
melt_df$Year   <- as.factor(melt_df$Year)

melt_df %>%
  ggplot(aes(x=Year, y=Value, color = Favour, shape = Measure)) +
  geom_point(size = 3, alpha = 0.85) +
  theme_calc() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_color_manual(values=c('red', 'blue')) +
  scale_shape_manual(values=c(0, 1)) +
  labs(
    title = 'Efficiency Gap and TPP Vote Spread',
    subtitle = subtitle_years(1996, 2016),
    x = 'Year',
    y = 'Difference (%)') +
  facet_wrap(State ~ .)


# TAS 2013

tas_2013_df <- tpp_df %>%
  filter(Year == 2013) %>%
  filter(State == 'TAS')

print(tas_2013_df)

# VIC 1996, 1998, 2007, 2016

melt_df %>%
  filter(State == 'VIC') %>%
  ggplot(aes(x=Year, y=Value, color = Favour, shape = Measure)) +
  geom_point(size = 3, alpha = 0.85) +
  theme_calc() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_color_manual(values=c('red', 'blue')) +
  scale_shape_manual(values=c(0, 1)) +
  labs(
    title = 'VIC Efficiency Gap and TPP Vote Spread',
    subtitle = subtitle_years(1996, 2016),
    x = 'Year',
    y = 'Difference (%)')
