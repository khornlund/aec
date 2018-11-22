library(tidyverse)
library(ggplot2)
library(ggthemes)
library(reshape2)

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
    Delta = sum(ALP_Waste) - sum(LIB_Waste),
    Total = sum(Total)) %>%
  mutate(
    Gap = Delta / Total)
gap_df$Favour <- map_chr(.x = gap_df$Gap, .f = favour_func)

gap_df %>%
  ggplot(aes(x=Year, y=Gap, color = Favour)) +
  geom_point(size = 2) +
  theme_calc() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  scale_color_manual(values=c('red', 'blue')) +
  labs(
    title = '',
    subtitle = '',
    x = 'Year',
    y = 'Efficiency Gap (%)') +
  facet_wrap(State ~ .)
