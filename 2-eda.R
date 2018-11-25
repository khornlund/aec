library(tidyverse)
library(ggplot2)
library(ggthemes)
library(reshape2)

data_fn <- 'data/aec/aec-hor-tpp-results.rds'
tpp_df <- readRDS(data_fn)

#------------------------------------------------------------------------------
# Util. funcs + Constants
#------------------------------------------------------------------------------

order_aus <- function(x) {
  return(ordered(x, levels = c('AUS', 'ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA')))
}

favour_func <- function(x) {
  if (x > 0) {
    return('LIB') 
  }
  return('ALP')
}

subtitle_years <- function(yfrom, yto) {
  return (paste0('Australian Federal Elections ', yfrom, ' - ', yto))
}

#------------------------------------------------------------------------------
# Total vote spread by state
#------------------------------------------------------------------------------

# compute LIB/ALP difference by year
year_state_tpp_df <- tpp_df %>%
  group_by(Year, State) %>%
  summarise(
    LIB_N = sum(LIB_N),
    ALP_N = sum(ALP_N))

# get stats for Aus as a whole
aus_tpp_df <- year_state_tpp_df %>%
  group_by(Year) %>%
  summarise(
    LIB_N = sum(LIB_N),
    ALP_N = sum(ALP_N)) %>%
  mutate(State = 'AUS')

year_state_tpp_df <- bind_rows(year_state_tpp_df, aus_tpp_df)
year_state_tpp_df$State <- order_aus(year_state_tpp_df$State)

year_state_tpp_df$Spread <- (year_state_tpp_df$LIB_N - year_state_tpp_df$ALP_N) / (year_state_tpp_df$LIB_N + year_state_tpp_df$ALP_N) * 100
year_state_tpp_df$Favour <- map_chr(.x = year_state_tpp_df$Spread, .f = favour_func)

# plot spread against year, facet by state
year_state_tpp_df %>%
  ggplot(aes(x=Year, y=Spread)) +
  geom_point(aes(color=Favour)) + 
  theme_calc() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_color_manual(values=c('red', 'blue')) +
  facet_wrap(State ~ .) +
  labs(title = 'TPP Voter Spread',
       subtitle = subtitle_years(1996, 2016)) +
  ylab('TPP Voter Spread (%)')

#------------------------------------------------------------------------------
# How does TPP voting predict seat wins
#------------------------------------------------------------------------------

divisions_tpp_df <- tpp_df %>%
  mutate(LIB_W = LIB_P > 0.5) %>%
  mutate(ALP_W = ALP_P > 0.5) %>%
  group_by(Year, State) %>%
  summarise(
    LIB_N_S = sum(LIB_W),
    ALP_N_S = sum(ALP_W)) %>%
  mutate(LIB_P_S = LIB_N_S / (LIB_N_S + ALP_N_S)) %>%
  mutate(ALP_P_S = ALP_N_S / (LIB_N_S + ALP_N_S))

aus_divisions_tpp_df <- divisions_tpp_df %>%
  group_by(Year) %>%
  summarise(
    LIB_N_S = sum(LIB_N_S),
    ALP_N_S = sum(ALP_N_S)) %>%
  mutate(State = 'AUS')

divisions_tpp_df <- bind_rows(divisions_tpp_df, aus_divisions_tpp_df) %>%
  mutate(State = order_aus(State)) %>%
  mutate(LIB_P_S = LIB_N_S / (LIB_N_S + ALP_N_S) * 100) %>%
  mutate(ALP_P_S = ALP_N_S / (LIB_N_S + ALP_N_S) * 100) %>%
  mutate(Spread_S = LIB_P_S - ALP_P_S)
divisions_tpp_df$Favour = map_chr(.x = divisions_tpp_df$Spread_S, .f = favour_func)

divisions_tpp_df %>%
  ggplot(aes(x=Year, y=Spread_S)) +
  geom_point(aes(color=Favour)) + 
  theme_calc() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_color_manual(values=c('red', 'blue')) +
  facet_wrap(State ~ .) +
  labs(title = 'TPP Division Spread',
       subtitle = subtitle_years(1996, 2016)) +
  ylab('TPP Division Spread (%)')

#------------------------------------------------------------------------------
# Number of seats per state
#------------------------------------------------------------------------------

seats_by_state <- tpp_df %>%
  group_by(Year, State) %>%
  summarise(Seats = n()) %>%
  filter(Year == '2016')

#------------------------------------------------------------------------------
# Compare to real seat data
#------------------------------------------------------------------------------

# Let's compare to the real seat data
wiki_df <- readRDS('data/wiki/wiki-hor-seat-results.rds')

real_vote_div_df <- wiki_df %>%
  mutate(
    Spread_V = LIB_TPP - ALP_TPP,
    Spread_S = (LIB_Seats - ALP_Seats) / Total_Seats * 100)

real_vote_div_df$Winner = as.factor(map_chr(.x = real_vote_div_df$Spread_S, .f = favour_func))

real_vote_div_df %>%
  ggplot(aes(x = Spread_V, y = Spread_S)) +
  geom_smooth(method='lm', color = 'grey') +
  geom_point(aes(color = Winner), size=2) +
  scale_color_manual(values=c('red', 'blue')) +
  theme_calc() +
  ylab('Seat Spread (%)') +
  xlab('Vote Spread (%)') +
  labs(
    title = 'TPP Vote v Seat Spread',
    subtitle = subtitle_years(1919, 2016))

#------------------------------------------------------------------------------
# Trends by decade
#------------------------------------------------------------------------------

get_year_grp <- function(year) {
  span <- 30
  decades <- seq(2020, 1900, -span)
  for (decade in decades) {
    if (year > decade)
      return(str_c(decade, ' - ', decade+span-1))
  }
}

real_vote_div_df %>%
  mutate(YearSpan = map_chr(.x = Year, .f = get_year_grp)) %>%
  ggplot(aes(x = Spread_V, y = Spread_S, color = Winner)) +
  geom_point(size=2) +
  scale_color_manual(values=c('red', 'blue')) +
  theme_calc() +
  ylab('Seat Spread (%)') +
  xlab('Vote Spread (%)') +
  labs(
    title = 'TPP Vote v Seat Spread',
    subtitle = subtitle_years(1919, 2016)) +
  facet_wrap(YearSpan ~ .)

