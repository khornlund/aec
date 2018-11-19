library(tidyverse)
library(ggplot2)
library(ggthemes)
library(reshape2)

data_fn <- 'data/aec-hor-tpp-results.rds'
tpp_df <- readRDS(data_fn)

#------------------------------------------------------------------------------
# Util. funcs
#------------------------------------------------------------------------------

order_aus <- function(x) {
  return(ordered(x, levels = c('AUS', 'ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA')))
}

PLOT_SUBTITLE <- 'Australian Federal Elections 1996 - 2016'

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
year_state_tpp_df$Favour <- year_state_tpp_df$Spread > 0

# plot spread against year, facet by state
year_state_tpp_df %>%
  ggplot(aes(x=Year, y=Spread)) +
  geom_point(aes(color=Favour), show.legend = FALSE) + 
  theme_calc() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_color_manual(values=c('red', 'blue')) +
  facet_wrap(State ~ .) +
  labs(title = 'TPP Voter Spread',
       subtitle = PLOT_SUBTITLE) +
  ylab('Voter Spread (%)')

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
  mutate(Spread_S = LIB_P_S - ALP_P_S) %>%
  mutate(Favour = Spread_S > 0)

divisions_tpp_df %>%
  ggplot(aes(x=Year, y=Spread_S)) +
  geom_point(aes(color=Favour), show.legend = FALSE) + 
  theme_calc() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_color_manual(values=c('red', 'blue')) +
  facet_wrap(State ~ .) +
  labs(title = 'TPP Division Spread',
       subtitle = 'Australian Federal Elections 1996 - 2016') +
  ylab('Division Spread (%)')

#------------------------------------------------------------------------------
# Number of seats per state
#------------------------------------------------------------------------------

seats_by_state <- tpp_df %>%
  group_by(Year, State) %>%
  summarise(Seats = n()) %>%
  filter(Year == '2016')

#------------------------------------------------------------------------------
# scatter plot vote v div spread
#------------------------------------------------------------------------------

vote_div_df <- tpp_df %>%
  mutate(LIB_W = LIB_P > 0.5) %>%
  mutate(ALP_W = ALP_P > 0.5) %>%
  group_by(Year) %>%
  summarise(
    LIB_V_N = sum(LIB_N),
    ALP_V_N = sum(ALP_N),
    LIB_S_N = sum(LIB_W),
    ALP_S_N = sum(ALP_W)) %>%
  mutate(Spread_V = (LIB_V_N - ALP_V_N)/(LIB_V_N + ALP_V_N)) %>%
  mutate(Spread_S = (LIB_S_N - ALP_S_N)/(LIB_S_N + ALP_S_N))

vote_div_df %>%
  ggplot(aes(x = Spread_V, y = Spread_S, label = Year)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(size = 3) +
  geom_label(hjust=-0.2, vjust=0.6) +
  theme_calc() +
  xlim(c(-0.1, 0.1)) +
  ylim(c(-0.33, 0.33)) +
  ylab('Division Spread (%)') +
  xlab('Vote Spread (%)') +
  labs(
    title = 'TPP Vote v Division Spread',
    subtitle = PLOT_SUBTITLE)

#------------------------------------------------------------------------------
# Compare to real seat data
#------------------------------------------------------------------------------

# Let's compare to the real seat data
seat_df <- readRDS('data/aec-hor-seat-results.rds')

favour_func <- function(x) {
  if (x > 0) {
    return('LIB') 
  }
  return('ALP')
}

real_vote_div_df <- seat_df %>%
  mutate(
    Spread_V = LIB_TPP - ALP_TPP,
    Spread_S = (LIB_Seats - ALP_Seats) / Total_Seats * 100)

real_vote_div_df$Favour = map(.x = real_vote_div_df$Spread_S, .f = favour_func)

real_vote_div_df %>%
  ggplot(aes(x = Spread_V, y = Spread_S, color = factor(Favour))) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(size = 3) +
  scale_color_manual(values=c('blue', 'red')) +
  ylab('Division Spread (%)') +
  xlab('Vote Spread (%)') +
  labs(
    title = 'TPP Vote v Division Spread',
    subtitle = PLOT_SUBTITLE)

