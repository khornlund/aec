library(tidyverse)
library(ggplot2)

data_fn <- 'data/aec-hor-results.rds'
df <- readRDS(data_fn)

# compute LIB/ALP difference by year
df.grp.yr.st <- df %>%
  group_by(Year, State) %>%
  summarise(
    LIB_N = sum(LIB_N),
    ALP_N = sum(ALP_N))

# get stats for Aus as a whole
df.aus <- df.grp.yr.st %>%
  group_by(Year) %>%
  summarise(
    LIB_N = sum(LIB_N),
    ALP_N = sum(ALP_N)) %>%
  mutate(State = 'AUS')

df.grp.yr.st <- bind_rows(df.grp.yr.st, df.aus)
df.grp.yr.st$State <- ordered(
  df.grp.yr.st$State, levels = c('AUS', 'ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA'))

df.grp.yr.st$Spread <- (df.grp.yr.st$LIB_N - df.grp.yr.st$ALP_N) / (df.grp.yr.st$LIB_N + df.grp.yr.st$ALP_N) * 100
df.grp.yr.st$Favour <- df.grp.yr.st$Spread > 0

# plot spread against year, facet by state
df.grp.yr.st %>%
  ggplot(aes(x=Year, y=Spread)) +
  geom_point(aes(color=Favour), show.legend = FALSE) + 
  theme_calc() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_color_manual(values=c('red', 'blue')) +
  facet_wrap(State ~ .) +
  labs(title = 'Australian Federal Election Spread by Year',
       subtitle = '1996 - 2016') +
  ylab('Spread (%) (LIB+)')


