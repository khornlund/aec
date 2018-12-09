library(tidyverse)
library(ggplot2)
library(ggthemes)
library(reshape2)
source("2-eda.R")
setwd("~/R/AEC")

jiggle <- function(tpp_df, year, p_jiggle, mu_size, sd_size) {
  
  df <- tpp_df %>% filter(Year == year)
  
  # Select divisions to extract voters from, and how many voters to shuffle
  extract_i <- sample(x = c(0, 1), size = nrow(df), replace = TRUE, p = c(1-p_jiggle, p_jiggle))
  extract_s <- rnorm(n = nrow(df), mean = mu_size, sd = sd_size)
  
  # Get amounts of voters to move
  df$LIB_M <- extract_i * pmin(extract_s * df$LIB_P, df$LIB_N)
  df$ALP_M <- extract_i * pmin(extract_s * df$ALP_P, df$ALP_N)
  
  # Remove voters
  df$LIB_N_removed <- df$LIB_N - df$LIB_M
  df$ALP_N_removed <- df$ALP_N - df$ALP_M
  
  # Select divisions to move voters to
  destination_i <- sample(seq(nrow(df)))
  df$LIB_A <- df$LIB_M[destination_i]
  df$ALP_A <- df$ALP_M[destination_i]
  df$LIB_N_added <- df$LIB_N_removed + df$LIB_A
  df$ALP_N_added <- df$ALP_N_removed + df$ALP_A
  
  df$LIB_Delta <- df$LIB_N_added - df$LIB_N
  df$ALP_Delta <- df$ALP_N_added - df$ALP_N
  
  jig_df <- data_frame(
    'Division' = df$Division, 
    'LIB_N' = df$LIB_N_added, 
    'ALP_N' = df$ALP_N_added) %>%
    mutate(
      Total = LIB_N + ALP_N,
      LIB_P = LIB_N / Total, # risk: divide-by-zero => NA values
      ALP_P = ALP_N / Total, # risk: divide-by-zero => NA values
      LIB_W = LIB_P > 0.5)
  
  df$LIB_W <- df$LIB_P > 0.5
  
  ALP_W_delta <- sum(jig_df$LIB_W, na.rm = TRUE) - sum(df$LIB_W, na.rm = TRUE)
  return(ALP_W_delta)
}

run_sim <- function(year, tpp_df, p_jiggle, mu_size, sd_size, N) {
  results <- replicate(N, jiggle(tpp_df, year, p_jiggle, mu_size, sd_size))
  df <- data_frame(
    'year' = year, 
    'p_jiggle' = p_jiggle,
    'mu_jiggle' = mu_size,
    'sd_jiggle' = sd_size,
    'mean_delta' = mean(results, na.rm = TRUE)
  )
  gc()
  return(df)
}

data_fn <- 'data/aec/aec-hor-tpp-results.rds'
tpp_df <- readRDS(data_fn)

set.seed(2)

years    <- unique(tpp_df$Year)   # year to simulate
p_jiggle <- 0.70   # probability of a district having its pop shuffled
mu_size  <- 30000  # mean size of pop to shuffle
sd_size  <- 5000   # sd of size to shuffle
N        <- 5000   # replications

df <- map_dfr(
  .x       = years, 
  .f       = run_sim, 
  tpp_df   = tpp_df,
  p_jiggle = p_jiggle, 
  mu_size  = mu_size, 
  sd_size  = sd_size, 
  N        = N)

write_csv(x = df, path = 'outputs/30000.csv')

a.df <- read_csv('outputs/25000.csv')
b.df <- read_csv('outputs/30000.csv')
c.df <- read_csv('outputs/35000.csv')

d.df <- bind_rows(a.df, b.df, c.df)

write_csv(x = d.df, path = 'data/montecarlo/map_randomisation.csv')

sim.df <- read_csv('data/montecarlo/map_randomisation.csv')

sim.df$Favour <- map_chr(.x = sim.df$mean_delta, .f = favour_func)

sim.df %>%
  filter(mu_jiggle == 30000) %>%
  ggplot(aes(x=factor(year), y=mean_delta, fill=Favour)) +
  geom_histogram(stat = 'identity', position = 'dodge') +
  theme_calc() +
  scale_color_manual(values=c('red', 'blue')) +
  labs(title = 'Mean Vote Changes due to Map Randomisation',
       subtitle = subtitle_years(1996, 2016),
       x = 'Year',
       y = 'Mean Change in Won Seats (+LIB / -ALP)')
  
  
  
