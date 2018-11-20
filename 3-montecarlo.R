library(tidyverse)
library(ggplot2)
library(ggthemes)
library(reshape2)
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

data_fn <- 'data/aec/aec-hor-tpp-results.rds'
tpp_df <- readRDS(data_fn)

set.seed(1)

year     <- 2010 # year to simulate
p_jiggle <- 0.70 # probability of a district having its pop shuffled
#mu_size  <- 20000 # mean size of pop to shuffle
sd_size  <- 5000 # sd of size to shuffle

for (mu_size in seq(20000, 35000, 5000)) {
  results <- replicate(10000, jiggle(tpp_df, year, p_jiggle, mu_size, sd_size))
  print(str_c('Mu=', mu_size, ' , mean seat change=', mean(results, na.rm = TRUE)))
}




