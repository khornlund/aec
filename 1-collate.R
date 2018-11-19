library(tidyverse)

# read .tsv files and collate into master .rds

data_f <- 'data'
pattern <- '-aec-hor-results.tsv'
years <- c('1996', '1998', '2001', '2004', '2007', '2010', '2013', '2016')

read_aec <- function(year) {
  fn <- paste0(data_f, '/', year, pattern)
  df <- read_delim(fn, delim = '\t')
  df$Year <- year
  return(df)
}

df <- map_dfr(.x = years, .f = read_aec) %>% na.omit()
saveRDS(df, file = paste(data_f, 'aec-hor-results.rds', sep = '/'))
