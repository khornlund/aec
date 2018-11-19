library(tidyverse)
library(rvest)
library(stringr)

url <- 'https://en.wikipedia.org/wiki/List_of_Australian_federal_elections'

h <- read_html(url)
tab <- h %>% html_nodes("table")
tab <- tab[[3]]
tab <- tab %>% html_table(fill = TRUE)

df <- tab[-c(1, 2, 3),]
names(df) <- c(
  'Year',
  'ALP_Primary',
  'LIB_Primary',
  'OTH_Primary',
  'ALP_TPP',
  'LIB_TPP',
  'ALP_Seats',
  'LIB_Seats',
  'OTH_Seats',
  'Total_Seats'
)

extract_year <- function(s) {
  match <- str_extract(s, '\\d{4}')
  return(as.numeric(match))
}

to_numeric <- function(x) {
  str <- str_replace(x, '%', '')
  return(as.numeric(str))
}

df$Year <- map(.x = df$Year, .f = extract_year)
df <- df %>% filter(Year >= 1919)
numeric_df <- map_dfc(.x = df, .f = to_numeric)

saveRDS(numeric_df, file = 'data/aec-hor-seat-results.rds')
write_delim(numeric_df, path = 'data/aec-hor-seat-results.tsv', delim = '\t')
