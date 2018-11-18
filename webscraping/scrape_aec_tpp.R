library(dplyr)
library(rvest)
library(stringr)

scrape_aec_tpp <- function(url, table_index, rstart, rend, colnames) {
  
  # Read URL
  h <- read_html(url)
  tab <- h %>% html_nodes("table")
  tab <- tab[[table_index]]
  tab <- tab %>% html_table(fill = TRUE)
  
  # convert to data.frame and relabel columns
  data.df <- tab[rstart : rend , ]
  names(data.df) <- colnames
  rownames(data.df) <- NULL
  
  # clean columns
  data.df$`ALP_P` <- data.df$`ALP_P` %>%
    as.numeric() / 100
  
  data.df$`LIB_P` <- data.df$`LIB_P` %>%
    as.numeric() / 100
  
  data.df$`LIB_N` <- data.df$`LIB_N` %>%
    str_replace(",", "") %>%
    as.numeric()
  
  data.df$`ALP_N` <- data.df$`ALP_N` %>%
    str_replace(",", "") %>%
    as.numeric()
  
  data.df$Total <- data.df$Total %>% 
    str_replace(",", "") %>%
    as.numeric()
  
  data.df <- subset(data.df, select = -c(Swing))
  
  message("Returning...")
  
  return(data.df)
}

get_colnames <- function(first_party) {
  colnames <- c(
    "Division", 
    "State",
    "ALP_N", 
    "ALP_P",
    "LIB_N", 
    "LIB_P",
    "Total", 
    "Swing")
  
  if (first_party == "ALP")
    order <- colnames
  else
    order <- colnames[c(1, 2, 5, 6, 3, 4, 7, 8)]
  
  return(order)
}

years <- c(2016, 2013, 2010, 2007, 2004)

urls <- c(
  "https://results.aec.gov.au/20499/Website/HouseTppByDivision-20499-NAT.htm",
  "https://results.aec.gov.au/17496/Website/HouseTppByDivision-17496-NAT.htm",
  "https://results.aec.gov.au/15508/Website/HouseTppByDivision-15508-NAT.htm",
  "https://results.aec.gov.au/13745/Website/HouseTppByDivision-13745-NAT.htm",
  "https://results.aec.gov.au/12246/results/HouseTppByDivision-12246-NAT.htm")

# indices of the table in the web page
table_indices <- c(1, 5, 5, 4, 4)

# order of parties in table
first_parties <- c("LIB", "ALP", "ALP", "LIB", "LIB")

# rows to import (same for every table for now - could change later)
rstart <- rep(2, 5)
rend   <- rep(151, 5)

sources <- data.frame(
  years, 
  urls, 
  table_indices, 
  first_parties, 
  rstart, 
  rend)

names(sources) <- c("Year", "URL", "Table Index", "First Party", "First Row", "Last Row")

for (i in 1:nrow(sources)) {
  source <- sources[i , ]
  temp.df <- NULL
  temp.df <- scrape_aec_tpp(
    url = as.character(source$`URL`),
    table_index = source$`Table Index`,
    rstart = source$`First Row`,
    rend = source$`Last Row`,
    colnames = get_colnames(source$`First Party`))

  #reorder columns
  temp.df <- temp.df[c(
    "Division",
    "State",
    "LIB_N",
    "LIB_P",
    "ALP_N",
    "ALP_P",
    "Total") ]
  
  filename <- paste0("data/", source$Year, "-aec-hor-results.tsv")
  message(paste0("Writing to: ", filename))
  write.table(
    x = temp.df, 
    file = filename,
    sep = "\t",
    row.names = FALSE)

  temp.df$Year = source$Year

  if (i == 1)
    data.df <- temp.df
  else
    data.df <- rbind(data.df, temp.df)
}

# filename <- "data/aec_tpp.csv"
# #saveRDS(data.df, file = filename)
# write.csv(data.df, file = filename, row.names=FALSE)
