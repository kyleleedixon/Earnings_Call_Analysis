###############################
#                             #
# Author: Kyle Dixon          #
# Created: July 28 2021       #
# Title: Text Data Processing #
#                             #
###############################

## Load libraries
library(dplyr)
library(httr)
library(quantmod)
library(readr)
library(rvest)
library(SnowballC)
library(sqldf)
library(tidytext)
library(tidyr)
library(tm)
library(xml2)

# Clear the environment
base::rm(list = base::ls())

## Initialize processed data list, start count, and available tickers
processed3 <- base::list()
iteration <- 1
available_tickers <- TTR::stockSymbols(exchange = c("AMEX", "NASDAQ", "NYSE"))$Symbol
available_tickers <- available_tickers[!(available_tickers %in% c("MON", "EDR", "AAN", "CHK", "GPOR", "IR", "PAY", "XL", "ACET"))]

## Loop through each file and process the raw data
for (i in c(1:5)) {
  
  i <- base::list.files("confirmed_lawsuits")[i]
  
  ## Read in .txt file as a vector
  #raw_text <- dplyr::pull(readr::read_delim(base::paste0("MAEC_Dataset/", i, "/text.txt"), delim = "\r\n", show_col_types = F), 1)
  raw_text <- dplyr::pull(readr::read_delim(base::paste0("confirmed_lawsuits/", i), delim = "\n\n", show_col_types = F), 1)
  
  ## CReate corpus
  corpus <- tm::Corpus(tm::VectorSource(raw_text))
  
  ## Case match and stem
  corpus <- tm::tm_map(corpus, tolower)
  corpus <- tm::tm_map(corpus, stemDocument)
  
  ## Create the document term matrix
  dtm <- tm::removeSparseTerms(tm::DocumentTermMatrix(corpus, control = base::list(minWordLength = 3)), sparse = 0.98)
  
  ## Summarize sentiments based on Loughran classification
  sentiments <- tidytext::tidy(dtm) 
  
  if (base::nrow(sentiments) > 0) {
    
    sentiments <- sentiments %>% dplyr::inner_join(tidytext::get_sentiments("loughran"), by = c(term = "word")) %>% 
      dplyr::count(sentiment, wt = count) %>% tidyr::spread(sentiment, n, fill = 0) 
    
    ## Check and adjust columns if neccessary
    if (!("negative" %in% base::colnames(sentiments))) {
      sentiments$negative <- 0
    }
    if (!("positive" %in% base::colnames(sentiments))) {
      sentiments$positive <- 0
    }
    if (!("uncertainty" %in% base::colnames(sentiments))) {
      sentiments$uncertainty <- 0
    }
    if (!("litigious" %in% base::colnames(sentiments))) {
      sentiments$litigious <- 0
    }
    if (!("constraining" %in% base::colnames(sentiments))) {
      sentiments$constraining <- 0
    }
    if (!("superfluous" %in% base::colnames(sentiments))) {
      sentiments$superfluous <- 0
    }
    
    ## Calculate overall sentiment
    sentiments <- sentiments %>% dplyr::mutate(sentiment = positive - negative) %>% 
      dplyr::arrange(sentiment)
    
    ## Add descriptor columns
    sentiments$date <- base::as.Date(base::substr(i, 1, 8), "%Y%m%d")
    sentiments$ticker <- base::substr(i, 10, base::nchar(i) - 4)
    
    if (base::substr(i, 10, base::nchar(i) - 4) %in% available_tickers) {
      
      ## Get DoD return for specific date and ticker
      prices <- quantmod::getSymbols(base::substr(i, 10, base::nchar(i) - 4), from = base::as.Date(base::substr(i, 1, 8), "%Y%m%d"), 
                                     to = base::as.Date(base::substr(i, 1, 8), "%Y%m%d") + 1, auto.assign = F)
      sentiments$return <- base::as.numeric((prices[, 4] - prices[, 1]) / prices[, 4])
      
    } else {
      
      sentiments$return <- base::as.numeric(NA)
      
    }
    
    ## Append to processed data set
    processed3 <- base::append(processed3, base::list(sentiments))
  }
  
  ## Print iteration count
  #base::print(base::paste0("Completed ", iteration, " of ", base::length(base::list.files("MAEC_Dataset"))))
  #base::print(base::paste0("Completed ", iteration, " of ", base::length(2070:3443)))
  iteration <- iteration + 1
}

## Bind processed data into a single data frame
processed3 <- dplyr::bind_rows(processed3)

## Scrape Stanford Law website
cases_filed <- base::list()
for (i in 58:71) {
  
  ## Scrape tables
  content <- xml2::read_html(base::paste0("https://securities.stanford.edu/filings.html?page=", i)) %>% rvest::html_table(fill = T)
  cases_filed <- base::append(cases_filed, base::list(content[[1]]))
  
}

## Clean and format
cases_filed <- dplyr::bind_rows(cases_filed)
cases_filed <- cases_filed[, c(2, 5)]
base::names(cases_filed) <- c("date", "ticker")
cases_filed$date <- base::as.Date(cases_filed$date, "%m/%d/%Y")

## Find quarter dates around lawsuit
cases_filed$before <- base::as.character(lubridate::quarter(cases_filed$date, with_year = T) - 0.1)
cases_filed$after <- base::as.character(lubridate::quarter(cases_filed$date, with_year = T))

for (i in 1:base::nrow(cases_filed)) {
  if (base::substring(cases_filed[i, ]$before, 6, 6) == "1") {
    
    cases_filed[i, ]$before <- base::paste0(base::substring(cases_filed[i, ]$before, 1, 4), "-", "01-01")
    cases_filed[i, ]$after <- base::paste0(base::substring(cases_filed[i, ]$before, 1, 4), "-", "03-31")
    
  } else if (base::substring(cases_filed[i, ]$before, 6, 6) == "2") {
    
    cases_filed[i, ]$before <- base::paste0(base::substring(cases_filed[i, ]$before, 1, 4), "-", "04-01")
    cases_filed[i, ]$after <- base::paste0(base::substring(cases_filed[i, ]$before, 1, 4), "-", "06-30")
    
  } else if (base::substring(cases_filed[i, ]$before, 6, 6) == "3") {
    
    cases_filed[i, ]$before <- base::paste0(base::substring(cases_filed[i, ]$before, 1, 4), "-", "07-01")
    cases_filed[i, ]$after <- base::paste0(base::substring(cases_filed[i, ]$before, 1, 4), "-", "09-30")
    
  } else if (base::substring(cases_filed[i, ]$before, 6, 6) == "4") {
    
    cases_filed[i, ]$before <- base::paste0(base::substring(cases_filed[i, ]$before, 1, 4), "-", "10-01")
    cases_filed[i, ]$after <- base::paste0(base::substring(cases_filed[i, ]$before, 1, 4), "-", "12-31")
    
  }
}

cases_filed$before <- base::as.Date(cases_filed$before, "%Y-%m-%d")
cases_filed$after <- base::as.Date(cases_filed$after, "%Y-%m-%d")
cases_filed$date <- NULL

processed$lawsuit <- base::as.numeric(NA)
for (i in 1:base::nrow(processed)) {
  
  tmp <- cases_filed[base::which(cases_filed$ticker == processed[i, ]$ticker & cases_filed$before <= processed[i, ]$date & cases_filed$after >= processed[i, ]$date), ]
  
  if (base::nrow(tmp) > 0) {
    processed[i, ]$lawsuit <- 1
  } else {
    processed[i, ]$lawsuit <- 0
  }
}
