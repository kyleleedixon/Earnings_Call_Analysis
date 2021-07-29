#########################
#                       #
# Author: Kyle Dixon    #
# Created: July 28 2021 #
# Title: Web App Demo   #
#                       #
#########################

## Load libraries
library(dplyr)
library(fmsb)
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
library(shiny)

textdata::lexicon_loughran(manual_download = T)

## Load in model
base::load("www/model.RData")

## Initialize processed data list, start count, and available tickers
processed <- base::list()

## Loop through each file and process the raw data
for (i in c(1:5)) {
  
  i <- base::list.files("www/demo_calls")[i]
  
  ## Read in .txt file as a vector
  #raw_text <- dplyr::pull(readr::read_delim(base::paste0("MAEC_Dataset/", i, "/text.txt"), delim = "\r\n", show_col_types = F), 1)
  raw_text <- dplyr::pull(readr::read_delim(base::paste0("www/demo_calls/", i), delim = "\n\n", show_col_types = F), 1)
  
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
    
    ## Get DoD return for specific date and ticker
    prices <- quantmod::getSymbols(base::substr(i, 10, base::nchar(i) - 4), from = base::as.Date(base::substr(i, 1, 8), "%Y%m%d"), 
                                   to = base::as.Date(base::substr(i, 1, 8), "%Y%m%d") + 1, auto.assign = F)
    sentiments$return <- base::as.numeric((prices[, 4] - prices[, 1]) / prices[, 4])
    
    ## Append to processed data set
    processed <- base::append(processed, base::list(sentiments))
  }
}

## Bind processed data into a single data frame
processed <- dplyr::bind_rows(processed)

predicted <- logit %>% predict(processed[, c(1:7, 10)], type = "response")

shiny::htmlTemplate("www/index.html", select = selectInput("ticker", "Select Ticker:", choices = processed$ticker), plot = plotOutput("plot"))