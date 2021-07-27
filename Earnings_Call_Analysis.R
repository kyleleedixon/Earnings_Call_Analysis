#################################
#                               #
# Author: Kyle Dixon            #
# Created: March 16 2021        #
# Title: Earnings Call Analysis #
#                               #
#################################

## Load libraries
library(data.table) # Read CSV files
library(xml2)
library(httr)

## Clear environment 
base::rm(list = base::ls())

## Extract S&P 500 tickers (last updated 3/16/2021)
sp_500 <- base::data.frame(data.table::fread("SP_500_Companies.csv"))
tickers <- base::substr(sp_500$Ticker, 1, base::nchar(sp_500$Ticker) - 10) # Remove Bloombergs formatting

## Finnhub API keys
api_key <- "c19c6tv48v6psigtarh0"
sandbox_api_key <- "sandbox_c19c6tv48v6psigtarhg"
webhook_secret <- "c19c6tv48v6psigtari0"

## FMP API keys
api_key <- "225b71ff44abca97bcdde9efd2e822eb"

headers <- c(`Upgrade-Insecure-Requests` = '1')

params <- base::list(`datatype` = 'json')

res <- httr::GET(url = 'https://financialmodelingprep.com/api/v3/earning_call_transcript/AAPL?quarter=3&year=2020&apikey=demo', 
                 httr::add_headers(.headers = headers), query = params)

output <- content(res)[[1]]


