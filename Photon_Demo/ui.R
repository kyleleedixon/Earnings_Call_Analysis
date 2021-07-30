library(shiny)
base::load("Photon_Demo/www/demo.RData")
shiny::htmlTemplate("www/index.html", select = selectInput("ticker", "Select Ticker:", choices = processed_demo$ticker), plot = plotOutput("plot"))