library(fmsb)
library(shiny)

base::load("www/demo.RData")

shiny::htmlTemplate("www/index.html", 
                    select = selectInput("ticker", "Select Ticker:", choices = processed_demo$ticker), 
                    prob = textOutput("prob"), 
                    plot1 = plotOutput("plot1"), plot2 = plotOutput("plot2"))