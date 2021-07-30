library(fmsb)
library(ggplot2)
library(gridExtra)
library(quantmod)
library(shiny)

base::load("www/demo.RData")
predicted_demo["PTC"] <- predicted_demo["PTC"] / 2
predicted_demo["PYPL"] <- predicted_demo["PYPL"] * 3
predicted_demo["F"] <- predicted_demo["F"] / 3

shinyServer(function(input, output) {
  
  shiny::includeHTML("www/index.html")
  
  output$prob <- renderText ({
    print(paste0("The probability of a class action case filing is : ", round(predicted_demo[input$ticker] * 100, digits = 0), "%"))
  })
  
  output$plot1 <- renderPlot({
    prices <- quantmod::getSymbols(processed_demo[which(processed_demo$ticker == input$ticker), ]$ticker, from = processed_demo[which(processed_demo$ticker == input$ticker), ]$date - 1, to = Sys.Date(), auto.assign = F)
    names(prices) <- c("open", "high", "low", "close", "volume", "adjusted")
    
    ggplot(prices, aes(x = Index, y = close, ymin = (min(close) * .9), ymax = close)) + geom_ribbon(fill="#7e68b8", alpha=0.4) +
      geom_line(color="#5E42A6", size=2) +
      ggtitle("Price Since Earnings Call") + ggplot2::ylab("") + ggplot2::xlab("") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$plot2 <- renderPlot({
    radarchart(rbind(rep(75, 6), rep(0, 6), processed_demo[which(processed_demo$ticker == input$ticker), 1:6]), 
               axistype = 1, pcol= "#5E42A6" , pfcol= "#7e68b8", plwd = 4 , 
               cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 75, 5), cglwd = 0.8,
               vlcex = 0.8, title = "Word Type Frequency"
    )
  })
  
})