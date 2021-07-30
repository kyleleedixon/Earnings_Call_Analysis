library(shiny)
base::load("Photon_Demo/www/demo.RData")

shinyServer(function(input, output) {
  
  shiny::includeHTML("www/index.html")
  
  output$text <- renderText({})
  
  output$plot <- renderPlot({
    radarchart(rbind(rep(70,6) , rep(0,6), processed[which(processed$ticker == input$ticker), 1:6]), axistype = 1, 
               
               #custom polygon
               pcol= "#5E42A6" , pfcol= "#7e68b8", plwd = 4 , 
               
               #custom the grid
               cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,20,5), cglwd = 0.8,
               
               #custom labels
               vlcex = 0.8, title = "Q2 Earnings Call Word Type Frequency"
    )
  })
  
})