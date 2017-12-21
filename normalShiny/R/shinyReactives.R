weatherData <- reactive({
  dailys[name == input$station & 
           !is.na(value),]
})

dateInterest <- reactive({
  input$stationDate
})