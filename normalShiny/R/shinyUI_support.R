output$dateSel <- renderUI({
  weatherMax <- max(weatherData()$date)
  dateInput("stationDate", "Select the date of interest:", 
            value = max(weatherMax),
            min = min(weatherData()$date),
            max = max(weatherMax))
})