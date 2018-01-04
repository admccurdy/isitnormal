rankUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("info")),
    plotOutput(ns("monthGraph")),
    plotOutput(ns("heatMonth"))
  )
}



elementRank <- function(input, output, session, weatherData, myElement, interestDate){
  myDate <- reactive({
    myReturn <- tryCatch({
      as.POSIXlt(interestDate())
      interestDate()
    }, error = function(e){
      max(filteredWeather()$date)
    })
    return(myReturn)
  })
  
  filteredWeather <- reactive({
    weatherData()[element == myElement,]
  })
  
  monthWeather <- reactive({
   filteredWeather()[month == month(myDate()),]
  })
  
  dayWeather <- reactive({
    monthWeather()[day == day(myDate()),]
  })
  
  yearWeather <- reactive({
    filteredWeather()[year == year(myDate())]
  })
  
  monthDistribution <- reactive({
    kde(monthWeather()[, value])
  })
  dayDistribution <- reactive({
    kde(monthWeather()[day == mday(myDate()), value])
  })
  
  dayRank <- reactive({
    myData <- monthWeather()[day == mday(myDate()),][order(value, decreasing = T), value]
    myValue <- filteredWeather()[date == myDate(), value]
    myReturn <- vector("list", 3)
    names(myReturn) <- c("rank", "percentile", "years")
    myReturn$rank <- ranker(myValue, myData)
    myReturn$percentile <- percenter(myValue, myData)
    myReturn$years <- length(myData)
    return(myReturn)
  })
  
  yearPercentile <- reactive({
    myData <- filteredWeather()
    myData[, monthDay := paste0(
      ifelse(nchar(month) == 1, paste0(0, month), month),
      ifelse(nchar(day) == 1, paste0(0, day), day))]
    myData <- myData[order(date),]
    myPercentiles <- sapply(split(myData, myData$monthDay), FUN = function(x){
      myValue <- x[year == year(myDate()), value]
      percenter(myValue, x$value)
    })
    myPercentiles <- data.table(id = myData[1, id], year = year(myDate()), 
                                element = myData[1, element], value = myPercentiles, 
                                day = seq_along(myPercentiles), 
                                date = myData[year == year(myDate()),][order(day),]$date)
    return(myPercentiles)
  })
  
  percentiles <- reactive({
    myData <- monthWeather()
    myPercentiles <- sapply(split(myData, myData$day), FUN = function(x){
      myValue <- x[year == year(myDate()), value]
      percenter(myValue, x$value)
    })
    myPercentiles <- data.table(id = myData[1, id], year = year(myDate()), 
                              element = myData[1, element], value = myPercentiles, 
                              day = seq_along(myPercentiles), 
                              date = myData[year == year(myDate()),][order(day),]$date)
    print(myPercentiles)
    return(myPercentiles)
  })
  
  output$dayGraph <- renderPlot({
    myData <- tempToF(dayWeather())
    
  })
  
  output$monthGraph <- renderPlot({

    # percentiles <- data.table("quantile" = percentiles, "day" = seq_along(percentiles))
    ggplot(percentiles(), aes(value)) + geom_density()
  })
  
  output$heatMonth <- renderPlot({
    myData <- percentiles()
    print(myData)
    myData[, weekDay := factor(weekdays(date), 
                               levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                          "Thursday", "Friday", "Saturday"))]
    myData[, week := week(date)]
    myData[, week := week - min(week) + 1]
    
    ggplot(myData, aes(weekDay, week, fill = value)) + 
      geom_tile(color = "white") + 
      scale_fill_gradient2(low = "blue", mid = "white", 
                           high = "red", midpoint = 50, limits = c(0,100)) +
      scale_y_reverse()
    
  })
  
  output$info <- renderUI({
    if(myElement %in% c("TMAX", "TMIN")){
      tagList(
        h5(paste0("The ", myElement, " was the ", dayRank()$rank, 
                  " warmest of the past ", dayRank()$years, " years")),
        h5(paste0("In other words it was warmer than ", dayRank()$percentile,
                  " % of years"))
      )  
    }else{
      tagList(
        h5("Your date was greater than"),
        h5(dayRank())
      )
    }
    
  })
  
}