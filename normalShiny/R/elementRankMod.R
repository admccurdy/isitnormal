rankUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("info")),
    plotOutput(ns("monthGraph"))
  )
}



elementRank <- function(input, output, session, weatherData, myElement, myDate){
  filteredWeather <- reactive({
    weatherData()[element == myElement,]
  })
  
  monthDistribution <- reactive({
    kde(filteredWeather()[month == month(myDate()), value])
  })
  dayDistribution <- reactive({
    kde(filteredWeather()[month == month(myDate()) &
                            day == mday(myDate()), value])
  })
  
  dayRank <- reactive({
    myData <- filteredWeather()[month == month(myDate()) &
                                  day == mday(myDate())
                                ,][order(value, decreasing = T), value]
    myValue <- filteredWeather()[date == myDate(), value]
    myReturn <- vector("list", 3)
    names(myReturn) <- c("rank", "percentile", "years")
    myReturn$rank <- ranker(myValue, myData)
    myReturn$percentile <- percenter(myValue, myData)
    myReturn$years <- length(myData)
    return(myReturn)
  })
  
  output$monthGraph <- renderPlot({
    myData <- filteredWeather()[month == month(myDate()),]
    percentiles <- sapply(split(myData, myData$day), FUN = function(x){
      myValue <- x[year == year(myDate()), value]
      percenter(myValue, x$value)
    })
    percentiles <- data.table("quantile" = percentiles, "day" = seq_along(percentiles))
    ggplot(percentiles, aes(quantile)) + geom_density() + ylim(0, 100)
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