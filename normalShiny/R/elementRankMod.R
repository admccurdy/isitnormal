rankUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("info"))
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
    print(myDate())
    print(class(myDate()))
    myData <- filteredWeather()[month == month(myDate()) &
                               day == mday(myDate())
                               ,][order(value, decreasing = T), value]
    myValue <- filteredWeather()[date == myDate(), value]
    myReturn <- vector("list", 3)
    names(myReturn) <- c("rank", "percentile", "years")
    myReturn$rank <- max(which(myData == myValue, arr.ind = T))
    myReturn$percentile <- round((sum(myData <= myValue) / 
                            length(myData)) * 100)
    myReturn$years <- length(myData)
    return(myReturn)
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