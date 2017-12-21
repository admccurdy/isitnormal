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
                               ,][order(value), value]
    myValue <- filteredWeather()[date == myDate(), value]
    myReturn <- vector("list", 2)
    names(myReturn) <- c("rank", "percentile")
    myReturn$rank <- max(which(myData == myValue, arr.ind = T))
    myReturn$percentile <- sum(myData <= myValue) / 
                            length(myData)
    return(myReturn)
  })
  
  output$info <- renderUI({
    tagList(
      h5("Your date was greater than"),
      h5(dayRank())
    )
  })
  
}