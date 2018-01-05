noaaUpdate <- function(existingData){
  stationStatus <- existingData %>% group_by(id) %>% 
    summarise(max(date, na.rm = T)) %>% data.table()
  stationStatus[, id := paste0("GHCND:", id)]
  
  availableData <- lapply(stationStatus$id, function(x){
    ncdc_stations(stationid = x, token = "zYkGbjKEoZXmvSVEmCgDAaOZpsIjViSy")
  })

}

# Create a function to convert airport data to GCDN data

modelData <- dailys[id %in% c("USW00093073", "USC00050372") & 
                      element %in% c("TMAX", "TMIN"),]
airportData <- modelData[id == "USW00093073",]
ghcndData <- modelData[id == "USC00050372",]
modelData <- merge(airportData, ghcndData, by = c("date", "element", "year", "month", "day"), all.x = T)
modelData <- modelData[!is.na(value.x) & !is.na(value.y), ]
modelMax <- modelData[element == "TMAX",]
modelMin <- modelData[element == "TMIN",]

maxRows <- 1:nrow(modelMax)
maxTrain <- sample(size = .1 * nrow(modelMax), x = maxRows)
maxTest <- maxRows[!maxRows %in% maxTrain]


tmaxModel <- lm(data = modelMax[maxTest,], value.x ~ value.y)
tmaxModel <- glm(data = modelMax[maxTest,], value.x ~ value.y, family = Gamma(link = "inverse"))

