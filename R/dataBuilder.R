library(data.table)
library(ggplot2)
library(dplyr)
library(rnoaa)
library(ks)


ptm <- proc.time()
stationList <- ghcnd_stations()
ptm - proc.time()

stationList <- stationList %>% data.table()

aspenStation <- stationList[longitude <= -106.41758 &
                              longitude >= -107.482302 &
                              latitude >= 38.879276 &
                              latitude <= 39.579248,]
snowtelStations <- aspenStation[substr(id, nchar(id), nchar(id)) %>% toupper() == "S",]
snowtelStations <- snowtelStations[element == "WESD",]






snowtelStations <- lapply(snowtelStations, precipCumSum)
snowtelStations <- 
snowtelStations2 <- lapply(snowtelStations, function(x)x[, value := value/median])



snotelDaily <- loadDLY(snowtelStations$id)
snotelDaily <- lapply(snotelDaily, returnMeltedValues)
snotelDaily <- lapply(snotelDaily, stationMedian)
snotelDaily <- lapply(snotelDaily, function(x)x %>% left_join(snowtelStations[, c("id", "name")]))
snotelDaily <- lapply(snotelDaily, data.table)
stationPlotter(snotelDaily[[1]], scale = F, startDate = 20161201, endDate = 20170501, plotMedian = F, elements = c("WESD"), ylims = c(.6, 1.7))
lapply(snotelDaily, stationPlotter, scale = F, startDate = 20171101, endDate = 20180501, plotMedian = T, elements = c("WESD"))

## Lookat at where we compare
snotelData <- copy(snotelDaily[[1]])
date <- 20180110
snowLook <- function(snotelData, myDate, elements){
  snotelData <- snotelData[element == elements,]
  myYear <- substr(date, 1, 4) %>% as.numeric()
  myMonth <- substr(date, 5, 6) %>% as.numeric()
  myDay <- substr(date, 7, 8) %>% as.numeric()
  peakDay <- snotelData[median == max(median, na.rm = T), c("month", "day", "median")][1]
  current <- snotelData[day == myDay & month == myMonth & year == myYear, c("value", "por")]
  setkey(current, day, month)
  snotelData[, por := value / median]
  dayData <- snotelData[day == myDay & month == myMonth,]
  snotelData[, date := paste0(year, ifelse(nchar(month) == 1, paste0("0", month), month),
                               ifelse(nchar(day) == 1, paste0("0", day), day))]
  barChart <- dayData %>% ggplot(aes(x = year, y = value)) + geom_bar(stat = "identity")
  yearsBelow <- dayData[por < current$por, year]
  yearsBelowAx <- dayData[por <= current$por + .05, year]
  yearsCaught <- snotelData[year %in% yearsBelowAx & year != myYear, ] %>% group_by(year) %>% 
    summarise('max' = max(value, na.rm = T), 'median' = peakDay$median)
  yearEnough <- snotelData %>% group_by(year) %>% summarise(maxSnow = max(value, na.rm = T)) %>%
    left_join(snotelData[day == myDay & month == myMonth, c("year", "value")]) %>% 
    mutate(snowLeft = maxSnow - value)
  snowNeeded <- peakDay$median - current$value
  yearEnough <- yearEnough %>% filter(!is.na(snowLeft), snowLeft > 300)
  yearEnough %>% filter(snowLeft >= snowNeeded * .95)
  
  snowLeftKDE <- kde(yearEnough$snowLeft)
  randomSL <- rkde(10000, snowLeftKDE)
  sum(randomSL >= snowNeeded)/length(randomSL)
}





snotelDaily <- rbindlist(snotelDaily)


snotelDaily <- snotelDaily %>% data.table()
snotelDaily <- yearToWY(snotelDaily)
snotelDaily <- snotelDaily[element %in% c("TMAX", "TMIN", "PRCP", "WESD", "SNOW"),]




stationsInterest <- aspenStation[id %in% c("USC00050370", "USC00050372", "USS0006K04S", "USC00053359"),]
stationsInterest <- stationsInterest[element == "PRCP",]
# saveRDS(myDailys, "normalShiny/data/dailys/dailys12_21.RDS")

myDailys <- loadDLY(stationsInterest$id)
myDailys <- lapply(myDailys, returnMeltedValues)
myDailys <- rbindlist(myDailys)

myDailys <- myDailys %>% left_join(stationsInterest[, c("id", "name")])
myDailys <- myDailys %>% data.table()
myDailys[, name := ifelse(name == "ASPEN 1SW", "ASPEN", name)]
myDailys <- myDailys[element %in% c("TMAX", "TMIN", "PRCP", "WESD", "SNOW"),]
myDailys[, date := as.Date(paste0(year, "-", month, "-", day))]
saveRDS(myDailys, "normalShiny/data/dailys/currentDaily.RDS")




