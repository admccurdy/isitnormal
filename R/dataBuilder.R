library(data.table)
library(ggplot2)
library(dplyr)
library(rnoaa)


ptm <- proc.time()
stationList <- ghcnd_stations()
ptm - proc.time()

stationList <- stationList %>% data.table()

aspenStation <- stationList[longitude <= -106.41758 &
                              longitude >= -107.482302 &
                              latitude >= 38.879276 &
                              latitude <= 39.579248,]
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




