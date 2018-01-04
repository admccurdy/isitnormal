ranker <- function(value, set){
  max(which(set == value, arr.ind = T))
}

percenter <- function(value, set){
  round((sum(set <= value, na.rm = T) / 
           length(set)) * 100)
}

tempToF <- function(stationData){
  stationData[, value := as.numeric(value)]
  stationData[element %in% c("TMAX", "TAVG", "TMIN"), value := (value/10 * 9/5) + 32]
}
