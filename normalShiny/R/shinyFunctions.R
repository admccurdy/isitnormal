ranker <- function(value, set){
  max(which(set == value, arr.ind = T))
}

percenter <- function(value, set){
  round((sum(set <= value) / 
           length(set)) * 100)
}