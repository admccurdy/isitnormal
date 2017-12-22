library(data.table)
library(ggplot2)
library(dplyr)
library(ks)

source("R/elementRankMod.R")
source("R/shinyFunctions.R")

dailys <- readRDS("data/dailys/currentDaily.RDS")
stations <- unique(dailys$name)