library(data.table)
library(ggplot2)
library(dplyr)
library(ks)

source("R/elementRankMod.R")

dailys <- readRDS("data/dailys/currentDaily.RDS")
stations <- unique(dailys$name)