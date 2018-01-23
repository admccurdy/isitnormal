library(leaps) # to use function "regsubsets"
library(car)# to use function "subsets"
library(lmSupport)# to use function "modelEffectSizes"
library(cvTools)
library(leaps)		# to provide combinations
library(MPV)		# to help estimate PRESS and consequently, GCV
library(locfit)
library(sm)
library(akima)
library(fields)
library(MASS)
library(knitr)
library(mgcv)
library(splines)
library("bestglm")
library(data.table)
library(dplyr)
library(dtplyr)
library(reshape)



noaaUpdate <- function(existingData){
  stationStatus <- existingData %>% group_by(id) %>% 
    summarise(max(date, na.rm = T)) %>% data.table()
  stationStatus[, id := paste0("GHCND:", id)]
  
  availableData <- lapply(stationStatus$id, function(x){
    ncdc_stations(stationid = x, token = "zYkGbjKEoZXmvSVEmCgDAaOZpsIjViSy")
  })

}

dailys <- readRDS("normalShiny/data/dailys/currentDaily.RDS")

# Create a function to convert airport data to GCDN data

dailysList <- dailys[element %in% c("TMAX", "TMIN"),]
dailysList <- split(dailysList, dailysList$name)
modelList <- dailysList[[1]]
modelList <- modelList[date == 10000000,]
modelList[, c("id", "name", "value") := NULL]

for(i in dailysList){
  setnames(i, "value", i[1, name])
  i[, c("id", "name") := NULL]
  modelList <- merge(i, modelList, by = c("date", "element", "day", "month", "year"), all.x = T)
}
setnames(modelList, names(modelList)[6:9], c("indy", "gwood", "airport", "aspen"))
modelList <- modelList[complete.cases(modelList),]

modelMax <- modelList[element == "TMAX",]
modelMin <- modelList[element == "TMIN",]


modeler <- function(myData){
  myData <- myData[, c("indy", "gwood", "airport", "aspen")]
  myRows <- 1:nrow(myData)
  trainRows <- sample(size = .1 * nrow(myData), x = myRows)
  myTest <- myData[myRows[!myRows %in% trainRows],]
  myTrain <- myData[myRows[myRows %in% trainRows],]
  
}
myData <- myTest
pOrderMax <- 2
myFamily = "gaussian" 
myLink = "ident"
readLine = T
predictVar <- "airport"

temp <- localPolyModeler(myData, 4, predictVar = "airport")


localPolyModeler <- function(myData, pOrderMax, myFamily = "gaussian", myLink = "ident", 
                             readLine = T, predictVar) {
  
  myData <- na.omit(myData)
  myData <- data.frame(myData)
  
  ###Determine Alphas###
  ######################
  N = nrow(myData)
  
  #Extract covariate names for later use
  covarName <- names(myData)[!names(myData) %in% predictVar]
  
  #Create Covariate frame
  covariates <- myData[,!names(myData) %in% predictVar]
  
  ###Use Leaps to create list of all possible combinations###
  #************************************************************#
  #If there is only one predictor (as is the case with lf in GAMS)
  #leaps fails control statement to avoid failure
  if(length(myData) == 2){
    combos <- matrix(T, 1,1)
    covariates <- data.frame(matrix(covariates, nrow = length(covariates), ncol = 1))
    colnames(covariates) <- covarName
  }else{
    combos <- leaps(covariates, myData[, predictVar], nbest = 25)
    combos <- combos$which
  }
  ncombos <- nrow(combos)
  
  
  finalGCV <- data.frame("alpha" = numeric(), "GCV" = numeric(), "pOrder" = numeric(), "combo" = numeric(), stringsAsFactors = F)
  
  ###Exhaustive search for best GCF using all combonations of deg, alpha, and covariates###
  #***************************************************************************************#
  #Outer loop cycles through combos, inner loop through degs, and GCV plot test alphas
  #All results along with relevant information are stored in dataframe above
  
  for(i in 1:ncombos){
    Xs <- covariates[, combos[i,]]
    
    if(length(Xs) == N){
      nvar <- 1
    }else{
      nvar <- length(Xs[1,])
    }
    for(j in 1:pOrderMax) {
      gcvFrame <- NULL
      Xs <- data.matrix(Xs)
      
      if(myFamily == "binomial"){
        minalpha = .51
      }else{
        minalpha=2*(nvar*pOrderMax+1)/N
      }
      
      myAlphas=seq(minalpha,1.0,length.out = 20)
      n=length(myAlphas)
      gcvValues <- gcvplot(myData[, predictVar] ~ Xs, alpha = myAlphas, deg = j,
                           kern = "bisq", ev = dat(), scale = T, 
                           family = myFamily, link = myLink)
      gcvFrame <- data.frame(cbind(gcvValues$alpha, gcvValues$values))
      colnames(gcvFrame) <- c("alpha", "GCV") 
      gcvFrame$pOrder <- j 
      gcvFrame$combo <- i
      finalGCV <- rbind(finalGCV, gcvFrame)
    }
  }
  
  #Sort final list of GCV Values
  finalGCV <- finalGCV[order(finalGCV$GCV),]
  rownames(finalGCV) <- NULL
  
  #Allows for selection of model to use
  #If select option is false takes best GCV
  print(head(finalGCV))
  if(readLine){
    rowNumber <- readline("What row number would you like to use? ")
  }else{
    rowNumber <- 1
  }
  
  #Extract Data from GCV List
  bestAlpha <- finalGCV[rowNumber, "alpha"]
  bestPorder <- finalGCV[rowNumber, "pOrder"]
  bestCombo <- finalGCV[rowNumber, "combo"]
  
  #Create frame of covariates
  if(length(covariates) == 1){
    Xs <- covariates
  }else{
    Xs <- data.frame(covariates[, combos[bestCombo,]])
    myData <- myData[,c(predictVar, names(Xs))]  
  }
  
  ###Create Loc Model###
  #********************#
  n <- names(Xs)
  locModel <- locfit(myData$precip ~ data.matrix(myData[names(myData) %in% n]), alpha = bestAlpha, deg = bestPorder, 
                     kern = "bisq", scale = T)
  
  ###Model creation tools store information as names and not the actual value
  #This is problmeatic when working with the model later this code chunk replaces all names with
  #values. Also it adds residuals and data to the model object which isn't good practice...
  locModel$call$family <- myFamily
  locModel$call$link <- myLink
  locModel$call$deg <- bestPorder
  rownames(myData) <- NULL
  locModel$model <- myData
  locModel$residuals <- resid(locModel, type = "response")
  locModel$call$alpha <- bestAlpha
  return(locModel)
}



maxTrain <- sample(size = .1 * nrow(modelMax), x = maxRows)
maxTest <- maxRows[!maxRows %in% maxTrain]


tmaxModel <- lm(data = modelMax[maxTest,], value.x ~ value.y)
tmaxModel <- glm(data = modelMax[maxTest,], value.x ~ value.y, family = Gamma(link = "inverse"))

