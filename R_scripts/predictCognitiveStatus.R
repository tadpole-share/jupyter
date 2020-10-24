#' Title
#'
#' @param Models the Prediciton models
#' @param TestDataFrame the data set to predict
#'
#' @return the list of prediciotns per subject
#' @export
#'
#' @examples
forecastCognitiveStatus <- function(Models,TestDataFrame){
  
  #args
  Models=CognitiveClassModels
  TestDataFrame = testingFrame
  ###
  predictors <- Models$predictors
  months <- as.numeric(names(table(TestDataFrame$M)))
  print(months)
  
  
  TestDataFrame$class <- numeric(nrow(TestDataFrame))
  TestDataFrame$TimeToEvent <- numeric(nrow(TestDataFrame))
  
  TestDataFrame$RID <- as.character(TestDataFrame$RID)
  library("FRESA.CAD")
  if (is.null(months)){
    months <- as.numeric(names(table(TestDataFrame$M)))
    print(months)
  }
  cpredictors <- predictors
  
  TestDataFrame <- TestDataFrame[order(TestDataFrame$EXAMDATE),]
  TestDataFrame <- TestDataFrame[order(as.numeric(TestDataFrame$RID)),]
  
  pdis <- TestDataFrame$RID
  lastTimepointSet <- TestDataFrame[c(pdis[1:(length(pdis)-1)] != pdis[-1],TRUE),]
  rownames(lastTimepointSet) <- lastTimepointSet$RID
  print(nrow(TestDataFrame))
  print(nrow(lastTimepointSet))
  
  BaseTimepointSet <- TestDataFrame[c(TRUE,pdis[-1] != pdis[1:(length(pdis)-1)]),]
  rownames(BaseTimepointSet) <- BaseTimepointSet$RID
  deltaFeaturepredictors <- predictors[regexpr('_bl', predictors) < 0][-(c(1:2))]
  print(nrow(BaseTimepointSet))
  
  TimePointsSubset <- list();
  Orderbytimepoint <- NULL
  m <- 0
  i <- 1;
  lastDate <- BaseTimepointSet$EXAMDATE
  names(lastDate) <- BaseTimepointSet$RID
  lastDX <- BaseTimepointSet$DX
  names(lastDX) <- BaseTimepointSet$RID
  print(sum(is.na(TestDataFrame$M)))
#  totR <- 0
  if (length(months) > 1){
    for (m in months){
      TimePointsSubset[[i]] <- subset(TestDataFrame,M == m)
      rownames(TimePointsSubset[[i]]) <- TimePointsSubset[[i]]$RID
      TimePointsSubset[[i]]$Year_bl_LastVisit <- lastTimepointSet[TimePointsSubset[[i]]$RID,"Years_bl"]
      TimePointsSubset[[i]]$Last_DX <-  lastTimepointSet[TimePointsSubset[[i]]$RID,"DX"]
      ldx <- TimePointsSubset[[i]]$DX
      names(ldx) <- TimePointsSubset[[i]]$RID
      ldx <- ldx[!is.na(TimePointsSubset[[i]]$DX)]
      lastDX[names(ldx)] <- ldx
      lastDate[names(ldx)] <- TimePointsSubset[[i]][names(ldx),"EXAMDATE"]
      TimePointsSubset[[i]]$TimeToLastVisit <- TimePointsSubset[[i]]$Year_bl_LastVisit - TimePointsSubset[[i]]$Years_bl
      deltaObservations <- TimePointsSubset[[i]][,deltaFeaturepredictors] - BaseTimepointSet[rownames(TimePointsSubset[[i]]),deltaFeaturepredictors]
      colnames(deltaObservations) <- paste("Delta",colnames(deltaObservations),sep="_")
      TimePointsSubset[[i]] <- cbind(TimePointsSubset[[i]],deltaObservations)
      Orderbytimepoint <- rbind(Orderbytimepoint,TimePointsSubset[[i]])
      i <- i + 1
    }
    TestDataFrame <- Orderbytimepoint
    Orderbytimepoint <- NULL
    print(nrow(TestDataFrame))
  }else{
    TestDataFrame <- lastTimepointSet
    TestDataFrame$M <- numeric(nrow(TestDataFrame))
    TestDataFrame$Year_bl_LastVisit <- numeric(nrow(TestDataFrame))
    TestDataFrame$TimeToLastVisit <- numeric(nrow(TestDataFrame))
    deltaObservations <- TestDataFrame[,deltaFeaturepredictors] - BaseTimepointSet[rownames(TestDataFrame),deltaFeaturepredictors]
    colnames(deltaObservations) <- paste("Delta",colnames(deltaObservations),sep="_")
    TestDataFrame <- cbind(TestDataFrame,deltaObservations)
    rownames(TestDataFrame) <- c(1:nrow(TestDataFrame))
    print(nrow(TestDataFrame))
    print(sum(is.na(lastTimepointSet)))
  }
  
  
  
  if (is.null(Models$CrossModels[[1]]$oridinalModels)){
    crossprediction <- predict(Models$CrossModels[[1]],lastTimepointSet)
  }else{
    crossprediction <- predict(Models$CrossModels[[1]]$oridinalModels,lastTimepointSet)
  }
  for (n in 2:length(Models$CrossModels)){
    if (is.null(Models$CrossModels[[n]]$oridinalModels)){
      crossprediction <- crossprediction + predict(Models$CrossModels[[n]],lastTimepointSet)
    } else {
      crossprediction <- crossprediction + predict(Models$CrossModels[[n]]$oridinalModels,lastTimepointSet)
    }
  }
  
  if (is.null(Models$CrossModels[[1]]$oridinalModels)){
    crossprediction <- as.integer(crossprediction/length(Models$CrossModels)+0.5)
  } else  {
    crossprediction <- as.data.frame(crossprediction[,(ncol(crossprediction)-2):ncol(crossprediction)]/length(Models$CrossModels))
    rownames(crossprediction) <- lastTimepointSet$RID
  }
  crossprediction <- crossprediction[order(as.numeric(rownames(crossprediction))),]
  crossprediction$pDX <- apply(crossprediction,1,which.max)
  crossprediction$DX <- lastTimepointSet[rownames(crossprediction),"DX"]
  crossprediction$date <- lastTimepointSet[rownames(crossprediction),"EXAMDATE"]

  TestDataFrame <- TestDataFrame[order(TestDataFrame$EXAMDATE),]
  TestDataFrame <- TestDataFrame[order(as.numeric(TestDataFrame$RID)),]
  pdis <- TestDataFrame$RID
  lastTimepointSet <- TestDataFrame[c(pdis[1:(length(pdis)-1)] != pdis[-1],TRUE),]
  rownames(lastTimepointSet) <- lastTimepointSet$RID
  print(nrow(lastTimepointSet))
  print(sum(is.na(lastTimepointSet)))
  
  

  
  MCITOADprediction <- predict(Models$MCIToADModels[[1]],lastTimepointSet)
  MCITOADTimeprediction <- predict(Models$MCIToADTimeModel[[1]],lastTimepointSet)
  sm <- summary(Models$MCIToADModels[[1]])
  MCIADAUC <- sm$tAUC
  
  MCITONCprediction <- predict(Models$MCIToNCModels[[1]],lastTimepointSet)
  MCITONCTimeprediction <- predict(Models$MCIToNCTimeModel[[1]],lastTimepointSet)
  sm <- summary(Models$MCIToNCModels[[1]])
  MCINCAUC <- sm$tAUC
  
  NCToMCIprediction <- predict(Models$NCToMCIModel[[1]],lastTimepointSet)
  NCToMCITimeprediction <- predict(Models$NCToMCITimeModel[[1]],lastTimepointSet)
  sm <- summary(Models$NCToMCIModel[[1]])
  NCMCIAUC <- sm$tAUC
  
  
  for (n in 2:length(Models$MCIToADModels)){
    MCITOADprediction <- MCITOADprediction + predict(Models$MCIToADModels[[n]],lastTimepointSet)
    MCITOADTimeprediction <- MCITOADTimeprediction + predict(Models$MCIToADTimeModel[[n]],lastTimepointSet)
    sm <- summary(Models$MCIToADModels[[n]])
    MCIADAUC <- MCIADAUC + sm$tAUC
    
    MCITONCprediction <- MCITONCprediction + predict(Models$MCIToNCModels[[n]],lastTimepointSet)
    MCITONCTimeprediction <- MCITONCTimeprediction + predict(Models$MCIToNCTimeModel[[n]],lastTimepointSet)
    sm <- summary(Models$MCIToNCModels[[n]])
    MCINCAUC <- MCINCAUC + sm$tAUC
    
    NCToMCIprediction <- NCToMCIprediction + predict(Models$NCToMCIModel[[n]],lastTimepointSet)
    NCToMCITimeprediction <- NCToMCITimeprediction + predict(Models$NCToMCITimeModel[[n]],lastTimepointSet)
    sm <- summary(Models$NCToMCIModel[[n]])
    NCMCIAUC <- NCMCIAUC + sm$tAUC
    
  }
  #Models$pMCItoADEvent
  #Models$pMCItoNCEvent
  #Models$pNCtoMCIEvent

  MCIADAUC <- MCIADAUC/length(Models$CrossModels)
  MCITOADprediction <- MCITOADprediction/length(Models$CrossModels)
  MCITOADTimeprediction <- MCITOADTimeprediction/length(Models$CrossModels)

  MCINCAUC <- MCINCAUC/length(Models$CrossModels)
  MCITONCprediction <- MCITONCprediction/length(Models$CrossModels)
  MCITONCTimeprediction <- MCITONCTimeprediction/length(Models$CrossModels)
  
  NCMCIAUC <- NCMCIAUC/length(Models$CrossModels)
  NCToMCIprediction <- NCToMCIprediction/length(Models$CrossModels)
  NCToMCITimeprediction <- NCToMCITimeprediction/length(Models$CrossModels)
  

  predictions <- list(orderedTestFrame = TestDataFrame,
                      predictedTimePointData = lastTimepointSet,
                      fullPredictors = predictors, 
                      crossprediction = crossprediction,
                      MCITOADprediction = MCITOADprediction,
                      MCITOADTimeprediction = MCITOADTimeprediction,
                      MCITONCprediction = MCITONCprediction,
                      MCITONCTimeprediction = MCITONCTimeprediction,
                      NCToMCIprediction = NCToMCIprediction,
                      NCToMCITimeprediction = NCToMCITimeprediction,
                      lastKownDX = lastDX,
                      lastDateDX = lastDate,
                      MCIADAUC = MCIADAUC,
                      MCINCAUC = MCINCAUC,
                      NCMCIAUC = NCMCIAUC,
                      pMCItoADEvent = Models$pMCItoADEvent,
                      pMCItoNCEvent = Models$pMCItoNCEvent,
                      pNCtoMCIEvent = Models$pNCtoMCIEvent
  )
  
  
  return (predictions)
}