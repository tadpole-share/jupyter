
#' TrainTadpoleRegresionModels
#'
#' @param AdjustedFrame 
#' @param predictors 
#' @param numberOfRandomSamples 
#' @param delta 
#' @param MLMethod 
#' @param hysteresis = hysteresis 
#'
#' @return
#' @export
#'
#' @examples
TrainTadpoleRegresionModels <- function(AdjustedFrame,
                                        predictors,
                                        numberOfRandomSamples=5,
                                        MLMethod=BSWiMS.model,
                                         ...){

  AdjustedFrame$RID <- as.character(AdjustedFrame$RID)
  AdjustedFrame$EXAMDATE <- as.Date(AdjustedFrame$EXAMDATE);

  suppressMessages(library("FRESA.CAD"))
  months <- as.numeric(names(table(AdjustedFrame$M)))
  print(months)
  cpredictors <- predictors
  
  AdjustedFrame <- AdjustedFrame[order(AdjustedFrame$EXAMDATE),]
  AdjustedFrame <- AdjustedFrame[order(as.numeric(AdjustedFrame$RID)),]
  
  pdis <- AdjustedFrame$RID
  lastTimepointSet <- AdjustedFrame[c(pdis[1:(length(pdis)-1)] != pdis[-1],TRUE),]
  print(table(lastTimepointSet$VISCODE))
  rownames(lastTimepointSet) <- lastTimepointSet$RID
  print(nrow(lastTimepointSet))
  print(table(lastTimepointSet$DX))

  Orderbytimepoint <- vector()
  m <- 0
  print("FOR 1")
  for (m in months){
    TimePointsSubset <- subset(AdjustedFrame,M == m)
    TimePointsSubset$TimeToLastVisit <- as.numeric(lastTimepointSet[TimePointsSubset$RID,"EXAMDATE"] - TimePointsSubset$EXAMDATE)/365.25
    TimePointsSubset$DeltaVentricle <- as.numeric(lastTimepointSet[TimePointsSubset$RID,"Ventricles"] - TimePointsSubset$Ventricles)
    TimePointsSubset$DeltaAdas13 <- as.numeric(lastTimepointSet[TimePointsSubset$RID,"ADAS13"] - TimePointsSubset$ADAS13)
    TimePointsSubset <- TimePointsSubset[complete.cases(TimePointsSubset[,predictors]),]
    Orderbytimepoint <- rbind(Orderbytimepoint,TimePointsSubset)
  }
  hist(Orderbytimepoint$TimeToLastVisit)
  hist(Orderbytimepoint$DeltaVentricle)
  hist(Orderbytimepoint$DeltaAdas13)
  
  print(sum(Orderbytimepoint$TimeToLastVisit < 0))
  
#  AdjustedFrame <- subset(Orderbytimepoint,TimeToLastVisit >= 0)
  AdjustedFrame <- Orderbytimepoint
  AdjustedFrame <- AdjustedFrame[order(AdjustedFrame$EXAMDATE),]
  AdjustedFrame <- AdjustedFrame[order(as.numeric(AdjustedFrame$RID)),]
  
  AdjustedFrame$SQRTimeToLastVisit <- AdjustedFrame$TimeToLastVisit*AdjustedFrame$TimeToLastVisit
  AdjustedFrame$SQRTTimeToLastVisit <- sqrt(AdjustedFrame$TimeToLastVisit)
  AdjustedFrame$CUBTimeToLastVisit <- AdjustedFrame$TimeToLastVisit*AdjustedFrame$SQRTimeToLastVisit
  AdjustedFrame$LOGTimeToLastVisit <- log(1+AdjustedFrame$TimeToLastVisit)
  AdjustedFrame$MeanCVLOGTimeToLastVisit <- log(1+AdjustedFrame$TimeToLastVisit)*AdjustedFrame$COMeanThickness
  AdjustedFrame$MMSELOGTimeToLastVisit <- log(1+AdjustedFrame$TimeToLastVisit)*AdjustedFrame$MMSE
  
  print(c(nrow(AdjustedFrame),ncol(AdjustedFrame)))
  
  predictorsAdas13 <- c(predictors,c("TimeToLastVisit","SQRTimeToLastVisit","LOGTimeToLastVisit","CUBTimeToLastVisit","SQRTTimeToLastVisit","MeanCVLOGTimeToLastVisit","MMSELOGTimeToLastVisit","DeltaAdas13"))
  predictorsVentricle <- c(predictors,c("TimeToLastVisit","SQRTimeToLastVisit","LOGTimeToLastVisit","CUBTimeToLastVisit","SQRTTimeToLastVisit","MeanCVLOGTimeToLastVisit","MMSELOGTimeToLastVisit","DeltaVentricle"))
  Orderbytimepoint <- NULL

  ## MCI Subset
  

  MCISubset <- subset(AdjustedFrame,DX == "NL to MCI" | DX == "MCI" | DX == "Dementia to MCI")
  
  MCISubset <- MCISubset[complete.cases(MCISubset),]
  print(nrow(MCISubset))
  print(sum(is.na(MCISubset)))
  
  MCI_ADAS_MODEL <- list();
  MCI_Ventricle_ICV_MODEL <- list();

  for (n in 1:numberOfRandomSamples)
  {
    randomnumber <- sample(1:nrow(MCISubset),nrow(MCISubset))
    MCI_Set <- MCISubset[randomnumber,]
    MCI_Set <- MCI_Set[order(as.numeric(MCI_Set$RID)),]
    RID <- MCI_Set$RID
    set1 <- MCI_Set[c(TRUE,RID[-1] != RID[1:length(RID)-1]),]
    rownames(set1) <- set1$RID
    print(table(set1$DX))
    print(nrow(set1))
    hist(set1$LOGTimeToLastVisit)
    
    setA <- set1[,predictorsAdas13]
    randomnumber <- sample(1:nrow(setA),nrow(setA),replace = TRUE)
    
    MCI_ADAS_MODEL[[n]] <- MLMethod(DeltaAdas13 ~ .,setA[randomnumber,],...)

    sm <- summary(MCI_ADAS_MODEL[[n]])
#    print(rownames(sm$coefficients))
    print(sm$R2)
    plot(set1$DeltaAdas13~predict(MCI_ADAS_MODEL[[n]],setA))
    
    MCI_ADAS_MODEL[[n]]$BSWiMS.model$bootCV$data <- NULL
    MCI_ADAS_MODEL[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    MCI_ADAS_MODEL[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL

    setA <- set1[,predictorsVentricle]
    MCI_Ventricle_ICV_MODEL[[n]] <- MLMethod(DeltaVentricle ~ .,setA[randomnumber,],...)
    sm <- summary(MCI_Ventricle_ICV_MODEL[[n]])
#    print(rownames(sm$coefficients))
    print(sm$R2)
    plot(set1$DeltaVentricle~predict(MCI_Ventricle_ICV_MODEL[[n]],setA))
    
    MCI_Ventricle_ICV_MODEL[[n]]$BSWiMS.model$bootCV$data <- NULL
    MCI_Ventricle_ICV_MODEL[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    MCI_Ventricle_ICV_MODEL[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL

  }
 
  predicitionModels <- list(MCI_Ventricle_ICV_MODEL = MCI_Ventricle_ICV_MODEL,
                            MCI_ADAS_MODEL=MCI_ADAS_MODEL
  )

  ## NC Subset
  
  NCSubset <- subset(AdjustedFrame,DX == "NL" | DX == "MCI to NL")
  NCSubset <- NCSubset[complete.cases(NCSubset),]
  print(nrow(NCSubset))
  print(sum(is.na(NCSubset)))
  
  NC_ADAS_MODEL <- list();
  NC_Ventricle_ICV_MODEL <- list();
  
  for (n in 1:numberOfRandomSamples)
  {
    randomnumber <- sample(1:nrow(NCSubset),nrow(NCSubset))
    NC_Set <- NCSubset[randomnumber,]
    NC_Set <- NC_Set[order(as.numeric(NC_Set$RID)),]
    RID <- NC_Set$RID
    set1 <- NC_Set[c(TRUE,RID[-1] != RID[1:length(RID)-1]),]
    rownames(set1) <- set1$RID
#    print(rownames(sm$coefficients))
    print(nrow(set1))
    hist(set1$LOGTimeToLastVisit)
    
    setA <- set1[,predictorsAdas13]
    randomnumber <- sample(1:nrow(setA),nrow(setA),replace = TRUE)
    
    NC_ADAS_MODEL[[n]] <- MLMethod(DeltaAdas13 ~ .,setA[randomnumber,],...)
    
    sm <- summary(NC_ADAS_MODEL[[n]])
#    print(rownames(sm$coefficients))
    print(sm$R2)
    plot(set1$DeltaAdas13~predict(NC_ADAS_MODEL[[n]],setA))
    
    NC_ADAS_MODEL[[n]]$BSWiMS.model$bootCV$data <- NULL
    NC_ADAS_MODEL[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    NC_ADAS_MODEL[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL
    
    setA <- set1[,predictorsVentricle]
    NC_Ventricle_ICV_MODEL[[n]] <- MLMethod(DeltaVentricle ~ .,setA[randomnumber,],...)
    sm <- summary(NC_Ventricle_ICV_MODEL[[n]])
#    print(rownames(sm$coefficients))
    print(sm$R2)
    plot(set1$DeltaVentricle~predict(NC_Ventricle_ICV_MODEL[[n]],setA))
    
    NC_Ventricle_ICV_MODEL[[n]]$BSWiMS.model$bootCV$data <- NULL
    NC_Ventricle_ICV_MODEL[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    NC_Ventricle_ICV_MODEL[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL
    
  }
  
  ## AD Subset
  
  ADSubset <- subset(AdjustedFrame,DX == "Dementia" | DX == "MCI to Dementia")
  ADSubset <- ADSubset[complete.cases(ADSubset),]
  print(nrow(ADSubset))
  print(sum(is.na(ADSubset)))
  
  AD_ADAS_MODEL <- list();
  AD_Ventricle_ICV_MODEL <- list();
  
  for (n in 1:numberOfRandomSamples)
  {
    randomnumber <- sample(1:nrow(ADSubset),nrow(ADSubset))
    AD_Set <- ADSubset[randomnumber,]
    AD_Set <- AD_Set[order(as.numeric(AD_Set$RID)),]
    RID <- AD_Set$RID
    set1 <- AD_Set[c(TRUE,RID[-1] != RID[1:length(RID)-1]),]
    rownames(set1) <- set1$RID
    print(nrow(set1))
    hist(set1$LOGTimeToLastVisit)
    
    setA <- set1[,predictorsAdas13]
    randomnumber <- sample(1:nrow(setA),nrow(setA),replace = TRUE)
    
    AD_ADAS_MODEL[[n]] <- MLMethod(DeltaAdas13 ~ .,setA[randomnumber,],...)
    
    sm <- summary(AD_ADAS_MODEL[[n]])
#    print(rownames(sm$coefficients))
    print(sm$R2)
    plot(set1$DeltaAdas13~predict(AD_ADAS_MODEL[[n]],setA))
    
    AD_ADAS_MODEL[[n]]$BSWiMS.model$bootCV$data <- NULL
    AD_ADAS_MODEL[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    AD_ADAS_MODEL[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL
    
    setA <- set1[,predictorsVentricle]
    AD_Ventricle_ICV_MODEL[[n]] <- MLMethod(DeltaVentricle ~ .,setA[randomnumber,],...)
    sm <- summary(AD_Ventricle_ICV_MODEL[[n]])
#    print(rownames(sm$coefficients))
    print(sm$R2)
    plot(set1$DeltaVentricle~predict(AD_Ventricle_ICV_MODEL[[n]],setA))
    
    AD_Ventricle_ICV_MODEL[[n]]$BSWiMS.model$bootCV$data <- NULL
    AD_Ventricle_ICV_MODEL[[n]]$BSWiMS.model$bootCV$testOutcome <- NULL
    AD_Ventricle_ICV_MODEL[[n]]$BSWiMS.model$bootCV$testPrediction <- NULL
    
  }

  predicitionModels <- list(MCI_Ventricle_ICV_MODEL = MCI_Ventricle_ICV_MODEL,
                            MCI_ADAS_MODEL=MCI_ADAS_MODEL,
                            NC_Ventricle_ICV_MODEL = NC_Ventricle_ICV_MODEL,
                            NC_ADAS_MODEL = NC_ADAS_MODEL,
                            AD_Ventricle_ICV_MODEL = AD_Ventricle_ICV_MODEL,
                            AD_ADAS_MODEL = AD_ADAS_MODEL
  )
  
 return (predicitionModels)
}