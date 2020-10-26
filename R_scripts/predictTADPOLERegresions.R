#' forecastCognitiveStatus
#' Forecast ADAS 13 and Ventricle to the specified date 
#' @param Models list of models to be used
#' @param TestDataFrame the frame to be tested
#'
#' @return
#' @export
#'
#' @examples
forecastRegressions <- function(Models,
                                TestDataFrame,
                                futuredate){
  
  #args
  #Models = ADAS_Ventricle_Models
  #TestDataFrame = testDataset
  #futuredate=afdate
  ##
  
  deltaTime <- as.numeric(futuredate - as.Date(TestDataFrame$EXAMDATE))/365.25
  
  hist(deltaTime)
  
  if (any(deltaTime > 10.0)){
    deltaTime[deltaTime > 10.0] <- 10.0
  }
  
  TestDataFrame$TimeToLastVisit <- deltaTime
  TestDataFrame$SQRTimeToLastVisit <- deltaTime*deltaTime
  TestDataFrame$LOGTimeToLastVisit <- log(1+deltaTime)
  TestDataFrame$CUBTimeToLastVisit <- deltaTime*deltaTime*deltaTime*deltaTime
  TestDataFrame$SQRTTimeToLastVisit <- sqrt(deltaTime)
  TestDataFrame$MeanCVLOGTimeToLastVisit <- log(1+deltaTime)*TestDataFrame$COMeanThickness
  TestDataFrame$MMSELOGTimeToLastVisit <- log(1+deltaTime)*TestDataFrame$MMSE
  
  
  TestDataFrame$DeltaAdas13 <- numeric(nrow(TestDataFrame))
  TestDataFrame$DeltaVentricle <- numeric(nrow(TestDataFrame))
  
  
  ADAS13_NC <- NULL
  ADAS13_MCI <- NULL
  ADAS13_AD <- NULL
  Ventricles_NC <- NULL
  Ventricles_MCI <- NULL
  Ventricles_AD <- NULL
  
  for (n in c(1:length(Models$MCI_Ventricle_ICV_MODEL))){
    ADAS13_NC <- cbind(ADAS13_NC,predict(Models$NC_ADAS_MODEL[[n]],TestDataFrame) + TestDataFrame$ADAS13)
    ADAS13_MCI <- cbind(ADAS13_MCI,predict(Models$MCI_ADAS_MODEL[[n]],TestDataFrame) + TestDataFrame$ADAS13)
    ADAS13_AD <- cbind(ADAS13_AD,predict(Models$AD_ADAS_MODEL[[n]],TestDataFrame) + TestDataFrame$ADAS13)
    
    Ventricles_NC <- cbind(Ventricles_NC,predict(Models$NC_Ventricle_ICV_MODEL[[n]],TestDataFrame) + TestDataFrame$Ventricles)
    Ventricles_MCI <- cbind(Ventricles_MCI,predict(Models$MCI_Ventricle_ICV_MODEL[[n]],TestDataFrame) + TestDataFrame$Ventricles)
    Ventricles_AD <- cbind(Ventricles_AD,predict(Models$AD_Ventricle_ICV_MODEL[[n]],TestDataFrame) + TestDataFrame$Ventricles)
  
  }
  
  rownames(ADAS13_NC) <- rownames(TestDataFrame)
  rownames(ADAS13_MCI) <- rownames(TestDataFrame)
  rownames(ADAS13_AD) <- rownames(TestDataFrame)
  rownames(Ventricles_NC) <- rownames(TestDataFrame)
  rownames(Ventricles_MCI) <- rownames(TestDataFrame)
  rownames(Ventricles_AD) <- rownames(TestDataFrame)
  
  predictions <- list(ADAS13_NC = as.data.frame(ADAS13_NC),
                      ADAS13_MCI = as.data.frame(ADAS13_MCI),
                      ADAS13_AD = as.data.frame(ADAS13_AD),
                      Ventricles_NC = as.data.frame(Ventricles_NC),
                      Ventricles_MCI = as.data.frame(Ventricles_MCI),
                      Ventricles_AD = as.data.frame(Ventricles_AD)
                      )

  return(predictions)
}
  