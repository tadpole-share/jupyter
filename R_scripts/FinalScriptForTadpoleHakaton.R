##Load the Datasets
library(readxl)

FinalScriptForTadpoleHakaton <- function(preTrain=TRUE){ 
  print(getwd())
  ### The scripts 
  source('../R_scripts/dataPreprocessing.R')
  source('../R_scripts/TADPOLE_Train.R')
  source('../R_scripts/predictCognitiveStatus.R')
  source('../R_scripts/FiveYearForecast.R')
  source('../R_scripts/TADPOLE_Train_ADAS_ICV.R')
  source('../R_scripts/predictTADPOLERegresions.R')
  
  
  TADPOLE_D1_D2_Dict <- read.csv("../data/TADPOLE_D1_D2_Dict.csv", na.strings=c("NA",-4,"-4.0",""," "))
  TADPOLE_D1_D2 <- read.csv("../data/TADPOLE_D1_D2.csv", na.strings=c("NA",-4,"-4.0",""," "))
  TADPOLE_D3 <- read.csv("../data/TADPOLE_D3.csv", na.strings=c("NA",-4,"-4.0",""," ","NaN"))
  submissionTemplate <- as.data.frame(read_excel("../data/TADPOLE_Simple_Submission_TeamName.xlsx"))
  
  submissionTemplate$`Forecast Date` <- as.Date(paste(submissionTemplate$`Forecast Date`,"-01",sep=""))
  submissionTemplate$`CN relative probability` <- as.numeric(nrow(submissionTemplate))
  submissionTemplate$`MCI relative probability` <-  as.numeric(nrow(submissionTemplate))
  submissionTemplate$`AD relative probability` <-  as.numeric(nrow(submissionTemplate))
  submissionTemplate$ADAS13 <-  as.numeric(nrow(submissionTemplate))
  submissionTemplate$`ADAS13 50% CI lower` <-  as.numeric(nrow(submissionTemplate))
  submissionTemplate$`ADAS13 50% CI upper` <-  as.numeric(nrow(submissionTemplate))
  submissionTemplate$Ventricles_ICV <-  as.numeric(nrow(submissionTemplate))
  submissionTemplate$`Ventricles_ICV 50% CI lower` <-  as.numeric(nrow(submissionTemplate))
  submissionTemplate$`Ventricles_ICV 50% CI upper` <-  as.numeric(nrow(submissionTemplate))
  
  TADPOLE_D1_D2$EXAMDATE <- as.Date(TADPOLE_D1_D2$EXAMDATE)
  TADPOLE_D3$EXAMDATE <- as.Date(TADPOLE_D3$EXAMDATE)
  submissionTemplate <- submissionTemplate[order(submissionTemplate$`Forecast Month`),]
  
  #Train Test Split
  TrainingSet <- subset(TADPOLE_D1_D2,D1==1)
  D2TesingSet <- subset(TADPOLE_D1_D2,D2==1)
  
  rownames(TrainingSet) <- paste(TrainingSet$RID,TrainingSet$VISCODE,sep="_")
  rownames(D2TesingSet) <- paste(D2TesingSet$RID,D2TesingSet$VISCODE,sep="_")
  rownames(TADPOLE_D3) <- paste(TADPOLE_D3$RID,TADPOLE_D3$VISCODE,sep="_")
  
  #Data Processing
  ### Data conditioning and preparation for D2
  
  TrainingSet <- TrainingSet[order(TrainingSet$EXAMDATE),]
  TrainingSet <- TrainingSet[order(as.numeric(TrainingSet$RID)),]
  
  D2TesingSet <- D2TesingSet[order(D2TesingSet$EXAMDATE),]
  D2TesingSet <- D2TesingSet[order(as.numeric(D2TesingSet$RID)),]
  
  if (preTrain == FALSE){
    dataTadpole <- dataTADPOLEPreprocesing(TrainingSet,
                                           D2TesingSet,
                                           TADPOLE_D1_D2_Dict,
                                           MinVisit=36,
                                           colImputeThreshold=0.25,
                                           rowImputeThreshold=0.10)
    save(dataTadpole,file="D2DataFrames_impute.RDATA")
    
    ## Train 25 Models for the D2 subjects
    CognitiveClassModels <- TrainTadpoleClassModels(dataTadpole$AdjustedTrainFrame,
                                                    predictors=c("AGE","PTGENDER",colnames(dataTadpole$AdjustedTrainFrame)[-c(1:22)]),
                                                    numberOfRandomSamples=25,
                                                    delta=TRUE,
                                                    MLMethod=BSWiMS.model,
                                                    NumberofRepeats = 1)
    
    save(CognitiveClassModels,file="CognitiveClassModels_25.RDATA")
    

  }
  else {
    load("D2DataFrames_impute.RDATA")
    load("CognitiveClassModels_25.RDATA")
    
  }
  
  ## Predict the models on D2 subjects
  predictADNI <- forecastCognitiveStatus(CognitiveClassModels,dataTadpole$testingFrame)
  
  
  ### Training the ADAS13 and Ventricles
  
  ### Get the original Data D3 Train
  dataTadpole$AdjustedTrainFrame$Ventricles <- TrainingSet[rownames(dataTadpole$AdjustedTrainFrame),"Ventricles"]/TrainingSet[rownames(dataTadpole$AdjustedTrainFrame),"ICV"]
  dataTadpole$AdjustedTrainFrame$ADAS13 <- TrainingSet[rownames(dataTadpole$AdjustedTrainFrame),"ADAS13"]
  if (preTrain == FALSE){
  ## Train 50 models based on D1 data
  CognitiveRegresModels <- TrainTadpoleRegresionModels(dataTadpole$AdjustedTrainFrame,
                                                  predictors=c("AGE","PTGENDER",colnames(dataTadpole$AdjustedTrainFrame)[-c(1:22)]),
                                                  numberOfRandomSamples=50,
                                                  MLMethod=BSWiMS.model,
                                                  NumberofRepeats = 1)
  
  save(CognitiveRegresModels,file="CognitiveRegresModels_50_Nolog.RDATA")
  } else{
    load("CognitiveRegresModels_50_Nolog.RDATA")
  }
  
  ### Ventricles and ADAS13 prediction preparation
  
  ## Transforming the test data set
  
  dataTadpole$testingFrame$Ventricles <- dataTadpole$Test_Imputed[rownames(dataTadpole$testingFrame),"Ventricles"]/dataTadpole$Test_Imputed[rownames(dataTadpole$testingFrame),"ICV"]
  dataTadpole$testingFrame$ADAS13 <- dataTadpole$Test_Imputed[rownames(dataTadpole$testingFrame),"ADAS13"]
  
  ## THe last time point required for forecasting ADAS13 and Ventricles
  ltptf <- dataTadpole$testingFrame
  ltptf <- ltptf[order(ltptf$EXAMDATE),]
  ltptf <- ltptf[order(as.numeric(ltptf$RID)),]
  rids <- ltptf$RID
  ltptf <- ltptf[c(rids[1:(length(rids)-1)] != rids[-1],TRUE),]
  rownames(ltptf) <- ltptf$RID
  
  if (preTrain == FALSE){
  ### Forecasting 5 years. The forcast transfomrs back to the actual space
  forecast <- FiveYearForeCast(predictADNI,
                               testDataset=ltptf,
                               ADAS_Ventricle_Models=CognitiveRegresModels,
                               Subject_datestoPredict=submissionTemplate)
  
  write.csv(forecast,file="ForecastD2_BORREGOS_TEC.csv")
  } else{
    forecast <- read.csv("ForecastD2_BORREGOS_TEC.csv")
  }
  
  
  #D3 Cross sectional
  
  ## First Remove D2 subjects from Training Set
  D3IDS <- TADPOLE_D3$RID
  D3TrainingSet <- TrainingSet[!(TrainingSet$RID %in% D3IDS),]
  
  if (preTrain == FALSE){
  ## Conditioning the data sets
  dataTadpoleD3 <- dataTADPOLEPreprocesing(D3TrainingSet,
                                           TADPOLE_D3,
                                           TADPOLE_D1_D2_Dict,
                                           MinVisit=18,
                                           colImputeThreshold=0.15,
                                           rowImputeThreshold=0.10,
                                           includeID=FALSE)
  save(dataTadpoleD3,file="D3DataFrames.RDATA")
  
  ## Build the 35 predictive models of cognitive status
  D3CognitiveClassModels <- TrainTadpoleClassModels(dataTadpoleD3$AdjustedTrainFrame,
                                                  predictors=c("AGE","PTGENDER",colnames(dataTadpoleD3$AdjustedTrainFrame)[-c(1:22)]),
                                                  numberOfRandomSamples=25,
                                                  MLMethod=BSWiMS.model,
                                                  NumberofRepeats = 1)
  save(D3CognitiveClassModels,file="D3CognitiveClassModels_25.RDATA")
  } else{
    load("D3DataFrames.RDATA")
    load("D3CognitiveClassModels_25.RDATA")
    
  }
  
  ## Predict all D3 congnitive status
  predictADNID3 <- forecastCognitiveStatus(D3CognitiveClassModels,dataTadpoleD3$testingFrame)
  
  
  ## Train D3 Correlations ADAS 13 and Ventricles
  dataTadpoleD3$AdjustedTrainFrame$Ventricles <- D3TrainingSet[rownames(dataTadpoleD3$AdjustedTrainFrame),"Ventricles"]/D3TrainingSet[rownames(dataTadpoleD3$AdjustedTrainFrame),"ICV"]
  dataTadpoleD3$AdjustedTrainFrame$ADAS13 <- D3TrainingSet[rownames(dataTadpoleD3$AdjustedTrainFrame),"ADAS13"]
  
  if (preTrain == FALSE){
  D3RegresModels <- TrainTadpoleRegresionModels(dataTadpoleD3$AdjustedTrainFrame,
                                                       predictors=c("AGE","PTGENDER",colnames(dataTadpoleD3$AdjustedTrainFrame)[-c(1:22)]),
                                                       numberOfRandomSamples=50,
                                                       MLMethod=BSWiMS.model,
                                                       NumberofRepeats = 1)
  
  save(D3RegresModels,file="D3RegresModelss_50_Nolog.RDATA")
  } else{
    load("D3RegresModelss_50_Nolog.RDATA")
  }
  ## Predict the D3 ADAS13 and Ventricles
  
  dataTadpoleD3$testingFrame$Ventricles <- dataTadpoleD3$Test_Imputed[rownames(dataTadpoleD3$testingFrame),"Ventricles"]/dataTadpoleD3$Test_Imputed[rownames(dataTadpoleD3$testingFrame),"ICV"]
  dataTadpoleD3$testingFrame$ADAS13 <- dataTadpoleD3$Test_Imputed[rownames(dataTadpoleD3$testingFrame),"ADAS13"]
  
  ### The last time D3 point
  ltptf <- dataTadpoleD3$testingFrame
  ltptf <- ltptf[order(ltptf$EXAMDATE),]
  ltptf <- ltptf[order(as.numeric(ltptf$RID)),]
  rids <- ltptf$RID
  ltptf <- ltptf[c(rids[1:(length(rids)-1)] != rids[-1],TRUE),]
  rownames(ltptf) <- ltptf$RID
  
  if (preTrain == FALSE){
  ## Forecast the testing set
  forecastD3 <- FiveYearForeCast(predictADNID3,
                                 testDataset=ltptf,
                                 ADAS_Ventricle_Models=D3RegresModels,
                                 Subject_datestoPredict=submissionTemplate)
  write.csv(forecastD3,file="ForecastD3_BORREGOS_TEC.csv")
  }else{
    forecastD3 <- read.csv("ForecastD3_BORREGOS_TEC.csv")
    
  }
  
}


