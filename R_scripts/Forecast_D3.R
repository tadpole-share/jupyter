suppressMessages(library("FRESA.CAD"))
suppressMessages(library(readxl))

AdjustedTrainFrame <- read.csv("data/temp/dataTadpoleD3$AdjustedTrainFrame.csv")
testingFrame <- read.csv("data/temp/dataTadpoleD3$testingFrame.csv")
Test_Imputed <- read.csv("data/temp/dataTadpoleD3$Test_Imputed.csv")

TADPOLE_D3 <- read.csv("data/TADPOLE_D3.csv", na.strings=c("NA",-4,"-4.0",""," ","NaN"))
train_df <- read.csv("data/temp/train_df.csv", na.strings=c("NA",-4,"-4.0",""," "))
TADPOLE_D1_D2_Dict <- read.csv("data/TADPOLE_D1_D2_Dict.csv", na.strings=c("NA",-4,"-4.0",""," "))
submissionTemplate <- as.data.frame(read_excel("data/TADPOLE_Simple_Submission_TeamName.xlsx"))

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
submissionTemplate <- submissionTemplate[order(submissionTemplate$`Forecast Month`),]

TADPOLE_D3$EXAMDATE <- as.Date(TADPOLE_D3$EXAMDATE)

TrainingSet <- subset(train_df,D1==1)

rownames(TrainingSet) <- paste(TrainingSet$RID,TrainingSet$VISCODE,sep="_")
rownames(TADPOLE_D3) <- paste(TADPOLE_D3$RID,TADPOLE_D3$VISCODE,sep="_")

TrainingSet <- TrainingSet[order(TrainingSet$EXAMDATE),]
TrainingSet <- TrainingSet[order(as.numeric(TrainingSet$RID)),]


#D3 Cross sectional
## First Remove D2 subjects from Training Set
D3IDS <- TADPOLE_D3$RID
D3TrainingSet <- TrainingSet[!(TrainingSet$RID %in% D3IDS),]
  

  source('R_scripts/TADPOLE_Train.R')
  ## Build the 35 predictive models of cognitive status
  D3CognitiveClassModels <- TrainTadpoleClassModels(AdjustedTrainFrame,
                                                    predictors=c("AGE","PTGENDER",colnames(AdjustedTrainFrame)[-c(1:22)]),
                                                    numberOfRandomSamples=25,
                                                    MLMethod=BSWiMS.model,
                                                    NumberofRepeats = 1)
  save(D3CognitiveClassModels,file="data/temp/D3CognitiveClassModels_25.RDATA")
 
  

## Predict all D3 congnitive status
  source('R_scripts/predictCognitiveStatus.R')
predictADNID3 <- forecastCognitiveStatus(D3CognitiveClassModels,testingFrame)


## Train D3 Correlations ADAS 13 and Ventricles
 AdjustedTrainFrame$Ventricles <- D3TrainingSet[rownames(AdjustedTrainFrame),"Ventricles"]/D3TrainingSet[rownames( AdjustedTrainFrame),"ICV"]
 AdjustedTrainFrame$ADAS13 <- D3TrainingSet[rownames(AdjustedTrainFrame),"ADAS13"]
  
  source('R_scripts/TADPOLE_Train_ADAS_ICV.R')
  D3RegresModels <- TrainTadpoleRegresionModels( AdjustedTrainFrame,
                                                predictors=c("AGE","PTGENDER",colnames( AdjustedTrainFrame)[-c(1:22)]),
                                                numberOfRandomSamples=50,
                                                MLMethod=BSWiMS.model,
                                                NumberofRepeats = 1)
  
  save(D3RegresModels,file="data/temp/D3RegresModelss_50_Nolog.RDATA")

## Predict the D3 ADAS13 and Ventricles
  source('R_scripts/predictTADPOLERegresions.R')
  
 testingFrame$Ventricles <-  Test_Imputed[rownames( testingFrame),"Ventricles"]/ Test_Imputed[rownames( testingFrame),"ICV"]
 testingFrame$ADAS13 <-  Test_Imputed[rownames( testingFrame),"ADAS13"]

### The last time D3 point
ltptf <-  testingFrame
ltptf <- ltptf[order(ltptf$EXAMDATE),]
ltptf <- ltptf[order(as.numeric(ltptf$RID)),]
rids <- ltptf$RID
ltptf <- ltptf[c(rids[1:(length(rids)-1)] != rids[-1],TRUE),]
rownames(ltptf) <- ltptf$RID

  source('R_scripts/FiveYearForecast.R')
  ## Forecast the testing set
  forecastD3 <- FiveYearForeCast(predictADNID3,
                                 testDataset=ltptf,
                                 ADAS_Ventricle_Models=D3RegresModels,
                                 Subject_datestoPredict=submissionTemplate)
  write.csv(forecastD3,file="data/temp/_ForecastD3_BORREGOS_TEC.csv")
