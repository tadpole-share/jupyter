
print(getwd())
suppressMessages(library("FRESA.CAD"))
suppressMessages(library(readxl))

AdjustedTrainFrame <- read.csv("data/temp/dataTadpole$AdjustedTrainFrame.csv")
testingFrame <- read.csv("data/temp/dataTadpole$testingFrame.csv")
Test_Imputed <- read.csv("data/temp/dataTadpole$Test_Imputed.csv")
submissionTemplate <- as.data.frame(read_excel("data/TADPOLE_Simple_Submission_TeamName.xlsx"))
train_df <- read.csv("data/temp/train_df.csv", na.strings=c("NA",-4,"-4.0",""," "))


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


TrainingSet <- subset(train_df,D1==1)
rownames(TrainingSet) <- paste(TrainingSet$RID,TrainingSet$VISCODE,sep="_")
rownames(AdjustedTrainFrame) <- paste(AdjustedTrainFrame$RID,AdjustedTrainFrame$VISCODE,sep="_")

TrainingSet <- TrainingSet[order(TrainingSet$EXAMDATE),]
TrainingSet <- TrainingSet[order(as.numeric(TrainingSet$RID)),]

IGNORE = TRUE
if (IGNORE == FALSE){
source('R_scripts/TADPOLE_Train.R')
## Train 25 Models for the D2 subjects
CognitiveClassModels <- TrainTadpoleClassModels(AdjustedTrainFrame,
                                                predictors=c("AGE","PTGENDER",colnames(AdjustedTrainFrame)[-c(1:22)]),
                                                numberOfRandomSamples=25,
                                                delta=TRUE,
                                                MLMethod=HLCM_EM,
                                                hysteresis = 0.1)

save(CognitiveClassModels,file="data/temp/HLCM_EM_CognitiveClassModels_25.RDATA")
}else{
  load("data/temp/HLCM_EM_CognitiveClassModels_25.RDATA")
}
# Predict the models on D2 subjects
source('R_scripts/predictCognitiveStatus.R')
predictADNI <- forecastCognitiveStatus(CognitiveClassModels,testingFrame)

########################Regresion models
### Training the ADAS13 and Ventricles
dim(AdjustedTrainFrame)
### Get the original Data D3 Train
AdjustedTrainFrame$Ventricles <- TrainingSet[rownames(AdjustedTrainFrame),"Ventricles"]/TrainingSet[rownames(AdjustedTrainFrame),"ICV"]
AdjustedTrainFrame$ADAS13 <- TrainingSet[rownames(AdjustedTrainFrame),"ADAS13"]


  ## Train 50 models based on D1 data
  source('R_scripts/TADPOLE_Train_ADAS_ICV.R')
  CognitiveRegresModels <- TrainTadpoleRegresionModels(AdjustedTrainFrame,
                                                       predictors=c("AGE","PTGENDER",colnames(AdjustedTrainFrame)[-c(1:22)]),
                                                       numberOfRandomSamples=50,
                                                       MLMethod=HLCM_EM,
                                                       hysteresis = 0.1)
  
  save(CognitiveRegresModels,file="data/temp/HLCM_EM_CognitiveRegresModels_50_Nolog.RDATA")

source('R_scripts/predictTADPOLERegresions.R')

#################################### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### Ventricles and ADAS13 prediction preparation
## Transforming the test data set

testingFrame$Ventricles <- Test_Imputed[rownames(testingFrame),"Ventricles"]/Test_Imputed[rownames(testingFrame),"ICV"]
testingFrame$ADAS13 <- Test_Imputed[rownames(testingFrame),"ADAS13"]

## THe last time point required for forecasting ADAS13 and Ventricles
ltptf <- testingFrame
ltptf <- ltptf[order(ltptf$EXAMDATE),]
ltptf <- ltptf[order(as.numeric(ltptf$RID)),]
rids <- ltptf$RID
ltptf <- ltptf[c(rids[1:(length(rids)-1)] != rids[-1],TRUE),]
rownames(ltptf) <- ltptf$RID

print("forecast_d2")
  ### Forecasting 5 years. The forcast transfomrs back to the actual space
  source('R_scripts/FiveYearForecast.R')
  forecast <- FiveYearForeCast(predictADNI,
                               testDataset=ltptf,
                               ADAS_Ventricle_Models=CognitiveRegresModels,
                               Subject_datestoPredict=submissionTemplate)
  
  write.csv(forecast,file="data/temp/HLCM_EM_ForecastD2_BORREGOS_TEC.csv")

