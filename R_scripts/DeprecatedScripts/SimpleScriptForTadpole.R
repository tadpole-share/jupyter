
library("devtools")
install_github("joseTamezPena/FRESA.CAD")

##Load the Datasets
library(readxl)



TADPOLE_D1_D2_Dict <- read.csv("./data/TADPOLE_D1_D2_Dict.csv", na.strings=c("NA",-4,"-4.0",""," "))
TADPOLE_D1_D2 <- read.csv("./data/TADPOLE_D1_D2.csv", na.strings=c("NA",-4,"-4.0",""," "))
TADPOLE_D3 <- read.csv("./data/TADPOLE_D3.csv", na.strings=c("NA",-4,"-4.0",""," ","NaN"))
TADPOLE_D4_corr <- read.csv("./data/TADPOLE_D4_corr.csv")

submissionTemplate <- read_excel("./data/TADPOLE_Simple_Submission_TeamName.xlsx")

submissionTemplate$`Forecast Date` <- as.Date(paste(submissionTemplate$`Forecast Date`,"-01",sep=""))
submissionTemplate$`CN relative probability` <- as.numeric(nrow(submissionTemplate))
submissionTemplate$`MCI relative probability` <-  as.numeric(nrow(submissionTemplate))
submissionTemplate$`AD relative probability` <-  as.numeric(nrow(submissionTemplate))

#DataSplit

TrainingSet <- subset(TADPOLE_D1_D2,D1==1)
D2TesingSet <- subset(TADPOLE_D1_D2,D2==1)

#DataProcessing

source('./R_scripts/dataPreprocessing.R')
source('./R_scripts/TADPOLE_Train.R')
source('./R_scripts/predictCognitiveStatus.R')



dataTadpole <- dataTADPOLEPreprocesing(TrainingSet,
                                       TADPOLE_D3,
                                       TADPOLE_D1_D2_Dict,
                                       MinVisit=36,
                                       colImputeThreshold=0.25,
                                       rowImputeThreshold=0.25)


save(dataTadpole,file="D3DataFrames.RDATA")
load(file="D3DataFrames.RDATA")

D3Testing <- dataTadpole$testingFrame

dataTadpole <- dataTADPOLEPreprocesing(TrainingSet,
                                       D2TesingSet,
                                       TADPOLE_D1_D2_Dict,
                                       MinVisit=36,
                                       colImputeThreshold=0.25,
                                       rowImputeThreshold=0.25)


save(dataTadpole,file="D2DataFrames.RDATA")
load(file="D2DataFrames.RDATA")


CognitiveClassModels <- TrainTadpoleClassModels(dataTadpole$AdjustedTrainFrame,
                        predictors=c("AGE","PTGENDER",colnames(dataTadpole$AdjustedTrainFrame)[-c(1:22)]),
                        numberOfRandomSamples=10,
                        MLMethod=BSWiMS.model,
                        NumberofRepeats = 5)


save(CognitiveClassModels,file="CognitiveClassModels_10b.RDATA")

load(file="CognitiveClassModels.RDATA")

#CognitiveClassModels$MCIToADModels[[1]]$BSWiMS.model$bootCV$data
#CognitiveClassModels$CrossModels[[1]]$BSWiMS.model$bootCV$data
#CognitiveClassModels$CrossModels[[1]]$oridinalModels$data

predictADNI <- forecastCognitiveStatus(CognitiveClassModels,dataTadpole$testingFrame)

hist(predictADNI$NCToMCITimeprediction)
hist(predictADNI$MCITOADTimeprediction)
hist(predictADNI$MCITONCTimeprediction)

statusLO <- (predictADNI$lastKownDX == "NL" | predictADNI$lastKownDX == "MCI to NL") + 
  2*(predictADNI$lastKownDX == "Dementia to MCI" | predictADNI$lastKownDX == "NL to MCI" | predictADNI$lastKownDX == "MCI") + 
  3*(predictADNI$lastKownDX == "MCI to Dementia" | predictADNI$lastKownDX == "Dementia")


table(predictADNI$crossprediction$pDX,statusLO)
table(predictADNI$NCToMCIprediction>0.5,statusLO)
table(predictADNI$MCITONCprediction>0.5,statusLO)


forecast <- FiveYearForeCast(predictADNI,Subject_datestoPredict=submissionTemplate)

take2018 <- as.data.frame(forecast[forecast$`Forecast Month`==6,])
class2018 <- as.numeric(apply(take2018[,c(4,5,6)],1,which.max))
names(class2018) <- take2018$RID
D4Dx <- TADPOLE_D4_corr$Diagnosis
names(D4Dx) <- TADPOLE_D4_corr$RID
class2018D4 <- class2018[names(D4Dx)]
table(class2018D4,D4Dx)
table(TADPOLE_D4_corr$DX_LastVisitADNI2,D4Dx)
table(TADPOLE_D4_corr$DX_LastVisitADNI2,class2018D4)
lastDX <- predictADNI$lastKownDX
lastDX <- lastDX[names(D4Dx)]
statusLODX <- (lastDX == "NL" |lastDX == "MCI to NL") + 
  2*(lastDX == "Dementia to MCI" | lastDX == "NL to MCI" | lastDX == "MCI") + 
  3*(lastDX == "MCI to Dementia" | lastDX == "Dementia")


table(statusLODX,D4Dx)

