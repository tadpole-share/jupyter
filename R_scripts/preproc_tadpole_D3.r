train_df <- read.csv("data/train_df.csv", na.strings=c("NA",-4,"-4.0",""," "))
TADPOLE_D1_D2_Dict <- read.csv("data/TADPOLE_D1_D2_Dict.csv", na.strings=c("NA",-4,"-4.0",""," "))
submissionTemplate <- as.data.frame(read_excel("data/TADPOLE_Simple_Submission_TeamName.xlsx"))


TADPOLE_D3$EXAMDATE <- as.Date(TADPOLE_D3$EXAMDATE)
rownames(TADPOLE_D3) <- paste(TADPOLE_D3$RID,TADPOLE_D3$VISCODE,sep="_")

TrainingSet <- subset(train_df,D1==1)

rownames(TrainingSet) <- paste(TrainingSet$RID,TrainingSet$VISCODE,sep="_")

TrainingSet <- TrainingSet[order(TrainingSet$EXAMDATE),]
TrainingSet <- TrainingSet[order(as.numeric(TrainingSet$RID)),]


#D3 Cross sectional
## First Remove D2 subjects from Training Set
D3IDS <- TADPOLE_D3$RID
D3TrainingSet <- TrainingSet[!(TrainingSet$RID %in% D3IDS),]

  ## Conditioning the data sets
  source('R_scripts/dataPreprocessing.R')
  dataTadpoleD3 <- suppressMessages(dataTADPOLEPreprocesing(D3TrainingSet,
                                                           TADPOLE_D3,
                                                           TADPOLE_D1_D2_Dict,
                                                           MinVisit=18,
                                                           colImputeThreshold=0.15,
                                                           rowImputeThreshold=0.10,
                                                           includeID=FALSE))
  #save(dataTadpoleD3,file="data/temp/D3DataFrames.RDATA")
  write.csv(dataTadpoleD3$AdjustedTrainFrame,"data/dataTadpoleD3$AdjustedTrainFrame.csv")
  write.csv(dataTadpoleD3$testingFrame,"data/dataTadpoleD3$testingFrame.csv")
  write.csv(dataTadpoleD3$Train_Imputed,"data/dataTadpoleD3$Train_Imputed.csv")
  write.csv(dataTadpoleD3$Test_Imputed,"data/dataTadpoleD3$Test_Imputed.csv")
  
