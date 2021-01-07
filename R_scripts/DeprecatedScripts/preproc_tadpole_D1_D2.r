

#TRAIN SETS of spliting is in the preprocess func
suppressMessages(library("FRESA.CAD"))
  
  TADPOLE_D1_D2 <- read.csv("data/train_testdf_D1D2.csv", na.strings=c("NA",-4,"-4.0",""," ","NaN"))
  TADPOLE_D1_D2$X <- NULL
  source('R_scripts/dataPreprocessing.R')
  TADPOLE_D1_D2_Dict <- read.csv("data/TADPOLE_D1_D2_Dict.csv", na.strings=c("NA",-4,"-4.0",""," "))
  
  TADPOLE_D1_D2$EXAMDATE <- as.Date(TADPOLE_D1_D2$EXAMDATE)
  #Train Test Split
  TrainingSet <- subset(TADPOLE_D1_D2,D1==1)
  D2TesingSet <- subset(TADPOLE_D1_D2,D2==1)
  rownames(TrainingSet) <- paste(TrainingSet$RID,TrainingSet$VISCODE,sep="_")
  rownames(D2TesingSet) <- paste(D2TesingSet$RID,D2TesingSet$VISCODE,sep="_")

  #Data Processing
  ### Data conditioning and preparation for D2
  
  TrainingSet <- TrainingSet[order(TrainingSet$EXAMDATE),]
  TrainingSet <- TrainingSet[order(as.numeric(TrainingSet$RID)),]
  
  D2TesingSet <- D2TesingSet[order(D2TesingSet$EXAMDATE),]
  D2TesingSet <- D2TesingSet[order(as.numeric(D2TesingSet$RID)),]
  
  
  
  dataTadpole <- suppressMessages(dataTADPOLEPreprocesing(TrainingSet,
                                             D2TesingSet,
                                             TADPOLE_D1_D2_Dict,
                                             MinVisit=36,
                                             colImputeThreshold=0.25,
                                             rowImputeThreshold=0.10))
      
      #save(dataTadpole,file="D2DataFrames_impute.RDATA")
      
  write.csv(dataTadpole$AdjustedTrainFrame,"data/dataTadpole$AdjustedTrainFrame.csv")
  write.csv(dataTadpole$testingFrame,"data/dataTadpole$testingFrame.csv")
  write.csv(dataTadpole$Train_Imputed,"data/dataTadpole$Train_Imputed.csv")
  write.csv(dataTadpole$Test_Imputed,"data/dataTadpole$Test_Imputed.csv")
    

  




