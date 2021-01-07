#train_frame: The raw training input data
#test_Frame: The raw testing data
#dictionary: The data dictionary
#MinVisit: The minumum observation in months for column imputation
#colImputeThreshold: The minimum fraction of missing data per column
#rowImputeThreshold: The minimum fraction of missing data per patient

#Returns the Adjusted Frames
#Train Adjusted
#Test Adjusted

dataTADPOLEPreprocesing <- function(train_frame,
                                    test_Frame,
                                    dictionary,
                                    MinVisit=36,
                                    colImputeThreshold=0.25,
                                    rowImputeThreshold=0.10,
                                    includeID=TRUE){
  library("FRESA.CAD")
  
  SetIDSColumns <- c("RID","D1","D2","SITE","COLPROT","ORIGPROT")
  DatesColumns <- c("VISCODE","EXAMDATE_bl","EXAMDATE","Years_bl","Month_bl","Month","M")
  Diagnosis <- c("DX","DX_bl","DXCHANGE")
  BasicColumns <- c("AGE","PTGENDER","PTEDUCAT","PTETHCAT","PTRACCAT","PTMARRY")
  QuestionsColumnsSet1 <- c("CDRSB","ADAS11","ADAS13","MMSE","RAVLT_learning","RAVLT_immediate","FAQ","MOCA","EcogPtMem","EcogPtLang","EcogPtVisspat","EcogPtPlan","EcogPtOrgan","EcogPtDivatt","EcogPtTotal","EcogSPMem","EcogSPLang","EcogSPVisspat","EcogSPPlan","EcogSPOrgan","EcogSPDivatt","EcogSPTotal")
  QuestionsColumnsSet2 <- c("CDRSB_bl","ADAS11_bl","ADAS13_bl","MMSE_bl","RAVLT_learning_bl","RAVLT_immediate_bl","FAQ_bl","MOCA_bl","EcogPtMem_bl","EcogPtLang_bl","EcogPtVisspat_bl","EcogPtPlan_bl","EcogPtOrgan_bl","EcogPtDivatt_bl","EcogPtTotal_bl","EcogSPMem_bl","EcogSPLang_bl","EcogSPVisspat_bl","EcogSPPlan_bl","EcogSPOrgan_bl","EcogSPDivatt_bl","EcogSPTotal_bl")
  BaseFeaturesColumnsData <- c("APOE4","FDG","PIB","AV45","Ventricles","Hippocampus","WholeBrain","Entorhinal","Fusiform","MidTemp","ICV","Ventricles_bl","Hippocampus_bl","WholeBrain_bl","Entorhinal_bl","Fusiform_bl","MidTemp_bl","ICV_bl","FDG_bl","PIB_bl","AV45_bl","ABETA_UPENNBIOMK9_04_19_17","TAU_UPENNBIOMK9_04_19_17","PTAU_UPENNBIOMK9_04_19_17")
  
  train_frame$RID <- as.numeric(train_frame$RID)
  train_frame$AGE <- as.numeric(train_frame$AGE)
  train_frame$PTGENDER <- 1*(train_frame$PTGENDER=="Male")
  test_Frame$RID <- as.numeric(test_Frame$RID)
  test_Frame$AGE <- as.numeric(test_Frame$AGE)
  test_Frame$PTGENDER <- 1*(test_Frame$PTGENDER=="Male")
  
  
  
  combinedBasicCol <- c(SetIDSColumns,DatesColumns,Diagnosis,BasicColumns)
  notQuantitative <- c(1:length(combinedBasicCol))
  
  volumeCOlumns <- subset(dictionary,UNITS=="mm3")$FLDNAME
  volumeCOlumns <- volumeCOlumns[order(dictionary[dictionary$FLDNAME %in% volumeCOlumns,"TEXT"])]
  
  AreaCOlumns <- subset(dictionary,UNITS=="mm2")$FLDNAME
  AreaCOlumns <- AreaCOlumns[order(dictionary[dictionary$FLDNAME %in% AreaCOlumns,"TEXT"])]
  
  ThicknessCOlumns <- subset(dictionary,UNITS=="mm")$FLDNAME
  ThicknessCOlumns <- ThicknessCOlumns[order(dictionary[dictionary$FLDNAME %in% ThicknessCOlumns,"TEXT"])]
  
  Rightlocations <- dictionary[regexpr('Right', dictionary$TEXT) > 0,"FLDNAME"]
  Leftlocations <- dictionary[regexpr('Left', dictionary$TEXT) > 0,"FLDNAME"]
  
  VolumeRight <- volumeCOlumns[volumeCOlumns %in% Rightlocations]
  VolumeLeft <- volumeCOlumns[volumeCOlumns %in% Leftlocations]
  
  AreaRight <- AreaCOlumns[AreaCOlumns %in% Rightlocations]
  AreaLeft <- AreaCOlumns[AreaCOlumns %in% Leftlocations]
  
  ThicknessRight <- ThicknessCOlumns[ThicknessCOlumns %in% Rightlocations]
  ThicknessLeft <- ThicknessCOlumns[ThicknessCOlumns %in% Leftlocations]
  
  otherVolumes <- volumeCOlumns[!(volumeCOlumns %in% c(Rightlocations,Leftlocations))]
  
  print(dictionary[dictionary$FLDNAME %in% ThicknessRight[1:5],"TEXT"])
  print(dictionary[dictionary$FLDNAME %in% ThicknessLeft[1:5],"TEXT"])
  

  train_frame_Transformed <- train_frame[,c(SetIDSColumns,DatesColumns,Diagnosis,BasicColumns,QuestionsColumnsSet1,QuestionsColumnsSet2,BaseFeaturesColumnsData,otherVolumes,VolumeRight,VolumeLeft,AreaRight,AreaLeft,ThicknessRight,ThicknessLeft)]
  testcolnames <- colnames(test_Frame)
  traincolnames <- colnames(train_frame_Transformed)
  print(c(length(testcolnames),length(traincolnames)))
  trainNotInTest <- !(traincolnames %in% testcolnames)
  if (sum(trainNotInTest)>0)
  {
    print(traincolnames[trainNotInTest])
    
    missingTestColumns <- NULL
    for (selcolumns in traincolnames[trainNotInTest])
    {
      missingTestColumns <- cbind(missingTestColumns,rep(NA,nrow(test_Frame)))
    }
    colnames(missingTestColumns) <- traincolnames[trainNotInTest]
    test_Frame_Transformed <- cbind(test_Frame,missingTestColumns)
  }
  else
  {
    test_Frame_Transformed <- test_Frame
  }
  Used_Train_columns <- colnames(train_frame_Transformed)
  test_Frame_Transformed <- test_Frame_Transformed[,Used_Train_columns]
  

##Train  
  
  colnumeric <- c((length(combinedBasicCol)+1):ncol(train_frame_Transformed))
  train_frame_Transformed[,colnumeric] <- sapply(train_frame_Transformed[,colnumeric],as.numeric)
  
  
  train_frame_Transformed[,otherVolumes] <- (train_frame_Transformed[,otherVolumes])^(1/3)
  train_frame_Transformed[,VolumeRight] <- (train_frame_Transformed[,VolumeRight])^(1/3)
  train_frame_Transformed[,VolumeLeft] <- (train_frame_Transformed[,VolumeLeft])^(1/3)
  train_frame_Transformed[,AreaRight] <- (train_frame_Transformed[,AreaRight])^1/2
  train_frame_Transformed[,AreaLeft] <- (train_frame_Transformed[,AreaLeft])^1/2
  train_frame_Transformed$nICV <- train_frame_Transformed$ICV^(1/3)
  
  
  ICV <- train_frame_Transformed$nICV
  
  train_frame_Transformed[,otherVolumes] <- (train_frame_Transformed[,otherVolumes])/ICV
  train_frame_Transformed[,VolumeRight] <- (train_frame_Transformed[,VolumeRight])/ICV
  train_frame_Transformed[,VolumeLeft] <- (train_frame_Transformed[,VolumeLeft])/ICV
  train_frame_Transformed[,AreaRight] <- (train_frame_Transformed[,AreaRight])/ICV
  train_frame_Transformed[,AreaLeft] <- (train_frame_Transformed[,AreaLeft])/ICV
  train_frame_Transformed[,ThicknessRight] <- (train_frame_Transformed[,ThicknessRight])/ICV
  train_frame_Transformed[,ThicknessLeft] <- (train_frame_Transformed[,ThicknessLeft])/ICV
  
  
  MeanVolume <- (train_frame_Transformed[,VolumeRight] + train_frame_Transformed[,VolumeLeft])/2
  colnames(MeanVolume) <- paste("Mean",colnames(MeanVolume),sep="_")
  DifVolume <- abs(train_frame_Transformed[,VolumeRight] - train_frame_Transformed[,VolumeLeft])
  colnames(DifVolume) <- paste("Dif",colnames(DifVolume),sep="_")
  train_frame_Transformed[,VolumeRight] <-NULL
  train_frame_Transformed[,VolumeLeft] <- NULL
  
  MeanArea <- (train_frame_Transformed[,AreaRight] + train_frame_Transformed[,AreaLeft])/2
  colnames(MeanArea) <- paste("Mean",colnames(MeanArea),sep="_")
  DifArea <- abs(train_frame_Transformed[,AreaRight] - train_frame_Transformed[,AreaLeft])
  colnames(DifArea) <- paste("Dif",colnames(DifArea),sep="_")
  train_frame_Transformed[,AreaRight] <- NULL
  train_frame_Transformed[,AreaLeft] <- NULL
  
  MeanThickness <- (train_frame_Transformed[,ThicknessRight] + train_frame_Transformed[,ThicknessLeft])/2
  colnames(MeanThickness) <- paste("Mean",colnames(MeanThickness),sep="_")
  DifThickness <- abs(train_frame_Transformed[,ThicknessRight] - train_frame_Transformed[,ThicknessLeft])
  colnames(DifThickness) <- paste("Dif",colnames(DifThickness),sep="_")
  train_frame_Transformed[,ThicknessRight] <- NULL
  train_frame_Transformed[,ThicknessLeft] <- NULL
  
  train_frame_Transformed <- cbind(train_frame_Transformed,MeanVolume,DifVolume,MeanArea,DifArea,MeanThickness,DifThickness)
  
  train_frame_Transformed$MeanVolumes <- apply(MeanVolume,1,mean,na.rm=TRUE)
  train_frame_Transformed$StdVolumes <- apply(MeanVolume,1,sd,na.rm=TRUE)
  train_frame_Transformed$COVOlumens <- train_frame_Transformed$StdVolumes/train_frame_Transformed$MeanVolumes
  
  train_frame_Transformed$MeanArea <- apply(MeanArea,1,mean,na.rm=TRUE)
  train_frame_Transformed$StdArea <- apply(MeanArea,1,sd,na.rm=TRUE)
  train_frame_Transformed$COArea <- train_frame_Transformed$StdArea/train_frame_Transformed$MeanArea
  
  train_frame_Transformed$MeanThickness <- apply(MeanThickness,1,mean,na.rm=TRUE)
  train_frame_Transformed$StdThickness <- apply(MeanThickness,1,sd,na.rm=TRUE)
  train_frame_Transformed$COMeanThickness <- train_frame_Transformed$StdThickness/train_frame_Transformed$MeanThickness
  
## Testing
  
  test_Frame_Transformed[,colnumeric] <- sapply(test_Frame_Transformed[,colnumeric],as.numeric)
  
  
  test_Frame_Transformed[,otherVolumes] <- (test_Frame_Transformed[,otherVolumes])^(1/3)
  test_Frame_Transformed[,VolumeRight] <- (test_Frame_Transformed[,VolumeRight])^(1/3)
  test_Frame_Transformed[,VolumeLeft] <- (test_Frame_Transformed[,VolumeLeft])^(1/3)
  test_Frame_Transformed[,AreaRight] <- (test_Frame_Transformed[,AreaRight])^1/2
  test_Frame_Transformed[,AreaLeft] <- (test_Frame_Transformed[,AreaLeft])^1/2
  test_Frame_Transformed$nICV <- test_Frame_Transformed$ICV^(1/3)
  
  
  ICV <- test_Frame_Transformed$nICV
  
  test_Frame_Transformed[,otherVolumes] <- (test_Frame_Transformed[,otherVolumes])/ICV
  test_Frame_Transformed[,VolumeRight] <- (test_Frame_Transformed[,VolumeRight])/ICV
  test_Frame_Transformed[,VolumeLeft] <- (test_Frame_Transformed[,VolumeLeft])/ICV
  test_Frame_Transformed[,AreaRight] <- (test_Frame_Transformed[,AreaRight])/ICV
  test_Frame_Transformed[,AreaLeft] <- (test_Frame_Transformed[,AreaLeft])/ICV
  test_Frame_Transformed[,ThicknessRight] <- (test_Frame_Transformed[,ThicknessRight])/ICV
  test_Frame_Transformed[,ThicknessLeft] <- (test_Frame_Transformed[,ThicknessLeft])/ICV
  
  
  MeanVolume <- (test_Frame_Transformed[,VolumeRight] + test_Frame_Transformed[,VolumeLeft])/2
  colnames(MeanVolume) <- paste("Mean",colnames(MeanVolume),sep="_")
  DifVolume <- abs(test_Frame_Transformed[,VolumeRight] - test_Frame_Transformed[,VolumeLeft])
  colnames(DifVolume) <- paste("Dif",colnames(DifVolume),sep="_")
  test_Frame_Transformed[,VolumeRight] <-NULL
  test_Frame_Transformed[,VolumeLeft] <- NULL
  
  MeanArea <- (test_Frame_Transformed[,AreaRight] + test_Frame_Transformed[,AreaLeft])/2
  colnames(MeanArea) <- paste("Mean",colnames(MeanArea),sep="_")
  DifArea <- abs(test_Frame_Transformed[,AreaRight] - test_Frame_Transformed[,AreaLeft])
  colnames(DifArea) <- paste("Dif",colnames(DifArea),sep="_")
  test_Frame_Transformed[,AreaRight] <- NULL
  test_Frame_Transformed[,AreaLeft] <- NULL
  
  MeanThickness <- (test_Frame_Transformed[,ThicknessRight] + test_Frame_Transformed[,ThicknessLeft])/2
  colnames(MeanThickness) <- paste("Mean",colnames(MeanThickness),sep="_")
  DifThickness <- abs(test_Frame_Transformed[,ThicknessRight] - test_Frame_Transformed[,ThicknessLeft])
  colnames(DifThickness) <- paste("Dif",colnames(DifThickness),sep="_")
  test_Frame_Transformed[,ThicknessRight] <- NULL
  test_Frame_Transformed[,ThicknessLeft] <- NULL
  
  test_Frame_Transformed <- cbind(test_Frame_Transformed,MeanVolume,DifVolume,MeanArea,DifArea,MeanThickness,DifThickness)
  
  test_Frame_Transformed$MeanVolumes <- apply(MeanVolume,1,mean,na.rm=TRUE)
  test_Frame_Transformed$StdVolumes <- apply(MeanVolume,1,sd,na.rm=TRUE)
  test_Frame_Transformed$COVOlumens <- test_Frame_Transformed$StdVolumes/test_Frame_Transformed$MeanVolumes
  
  test_Frame_Transformed$MeanArea <- apply(MeanArea,1,mean,na.rm=TRUE)
  test_Frame_Transformed$StdArea <- apply(MeanArea,1,sd,na.rm=TRUE)
  test_Frame_Transformed$COArea <- test_Frame_Transformed$StdArea/test_Frame_Transformed$MeanArea
  
  test_Frame_Transformed$MeanThickness <- apply(MeanThickness,1,mean,na.rm=TRUE)
  test_Frame_Transformed$StdThickness <- apply(MeanThickness,1,sd,na.rm=TRUE)
  test_Frame_Transformed$COMeanThickness <- test_Frame_Transformed$StdThickness/test_Frame_Transformed$MeanThickness
  test_Frame_Transformed$nICV <- test_Frame_Transformed$ICV^(1/3)
  
## Train Imputation    
  
  checkmissing <- colImputeThreshold*nrow(train_frame_Transformed)
  
  table(train_frame_Transformed$VISCODE)
  sum(is.na(train_frame_Transformed$DX))
  
  shortvisits <- subset(train_frame_Transformed,M <= MinVisit)
  
  tokeep = !(colnames(shortvisits) %in% c("DX","nICV"))
  
  missingData <- c(rep(FALSE,length(notQuantitative)),
                   apply(is.na(shortvisits[,-notQuantitative]),2,sum) > checkmissing)

  missingData <- missingData & tokeep

  print(sum(!missingData))
  
  train_frame_Transformed_red <- train_frame_Transformed[,!missingData]
  print(colnames(train_frame_Transformed_red))

  checkRowmissing <- rowImputeThreshold*ncol(train_frame_Transformed_red)
  mssingRowData <- apply(is.na(train_frame_Transformed_red),1,sum) >= checkRowmissing
  print(sum(!mssingRowData))
  
  train_frame_Transformed_red <- train_frame_Transformed_red[!mssingRowData,]
  
#  colnames(train_frame_Transformed_red)

  theincluded <- unique(c("RID","AGE","PTGENDER",colnames(train_frame_Transformed_red)[-notQuantitative]))
  colnotincluded <- !(colnames(train_frame_Transformed_red) %in% theincluded)
  
  
  TadpoleOnlyFeatures <- train_frame_Transformed_red[,theincluded]
  
  
  TadpoleTrain_Imputed <- train_frame_Transformed_red
  allAdustedZrank <- TadpoleTrain_Imputed
  TadpoleTrain_Imputed <- cbind(train_frame_Transformed_red[,colnotincluded],nearestNeighborImpute(TadpoleOnlyFeatures,catgoricCol=c("RID","PTGENDER")))
  checkVV <- TadpoleTrain_Imputed$Ventricles < 0.99*TadpoleTrain_Imputed$Ventricles_bl
  checkVV <- checkVV | TadpoleTrain_Imputed$Ventricles > 1.5*TadpoleTrain_Imputed$Ventricles_bl
  checkVV <- checkVV & (is.na(TadpoleOnlyFeatures$Ventricles) & !is.na(TadpoleOnlyFeatures$Ventricles_bl))
  print(sum(checkVV))
  TadpoleTrain_Imputed$Ventricles[checkVV] <- TadpoleTrain_Imputed$Ventricles_bl[checkVV]
  checkICV <- TadpoleTrain_Imputed$ICV < 0.95*TadpoleTrain_Imputed$ICV_bl | TadpoleTrain_Imputed$ICV > 1.20*TadpoleTrain_Imputed$ICV_bl
  checkICV <- checkICV & is.na(TadpoleOnlyFeatures$ICV) & !is.na(TadpoleOnlyFeatures$ICV_bl)
  TadpoleTrain_Imputed$ICV[checkICV] <- TadpoleTrain_Imputed$ICV_bl[checkICV]
  print(sum(checkICV))
  
  fnames <- colnames(TadpoleTrain_Imputed)
  fnames <- str_replace_all(fnames," ","_")
  fnames <- str_replace_all(fnames,"/","_")
  fnames <- str_replace_all(fnames,":","_")
  fnames <- str_replace_all(fnames,"//.","_")
  colnames(TadpoleTrain_Imputed) <- fnames

  table(TadpoleTrain_Imputed$PTGENDER)
#  TadpoleTrain_Imputed$PTGENDER <- 1*(TadpoleTrain_Imputed$PTGENDER=="Male")
#  table(TadpoleTrain_Imputed$PTGENDER)

  TadpoleTrain_Imputed$AGE <- TadpoleTrain_Imputed$AGE + TadpoleTrain_Imputed$Years_bl

  cognitiveNormal <- subset(TadpoleTrain_Imputed,DX=="NL" & VISCODE=="bl")

  predictors <- colnames(cognitiveNormal)[-notQuantitative]
  predictors <- cbind(predictors,predictors)

  allAdusted <- featureAdjustment(predictors, baseModel="1+AGE+nICV",data=TadpoleTrain_Imputed,referenceframe=cognitiveNormal,strata="PTGENDER", type = "LM", pvalue = 0.001)
  adjustedContol <- subset(allAdusted,DX=="NL" & VISCODE=="bl")


  trainAdustedZrank <- rankInverseNormalDataFrame(predictors,
                                                allAdusted,
                                                adjustedContol,
                                                strata="PTGENDER")
  
## Testing imputation

    newrawnames <- paste(train_frame_Transformed_red$RID,train_frame_Transformed_red$VISCODE,sep="_")
    rownames(train_frame_Transformed_red) <- newrawnames
    
    test_Frame_Transformed_red <- test_Frame_Transformed[,!missingData]
    print(nrow(test_Frame_Transformed_red))
    newrawnames <- paste(test_Frame_Transformed_red$RID,test_Frame_Transformed_red$VISCODE,sep="_")
    testIDS <- unique(test_Frame_Transformed_red$RID)
    
    
    print(c(length(newrawnames),length(unique(newrawnames))))
    rownames(test_Frame_Transformed_red) <- newrawnames
    
    trainIDInTest <- train_frame_Transformed_red[train_frame_Transformed_red$RID %in% testIDS,]
    
    rownames(trainIDInTest) <- paste(trainIDInTest$RID,trainIDInTest$VISCODE,sep="_")
    trainObsnotinTest <- !(rownames(trainIDInTest) %in% rownames(test_Frame_Transformed_red))
    traininnotest <- trainIDInTest[trainObsnotinTest,]
    
    
    test_Frame_Transformed_red <- rbind(test_Frame_Transformed_red,traininnotest)
    print(nrow(test_Frame_Transformed_red))
    test_Frame_Transformed_red <- test_Frame_Transformed_red[order(test_Frame_Transformed_red$EXAMDATE),]
    test_Frame_Transformed_red <- test_Frame_Transformed_red[order(as.integer(test_Frame_Transformed_red$RID)),]
    
    
#    checkRowmissing <- 3 #at least 3 features
#    mssingRowData <- apply(is.na(test_Frame_Transformed_red),1,sum) > checkRowmissing
#    print(sum(!mssingRowData))
#    test_Frame_Transformed_red <- test_Frame_Transformed_red[!mssingRowData,]
    
    test_Frame_Transformed_red$RID <- as.numeric(test_Frame_Transformed_red$RID)
    test_Frame_Transformed_red$AGE <- as.numeric(test_Frame_Transformed_red$AGE)
#    test_Frame_Transformed_red$PTGENDER <- 1*(test_Frame_Transformed_red$PTGENDER=="Male")
    
    train_frame_Transformed_red$RID <- as.numeric(train_frame_Transformed_red$RID)
    train_frame_Transformed_red$AGE <- as.numeric(train_frame_Transformed_red$AGE)
#    train_frame_Transformed_red$PTGENDER <- 1*(train_frame_Transformed_red$PTGENDER=="Male")
    
#    print(colnames(test_Frame_Transformed_red))
    
    theincluded <- unique(c("AGE","PTGENDER",colnames(test_Frame_Transformed_red)[-notQuantitative]))
    colcategorical <- c("PTGENDER")
    if (includeID)
    {
      theincluded <- c("RID",theincluded)
      colcategorical <- c("RID",colcategorical)
    }
    TadpoleOnlyFeatures <- test_Frame_Transformed_red[,theincluded]
    TadpoleRefOnlyFeatures <- train_frame_Transformed_red[,theincluded]
    
    print(ncol(TadpoleOnlyFeatures))
#    print(colnames(TadpoleOnlyFeatures))
    colnotincluded <- !(colnames(test_Frame_Transformed_red) %in% theincluded)
    print(colnames(test_Frame_Transformed_red)[colnotincluded])
#    print(summary(TadpoleOnlyFeatures))
#    print(summary(TadpoleRefOnlyFeatures))
    
    Tadpole_Imputed <- nearestNeighborImpute(TadpoleOnlyFeatures,TadpoleRefOnlyFeatures,catgoricCol=colcategorical,useorder=includeID)
    checkVV <- Tadpole_Imputed$Ventricles < 0.99*Tadpole_Imputed$Ventricles_bl
    checkVV <- checkVV | Tadpole_Imputed$Ventricles > 1.5*Tadpole_Imputed$Ventricles_bl
    Tadpole_Imputed$Ventricles[checkVV & is.na(TadpoleOnlyFeatures$Ventricles)] <- Tadpole_Imputed$Ventricles_bl[checkVV & is.na(TadpoleOnlyFeatures$Ventricles)]
    Tadpole_Imputed$Ventricles_bl[checkVV & is.na(TadpoleOnlyFeatures$Ventricles_bl)] <- Tadpole_Imputed$Ventricles[checkVV & is.na(TadpoleOnlyFeatures$Ventricles_bl)]
    checkICV <- Tadpole_Imputed$ICV < 0.95*Tadpole_Imputed$ICV_bl | Tadpole_Imputed$ICV > 1.20*Tadpole_Imputed$ICV_bl
    Tadpole_Imputed$ICV[checkICV & is.na(TadpoleOnlyFeatures$ICV)] <- Tadpole_Imputed$ICV_bl[checkICV & is.na(TadpoleOnlyFeatures$ICV)]
    Tadpole_Imputed$ICV_bl[checkICV  & is.na(TadpoleOnlyFeatures$ICV_bl)] <- Tadpole_Imputed$ICV[checkICV  & is.na(TadpoleOnlyFeatures$ICV_bl)]
    
    
        
    print(c(nrow(Tadpole_Imputed),ncol(Tadpole_Imputed)))
    Tadpole_Imputed <- cbind(test_Frame_Transformed_red[,colnotincluded],Tadpole_Imputed)
    print(c(nrow(Tadpole_Imputed),ncol(Tadpole_Imputed)))
    
    fnames <- colnames(Tadpole_Imputed)
    fnames <- str_replace_all(fnames," ","_")
    fnames <- str_replace_all(fnames,"/","_")
    fnames <- str_replace_all(fnames,":","_")
    fnames <- str_replace_all(fnames,"//.","_")
    colnames(Tadpole_Imputed) <- fnames
   
    print(table(Tadpole_Imputed$PTGENDER))

    testAdusted <- featureAdjustment(predictors, baseModel="1+AGE+nICV",data=Tadpole_Imputed,referenceframe=cognitiveNormal,strata="PTGENDER", type = "LM", pvalue = 0.001)
   
    print(c(nrow(testAdusted),ncol(testAdusted)))
    
    testAdustedZrank <- rankInverseNormalDataFrame(predictors, 
                                                   testAdusted, 
                                                   adjustedContol,
                                                   strata="PTGENDER")
    print(c(nrow(testAdustedZrank),ncol(testAdustedZrank)))
    
  
  
  DataFrames <- list(AdjustedTrainFrame=trainAdustedZrank,
                     testingFrame = testAdustedZrank,
                     Train_Imputed = TadpoleTrain_Imputed,
                     Test_Imputed = Tadpole_Imputed
                     )
  
  
  return (DataFrames)
}